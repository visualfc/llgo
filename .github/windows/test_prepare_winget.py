import importlib.util
import hashlib
import json
from pathlib import Path
import tempfile
import unittest
import zipfile

spec = importlib.util.spec_from_file_location('prepare_winget', Path(__file__).with_name('prepare-winget.py'))
winget = importlib.util.module_from_spec(spec)
spec.loader.exec_module(winget)


class WinGetTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.output = self.root / 'output'
        for profile in winget.PROFILES:
            for arch in winget.ARCHITECTURES:
                self.fixture(profile, arch)

    def fixture(self, profile, arch, **overrides):
        path = self.root / f'llgo1.2.3.windows-{arch}-{profile}.zip'
        metadata = dict(version='1.2.3', goos='windows', goarch=arch, abi=profile, commit='a' * 40)
        metadata.update(overrides)
        with zipfile.ZipFile(path, 'w') as archive:
            archive.writestr('release.json', json.dumps(metadata))
            for name in ('bin/llgo.exe', 'runtime/go.mod', 'WINDOWS.md'):
                archive.writestr(name, 'fixture')
        digest = hashlib.sha256(path.read_bytes()).hexdigest()
        path.with_suffix('.zip.sha256').write_text(f'{digest}  {path.name}\n')
        return path

    def test_two_profiles_two_architectures_real_hashes(self):
        winget.prepare(self.root, self.output, '1.2.3')
        files = list(self.output.rglob('*.yaml'))
        self.assertEqual(len(files), 6)
        for label in ('MSVC', 'MinGW'):
            path = self.output / 'manifests/x/XGo/LLGo' / label / '1.2.3'
            text = (path / f'XGo.LLGo.{label}.installer.yaml').read_text()
            self.assertIn('Architecture: x64', text)
            self.assertIn('Architecture: arm64', text)
            self.assertEqual('ArchiveBinariesDependOnPath: true' in text, label == 'MinGW')
            self.assertNotIn('ProductCode', text)
            self.assertNotIn('Scope:', text)
            for arch in ('amd64', 'arm64'):
                archive = self.root / f'llgo1.2.3.windows-{arch}-{label.lower()}.zip'
                self.assertIn(hashlib.sha256(archive.read_bytes()).hexdigest(), text)
                self.assertIn('/releases/download/v1.2.3/' + archive.name, text)

    def test_corrupted_archive_rejected_before_output(self):
        path = self.root / 'llgo1.2.3.windows-arm64-mingw.zip'
        path.write_bytes(path.read_bytes() + b'tampered')
        with self.assertRaisesRegex(ValueError, 'checksum'):
            winget.prepare(self.root, self.output, '1.2.3')
        self.assertFalse(self.output.exists())

    def test_wrong_architecture_rejected(self):
        self.fixture('mingw', 'arm64', goarch='amd64')
        with self.assertRaisesRegex(ValueError, 'metadata'):
            winget.prepare(self.root, self.output, '1.2.3')

    def test_mixed_commits_rejected(self):
        self.fixture('mingw', 'arm64', commit='b' * 40)
        with self.assertRaisesRegex(ValueError, 'different source commits'):
            winget.prepare(self.root, self.output, '1.2.3')


if __name__ == '__main__':
    unittest.main()
