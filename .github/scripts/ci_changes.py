"""Shared Git change records for CI policy and benchmark reporting."""

from dataclasses import dataclass
import subprocess


@dataclass(frozen=True)
class Change:
    status: str
    paths: tuple[str, ...]


def parse_name_status_z(data: bytes) -> list[Change]:
    """Preserve both sides of renames and arbitrary Git path characters."""
    fields = data.decode("utf-8", errors="surrogateescape").split("\0")
    if fields and not fields[-1]:
        fields.pop()
    changes = []
    index = 0
    while index < len(fields):
        status = fields[index]
        index += 1
        count = 2 if status.startswith(("R", "C")) else 1
        if index + count > len(fields) or not status:
            raise ValueError(f"incomplete git name-status record for {status!r}")
        paths = tuple(fields[index : index + count])
        if not all(paths):
            raise ValueError("empty path in git name-status record")
        changes.append(Change(status, paths))
        index += count
    return changes


def git(*args: str) -> bytes:
    return subprocess.run(
        ["git", *args], check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE
    ).stdout
