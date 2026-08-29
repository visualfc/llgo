param(
  [ValidateSet("msvc", "mingw")]
  [string]$Profile = "msvc"
)

$ErrorActionPreference = "Stop"

function Assert-Success([string]$Operation, [int]$ExitCode = $LASTEXITCODE) {
  if ($ExitCode -ne 0) {
    throw "$Operation failed with exit code $ExitCode"
  }
}

function Invoke-NativeCapture([string]$Executable, [object[]]$ArgumentList = @()) {
  # Windows PowerShell 5.1 promotes redirected native stderr to ErrorRecord.
  # Capture each record as its original text under Continue so LLGo's normal
  # -x trace and println stderr remain data; pwsh 7 follows the same path.
  $savedErrorActionPreference = $ErrorActionPreference
  try {
    $ErrorActionPreference = "Continue"
    $output = (& $Executable @ArgumentList 2>&1 |
      ForEach-Object { $_.ToString() }) -join [Environment]::NewLine
    $exitCode = $LASTEXITCODE
  } finally {
    $ErrorActionPreference = $savedErrorActionPreference
  }
  return [PSCustomObject]@{ Output = $output; ExitCode = $exitCode }
}

function Assert-NativeOutput([string]$Executable) {
  # Go's implementation-defined println builtin may write to stderr. Capture
  # both streams because this smoke test validates execution, not stream choice.
  $result = Invoke-NativeCapture $Executable
  Assert-Success "Running $Executable" $result.ExitCode
  $output = $result.Output.Trim()
  $want = "windows-$Profile-profile"
  if ($output -ne $want) {
    throw "$Executable printed '$output', want '$want'"
  }
  $dependents = (& llvm-readobj.exe --coff-imports $Executable | Out-String)
  Assert-Success "Inspecting $Executable"
  $forbidden = if ($Profile -eq "msvc") {
    '(?i)(msys-2\.0|cygwin1|libwinpthread)\.dll'
  } else {
    '(?i)(msys-2\.0|cygwin1)\.dll'
  }
  if ($dependents -match $forbidden) {
    throw "$Executable has an unsupported POSIX-emulation dependency:`n$dependents"
  }
}

$sourceDir = Join-Path $env:RUNNER_TEMP "llgo-windows-toolchain-profile"
New-Item -ItemType Directory -Force $sourceDir | Out-Null
@'
module example.com/llgo-windows-toolchain-profile

go 1.27
'@ | Set-Content -Encoding ascii (Join-Path $sourceDir "go.mod")
$mainSource = @'
package main

func main() {
	println("windows-$Profile-profile")
}
'@
$mainSource.Replace('$Profile', $Profile) | Set-Content -Encoding utf8 (Join-Path $sourceDir "main.go")

$llgo = (Get-Command llgo.exe).Source
foreach ($tool in @("clang.exe", "clang++.exe", "llvm-config.exe")) {
  $path = (Get-Command $tool).Source
  if ($Profile -eq "msvc" -and $path -match '(?i)[\\/](msys64|cygwin)[\\/]') {
    throw "$tool unexpectedly resolves through a POSIX environment: $path"
  }
  if ($Profile -eq "mingw" -and $path -notmatch '(?i)[\\/]clang64[\\/]') {
    throw "$tool does not belong to the independent CLANG64 profile: $path"
  }
}
$pkgConfig = (Get-Command pkg-config).Source
if ($pkgConfig -notmatch '(?i)[\\/]llgo-(msvc|mingw)-tools[\\/]pkg-config\.cmd$') {
  throw "pkg-config does not resolve to the profile-local command wrapper: $pkgConfig"
}
$pkgConfigShell = Join-Path (Split-Path $pkgConfig) "pkg-config"
if (-not (Test-Path $pkgConfigShell)) {
  throw "The shell-compatible pkg-config wrapper was not found: $pkgConfigShell"
}
if ($env:PKG_CONFIG -or $env:PKG_CONFIG_PATH) {
  throw "The $Profile profile unexpectedly requires PKG_CONFIG or PKG_CONFIG_PATH"
}
if ($Profile -eq "msvc" -and $env:LLGO_MSYS2_LOCATION) {
  throw "The MSVC lane still exports LLGO_MSYS2_LOCATION"
}
& pkg-config --modversion llvm-19 | Out-Null
Assert-Success "Reading LLVM metadata through the profile-local pkg-config"

$compilerTarget = (& clang.exe -dumpmachine).Trim()
Assert-Success "Reading the Clang target"
$targetPattern = if ($Profile -eq "msvc") { '-windows-msvc$' } else { '-(windows-gnu|mingw32)$' }
if ($compilerTarget -notmatch $targetPattern) {
  throw "$Profile Clang reports incompatible target $compilerTarget"
}

$savedCC = $env:CC
$savedCXX = $env:CXX
try {
  Remove-Item Env:CC -ErrorAction SilentlyContinue
  Remove-Item Env:CXX -ErrorAction SilentlyContinue

  $powershellExe = Join-Path $sourceDir "powershell.exe"
  Push-Location $sourceDir
  try {
    $result = Invoke-NativeCapture $llgo @("build", "-x", "-o", $powershellExe, ".")
    Assert-Success "Building with unset CC/CXX from PowerShell" $result.ExitCode
    $trace = $result.Output
  } finally {
    Pop-Location
  }
  $canonicalTarget = if ($Profile -eq "msvc") { 'x86_64-pc-windows-msvc' } else { 'x86_64-w64-windows-gnu' }
  if ($trace -notmatch [regex]::Escape($canonicalTarget)) {
    throw "Unset CC/CXX did not select the canonical $Profile target:`n$trace"
  }
  Assert-NativeOutput $powershellExe

  $cmdExe = Join-Path $sourceDir "cmd.exe"
  $cmdLine = 'cd /d "' + $sourceDir + '" && "' + $llgo + '" build -o "' + $cmdExe + '" .'
  & $env:ComSpec /d /s /c $cmdLine
  Assert-Success "Building with unset CC/CXX from cmd.exe"
  Assert-NativeOutput $cmdExe
} finally {
  if ($null -eq $savedCC) { Remove-Item Env:CC -ErrorAction SilentlyContinue } else { $env:CC = $savedCC }
  if ($null -eq $savedCXX) { Remove-Item Env:CXX -ErrorAction SilentlyContinue } else { $env:CXX = $savedCXX }
}
