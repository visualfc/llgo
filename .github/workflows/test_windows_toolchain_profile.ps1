param(
  [switch]$SkipNativeShells,
  [string[]]$BashPaths = @()
)

$ErrorActionPreference = "Stop"

function Assert-Success([string]$Operation) {
  if ($LASTEXITCODE -ne 0) {
    throw "$Operation failed with exit code $LASTEXITCODE"
  }
}

function Assert-NativeOutput([string]$Executable) {
  $output = (& $Executable | Out-String).Trim()
  Assert-Success "Running $Executable"
  if ($output -ne "windows-msvc-shell") {
    throw "$Executable printed '$output', want 'windows-msvc-shell'"
  }
  $dependents = (& dumpbin.exe /nologo /dependents $Executable | Out-String)
  Assert-Success "Inspecting $Executable"
  if ($dependents -match '(?i)(msys-2\.0|cygwin1|libwinpthread)\.dll') {
    throw "$Executable has an unsupported POSIX-emulation dependency:`n$dependents"
  }
}

function Quote-Sh([string]$Value) {
  if ($Value.Contains("'")) {
    throw "A shell-smoke path contains an unsupported single quote: $Value"
  }
  return "'$Value'"
}

$sourceDir = Join-Path $env:RUNNER_TEMP "llgo-windows-toolchain-profile"
New-Item -ItemType Directory -Force $sourceDir | Out-Null
@'
module example.com/llgo-windows-toolchain-profile

go 1.26
'@ | Set-Content -Encoding ascii (Join-Path $sourceDir "go.mod")
@'
package main

func main() {
	println("windows-msvc-shell")
}
'@ | Set-Content -Encoding utf8 (Join-Path $sourceDir "main.go")

$llgo = (Get-Command llgo.exe).Source
if (-not $SkipNativeShells) {
  foreach ($tool in @("clang.exe", "clang++.exe", "llvm-config.exe")) {
    $path = (Get-Command $tool).Source
    if ($path -match '(?i)[\\/](msys64|cygwin)[\\/]') {
      throw "$tool unexpectedly resolves through a POSIX environment: $path"
    }
  }
  if (-not $env:PKG_CONFIG -or -not (Test-Path $env:PKG_CONFIG)) {
    throw "PKG_CONFIG does not name an installed native executable: $env:PKG_CONFIG"
  }
  if ($env:PKG_CONFIG -match '(?i)[\\/](msys64|cygwin|strawberry)[\\/]') {
    throw "PKG_CONFIG unexpectedly resolves through a bundled POSIX or Perl environment: $env:PKG_CONFIG"
  }
  & $env:PKG_CONFIG --modversion llvm-19 | Out-Null
  Assert-Success "Reading LLVM metadata with native pkgconf"
  if ($env:LLGO_MSYS2_LOCATION) {
    throw "The native Windows lane still exports LLGO_MSYS2_LOCATION"
  }

  $savedCC = $env:CC
  $savedCXX = $env:CXX
  try {
    Remove-Item Env:CC -ErrorAction SilentlyContinue
    Remove-Item Env:CXX -ErrorAction SilentlyContinue

    $powershellExe = Join-Path $sourceDir "powershell.exe"
    Push-Location $sourceDir
    try {
      $trace = (& $llgo build -x -o $powershellExe . 2>&1 | Out-String)
      Assert-Success "Building with unset CC/CXX from PowerShell"
    } finally {
      Pop-Location
    }
    if ($trace -notmatch 'x86_64-pc-windows-msvc') {
      throw "Unset CC/CXX did not select the canonical MSVC target:`n$trace"
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
}

foreach ($bash in $BashPaths) {
  if (-not (Test-Path $bash)) {
    throw "Requested shell was not found: $bash"
  }
  $sourceUnix = (& $bash -lc "cygpath -u $(Quote-Sh $sourceDir)").Trim()
  Assert-Success "Converting the source path in $bash"
  $llgoUnix = (& $bash -lc "cygpath -u $(Quote-Sh $llgo)").Trim()
  Assert-Success "Converting the LLGo path in $bash"
  $name = if ($bash -match '(?i)[\\/]cygwin[\\/]') {
    "cygwin"
  } elseif ($bash -match '(?i)[\\/]msys[^\\/]*[\\/]') {
    "msys2"
  } else {
    (Split-Path (Split-Path $bash -Parent) -Leaf) -replace '[^A-Za-z0-9_.-]', '-'
  }
  $outputUnix = "$sourceUnix/$name.exe"
  $command = "cd $(Quote-Sh $sourceUnix) && unset CC CXX && $(Quote-Sh $llgoUnix) build -x -o $(Quote-Sh $outputUnix) . && $(Quote-Sh $outputUnix)"
  $trace = (& $bash -lc $command 2>&1 | Out-String)
  Assert-Success "Building and running through $bash"
  if ($trace -notmatch 'x86_64-pc-windows-msvc' -or $trace -notmatch 'windows-msvc-shell') {
    throw "The shell did not preserve the default MSVC profile:`n$trace"
  }
  Assert-NativeOutput (Join-Path $sourceDir "$name.exe")
}
