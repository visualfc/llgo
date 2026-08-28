param(
  [Parameter(Mandatory = $true)]
  [string]$LLGo,
  [ValidateSet("msvc", "mingw")]
  [string]$Profile = "msvc",
  [Parameter(Mandatory = $true)]
  [ValidateSet("386", "amd64", "arm64")]
  [string]$GoArch
)

$ErrorActionPreference = "Stop"

if (-not (Test-Path $LLGo)) {
  throw "LLGo compiler was not found at $LLGo"
}
$LLGo = (Resolve-Path $LLGo).Path
$root = (Get-Location).Path
$clangExe = (Get-Command clang.exe).Source
$llvmNmExe = (Get-Command llvm-nm.exe).Source
$readObjExe = (Get-Command llvm-readobj.exe).Source
$out = Join-Path $env:RUNNER_TEMP ("llgo-windows-runtime-" + [Guid]::NewGuid())
New-Item -ItemType Directory $out | Out-Null

$env:LLGO_ROOT = $root
$env:LLGO_BUILD_CACHE = "off"

# Each lane executes its selected architecture below. Also compile the raw
# SyscallN bridge for every Go-supported Windows architecture so a change in
# one lane cannot silently break assembly selected by another lane.
$syscallAsm = Join-Path $root "runtime\internal\lib\runtime\_wrap\syscall_windows.S"
$targetSuffix = if ($Profile -eq "msvc") { "pc-windows-msvc" } else { "w64-windows-gnu" }
foreach ($syscallTarget in @(
  @{ Triple = "i686-$targetSuffix"; Symbol = "_llgo_windows_syscall" },
  @{ Triple = "x86_64-$targetSuffix"; Symbol = "llgo_windows_syscall" },
  @{ Triple = "aarch64-$targetSuffix"; Symbol = "llgo_windows_syscall" }
)) {
  $syscallObj = Join-Path $out ("syscall-{0}.obj" -f $syscallTarget.Triple)
  & $clangExe "--target=$($syscallTarget.Triple)" -c $syscallAsm -o $syscallObj
  if ($LASTEXITCODE -ne 0) {
    exit $LASTEXITCODE
  }
  $symbols = & $llvmNmExe --defined-only $syscallObj | Out-String
  if (-not $symbols.Contains($syscallTarget.Symbol)) {
    throw "$($syscallTarget.Triple) bridge is missing $($syscallTarget.Symbol)"
  }
}

$runtime = Join-Path $out "windows-runtime-smoke.exe"
$stdlib = Join-Path $out "windows-stdlib-smoke.exe"
$ffi = Join-Path $out "windows-ffi-smoke.exe"
$empty = Join-Path $out "windows-empty-smoke.exe"
$coreFault = Join-Path $out "windows-core-fault-smoke.exe"
$network = Join-Path $out "windows-network-smoke.exe"

# These fixtures cover minimal-runtime links and process behavior that a
# testing binary can accidentally satisfy through optional stdlib imports.
Push-Location runtime
try {
  & $LLGo build -o $runtime .\_test\windowsruntime
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
  & $LLGo build -tags=nogc -o $stdlib .\_test\windowsstdlib
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
  & $LLGo build -o $ffi .\_test\windowsffi
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
  & $LLGo build -o $empty .\_test\windowsempty
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
  & $LLGo build -o $coreFault .\_test\windowscorefault
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
  & $LLGo build -o $network .\_test\windowsnetwork
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
} finally {
  Pop-Location
}

& .\.github\workflows\check_windows_imports.ps1 `
  -ReadObj $readObjExe `
  -Artifacts @($runtime, $stdlib, $ffi, $empty, $coreFault, $network)

$expectedMachine = switch ($GoArch) {
  "386" { "IMAGE_FILE_MACHINE_I386" }
  "amd64" { "IMAGE_FILE_MACHINE_AMD64" }
  "arm64" { "IMAGE_FILE_MACHINE_ARM64" }
}
foreach ($artifact in @($runtime, $stdlib, $ffi, $empty, $coreFault, $network)) {
  $headers = (& $readObjExe --file-headers $artifact | Out-String)
  if ($LASTEXITCODE -ne 0 -or -not $headers.Contains($expectedMachine)) {
    throw "$artifact is not a windows/$GoArch PE image:`n$headers"
  }
}

Write-Host "==> windows-runtime-smoke.exe"
& $runtime
if ($LASTEXITCODE -ne 0) {
  throw "windows-runtime-smoke.exe exited with code $LASTEXITCODE"
}

Write-Host "==> windows-runtime-smoke.exe (unrecovered fault)"
$env:LLGO_TEST_UNRECOVERED_FAULT = "1"
$savedErrorActionPreference = $ErrorActionPreference
try {
  # Windows PowerShell 5 turns redirected native stderr into a terminating
  # NativeCommandError. This invocation is expected to fail and its stderr is
  # the value asserted below.
  $ErrorActionPreference = "Continue"
  $faultOutput = & $runtime 2>&1 | Out-String
  $faultExitCode = $LASTEXITCODE
} finally {
  $ErrorActionPreference = $savedErrorActionPreference
  Remove-Item Env:LLGO_TEST_UNRECOVERED_FAULT
}
Write-Host $faultOutput
$normalizedFaultOutput = $faultOutput.Replace('\', '/')
if ($faultExitCode -eq 0) {
  throw "unrecovered Windows fault exited successfully"
}
foreach ($expected in @(
  "runtime error: invalid memory address or nil pointer dereference",
  "main.windowsNilFault",
  "windowsruntime/main.go"
)) {
  if (-not $normalizedFaultOutput.Contains($expected)) {
    throw "unrecovered Windows fault output is missing '$expected'"
  }
}
if ($normalizedFaultOutput.Contains("github.com/xgo-dev/llgo/runtime/internal/clite/tls.init")) {
  throw "unrecovered Windows fault traceback continued past runtime.goexit"
}

Write-Host "==> windows-stdlib-smoke.exe"
& $stdlib
if ($LASTEXITCODE -ne 0) {
  throw "windows-stdlib-smoke.exe exited with code $LASTEXITCODE"
}

Write-Host "==> windows-stdlib-smoke.exe (os.Exit)"
$env:LLGO_TEST_OS_EXIT = "1"
& $stdlib
$exitCode = $LASTEXITCODE
Remove-Item Env:LLGO_TEST_OS_EXIT
if ($exitCode -ne 23) {
  throw "os.Exit(23) returned exit code $exitCode"
}

foreach ($artifact in @(
  @{ Name = "windows-ffi-smoke.exe"; Path = $ffi },
  @{ Name = "windows-empty-smoke.exe"; Path = $empty },
  @{ Name = "windows-core-fault-smoke.exe"; Path = $coreFault },
  @{ Name = "windows-network-smoke.exe"; Path = $network }
)) {
  Write-Host "==> $($artifact.Name)"
  & $artifact.Path
  if ($LASTEXITCODE -ne 0) {
    throw "$($artifact.Name) exited with code $LASTEXITCODE"
  }
}
