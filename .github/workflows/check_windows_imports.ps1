param(
    [Parameter(Mandatory = $true)]
    [string]$ReadObj,

    [Parameter(Mandatory = $true)]
    [string[]]$Artifacts
)

$ErrorActionPreference = "Stop"

if (-not (Test-Path -LiteralPath $ReadObj -PathType Leaf)) {
    throw "llvm-readobj was not found at $ReadObj"
}

$forbiddenImports = @(
    "cygwin1.dll",
    "msys-2.0.dll"
)

foreach ($artifact in $Artifacts) {
    if (-not (Test-Path -LiteralPath $artifact -PathType Leaf)) {
        throw "Windows artifact was not found at $artifact"
    }

    $output = & $ReadObj --coff-imports $artifact | Out-String
    if ($LASTEXITCODE -ne 0) {
        throw "llvm-readobj failed for $artifact with exit code $LASTEXITCODE"
    }

    $imports = @(
        $output -split "`r?`n" |
            ForEach-Object {
                if ($_ -match '^\s*Name:\s*(\S+)\s*$') {
                    $Matches[1].ToLowerInvariant()
                }
            }
    )
    Write-Host "==> $artifact"
    $imports | ForEach-Object { Write-Host "    $_" }

    foreach ($name in $imports) {
        if ($name -in $forbiddenImports -or
            $name -like "libwinpthread*.dll" -or
            $name -like "libgcc_s*.dll" -or
            $name -like "libstdc++*.dll") {
            throw "$artifact imports unsupported POSIX/MinGW runtime $name"
        }
    }
}
