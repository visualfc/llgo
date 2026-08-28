function Get-LLGoWindowsMSVCTarget {
  param(
    [Parameter(Mandatory = $true)]
    [ValidateSet("386", "amd64", "arm64")]
    [string]$GoArch
  )

  switch ($GoArch) {
    "386" {
      return [PSCustomObject]@{
        GoArch        = "386"
        Triple        = "i686-pc-windows-msvc"
        VcpkgTriplet  = "x86-windows"
        VisualStudio  = "x86"
        Python        = "x86"
      }
    }
    "amd64" {
      return [PSCustomObject]@{
        GoArch        = "amd64"
        Triple        = "x86_64-pc-windows-msvc"
        VcpkgTriplet  = "x64-windows"
        VisualStudio  = "x64"
        Python        = "x64"
      }
    }
    "arm64" {
      return [PSCustomObject]@{
        GoArch        = "arm64"
        Triple        = "aarch64-pc-windows-msvc"
        VcpkgTriplet  = "arm64-windows"
        VisualStudio  = "arm64"
        Python        = "arm64"
      }
    }
  }
}

function Find-LLGoVisualStudio2022 {
  param(
    [Parameter(Mandatory = $true)]
    [ValidateSet("386", "amd64", "arm64")]
    [string]$GoArch
  )

  $vswhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
  $components = @("Microsoft.VisualStudio.Component.VC.Tools.x86.x64")
  if ($GoArch -eq "arm64") {
    $components += "Microsoft.VisualStudio.Component.VC.Tools.ARM64"
  }
  $arguments = @(
    "-latest",
    "-products", "*",
    "-version", "[17.0,18.0)",
    "-property", "installationPath"
  )
  foreach ($component in $components) {
    $arguments += @("-requires", $component)
  }
  $installPath = & $vswhere @arguments
  if (-not $installPath) {
    throw "Visual Studio 2022 C++ tools for windows/$GoArch were not found"
  }
  return $installPath
}

function Enter-LLGoVisualStudio2022 {
  param(
    [Parameter(Mandatory = $true)]
    [ValidateSet("386", "amd64", "arm64")]
    [string]$GoArch
  )

  $target = Get-LLGoWindowsMSVCTarget -GoArch $GoArch
  $installPath = Find-LLGoVisualStudio2022 -GoArch $GoArch
  Import-Module "$installPath\Common7\Tools\Microsoft.VisualStudio.DevShell.dll"
  Enter-VsDevShell -VsInstallPath $installPath `
    -SkipAutomaticLocation `
    -DevCmdArguments "-arch=$($target.VisualStudio) -host_arch=x64"
  return $target
}

function Export-LLGoVisualStudioEnvironment {
  foreach ($name in @(
    "INCLUDE",
    "LIB",
    "LIBPATH",
    "UCRTVersion",
    "UniversalCRTSdkDir",
    "VCINSTALLDIR",
    "VCToolsInstallDir",
    "VCToolsVersion",
    "WindowsSdkDir",
    "WindowsSDKVersion"
  )) {
    $value = [Environment]::GetEnvironmentVariable($name)
    if ($value) {
      Add-Content -Encoding utf8 $env:GITHUB_ENV "$name=$value"
    }
  }

  @("cl.exe", "lib.exe", "link.exe", "dumpbin.exe", "nmake.exe", "rc.exe", "mt.exe") |
    ForEach-Object { Split-Path (Get-Command $_).Source } |
    Select-Object -Unique |
    ForEach-Object { Add-Content -Encoding utf8 $env:GITHUB_PATH $_ }
}
