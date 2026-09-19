#Requires -Version 7

<#
.SYNOPSIS
  Stages mediacopy3000.exe with everything it needs at run time, then builds an MSI.

.DESCRIPTION
  scripts/package.sh does this job for Linux and macOS. That file is a POSIX shell
  script, so Windows gets its own. The two share no code.

  The staged tree is the contract with packaging/windows/mediacopy3000.wxs:

    dist-package/windows/
      mediacopy3000.exe
      *.dll                                      the closure, minus C:\Windows
      lib/gdk-pixbuf-2.0/2.10.0/loaders.cache
      lib/gdk-pixbuf-2.0/2.10.0/loaders/*.dll
      share/glib-2.0/schemas/gschemas.compiled
      share/icons/{Adwaita,hicolor}/...
      etc/fonts/...
      share/mediacopy3000/assets/{styles.css,themes/}

.EXAMPLE
  ./scripts/package.ps1 -Version 0.1.0
#>

param(
    [string]$Version = '0.0.0',
    [string]$Ucrt,
    [string]$Stage = 'dist-package/windows',
    [switch]$SkipMsi
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

if (-not $Ucrt) {
    $candidates = @(
        [Environment]::GetEnvironmentVariable('GHCUP_MSYS2')
        'C:\ghcup\msys64'
        'C:\msys64'
    ) | Where-Object { $_ } | ForEach-Object { $_.TrimEnd('\', '/') + '\ucrt64' }
    $Ucrt = $candidates | Where-Object { Test-Path $_ } | Select-Object -First 1
    if (-not $Ucrt) { throw "no MSYS2 UCRT64 tree found; pass -Ucrt" }
    Write-Host "using the MSYS2 UCRT64 tree at $Ucrt"
}

function Copy-Into {
    param([Parameter(Mandatory)][string]$Source, [Parameter(Mandatory)][string]$Destination)
    $parent = Split-Path -Parent $Destination
    if ($parent -and -not (Test-Path $parent)) {
        New-Item -ItemType Directory -Path $parent -Force | Out-Null
    }
    Copy-Item -Path $Source -Destination $Destination -Recurse -Force
}

function Assert-Tool {
    param([Parameter(Mandatory)][string]$Path)
    if (-not (Test-Path $Path)) { throw "missing tool: $Path" }
}

# --- stage -------------------------------------------------------------------

if (Test-Path $Stage) { Remove-Item -Recurse -Force $Stage }
New-Item -ItemType Directory -Path $Stage -Force | Out-Null

$found = Get-ChildItem -Recurse -Filter mediacopy3000.exe dist-newstyle -ErrorAction SilentlyContinue |
    Select-Object -First 1
if (-not $found) { throw 'mediacopy3000.exe not found under dist-newstyle; run cabal build first' }
$exe = $found.FullName
Copy-Into $exe "$Stage/mediacopy3000.exe"

# The closure of DLLs, minus the ones Windows itself provides.
# ntldd -R prints lines of the form "name => path (base)"; the path is what we want.
Assert-Tool "$Ucrt\bin\ntldd.exe"
$dlls = & "$Ucrt\bin\ntldd.exe" -R $exe |
    ForEach-Object { if ($_ -match '=>\s+(\S.*?)\s+\(') { $Matches[1] } } |
    Where-Object { $_ -and $_ -notmatch '^[A-Za-z]:\\[Ww][Ii][Nn][Dd][Oo][Ww][Ss]\\' } |
    Sort-Object -Unique
foreach ($dll in $dlls) {
    if (Test-Path $dll) { Copy-Into $dll "$Stage/$(Split-Path -Leaf $dll)" }
}
Write-Host "bundled $($dlls.Count) DLLs"
if ($dlls.Count -lt 20) { throw "only $($dlls.Count) DLLs resolved; ntldd output was not understood" }

# gdk-pixbuf loaders, and the cache that names them.
$loaders = "$Stage/lib/gdk-pixbuf-2.0/2.10.0/loaders"
New-Item -ItemType Directory -Path $loaders -Force | Out-Null
Copy-Item "$Ucrt/lib/gdk-pixbuf-2.0/2.10.0/loaders/*.dll" $loaders -Force
Assert-Tool "$Ucrt\bin\gdk-pixbuf-query-loaders.exe"
& "$Ucrt\bin\gdk-pixbuf-query-loaders.exe" |
    Out-File -FilePath "$Stage/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache" -Encoding ascii

# GSettings schemas.
Copy-Into "$Ucrt/share/glib-2.0/schemas/gschemas.compiled" `
    "$Stage/share/glib-2.0/schemas/gschemas.compiled"

# Icon themes, with their caches.
foreach ($theme in 'Adwaita', 'hicolor') {
    Copy-Into "$Ucrt/share/icons/$theme" "$Stage/share/icons/$theme"
    & "$Ucrt\bin\gtk4-update-icon-cache.exe" --force --quiet "$Stage/share/icons/$theme"
}

# fontconfig.
Copy-Into "$Ucrt/etc/fonts" "$Stage/etc/fonts"

# The application's own data. resolveAsset in src/gtk/MediaCopy/Gtk/Assets.hs asks for the
# path "assets/styles.css", so the data directory must hold an "assets" directory.
Copy-Into 'assets/styles.css' "$Stage/share/mediacopy3000/assets/styles.css"
Copy-Into 'assets/themes' "$Stage/share/mediacopy3000/assets/themes"

Write-Host "staged into $Stage"
if ($SkipMsi) { return }

# --- harvest -----------------------------------------------------------------

# The staged tree holds a few hundred files, so the component list is generated.
# The shape is nested Directory elements under INSTALLFOLDER, one Component per file,
# and a ComponentGroup of ComponentRefs. That construct is stable across WiX 3, 4 and 5.

$fragmentPath = 'packaging/windows/staged-files.wxs'
$stageFull = (Resolve-Path $Stage).Path
$lines = [System.Collections.Generic.List[string]]::new()
$refs = [System.Collections.Generic.List[string]]::new()
$script:componentIndex = 0

function Write-Directory {
    param(
        [Parameter(Mandatory)][string]$Path,
        [Parameter(Mandatory)][int]$Depth
    )
    $pad = ' ' * (($Depth + 3) * 2)

    foreach ($file in Get-ChildItem -LiteralPath $Path -File | Sort-Object Name) {
        $script:componentIndex++
        $id = "cmp$($script:componentIndex)"
        $refs.Add($id)
        $lines.Add("$pad<Component Id=`"$id`" Guid=`"*`">")
        $lines.Add("$pad  <File Id=`"fil$($script:componentIndex)`" Source=`"$($file.FullName)`" KeyPath=`"yes`" />")
        $lines.Add("$pad</Component>")
    }

    foreach ($dir in Get-ChildItem -LiteralPath $Path -Directory | Sort-Object Name) {
        $script:componentIndex++
        $dirId = "dir$($script:componentIndex)"
        $lines.Add("$pad<Directory Id=`"$dirId`" Name=`"$($dir.Name)`">")
        Write-Directory -Path $dir.FullName -Depth ($Depth + 1)
        $lines.Add("$pad</Directory>")
    }
}

Write-Directory -Path $stageFull -Depth 0

$fragment = @()
$fragment += '<?xml version="1.0" encoding="utf-8"?>'
$fragment += '<!-- Generated by scripts/package.ps1. Do not edit. -->'
$fragment += '<Wix xmlns="http://wixtoolset.org/schemas/v4/wxs">'
$fragment += '  <Fragment>'
$fragment += '    <DirectoryRef Id="INSTALLFOLDER">'
$fragment += $lines
$fragment += '    </DirectoryRef>'
$fragment += '  </Fragment>'
$fragment += '  <Fragment>'
$fragment += '    <ComponentGroup Id="StagedFiles">'
foreach ($id in $refs) { $fragment += "      <ComponentRef Id=`"$id`" />" }
$fragment += '    </ComponentGroup>'
$fragment += '  </Fragment>'
$fragment += '</Wix>'
$fragment | Out-File -FilePath $fragmentPath -Encoding utf8
Write-Host "harvested $($refs.Count) files into $fragmentPath"

# --- build -------------------------------------------------------------------

# WiX 7 is a .NET tool. The runner image carries only WiX 3.14, so install it.
dotnet tool install --global wix --version 7.0.0
if ($LASTEXITCODE -ne 0) {
    dotnet tool update --global wix --version 7.0.0
    if ($LASTEXITCODE -ne 0) { throw 'could not install the wix tool' }
}

New-Item -ItemType Directory -Path 'dist-package/out' -Force | Out-Null
$msi = "dist-package/out/mediacopy3000-$Version-x64.msi"

wix build `
    --acceptEula wix7 `
    -d Version=$Version `
    -arch x64 `
    -out $msi `
    packaging/windows/mediacopy3000.wxs `
    $fragmentPath
if ($LASTEXITCODE -ne 0) { throw "wix build failed with $LASTEXITCODE" }

Write-Host "wrote $msi"
