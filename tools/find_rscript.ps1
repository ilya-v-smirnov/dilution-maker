# ==============================================================================
# find_rscript.ps1  (used by run_app.bat)
# Prints the path to Rscript.exe for this project: the R version recorded in
# renv.lock if it is installed, otherwise the newest installed R.
# Looks in the registry (R installer), the default install folders and PATH.
# ==============================================================================
param([string]$LockFile = "renv.lock")
$ErrorActionPreference = "SilentlyContinue"

$homes = @()

# R installer records every installed version in the registry
foreach ($root in "HKLM:\SOFTWARE\R-core\R", "HKLM:\SOFTWARE\R-core\R64",
                  "HKCU:\SOFTWARE\R-core\R", "HKCU:\SOFTWARE\R-core\R64") {
  foreach ($key in Get-ChildItem $root) {
    $p = (Get-ItemProperty $key.PSPath).InstallPath
    if ($p) { $homes += $p }
  }
}

# Default install folders (all users / current user)
foreach ($base in "$env:ProgramFiles\R", "$env:LOCALAPPDATA\Programs\R") {
  foreach ($d in Get-ChildItem $base -Directory -Filter "R-*") { $homes += $d.FullName }
}

# Rscript on PATH (...\R-x.y.z\bin\Rscript.exe or ...\bin\x64\Rscript.exe)
$cmd = Get-Command Rscript.exe
if ($cmd -and ($cmd.Source -match '^(.*\\R-\d+\.\d+\.\d+)\\')) { $homes += $Matches[1] }

$found = foreach ($h in ($homes | ForEach-Object { $_.TrimEnd('\') } | Select-Object -Unique)) {
  $exe = Join-Path $h "bin\Rscript.exe"
  if ((Test-Path $exe) -and ($h -match 'R-(\d+\.\d+\.\d+)$')) {
    [pscustomobject]@{ Exe = $exe; Version = [version]$Matches[1] }
  }
}
if (-not $found) { exit 1 }

$pick = $null
if (Test-Path $LockFile) {
  $locked = (Get-Content $LockFile -Raw | ConvertFrom-Json).R.Version
  if ($locked) {
    $pick = $found | Where-Object { $_.Version -eq [version]$locked } | Select-Object -First 1
  }
}
if (-not $pick) {
  $pick = $found | Sort-Object Version -Descending | Select-Object -First 1
}
Write-Output $pick.Exe
