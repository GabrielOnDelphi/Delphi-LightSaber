<#
  Measure-ReplaceFile.ps1

  Calls kernel32!ReplaceFileW from PowerShell in the cases that the Microsoft page does not state
  (https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-replacefilew),
  and prints the return value, GetLastError and the files left after each call.
  Cited by C:\Projects\LightSaber\LightCore.INIFileMem.pas.

  Each run writes into a NEW folder under -RunRoot. The script deletes nothing.
  Cases 1-7 show what one call does. Case 8 repeats "write the temp file, then swap it in at once" -Loops times,
  because an antivirus that scans a file right after it is written might block the swap.
  Windows Defender skips C:\Projects on this PC, so pass a -RunRoot outside it (C:\test delete\...) to see the scan.

  Run:  powershell -NoProfile -File "C:\Projects\LightSaber\Docs\ReplaceFile - measured behaviour\Measure-ReplaceFile.ps1" [-RunRoot <folder>] [-Loops <n>]
#>
param(
  [string]$RunRoot = 'C:\Projects\LightSaber\Temp\ReplaceFile measure',
  [int]$Loops = 300
)

$ErrorActionPreference = 'Stop'

Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;
public static class K32Replace {
  [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true, EntryPoint = "ReplaceFileW")]
  public static extern bool ReplaceWithBackup(string replaced, string replacement, string backup, uint flags, IntPtr exclude, IntPtr reserved);

  [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true, EntryPoint = "ReplaceFileW")]
  public static extern bool ReplaceNoBackup(string replaced, string replacement, IntPtr backup, uint flags, IntPtr exclude, IntPtr reserved);
}
'@

$REPLACEFILE_IGNORE_MERGE_ERRORS = [uint32]2

$run = Join-Path $RunRoot (Get-Date -Format 'yyyy.MM.dd - HH.mm.ss')
New-Item -ItemType Directory -Path $run | Out-Null
"Run folder: $run"
"Windows: " + [Environment]::OSVersion.VersionString
""

function New-CaseFolder([string]$Name) {
  $dir = Join-Path $run $Name
  New-Item -ItemType Directory -Path $dir | Out-Null
  return $dir
}

function Write-Text([string]$Path, [string]$Text) {
  [IO.File]::WriteAllText($Path, $Text)
}

function Show-State([string]$Dir) {
  $files = @(Get-ChildItem -LiteralPath $Dir -Force)
  if ($files.Count -eq 0) { "    (folder is empty)"; return }
  foreach ($f in $files) {
    "    {0,-16} content='{1}'  attributes={2}" -f $f.Name, [IO.File]::ReadAllText($f.FullName), $f.Attributes
  }
}

function Invoke-Replace([string]$Dir, [bool]$WithBackup) {
  $target      = Join-Path $Dir 'target.ini'
  $replacement = Join-Path $Dir 'target.ini.tmp'
  $backup      = Join-Path $Dir 'target.ini.bak'
  if ($WithBackup) {
    $ok  = [K32Replace]::ReplaceWithBackup($target, $replacement, $backup, $REPLACEFILE_IGNORE_MERGE_ERRORS, [IntPtr]::Zero, [IntPtr]::Zero)
    $err = [Runtime.InteropServices.Marshal]::GetLastWin32Error()
  } else {
    $ok  = [K32Replace]::ReplaceNoBackup($target, $replacement, [IntPtr]::Zero, $REPLACEFILE_IGNORE_MERGE_ERRORS, [IntPtr]::Zero, [IntPtr]::Zero)
    $err = [Runtime.InteropServices.Marshal]::GetLastWin32Error()
  }
  if ($ok) { "  ReplaceFileW returned TRUE" }
  else     { "  ReplaceFileW returned FALSE, GetLastError = $err" }
}


"CASE 1 - the file to replace does not exist (first save), backup name given"
$d = New-CaseFolder 'case1'
Write-Text (Join-Path $d 'target.ini.tmp') 'NEW'
"  Before:"; Show-State $d
Invoke-Replace $d $true
"  After:";  Show-State $d
""

"CASE 2 - the file to replace does not exist (first save), no backup name"
$d = New-CaseFolder 'case2'
Write-Text (Join-Path $d 'target.ini.tmp') 'NEW'
"  Before:"; Show-State $d
Invoke-Replace $d $false
"  After:";  Show-State $d
""

"CASE 3 - normal save: the file to replace exists, no backup file yet"
$d = New-CaseFolder 'case3'
Write-Text (Join-Path $d 'target.ini')     'OLD'
Write-Text (Join-Path $d 'target.ini.tmp') 'NEW'
"  Before:"; Show-State $d
Invoke-Replace $d $true
"  After:";  Show-State $d
""

"CASE 4 - a backup file already exists (left by an earlier save)"
$d = New-CaseFolder 'case4'
Write-Text (Join-Path $d 'target.ini')     'OLD'
Write-Text (Join-Path $d 'target.ini.tmp') 'NEW'
Write-Text (Join-Path $d 'target.ini.bak') 'STALE'
"  Before:"; Show-State $d
Invoke-Replace $d $true
"  After:";  Show-State $d
""

"CASE 5 - the file to replace is read-only, backup name given"
$d = New-CaseFolder 'case5'
$t = Join-Path $d 'target.ini'
Write-Text $t                              'OLD'
Write-Text (Join-Path $d 'target.ini.tmp') 'NEW'
(Get-Item -LiteralPath $t).IsReadOnly = $true
"  Before:"; Show-State $d
Invoke-Replace $d $true
"  After:";  Show-State $d
foreach ($f in Get-ChildItem -LiteralPath $d -Force) { $f.IsReadOnly = $false }   # So a later clean-up can recycle the folder
""

"CASE 6 - the file to replace is held open by another handle (read access, share mode READ only)"
$d = New-CaseFolder 'case6'
$t = Join-Path $d 'target.ini'
Write-Text $t                              'OLD'
Write-Text (Join-Path $d 'target.ini.tmp') 'NEW'
$lock = [IO.File]::Open($t, [IO.FileMode]::Open, [IO.FileAccess]::Read, [IO.FileShare]::Read)
try {
  "  Before:"; Show-State $d
  Invoke-Replace $d $true
} finally {
  $lock.Dispose()
}
"  After (handle closed):"; Show-State $d
""

"CASE 7 - a backup file already exists and is read-only"
$d = New-CaseFolder 'case7'
$b = Join-Path $d 'target.ini.bak'
Write-Text (Join-Path $d 'target.ini')     'OLD'
Write-Text (Join-Path $d 'target.ini.tmp') 'NEW'
Write-Text $b                              'STALE'
(Get-Item -LiteralPath $b).IsReadOnly = $true
"  Before:"; Show-State $d
Invoke-Replace $d $true
"  After:";  Show-State $d
foreach ($f in Get-ChildItem -LiteralPath $d -Force) { $f.IsReadOnly = $false }
""

"CASE 8 - write the temp file, then swap it in at once, $Loops times (first loop: rename, because no target exists yet)"
$d = New-CaseFolder 'case8'
$target      = Join-Path $d 'target.ini'
$replacement = Join-Path $d 'target.ini.tmp'
$backup      = Join-Path $d 'target.ini.bak'
$fails = @{}
$watch = [Diagnostics.Stopwatch]::StartNew()
for ($i = 1; $i -le $Loops; $i++) {
  Write-Text $replacement ("[S]`r`nK=$i`r`n" + ('x' * 8000))
  if (-not (Test-Path -LiteralPath $target)) {
    [IO.File]::Move($replacement, $target)
    continue
  }
  $ok  = [K32Replace]::ReplaceWithBackup($target, $replacement, $backup, $REPLACEFILE_IGNORE_MERGE_ERRORS, [IntPtr]::Zero, [IntPtr]::Zero)
  $err = [Runtime.InteropServices.Marshal]::GetLastWin32Error()
  if (-not $ok) { $fails[$err] = 1 + [int]$fails[$err] }
}
$watch.Stop()
"  Folder: $d"
"  Time: {0} ms for {1} loops ({2:N3} ms per loop)" -f $watch.ElapsedMilliseconds, $Loops, ($watch.ElapsedMilliseconds / $Loops)
if ($fails.Count -eq 0) { "  Failed swaps: 0" }
else { foreach ($k in $fails.Keys) { "  Failed swaps with GetLastError = {0}: {1}" -f $k, $fails[$k] } }
"  Last value in the target: " + (([IO.File]::ReadAllText($target)) -split "`r`n")[1]
