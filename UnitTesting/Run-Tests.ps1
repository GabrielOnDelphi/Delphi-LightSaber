# WHAT: runs every DUnitX test EXE of LightSaber, one after the other, and says whether the run is green (Rule 7 of the testing rules).
# CALLER: a person or a Claude session after the test projects were built through the light-compiler agent; a release script before it ships.
# It writes no source file: only UnitTesting\Test runs.log, the run output in C:\Projects\LightSaber\Temp\, and what the test EXEs write themselves.
#
# It only RUNS the test EXEs. It never builds them: build each test .dproj through the light-compiler agent first.
# The EXEs it runs:
#   UnitTesting\Tests_LightCore.exe, Tests_LightVcl.Common.exe, Tests_LightVcl.Internet.exe, Tests_LightVcl.Graphics.exe,
#   Tests_LightVcl.Visual.exe, Tests_LightVcl.Forms.exe, Tests_LightFmx.exe,
#   FrameVCL\AutoTranslator\UnitTesting\Tests_LightVcl.Translator.exe,
#   and, only with -Live, FrameVCL\AutoTranslator\UnitTesting\Tests_LightVcl.TranslatorLive.exe
#   (it calls the real DeepL server when a key is set: DEEPL_API_KEY, or %APPDATA%\LightSaber\DeepL.key).
#   Not run: UnitTesting\FtpLiveTest.exe (a live FTP test against the production server) and the stale
#   UnitTesting\Tests_LightVcl.Translator.dpr (it cannot build; the real one is in FrameVCL\AutoTranslator\UnitTesting\).
#
# A run FAILS when, for any EXE:
#   - the EXE is missing, or prints no "Tests Found" line, or runs longer than -TimeoutMinutes;
#   - "Tests Failed" or "Tests Errored" is not 0;
#   - the EXE's exit code is not 0 (1 = a test failed or the runner raised; 2 = a fixture folder is missing;
#     100 = a bad command-line option). The fixture check is in each runner: UnitTesting\Test.RunnerChecks.pas;
#   - FastMM4 left <exe>_MemoryManager_EventLog.txt beside the EXE (a leak or a heap error), or printed "FastMM4: NOT INSTALLED".
#     ONE exception, the known RTL leak of System.Devices: c:\Delphi\Delphi 13\source\rtl\common\System.Devices.pas:351
#     (TDeviceInfo.SelectDevices, called on every FMX form load by c:\Delphi\Delphi 13\source\fmx\FMX.Forms.pas:4076)
#     overwrites FThisDevice without freeing the old TDeviceInfo, and each one owns a TDictionary<string,string>.
#     A log is accepted when EVERY block in it is a leak of System.Devices.TDeviceInfo or of
#     System.Generics.Collections.TDictionary<System.string,System.string> whose allocation stack holds a [System.Devices] frame,
#     and the summary names only those two classes. Anything else in the log (another class, the same class allocated
#     elsewhere, a medium or large block, a heap error) fails the run. The rule: function Test-FastMMLog below;
#   - with -Release: the EXE is older than the newest unit its .dpr names (without -Release that is only a WARNING).
# Before each EXE runs, an old <exe>_MemoryManager_EventLog.txt is recycled, so an old log cannot fail a clean run.
# For a failed or errored test the script prints the test's full name and message, from the "Failing Tests" and
# "Tests With Errors" sections that DUnitX writes at the end of its output (c:\Delphi\Delphi 13\source\DunitX\DUnitX.Loggers.Console.pas).
# Each EXE appends one line to UnitTesting\Test runs.log: date and time, EXE, found, passed, failed, errored, FastMM log yes/no/rtl-only, exit code.
# The full console output of each EXE goes to C:\Projects\LightSaber\Temp\Test run - <exe name>.txt.
#
# Parameters:
#   -Live                    also run Tests_LightVcl.TranslatorLive.exe
#   -Release                 an EXE older than its source fails the run (use it before a release)
#   -AllowMissingFixtures    pass --allow-missing-fixtures to every EXE (a missing fixture folder then does not fail)
#   -TimeoutMinutes <n>      per EXE, default 20
#   -CheckFastMMLog <path>   run no EXE: only judge one FastMM log file by the rule above, print the verdict, exit 0 (accepted) or 1
#
# Exit codes of this script: 0 = every EXE green; 1 = at least one EXE failed (the lines starting with FAIL say why).
#
# Example: powershell -NoProfile -ExecutionPolicy Bypass -File "C:\Projects\LightSaber\UnitTesting\Run-Tests.ps1"

param(
    [switch]$Live,
    [switch]$Release,
    [switch]$AllowMissingFixtures,
    [int]$TimeoutMinutes = 20,
    [string]$CheckFastMMLog = ''
)

$ErrorActionPreference = 'Stop'

$UnitTesting = $PSScriptRoot
$Root        = Split-Path $UnitTesting -Parent
$Translator  = Join-Path $Root 'FrameVCL\AutoTranslator\UnitTesting'
$TempFolder  = Join-Path $Root 'Temp'
$RunLog      = Join-Path $UnitTesting 'Test runs.log'
$RecycleBin  = 'c:\AI\Tools\System\RecycleBin.exe'

$Projects = @(
    (Join-Path $UnitTesting 'Tests_LightCore.dpr'),
    (Join-Path $UnitTesting 'Tests_LightVcl.Common.dpr'),
    (Join-Path $UnitTesting 'Tests_LightVcl.Internet.dpr'),
    (Join-Path $UnitTesting 'Tests_LightVcl.Graphics.dpr'),
    (Join-Path $UnitTesting 'Tests_LightVcl.Visual.dpr'),
    (Join-Path $UnitTesting 'Tests_LightVcl.Forms.dpr'),
    (Join-Path $UnitTesting 'Tests_LightFmx.dpr'),
    (Join-Path $Translator  'Tests_LightVcl.Translator.dpr')
)
if ($Live) { $Projects += (Join-Path $Translator 'Tests_LightVcl.TranslatorLive.dpr') }

if (-not (Test-Path $TempFolder)) { New-Item -ItemType Directory -Path $TempFolder | Out-Null }


# The newest of the .dpr itself and every unit it names with  in '<path>.pas'
function Get-NewestSource([string]$DprPath) {
    $folder = Split-Path $DprPath -Parent
    $newest = Get-Item $DprPath
    foreach ($m in [regex]::Matches((Get-Content $DprPath -Raw), "\bin\s+'([^']+\.pas)'", 'IgnoreCase')) {
        $unit = Join-Path $folder $m.Groups[1].Value
        if (Test-Path $unit) {
            $item = Get-Item $unit
            if ($item.LastWriteTime -gt $newest.LastWriteTime) { $newest = $item }
        }
    }
    return $newest
}

# The number after "Tests <Name> :" in the console output, or $null when the line is missing
function Get-Count([string[]]$Lines, [string]$Name) {
    foreach ($line in $Lines) {
        if ($line -match "^\s*Tests $Name\s*:\s*(\d+)") { return [int]$Matches[1] }
    }
    return $null
}

# Judges a FastMM4 event log. Returns '' when the log holds only the known System.Devices RTL leak (see the header),
# else the first reason why it does not. Unknown content is never accepted.
function Test-FastMMLog([string]$LogPath) {
    $knownClasses = @('System.Devices.TDeviceInfo', 'System.Generics.Collections.TDictionary<System.string,System.string>')
    $lines = @(Get-Content $LogPath)

    # Cut the log into sections at the separator lines  ----...2026/11/2 21:45:33...----
    $sections = New-Object System.Collections.Generic.List[object]
    $current  = New-Object System.Collections.Generic.List[string]
    foreach ($line in $lines) {
        if ($line -match '^-{10,}.*-{10,}\s*$') {
            if ($current.Count -gt 0) { $sections.Add($current.ToArray()) }
            $current = New-Object System.Collections.Generic.List[string]
        }
        else { $current.Add($line) }
    }
    if ($current.Count -gt 0) { $sections.Add($current.ToArray()) }

    $blockCount = @{}   # class -> number of accepted leak blocks
    $summary    = @{}   # class -> number the summary names
    $sawSummary = $false

    foreach ($sec in $sections) {
        $text = @($sec | Where-Object { $_.Trim() -ne '' })
        if ($text.Count -eq 0) { continue }

        if ($sec -match 'A memory block has been leaked') {
            $alloc     = ($sec | Where-Object { $_ -match '^The allocation number is:' } | Select-Object -First 1)
            $stackIdx  = -1
            $classIdx  = -1
            for ($i = 0; $i -lt $sec.Count; $i++) {
                if ($stackIdx -lt 0 -and $sec[$i] -match 'stack trace \(return addresses\)') { $stackIdx = $i }
                if ($sec[$i] -match '^The block is currently used for an object of class:') { $classIdx = $i; break }
            }
            if ($classIdx -lt 0) { return "a leaked block that is not an object ($alloc)" }
            $class = ($sec[$classIdx] -replace '^The block is currently used for an object of class:\s*', '').Trim()
            if ($knownClasses -notcontains $class) { return "a leaked $class block ($alloc)" }
            if ($stackIdx -lt 0) { return "a leaked $class block with no allocation stack ($alloc)" }
            # Only the allocation stack counts: the class line itself contains the text System.Devices
            $stack = $sec[($stackIdx + 1)..($classIdx - 1)]
            if (-not ($stack -match '\[System\.Devices\]')) { return "a leaked $class block allocated outside System.Devices ($alloc)" }
            $blockCount[$class] = 1 + [int]$blockCount[$class]
        }
        elseif ($sec -match 'This application has leaked memory') {
            $sawSummary = $true
            foreach ($line in $text) {
                if ($line -match '^This application has leaked memory' -or $line -match '^Note: ') { continue }
                if ($line -notmatch '^\s*\d+ - \d+ bytes:\s*(.+)$') { return "the summary names something else: $line" }
                foreach ($item in ($Matches[1] -split ', ')) {
                    if ($item -notmatch '^(.+) x (\d+)\s*$') { return "the summary line cannot be read: $line" }
                    $class = $Matches[1].Trim()
                    if ($knownClasses -notcontains $class) { return "the summary names a leaked $class" }
                    $summary[$class] = [int]$Matches[2] + [int]$summary[$class]
                }
            }
        }
        else { return "a FastMM event that is not a leak: $($text[0])" }
    }

    if ($blockCount.Count -eq 0) { return 'the log holds no leak block' }
    if (-not $sawSummary)        { return 'the log has no leak summary' }
    foreach ($class in $knownClasses) {
        if ([int]$blockCount[$class] -ne [int]$summary[$class]) {
            return "the summary counts $([int]$summary[$class]) x $class but the log details $([int]$blockCount[$class])"
        }
    }
    return ''
}

if ($CheckFastMMLog -ne '') {
    if (-not (Test-Path $CheckFastMMLog)) { Write-Host "FAIL: $CheckFastMMLog not found"; exit 1 }
    $reason = Test-FastMMLog $CheckFastMMLog
    if ($reason -eq '') { Write-Host "ACCEPTED: $CheckFastMMLog holds only the known System.Devices RTL leak"; exit 0 }
    Write-Host "REJECTED: $CheckFastMMLog - $reason"
    exit 1
}


$Failures  = New-Object System.Collections.Generic.List[string]
$Warnings  = New-Object System.Collections.Generic.List[string]
$FailedExe = New-Object System.Collections.Generic.HashSet[string]   # One EXE can add two FAIL lines (stale, then failed tests); the RESULT line counts EXEs

foreach ($dpr in $Projects) {
    $name    = [IO.Path]::GetFileNameWithoutExtension($dpr)
    $exe     = [IO.Path]::ChangeExtension($dpr, '.exe')
    $fastLog = Join-Path (Split-Path $exe -Parent) ($name + '_MemoryManager_EventLog.txt')
    $outFile = Join-Path $TempFolder ("Test run - $name.txt")
    $errFile = Join-Path $TempFolder ("Test run - $name - stderr.txt")

    Write-Host ''
    Write-Host "=== $name"

    if (-not (Test-Path $exe)) {
        $Failures.Add("FAIL ${name}: $exe not found - build $([IO.Path]::ChangeExtension($dpr, '.dproj')) first")
        $null = $FailedExe.Add($name)
        Write-Host $Failures[$Failures.Count - 1]
        continue
    }

    # Is the EXE older than its source?
    $newest = Get-NewestSource $dpr
    $exeTime = (Get-Item $exe).LastWriteTime
    if ($newest.LastWriteTime -gt $exeTime) {
        $msg = "${name}: the EXE ($($exeTime.ToString('yyyy-MM-dd HH:mm:ss'))) is older than $($newest.FullName) ($($newest.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss'))) - rebuild it"
        if ($Release) { $Failures.Add("FAIL $msg"); $null = $FailedExe.Add($name); Write-Host "FAIL $msg" }
        else          { $Warnings.Add("WARNING $msg"); Write-Host "WARNING $msg" }
    }

    # An old FastMM log must not fail a clean run
    if (Test-Path $fastLog) { & $RecycleBin $fastLog | Out-Null }

    $exeArgs = @()
    if ($AllowMissingFixtures) { $exeArgs += '--allow-missing-fixtures' }

    $startParams = @{
        FilePath               = $exe
        WorkingDirectory       = (Split-Path $exe -Parent)
        RedirectStandardOutput = $outFile
        RedirectStandardError  = $errFile
        NoNewWindow            = $true
        PassThru               = $true
    }
    if ($exeArgs.Count -gt 0) { $startParams.ArgumentList = $exeArgs }

    $proc = Start-Process @startParams
    $null = $proc.Handle   # Without this read, ExitCode stays empty after WaitForExit
    if (-not $proc.WaitForExit($TimeoutMinutes * 60 * 1000)) {
        $proc.Kill()
        $Failures.Add("FAIL ${name}: still running after $TimeoutMinutes minutes - killed")
        $null = $FailedExe.Add($name)
        Write-Host $Failures[$Failures.Count - 1]
        continue
    }
    $exitCode = $proc.ExitCode

    $lines   = @(Get-Content $outFile -ErrorAction SilentlyContinue)
    $found   = Get-Count $lines 'Found'
    $passed  = Get-Count $lines 'Passed'
    $ignored = Get-Count $lines 'Ignored'
    $failed  = Get-Count $lines 'Failed'
    $errored = Get-Count $lines 'Errored'
    $hasFastLog = Test-Path $fastLog
    $fastState  = 'no'
    $fastReason = ''
    if ($hasFastLog) {
        $fastReason = Test-FastMMLog $fastLog
        if ($fastReason -eq '') { $fastState = 'rtl-only' } else { $fastState = 'yes' }
    }

    $inFailList = $false
    foreach ($line in $lines) {
        if ($line -match '^\s*Tests (Found|Ignored|Passed|Failed|Errored)\s*:' -or $line -match '^MISSING FIXTURE' -or $line -match '^FastMM4') { Write-Host "  $line" }
        # The names and messages of the failed and errored tests, from the end of the DUnitX output
        if ($line -match '^(Failing Tests|Tests With Errors)\s*$') { $inFailList = $true; Write-Host "  $line"; continue }
        if ($inFailList -and $line.Trim() -ne '') { Write-Host "  $line" }
    }

    $why = @()
    if ($null -eq $found)                         { $why += 'no "Tests Found" line - the runner stopped before the tests ran' }
    if ($failed  -gt 0)                           { $why += "$failed failed" }
    if ($errored -gt 0)                           { $why += "$errored errored" }
    if ($fastState -eq 'yes')                     { $why += "FastMM log $fastLog ($fastReason)" }
    if ($fastState -eq 'rtl-only')                { Write-Host "  NOTE: FastMM log $fastLog holds only the known System.Devices RTL leak - accepted" }
    if ($lines -match 'FastMM4: NOT INSTALLED')   { $why += 'FastMM4 not installed - leaks were not checked' }
    if ($exitCode -ne 0) {
        switch ($exitCode) {
            1       { $why += 'exit code 1 (a test failed, or the runner raised an exception)' }
            2       { $why += 'exit code 2 (a fixture folder is missing - see the MISSING FIXTURE lines)' }
            100     { $why += 'exit code 100 (a bad command-line option)' }
            default { $why += "exit code $exitCode" }
        }
    }

    if ($why.Count -gt 0) {
        $Failures.Add("FAIL ${name}: " + ($why -join '; ') + "  (output: $outFile)")
        $null = $FailedExe.Add($name)
        Write-Host $Failures[$Failures.Count - 1]
    }
    else {
        Write-Host "  OK"
    }

    $logLine = '{0} | {1} | found {2} | passed {3} | ignored {4} | failed {5} | errored {6} | FastMM log {7} | exit code {8}' -f `
        (Get-Date -Format 'yyyy-MM-dd HH:mm:ss'), "$name.exe", $found, $passed, $ignored, $failed, $errored, $fastState, $exitCode
    Add-Content -Path $RunLog -Value $logLine -Encoding UTF8
}

Write-Host ''
foreach ($w in $Warnings) { Write-Host $w }
if ($Failures.Count -gt 0) {
    Write-Host "RESULT: RED - $($FailedExe.Count) of $($Projects.Count) test EXEs failed:"
    foreach ($f in $Failures) { Write-Host "  $f" }
    exit 1
}
Write-Host "RESULT: GREEN - $($Projects.Count) of $($Projects.Count) test EXEs, 0 failed, 0 errored, no FastMM log except the known System.Devices RTL leak"
exit 0
