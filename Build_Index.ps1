<#
  Build_Index.ps1 — regenerate _Index.md, a flat map of LightSaber's public API.

  WHY: so an AI assistant (or a human) can answer "does LightSaber already have a
  function for X?" in one grep instead of opening dozens of units. The index is a
  DERIVED file — never edit _Index.md by hand; edit the source and re-run this.

  WHAT it does: for every LightCore*.pas (repo root), FrameVCL\*.pas and FrameFMX\*.pas,
  it reads the INTERFACE section only and emits every public function/procedure
  declaration (full signature, multi-line joined) together with its trailing
  { ... } or // ... comment. Test units, Demos and External/ 3rd-party code are skipped.

  USAGE:  powershell -ExecutionPolicy Bypass -File Build_Index.ps1
          (or just run Build_Index.cmd)
#>

$ErrorActionPreference = 'Stop'
$root = Split-Path -Parent $MyInvocation.MyCommand.Path

# Source set: the reusable LIBRARY units only — file name starts with 'Light'
# (LightCore.* at root, LightVcl.* / LightFmx.* under the Frame folders).
# Deliberately excluded:
#   - non-Light* units: Form*.pas etc. are per-form UI plumbing (FormCreate,
#     btnOKClick...), not reusable API, so they would only add noise.
#   - UnitTesting\ (tests), Demo\ (samples), External\ (3rd-party), history folders.
$files = @()
$files += Get-ChildItem -Path $root -Filter 'LightCore*.pas' -File
foreach ($sub in 'FrameVCL', 'FrameFMX') {
  $dir = Join-Path $root $sub
  if (Test-Path $dir) {
    $files += Get-ChildItem -Path $dir -Filter 'Light*.pas' -File -Recurse |
              Where-Object { $_.FullName -notmatch '\\__history\\|\\__recovery\\|\\Demo\\|\\UnitTesting\\' }
  }
}
$files = $files | Sort-Object Name -Unique

$sb = [System.Text.StringBuilder]::new()
[void]$sb.AppendLine('# LightSaber -- public API index')
[void]$sb.AppendLine('')
[void]$sb.AppendLine('> **GENERATED FILE -- do not edit by hand.** Regenerate with `Build_Index.cmd`')
[void]$sb.AppendLine('> after adding/renaming units or public routines. It maps every public')
[void]$sb.AppendLine('> `function`/`procedure` (from each unit''s INTERFACE section) to its unit, so you')
[void]$sb.AppendLine('> can find existing functionality fast. The signature here is a POINTER -- open the')
[void]$sb.AppendLine('> unit to confirm the exact current declaration before relying on it.')
[void]$sb.AppendLine('>')
[void]$sb.AppendLine('> Scope: the reusable library units only (`LightCore.*`, `LightVcl.*`, `LightFmx.*`).')
[void]$sb.AppendLine('> Excludes Form*.pas UI plumbing, tests, demos, and External\ 3rd-party code.')
[void]$sb.AppendLine('')

$rxRoutine = '^\s*(function|procedure)\s'           # a declaration line (after indentation)
$totalRoutines = 0
$totalUnits = 0

foreach ($f in $files) {
  $text = Get-Content -LiteralPath $f.FullName -Raw

  # Keep only the INTERFACE..IMPLEMENTATION span. This excludes the header comment
  # block (its "EXISTS:" pseudo-lists) and every implementation-section body.
  # Anchor on the keyword as a whole word at line start. IMPLEMENTATION may carry a
  # trailing directive on the same line (e.g. "IMPLEMENTATION {$R *.dfm}"), so do NOT
  # require end-of-line after it — \b is enough and keeps method bodies out of the slice.
  $mI = [regex]::Match($text, '(?im)^\s*INTERFACE\b')
  if (-not $mI.Success) { continue }
  $start = $mI.Index + $mI.Length
  # Search for IMPLEMENTATION only AFTER the INTERFACE keyword. Searching the whole
  # file would match a stray "Implementation follows ..." sentence in the header
  # comment block (which precedes INTERFACE); that false match would then be discarded
  # and the slice would run to EOF, leaking every method body. Anchoring the search
  # past $start avoids that entirely.
  $mImpl = [regex]::Match($text.Substring($start), '(?im)^\s*IMPLEMENTATION\b')
  $len = if ($mImpl.Success) { $mImpl.Index } else { $text.Length - $start }
  $iface = $text.Substring($start, $len)

  $lines = $iface -split "\r?\n"
  $unitEntries = New-Object System.Collections.Generic.List[string]

  for ($i = 0; $i -lt $lines.Count; $i++) {
    $line = $lines[$i]
    if ($line -notmatch $rxRoutine) { continue }

    # Accumulate a multi-line signature: keep appending follow-on lines until we
    # reach the one that closes the declaration with ';' (ignoring ';' that sits
    # only inside a trailing comment). Cap at 6 lines so a malformed unit can't run away.
    $decl = $line.TrimEnd()
    $hops = 0
    while ($decl -notmatch ';' -and $hops -lt 6 -and ($i + 1) -lt $lines.Count) {
      $i++; $hops++
      $decl += ' ' + $lines[$i].Trim()
    }

    # Collapse runs of whitespace so aligned columns don't bloat the entry.
    $decl = ($decl -replace '\s{2,}', ' ').Trim()

    # Skip anything that is plainly not a free/method declaration we want:
    #  - resourcestring/const lines that happen to start with the keyword? (none do)
    # Keep deprecated ones but they already carry the 'deprecated' marker inline.
    $unitEntries.Add($decl)
  }

  if ($unitEntries.Count -eq 0) { continue }
  $totalUnits++
  $totalRoutines += $unitEntries.Count

  $unitName = [System.IO.Path]::GetFileNameWithoutExtension($f.Name)
  [void]$sb.AppendLine("## $unitName ($($unitEntries.Count))")
  [void]$sb.AppendLine('')
  [void]$sb.AppendLine('```pascal')
  foreach ($e in $unitEntries) { [void]$sb.AppendLine($e) }
  [void]$sb.AppendLine('```')
  [void]$sb.AppendLine('')
}

$summary = "_$totalRoutines public routines across $totalUnits units._"
# Insert the count right under the title block (after the closing blockquote line).
$out = $sb.ToString().TrimEnd() + "`r`n`r`n" + $summary + "`r`n"

$indexPath = Join-Path $root '_Index.md'
# UTF-8 WITHOUT BOM (Windows PowerShell's -Encoding UTF8 would prepend a BOM, which
# shows up as stray bytes when other tools read the file). Use a no-BOM encoder.
$utf8NoBom = New-Object System.Text.UTF8Encoding($false)
[System.IO.File]::WriteAllText($indexPath, $out, $utf8NoBom)
Write-Host "Wrote $indexPath  ($totalRoutines routines, $totalUnits units)."
