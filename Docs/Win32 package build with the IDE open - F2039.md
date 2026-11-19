# Win32 package build with the Delphi IDE open - F2039

The rule (in `C:\Projects\CLAUDE.md` and `C:\Projects\Biology\CLAUDE.md`): close the IDE before a Win32 package build; Win64 builds with the IDE open. The failed build is harmless - repair nothing, close the IDE and build Win32 again, never kill `bds.exe`. This file holds why.

**Only the Win32 build fails with the IDE open. Both were run, not reasoned (2026-11-01, `C:\Projects\Biology\!BioControl Lib\BioControl.dproj` Debug, same `bds.exe` session):**
- Win32: `status: error`, exit code 1, after 0.8 s:
  ```
  F2039 Could not create output file 'C:\Users\Public\Documents\Embarcadero\Studio\37.0\Bpl\LightCore370.bpl'
  ```
- Win64: exit code 0, no errors; `Bpl\Win64\LightCore370.bpl` and `Bpl\Win64\BioControl370.bpl` got new timestamps.

## Why the Win32 build fails

- `delphi-compiler.exe` runs MSBuild with **`/t:rebuild`** on every compile (`Target := 'rebuild'` in `TMSBuildRunner.BuildCommandLine`, `C:\Delphi\AI Delphi compiler\Compilar.MSBuild.pas`) - that is Clean+Build, with no `/t:Make` mode.
- `C:\Projects\LightSaber\LightCore.dpk:31` and `C:\Projects\Biology\!BioControl Lib\BioControl.dpk:32` are both `{$IMPLICITBUILD ON}`, so a rebuild implicitly rebuilds `LightCore370.bpl` first.
- `C:\Delphi\Delphi 13\bin\bds.exe` is a 32-bit process (`IsWow64Process` = TRUE). Its module list holds `C:\Users\Public\Documents\Embarcadero\Studio\37.0\Bpl\LightCore370.bpl`, loaded because the installed `LightVcl.*` and `LightFmx*` packages require it (`LightCore370.bpl` itself is not in `HKCU\Software\Embarcadero\BDS\37.0\Known Packages`). So the IDE holds the Win32 BPL open and the link fails. The stale `LightCore370.bpl.old` in the Bpl folder is the fingerprint of this happening before.
- Why Win64 is free: neither `.dproj` sets `DCC_BplOutput`, so MSBuild takes `DelphiDLLOutputPath` from `C:\Users\trei\AppData\Roaming\Embarcadero\BDS\37.0\EnvOptions.proj`: `$(BDSCOMMONDIR)\Bpl` for Win32 (line 126), `$(BDSCOMMONDIR)\Bpl\$(Platform)` for Win64 (line 186). A 32-bit IDE cannot load the 64-bit BPLs in `Bpl\Win64\`, so nothing holds them.
- "The BPL is not installed in the IDE" is not the reason: `BioControl370.bpl` is in Known Packages but also in `Disabled Packages` and not loaded, and the Win32 build dies on `LightCore370.bpl` before it reaches BioControl.

To list the BPLs the IDE has loaded, run `(Get-Process bds).Modules` from 32-bit PowerShell (`C:\Windows\SysWOW64\WindowsPowerShell\v1.0\powershell.exe`): 64-bit PowerShell sees only 7 WOW64 modules of a 32-bit process.

## Why the failed build is harmless

It dies inside LightCore's implicit build, before BioControl's own link: after a confirmed F2039, `BioControl370.bpl` still carried its previous timestamp, untouched. Only intermediate DCUs were rewritten. On 2026-11-01 both Win32 BPLs kept their 2026-10-31 timestamps after the F2039. So the fix is simply to close the IDE and build Win32 again. Never kill `bds.exe`.

## Tried, and still open

One dead end, so nobody re-derives it: the old escape hatch was `Build.cmd` with `/t:Make` (incremental, no implicit package rebuild). Deleted, and the agent cannot reproduce it.

One open lead for Win32, **untested**: the agent's `--test` flag. It writes to `C:\Delphi\AI Delphi compiler\Temp\TestBuilds\<PID>` (`TestScratchDir` in `C:\Delphi\AI Delphi compiler\Compilar.Types.pas`; an older note said `W:\temp\compilar`, which was upstream code, replaced locally on 2026-08-11). It also sends `DCC_BplOutput` and `DCC_DcpOutput` there (the test-mode `ExtraProps` in `TMSBuildRunner.BuildCommandLine`, `Compilar.MSBuild.pas`), so the implicit rebuild of `LightCore370.bpl` may write into the scratch folder instead of the locked Bpl folder. Not yet tried with the IDE open (on 2026-10-07 the IDE was not running).
