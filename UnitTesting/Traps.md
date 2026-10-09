# Traps - the LightSaber test suite only

**The general DUnitX and Delphi traps moved on 2026-10-08 to `C:\Users\trei\.claude\skills\light-review-DUnitX\references\traps.md`** (an assertion that checks nothing, `Assert.Pass` versus `FailsOnNoAsserts`, the leak count that is always 0, FastMM logs, FMX and VCL quirks). Read that file first. This file holds only what is true of THIS suite. The traps of the LIBRARY stay in [..\HandOver.md](../HandOver.md), under `## Traps`.

How to build and run the suite is in [..\CLAUDE.md](../CLAUDE.md); what makes a test fake is in `c:\Projects\CLAUDE.md`, section "Unit Testing".

**The four rules every test here must follow** (the `[Ignore]` attribute for a test that shows a window, `FailsOnNoAsserts`, the FastMM log, the BioniX project that links the `Test.LightVcl.Visual.*` units) are in [CLAUDE.md](CLAUDE.md) in this folder.

- **`Tests_LightVcl.Forms.dpr` must NOT `FreeAndNil(MainForm)`** - `Application` owns it, so the test project double-frees it on shutdown.
- **An FMX test project must NOT get `--define:FRAMEWORK_VCL`.**
- **Only `Tests_LightVcl.Visual.dpr` and `Tests_LightFmx.dpr` hold `{$R *.res}`** (version 1.2.3.4 in their `.dproj`, since 2026-10-08); the other runners have no version resource.
- **The topmost tests of `Test.LightCore.Win.Window.pas` probe Windows first**, because Windows cleared `WS_EX_TOPMOST` in 7 of 40 runs (10 runs after the change, 0 failed).
- **`GetSysFileTime` falls through to `c:\pagefile.sys`** for a normal user, because the hive `C:\Windows\system32\config\software` is invisible to it.
- **The fake-test job's records** (2026-10-07..08): `..\_Unit testing conclusiosn\Fake-test fixes 2026-10\`.
