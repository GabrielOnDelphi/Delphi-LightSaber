# Traps — writing and building the DUnitX tests

Mistakes already paid for in this test suite. Read before writing a new `[Test]` or wiring a new test
project. The traps that apply to the LIBRARY rather than to the tests stay in
[..\HandOver.md](../HandOver.md), under `## Traps`.

How to build and run the suite is in [..\CLAUDE.md](../CLAUDE.md); what makes a test fake is in the
"Fake Test Prevention" section of the same file.

- **`TForm.Create(nil)` raises `EResNotFound` in FMX.** `InitInheritedComponent` returns FALSE when no ancestor has a `.fmx` resource — use `CreateNew`.
- **`FMX.Objects` declares its own `TPath`**, shadowing `System.IOUtils.TPath` when it comes later in the uses clause. Symptom: `E2003 Undeclared identifier: 'Combine'`.
- **`Assert.WillRaise` matches the exception class EXACTLY** (`DUnitX.Assert.pas:1165`). Naming an ancestor such as `Exception` always fails when the code raises a descendant. Use the real class, `WillRaiseDescendant`, or NIL for "any exception".
- **Never use the `[WillRaise]` ATTRIBUTE in DUnitX here**, and always pass the message argument to `Assert.WillRaise`/`WillNotRaise`. The attribute makes DUnitX build a `TDUnitXExceptionTest`, and a run holding one leaks its whole fixture tree — measured: two attributes produced a leak dump of about 1371 objects with every test passing.
- **Inside a DUnitX test unit the intrinsic `Assert()` is shadowed by DUnitX's `Assert` CLASS.** `Assert(X = NIL, 'msg')` fails to parse with a misleading `E2029`. Use `Assert.IsNull`, or qualify as `System.Assert`.
- **`EInOutArgumentException` does NOT descend from `EInOutError`** — it descends from `EArgumentException` (`System.SysUtils.pas:516`).
- **A failed generic inference poisons overload resolution for the REST of the unit.** Two `E2532` on `Assert.AreEqual<T>` produced eight bogus `E2250` further down the same file. Fix the FIRST error and re-measure.
- **`Tests_LightVcl.Forms.dpr` must NOT `FreeAndNil(MainForm)`** — `Application` owns it, so the test project double-frees it on shutdown.
- **Check unit-to-project linkage before trusting a green build.** Three FMX units are linked by zero projects — `FormAbout.pas`, `FormUpdaterNotifier.pas`, `FormUpdaterSettings.pas` — so their fixes cannot be compile-verified at all.
- **A green run with a FastMM log beside the test EXE is NOT green.** In the Debug config every test `.dpr` links FastMM4 in FullDebugMode, which writes each leak and each write into freed memory to `<exe name>_MemoryManager_EventLog.txt` in the EXE's folder (for example `UnitTesting\Tests_LightVcl.Graphics_MemoryManager_EventLog.txt`); the `Tests Failed / Errored / Leaked` lines do not count them (measured: a scratch test that leaked and wrote into freed memory still PASSED). Build with `--property=DCC_MapFile=3` to get unit, routine and line names in its stack traces instead of bare addresses. FastMM deletes the file at every start, so it describes the last run only. The first console line must read `FastMM4 4.993: installed`; `NOT INSTALLED` means `FastMM_FullDebugMode.dll` (from `c:\Delphi\Delphi 13\bin`, on PATH) was not found and nothing was checked.
- ⚠ **`Assert.WillNotRaise(proc, Exception)` checks almost nothing**: it fails only when the raised class is EXACTLY `Exception` and silently passes an access violation or any other descendant (`c:\Delphi\Delphi 13\source\DunitX\DUnitX.Assert.pas:1190`). Write `Assert.WillNotRaiseAny`, or `WillNotRaise(proc)` with the default class `nil` (line 208). Counted 2026-10-07: 44 such calls in 17 units of `UnitTesting\`, listed in section 5.2 of `..\_Unit testing conclusiosn\Fake-test audit 2026-10-07.md`. `Assert.WillRaise(proc, E)` is the opposite: an EXACT class match (line 1173 → 1317), and so is its method-pointer overload (lines 1199-1207 call the same routine).
- ⚠ **`runner.FailsOnNoAsserts := TRUE` never catches an `Assert.Pass`-only test**: `Assert.Pass` raises `ETestPass`, which escapes before the assert-count check and is recorded as a success (`c:\Delphi\Delphi 13\source\DunitX\DUnitX.TestRunner.pas:758-762`, `:824-825`). The guard is TRUE only in `Tests_LightVcl.Common.dpr:123` and `Tests_LightVcl.Forms.dpr:126`; the other 5 runners set FALSE, with no reason given.
- ⚠ **DUnitX's `Tests Leaked` line is always 0 in these test projects**: its default monitor returns 0 for every measurement (`c:\Delphi\Delphi 13\source\DunitX\DUnitX.MemoryLeakMonitor.Default.pas:106`, `:111`, `:116`). A leak shows only in the FastMM log (the trap above).
- **DUnitX writes `dunitx-results.xml` beside the executable**, so a second test run overwrites the file the previous run left in `c:\Projects\LightSaber\UnitTesting\`. Pass `--xmlfile:<path>` to keep a result, or restore it from a backup if a verification run clobbered it.
- **Run a new test on the OLD code first and see it fail.** The prompt of 2026-10-05 proposed checking `Result.Canvas.HandleAllocated = FALSE` after `ExtractThumbnailJpg`; that test passes on any code, because `StretchProport` → `StretchF` reads `BMP.Handle`, which frees the canvas DC before the routine returns (found 2026-10-06).
- **`FRAMEWORK_VCL` needs NO `--define`.** Every `Tests_LightVcl.*` `.dproj` sets `<FrameworkType>VCL</FrameworkType>`, and `c:\Delphi\Delphi 13\bin\CodeGear.Delphi.Targets:151` appends `FRAMEWORK_$(FrameworkType)` to `DCC_Define` by itself. Passing it anyway is harmless; FMX test projects must NOT get it. Package projects need `--property=ProductVersion=37.0`, or their DCU files land in `_Win32_Debug` rather than `37.0_Win32_Debug`.

Added at the close of the fake-test job (2026-10-08; records in `..\_Unit testing conclusiosn\Fake-test fixes 2026-10\`):

- **Since 2026-10-08 `FailsOnNoAsserts` is TRUE in all 7 runners** `UnitTesting\Tests_*.dpr` (the trap above about `Assert.Pass` still holds).
- **A 0x0 window from `AllocateHWnd` does not get `WS_EX_TOPMOST`** from `SetWindowPos(HWND_TOPMOST)`, although the call returns TRUE. Measured, not explained. Size it (for example 100x100) first.
- **Windows can clear `WS_EX_TOPMOST` a few seconds into the life of the process** (seen in 7 of 40 runs as `ExStyle=00000080`). The topmost tests of `Test.LightCore.Win.Window.pas` therefore probe Windows first (10 runs after the change, 0 failed).
- **FMX `RegisterComponents` always raises `EComponentError` outside the IDE** (`c:\Delphi\Delphi 13\source\rtl\common\System.Classes.pas:4381-4384`). A test cannot call it.
- **`GetVersionEx` reports build 9200 to the test EXE**: its manifest has no Windows 10 entry, so Windows lies about its version. Never assert the real build number through it.
- **`TSocket.Receive(Buffer[0], N)` binds to the open-array overload and reads nothing.** Pass the flags argument `[]` to reach the untyped-buffer overload.
- **A listening `TSocket` raises from `shutdown()` when it is freed.** Call `Close(TRUE)` before `FreeAndNil`.
- **The hive `C:\Windows\system32\config\software` is invisible to a normal user**, so `GetSysFileTime` falls through to `c:\pagefile.sys`. A test must not expect the hive's date.
- **GDI `FillRect` on a `pf32bit` `TBitmap` leaves the alpha byte 0**, so GR32 treats the image as fully transparent. Set the alpha yourself in test input meant for GR32.
- **`TStyleManager.Enabled` is FALSE in the console test EXE.** A test of a style-dependent branch reaches only the unstyled path.
- **`TActivityIndicator` ignores `Width` and `Height`** assigned in code. Do not assert a size you assigned.
- **An Edit restore (after a mutation proof) can drop padding spaces or a line break in the product line.** Run `git diff --stat` on the product file after each restore; it must show no change.
- **The `light-compiler` agent once reported success without relinking `Tests_LightCore.exe`.** Check the time stamp of the test EXE before trusting a run after a build.
- **A test EXE has a version resource only if its runner holds `{$R *.res}`** - `VerInfo_IncludeVerInfo` in the `.dproj` alone is not enough. Since 2026-10-08 `Tests_LightVcl.Visual.dpr` and `Tests_LightFmx.dpr` have it (version 1.2.3.4 in their `.dproj`); the other runners still have none.
- **The `Test.LightVcl.Visual.*` units are also linked by `C:\Projects\BioniX\SourceCode\BioniX VCL\UnitTesting\Tests_BioniX.dproj`.** Never hard-code a value only the LightSaber test EXE has (its name, its version); read it at run time, for example `GetFileVersionInfo` on `ParamStr(0)`.
