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
- **`FRAMEWORK_VCL` needs NO `--define`.** Every `Tests_LightVcl.*` `.dproj` sets `<FrameworkType>VCL</FrameworkType>`, and `c:\Delphi\Delphi 13\bin\CodeGear.Delphi.Targets:151` appends `FRAMEWORK_$(FrameworkType)` to `DCC_Define` by itself. Passing it anyway is harmless; FMX test projects must NOT get it. Package projects need `--property=ProductVersion=37.0`, or their DCU files land in `_Win32_Debug` rather than `37.0_Win32_Debug`.
