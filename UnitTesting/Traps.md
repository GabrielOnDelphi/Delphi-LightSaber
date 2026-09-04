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
- **Required defines are NOT stored in the `.dproj` — pass via `--define`.** All `Tests_LightVcl.*` projects need `FRAMEWORK_VCL`. FMX test projects must NOT get it. Package projects need `--property=ProductVersion=37.0` instead, or their DCU files land in `_Win32_Debug` rather than `37.0_Win32_Debug`.
