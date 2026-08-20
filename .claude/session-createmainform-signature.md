# session-createmainform-signature

Work done with Opus 5, 2026-08-20.

## Goal
`TAppData.CreateMainForm` (VCL) took five parameters — slot 3 was `MainFormOnTaskbar: Boolean`, slot 4 `Show: Boolean`, slot 5 `AutoState`. The FMX twin's slot 3 is `AutoState`. A DPR line copied between frameworks compiled clean and silently degraded `AutoState` to `asPosOnly`. Fix chosen: DELETE the two Booleans (they only wrote `Application.MainFormOnTaskbar` / `Application.ShowMainForm`, which are Application-wide, not per-form), making the VCL signature identical to FMX.

New signature:
```pascal
CreateMainForm(aClass: TFormClass;                AutoState: TAutoState= asPosOnly); overload;
CreateMainForm(aClass: TFormClass; OUT Reference; AutoState: TAutoState= asPosOnly); overload;
```

## Why it is safe
- RTL defaults are IDENTICAL to the old parameter defaults: `FMainFormOnTaskBar := False` (`c:\Delphi\Delphi 13\source\vcl\Vcl.Forms.pas:12316`), `FShowMainForm := True` (`:12311`). A call that never passed them is unchanged.
- No stale call can silently survive: a `Boolean` cannot bind to `TAutoState`, and a constant cannot bind to an untyped `OUT`. Every old 3/4/5-argument call is now a compile error.
- `CreateMainForm` now READS `Application.ShowMainForm` (instead of writing it) to decide whether to call `Form.Show`. Same observable behaviour.

## Done
- `FrameVCL\LightVcl.Visual.AppData.pas` — both overloads, both implementations, unit header, doc comments. Header date bumped to 2026.08.20. Backup at `D:\Projects\LightSaber\2026.08.20 - 13.08`.
- Swept **86 DPR files** across `c:\Projects` (72 got `Application.MainFormOnTaskbar:= TRUE;`, 6 got `Application.ShowMainForm:= FALSE;`, 1 already set the flag itself). Scripts kept: `scratchpad\sweep.py`, `addforms.py`, `fixcomments.py`.
- **18 DPRs also needed `Vcl.Forms` added to their uses clause** — they never named `Application` before. This is the one real trap of this refactor; a DPR that delegates everything to AppData has no reason to have `Vcl.Forms`.
- Fixed 5 stale comments that named the removed parameters.
- Docs: `LightSaber\CLAUDE.md` (new section), `c:\Projects\CLAUDE.md` (AppData init pattern), `Docs\Skins-VCL.md:20`, `LightProteus\KeyGen SciVance v832\CLAUDE.md`, and the now-obsolete trap entry in `Project Support\HandOver.md`.
- `_Index.md` regenerated (2922 routines, 218 units) — it now lists both frameworks with matching shapes at `:1230` and `:3055-3056`.
- `Demo\Core\Demo LightCore IO\Demo_ccIO.dproj` — added `..\..\..\External\` to `DCC_UnitSearchPath` (pre-existing breakage, `VclUtilsExt` not found, unrelated to this change).

## Compiled — 11 projects clean, via the light-compiler agent
LightSaber: `VCL_TemplateSimple`, `VCL_TemplateFull` (1 expected madExcept hint), `VCL_TemplateMicro`, `Demo_ccIO` (1 pre-existing W1058 in the demo's own `MainForm.pas:182`), `VCL_Uninstaller`, `UpdateDemo`, `VCL_Demo_VisualControls`, `VCL_Demo_AutoTranslator`.
Consumers outside the repo: `Digger`, `TextUnwrap`, `BlizzardDeScrewer` — all 0 errors 0 warnings 0 hints.

Two pre-existing failures, neither caused by this change:
- `KeepItAlive.dproj` — its `DCC_UnitSearchPath` points at `c:\MyProjects\Packages\LightSaber\`, which does not exist. `F2613 Unit 'LightVcl.Visual.AppData' not found`, i.e. it fails before reaching any call. NOT fixed.
- `VclUtilsExt` missing from `DCC_UnitSearchPath` — hit `Demo_ccIO.dproj` and `VCL_Demo_AutoTranslator.dproj`; added `External\` to both. Already a known trap (`Project Support\HandOver.md`).

## Not done / open
- **Compile coverage is a spot-check, not proof.** ~75 of the 86 swept DPRs were never built — BioniX, LightProteus, most of the tools. The edits are uniform, and the `Vcl.Forms` fix ran over the whole swept list, so the residual risk is low but not zero.
- `CreateForm` was deliberately NOT changed — its slot 3 is still `Show: Boolean` while FMX's is `aAutoState`. Its `Show` is genuinely per-form, so removing it is a separate decision. `CreateFormHidden` already covers `Show=FALSE`.
- The two `FMX\VCL to FMX converter\_OLD code\...\Light_VCL2FMX.dpr` copies still hold the old bug. They reference `cbAppDataFmx`, a unit that no longer exists, so they cannot compile either way. Left alone.
- Still open from the previous session: `AddLibraryPaths.dpr` and `Fix_SetFocus.dpr` leave `AutoState` at the `asPosOnly` default. Deliberate or the same slip? Gabriel's call — changing it to `asFull` changes what those tools persist.
