# VCL skins / styles — the single source of truth

This file replaces the long comment blocks that used to live in `FrameVCL\FormSkinsDisk.pas` and `FrameVCL\FormSkinsRes.pas`. Those units now carry a short header pointing here.

Every claim below carries a `file:line` citation. RTL citations are against `c:\Delphi\Delphi 13\source\` (Delphi 13.1) and were re-verified 2026-08-06.

---

## 1. The ordering contract

**`FormSkinsDisk.LoadLastStyle` must run while `Application.MainForm` is still NIL — i.e. BEFORE `AppData.CreateMainForm`.**

The unit enforces this with a hard `RAISE` (`FrameVCL\FormSkinsDisk.pas:146-147`), not an `Assert` — assertions are compiled out in Release, and this failure mode ships silently.

Correct DPR shape:

```delphi
AppData:= TAppData.Create(AppName, '', MultiThreaded);
LoadLastStyle('Light AmethystKamri.vsf');    // <-- here. Empty string = default Windows theme
AppData.CreateMainForm(TMainForm, MainForm, FALSE, TRUE, asFull);
AppData.Run;
```

Live example: `Demo\VCL\Template App Full\VCL_TemplateFull.dpr:38-44`.

### What breaks if it runs later

`TStyleManager.SetStyle` walks `Screen.Forms` and broadcasts `CM_CUSTOMSTYLECHANGED` to every form with an allocated handle (`Vcl.Themes.pas:6134-6139`):

```delphi
for I := 0 to Screen.FormCount - 1 do
  if Screen.Forms[I].HandleAllocated and (Screen.Forms[I].FormStyle <> fsMDIChild)  then
    if IsWindowVisible(Screen.Forms[I].Handle) then
      PostMessage(Screen.Forms[I].Handle, CM_CUSTOMSTYLECHANGED, 0, 0)
    else
      SendMessage(Screen.Forms[I].Handle, CM_CUSTOMSTYLECHANGED, 0, 0);
```

**That `PostMessage` / `SendMessage` asymmetry is the whole story.** A VISIBLE form gets the message queued; an INVISIBLE one is hit synchronously, inside the `SetStyle` call itself.

The receiving form answers with a handle recreation (`Vcl.Forms.pas:6554-6558`):

```delphi
if not ((StyleElements = []) or (StyleName = TStyleManager.SystemStyleName)) then
begin
  Perform(WM_SETREDRAW, Winapi.Windows.WPARAM(LongBool(False)), 0);
  Perform(CM_RECREATEWND, 0, 0);
end;
```

For an ordinary form that condition is TRUE, so the recreation always happens:
- `StyleElements` defaults to `[seFont, seClient, seBorder]` — not `[]`.
- `TControl.GetStyleName` returns `FStyleName`, and walks to the Parent only when it is empty and a Parent exists (`Vcl.Controls.pas:9115-9121`). A top-level form has no Parent, so it returns `''`, which is never equal to `TStyleManager.SystemStyleName` = `cSystemStyleName` = `'Windows'` (`Vcl.Themes.pas:1762`). `TCustomForm` does not override `GetStyleName`.

`CM_RECREATEWND` lands in `TWinControl.CMRecreateWnd`, which calls `DestroyHandle` (`Vcl.Controls.pas:10759-10771`). **The window handle is replaced.** Three consequences, in the order they bite:

1. **The main menu style hook leaks and crashes later.** See [BUG 5](#bug-5--tmainmenubarstylehook-leak-on-any-live-setstyle) — this is the shipped BioniX v15.30 crash.
2. **Anything bound to the old handle is silently lost.** `DragAcceptFiles` is the type case: the VCL never re-registers it — `DragAcceptFiles` has **zero** hits in the whole of `c:\Delphi\Delphi 13\source\vcl\`. Drag & drop just stops working, with no error.
3. **The queued `WM_POSTINIT` dies with the handle.** `TAppData.CreateMainForm` posts it to the main form (`FrameVCL\LightVcl.Visual.AppData.pas:393`) to defer `FormPostInitialize` until the message loop runs. With `Application.ShowMainForm := FALSE` the form is invisible, so the broadcast takes the `SendMessage` branch and the handle dies **synchronously** — before the loop ever pumps. `FormPostInitialize` never fires, and everything it was supposed to initialize stays NIL.

### And the error is invisible

An exception raised BEFORE `CreateMainForm` is visible. One raised after, in an app that set `Application.ShowMainForm := FALSE`, is owned by the still-hidden main form, so the box **never appears** — the app hangs in a modal loop while looking alive (window up, process Responding = TRUE). Documented at `FrameVCL\LightVcl.Visual.AppData.pas:361`; it bit DnaBaser on Win64 in 2026-07.

That is also why the guard is unconditional and placed first, before the INI is even read: guarding only the real `SetStyle` call would let the misuse ship and fail in the field, on the first machine where a user actually picks a skin.

---

## 2. `FormCreate` vs `FormPostInitialize` — the single most misunderstood rule

**`Application.MainForm` is NIL during the main form's `FormCreate`, and assigned by the time `FormPostInitialize` runs.**

`TApplication.CreateForm` runs the entire constructor at `Vcl.Forms.pas:13571` and only assigns `FMainForm` at `:13582`, after the constructor returned:

```delphi
    Instance := TComponent(InstanceClass.NewInstance);
    TComponent(Reference) := Instance;
    try
      Instance.Create(Self);          // <-- :13571  FormCreate runs in here. FMainForm still NIL
    ...
    if (FMainForm = nil) and (Instance is TForm) then
    begin
      TForm(Instance).HandleNeeded;
      FMainForm := TForm(Instance);   // <-- :13582  only now
```

`FormPostInitialize` runs much later: `TAppData.CreateMainForm` posts `WM_POSTINIT` (`FrameVCL\LightVcl.Visual.AppData.pas:393`, constant at `FrameVCL\LightVcl.Visual.AppDataForm.pas:72`), and `TLightForm.WMPostInit` calls `FormPostInitialize` once the message loop is pumping (`FrameVCL\LightVcl.Visual.AppDataForm.pas:142-157`).

Consequences:

| Called from | `Application.MainForm` | `FormSkinsDisk.LoadLastStyle` |
|---|---|---|
| DPR, before `CreateMainForm` | NIL | **correct** |
| main form's `FormCreate` | NIL | passes the guard, but see the caveat below |
| `FormPostInitialize` | assigned | **raises** |
| any button / menu handler | assigned | **raises** |

This is exactly why `Demo\VCL\Template App Full` broke: its `uInitialization.LateInitialization` is called from `FormPostInitialize`, so the skin load had to move out into the DPR (`Demo\VCL\Template App Full\uInitialization.pas:43` now says so). DevChat, Dict my EnRo and My transistors never broke — they call it from `FormCreate`.

**Caveat for the `FormCreate` case.** The guard passes there, but the form is already being constructed. If anything earlier in `FormCreate` touched `Handle` (allocating the window), `SetStyle` would find it in `Screen.Forms` and recreate it. Both surviving consumers call `LoadLastStyle` before their first `Handle` reference (DevChat: `FormMain.pas:131`, then `DragAcceptFiles(Handle, ...)` at `:132`). It works, but it works by ordering luck inside one procedure. **New code puts the call in the DPR.**

---

## 3. The three units — three DIFFERENT contracts. Do not unify them

| Unit | Styles come from | INI key | Value stored | Guard? | Contract |
|---|---|---|---|---|---|
| `FrameVCL\FormSkinsDisk.pas` | `.vsf` files in `AppSysDir\Skins\` | `LastStyle` (falls back to `LastSkin`) | **filename** (`'Auric 9.vsf'`) | **raises** if `Application.MainForm <> NIL` | call before `AppData.CreateMainForm` |
| `FrameVCL\FormSkinsRes.pas` | styles linked into the EXE via `{$R *.vsf}` | `LastSkin` | style **NAME** (`'Auric'`) | none | classic `ShowMainForm := FALSE` → `CreateForm` → `LoadLastStyle` → `Show` |
| `FrameFMX\FormSkinsDisk.pas` | `.style` / `.fsf` files | `LastStyle` | filename | none needed | FMX defers form creation until `Application.Run` |

Both VCL units export a procedure called `LoadLastStyle` and both declare a `DefWinTheme` constant with the same text — but they are **not** interchangeable. Passing `'Carbon.vsf'` to the resource loader, or `'Carbon'` to the disk loader, silently loads nothing.

### Why `FormSkinsRes` has no guard, and must not get one

Its consumers call `LoadLastStyle` from inside the main form. QuickSilver JpgCompressor calls it from `FormPostInitialize` (`c:\Projects\Projects GRAPHICS\Project QuickSilver Viewer\module JpgCompressor\FormMain.pas:248`), where `Application.MainForm` is already assigned — the guard would raise at startup. And because that app sets `Application.ShowMainForm := FALSE`, the error box would be owned by the hidden main form and never appear (see §1).

It also uses `TrySetStyle`, not `SetStyle` (`FrameVCL\FormSkinsRes.pas:105`). `SetStyle(Name)` raises `ECustomStyleException` when the name is unknown (`Vcl.Themes.pas:6149-6153`); with resource-linked styles that happens the moment a build drops a `.vsf` that a user had already selected. `TrySetStyle(Name, FALSE)` keeps the default theme instead of killing startup.

`FormSkinsDisk` deliberately still calls `SetStyle` (`FrameVCL\FormSkinsDisk.pas:121`) — but only after `TStyleManager.IsValidStyle` confirmed the file and gave it the style's real name, so the "not found" case cannot arise.

### Why FMX needs no guard at all

FMX `TApplication.CreateForm` does not construct anything before `Run`: while `FIsRealCreateFormsCalled` is FALSE it only appends to `FCreateForms` (`FMX.Forms.pas:1587-1606`). The real construction happens in `RealCreateForms`, called from the platform service — `FMX.Platform.Win.pas:760` on Windows, `FMX.Platform.Android.pas:502-504` on Android. So at `LoadLastStyle` time no form exists, whatever the DPR looks like.

The FMX unit carries its own, unrelated complication instead: a **poison marker** in the INI (`IniKeyStyleLoading`), armed before the style load and cleared by a 1 ms `TTimer` once the pump starts, so a style file that crashes form streaming is dropped on the next launch (`FrameFMX\FormSkinsDisk.pas:271-324`). That mechanism belongs to FMX only; it is documented in that unit's own header.

---

## 4. Known VCL bugs and design constraints

Moved here from the `FormSkinsDisk.pas` / `FormSkinsRes.pas` headers.

### BUG 1 — `SetStyle` breaks modal z-order

`RecreateWnd` destroys and recreates the modal form's window, and the new window lacks the Windows-level owner relationship that enforces z-order. The dialog ends up **behind** its disabled owner — the app looks frozen.

The old workaround (`Application.ProcessMessages` + `BringToFront`) does not work: `BringToFront` is only `SetWindowPos(HWND_TOP)`, which cannot repair broken window ownership.

- Fix 1 — `PopupMode = pmAuto` in the DFM (`FrameVCL\FormSkinsDisk.dfm:22`, `FrameVCL\FormSkinsRes.dfm:18`). VCL then sets the correct `WndParent` in `CreateParams` during the recreation. Set it at design time; changing it at runtime triggers an extra `RecreateWnd`.
- Fix 2 — `TfrmSkinRes.ReassertZOrder` (`FrameVCL\FormSkinsRes.pas:212-217`): `HWND_TOPMOST` then `HWND_NOTOPMOST` forces Windows to recompute z-order, then `SetForegroundWindow` restores focus.
- Fix 3 — the `DefWinTheme` branch (`SetStyle('Windows')`) also recreates handles and originally had no z-order repair. It now calls `ReassertZOrder` too (`FrameVCL\FormSkinsRes.pas:236`).

In `FormSkinsDisk` this can no longer fire at all: that dialog never calls `SetStyle` (see BUG 5), and `LoadLastStyle` runs before any form exists. `PopupMode = pmAuto` is kept there as a legacy precaution.

Source: https://blogs.embarcadero.com/popupmode-and-popupparent/

### BUG 2 — `IsValidStyle` needs `Vcl.Styles` in USES

`TStyleManager.IsValidStyle` iterates `FStyleClassDescriptors` (`Vcl.Themes.pas:5926-5944`); the `.vsf` descriptor is only registered when `Vcl.Styles` is linked. Without it, every file is reported invalid (XE7+).

Source: http://stackoverflow.com/questions/30328644

### BUG 3 — `caFree` in `FormClose`

Historically unsafe, **fixed in Delphi 11** (RSP-33140). Both selectors use it (`FrameVCL\FormSkinsDisk.pas:190`, `FrameVCL\FormSkinsRes.pas:154`).

### BUG 4 — EAV in `TMainMenuBarStyleHook` on dialog close (historical)

A previous workaround posted a second `CM_RECREATEWND` via `ForceQueue` in `FormPreRelease`. It could fire inside `ProcessMenuLoop`'s `DispatchMessage` loop and destroy the menu hook mid-loop.

Fix: **do not post a second `RecreateWnd`.** The single `CM_CUSTOMSTYLECHANGED` that `SetStyle` already broadcasts is enough.

Related: RSP-38114, RSP-39197 (partially fixed in 11.3; the menu-bar variant persists in 13.1). HeidiSQL #465 hit the same family.

### BUG 5 — `TMainMenuBarStyleHook` leak on any live `SetStyle`

**This is the one that cost months.** Confirmed with FastMM on 2026-05-14/15, Delphi 13.1.

Once a main form owning a `TMainMenu` exists, **every** `SetStyle` leaks a `TMainMenuBarStyleHook`. The leaked hook sits in freed memory while the form's private `FMainMenuBarHook` still points at it. The AV does not fire inside `SetStyle` — it fires later, on the next main-menu click, far away from the cause. One single live `SetStyle` is enough. This is the shipped BioniX v15.30 crash.

`RecreateWnd` does **not** reliably rebuild that hook. An older comment in `FormSkinsRes.pas` claimed it did; that claim was wrong and has been removed.

Two mitigations, and only these two work:

1. **Load the style before the main form exists** — `LoadLastStyle` runs before `CreateMainForm`, so no `TMainMenuBarStyleHook` exists yet. The startup `SetStyle` is safe.
2. **Never call `SetStyle` at runtime.** `TfrmStyleDisk.lBoxClick` only writes the choice to the INI and tells the user to close and reopen the app (`FrameVCL\FormSkinsDisk.pas:266-279`). Same UX as HeidiSQL #465.

Auto-restart via `AppData.Restart` was **rejected**: it races the host app's window-based single-instance check. `FindWindow` on the class name still finds the dying window during finalization, so the new instance resurrects the old one and exits.

`FormSkinsRes` was deliberately **not** converted — it still applies styles live (`FrameVCL\FormSkinsRes.pas:222-249`). Its consumers are small apps without a styled main menu. Do not "fix" it by copying `FormSkinsDisk`'s guard (see §3).

Diagnostic notes: `c:\Delphi\Styles & resources\Known bugs in VCL styles\02 - TMainMenuBarStyleHook use-after-free.md`

---

## 5. INI keys and their history

| Unit | Live key | Read fallback | Written by |
|---|---|---|---|
| `FormSkinsDisk` | `LastStyle` | `LastSkin` (read only, never written) | `lBoxClick` (`:276`), `FormPreRelease` (`:200`) |
| `FormSkinsRes` | `LastSkin` | — | `FormPreRelease` (`:164`) |
| `FrameFMX\FormSkinsDisk` | `LastStyle` | — | `lBoxChange` (`:521`, `:529`), `FormPreRelease` (`:414`) |

Section and file: `LightCore.INIFileQuick` writes into section `AppDataCore.AppName` of `AppDataCore.IniFile` (`LightCore.INIFileQuick.pas:79-99`), i.e. `%AppData%\<AppName>\<AppName>.ini` (`LightCore.AppData.pas:436-441`).

**History.** Commit `03eb0a81` (2026-02-23) renamed `FormSkinsDisk`'s key from `LastSkin` to `LastStyle` **with no fallback**, silently orphaning every skin a user had picked before that date — on upgrade they all reverted to the default. The read fallback (`IniKeyStyleOld`) was added 2026-08-01. We only ever WRITE `LastStyle`, so an old INI migrates forward on the first save.

The two `LastSkin` keys are **not** the same thing: `FormSkinsRes` still uses it as its live key and stores a style NAME, while `FormSkinsDisk` reads it as a legacy key holding a `.vsf` FILENAME. No application uses both units, so the two never meet.

---

## 6. Consumers

`FormSkinsDisk` (disk `.vsf`), called from the DPR before `CreateMainForm` — the correct pattern:

| App | Call site |
|---|---|
| BioniX | `c:\Projects\BioniX\SourceCode\BionixWallpaper.dpr:140` |
| Template App Full | `Demo\VCL\Template App Full\VCL_TemplateFull.dpr:41-42` |
| QuickSilver Viewer | `c:\Projects\Projects GRAPHICS\Project QuickSilver Viewer\QuickSilverMain.dpr:37` |
| OrinocoReader VCL | `c:\Projects\Project OrinocoReader\Frame VCL\Source\OrinocoReaderVCL.dpr:43` |
| DelphiJobs | `c:\Projects\Project DelphiJobs\DelphiPortal.dpr:40-41` |
| LabBook VCL | `c:\Projects\Project LabBook\VCL\LabBook GUI\GUI_VCL.dpr:64-65` |
| Stormy | `c:\Projects\Projects INTERNET\!Stormy web site builder\!Stormy TWebBrowser\StormyWebSite_TWebBrowser.dpr:36` |

`FormSkinsDisk`, called from a form's `FormCreate` — legal (`MainForm` is still NIL) but fragile, see §2:

| App | Call site |
|---|---|
| Claude TokenVampire | `c:\Projects\Projects AI\Claude TokenVampire\Source\FormMain.pas:111` (in `FormCreate`) |
| Cubic VCL tester | `c:\Projects\Testers\Cubic VCL tester GLOBAL\TesterForm.pas:102` (in `FormCreate`) |

`FormSkinsRes` (styles linked into the EXE):

| App | Call site |
|---|---|
| DevChat | `c:\Projects\Projects INTERNET\DevChat\FormMain.pas:131` (in `FormCreate`) |
| Dict my EnRo | `c:\Projects\Projects Text\Util - Dict my EnRo\FormMain.pas:137` (in `FormCreate`) |
| My transistors | `c:\Projects\Projects System\Project My transistors\SourceCode (dupe@)\FormMain.pas:224` (in `FormCreate`) |
| QuickSilver JpgCompressor | `c:\Projects\Projects GRAPHICS\Project QuickSilver Viewer\module JpgCompressor\FormMain.pas:248` (in `FormPostInitialize` — the reason `FormSkinsRes` must stay guard-free) |

Selector-only consumers (they show `TfrmStyleDisk.ShowAsModal` but do not call `LoadLastStyle` themselves): DnaBaser (`Routines.pas:43`), LearnAssist, Text unwrap, BioniX `MainForm.pas` / `FormSettings`, Template App Full `FormSettings.pas`.

**Dead consumer, do not count it:** Power Email Extractor (`MainUnit.pas:258`, in `FormPostInitialize`) still links a `FormSkins.pas` from the LightSaber root — a unit that no longer exists. That project does not build against current LightSaber.

---

## 7. Rules for new code

1. New VCL app with disk skins → call `FormSkinsDisk.LoadLastStyle` in the **DPR**, between `TAppData.Create` and `AppData.CreateMainForm`. Nowhere else.
2. Never call `TStyleManager.SetStyle` after the main form exists. If the user picks a skin at runtime, write the INI and ask for a restart.
3. Never add `FormSkinsDisk`'s guard to `FormSkinsRes`.
4. `Vcl.Themes` and `Vcl.Styles` must be in the DPR USES list **before** `Vcl.Forms`.
5. Neither unit may go into a package — both enforce it with `{$DENYPACKAGEUNIT ON}`.
6. Style-aware code reads colours via `StyleServices.GetStyleColor` / `GetStyleFontColor` / `GetSystemColor` (`Vcl.Themes`), plus `TControl.IsLightStyleColor`.
7. If you must run code after a style change, remember the handle is new: re-register anything bound to the old one (`DragAcceptFiles`, subclassing, `SetWindowLong`).

To link a style into the EXE for `FormSkinsRes`, add a resource directive to the DPR — an `$R` on the `.vsf` (e.g. `Carbon.vsf`) — then pass the style NAME (`'Carbon'`), never the filename, to `LoadLastStyle`. That unit's classic startup flow is: `Application.ShowMainForm := FALSE` → create the main form → `LoadLastStyle` → `MainForm.Show`.

Style folders on this machine: `c:\Delphi\Styles & resources\VCL Styles\`, `c:\Projects\Packages\VCL Styles utils\Styles\`, `c:\Users\Public\Documents\Embarcadero\Studio\XX.0\Styles\`.

External references collected from the old unit headers:
- PopupMode / PopupParent: https://blogs.embarcadero.com/popupmode-and-popupparent/
- Modal form hidden behind its owner: https://www.experts-exchange.com/questions/26286057/Delphi-7-modal-form-hides-behind-window-on-a-Windows-7-box.html
- `IsValidStyle` needs `Vcl.Styles`: http://stackoverflow.com/questions/30328644
- `caFree` / RSP-33140 and how it used to be patched: https://stackoverflow.com/questions/70840792/how-to-patch-vcl-forms-pas
- Changing the style at runtime (Packt): https://subscription.packtpub.com/book/application_development/9781783559589/1/ch01lvl1sec10/changing-the-style-of-your-vcl-application-at-runtime
- Tester projects: `c:\Projects-3rd_Packages\VCL Styles Tools\FrmSkins tester\SkinSettingsTemplate.dpr`, `c:\Projects\Packages\VCL Styles Tools\FrmSkins tester\`

---

## 8. Regression tests

`UnitTesting\Test.FormSkinsDisk.pas` and `UnitTesting\Test.FormSkinsRes.pas`, in `Tests_LightVcl.Forms.dproj` (build via the `light-compiler` agent, needs `--define FRAMEWORK_VCL`).

They cover, per unit:
- the ordering guard — `FormSkinsDisk.LoadLastStyle` raises with a main form alive, and does not when `Application.MainForm` is NIL; `FormSkinsRes.LoadLastStyle` must NOT raise with a main form alive;
- the INI round-trip and the `LastSkin` → `LastStyle` read fallback, including that only the new key is ever written;
- graceful degradation — missing file, invalid file, `DefWinTheme`, empty INI + empty default;
- `TStyleManager.ActiveStyle.Name` actually changing after a successful load — the assertion that proves the style was applied, not merely that no exception fired;
- the selector writing the INI and **not** applying the style live — the BUG 5 regression net;
- the selector preselecting the configured skin (see §9).

The test fixture frees the dummy main form to reach the "MainForm is NIL" state and recreates it afterwards; `TApplication.ControlDestroyed` clears `FMainForm` on destruction (`Vcl.Forms.pas:12477-12479`), which is what makes that possible. For the same reason the test project must NOT free its own main form (`UnitTesting\Tests_LightVcl.Forms.dpr` — `Application` owns it and frees it at shutdown).

**The tests were mutation-proven on 2026-08-06.** Three deliberate defects were injected into `FormSkinsDisk.pas` and the suite rebuilt: removing the ordering guard failed 2 tests, making `lBoxClick` apply the style live failed 1 test (and produced a memory leak at shutdown — BUG 5 showing itself), and removing the legacy-key fallback failed 2 tests. A test that survives its own defect is fake; these did not survive.

### End-to-end run

`Demo\VCL\Template App Full` links the Autopilot bridge (`AUTOPILOT` in the Debug `DCC_Define` only) and was driven over MCP on 2026-08-06. It is the right target because it owns a `TMainMenu`, which is what BUG 5 needs. The scenario, all steps confirmed:

1. Start with `LastStyle=CyanDusk.vsf` → the app comes up in that skin.
2. Open Settings → Skins → pick `Light AmethystKamri.vsf`.
3. The dialog shows the restart prompt, writes `LastStyle=Light AmethystKamri.vsf`, leaves `LastSkin=CyanDusk.vsf` untouched, and the running app **keeps the old skin** — no live `SetStyle`.
4. Restart → the new skin is active.
5. Open the main menu (real `SC_KEYMENU` path, so the styled `TMainMenuBarStyleHook` paints) → no AV; the app then closed cleanly.

## 9. Fixed while writing this file (2026-08-06)

- **The selector never preselected anything.** `PopulateStyles` looked up `TStyleManager.ActiveStyle.Name` (the style's internal name, e.g. `'Cyan Dusk'`) in a list of `.vsf` FILENAMES (`'CyanDusk.vsf'`), so `IndexOf` always returned -1. Confirmed live via Autopilot (`lBox.ItemIndex` read back as `-1` with a skin active). Now matches on `CurrentStyleName` and falls back to entry 0, exactly like the FMX twin (`FrameFMX\FormSkinsDisk.pas:496-498`). Assigning `ItemIndex` cannot fire `OnClick` — `TCustomListBox.SetItemIndex` only sends `LB_SETCURSEL` (`Vcl.StdCtrls.pas:7564-7573`).
- **The dialog's label still said "Click skin to load it"** — untrue since the unit stopped applying styles live. Now "Click a skin to choose it (applied at next start)".
- **`FormSkinsRes.dfm` was missing `PopupMode = pmAuto`** although its own header documented it as BUG 1's Fix 1 — and that unit is the one that actually applies styles live, so it is the one that needs it. Added (`FrameVCL\FormSkinsRes.dfm:18`), matching `FormSkinsDisk.dfm:22`. ⚠ Not verified at runtime: no app in this repo drives `FormSkinsRes`, so the modal z-order path was not reproduced. `ReassertZOrder` (Fix 2) was already present and is the runtime belt-and-braces.
- **`LoadStyleFromFile` could abort startup.** `TStyleManager.IsValidStyle` opens a `TFileStream` (`Vcl.Themes.pas:5926-5944`), so a locked or truncated `.vsf` raised out of `LoadLastStyle` — before `Application.Run`, where the error box is invisible (§1). It is now wrapped in `try..except` that logs, shows, and continues on the default theme — the same shape the FMX twin already used. It also logs to `AppDataCore.RamLog` on the invalid-file path, so the failure is visible in a headless run.
