# CreateMainForm — VCL and FMX take the same parameters (changed 2026-08-20, Opus 5)

Moved here from `C:\Projects\LightSaber\CLAUDE.md` on 2026-09-16. The rules that still bind are kept there, short, under the same heading.

```pascal
CreateMainForm(aClass: TFormClass;                AutoState: TAutoState= asPosOnly); overload;   // VCL, LightVcl.Visual.AppData
CreateMainForm(aClass: TFormClass; OUT Reference; AutoState: TAutoState= asPosOnly); overload;
CreateMainForm(aClass: TComponentClass; OUT aReference; aAutoState: TAutoState= asPosOnly);     // FMX, LightFmx.Common.AppData
```

The VCL version used to take two more parameters — `MainFormOnTaskbar: Boolean= FALSE` and `Show: Boolean= TRUE` — in slots 3 and 4, ahead of `AutoState`. **That made a third positional argument mean `AutoState` on FMX but `MainFormOnTaskbar` on VCL**, so a DPR line copied between the two frameworks compiled clean and silently lost its `AutoState`, degrading it to `asPosOnly`: the app then saved its window position but none of its controls. It bit PriceCalculator (found 2026-08-19) and both `_OLD code` copies of the VCL2FMX converter still show the slip.

They are gone because they were never per-form settings — the body just wrote `Application.MainFormOnTaskbar` and `Application.ShowMainForm`, which are Application-wide. **Set them in the DPR before the call**, exactly like an IDE-generated DPR does:

```pascal
Application.MainFormOnTaskbar:= TRUE;    // RTL default FALSE. TRUE = the taskbar button belongs to the main form
Application.ShowMainForm:= FALSE;        // RTL default TRUE.  FALSE = start hidden (systray, splash, skin loading)
AppData.CreateMainForm(TMainForm, MainForm, asFull);
```

`CreateMainForm` now READS `Application.ShowMainForm` to decide whether to show the form, and no longer writes either flag. The RTL defaults are identical to the old parameter defaults (`Vcl.Forms.pas:12311` and `:12316`), so a call that never passed them is unchanged.

## The "set it too late" hazard — CLOSED at the root, 2026-08-20 (Opus 5)

Removing the parameters had one cost: as a parameter, setting `MainFormOnTaskbar` too late was **impossible**; as a flag it was only **discouraged**, and getting it wrong failed silently. Setting it after `CreateMainForm` makes `TApplication.SetMainFormOnTaskBar` do `FMainForm.Perform(CM_RECREATEWND)` (`Vcl.Forms.pas:14758`), and the new handle discarded the `WM_POSTINIT` that `CreateMainForm` posted — so `FormPostInitialize` never fired and the app came up half-initialized with nothing raised.

The first fix proposed was a guard in `TAppData.Run` comparing the flag against its value at create time. **That was the wrong fix** — it patches one trigger of a root cause that had four:

| Trigger | Where it was documented |
|---|---|
| `MainFormOnTaskbar` set after `CreateMainForm` | this section |
| `LoadLastStyle` / any `SetStyle` on a live main form | `FormSkinsDisk.pas` (hard `RAISE` guard) |
| `Application.ShowMainForm := FALSE` + a style broadcast | [Skins-VCL.md](Skins-VCL.md) §3 |
| Loading skins from the main form itself | `BioniX\SourceCode\MainForm.pas` (comment) |

All four are the same defect: **the post-init step was anchored to a window HANDLE, and the VCL recreates that handle for perfectly ordinary reasons.** So the root cause was removed instead. `TAppData.CreateMainForm` now calls **`TLightForm.SchedulePostInitialize`**, which uses `TThread.ForceQueue` — an entry in the RTL queue, which no window owns. It is drained by `CheckSynchronize`, pumped two independent ways: `WM_NULL` in `TApplication.WndProc` (`Vcl.Forms.pas:13086`, woken by `WakeMainThread` `:14669`) and again in `TApplication.Idle` (`:14067`). This is what the FMX twin already did (`LightFmx.Common.AppData.Form.pas`), so both frameworks now use one mechanism.

Consequences:
- **Setting `MainFormOnTaskbar` late is now a style rule, not a correctness rule** — it still costs a window recreate (flicker, focus dance), so keep setting it before the call, but nothing breaks silently any more. `Application.ShowMainForm` never had the constraint.
- `WM_POSTINIT` and `TLightForm.WMPostInit` are **kept** for backward compatibility (an application might post it). Both routes funnel into `RunPostInitialize`, which is guarded by `FPostInitDone` so the body runs exactly once.
- `TLightForm.DoDestroy` calls `TThread.RemoveQueuedEvents(QueuedPostInitialize)` — mandatory, because the queued entry holds `Self` and a form freed before the queue drains would be a use-after-free. This is why `QueuedPostInitialize` is a **named method** and not an anonymous block: `RemoveQueuedEvents` matches on the method.
- **The `FormSkinsDisk.pas` guard stays.** Only one of its three reasons was fixed; the `TMainMenuBarStyleHook` leak (BUG 5) and the silently-unregistered `DragAcceptFiles` are untouched by this.

Migration was mechanical and ran across `c:\Projects` on 2026-08-20 (86 DPR files). **A stale 4- or 5-argument call cannot compile**: a `Boolean` will not bind to `TAutoState`, and a constant will not bind to an untyped `OUT`. So there is no silent-drift path — the migration is either done or the project does not build.

⚠ **It was not actually complete, and "does not build" hid that for three days.** `DnaBaser5.DPR` was missed and failed with `E2250` on `AppData.CreateMainForm(TFrmBaser, FrmBaser, FALSE)`; nobody noticed because nobody rebuilt DnaBaser between 2026-08-20 and 2026-08-23. **The file is named `.DPR` in upper case, and a case-sensitive `*.dpr` glob skips it** — that is almost certainly how the sweep lost it. Migrated and rebuilt clean on 2026-08-23 (Opus 5). Note the half-migrated shape it was left in: the two `Application.MainFormOnTaskbar` / `Application.ShowMainForm` lines had been added correctly, only the argument was never stripped. **Any future repo-wide sweep over Delphi sources must match the extension case-insensitively.**

⚠ `CreateForm` was NOT changed and still has the mismatch: its slot 3 is `Show: Boolean`, while FMX `CreateForm`'s slot 3 is `aAutoState`. Its `Show` is genuinely per-form (it shows *that* form, not `Application.ShowMainForm`), so removing it is a different decision — `CreateFormHidden` already covers `Show=FALSE`.
