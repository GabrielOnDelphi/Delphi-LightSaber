# CoolTrayIcon / TextTrayIcon

Two VCL components that put an icon in the Windows notification area (the "system tray").

- **TCoolTrayIcon** — the tray icon itself: tooltip, balloon notification, popup menu, mouse and keyboard events, icon animation from a TImageList, and minimize-to-tray.
- **TTextTrayIcon** — descends from TCoolTrayIcon and draws text into the icon, over an optional background icon. Useful for a counter or a countdown.

Both register on the component palette page `3rd_party`.

## Credit

Original components by Troels Jakobsen (freeware, 2006), with portions by Jouni Airaksinen. The original is at https://github.com/coolshou/CoolTrayIcon — this is a heavy rewrite of it, not a patch.

## What this fork changed

- **Fixed on 64-bit.** The original stored the component's own pointer in `NOTIFYICONDATA.uID` and cast the message `wParam` straight back to an object. `uID` is a 32-bit `UINT`, so the pointer was truncated and the cast produced garbage. Icons now get sequential ids and a small registry does the reverse lookup. `TSimpleTimer` had the same bug in its timer id (`FId: UINT`) — it is `NativeUInt` now.
- Dropped the Windows NT code paths and the pre-Unicode Delphi conditionals.
- `MinimizeToTray` no longer needs an `OnMinimizeToTray` handler. Before, only the taskbar route worked without one, while the form's own minimize button fell through to a plain Windows minimize. Note for existing users: with `MinimizeToTray = True` and no handler, that minimize button is now swallowed, so `Application.OnMinimize` no longer fires on that route.
- New `FocusFormOnShow` (public, deliberately not published). `True`, the default, is the old behaviour — the component calls `SetForegroundWindow` when the owner form is shown. Set it to `False` when your application must appear on screen without taking the keyboard focus away from the user.
- New `PutIconOnlyInTask` and `PutIconOnlyInTray`.
- **Breaking change, 09.2026:** `ShowBalloonHint` and `ShowBalloonHintUnicode` lost their last parameter, `TimeoutSecs`. Windows has ignored `NOTIFYICONDATA.uTimeout` since Vista — the balloon now stays up for as long as the system accessibility settings say — so the parameter did nothing at all. Delete the last argument from your calls; there is no replacement.

## Requirements

- VCL, Windows. Written for both Win32 and Win64. Developed and compiled with Delphi 13 (Athens). The code uses unit-scoped RTL names (`Winapi.Windows`, `Vcl.Forms`), so Delphi XE2 is the theoretical floor, but older versions are not tested.
- **`Application.MainFormOnTaskbar` must be `False`**, and it must be set before `Application.Run`. The components do not work correctly with `True`.
- Do not drive the component while the application is still starting up. To start minimized, use the `OnStartup` event and its `ShowMainForm` parameter, or let the application come up and hide it a moment later from a timer.

## Files

| File | What it is |
|---|---|
| `CoolTrayIcon.pas` | TCoolTrayIcon |
| `TextTrayIcon.pas` | TTextTrayIcon |
| `SimpleTimer.pas` | TSimpleTimer, a lightweight timer built on TObject. TCoolTrayIcon needs it. |
| `*.dcr` | component palette icons |

## Usage

```pascal
procedure TForm1.TrayIconClick(Sender: TObject);
begin
  TrayIcon.PutIconOnlyInTask;
end;

procedure TForm1.appEventsMinimize(Sender: TObject);
begin
  TrayIcon.PutIconOnlyInTray;
end;
```

## License

Freeware, same as the original.
