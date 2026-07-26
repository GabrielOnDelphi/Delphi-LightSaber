# CrashTest — deliberate-crash FMX app (crash bed for `/light-bug-Android`)

Disposable test project. It exists to **crash on demand** so the `/light-bug-Android` skill (PC-side Android crash triage over adb) can be exercised end-to-end on a real phone. It proved that skill on 2026-07-22 (Opus 4.8) on a OnePlus Nord AC2003 / Android 12. Kept as a re-runnable crash bed — not a product.

## What it is

- Copy of `..\Template - Minimal app + Internal log\`, kept inside `Demo\FMX\` so the `..\..\..\` relative unit paths still resolve.
- Project filename `FMX_MinimalApp_WithLog1` → package **`com.embarcadero.FMX_MinimalApp_WithLog1`**.
- Two edits vs the template:
  - `FMX_MinimalApp_WithLog1.dpr` — first line is `InstallExceptionLogger('CrashTest-Exceptions.log')` (LightSaber `LightCore.ExceptionLogger.pas`: hooks `RaiseExceptObjProc`, writes every raise to `Documents\`).
  - `MainForm.pas` / `.fmx` — two crash buttons + a bottom `TLogViewer` strip wired to the RamLog.

## The two crash buttons

| Button | Code | What it exercises |
|---|---|---|
| **btnCrashPascal** | `raise Exception.Create(...)` (`MainForm.pas:54`) | "Pascal exception" row of the skill's classification table. FMX on Android 12 shows a dialog and keeps running. |
| **btnCrashAV** | nil-deref `P^:= 42` (`MainForm.pas:64`) | SIGSEGV → Delphi converts to `EAccessViolation`, logs the faulting PC → symbolication path. |

Both land in the exception log with timestamp / thread / class / message.

## Re-run it (the whole skill pipeline)

Constants: `ADB = c:\Delphi\Delphi 13\CatalogRepository\AndroidSDK-37.0.59082.6021\platform-tools\adb.exe`

1. **Build + deploy (headless, Android64/Debug).** Normally forbidden by CLAUDE.md — ask Gabriel first, or use IDE F9. If headless: `rsvars.bat` then MSBuild `/t:Make;Deploy /p:Platform=Android64 /p:Config=Debug`.
   - Gotcha: the build output dir is `_Android64_Debug\` (leading underscore). If `FMX_MinimalApp_WithLog1.deployproj` points at `37.0_Android64_Debug\`, hand-edit those path refs (sticks with no IDE running; `.dproj` untouched).
2. **Install for the foreground user:** `& $ADB install -r --user 0 <apk>`. Plain `install -r` can update a stale copy on another Android user (Guest / user 10) and leave user 0 without it. APK: `_Android64_Debug\FMX_MinimalApp_WithLog1\bin\FMX_MinimalApp_WithLog1.apk`.
3. **Launch:** `& $ADB shell am start -n com.embarcadero.FMX_MinimalApp_WithLog1/com.embarcadero.firemonkey.FMXNativeActivity`. If the screen is asleep the screencap is all-black — `input keyevent KEYCODE_WAKEUP` + `wm dismiss-keyguard` first. Dismiss the FMX exception dialog before tapping the next button (a modal dialog eats taps aimed behind it).
4. **Pull the log:** `& $ADB shell run-as com.embarcadero.FMX_MinimalApp_WithLog1 cat files/CrashTest-Exceptions.log` (debug-signed builds only).
5. **Symbolicate a native address** (addr2line-first, `.map`-last-resort — and there is NO `.map` on Android): point `llvm-addr2line` at the **unstripped build-output** `.so` (`_Android64_Debug\libFMX_MinimalApp_WithLog1.so`, ~69 MB, full DWARF), NOT the deployed APK `.so` (~39 MB, `.dynsym` only).
   - `ELFoffset = faultPC − rxBase`; `rxBase` = the `r-xp` mapping start in `run-as ... cat /proc/<pid>/maps` (file offset 0).
   - `llvm-addr2line -e _Android64_Debug\libFMX_MinimalApp_WithLog1.so -f -C -i 0x<ELFoffset>`
   - Live example 2026-07-22: PC `0x7D779CB104` − base `0x7D761AD000` = `0x181E104` → `TForm1::btnCrashAVClick` / `MainForm.pas:64`.

## See also

- Skill: `c:\Users\trei\.claude\skills\light-bug-Android\` (`SKILL.md` + `Reference - Android crash triage.md`).
- Full acceptance-test record: `c:\Projects\FMX\Bug reporter FMX\HandOver.md` → "Acceptance test — 2026-07-22".

## Delete when no longer needed

`C:\Projects\LightSaber\Demo\FMX\CrashTest\` + uninstall from the phone for **both** users: `& $ADB uninstall com.embarcadero.FMX_MinimalApp_WithLog1` and `& $ADB uninstall --user 10 com.embarcadero.FMX_MinimalApp_WithLog1`.
