# Test units that belonged to no project — WIRED IN 2026-09-03/04 (Opus 5)

On 2026-09-03 this folder held **164** files named `Test*.pas`. A scan said 57 of them were named in
no `.dpr` and in no `.dproj` anywhere under `c:\Projects`, so nothing compiled them and nothing ran
them. That scan missed four (see the warning below), so the real count is **61**. Of those 61:
**58 are wired into a LightSaber test project**, one more (`Test.cpProteus.pas`) is wired into the
LightProteus test project where it belongs, one (`Test.ProteusDemoUtils.pas`) was recycled because
it tested a copy of the code pasted into itself, and **one is still out** — its reason is at the bottom.

⚠ **That scan of 2026-09-03 undercounted, and the reason is worth remembering.** It tested whether a
unit's name appears anywhere in a project file as a plain substring. A short name is a substring of a
longer one, so four orphans were hidden behind units that ARE linked:
`Test.LightVcl.Graph.Loader` hid inside `Test.LightVcl.Graph.Loader.Thread`,
`Test.LightVcl.Graph.Resize` inside `Test.LightVcl.Graph.ResizeVCL`,
`Test.LightVcl.Graph.Util` inside `Test.LightVcl.Graph.UtilGray`, and `Test.cpProteus` inside
`Test.cpProteusIO`. **Any future scan must require a word boundary after the name** — in a regular
expression, `(?![A-Za-z0-9_.])`. Those four were found on 2026-09-04. Three went into
`Tests_LightVcl.Graphics` and carried **158 further tests** between them (Loader 37, Resize 54,
Util 67); the fourth, `Test.cpProteus` with its 26 tests, belonged to LightProteus all along.

## What the test projects run now

| Project | Tests on 2026-09-03 | Tests now | Result |
|---|---|---|---|
| `Tests_LightCore` | 1180 | **1389** | 1389 pass, 0 failed, 0 leaked |
| `Tests_LightVcl.Common` | 458 | **584** | 583 pass, 1 ignored, 0 failed, 0 leaked |
| `Tests_LightVcl.Internet` | 5 | **123** | 123 pass, 0 failed, 0 leaked |
| `Tests_LightVcl.Visual` | 48 | **394** | 394 pass, 0 failed, 0 leaked |
| `Tests_LightVcl.Graphics` | 277 | **854** | 854 pass, 0 ignored, 0 failed, 0 errored, 0 leaked |
| `Tests_LightFmx` | 356 | **369** | 369 pass, 0 failed, 0 leaked |
| `Tests_LightVcl.Forms` | 379 | 379 | 379 pass, 0 failed, 0 leaked |

**1968 → 3344 in the five projects the job touched.** Every one of the seven builds Win32/Debug with
0 errors, 0 warnings, 0 hints, and every one runs green.

## THE fix that makes a headless run safe — `TAppDataCore.TEST_MODE`

**Every unattended test run must set `TAppDataCore.TEST_MODE := TRUE` before the first test.** It was
missing from five of the seven test projects, and that single missing line is what froze the Graphics
suite for 15 minutes on 2026-09-03 waiting for somebody to click a message box.

What the flag does: `MesajGeneric` in `c:\Projects\LightSaber\FrameVCL\LightVcl.Common.Dialogs.pas:96`
returns 0 immediately instead of calling `Application.MessageBox`, and `ShowModal` / `Show` are
bypassed in `LightVcl.Visual.AppData.pas` (lines 521, 533, 543). Its own declaration in
`c:\Projects\LightSaber\LightCore.AppData.pas:103` says so: *"When TRUE, ShowModal/Show calls are
bypassed. Set this in test setup to prevent forms from blocking tests."*

`Tests_LightFmx.dpr:78` and `Tests_LightVcl.Forms.dpr:83` already did it. `Tests_LightCore.dpr`,
`Tests_LightVcl.Common.dpr`, `Tests_LightVcl.Graphics.dpr`, `Tests_LightVcl.Internet.dpr` and
`Tests_LightVcl.Visual.dpr` did not. All five now do. Measured 2026-09-04: with the flag set, the
Graphics suite runs `Test.LightVcl.Graph.Loader`, whose 37 tests include ten that deliberately load a
file that is not there, and never stops for a dialog.

⚠ This makes the TEST RUN safe. **It does not fix the shipped library** — see the first open item at
the bottom.

## Wired in

### `Tests_LightCore` — 8 units
`Test.LightCore.Pascal`, `.Platform`, `.Reports`, `.RttiSetToString`, `.SearchResult`,
`.StringListA`, `.WrapString`, and `Test.LightCore.CompilerVersions`.

The last one needed a file rename before Delphi would take it: the file was called
`Test.LightCore.Compiler.pas` while the unit inside declared itself `Test.LightCore.CompilerVersions`,
and Delphi rejects a unit whose declared name differs from its file name. Renamed on 2026-09-04; its
18 tests all pass.

### `Tests_LightVcl.Common` — 8 units
`Test.LightVcl.Common.CursorGuard`, `.LogViewer`, `.Process`, `.Shell`, `.SystemTime`,
`.WMIResolution`, `.WinVersionApi`, `.WindowMetrics`.

### `Tests_LightVcl.Internet` — 5 units
`Test.LightVcl.Internet.CommonWebDown`, `.Download.Indy`, `.Download.Thread`, `.Download.WinInet`,
`.Email`.

### `Tests_LightVcl.Visual` — 15 units
`Test.LightVcl.Visual.CalendarCanvas`, `.CheckBox`, `.Edit`, `.ListBox`, `.Memo`,
`.MinimalPathLabel`, `.Panel`, `.PathEdit`, `.RichEdit`, `.RichEditResize`, `.RichLog`,
`.RichLogTrack`, `.RichLogUtils`, `.RichRamLog`, `.Timer`.

### `Tests_LightVcl.Graphics` — 21 units
`Test.LightVcl.Graph.Bitmap`, `.BkgColor`, `.BkgColorParams`, `.Convert`, `.FX.RotateGr32`, `.Gif`,
`.GrabAviFrame`, `.Loader.Thread`, `.ResizeFMX`, `.ResizeGr32`, `.ResizeParamFrame`, `.ResizeParams`,
`.ResizeVCL`, `.ResizeWinGDI`, `.ResizeWinThumb`, `.ResizeWinWIC`, `.ShadowText` (2026-09-03), plus
`.Loader` (37 tests), `.Resize` (54), `.Util` (67) and `.RainShelter` (18) on 2026-09-04.

`Test.LightVcl.Graph.RainShelter` is the file that used to be called
`Test.LightVcl.Graph.Loader.RainDrop.pas`. The unit it was written against,
`LightVcl.Graph.Loader.RainDrop.pas`, no longer exists — it was split into
`FrameVCL\LightVcl.Graph.RainShelter.pas` and `FrameVCL\LightVcl.Graph.RainDropParams.pas`. The
mapping turned out to be mechanical and is now done: the routine `IsRainDrop` is called
`IsRainShelter`, the standalone `LoadRainShelter` became the class method `TRainShelter.LoadBitmap`,
and the file-extension constant is `RainDrop` in `LightCore.IO.pas:138` (the test called it
`RainDropExt`, a name that exists nowhere).

⚠ `Tests_LightVcl.Graphics.dproj` had `TESTINSIGHT` **defined**, unlike the other six test projects
which define `TESTINSIGHT_` (with the underscore, so the name is NOT defined). With TESTINSIGHT
defined the project builds as a TestInsight application with no console output, so it cannot be run
from the command line at all. Changed to `TESTINSIGHT_` to match the rest.

## Still OUT — 1 unit

- **`Test.LightVcl.Graph.BkgColorEditor.pas`** (10 tests) — every one of its tests calls
  `Application.CreateForm(TfrmBorderEditor, Form)`. `c:\Projects\CLAUDE.md` says **"No form tests"**
  for this repository. Note the reason recorded on 2026-09-03 — that the form opens a modal box and
  hangs the run — is **no longer the blocker**: `TAppDataCore.TEST_MODE` (see above) suppresses that
  box. It stays out on the rule, not on the hang.

### The other three were settled on 2026-09-04 (Gabriel's decisions)

- **`Test.FormScreenCapture.pas`** — wired into `Tests_LightFmx.dproj`, with 13 of its 16 tests
  running. The two fixtures that touch no form, `TTestScreenCaptureManager` and `TTestOverlayStyle`,
  are registered. The third, `TTestFormScreenCapture` (3 tests), calls `TfrmScreenCapture.Create(nil)`
  — a real FMX form — so its `TDUnitX.RegisterTestFixture` line is commented out under the
  "No form tests" rule, with the reason written beside it. `Tests_LightFmx` went 356 → 369 tests.
  ⚠ The reason recorded on 2026-09-03, that the unit under test lives inside a demo application,
  was **wrong**: `FormScreenCapture.pas` and `LightFmx.Visual.ScreenCapture.pas` are both in
  `c:\Projects\LightSaber\FrameFMX\` and both are listed in `FrameFMX\LightFmxVisual.dpk`.

- **`Test.cpProteus.pas`** — moved to `c:\Projects\LightProteus\ProteusSource\DUnitX\` and wired
  into `ProteusTests.dpr` and `ProteusTests.dproj`. That project already linked `cpProteus.pas`
  (line 25) but had no test unit for it, so this file was the missing one sitting in the wrong
  repository. 26 tests. **It has not been built or run yet — that is LightProteus work.**

- **`Test.ProteusDemoUtils.pas`** — recycled. All 8 of its tests exercised a copy of `IntToBin`
  pasted into the test file itself, so they proved nothing about any shipped unit. The five cases it
  covered that the LightProteus tests did not — zero, one, truncation to fewer digits, 16 bits, and
  powers of two — were added to
  `c:\Projects\LightProteus\ProteusSource\DUnitX\Test.cpProteusUtils.pas`, where they run against
  the real `IntToBin` at `c:\Projects\LightProteus\ProteusSource\cpProteusUtils.pas:248`.

## Library bugs found by compiling and running code no project had ever built

**Fixed:**
1. `FrameVCL\LightVcl.Visual.RichRamLog.pas` did not compile at all — it calls `String2TSL` twice but
   never named `LightCore.StringList`. **The reason nobody noticed is now measured: that file is in
   no package.** Eleven library units are in no package at all — see the entry at the top of
   `c:\Projects\LightSaber\HandOver.md`.
2. `FrameVCL\LightVcl.Visual.MinimalPathLabel.pas` raised `EInvalidOperation` when `CaptionMin` was
   set before the label was parented, because it used `Canvas` with no parent window.
3. `FrameVCL\LightVcl.Visual.RichLogTrack.pas` asserted `Log <> NIL` in `TrackBarChange`, forbidding
   the very order (`Verbosity` first, `Log` after) that its own `setRichLog` is written to support —
   and with assertions off, that line was an access violation on a NIL log.
4. `FrameVCL\LightVcl.Graph.GrabAviFrame.pas` promised in its header to return a black bitmap when
   the icon file is missing, but instead reached `LoadGraph`, whose first line puts a **modal box**
   on screen, and leaked the bitmap it had just created.
5. `FrameVCL\LightVcl.Graph.Gif.pas` — `IsAnimated` RAISED for a missing `.gif` path while returning
   FALSE for a missing `.avi` path. It now answers FALSE for any file that is not there.
6. `FrameVCL\LightVcl.Graph.ResizeWinThumb.pas:86` computed
   `hImagList16 + (hImagList16 - hImagList32)` on three `NativeUInt` values. When the second handle is
   the larger one that subtraction wraps below zero, and a build with overflow checking on — which
   is what Debug uses — raises `EIntOverflow`. **21 tests of that unit errored on it.** The arithmetic
   now runs through `NativeInt`, which gives the same bit pattern the unsigned form produced with
   checking off, so Release is unchanged.
7. **`SimilarColor` said a colour is not similar to itself** (`FrameVCL\LightVcl.Graph.Util.pas:384`).
   It compared with a strict `<`: `abs(R1-R2) < Tolerance`. With `Tolerance = 0` that is `abs(0) < 0`,
   which is FALSE — so at tolerance 0 the routine returned FALSE for **every** pair of colours,
   identical ones included. Now `<=`, so Tolerance means "a difference of at most this much is still
   similar". Its one caller is `GetBorderDominantColor`
   (`FrameVCL\LightVcl.Graph.BkgColor.pas:732`), which groups the pixels along a wallpaper border into
   buckets of similar colour; its default Tolerance is 8, so the change widens each bucket by one
   unit and at tolerance 0 it finally groups exact duplicates instead of calling every pixel unique.
   Found by `Test.LightVcl.Graph.Util.TestSimilarColor_ExactMatch`.

**The image loader ignores the box it is given — fixed in the caller, NOT in the shared routine:**

`TBkgImgLoader`, the background thumbnail thread in
`FrameVCL\LightVcl.Graph.Loader.Thread.pas`, produced a **110-pixel-wide** thumbnail when asked for
100 (measured 2026-09-04, a 200x150 bitmap into a 100x100 box).

The cause is not that `LoadAndStretch` ignores the box. It fits the image to 100x75 and then
**deliberately adds 10 % back**. That is `RResizeParams.computeAutodetect` in
`FrameVCL\LightVcl.Graph.ResizeParams.pas`: auto-detect first tries to FILL the box, and when the
crop would be too large it falls back to Fit and then enlarges the result by `FitTolerance`
(10 by default) — its own comment says this is *"in order to get rid of some of the black bars"*.

**That is right for a wallpaper and wrong for a thumbnail.** BioniX calls the same routine to fill a
screen in seven places (`BxAIEngine.pas`, `BxAIBatchProcessor.pas`, `BxAIMaskGen.pas`,
`PluginRainDrop.pas`), so the shared `SmartStretch` was left exactly as it is. What changed is the one
caller: `TBkgImgLoader.ProcessFile` now builds an `RResizeParams` with `ResizeOpp := roFit`. A
thumbnail must stay inside its cell — `TCubicThumbs.DrawCell`
(`FrameVCL\LightVcl.Visual.ThumbViewerM.pas:416`) centres the bitmap with
`x := aRect.Left + (DefaultColWidth - BMP.Width) DIV 2` in a cell only `ThumbWidth + 2*CellSpacing`
wide, so a 10 % overshoot puts `x` left of the cell and the thumbnail paints over its neighbour.

⚠ **Three more callers have the same defect and were NOT changed:**
`FrameVCL\LightVcl.Visual.ThumbViewerM.pas:259` and `FrameVCL\LightVcl.Graph.Loader.pas:533` (both
thumbnails), and — outside this repository —
`c:\Projects\Projects GRAPHICS\Project QuickSilver Viewer\uImageRec.pas:276`, whose own comment reads
*"Fit within MaxSide x MaxSide, aspect kept"*, which is exactly what it does not get. The doc comments
on `SmartStretch(BMP; MaxWidth, MaxHeight)` and `LoadAndStretch(FileName; MaxWidth, MaxHeight)` in
`FrameVCL\LightVcl.Graph.Resize.pas` both used to promise a fit; they now state the overshoot and
point at the `roFit` overload.

**Two test bugs worth remembering, because both looked like library bugs:**
- `TestLoadMultipleImages` reported "Expected 5 but got 1". Its counting loop was
  `repeat Bmp:= PopPicture; if Bmp <> NIL then begin Inc(Count); FreeAndNil(Bmp); end; until Bmp = NIL;`
  — **`FreeAndNil` sets `Bmp` to NIL**, so the exit condition was true right after the first
  thumbnail and the count was always 1. Nothing was wrong with the loader.
- Three tests in `Test.LightVcl.Graph.Loader` died with an access violation and leaked 3 `TBitmap` +
  3 `TBitmapImage`. Its GIF helper did `Gif.Add(TBitmap.Create)` and then painted through
  `Gif.Images[0].Bitmap`. Both halves are wrong, and both were checked against the Delphi 13 RTL
  source `c:\Delphi\Delphi 13\source\vcl\Vcl.Imaging.GIFImg.pas`: `TGIFImage.Add` (line 11337)
  **copies** its source (`Result.Assign(Source)`) and never takes ownership, so the bitmap leaked;
  and the bitmap had no size, so the frame was Empty, and `TGIFFrame.GetBitmap` returns **NIL** for
  an empty frame (line 7290), which is the read of address 0.

## Two library changes Gabriel decided on 2026-09-04

**1. The 16 modal-box asserts in the image loader are gone — the loader now RAISES.**
`FrameVCL\LightVcl.Graph.Loader.pas` had 16 calls to `Assert(FileExistsMsg(FileName))`, and
`FileExistsMsg` (`FrameVCL\LightVcl.Common.IO.pas:180-187`) calls `MessageError`, which puts a modal
box on screen and waits for a click. Two faults in one line: an unattended program waited for ever,
and because `Assert` is compiled out in Release the same code silently carried on with a missing file
in the build customers run. All 16 now call a new private guard `CheckFileExists`, which raises
`EFileNotFoundException` (`System.SysUtils`, declared at
`c:\Delphi\Delphi 13\source\rtl\sys\System.SysUtils.pas:511`). It never blocks and it survives
into Release.

Two callers had to be adjusted, because both report failure by a value rather than an exception:
`TCacheObj.MakeThumb` (`FrameVCL\LightVcl.Graph.Cache.pas`) now returns FALSE for a file that
vanished, and `GetVideoPlayerLogo` (`FrameVCL\LightVcl.Graph.GrabAviFrame.pas`) already tested
`FileExists` first — its comment was brought up to date. Nine tests in
`Test.LightVcl.Graph.Loader.pas` moved from expecting `EAssertionFailed` to `EFileNotFoundException`;
the three that assert a NIL PARAMETER still expect `EAssertionFailed`, because those are genuine
programmer-error asserts.

**2. `TFileThumb.MaxSize` is 8192, not 65535** (`FrameVCL\LightVcl.Graph.ResizeWinThumb.pas`).
`SetSize` clamps to `MaxSize` and then calls `FBmp.SetSize(MaxSize, MaxSize)`. At 65535 that is a
12.9 GB bitmap, which Windows refuses with *"The handle is invalid."*, so the clamp protected
nothing. 8192x8192 at 32 bits is 256 MB, which Windows allocates. The test that proved the old value
broken, `TestSetWidth_AboveMaximum_ClampsToMax` in `Test.LightVcl.Graph.ResizeWinThumb.pas`, is no
longer `[Ignore]`d and passes.

## Open, for Gabriel to decide

0. **Two more modal boxes of the same shape, NOT changed — they were outside the decision above.**
   `FrameVCL\LightVcl.Graph.Bitmap.pas:135`: `SetLargeSize` catches `EOutOfMemory`, shows a warning
   box, and then returns normally, so the caller never learns the bitmap was not resized — a
   swallowed exception, which `c:\Projects\CLAUDE.md` bans outright.
   `FrameVCL\LightVcl.Graph.Gif.pas:206`: `ExtractFrame` shows a box for a bad frame number and then
   returns NIL; the NIL already signals the error, so the box adds only the blocking.

1. ~~**The shipped library still opens modal message boxes on ordinary failures.**~~ — DONE, see
   above. There are **16 calls
   to `Assert(FileExistsMsg(FileName))` in `FrameVCL\LightVcl.Graph.Loader.pas`**, and `FileExistsMsg`
   (`FrameVCL\LightVcl.Common.IO.pas:180-187`) shows an error box when the file is absent;
   `LightVcl.Graph.Bitmap.pas:135` and `LightVcl.Graph.Gif.pas:206` do the same with
   `MessageWarning`. `TAppDataCore.TEST_MODE` protects the test run, **not a shipped application**: an
   unattended program that loads a missing image still waits for ever. There is a second edge —
   `Assert` is compiled out in Release, so in a Release build the same call silently continues with a
   missing file instead of stopping.
2. ~~**`TFileThumb`'s `MaxSize = 65535`**~~ — DONE, it is 8192, see above.
3. **`Test.LightVcl.Common.WMIResolution.Test_GetMonitorInfoWMI_WithoutCoInitialize_RaisesException`**
   is marked `[Ignore]`: it claims the call raises without `CoInitialize`, and it does not — measured
   both on the test thread and on a brand new thread.
4. **`Test.cpProteus.pas` has been moved into the LightProteus repository but never built there.**
   Building and running `c:\Projects\LightProteus\ProteusSource\DUnitX\ProteusTests.dproj` is
   LightProteus work, not LightSaber work, and it has not been done.

## A DUnitX rule this work established

**Never use the `[WillRaise]` ATTRIBUTE in this repository, and always pass the message argument to
`Assert.WillRaise` / `Assert.WillNotRaise`.**

- The attribute form makes DUnitX build a `TDUnitXExceptionTest`, and a run holding one leaks its
  whole fixture tree at shutdown — measured: two such attributes produced a FastMM leak dump of
  about 1371 fixture, test and result objects even though every test passed. Use
  `Assert.WillRaise(<anonymous method>, <class>, <message>)` inside the test body instead.
- DUnitX declares each of `WillRaise`, `WillNotRaise`, `WillRaiseAny` and `WillNotRaiseAny` twice —
  once taking a `TTestLocalMethod`, once a `TTestMethod` — with defaults for every parameter after
  the first. A call that stops before the message argument can fail to compile with E2250. About 160
  such calls were given a message.
  ⚠ **Do not read E2250 as proof that the message was missing.** In both files where it appeared on
  2026-09-04 the same file also had a real `E2003 Undeclared identifier: 'Rect'`, and a
  two-argument `Assert.WillRaise(<anonymous method>, EAssertionFailed)` compiles perfectly well
  elsewhere — `Test.LightVcl.Graph.Loader.pas` has twelve of them and builds with 0 errors. So at
  least some of those E2250 reports were knock-on errors from the missing `System.Types` unit.
  Passing the message is still the house rule; it just is not always what E2250 was complaining
  about.
