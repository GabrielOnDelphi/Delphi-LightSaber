# Test units that belong to no project (found 2026-09-03, Opus 5)

`c:\Projects\LightSaber\UnitTesting\` holds **164** files named `Test*.pas`. **57 of them are named in no `.dpr` and in no `.dproj` anywhere under `c:\Projects`**, so nothing ever compiles them and nothing ever runs them. Between them they hold **1316** methods marked `[Test]` — more than the whole `Tests_LightCore.exe` suite, which runs 1180.

How the list was made: for every `Test*.pas` in that folder, search every `.dpr` and every `.dproj` under `c:\Projects` for the unit name. No hit anywhere means no project links it.

**A unit that has never been compiled is not merely unrun — it may not even build.** That was true of the one already fixed. `Test.LightCore.IOPlatformFile.pas` was added to `Tests_LightCore.dpr` on 2026-09-03 and failed at once with `E2250: There is no overloaded version of 'WinToUnix' that can be called with these arguments`: its test `TestWinToUnix_WithNotify` passes an anonymous method to a callback that was declared as a plain procedure pointer. Expect more of that in the 57 below. Add them in small batches, one project at a time, and build after each batch.

The "where it probably belongs" column is a guess from the unit name alone. Nobody has checked that the named project actually links the source unit each test needs.

| Where it probably belongs | Units | `[Test]` methods |
|---|---|---|
| `Tests_LightCore.dproj` | 8 | 218 |
| `Tests_LightVcl.Common.dproj` | 8 | 134 |
| `Tests_LightVcl.Graphics.dproj` | 19 | 448 |
| `Tests_LightVcl.Internet.dproj` | 5 | 127 |
| `Tests_LightVcl.Visual.dproj` | 15 | 361 |
| No obvious home | 2 | 28 |
| **Total** | **57** | **1316** |

## Belongs in `Tests_LightCore.dpr` + `Tests_LightCore.dproj`

- `Test.LightCore.Compiler.pas` - 20 tests
- `Test.LightCore.Pascal.pas` - 62 tests
- `Test.LightCore.Platform.pas` - 26 tests
- `Test.LightCore.Reports.pas` - 15 tests
- `Test.LightCore.RttiSetToString.pas` - 23 tests
- `Test.LightCore.SearchResult.pas` - 26 tests
- `Test.LightCore.StringListA.pas` - 25 tests
- `Test.LightCore.WrapString.pas` - 21 tests

## Belongs in `Tests_LightVcl.Common.dpr` + `Tests_LightVcl.Common.dproj`

- `Test.LightVcl.Common.CursorGuard.pas` - 7 tests
- `Test.LightVcl.Common.LogViewer.pas` - 13 tests
- `Test.LightVcl.Common.Process.pas` - 10 tests
- `Test.LightVcl.Common.Shell.pas` - 28 tests
- `Test.LightVcl.Common.SystemTime.pas` - 18 tests
- `Test.LightVcl.Common.WMIResolution.pas` - 13 tests
- `Test.LightVcl.Common.WinVersionApi.pas` - 21 tests
- `Test.LightVcl.Common.WindowMetrics.pas` - 24 tests

## Belongs in `Tests_LightVcl.Graphics.dpr` + `Tests_LightVcl.Graphics.dproj`

- `Test.LightVcl.Graph.Bitmap.pas` - 70 tests
- `Test.LightVcl.Graph.BkgColor.pas` - 21 tests
- `Test.LightVcl.Graph.BkgColorEditor.pas` - 11 tests
- `Test.LightVcl.Graph.BkgColorParams.pas` - 21 tests
- `Test.LightVcl.Graph.Convert.pas` - 37 tests
- `Test.LightVcl.Graph.FX.RotateGr32.pas` - 28 tests
- `Test.LightVcl.Graph.Gif.pas` - 20 tests
- `Test.LightVcl.Graph.GrabAviFrame.pas` - 13 tests
- `Test.LightVcl.Graph.Loader.RainDrop.pas` - 19 tests
- `Test.LightVcl.Graph.Loader.Thread.pas` - 11 tests
- `Test.LightVcl.Graph.ResizeFMX.pas` - 15 tests
- `Test.LightVcl.Graph.ResizeGr32.pas` - 23 tests
- `Test.LightVcl.Graph.ResizeParamFrame.pas` - 27 tests
- `Test.LightVcl.Graph.ResizeParams.pas` - 29 tests
- `Test.LightVcl.Graph.ResizeVCL.pas` - 28 tests
- `Test.LightVcl.Graph.ResizeWinGDI.pas` - 11 tests
- `Test.LightVcl.Graph.ResizeWinThumb.pas` - 23 tests
- `Test.LightVcl.Graph.ResizeWinWIC.pas` - 18 tests
- `Test.LightVcl.Graph.ShadowText.pas` - 23 tests

## Belongs in `Tests_LightVcl.Internet.dpr` + `Tests_LightVcl.Internet.dproj`

- `Test.LightVcl.Internet.CommonWebDown.pas` - 6 tests
- `Test.LightVcl.Internet.Download.Indy.pas` - 12 tests
- `Test.LightVcl.Internet.Download.Thread.pas` - 18 tests
- `Test.LightVcl.Internet.Download.WinInet.pas` - 17 tests
- `Test.LightVcl.Internet.Email.pas` - 74 tests

## Belongs in `Tests_LightVcl.Visual.dpr` + `Tests_LightVcl.Visual.dproj`

- `Test.LightVcl.Visual.CalendarCanvas.pas` - 38 tests
- `Test.LightVcl.Visual.CheckBox.pas` - 15 tests
- `Test.LightVcl.Visual.Edit.pas` - 20 tests
- `Test.LightVcl.Visual.ListBox.pas` - 49 tests
- `Test.LightVcl.Visual.Memo.pas` - 39 tests
- `Test.LightVcl.Visual.MinimalPathLabel.pas` - 23 tests
- `Test.LightVcl.Visual.Panel.pas` - 14 tests
- `Test.LightVcl.Visual.PathEdit.pas` - 32 tests
- `Test.LightVcl.Visual.RichEdit.pas` - 15 tests
- `Test.LightVcl.Visual.RichEditResize.pas` - 12 tests
- `Test.LightVcl.Visual.RichLog.pas` - 35 tests
- `Test.LightVcl.Visual.RichLogTrack.pas` - 14 tests
- `Test.LightVcl.Visual.RichLogUtils.pas` - 12 tests
- `Test.LightVcl.Visual.RichRamLog.pas` - 28 tests
- `Test.LightVcl.Visual.Timer.pas` - 15 tests

## No obvious home

- `Test.FormScreenCapture.pas` - 19 tests
- `Test.ProteusDemoUtils.pas` - 9 tests

`Test.FormScreenCapture.pas` and `Test.ProteusDemoUtils.pas` name no LightSaber layer, so which project should own them is a real question, not a lookup.
