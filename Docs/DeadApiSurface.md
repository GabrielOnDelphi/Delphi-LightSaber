# LightSaber - routines with no application caller

Measured 2026-08-23 (Opus 5), after `LightVcl.Common.VclUtils.DoubleBuffer` was deleted.

## How this was measured

Every `.pas`, `.dpr`, `.dfm` and `.fmx` file under `c:\Projects` was tokenised - **5825 files**. Backup, History, `_OLD`, `before upgrade`, `_CLEAN UP THIS`, `Older versions`, `External` and `Cod scos pe bara-` folders were excluded as dead code. Each identifier was counted separately per source: a real application, another LightSaber unit, a LightSaber Demo app, or the test suite.

`.dfm` and `.fmx` are included on purpose - an event handler wired in the form designer is referenced nowhere in Pascal and would look dead otherwise.

Only **top-level (standalone) routines** are counted - the ones declared directly in a unit's INTERFACE section. Class methods, `Register` procedures and virtual overrides are excluded, because the framework calls those, not application code.

### What this measurement cannot see

- Matching is by **bare routine name**, so overloads collapse into one entry and a routine sharing a name with something common counts as alive. **The real dead count is therefore a floor, not a ceiling.**
- A routine reached only through a procedural variable or an RTTI lookup would look dead. Rare in this codebase, but not impossible.
- "No caller" is not the same as "broken". For a general-purpose library, unused API is normal. What it does mean is that **nothing but the unit test would notice if it rotted** - and `DoubleBuffer` proved a test can pin the wrong behaviour.

## Result

| Bucket | Routines | Share |
|---|---:|---:|
| Used by a real application (or a Demo app) | 875 | 66.4% |
| Used only inside LightSaber, never by an app | 102 | 7.7% |
| ONLY its own unit test calls it | 241 | 18.3% |
| NOBODY calls it - not an app, not a test, not another unit | 100 | 7.6% |
| **Total standalone library routines** | **1318** | |

**341 routines (26% of the standalone public API) have no application consumer.**

Six were re-checked by an independent `grep` over the same tree, and all six confirmed zero application hits: `FindSubcontrolAtPos`, `BlinkControl`, `ScrollFormCaption`, `CopyControl2Png`, `GetFolderSize`, `IsICO`.

## Per unit

`never referenced` = not one application, not one test, not one other LightSaber unit.
`test only` = the unit test is the sole caller. This is exactly the bucket `DoubleBuffer` was in.

### LightCore.IO.pas  -  24

- **never referenced** (15): `AppendTo`, `CopyFileTop`, `ExtractRelativePath_`, `FileNameIsValid_`, `GetDownloadsPath`, `GetFolderSize`, `GetMoviesPath`, `GetMusicPath`, `GetSharedDocumentsPath`, `IsEMF`, `IsICO`, `IsImage2Bmp`, `IsWMF`, `MoveFolderRel`, `MoveFolderSlow`
- **test only** (9): `AppendFileExtension`, `CheckPathLength`, `ExtensionToMimeType`, `ExtractFileExtUp`, `ExtractFirstFolder`, `ExtractParentFolder`, `IsSubfolder`, `ListFilesAndFolderOf`, `SameFolderFromFile`

### LightCore.pas  -  24

- **never referenced** (5): `ReplaceStringAnsi`, `SplitStringList`, `SplitStringListI`, `SplitStrings`, `UnicodeToAnsi`
- **test only** (19): `FixNumber`, `FormatBytesMB`, `GenerateRandStringLet`, `IncrementStringNo`, `IsUpcase`, `IsUpcaseLetter`, `IsWordSeparator`, `LastLetterInString`, `LeadingZeros2`, `LevenshteinDistance`, `LevenshteinSimilarity`, `RemoveNonAlphanum`, `RemoveTabs`, `SplitNumber_Start`, `SplitStringAtPos`, `SplitStringAtPos`, `StringSumm`, `StringSumm`, `WordCountStrict`

### LightCore.Binary.pas  -  19

- **never referenced** (2): `MakeCardinal_Slow`, `SerializeWord`
- **test only** (17): `Base256to255`, `ByteToBin`, `ClearBit`, `MakeCardinal`, `MakeCardinal`, `MakeCardinal_`, `ReverseByte`, `ReverseByte2`, `ReverseByte3`, `RotateLeft32`, `RotateLeft64`, `RotateRight32`, `RotateRight64`, `SerializeCardinal`, `StringIsHexNumber`, `SwapUInt64`, `WordToBin`

### LightCore.Internet.pas  -  18

- **never referenced** (7): `CollectIPAddress`, `ExtractFilePath_FromURL`, `ExtractIpFrom`, `ExtractProxiesFrom`, `IsURL`, `UrlExtractDomainWWW`, `ValidURL`
- **test only** (11): `ExpandURLs`, `ExtractProxyFrom`, `IpExtractPort`, `ServerStatus2String`, `SplitIpFromAdr`, `UrlExtractResource`, `UrlRemovePort`, `UrlRemoveStart`, `ValidateIpAddress`, `ValidatePort`, `ValidateProxyAdr`

### LightVcl.Common.Shell.pas  -  15

- **never referenced** (6): `AddContextMenu`, `AddContextMenu`, `AssociateSelf_ShellMenu`, `RemoveContextMenu`, `RemoveShowDesktopFile`, `RestoreShowDesktopFile`
- **test only** (9): `AddUninstaller`, `CreateShortcut_SendTo`, `ExtractPathFromLnkFile`, `GetAssociatedApp`, `InstallINF`, `InvokePropertiesDialog`, `InvokeStartMenu`, `IsApiFunctionAvailable`, `IsTaskbarAutoHideOn`

### LightCore.Time.pas  -  12

- **never referenced** (1): `DaysBetweenEx`
- **test only** (11): `Cardinal2Date`, `CurrentDateToString`, `Date2Cardinal`, `DateIsToday`, `DateToTime_`, `EarlierThan`, `GetUniversalDateFormat`, `SameDateEx`, `StringIsDate`, `StringIsTime`, `TodayIs`

### LightVcl.Common.VclUtils.pas  -  12

- **never referenced** (1): `FindSubcontrolAtPos`
- **test only** (11): `ActionVisibility`, `BlinkControl`, `CopyControl2Bitmap`, `CopyControl2Png`, `CreateControl`, `MenuVisibility`, `RefreshNow`, `ScrollAppTitle`, `ScrollFormCaption`, `ShowComponentState`, `ShowControlState`

### LightVcl.Graph.Util.pas  -  12

- **never referenced** (3): `CombinePixels`, `RGB2Color`, `ThemeGetPanelElementColor`
- **test only** (9): `BlendColors`, `ChangeBrightness`, `ComplementaryColor`, `HtmlToColor`, `Integer2Color`, `ThemeColorBkg`, `ThemeColorButtonFace`, `ThemeColorHilight`, `WindowsThemesEnabled`

### LightVcl.Internet.Common.pas  -  11

- **never referenced** (9): `CheckURLStartMsg`, `CreateUrlOnDesktop`, `IE_DeleteCache`, `IE_DisableProxy`, `IE_EnableProxy`, `IE_EndSession`, `IE_SetProxy`, `PathIsURLA`, `ResolveAddress`
- **test only** (2): `IsPortOpened`, `PCConnected2Internet`

### LightVcl.Common.Registry.pas  -  10

- **never referenced** (2): `RegReadMultiSzString`, `RegReadMultiSzStringSP`
- **test only** (8): `Convert_HKey2Str`, `Convert_Str2HKey`, `RegClearKey`, `RegHasSubKeys`, `RegReadValuePairs`, `RegValueExist`, `RegWriteBool`, `RegWriteValuePairs`

### LightVcl.Common.Window.pas  -  10

- **never referenced** (5): `MinAllWnd_ByHandle`, `MinAllWnd_ByShell`, `MinAllWnd_ByShell2`, `MinAllWnd_ByWinMKey`, `Remove_X_Button`
- **test only** (5): `FindChildForm`, `GetTextFromHandle`, `IsApplicationRunning`, `MaximizeForm`, `RestoreWindow`

### LightVcl.Common.System.pas  -  9

- **never referenced** (3): `JiggleMouse`, `PrintScreenActiveWnd`, `PrintScreenFull`
- **test only** (6): `BiosID`, `FreeUninstalledFont`, `GetDisplayModes`, `ServiceGetStatus`, `ServiceGetStatusName`, `UseUninstalledFont`

### LightVcl.Graph.Loader.pas  -  9

- **never referenced** (4): `LoadEMF`, `LoadFromResource`, `LoadICO`, `LoadJ2K`
- **test only** (5): `LoadBMP`, `LoadGIF`, `LoadGIF`, `LoadPNG`, `LoadToTImage`

### LightVcl.Common.WinVersion.pas  -  8

- **test only** (8): `GetOSDetails`, `IsWindows10`, `IsWindows10Up`, `IsWindows11`, `IsWindows11Up`, `IsWindows7`, `IsWindows8`, `IsWindowsVista`

### LightCore.TextFile.pas  -  7

- **never referenced** (3): `CountCharAppearance`, `IsValidUtf8File`, `IsValidUtf8Stream`
- **test only** (4): `ContainsUnicodeChars`, `DetectFileEncoding`, `FirstLineFromFile`, `GenerateRandomTextFile`

### LightVcl.Common.IO.pas  -  7

- **never referenced** (3): `GetMyDocumentsAPI`, `GetMyPicturesAPI`, `GetSaveDialog`
- **test only** (4): `GetDriveTypeS`, `GetPosAfterExtendedPrefix`, `GetSpecialFolders`, `SetCompressionAtr`

### LightVcl.Common.PowerUtils.pas  -  7

- **never referenced** (3): `InitSystemShutdown`, `MonitorsOff`, `WinExit`
- **test only** (4): `BatteryAsText`, `IsHibernateAllowed`, `IsPwrShutdownAllowed`, `IsPwrSuspendAllowed`

### LightVcl.Graph.Desktop.pas  -  7

- **never referenced** (3): `SetSystemColor`, `SetWallpaperBroadcast`, `SysForceTileWallpaper`
- **test only** (4): `DrawOnWindowBitBlt`, `GetDPI`, `GetDesktopResolutionAPI`, `GetShellWindow`

### LightCore.HTML.pas  -  7

- **test only** (7): `ExtractAttribValueIE`, `FindQuoteEnd`, `FindQuoteStart`, `IsSafeHtmlChar`, `LinkHasNoFollow`, `LinkOpensInNewWind`, `SanitizeText`

### LightVcl.Common.WindowMetrics.pas  -  7

- **test only** (7): `GetWin3DBorderHeight`, `GetWin3DBorderWidth`, `GetWinBorderHeight`, `GetWinBorderWidth`, `SetProportionalThumbH`, `SetProportionalThumbV`, `SetScrollbarWidth`

### LightVcl.Graph.Bitmap.pas  -  6

- **test only** (6): `AspectOrientation`, `AspectOrientation`, `ClearImage`, `EnlargeCanvas`, `PredictBitmapRamSize`, `PredictBitmapRamSize`

### LightVcl.Graph.Text.pas  -  6

- **test only** (6): `CenterTextY`, `CenterTextY`, `DrawTextCentered`, `GetFontHeight`, `ShadowDownLeft`, `ShadowDownRight`

### LightVcl.Common.CenterControl.pas  -  5

- **never referenced** (2): `CorrectFormPositionMainMonitor`, `CorrectFormPositionMonitor`
- **test only** (3): `CenterChildX`, `CenterInvalidChild`, `CorrectMDIFormPosition`

### LightVcl.Internet.Email.pas  -  5

- **never referenced** (2): `OpenDefaultEmail`, `OpenDefaultEmailEx`
- **test only** (3): `CorrectEmailAddress`, `FailCode2Str`, `SplitEmailAddress`

### LightVcl.Common.Reports.pas  -  4

- **never referenced** (4): `GenerateHardwareRepTSL`, `GenerateWinPathRep`, `GenerateWinPathRepEx`, `ScreenResApi`

### LightCore.EncodeMime.pas  -  4

- **test only** (4): `DeMimeString`, `DeMimeStringA`, `MimeString`, `MimeStringA`

### LightVcl.Common.Keyboard.pas  -  4

- **test only** (4): `GetModifierKeyState`, `IsAltDown`, `IsCtrlDown`, `IsShiftDown`

### LightVcl.Common.SystemPermissions.pas  -  4

- **test only** (4): `AppElevationLevel`, `CurrentUserHasAdminRights`, `IsUserAdmin`, `OsHasNTSecurity`

### LightVcl.Common.SystemTime.pas  -  4

- **test only** (4): `CurrentSysTimeStore`, `CurrentSysTimeValid`, `GetSysFileTime`, `UserIdleTime`

### LightVcl.Graph.BkgColor.pas  -  4

- **test only** (4): `GetBorderDominantColor`, `HasBlackBorder`, `LineIsBlack`, `RemoveBorder`

### LightFmx.Common.CenterControl.pas  -  4

- **test only** (4): `CenterFormOnDesktop`, `CenterFormOnParent`, `EnsureControlVisible`, `EnsureControlVisible`

### LightVcl.Internet.HTMLImg.pas  -  3

- **never referenced** (3): `ExtractImagesFromAHREF`, `MakeImgFullPath`, `MakeImgRelativePath`

### LightFmx.Common.Graph.pas  -  3

- **never referenced** (3): `FlipBitmapHorizontal`, `FlipBitmapHorizontalP`, `FlipBitmapVertical`

### LightCore.EncodeXOR.pas  -  3

- **test only** (3): `BetterDecode`, `BetterEncode`, `EncodeDecode_NOT`

### LightCore.Platform.pas  -  3

- **test only** (3): `AppIs64Bit`, `GenerateAppBitnessRep`, `OsArchitecture`

### LightVcl.Common.Debugger.pas  -  3

- **test only** (3): `ExitIfUnderDebugger`, `HaltApplication`, `LastErrorMsgStr`

### LightVcl.Common.EnvironmentVar.pas  -  3

- **test only** (3): `GetEnvironmentVars`, `GetEnvironmentVars`, `SetEnvironmentVars`

### LightCore.System.pas  -  2

- **never referenced** (2): `DisposeAndNil`, `GetResourceAsString`

### LightVcl.Internet.HTML.pas  -  2

- **never referenced** (2): `GetFormByNumber`, `SetFieldValue`

### LightCore.Debugger.pas  -  2

- **test only** (2): `LogFile_Add`, `LogFile_Init`

### LightCore.INIFileQuick.pas  -  2

- **test only** (2): `ReadDbl`, `WriteDbl`

### LightCore.Math.pas  -  2

- **test only** (2): `EnsureZero`, `FastModulo`

### LightCore.Pascal.pas  -  2

- **test only** (2): `IsReservedKeyword`, `RelaxedSearchEx`

### LightCore.WrapString.pas  -  2

- **test only** (2): `TruncateToWord`, `WrapStringForcedA`

### LightVcl.Common.WinVersionApi.pas  -  2

- **test only** (2): `GetWinVerNetServer`, `GetWinVersionEx`

### LightVcl.Graph.Alpha.pas  -  2

- **test only** (2): `GetTransparentBitmapFromImagelist`, `TransparencyBlend`

### LightCore.ExceptionLogger.pas  -  1

- **never referenced** (1): `ExceptionLogPath`

### LightVcl.Common.MutexSingleInstance.pas  -  1

- **never referenced** (1): `IsSingleInstance`

### LightVcl.Common.SystemSecurity.pas  -  1

- **never referenced** (1): `Hack_DisableSystemKeys`

### LightFmx.Common.CamUtils.pas  -  1

- **never referenced** (1): `ScanMediaFile`

### LightFmx.Common.CrashHandler.pas  -  1

- **never referenced** (1): `CrashLogPath`

### LightFmx.Common.Screen.pas  -  1

- **never referenced** (1): `IsDesktopScreen`

### LightFmx.Visual.SvgButton.pas  -  1

- **never referenced** (1): `ToggleSvgIcon`

### LightCore.AppData.pas  -  1

- **test only** (1): `ExtractPathFromCmdLine`

### LightCore.EncodeCRC.pas  -  1

- **test only** (1): `CRC32Stream`

### LightVcl.Common.Dialogs.pas  -  1

- **test only** (1): `MesajTaskDlg`

### LightVcl.Common.ExecuteShell.pas  -  1

- **test only** (1): `ExecuteFileAndWait`

### LightVcl.Common.Sound.pas  -  1

- **test only** (1): `PlayTone`

### LightVcl.Graph.FX.Gradient.pas  -  1

- **test only** (1): `DrawRedPattern`

### LightVcl.Graph.FX.pas  -  1

- **test only** (1): `TileBitmap`

### LightVcl.Graph.ResizeFMX.pas  -  1

- **test only** (1): `ResizeFmxF`

### LightVcl.Graph.UtilGray.pas  -  1

- **test only** (1): `GetAverageColorPf32`

### LightVcl.Internet.Download.Indy.pas  -  1

- **test only** (1): `DownloadThread`

### LightVcl.Visual.CalendarCanvas.pas  -  1

- **test only** (1): `CalculateDayOfYear`

### LightFmx.Common.Dialogs.pas  -  1

- **test only** (1): `ResolveErrorCaption`

### LightFmx.Common.Helpers.pas  -  1

- **test only** (1): `FindImmediateParentForm`

