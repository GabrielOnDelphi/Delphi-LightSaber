# SVG icons in a VCL program - how to use them

Every icon of a VCL program can be a solid one-colour shape, written as SVG text in a Pascal constant and drawn with GDI+ at any size.
Use it when a program needs icons that stay sharp at every DPI and stay readable under every VCL style (skin), without bitmap files.

BioniX (`c:\Projects\BioniX\SourceCode\`) was the first program to do it, in October 2026: every menu, toolbar and button icon.
This guide lets a session that never saw BioniX do the same in another VCL program.
FMX programs use other units, not described here: `c:\Projects\LightSaber\FrameFMX\LightFmx.Visual.SvgButton.pas` and `c:\Projects\LightSaber\FrameFMX\LightFmx.Visual.SvgFlatButton.pas`.

## The rules (Gabriel's decisions)

1. **A solid filled shape, in white or `#202020`, never an accent colour.** The routines pick one of these two; the shape keeps no colour of its own.
2. **The same shapes in every mode**, also with no skin ("Windows default theme").
3. **No Skia.** Gabriel: "I don't really want to make LightSaber dependent on skia." The drawing is GDI+ only (`Winapi.GDIPOBJ`, part of Windows), through the two LightSaber units below. No DLL ships.
4. **Never adapt the look to a skin's name.** Measure the colours the skin really paints behind the icon. One skin can have a dark window with white menus and silver buttons (Obsidian, see "Traps"). The same rule is in `c:\Projects\CLAUDE.md`, section GUI.

## The two units

| Unit | What it does | Package |
|---|---|---|
| `c:\Projects\LightSaber\LightCore.SvgShapes.pas` | Reads SVG text into a list of shapes (`ParseSvg` returns a `TSvgImage`). Draws nothing. RTL only, so a console or Android build can use it too. | `LightCore.dpk` |
| `c:\Projects\LightSaber\FrameVCL\LightVcl.Visual.SvgSpeedButton.pas` | Draws the shapes with GDI+: the button `TLightSvgSpeedButton`, the routines for image lists and `TButton`, and the routines that measure the skin's colours. | `LightVcl.Visual.dpk` |

A form needs only `uses LightVcl.Visual.SvgSpeedButton`.
Tests: `c:\Projects\LightSaber\UnitTesting\Test.LightCore.SvgShapes.pas` and `c:\Projects\LightSaber\UnitTesting\Test.LightVcl.Visual.SvgSpeedButton.pas`.

## 1. The SVG subset that `LightCore.SvgShapes` reads

| Part | Supported |
|---|---|
| Elements | `svg` (`viewBox`, or `width` + `height`), `path`, `rect` (with `rx`, `ry`), `circle`, `ellipse`, `polygon`. `title`, `desc`, `metadata`, comments and namespaced elements (`inkscape:...`, `sodipodi:...`) are skipped. |
| Path data | `M L H V C Z` and the relative forms `m l h v c z`. Packed numbers (`M10-5.5.5`) and exponents. |
| Paint | `fill` and `stroke` = `none`, `currentColor`, `#rgb`, `#rrggbb`, `black`, `white`. `stroke-width`, `fill-rule`. Set on the `<svg>` element, they are inherited by every shape. |
| Transform | `matrix`, `translate`, `scale`, `rotate(a)`, `rotate(a cx cy)`, and a list of them. |

Everything else that changes the drawing raises `ESvgUnsupported`, whose message names the thing: arcs (`A`), quadratic and smooth curves (`Q T S`), gradients, `<g>`, `<use>`, the `style` attribute (CSS), and also `opacity`, `stroke-linecap`, `stroke-linejoin`, `stroke-dasharray` and the other attributes in the constant `UnsupportedAttributes`.
Empty or malformed text raises `ESvgError` (`ESvgUnsupported` descends from it).
So an icon copied from an icon library needs a hand edit when it uses any of these.

The colour named in the SVG text plays no part: every shape is drawn in the one colour the caller passes.
BioniX writes every shape in a viewBox `0 0 24 24`, with 2-3 units of margin, strokes of at least 2 units and no detail smaller than 2 units, because at 16 px one unit is 0.67 px.

## 2. A button: `TLightSvgSpeedButton`

`TLightSvgSpeedButton` is a `TSpeedButton` descendant.
Its `Paint` calls the inherited `Paint` first, so the skin (or Windows) still draws the face and the hot and pressed frames; then the icon is blended on top, centred.

| Property or routine | Meaning |
|---|---|
| `SvgText` | The SVG text. Assigning a text the reader cannot read raises `ESvgError` at once. |
| `GlyphColor`, `HotColor`, `PressedColor` | The icon colour in the normal, mouse-over and pressed state. Default `clBlack`. |
| `DisabledOpacity` | The disabled state draws `GlyphColor` at this opacity, 0..255. Default 102 = 40 %. |
| `GlyphSize` | The icon side in pixels. 0 = 64 % of the smaller side of the button. |
| `PickColorsFromStyle` | Sets the three colours to white or `#202020` against the colour the skin draws: the panel behind the button, and the hot and pressed tool-button faces. Made for a flat button that sits on a `TPanel`. |
| `ActualGlyphSize`, `GlyphBitmap` | The size and the cached bitmap of one state (used by tests). |

```pascal
uses LightVcl.Visual.SvgSpeedButton;

btnNext:= TLightSvgSpeedButton.Create(Self);
btnNext.Parent:= pnlToolbar;
btnNext.Flat:= TRUE;
btnNext.SvgText:= SvgNextItem;     { A string constant holding the SVG text }
btnNext.PickColorsFromStyle;       { After the skin is active }
```

BioniX creates these buttons in code, not in the DFM: a bad `SvgText` stored in a DFM would raise while the form loads.

**How the glyph size is chosen in BioniX.** The mini-monitor taskbar resizes with the window.
`TMonitorVis.squareControls` in `c:\Projects\BioniX\SourceCode\BioniX VCL\vclMonitor.pas` makes every taskbar control a square as tall as the taskbar and sets `GlyphSize:= Max(1, RoundEx(Side * GlyphPercent))`, with the constant `GlyphPercent = 0.64` (64 % of the button side).
`TMonitorVis.ArrangeTaskBar` calls it, and `TMonitorVis.SetSize` calls `ArrangeTaskBar` on every resize.

## 3. Icons that are not on a `TLightSvgSpeedButton`

| Target | Routine |
|---|---|
| A `TImageList` (menus, `TToolBar`, `TActionList`) | `SvgToImageList(ImageList, Index, SvgText, Color, OutlineColor)`. The list must have `ColorDepth = cd32Bit`. The picture is drawn at `Min(Width, Height)` of the list and centred. `Index < Count` replaces that picture, `Index = Count` appends one, any other index raises `ESvgRenderError`. |
| A `TButton` | `SetButtonSvg(Button, SvgText, Color, GlyphSize, OutlineColor)`. Creates an image list owned by the button: picture 0 is the glyph (`ImageIndex`), picture 1 the same glyph at 40 % (`DisabledImageIndex`). `GlyphSize` 0 = 64 % of the smaller button side. A second call replaces both pictures. Raises when the button already uses an image list that `SetButtonSvg` did not create. |
| A bitmap of your own | `RenderSvgGlyph(ParseSvg(SvgText), Size, Color, OutlineColor)` returns a `pf32bit` bitmap with premultiplied alpha. The caller frees it. |

`SetButtonSvg` takes a `TButton`. A `TBitBtn` descends from `TCustomButton`, not from `TButton` (`c:\Delphi\Delphi 13\source\vcl\Vcl.Buttons.pas`: `TBitBtn = class(TCustomButton)`), so `SetButtonSvg` does not accept it, and BioniX has no `TBitBtn` with an SVG icon.
`TBitBtn` publishes `Images`, `ImageIndex` and `DisabledImageIndex`, so a `cd32Bit` image list filled with `SvgToImageList` should work [not tried]. The public routines have no 40 % opacity parameter, so such a button gets no faded disabled picture.

Each `SvgToImageList` call fires `OnChange` of the list: wrap many calls in `BeginUpdate` / `EndUpdate`.
An image list holds one picture per index, so a menu or toolbar icon has no hot colour.
For the disabled state of an image list, BioniX sets `ImageList.DisabledStyle:= diDesaturateBlend50`. It was measured as the only `TDisabledStyle` that makes both a white and a `#202020` picture fainter.

```pascal
GlyphColor:= StyleGlyphColor(Self);     { Or BestContrastColorOn, see section 4 }
imgList.BeginUpdate;
try
  SvgToImageList(imgList, 5, SvgGear, GlyphColor, GlyphOutlineColor(GlyphColor));
  SvgToImageList(imgList, imgList.Count, SvgPlay, GlyphColor, GlyphOutlineColor(GlyphColor));   { Appends }
finally
  imgList.EndUpdate;
end;
imgList.DisabledStyle:= diDesaturateBlend50;

SetButtonSvg(btnPlay, SvgPlay, StyleButtonGlyphColor(btnPlay), 0, GlyphOutlineColor(StyleButtonGlyphColor(btnPlay)));
```

All these routines raise `ESvgRenderError` (or `ESvgError` for SVG text they cannot read) and never show a dialog.

## 4. Colour

**Draw the icons after the skin is active.** `FormSkinsDisk.LoadLastStyle` runs in the DPR before `AppData.CreateMainForm`, so the `FormCreate` of the main form is the first place where the skin and the DFM image list both exist. BioniX does it in `TfrmBioniX.FormCreate` (`c:\Projects\BioniX\SourceCode\MainForm.pas`).
BioniX applies a new skin only at the next start. A program that changes the skin while it runs must draw the icons again after the change [not tried].

**Measure the surface, then pick white or `#202020`.** Each routine below draws the skin's own element into a small bitmap and reads the centre pixel; with no skin it returns the Windows colour in brackets.

| Routine | The surface it measures |
|---|---|
| `StyleMenuColor` | a popup menu, behind its items (`clMenu`) |
| `StyleMenuBarColor` | the menu bar of a form (`clMenuBar`) |
| `StyleButtonFaceColor` | a `TButton` in its normal state (`clBtnFace`) |
| `StyleToolButtonColor` | a `TToolButton` of a `TToolBar` in its normal state (`clBtnFace`) |
| `StyleMenuHighlightColor` | a selected menu item (`clHighlight`) |
| `StyleGlyphColor` | already the answer: white or `#202020` against the panel colour (`clBtnFace`) |
| `StyleButtonGlyphColor` | already the answer: white or `#202020` against `StyleButtonFaceColor` |

Then:
- `BestContrastColor(Background)` - white or `#202020`, whichever has the higher `ContrastRatio` (the WCAG 2 contrast ratio, 1 to 21) on that background.
- `BestContrastColorOn([...])` - for one image list whose pictures appear on several surfaces: the colour whose worst contrast over all the surfaces is the best.

BioniX, in `TfrmBioniX.FormCreate`:

```pascal
GlyphColor   := BestContrastColorOn([StyleMenuColor(Self), StyleButtonFaceColor(Self), StyleToolButtonColor(Self)]);
BarGlyphColor:= BestContrastColor(StyleMenuBarColor(Self));
ApplySvgIcons    (imgCollection, GlyphColor, GlyphOutlineColor(GlyphColor));
ApplyMenuBarIcons(imgCollection, MainMenu, BarGlyphColor, GlyphOutlineColor(BarGlyphColor));
```

The menu bar gets its own copies of its pictures in its own colour, because the bar can be dark while the popup menus are light. `TfrmBioniX.FormCreate` also writes the measured colours to the log (`AppDataCore.LogInfo`, line "SVG icons: menu ..."): read it there when a skin looks wrong.

**The dark rim on white icons.** `GlyphOutlineColor(GlyphColor)` returns `#202020` for a white glyph and `clNone` for any other colour.
Pass it as `OutlineColor`: every shape is then stroked with a 1.5 px pen in that colour and filled on top, which leaves a thin dark rim outside the shape.
Gabriel's rule (2026-10-30, "always rim"): every white glyph gets the rim, in image lists and `SetButtonSvg` buttons alike. Reason: a white glyph is chosen for a dark skin, and it vanishes on that skin's light hot, selected or focused surfaces, which are not measured one by one.

**The disabled state is 40 %:** `TLightSvgSpeedButton.DisabledOpacity` (default 102) and picture 1 of `SetButtonSvg`. An image list uses `diDesaturateBlend50` (section 3).

## 5. Where BioniX keeps its shapes - the example to copy

`c:\Projects\BioniX\SourceCode\BioniX VCL\BxSvgIcons.pas`:
- One string constant per shape (`SvgGear`, `SvgPlay`, `SvgNextItem`, ...), each with a one-line comment saying what it draws.
- `ReplacedIcons` - a table of (picture index in `imgCollection`, SVG text) for the pictures of `MainForm.dfm` that get redrawn. `AppendedIcons` - the shapes appended after them. `DfmPictureCount` guards against a changed DFM.
- `ApplySvgIcons` draws both tables into the image list and sets `DisabledStyle`. `ApplyMenuBarIcons` gives the menu bar its copies. `SvgOfPicture` returns the SVG text of one picture index.
- Tests: `c:\Projects\BioniX\SourceCode\BioniX VCL\UnitTesting\TestBxSvgIcons.pas`.

The image list `imgCollection` stays in `MainForm.dfm` with its old 16x16 pictures; the shapes are drawn over them at start. Going back to the old icons is removing the one call in `TfrmBioniX.FormCreate`, plus the `ImageIndex` changes of the actions that use the appended pictures.
Other callers to look at: `c:\Projects\BioniX\SourceCode\BioniX VCL\FormCountDownEditor.pas` (`SetButtonSvg`), `c:\Projects\BioniX\SourceCode\BioniX VCL\pluginAnimCtrlForm.pas` (`PickColorsFromStyle`).

## 6. How to check the result on screen

Capture every window that shows icons under three skins: no skin, one dark skin and one light skin. BioniX used no skin, Auric (dark), Obsidian (dark window, light menus and buttons) and AmethystKamri (light).
- Choose the skin before the start: `FormSkinsDisk.LoadLastStyle` reads the INI key `LastStyle` (a `.vsf` file name, or `Windows default theme` for no skin). Edit it while the program is closed.
- Capture with `c:\AI\Tools\GUI driving\Capture-Window.ps1`; `-Scale 4` enlarges small glyphs pixel by pixel. Open the `.png` with the Read tool.
- Capture an open menu with `-Screen`: in BioniX, `PrintWindow` half-painted two menus under no skin.
- Look at each icon in the normal, hot, pressed, disabled and highlighted-menu-item states, and compare with the log line of the measured colours.

BioniX's captures and verdicts: `c:\Projects\BioniX\Experiment in progress - SVG icons\Screenshots\` (`Judgement.md`, `Judgement - rim.md`, `Judgement - surfaces.md`).

## 7. Traps found in BioniX

- **Auric: a white glyph on the yellow menu highlight almost vanished.** Measured highlight `3AD9FF` (a `TColor`, so BGR: yellow). Fixed by the dark rim, now on every white glyph.
- **Obsidian: dark panel, but white popup menus and silver buttons.** Measured: menu `FFFBFF`, button `D7D1CD`, tool button `BAB8B6`, menu bar `535353`. The first version took the colour from the panel only (`StyleGlyphColor`) and drew white glyphs on the white menus. Fixed by measuring each surface and using `BestContrastColorOn`; the dark menu bar keeps white glyphs through `ApplyMenuBarIcons`.
- **The mini-monitor gear came out about 7 px.** It sits on the wallpaper picture, not on a skin surface. Fixed: an own dark square (`pnlSettings`, colour `$303030`, `StyleElements:= []` so no skin repaints it), 72 % of the taskbar height, with the white gear at 80 % of the square (16 px on the smallest taskbar). A glyph on a picture needs its own fixed background and a fixed white colour, not the skin colours.
- **A disabled white glyph on a `TButton` was unreadable.** Without a disabled picture, `TButton` builds one itself and the white nearly vanishes. Fixed by picture 1 at 40 % in `SetButtonSvg`.
- **Under a VCL skin, a script cannot open the main menu from outside.** `GetMenu` returns 0, so `Press-VclControl.ps1 -MenuPath` fails. To drive the menus, set no skin in the INI first.
- **A `TSpeedButton` does not publish `OnDragOver` and `OnDragDrop`** (compiler error E2362). BioniX replaced a `TLabel` that took drops with a `TLightSvgSpeedButton` and reached the two events through a cracker class (`TSettingsButtonAccess` in `vclMonitor.pas`).
- **Not seen on screen:** the hot frame of a taskbar button under Auric, and a real Windows scaling of 150 % or 200 % (only renders at 24, 36 and 48 px were checked).
