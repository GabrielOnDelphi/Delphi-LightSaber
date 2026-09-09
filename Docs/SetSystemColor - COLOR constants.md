Explains the `PropertyToChange` parameter of `SetSystemColor` in `c:\Projects\LightSaber\FrameVCL\LightVcl.Graph.Desktop.pas`

`PropertyToChange` is one of the `COLOR_*` constants declared in `Winapi.Windows`. Each one names a part of the Windows colour scheme. Several names are aliases of each other, and those are grouped on one row below.

Microsoft's own list: https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-setsyscolors

| Constant | What it colours |
|---|---|
| `COLOR_3DDKSHADOW` | Dark shadow for three-dimensional display elements |
| `COLOR_3DFACE`, `COLOR_BTNFACE` | Face colour for three-dimensional display elements |
| `COLOR_3DHILIGHT`, `COLOR_3DHIGHLIGHT`, `COLOR_BTNHILIGHT`, `COLOR_BTNHIGHLIGHT` | Highlight colour for three-dimensional display elements, on the edges facing the light source |
| `COLOR_3DLIGHT` | Light colour for three-dimensional display elements, on the edges facing the light source |
| `COLOR_3DSHADOW`, `COLOR_BTNSHADOW` | Shadow colour for three-dimensional display elements, on the edges facing away from the light source |
| `COLOR_ACTIVEBORDER` | Active window border |
| `COLOR_ACTIVECAPTION` | Active window caption |
| `COLOR_APPWORKSPACE` | Background of a multiple document interface (MDI) application |
| `COLOR_BACKGROUND`, `COLOR_DESKTOP` | The desktop |
| `COLOR_BTNTEXT` | Text on push buttons |
| `COLOR_CAPTIONTEXT` | Text in a caption, a size box and a scroll bar arrow box |
| `COLOR_GRAYTEXT` | Greyed (disabled) text. Set to 0 when the display driver does not support a solid grey |
| `COLOR_HIGHLIGHT` | The selected items in a control |
| `COLOR_HIGHLIGHTTEXT` | Text of the selected items in a control |
| `COLOR_INACTIVEBORDER` | Inactive window border |
| `COLOR_INACTIVECAPTION` | Inactive window caption |
| `COLOR_INACTIVECAPTIONTEXT` | Text in an inactive caption |
| `COLOR_INFOBK` | Background of a tooltip control |
| `COLOR_INFOTEXT` | Text of a tooltip control |
| `COLOR_MENU` | Menu background |
| `COLOR_MENUTEXT` | Text in a menu |
| `COLOR_SCROLLBAR` | The grey area of a scroll bar |
| `COLOR_WINDOW` | Window background |
| `COLOR_WINDOWFRAME` | Window frame |
| `COLOR_WINDOWTEXT` | Text in a window |
