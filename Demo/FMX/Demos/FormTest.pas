UNIT FormTest;

{=============================================================================================================
   2025.04
   www.GabrielMoraru.com
==============================================================================================================

   Demo form for LightSaber FMX visual components:
     Tab 1 - TLightPanel (VisibleAtRuntime property)
     Tab 2 - TLightColorPalette (color picker)
     Tab 3 - TFlatButton (flat SVG icon buttons)

=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Types, System.Classes, System.UITypes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  LightFmx.Visual.Panel, FMX.TabControl, FMX.Layouts, LightFmx.Visual.ColorPalette,
  FMX.Objects, LightFmx.Visual.SvgFlatButton, FMX.Ani,
  LightFmx.Common.AppData, FormSkinsDisk, LightFmx.Visual.CheckBox;

TYPE
  TForm2 = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Button1: TButton;
    LightPanel2: TLightPanel;
    lblVisible: TLabel;
    LightPanel1: TLightPanel;
    Label1: TLabel;
    btnAddColor: TButton;
    btnReset: TButton;
    rectSelected: TRectangle;
    ColorPalette1: TLightColorPalette;
    lblSelectedHex: TLabel;
    TabItem3: TTabItem;
    { TFlatButton demo - design-time components }
    lblFeedback: TLabel;
    Layout1: TLayout;
    btnTopHome: TFlatButton;
    btnTopStar: TFlatButton;
    btnTopBell: TFlatButton;
    lblRow4: TLabel;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    lblRow3: TLabel;
    btnSmall1: TFlatButton;
    btnSmall2: TFlatButton;
    btnSmall3: TFlatButton;
    btnSmall4: TFlatButton;
    btnNotify: TFlatButton;
    btnFavorite: TFlatButton;
    lblRow2: TLabel;
    btnHome: TFlatButton;
    lblRow1: TLabel;
    btnSettings: TFlatButton;
    btnProfile: TFlatButton;
    TabItem4: TTabItem;
    Button2: TButton;
    StyleBook1: TStyleBook;
    Label2: TLabel;
    Layout5: TLayout;
    chkHover: TLightCheckBox;
    Layout6: TLayout;
    FlatButton1: TFlatButton;
    FlatButton2: TFlatButton;
    FlatButton3: TFlatButton;
    Label3: TLabel;
    chkAutoCompact: TLightCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnAddColorClick(Sender: TObject);
    procedure btnClearColorsClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure ColorPalette1ColorSelected(Sender: TObject; const AColor: TAlphaColor);
    procedure UpdateSelectedDisplay(AColor: TAlphaColor);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure chkHoverChange(Sender: TObject);
    procedure chkAutoCompactChange(Sender: TObject);
  private
    chkCompact  : TLightCheckBox;
    chkToggled  : TLightCheckBox;
    chkShowText : TLightCheckBox;
    procedure SetupFlatButtons;
    procedure chkCompactChange(Sender: TObject);
    procedure chkToggledChange(Sender: TObject);
    procedure chkShowTextChange(Sender: TObject);
    procedure DemoButtonClick(Sender: TObject);
    procedure DemoToggleClick(Sender: TObject);
  public
  end;

var
  Form2: TForm2;

IMPLEMENTATION
{$R *.fmx}


CONST
  { Tabler Icons - 24x24 viewBox, stroke-based. Bounding-box path excluded. }
  SVG_HOME     = 'M5 12l-2 0l9 -9l9 9l-2 0 M5 12v7a2 2 0 0 0 2 2h10a2 2 0 0 0 2 -2v-7 M9 21v-6a2 2 0 0 1 2 -2h2a2 2 0 0 1 2 2v6';
  SVG_SETTINGS = 'M10.325 4.317c.426 -1.756 2.924 -1.756 3.35 0a1.724 1.724 0 0 0 2.573 1.066c1.543 -.94 3.31 .826 2.37 2.37a1.724 1.724 0 0 0 1.065 2.572c1.756 .426 1.756 2.924 0 3.35a1.724 1.724 0 0 0 -1.066 2.573c.94 1.543 -.826 3.31 -2.37 2.37a1.724 1.724 0 0 0 -2.572 1.065c-.426 1.756 -2.924 1.756 -3.35 0a1.724 1.724 0 0 0 -2.573 -1.066c-1.543 .94 -3.31 -.826 -2.37 -2.37a1.724 1.724 0 0 0 -1.065 -2.572c-1.756 -.426 -1.756 -2.924 0 -3.35a1.724 1.724 0 0 0 1.066 -2.573c-.94 -1.543 .826 -3.31 2.37 -2.37c1 .608 2.296 .07 2.572 -1.065 M9 12a3 3 0 1 0 6 0a3 3 0 0 0 -6 0';
  SVG_USER     = 'M3 12a9 9 0 1 0 18 0a9 9 0 1 0 -18 0 M9 10a3 3 0 1 0 6 0a3 3 0 1 0 -6 0 M6.168 18.849a4 4 0 0 1 3.832 -2.849h4a4 4 0 0 1 3.834 2.855';
  SVG_STAR     = 'M12 17.75l-6.172 3.245l1.179 -6.873l-5 -4.867l6.9 -1l3.086 -6.253l3.086 6.253l6.9 1l-5 4.867l1.179 6.873z';
  SVG_BELL     = 'M10 5a2 2 0 1 1 4 0a7 7 0 0 1 4 6v3a4 4 0 0 0 2 3h-16a4 4 0 0 0 2 -3v-3a7 7 0 0 1 4 -6 M9 17v1a3 3 0 0 0 6 0v-1';
  SVG_SEARCH   = 'M10 10m-7 0a7 7 0 1 0 14 0a7 7 0 1 0 -14 0 M21 21l-6 -6';


procedure TForm2.FormCreate(Sender: TObject);
begin
  Caption:= 'LightPanel2.Visible is '+ BoolToStr(LightPanel2.Visible, TRUE);
  lblSelectedHex.Text:= 'No color selected';
  rectSelected.Fill.Color:= TAlphaColorRec.Null;

  SetupFlatButtons;

  { Warm up the FMX glow effect pipeline.
    The first time a TGlowEffect is enabled, FMX initializes the Direct2D effect
    infrastructure (filter graph, offscreen buffers). This one-time cost causes a
    ~1 second delay on the first hover. Toggling IsToggled here pays the cost at startup. }
  btnHome.IsToggled:= TRUE;
  btnHome.IsToggled:= FALSE;
end;



{=============================================================================================================
   FLAT BUTTON DEMO
   Buttons are placed at design time in the .fmx.
   SVG icons and event handlers are assigned here (SVG path data can't be set in the designer).
=============================================================================================================}

procedure TForm2.SetupFlatButtons;
begin
  { Row 1: Icon + Text }
  btnHome.LoadSvgPath(SVG_HOME);
  btnHome.Text:= 'Home';
  btnHome.OnClick:= DemoButtonClick;

  btnSettings.LoadSvgPath(SVG_SETTINGS);
  btnSettings.Text:= 'Settings';
  btnSettings.OnClick:= DemoButtonClick;

  btnProfile.LoadSvgPath(SVG_USER);
  btnProfile.Text:= 'Profile';
  btnProfile.OnClick:= DemoButtonClick;

  { Row 2: Toggle mode }
  btnFavorite.LoadSvgPath(SVG_STAR);
  btnFavorite.Text:= 'Favorite';
  btnFavorite.OnClick:= DemoToggleClick;

  btnNotify.LoadSvgPath(SVG_BELL);
  btnNotify.Text:= 'Notifications';
  btnNotify.OnClick:= DemoToggleClick;

  { Row 3: Icon only (ipCenter) }
  btnSmall1.LoadSvgPath(SVG_HOME);
  btnSmall1.OnClick:= DemoButtonClick;

  btnSmall2.LoadSvgPath(SVG_SETTINGS);
  btnSmall2.OnClick:= DemoButtonClick;

  btnSmall3.LoadSvgPath(SVG_USER);
  btnSmall3.OnClick:= DemoButtonClick;

  btnSmall4.LoadSvgPath(SVG_SEARCH);
  btnSmall4.OnClick:= DemoButtonClick;

  { Row 4: Label under icon (ipTop) - IconPosition set in .fmx }
  btnTopHome.LoadSvgPath(SVG_HOME);
  btnTopHome.Text:= 'Home';
  btnTopHome.OnClick:= DemoButtonClick;

  btnTopStar.LoadSvgPath(SVG_STAR);
  btnTopStar.Text:= 'Star';
  btnTopStar.OnClick:= DemoButtonClick;

  btnTopBell.LoadSvgPath(SVG_BELL);
  btnTopBell.Text:= 'Alerts';
  btnTopBell.OnClick:= DemoButtonClick;

  { Custom row: icon+text buttons for Compact / AutoCompact testing }
  FlatButton1.LoadSvgPath(SVG_HOME);
  FlatButton1.Text:= 'Home';
  FlatButton1.IconPosition:= ipLeft;
  FlatButton1.OnClick:= DemoButtonClick;

  FlatButton2.LoadSvgPath(SVG_SETTINGS);
  FlatButton2.Text:= 'Settings';
  FlatButton2.IconPosition:= ipLeft;
  FlatButton2.OnClick:= DemoButtonClick;

  FlatButton3.LoadSvgPath(SVG_USER);
  FlatButton3.Text:= 'Profile';
  FlatButton3.IconPosition:= ipLeft;
  FlatButton3.OnClick:= DemoButtonClick;

  { Runtime checkboxes — created alongside the design-time chkHover and chkAutoCompact in Layout5 }
  chkCompact:= TLightCheckBox.Create(Self);
  chkCompact.Parent:= Layout5;
  chkCompact.Align:= TAlignLayout.Top;
  chkCompact.Text:= 'Compact (manual)';
  chkCompact.Margins.Rect:= TRectF.Create(3, 3, 3, 3);
  chkCompact.OnChange:= chkCompactChange;

  chkToggled:= TLightCheckBox.Create(Self);
  chkToggled.Parent:= Layout5;
  chkToggled.Align:= TAlignLayout.Top;
  chkToggled.Text:= 'IsToggled';
  chkToggled.Margins.Rect:= TRectF.Create(3, 3, 3, 3);
  chkToggled.OnChange:= chkToggledChange;

  chkShowText:= TLightCheckBox.Create(Self);
  chkShowText.Parent:= Layout5;
  chkShowText.Align:= TAlignLayout.Top;
  chkShowText.Text:= 'Show text';
  chkShowText.IsChecked:= True;
  chkShowText.Margins.Rect:= TRectF.Create(3, 3, 3, 3);
  chkShowText.OnChange:= chkShowTextChange;
end;


procedure TForm2.DemoButtonClick(Sender: TObject);
VAR btn: TFlatButton;
begin
  btn:= Sender as TFlatButton;
  if btn.Text <> ''
  then lblFeedback.Text:= 'Clicked: ' + btn.Text
  else lblFeedback.Text:= 'Clicked: (icon-only button)';
end;


procedure TForm2.DemoToggleClick(Sender: TObject);
VAR btn: TFlatButton;
begin
  btn:= Sender as TFlatButton;
  btn.IsToggled:= NOT btn.IsToggled;
  lblFeedback.Text:= btn.Text + ': toggled ' + BoolToStr(btn.IsToggled, TRUE);
end;



procedure TForm2.Button3Click(Sender: TObject);
begin
end;



{=============================================================================================================
   SKINS
=============================================================================================================}

procedure TForm2.Button2Click(Sender: TObject);
VAR Skins: TfrmStyleDisk;
begin  //ToDo 5: don't create it again if already exists!
  AppData.CreateForm(TfrmStyleDisk, Skins);     //del: if we want to be able to work with non-modal forms, the modal form needs to inform us when the user changed the style. We need an EVENT or callback in TfrmStyleDisk
  Skins.Show;
end;




{=============================================================================================================
   LIGHT PANEL
=============================================================================================================}

procedure TForm2.Button1Click(Sender: TObject);
begin
  LightPanel2.Visible:= TRUE;
end;



{=============================================================================================================
   COLOR PALETE
=============================================================================================================}

{ Event fired when user clicks a color swatch }
procedure TForm2.ColorPalette1ColorSelected(Sender: TObject; const AColor: TAlphaColor);
begin
  UpdateSelectedDisplay(AColor);
end;


{ Update the UI to show the selected color }
procedure TForm2.UpdateSelectedDisplay(AColor: TAlphaColor);
begin
  lblSelectedHex.Text:= 'Selected: #' + IntToHex(AColor, 8);
  rectSelected.Fill.Color:= AColor;
end;


{ Reset to default color palette }
procedure TForm2.btnResetClick(Sender: TObject);
begin
  ColorPalette1.SetColors([
    TAlphaColorRec.Black,     TAlphaColorRec.Darkgray,  TAlphaColorRec.Gray,
    TAlphaColorRec.Lightgray, TAlphaColorRec.White,     TAlphaColorRec.Red,
    TAlphaColorRec.Orange,    TAlphaColorRec.Yellow,    TAlphaColorRec.Lime,
    TAlphaColorRec.Cyan,      TAlphaColorRec.Blue,      TAlphaColorRec.Purple,
    TAlphaColorRec.Magenta,   TAlphaColorRec.Pink,      TAlphaColorRec.Brown,
    TAlphaColorRec.Navy,      TAlphaColorRec.Teal,      TAlphaColorRec.Olive
  ]);
end;


{ Add a random color to the palette }
procedure TForm2.btnAddColorClick(Sender: TObject);
VAR
  RandomColor: TAlphaColor;
begin
  RandomColor:= TAlphaColor($FF000000 OR Cardinal(Random($FFFFFF)));
  ColorPalette1.AddColor(RandomColor);
end;


{ Clear all colors from the palette }
procedure TForm2.btnClearColorsClick(Sender: TObject); // unused?
begin
  ColorPalette1.ClearColors;
  lblSelectedHex.Text:= 'No color selected';
  rectSelected.Fill.Color:= TAlphaColorRec.Null;
end;


procedure TForm2.chkHoverChange(Sender: TObject);
VAR i: Integer;
begin
  for i:= 0 to Layout6.ControlsCount - 1 do
    if Layout6.Controls[i] is TFlatButton
    then TFlatButton(Layout6.Controls[i]).HoverBackground:= chkHover.IsChecked;
end;


procedure TForm2.chkAutoCompactChange(Sender: TObject);
VAR i: Integer;
begin
  for i:= 0 to Layout6.ControlsCount - 1 do
    if Layout6.Controls[i] is TFlatButton
    then TFlatButton(Layout6.Controls[i]).AutoCompact:= chkAutoCompact.IsChecked;
end;


procedure TForm2.chkCompactChange(Sender: TObject);
VAR i: Integer;
begin
  for i:= 0 to Layout6.ControlsCount - 1 do
    if Layout6.Controls[i] is TFlatButton
    then TFlatButton(Layout6.Controls[i]).Compact:= chkCompact.IsChecked;
end;


procedure TForm2.chkToggledChange(Sender: TObject);
VAR i: Integer;
begin
  for i:= 0 to Layout6.ControlsCount - 1 do
    if Layout6.Controls[i] is TFlatButton
    then TFlatButton(Layout6.Controls[i]).IsToggled:= chkToggled.IsChecked;
end;


{ Toggles button text on/off. Saves original text to TagString before clearing. }
procedure TForm2.chkShowTextChange(Sender: TObject);
VAR
  i: Integer;
  Btn: TFlatButton;
begin
  for i:= 0 to Layout6.ControlsCount - 1 do
    if Layout6.Controls[i] is TFlatButton then
      begin
        Btn:= TFlatButton(Layout6.Controls[i]);
        if chkShowText.IsChecked then
          begin
            if Btn.TagString <> ''
            then Btn.Text:= Btn.TagString;
          end
        else
          begin
            if Btn.Text <> ''
            then Btn.TagString:= Btn.Text;
            Btn.Text:= '';
          end;
      end;
end;

end.






