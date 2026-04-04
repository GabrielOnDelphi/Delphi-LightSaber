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
  FMX.Objects, LightFmx.Visual.SvgFlatButton;

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
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnAddColorClick(Sender: TObject);
    procedure btnClearColorsClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure ColorPalette1ColorSelected(Sender: TObject; const AColor: TAlphaColor);
    procedure UpdateSelectedDisplay(AColor: TAlphaColor);
  private
    lblClickFeedback: TLabel;
    procedure BuildFlatButtonDemo;
    procedure DemoButtonClick(Sender: TObject);
    procedure DemoToggleClick(Sender: TObject);
  public
  end;

var
  Form2: TForm2;

IMPLEMENTATION
{$R *.fmx}



procedure TForm2.FormCreate(Sender: TObject);
begin
  Caption:= 'LightPanel2.Visible is '+ BoolToStr(LightPanel2.Visible, TRUE);

  // Initialize the selected color display
  lblSelectedHex.Text := 'No color selected';
  rectSelected.Fill.Color := TAlphaColorRec.Null;

  BuildFlatButtonDemo;
end;



{=============================================================================================================
   FLAT BUTTON DEMO
=============================================================================================================}
CONST
  { Tabler Icons - 24x24 viewBox, stroke-based. Bounding-box path (M0 0h24v24H0z) excluded. }
  SVG_HOME     = 'M5 12l-2 0l9 -9l9 9l-2 0 M5 12v7a2 2 0 0 0 2 2h10a2 2 0 0 0 2 -2v-7 M9 21v-6a2 2 0 0 1 2 -2h2a2 2 0 0 1 2 2v6';
  SVG_SETTINGS = 'M10.325 4.317c.426 -1.756 2.924 -1.756 3.35 0a1.724 1.724 0 0 0 2.573 1.066c1.543 -.94 3.31 .826 2.37 2.37a1.724 1.724 0 0 0 1.065 2.572c1.756 .426 1.756 2.924 0 3.35a1.724 1.724 0 0 0 -1.066 2.573c.94 1.543 -.826 3.31 -2.37 2.37a1.724 1.724 0 0 0 -2.572 1.065c-.426 1.756 -2.924 1.756 -3.35 0a1.724 1.724 0 0 0 -2.573 -1.066c-1.543 .94 -3.31 -.826 -2.37 -2.37a1.724 1.724 0 0 0 -1.065 -2.572c-1.756 -.426 -1.756 -2.924 0 -3.35a1.724 1.724 0 0 0 1.066 -2.573c-.94 -1.543 .826 -3.31 2.37 -2.37c1 .608 2.296 .07 2.572 -1.065 M9 12a3 3 0 1 0 6 0a3 3 0 0 0 -6 0';
  SVG_USER     = 'M3 12a9 9 0 1 0 18 0a9 9 0 1 0 -18 0 M9 10a3 3 0 1 0 6 0a3 3 0 1 0 -6 0 M6.168 18.849a4 4 0 0 1 3.832 -2.849h4a4 4 0 0 1 3.834 2.855';
  SVG_STAR     = 'M12 17.75l-6.172 3.245l1.179 -6.873l-5 -4.867l6.9 -1l3.086 -6.253l3.086 6.253l6.9 1l-5 4.867l1.179 6.873z';
  SVG_BELL     = 'M10 5a2 2 0 1 1 4 0a7 7 0 0 1 4 6v3a4 4 0 0 0 2 3h-16a4 4 0 0 0 2 -3v-3a7 7 0 0 1 4 -6 M9 17v1a3 3 0 0 0 6 0v-1';
  SVG_SEARCH   = 'M10 10m-7 0a7 7 0 1 0 14 0a7 7 0 1 0 -14 0 M21 21l-6 -6';


procedure TForm2.BuildFlatButtonDemo;

  { Helper: creates a TFlatButton and parents it to TabItem3 }
  function MakeButton(X, Y, W, H: Single; CONST SvgData, Caption: string): TFlatButton;
  begin
    Result:= TFlatButton.Create(Self);
    Result.Parent:= TabItem3;
    Result.SetBounds(X, Y, W, H);
    if SvgData <> ''
    then Result.LoadSvgPath(SvgData);
    Result.TextLabel.Text:= Caption;
  end;

VAR
  btn: TFlatButton;
  lbl: TLabel;
  X, Y: Single;
begin
  X:= 16;
  Y:= 10;

  { Feedback label - shows which button was clicked }
  lblClickFeedback:= TLabel.Create(Self);
  lblClickFeedback.Parent:= TabItem3;
  lblClickFeedback.SetBounds(X, Y, 580, 22);
  lblClickFeedback.Text:= 'Click any button below...';
  Y:= Y + 35;

  { --- Row 1: Icon + Text (default layout, icon left) --- }
  lbl:= TLabel.Create(Self);
  lbl.Parent:= TabItem3;
  lbl.SetBounds(X, Y, 300, 18);
  lbl.Text:= 'Icon + Text (icon left):';
  Y:= Y + 22;

  btn:= MakeButton(X, Y, 130, 36, SVG_HOME, 'Home');
  btn.OnClick:= DemoButtonClick;

  btn:= MakeButton(X + 140, Y, 130, 36, SVG_SETTINGS, 'Settings');
  btn.OnClick:= DemoButtonClick;

  btn:= MakeButton(X + 280, Y, 130, 36, SVG_USER, 'Profile');
  btn.OnClick:= DemoButtonClick;

  Y:= Y + 50;

  { --- Row 2: Toggle buttons --- }
  lbl:= TLabel.Create(Self);
  lbl.Parent:= TabItem3;
  lbl.SetBounds(X, Y, 300, 18);
  lbl.Text:= 'Toggle mode (click to toggle):';
  Y:= Y + 22;

  btn:= MakeButton(X, Y, 140, 36, SVG_STAR, 'Favorite');
  btn.OnClick:= DemoToggleClick;

  btn:= MakeButton(X + 150, Y, 160, 36, SVG_BELL, 'Notifications');
  btn.OnClick:= DemoToggleClick;

  Y:= Y + 50;

  { --- Row 3: Icon only (no text) --- }
  lbl:= TLabel.Create(Self);
  lbl.Parent:= TabItem3;
  lbl.SetBounds(X, Y, 300, 18);
  lbl.Text:= 'Icon only (no text):';
  Y:= Y + 22;

  MakeButton(X,       Y, 36, 36, SVG_HOME, '').OnClick:= DemoButtonClick;
  MakeButton(X + 44,  Y, 36, 36, SVG_SETTINGS, '').OnClick:= DemoButtonClick;
  MakeButton(X + 88,  Y, 36, 36, SVG_USER, '').OnClick:= DemoButtonClick;
  MakeButton(X + 132, Y, 36, 36, SVG_SEARCH, '').OnClick:= DemoButtonClick;

  Y:= Y + 50;

  { --- Row 4: Icon top layout --- }
  lbl:= TLabel.Create(Self);
  lbl.Parent:= TabItem3;
  lbl.SetBounds(X, Y, 300, 18);
  lbl.Text:= 'Icon top:';
  Y:= Y + 22;

  btn:= MakeButton(X, Y, 70, 56, SVG_HOME, 'Home');
  btn.IconPosition:= ipTop;
  btn.OnClick:= DemoButtonClick;

  btn:= MakeButton(X + 80, Y, 70, 56, SVG_STAR, 'Star');
  btn.IconPosition:= ipTop;
  btn.OnClick:= DemoButtonClick;

  btn:= MakeButton(X + 160, Y, 70, 56, SVG_BELL, 'Alerts');
  btn.IconPosition:= ipTop;
  btn.OnClick:= DemoButtonClick;
end;


procedure TForm2.DemoButtonClick(Sender: TObject);
VAR btn: TFlatButton;
begin
  btn:= Sender as TFlatButton;
  if btn.TextLabel.Text <> ''
  then lblClickFeedback.Text:= 'Clicked: ' + btn.TextLabel.Text
  else lblClickFeedback.Text:= 'Clicked: (icon-only button)';
end;


procedure TForm2.DemoToggleClick(Sender: TObject);
VAR btn: TFlatButton;
begin
  btn:= Sender as TFlatButton;
  btn.IsToggled:= NOT btn.IsToggled;
  lblClickFeedback.Text:= btn.TextLabel.Text + ': toggled ' + BoolToStr(btn.IsToggled, TRUE);
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
  // Show hex value
  lblSelectedHex.Text := 'Selected: #' + IntToHex(AColor, 8);

  // Show color preview
  rectSelected.Fill.Color := AColor;
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
  // Generate random RGB with full opacity
  RandomColor := TAlphaColor($FF000000 OR Cardinal(Random($FFFFFF)));
  ColorPalette1.AddColor(RandomColor);
end;


{ Clear all colors from the palette }
procedure TForm2.btnClearColorsClick(Sender: TObject); // unused?
begin
  ColorPalette1.ClearColors;
  lblSelectedHex.Text := 'No color selected';
  rectSelected.Fill.Color := TAlphaColorRec.Null;
end;


end.
