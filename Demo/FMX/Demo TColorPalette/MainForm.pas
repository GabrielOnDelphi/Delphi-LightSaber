UNIT MainForm;

{-------------------------------------------------------------------------------------------------------------
   TColorPalette Demo

   Demonstrates the TColorPalette component:
     - Color selection
     - Displaying selected color (hex value and color preview)
     - Dynamic color set changes
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation,
  LightFmx.Visual.ColorPalette;

TYPE
  TFormPalette = class(TForm)
    ColorPalette1  : TColorPalette;
    btnReset       : TButton;
    btnAddColor: TButton;
    lblSelectedHex: TLabel;
    rectSelected: TRectangle;
    procedure ColorPalette1ColorSelected(Sender: TObject; const AColor: TAlphaColor);
    procedure btnResetClick(Sender: TObject);
    procedure btnAddColorClick(Sender: TObject);
    procedure btnClearColorsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure UpdateSelectedDisplay(AColor: TAlphaColor);
  public
  end;

VAR
  FormPalette: TFormPalette;


IMPLEMENTATION
{$R *.fmx}


procedure TFormPalette.FormCreate(Sender: TObject);
begin
  // Initialize the selected color display
  lblSelectedHex.Text := 'No color selected';
  rectSelected.Fill.Color := TAlphaColorRec.Null;
end;


{ Event fired when user clicks a color swatch }
procedure TFormPalette.ColorPalette1ColorSelected(Sender: TObject; const AColor: TAlphaColor);
begin
  UpdateSelectedDisplay(AColor);
end;


{ Update the UI to show the selected color }
procedure TFormPalette.UpdateSelectedDisplay(AColor: TAlphaColor);
begin
  // Show hex value
  lblSelectedHex.Text := 'Selected: #' + IntToHex(AColor, 8);

  // Show color preview
  rectSelected.Fill.Color := AColor;
end;


{ Reset to default color palette }
procedure TFormPalette.btnResetClick(Sender: TObject);
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
procedure TFormPalette.btnAddColorClick(Sender: TObject);
VAR
  RandomColor: TAlphaColor;
begin
  // Generate random RGB with full opacity
  RandomColor := TAlphaColor($FF000000 OR Cardinal(Random($FFFFFF)));
  ColorPalette1.AddColor(RandomColor);
end;


{ Clear all colors from the palette }
procedure TFormPalette.btnClearColorsClick(Sender: TObject);
begin
  ColorPalette1.ClearColors;
  lblSelectedHex.Text := 'No color selected';
  rectSelected.Fill.Color := TAlphaColorRec.Null;
end;


end.
