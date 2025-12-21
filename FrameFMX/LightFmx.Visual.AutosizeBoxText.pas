UNIT LightFmx.Visual.AutosizeBoxText;

{-------------------------------------------------------------------------------------------------------------
   GabrielMoraru.com
   2025.12
--------------------------------------------------------------------------------------------------------------
   Same as LightFmx.Visual.AutosizeBox.pas but it adds text (caption) capabilities.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Classes, System.Math,
  FMX.StdCtrls, FMX.Graphics, FMX.Types, FMX.Controls, FMX.Objects,
  LightFmx.Visual.AutoSizeBox;

TYPE
  TAutosizeBoxText = class(TAutoSizeBox)
  private
    FTextLabel: TLabel;
    FText: string;
    procedure SetText(const Value: string);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    property Text: string read FText write SetText;
    procedure UpdateSize; override;
  end;


function MakeTextBubble(Parent: TControl; const Text: string; BoxType: TBoxType): TAutosizeBoxText;
procedure Register;


IMPLEMENTATION


constructor TAutosizeBoxText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Internal label
  FTextLabel := TLabel.Create(Self);
  FTextLabel.Parent   := Self;
  FTextLabel.Align    := TAlignLayout.Top; // This is important!
  FTextLabel.AutoSize := TRUE;
  FTextLabel.WordWrap := True;
  FTextLabel.HitTest  := False;
  FTextLabel.Stored   := False;            //  tells the Form Designer not to write the component's state to the .fmx or .dfm file when the parent component (TAutoHeightRectangle) is created at design time. Since the TAutoHeightRectangle's constructor already creates and configures the internal FTextLabel, we don't want the DFM to create it again, which would lead to duplicate controls and memory leaks if the component user were to place the control on a form and save it.
  FTextLabel.TextSettings.Font.Size := 20;

  // Set initial text directly to the label. Do NOT call SetText or Resize here.
  FText := 'Self-sizing WhatApp-like bubble. ' +
    'We are checking if the TAutoHeightRectangle component can correctly detect the wrapped height of its TLabel child and adjust its own Height property accordingly when the width of the boxConversation changes. ' + #13#10+
    'The goal is to eliminate the manual size calculation from the form unit.';
  FTextLabel.Text := FText;
end;


procedure TAutosizeBoxText.SetText(const Value: string);
begin
  if FText <> Value then
    begin
      FText:= Value;
      FTextLabel.Text:= Value;
      // In FMX, setting Text often triggers an internal RGN_Change (Region Change) message which leads to a Resize
      UpdateSize;
    end;
end;


// Perform the actual height adjustment.
procedure TAutosizeBoxText.UpdateSize;
VAR MinHeight: Single;
begin
  if Width <= 0 then Exit;
  MinHeight:= FTextLabel.Height + Self.Padding.Top + Self.Padding.Bottom + CTextHeightBuffer;
  Self.Height:= Ceil(MinHeight);
end;






{------------------------------------------------------------------------------------------------------------}

function MakeTextBubble(Parent: TControl; const Text: string; BoxType: TBoxType): TAutosizeBoxText;
begin
  Result := TAutosizeBoxText.Create(Parent);
  Result.Parent := Parent;
  Result.Stored := FALSE;
  Result.BoxType:= BoxType;
  Result.Text   := Text;
  Result.Position.Y:= 99999999;
  Result.UpdateSize;   // We use Bubble.UpdateHeight here to force the initial sizing (which also sets Bubble.Height).
end;


procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TAutosizeBoxText]);
end;


end.
