UNIT cmMemoInputBox;

{=============================================================================================================
   2023.01
   See Copyright.txt
==============================================================================================================

   A box in which the user can enter some text.
   The entered text is returned in Value.

   This unit is dependencies free!

=============================================================================================================}

INTERFACE
USES
  Winapi.Windows, System.SysUtils, Vcl.Controls, System.Types, Vcl.Forms, Vcl.StdCtrls, Vcl.Consts, Vcl.Graphics;


function GetAverageCharSize(Canvas: TCanvas): TPoint;
function MemoInputQuery(const aCaption, aPrompt: string; VAR Value: string; CONST xScale: Integer= 2): Boolean;  { The smaller ScaleFactor the larger the width of the window }


IMPLEMENTATION



function GetAverageCharSize(Canvas: TCanvas): TPoint;   { Average text-size on specified canvas. Function copied from Vcl.Dialogs.PAS because it is not exposed there in "Interface" }
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;



function MemoInputQuery(const ACaption, APrompt: string; VAR Value: string; CONST xScale: Integer= 2): Boolean;
VAR
  Form: TForm;
  lblPrompt: TLabel;
  Memo: TMemo;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result:= False;
  Form  := TForm.Create(Application);
  with Form DO
   TRY
    Canvas.Font := Font;
    DialogUnits := GetAverageCharSize(Canvas);
    BorderStyle := bsDialog;
    Caption     := ACaption;
    ClientWidth := MulDiv(180, DialogUnits.X, xScale);
    PopupMode   := pmAuto;
    Position    := poScreenCenter;
    lblPrompt   := TLabel.Create(Form);

    with lblPrompt do
    begin
      Parent := Form;
      Caption:= APrompt;
      Left   := 5;
      Top    := MulDiv(8, DialogUnits.Y, 8);
      Constraints.MaxWidth := MulDiv(164, DialogUnits.X, xScale);
      WordWrap := True;
    end;

    Memo := TMemo.Create(Form);
    with Memo do
    begin
      Parent := Form;
      Left   := lblPrompt.Left;
      Top    := lblPrompt.Top + lblPrompt.Height + 5;
      Width  := Form.ClientWidth- Left- 5;
      //MaxLength := 255;
      Text   := Value;
      SelectAll;
    end;

    ButtonTop    := Memo.Top + Memo.Height + 15;
    ButtonWidth  := 82;
    ButtonHeight := 27;

    with TButton.Create(Form) do
    begin
      Parent := Form;
      Caption := vcl.Consts.SMsgDlgOK;
      ModalResult := mrOk;
      Default := True;
      SetBounds(16, ButtonTop, ButtonWidth, ButtonHeight);
    end;

    with TButton.Create(Form) do
    begin
      Parent := Form;
      Caption := SMsgDlgCancel;
      ModalResult := mrCancel;
      Cancel := True;
      SetBounds(22+ ButtonWidth, ButtonTop, ButtonWidth, ButtonHeight);
      Form.ClientHeight := Top + Height + 13;
    end;

    if ShowModal = mrOk then
     begin
      Value := Memo.Text;
      Result:= True;
     end;
   FINALLY
    FreeAndNil(Form);
   END;
end;


end.
