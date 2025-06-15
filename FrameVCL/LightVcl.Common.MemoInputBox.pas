UNIT LightVcl.Common.MemoInputBox;

{=============================================================================================================
   2023.01
   www.GabrielMoraru.com
   See Copyright file
==============================================================================================================

   A box in which the user can enter some text.
   The entered text is returned in Value.

   This unit is dependencies free!

=============================================================================================================}

INTERFACE
USES
  Winapi.Windows,
  System.SysUtils, System.Types,
  Vcl.Controls,  Vcl.Forms, Vcl.StdCtrls, Vcl.Consts, Vcl.Graphics;


function MemoInputQuery(const aCaption, aPrompt: string; VAR Value: string; CONST xScale: Integer= 2): Boolean;  { The smaller ScaleFactor the larger the width of the window }


IMPLEMENTATION

USES LightVcl.Common.EllipsisText;


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

