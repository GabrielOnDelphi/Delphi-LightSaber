UNIT LightVcl.Common.MemoInputBox;

{=============================================================================================================
   2023.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================

   A box in which the user can enter multiline text.
   The entered text is returned in Value.

   Dependencies:
     LightVcl.Common.EllipsisText (for GetAverageCharSize)

=============================================================================================================}

INTERFACE
USES
  Winapi.Windows,
  System.SysUtils, System.Types,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.Consts, Vcl.Graphics;


function MemoInputQuery(const aCaption, aPrompt: string; VAR Value: string; CONST xScale: Integer= 2): Boolean;  { The smaller xScale the larger the width of the window }


IMPLEMENTATION

USES LightVcl.Common.EllipsisText;


function MemoInputQuery(const ACaption, APrompt: string; VAR Value: string; CONST xScale: Integer= 2): Boolean;
VAR
  Form: TForm;
  lblPrompt: TLabel;
  Memo: TMemo;
  btnOK, btnCancel: TButton;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result:= FALSE;

  Form:= TForm.Create(Application);
  TRY
    Form.Canvas.Font:= Form.Font;
    DialogUnits:= GetAverageCharSize(Form.Canvas);

    Form.BorderStyle:= bsDialog;
    Form.Caption:= ACaption;
    Form.ClientWidth:= MulDiv(180, DialogUnits.X, xScale);
    Form.PopupMode:= pmAuto;
    Form.Position:= poScreenCenter;

    { Prompt label }
    lblPrompt:= TLabel.Create(Form);
    lblPrompt.Parent:= Form;
    lblPrompt.Caption:= APrompt;
    lblPrompt.Left:= 5;
    lblPrompt.Top:= MulDiv(8, DialogUnits.Y, 8);
    lblPrompt.Constraints.MaxWidth:= MulDiv(164, DialogUnits.X, xScale);
    lblPrompt.WordWrap:= TRUE;

    { Memo for text input }
    Memo:= TMemo.Create(Form);
    Memo.Parent:= Form;
    Memo.Left:= lblPrompt.Left;
    Memo.Top:= lblPrompt.Top + lblPrompt.Height + 5;
    Memo.Width:= Form.ClientWidth - Memo.Left - 5;
    Memo.Height:= 80;  { Set explicit height for multiline input }
    Memo.Text:= Value;
    Memo.SelectAll;

    ButtonTop:= Memo.Top + Memo.Height + 15;
    ButtonWidth:= 82;
    ButtonHeight:= 27;

    { OK button }
    btnOK:= TButton.Create(Form);
    btnOK.Parent:= Form;
    btnOK.Caption:= SMsgDlgOK;
    btnOK.ModalResult:= mrOk;
    btnOK.Default:= TRUE;
    btnOK.SetBounds(16, ButtonTop, ButtonWidth, ButtonHeight);

    { Cancel button }
    btnCancel:= TButton.Create(Form);
    btnCancel.Parent:= Form;
    btnCancel.Caption:= SMsgDlgCancel;
    btnCancel.ModalResult:= mrCancel;
    btnCancel.Cancel:= TRUE;
    btnCancel.SetBounds(22 + ButtonWidth, ButtonTop, ButtonWidth, ButtonHeight);

    Form.ClientHeight:= btnCancel.Top + btnCancel.Height + 13;

    if Form.ShowModal = mrOk then
      begin
        Value:= Memo.Text;
        Result:= TRUE;
      end;
  FINALLY
    FreeAndNil(Form);
  END;
end;


end.

