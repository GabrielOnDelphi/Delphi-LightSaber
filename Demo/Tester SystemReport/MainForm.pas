unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, cbAppData;

type
  TfrmMain = class(TForm)
    Memo: TMemo;
  private
    procedure LateInitialize(VAR Msg: TMessage); message MSG_LateFormInit;
  public
  end;

var
  frmMain: TfrmMain;

IMPLEMENTATION {$R *.dfm}
USES cmDebugger;



procedure TfrmMain.LateInitialize(var Msg: TMessage);
begin
  Memo.Text:= cmDebugger.GenerateSystemRep;
end;

end.
