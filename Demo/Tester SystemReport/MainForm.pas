unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, cbAppData, cbAppDataForm;

type
  TfrmMain = class(TLightForm)
    Memo: TMemo;
  private
    procedure LateInitialize; override;
  public
  end;

var
  frmMain: TfrmMain;

IMPLEMENTATION {$R *.dfm}
USES cmDebugger;



procedure TfrmMain.LateInitialize(var Msg: TMessage);
begin
  inherited latei;
  Memo.Text:= cmDebugger.GenerateSystemRep;
end;

end.
