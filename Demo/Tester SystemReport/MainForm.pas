unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, cbAppDataForm,Vcl.Dialogs, Vcl.StdCtrls, cbAppData, cbAppDataForm;

type
  TfrmMain = class(TLightForm)
    Memo: TMemo;
  private
    procedure LateInitialize; {don't forget inherited LateInitialize!} override;
  public
  end;

var
  frmMain: TfrmMain;

IMPLEMENTATION {$R *.dfm}
USES cmDebugger;



procedure TfrmMain.LateInitialize;
begin
  inherited LateInitialize;
  Memo.Text:= cmDebugger.GenerateSystemRep;
end;

end.
