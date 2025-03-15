unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, cbAppDataForm,Vcl.Dialogs, Vcl.StdCtrls, ccAppData, cbAppDataVCL
, cbAppDataForm;

type
  TfrmMain = class(TLightForm)
    Memo: TMemo;
  private
    procedure FormInitialize; {don't forget inherited in FormInitialize!} override;
  public
  end;

var
  frmMain: TfrmMain;

IMPLEMENTATION {$R *.dfm}
USES cmDebugger;



procedure TfrmMain.FormInitialize;
begin
  inherited FormInitialize;
  Memo.Text:= cmDebugger.GenerateSystemRep;
end;

end.
