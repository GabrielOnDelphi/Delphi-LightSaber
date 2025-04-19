unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ccAppData, cbAppDataVCL, cbAppDataForm;

type
  TfrmMain = class(TLightForm)
    Memo: TMemo;
  private
  public
    procedure FormPostInitialize; override;
  end;

var
  frmMain: TfrmMain;

IMPLEMENTATION {$R *.dfm}
USES cmDebugger;



procedure TfrmMain.FormPostInitialize;
begin
  inherited FormPostInitialize;
  Memo.Text:= cmDebugger.GenerateSystemRep;
end;

end.
