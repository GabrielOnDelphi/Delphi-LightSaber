unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ccAppData, LightCom.AppData, LightCom.AppDataForm;

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
USES LightCom.Debugger;



procedure TfrmMain.FormPostInitialize;
begin
  inherited FormPostInitialize;
  Memo.Text:= LightCom.Debugger.GenerateSystemRep;
end;

end.
