unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, LighCore.AppData, LightVcl.Common.AppData, LightVcl.Common.AppDataForm;

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
USES LightVcl.Common.Debugger;



procedure TfrmMain.FormPostInitialize;
begin
  inherited FormPostInitialize;
  Memo.Text:= LightVcl.Common.Debugger.GenerateSystemRep;
end;

end.
