unit MainForm;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, LightVcl.Visual.AppDataForm;

type
  TfrmMain = class(TLightForm)
    Memo: TMemo;
  public
    procedure FormPostInitialize; override;
  end;


IMPLEMENTATION {$R *.dfm}
USES
   LightCore.Reports, LightVCL.Common.Reports, LightVcl.Common.Debugger;



procedure TfrmMain.FormPostInitialize;
begin
  inherited FormPostInitialize;
  Memo.Text:= LightVCL.Common.Reports.GenerateSystemReport;
  Memo.Lines.Add(LightVcl.Common.Debugger.GenerateCompilerReport);
end;

end.
