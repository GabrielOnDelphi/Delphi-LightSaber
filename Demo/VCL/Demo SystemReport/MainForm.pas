unit MainForm;

interface

uses
  System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, LightVcl.Visual.AppDataForm;

type
  TfrmMain = class(TLightForm)
    Memo: TMemo;
  public
    procedure FormPostInitialize; override;
  end;


IMPLEMENTATION {$R *.dfm}
USES
   LightVCL.Common.Reports;



procedure TfrmMain.FormPostInitialize;
begin
  inherited FormPostInitialize;
  Memo.Clear;
  Memo.Lines.Add(LightVCL.Common.Reports.GenerateVCLReport);
end;

end.
