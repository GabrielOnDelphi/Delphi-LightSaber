UNIT FormMain;

INTERFACE

USES
  System.Classes, Vcl.StdCtrls, Vcl.Forms, Vcl.Controls,
  Vcl.ExtCtrls, LightVcl.Visual.AppDataForm;

const
   CRLF = #13#10;

TYPE
 TfrmTester = class(TLightForm)
    pnlRight: TPanel;
    btnStart: TButton;
    Memo: TMemo;
    procedure btnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
  private
  public
    procedure FormPostInitialize; override; // Called after the main form was fully created
 end;


IMPLEMENTATION  {$R *.dfm}

USES
   LightVcl.Common.WinVersionApi, LightVcl.Common.WinVersion, LightVcl.Common.ExeVersion;


procedure TfrmTester.FormCreate(Sender: TObject);
begin
 //
end;


procedure TfrmTester.FormPostInitialize;
begin
 inherited FormPostInitialize;
 btnStartClick(Self);
end;


procedure TfrmTester.btnStartClick(Sender: TObject);
begin
  Memo.Clear;
  Memo.Lines.Add('LightCom.WinVersion.pas');
  Memo.Lines.Add('');
  Memo.Lines.Add(LightVcl.Common.WinVersion.GenerateReport);

  Memo.Lines.Add('_________________');
  Memo.Lines.Add('');
  Memo.Lines.Add('LightCom.WinVersionApi.pas');
  Memo.Lines.Add('');
  Memo.Lines.Add(LightVcl.Common.WinVersionApi.GenerateReport);
  LightVcl.Common.WinVersion.GenerateReport;

  Memo.Lines.Add('_________________');
  Memo.Lines.Add('');
  Memo.Lines.Add('LightCom.ExeVersion.pas');
  Memo.Lines.Add(LightVcl.Common.ExeVersion.GetVersionInfo(ParamStr(0), TRUE));
end;


end.
