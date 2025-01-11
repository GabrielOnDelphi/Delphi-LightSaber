UNIT FormMain;

INTERFACE

USES
  WinApi.Windows, Winapi.ShellAPI, WinApi.Messages, System.SysUtils, System.Classes, Vcl.StdCtrls, VCL.Forms, Vcl.Controls, Vcl.Samples.Spin,
  Vcl.ComCtrls, Vcl.ExtCtrls, cbAppData, cbAppDataForm;

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
    procedure LateInitialize; override; // Called after the main form was fully created
  public
 end;

VAR
   frmTester: TfrmTester;

IMPLEMENTATION  {$R *.dfm}

USES
   cbVersion, cmWinVersionOthers;


procedure TfrmTester.FormCreate(Sender: TObject);
begin
 //
end;


procedure TfrmTester.LateInitialize;
begin
 inherited LateInitialize;
 btnStartClick(Self);
end;


procedure TfrmTester.btnStartClick(Sender: TObject);
begin
 Memo.Text:= '';
 Memo.Lines.Add('');
 Memo.Lines.Add('cbVersion');
 Memo.Lines.Add(cbVersion.GenerateReport);
 Memo.Lines.Add('');
 Memo.Lines.Add('cmWinVersionOthers');
 Memo.Lines.Add(cmWinVersionOthers.GenerateReport);
end;







end.
