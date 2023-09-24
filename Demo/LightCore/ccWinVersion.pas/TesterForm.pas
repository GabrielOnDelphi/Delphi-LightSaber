UNIT TesterForm;

INTERFACE

USES
  WinApi.Windows, Winapi.ShellAPI, WinApi.Messages, System.SysUtils, System.Classes, Vcl.StdCtrls, VCL.Forms, Vcl.Controls, Vcl.Samples.Spin,
  Vcl.ComCtrls,Vcl.ExtCtrls;

const
   MSG_LateInitialize= WM_APP + 4711;
   CRLF = #13#10;

TYPE
 TfrmTester = class(TForm)
    pnlRight: TPanel;
    btnStart: TButton;
    Memo: TMemo;
    procedure btnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
  private
    procedure LateInitialize(VAR message: TMessage);  message MSG_LateInitialize;
  public
 end;

VAR
   frmTester: TfrmTester;

IMPLEMENTATION  {$R *.dfm}

USES
   ccWinVersion, cmWinVersionOthers;


procedure TfrmTester.FormCreate(Sender: TObject);
begin
 PostMessage(Self.Handle, MSG_LateInitialize, 0, 0); { This will call LateInitialize }
end;


procedure TfrmTester.LateInitialize;
begin
 btnStartClick(Self);
end;


procedure TfrmTester.btnStartClick(Sender: TObject);
begin
 Memo.Text:= '';
 Memo.Lines.Add('');
 Memo.Lines.Add('ccWinVersion');
 Memo.Lines.Add(ccWinVersion.GenerateReport);
 Memo.Lines.Add('');
 Memo.Lines.Add('cmWinVersionOthers');
 Memo.Lines.Add(cmWinVersionOthers.GenerateReport);
end;







end.
