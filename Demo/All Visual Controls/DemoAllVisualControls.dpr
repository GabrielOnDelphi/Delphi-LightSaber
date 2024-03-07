program DemoAllVisualControls;

uses
  FastMM4,
  ccAppData,
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain};

{$R *.res}

begin
  AppData:= TAppData.Create('Cubic Tester WinVer');
  AppData.CreateMainForm(TfrmMain, frmMain, TRUE);
  Application.Run;
end.
