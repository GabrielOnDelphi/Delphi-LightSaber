program DemoAllVisualControls;

uses
  FastMM4,
  cbAppData,
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain};

{$R *.res}

begin
  AppData:= TAppData.Create('Light Tester WinVer');
  AppData.CreateMainForm(TfrmMain, frmMain, TRUE);
  Application.Run;
end.
