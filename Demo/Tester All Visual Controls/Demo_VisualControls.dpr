program Demo_VisualControls;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  cbAppDataVCL,
  MainForm in 'MainForm.pas' {frmMain};

{$R *.res}

begin
  AppData:= TAppData.Create('Light Tester WinVer');
  AppData.CreateMainForm(TfrmMain, frmMain, TRUE);
  AppData.Run;
end.
