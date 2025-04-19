program Demo_WinVers;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  cbAppDataVCL,
  FormMain in 'FormMain.pas' {frmTester},
  FormRamLog in '..\..\FrameVCL\FormRamLog.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light Tester WinVer');
  AppData.CreateMainForm(TfrmTester, frmTester, TRUE);
  TfrmRamLog.CreateGlobalLog;
  AppData.Run;
end.
