program Demo_WinVers;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  LightCom.AppData,
  FormMain in 'FormMain.pas' {frmTester},
  LightVcl.LogForm in '..\..\FrameVCL\LightVcl.LogForm.pas',
  ccAppData in '..\..\ccAppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Tester WinVer');
  AppData.CreateMainForm(TfrmTester, frmTester, TRUE, TRUE, asFull);
  AppData.Run;
end.
