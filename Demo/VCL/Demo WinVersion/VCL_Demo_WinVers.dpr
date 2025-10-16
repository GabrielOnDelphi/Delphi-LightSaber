program VCL_Demo_WinVers;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  FormMain in 'FormMain.pas' {frmTester},
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Tester WinVer');
  AppData.CreateMainForm(TfrmTester, TRUE, TRUE, asFull);
  AppData.Run;
end.
