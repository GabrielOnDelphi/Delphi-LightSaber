program VCL_Demo_WinVers;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  FormMain in 'FormMain.pas' {frmTester},
  ccAppData in '..\..\..\ccAppData.pas',
  LightVcl.Common.AppDataForm in '..\..\..\FrameVCL\LightCom.AppDataForm.pas',
  LightVcl.Common.IniFile in '..\..\..\FrameVCL\LightCom.IniFile.pas',
  LightVcl.Common.AppData in '..\..\..\FrameVCL\LightCom.AppData.pas',
  LightVcl.LogForm in '..\..\..\FrameVCL\LightVcl.LogForm.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Tester WinVer');
  AppData.CreateMainForm(TfrmTester, frmTester, TRUE, TRUE, asFull);
  AppData.Run;
end.
