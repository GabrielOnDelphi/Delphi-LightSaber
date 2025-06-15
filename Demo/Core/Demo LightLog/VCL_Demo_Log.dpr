program VCL_Demo_Log;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  FormMain in 'FormMain.pas' {MainForm},
  LightCore.INIFile in '..\..\LightCore.INIFile.pas',
  LightVcl.Common.AppData in '..\..\FrameVCL\LightCom.AppData.pas',
  LightCore.AppData in '..\..\LightCore.AppData.pas',
  LightVcl.Common.LogFilter in '..\..\FrameVCL\LightCom.LogFilter.pas',
  LightVcl.Common.LogViewer in '..\..\FrameVCL\LightCom.LogViewer.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo Logging System');
  AppData.CreateMainForm(TMainForm, MainForm, TRUE, TRUE, asFull);
  AppData.Run;
end.
