program VCL_Demo_Log;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  FormMain in 'FormMain.pas' {MainForm},
  ccINIFile in '..\..\ccINIFile.pas',
  LightVcl.Common.AppData in '..\..\FrameVCL\LightCom.AppData.pas',
  ccAppData in '..\..\ccAppData.pas',
  LightVcl.Common.LogFilter in '..\..\FrameVCL\LightCom.LogFilter.pas',
  LightVcl.Common.LogViewer in '..\..\FrameVCL\LightCom.LogViewer.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo Logging System');
  AppData.CreateMainForm(TMainForm, MainForm, TRUE, TRUE, asFull);
  AppData.Run;
end.
