program VCL_Demo_Log;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  FormMain in 'FormMain.pas' {MainForm},
  ccINIFile in '..\..\ccINIFile.pas',
  LightCom.AppData in '..\..\FrameVCL\LightCom.AppData.pas',
  ccAppData in '..\..\ccAppData.pas',
  LightCom.LogFilter in '..\..\FrameVCL\LightCom.LogFilter.pas',
  LightCom.LogViewer in '..\..\FrameVCL\LightCom.LogViewer.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo Logging System');
  AppData.CreateMainForm(TMainForm, MainForm, TRUE, TRUE, asFull);
  AppData.Run;
end.
