program VCL_Demo_Log;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }

  LightCore.INIFile in '..\..\LightCore.INIFile.pas',
  LightCore.AppData in '..\..\LightCore.AppData.pas',  
  FormMain in 'FormMain.pas' {MainForm},  
  LightVcl.Common.AppData in '..\..\FrameVCL\LightCom.AppData.pas',
  LightVcl.Common.LogFilter in '..\..\FrameVCL\LightCom.LogFilter.pas',
  LightVcl.Common.LogViewer in '..\..\FrameVCL\LightCom.LogViewer.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo VCL Visual Log');
  AppData.CreateMainForm(TMainForm, MainForm, TRUE, TRUE, asFull);
  AppData.Run;
end.
