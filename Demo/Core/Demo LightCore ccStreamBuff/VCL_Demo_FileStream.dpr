program VCL_Demo_FileStream;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  FormMain in 'FormMain.pas' {MainForm},
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightVcl.Common.AppData in '..\..\..\FrameVCL\LightVcl.Common.AppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo FileStream');
  AppData.CreateMainForm(TMainForm, MainForm, True, True, asFull);    // Main form
  AppData.Run;
end.

