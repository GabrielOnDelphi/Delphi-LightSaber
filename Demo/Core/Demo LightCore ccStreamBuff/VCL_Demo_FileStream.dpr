program VCL_Demo_FileStream;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  LightVcl.Common.AppData,
  FormMain in 'FormMain.pas' {MainForm},
  LightVcl.LogForm in '..\..\FrameVCL\LightVcl.LogForm.pas',
  LightCore.AppData in '..\..\LightCore.AppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo FileStream');
  AppData.CreateMainForm(TMainForm, MainForm, True, True, asFull);    // Main form
  AppData.Run;
end.

