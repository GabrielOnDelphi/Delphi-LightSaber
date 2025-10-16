program VCL_Demo_Log;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  FormMain in 'FormMain.pas' {MainForm},
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light VCL Log Demo');
  AppData.CreateMainForm(TMainForm, MainForm, TRUE, TRUE, asFull);
  AppData.Run;
end.
