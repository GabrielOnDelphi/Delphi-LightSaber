program Demo_FileStream;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  LightCom.AppData,
  FormMain in 'FormMain.pas' {MainForm},
  LightVcl.LogForm in '..\..\FrameVCL\LightVcl.LogForm.pas',
  ccAppData in '..\..\ccAppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo FileStream');
  AppData.CreateMainForm(TMainForm, MainForm, True, True, asFull);    // Main form
  AppData.Run;
end.

