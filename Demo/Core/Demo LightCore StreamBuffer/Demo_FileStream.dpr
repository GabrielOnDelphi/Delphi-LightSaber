program Demo_FileStream;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  FormMain in 'FormMain.pas' {MainForm},
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  Vcl.Forms;

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('LightStream Demo');
  Application.MainFormOnTaskbar:= TRUE;
  AppData.CreateMainForm(TMainForm, asFull);    // Main form
  AppData.Run;
end.

