program Demo_ccIO;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  MainForm in 'MainForm.pas' {frmTestIO},
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas';

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo IO');
  AppData.CreateMainForm(TfrmTestIO, frmTestIO, TRUE, TRUE, asFull);
  AppData.Run;
end.
