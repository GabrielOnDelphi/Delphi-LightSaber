program VCL_Demo_LightCore.IO;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  MainForm in 'MainForm.pas' {frmTestIO},
  LightVcl.Common.AppData,
  LightCore.AppData in '..\..\LightCore.AppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo IO');
  AppData.CreateMainForm(TfrmTestIO, frmTestIO, TRUE, TRUE, asFull);
  AppData.Run;
end.
