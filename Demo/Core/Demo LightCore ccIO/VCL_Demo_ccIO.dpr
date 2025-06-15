program VCL_Demo_ccIO;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  MainForm in 'MainForm.pas' {frmTestIO},
  LightVcl.Common.AppData,
  LighCore.AppData in '..\..\LighCore.AppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo IO');
  AppData.CreateMainForm(TfrmTestIO, frmTestIO, TRUE, TRUE, asFull);
  AppData.Run;
end.
