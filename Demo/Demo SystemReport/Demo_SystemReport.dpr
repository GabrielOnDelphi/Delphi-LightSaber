program Demo_SystemReport;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  LightCom.AppData in '..\..\FrameVCL\LightCom.AppData.pas',
  ccAppData in '..\..\ccAppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo System Report');
  AppData.CreateMainForm(TfrmMain, frmMain, True, True, asFull);
  AppData.Run;
end.
