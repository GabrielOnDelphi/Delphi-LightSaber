program Demo_VisualControls;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  LightCom.AppData,
  MainForm in 'MainForm.pas' {frmMain},
  ccAppData in '..\..\ccAppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo Visual Controls');
  AppData.CreateMainForm(TfrmMain, frmMain, TRUE, TRUE, asFull);
  AppData.Run;
end.
