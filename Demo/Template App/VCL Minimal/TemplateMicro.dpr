program TemplateMicro;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  MainForm in 'MainForm.pas' {frmMain},
  LightCom.AppData,
  ccINIFile,
  ccAppData in '..\..\..\ccAppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;
  
  AppData:= TAppData.Create('Light Template Micro');
  AppData.CreateMainForm(TfrmMain, TRUE, TRUE, asFull);
  AppData.Run;
end.
