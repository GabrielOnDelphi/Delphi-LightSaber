program Uninstaller;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  UninstallerForm in 'UninstallerForm.pas' {frmMain},
  Vcl.Themes,
  Vcl.Styles,
  LightCom.AppData,
  cvCountDown in '..\..\FrameVCL\LightSaber\cvCountDown.pas',
  LightCom.Shell in '..\..\FrameVCL\LightSaber\LightCom.Shell.pas',
  ccAppData in '..\..\ccAppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Template Uninstaller');   // stackoverflow.com/questions/75449673/is-it-ok-to-create-an-object-before-application-initialize
  UninstallerForm.CreateCopy;
  AppData.CreateMainForm(TfrmMain, frmMain, TRUE, TRUE, asFull);
  AppData.Run;
end.


