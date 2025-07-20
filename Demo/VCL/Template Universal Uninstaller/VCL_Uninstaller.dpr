program VCL_Uninstaller;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  Vcl.Themes,
  Vcl.Styles,
  UninstallerForm in 'UninstallerForm.pas' {frmMain},
  LightVcl.Common.AppData in '..\..\..\FrameVCL\LightVcl.Common.AppData.pas',
  LightVcl.Common.AppDataForm in '..\..\..\FrameVCL\LightVcl.Common.AppDataForm.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightVcl.Common.Shell in '..\..\..\FrameVCL\LightVcl.Common.Shell.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Template Uninstaller');   // stackoverflow.com/questions/75449673/is-it-ok-to-create-an-object-before-application-initialize
  UninstallerForm.CreateCopy;
  AppData.CreateMainForm(TfrmMain, frmMain, TRUE, TRUE, asFull);
  AppData.Run;
end.


