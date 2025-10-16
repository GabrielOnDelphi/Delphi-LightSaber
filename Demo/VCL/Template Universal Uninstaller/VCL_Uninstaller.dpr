program VCL_Uninstaller;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  Vcl.Themes,
  Vcl.Styles,
  UninstallerForm in 'UninstallerForm.pas' {frmMain},
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  LightVcl.Visual.AppDataForm in '..\..\..\FrameVCL\LightVcl.Visual.AppDataForm.pas',
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


