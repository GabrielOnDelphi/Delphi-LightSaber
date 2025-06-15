program VCL_Uninstaller;

uses
  {$IFDEF DEBUG}FastMM4, {$ENDIF }
  Vcl.Themes,
  Vcl.Styles,
  UninstallerForm in 'UninstallerForm.pas' {frmMain},
  LightVcl.Common.Shell in '..\..\..\FrameVCL\LightSaber\LightCom.Shell.pas',
  ccAppData in '..\..\..\ccAppData.pas',
  LightVcl.Common.AppDataForm in '..\..\..\FrameVCL\LightCom.AppDataForm.pas',
  LightVcl.Common.IniFile in '..\..\..\FrameVCL\LightCom.IniFile.pas',
  LightVcl.Common.AppData in '..\..\..\FrameVCL\LightCom.AppData.pas',
  LightVcl.LogForm in '..\..\..\FrameVCL\LightVcl.LogForm.pas';


{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Template Uninstaller');   // stackoverflow.com/questions/75449673/is-it-ok-to-create-an-object-before-application-initialize
  UninstallerForm.CreateCopy;
  AppData.CreateMainForm(TfrmMain, frmMain, TRUE, TRUE, asFull);
  AppData.Run;
end.


