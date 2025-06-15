program VCL_Uninstaller;

uses
  {$IFDEF DEBUG}FastMM4, {$ENDIF }
  Vcl.Themes,
  Vcl.Styles,
  UninstallerForm in 'UninstallerForm.pas' {frmMain},
  LightCom.Shell in '..\..\..\FrameVCL\LightSaber\LightCom.Shell.pas',
  ccAppData in '..\..\..\ccAppData.pas',
  LightCom.AppDataForm in '..\..\..\FrameVCL\LightCom.AppDataForm.pas',
  LightCom.IniFile in '..\..\..\FrameVCL\LightCom.IniFile.pas',
  LightCom.AppData in '..\..\..\FrameVCL\LightCom.AppData.pas',
  LightVcl.LogForm in '..\..\..\FrameVCL\LightVcl.LogForm.pas';


{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Template Uninstaller');   // stackoverflow.com/questions/75449673/is-it-ok-to-create-an-object-before-application-initialize
  UninstallerForm.CreateCopy;
  AppData.CreateMainForm(TfrmMain, frmMain, TRUE, TRUE, asFull);
  AppData.Run;
end.


