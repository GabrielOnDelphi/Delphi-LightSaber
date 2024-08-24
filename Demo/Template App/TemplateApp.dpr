program TemplateApp;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  WinApi.Windows,
  VCL.Forms,
  Vcl.Themes,
  Vcl.Styles,
  uInitialization in 'uInitialization.pas',
  FormSettings in 'FormSettings.pas' {frmSettings},
  cbAppData in '..\..\cbAppData.pas',
  FormMain in 'FormMain.pas',
  FormSplashScreen in '..\..\FormSplashScreen.pas',
  FormUniversalEula in '..\..\FormUniversalEula.pas',
  FormUpdaterNotifier in '..\..\Updater\FormUpdaterNotifier.pas',
  ciUpdater in '..\..\Updater\ciUpdater.pas',
  ciUpdaterRec in '..\..\Updater\ciUpdaterRec.pas',
  FormUpdaterEditor in '..\..\Updater\FormUpdaterEditor.pas',
  FormUpdaterRecEditor in '..\..\Updater\FormUpdaterRecEditor.pas',
  FormAbout in '..\..\FormAbout.pas',
  FormSkinsDisk in '..\..\FormSkinsDisk.pas',
  FormSkinsRes in '..\..\FormSkinsRes.pas',
  cvINIFile in '..\..\cvINIFile.pas',
  cbDialogs in '..\..\cbDialogs.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light Commercial Template');     { Start with cubic for templates }  { Absolutelly critical if you use the SaveForm/LoadForm functionality. This string will be used as the name of the INI file. }
  AppData.CreateMainForm(TMainForm, MainForm, TRUE);
  Application.Run;
end.
