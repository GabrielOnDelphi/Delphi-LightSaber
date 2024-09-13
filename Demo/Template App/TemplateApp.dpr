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
  FormMain in 'FormMain.pas',
  FormSettings in 'FormSettings.pas' {frmSettings},
  cbAppData in '..\..\cbAppData.pas',
  FormSplashScreen in '..\..\FormSplashScreen.pas',
  FormUniversalEula in '..\..\FormUniversalEula.pas',
  FormUpdaterNotifier in '..\..\Updater\FormUpdaterNotifier.pas',
  ciUpdater in '..\..\Updater\ciUpdater.pas',
  ciUpdaterRec in '..\..\Updater\ciUpdaterRec.pas',
  FormUpdaterRecEditor in '..\..\Updater\FormUpdaterRecEditor.pas',
  FormAbout in '..\..\FormAbout.pas',
  FormSkinsDisk in '..\..\FormSkinsDisk.pas',
  FormSkinsRes in '..\..\FormSkinsRes.pas',
  cbDialogs in '..\..\cbDialogs.pas',
  cbIniFile in '..\..\cbIniFile.pas',
  cmGuiSettings in '..\..\cmGuiSettings.pas',
  cpProteusCertificate in '..\..\..\LightProteus\cpProteusCertificate.pas',
  cTranslate in '..\..\..\LightAutoTranslator\cTranslate.pas',
  FormSelectLang in '..\..\..\LightAutoTranslator\FormSelectLang.pas',
  FormTranslator in '..\..\..\LightAutoTranslator\FormTranslator.pas',
  cpProteus in '..\..\..\LightProteus\cpProteus.pas',
  cpProteusIO in '..\..\..\LightProteus\cpProteusIO.pas',
  cpProteusUtils in '..\..\..\LightProteus\cpProteusUtils.pas',
  FormUpdaterSettings in '..\..\Updater\FormUpdaterSettings.pas',
  FormRamLog in '..\..\FormRamLog.pas',
  cbLogLines in '..\..\cbLogLines.pas',
  cbLogRam in '..\..\cbLogRam.pas',
  cvLog in '..\..\cvLog.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light Commercial Template');     { Start with cubic for templates }  { Absolutelly critical if you use the SaveForm/LoadForm functionality. This string will be used as the name of the INI file. }
  AppData.CreateMainForm(TMainForm, MainForm, TRUE);
  TfrmRamLog.CreateFormAppData;
  Application.Run;
end.
