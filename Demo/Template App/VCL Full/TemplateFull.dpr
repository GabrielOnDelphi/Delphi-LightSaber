program TemplateFull;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  ccINIFile,
  cbAppDataVCL,
  FormMain in 'FormMain.pas' {MainForm},
  FormRamLog in '..\..\FormRamLog.pas',
  FormSettings in 'FormSettings.pas',
  FormAbout in '..\..\FormAbout.pas',
  FormSkinsDisk in '..\..\FormSkinsDisk.pas',
  FormSkinsRes in '..\..\FormSkinsRes.pas',
  FormSplashScreen in '..\..\FormSplashScreen.pas',
  FormUniversalEula in '..\..\FormUniversalEula.pas',
  FormUpdaterNotifier in '..\..\Updater\FormUpdaterNotifier.pas',
  FormUpdaterRecEditor in '..\..\Updater\FormUpdaterRecEditor.pas',
  FormUpdaterSettings in '..\..\Updater\FormUpdaterSettings.pas',
  uInitialization in 'uInitialization.pas',
  ciUpdater in '..\..\Updater\ciUpdater.pas';

{$R *.res}

begin
  CONST
     MultiThreaded= FALSE;         // True => Only if we need to use multithreading in the Log.
  CONST
     AppName= 'Light Template Full';       // Absolutelly critical if you use the SaveForm/LoadForm functionality. This string will be used as the name of the INI file.

  AppData:= TAppData.Create(AppName, '', MultiThreaded);
  AppData.CreateMainForm(TMainForm, MainForm, FALSE, TRUE, asFull);
  TfrmRamLog.CreateGlobalLog;
  
  AppData.Run;
end.
