program TemplateApp;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  WinApi.Windows,
  VCL.Forms,
  cbAppData in '..\..\cbAppData.pas',
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
  CONST MultiThreaded= TRUE; // True => Only if we need to use multithreading in the Log.

  AppData:= TAppData.Create('Light Commercial Template', '', TRUE, MultiThreaded); { Absolutelly critical if you use the SaveForm/LoadForm functionality. This string will be used as the name of the INI file. }
  AppData.CreateMainForm(TMainForm, MainForm, True, True);
  TfrmRamLog.CreateGlobalLog;
  
  // Warning: if the main form is set not to auto show so we can manually show it later, this will break the MSG_LateFormInit system
  // This is because TrySetStyle modifies the application styles, which can internally recreate the form or affect its messaging loop.
  // This interruption likely results in the MSG_LateFormInit message being lost or not dispatched as expected.
  // Call TrySetStyle late, or make the main form visible before you call TrySetStyle!
  //TStyleManager.TrySetStyle('Amakrits');
  
  Application.Run;
end.
