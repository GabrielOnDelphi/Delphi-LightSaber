UNIT uInitialization;
{=============================================================================================================
   Gabriel Moraru
   2025.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Can be used as template for future applications.
--------------------------------------------------------------------------------------------------------------

=============================================================================================================}

INTERFACE

USES
   Winapi.ShellApi, System.SysUtils, Vcl.Forms, LightVcl.Visual.AppDataForm,Vcl.Dialogs, Vcl.Controls;

procedure LateInitialization;

IMPLEMENTATION

USES
  chHardID, LightVcl.Common.Shell, LightVcl.Common.ExecuteShell, LightVcl.Common.GuiSettings,
  cpCertificate, LightCore.AppData, LightVcl.Visual.AppData, LightVcl.Common.CenterControl, LightVcl.Common.Translate, ciUpdater,
  FormMain, FormUniversalEula, FormSkinsDisk, FormSettings, FormSplashScreen, FormUpdaterNotifier;


procedure LateInitialization;
begin
  { Add your brand here }
  {AppData.CompanyName    := 'BuyTime Ltd';
  AppData.ProductHome    := 'https://YourWebsiteHere.com';
  AppData.ProductSupport := 'https://YourWebsiteHere.com';
  AppData.ProductUninstal:= 'https://YourWebsiteHere.com'; }

  { Settings }
  { This object is used to store user's GUI settings. They are editable via the TFormSettings.
    However, these settings should be accessible EVEN if the form is not live/created.
    This is a good example of GUI - business logic sepparation. }
  GuiSettings:= TGuiSettings.Create;
  GuiSettings.Load;

  { Skins }
  if AppData.RunningFirstTime
  AND NOT AppData.RunningHome
  then LoadLastSkin('Light AmethystKamri.vsf') // 'Light AmethystKamri.vsf' has too much blue
  else LoadLastSkin();

  { Trial/License }
  { Load the log early othewise it will overwrite the existing text }
  chHardID.HDIDValid:= TRUE;
  MainForm.Proteus.VerboseLogActive:= FileExists(Appdata.AppSysDir+ 'ProteusVerboseLog');
  MainForm.Proteus.ProductName:= AppData.AppName;
  MainForm.Proteus.DefaultKey := cpCertificate.GenerateTrialCertificate(AppData.AppName, 1, 365).GenerateKeyString;
  MainForm.Proteus.Initialize;

  if MainForm.Proteus.CurCertif.Demo
  then AppData.MainFormCaption('Trial expired!');

  { Splash screen }
  if NOT AppData.RunningFirstTime
  AND NOT AppData.BetaTesterMode           // It is confusing for the user to see this splash screen when the program starts for the first time AND the "setup" wizard is presented.
  then ShowSplashScreen(15292);            // This number is the filesize of the image (bytes). It is used a protection so the user cannot change it.

  { SHOW MAIN WINDOW }                     // Comes after LoadForm()
  if AppData.StartMinim
  then                                     // IF THE APPLICATION WAS MINIMIZED ON CLOSE THEN I MINIMIZE IT ON OPEN or IF IT IS SET IN USER PREFERENCES TO LEAVE MINIMIZED
   begin
     //Application.Minimize;
     MainForm.TrayIcon.PutIconOnlyInTray;  // SYS TRAY ICON. Put the correct icon in systray
   end
  else
   begin
     MainForm.Show;
     MainForm.Update;                       // Let form refresh. Else I get nasty black stripes in playlist
     MainForm.TrayIcon.PutIconOnlyInTask;   // Restore the app, but don't automatically show its taskbar icon
   end;

  { First run }
  if AppData.RunningFirstTime then
   begin
    // Preparation of the main form
    AppData.MainFormCaption('Welcome...');
    CenterForm(MainForm);
    MainForm.pgCtrl.ActivePage:= MainForm.tabMain;            // Default page to show

    if NOT AppData.RunningHome then
      begin
        // Desktop shortcuts
        LightVcl.Common.Shell.CreateShortcut(AppData.AppName, TRUE);        // OnDesktop
        LightVcl.Common.Shell.CreateShortcut(AppData.AppName, FALSE);       // OnStartMenu

        // File association
        AssociateWith('.LightSaber', AppData.AppName, FALSE, FALSE, TRUE);

        // Welcome page
        if NOT AppData.BetaTesterMode
        then LightVcl.Common.ExecuteShell.ExecuteURL(AppData.ProductWelcome);

        FormUniversalEula.ShowEulaModal;                      // EULA
        TfrmSkinDisk.ShowAsModal;                              // Choose skin. There is a bug: Form losses modal attribute after applying skin. So, I can call this ONLY at the end of initialization procedure. Even though I can click the main form, the Skins form is still marked as modal.
      end;
   end;

  { Auto updater / Internet news }
  AppData.MainFormCaption('Starting autoupdater...');
  Updater:= TUpdater.Create(UpdaterDemoURL);
  Updater.URLDownload  := AppData.ProductHome;
  Updater.URLRelHistory:= AppData.ProductHome;
  Updater.CheckForNews;

  MainForm.FontSizeChanged;
  Winapi.ShellApi.DragAcceptFiles(MainForm.Handle, True);     // Accept the dropped files from Windows Explorer
  Application.OnHint:= MainForm.CanShowHint;
  AppData.MainFormCaption('');
end;



end.
