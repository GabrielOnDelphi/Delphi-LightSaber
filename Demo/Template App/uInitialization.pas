UNIT uInitialization;
{--------------------------------------------------------------------------------------------------
  Initialization
  2024.09
--------------------------------------------------------------------------------------------------}

INTERFACE

USES
   Winapi.ShellApi, System.SysUtils, Vcl.Forms, Vcl.Dialogs, Vcl.Controls;

   //Mutex        = 'LabBook.SingleInstanceMutex';         { Do not change it. Must be the same as the one in Uninstaller }
   //UpdaterURL= HomePage+ '/LabBook.updater.bin';

procedure LateInitialization;

IMPLEMENTATION

USES
  chHardID, csShell, csExecuteShell, cmGuiSettings,
  cpCertificate, cbAppData, cbCenterControl, cTranslate, ciUpdater,
  FormMain, FormUniversalEula, FormSkinsDisk, FormSettings, FormSplashScreen, FormUpdaterNotifier;


procedure LateInitialization;
begin
 { Application }
 AppData.CompanyName    := 'SciVance Technologies';
 ///AppData.ProductHome    := HomePage;
 ///AppData.ProductSupport := HomePage;
 ///AppData.ProductUninstal:= HomePage;

 { Settings }
 GuiSettings:= TGuiSettings.Create;
 GuiSettings.Load;

 { Skins }
 if AppData.RunningFirstTime
 AND NOT AppData.RunningHome
 then LoadLastSkin('Light AmethystKamri.vsf') // 'Light AmethystKamri.vsf' too much blue
 else LoadLastSkin();

 { Translator }
 AppData.MainFormCaption('Loading translations...');
 Translator:= TTranslator.Create;
 if NOT AppData.RunningHome
 then Translator.LoadLastTranslation;   // Load last language and apply it to all existing forms

 { LICENSE }
 { Load the log early othewise it will overwrite the existing text }
 MainForm.Proteus.VerboseLogActive:= FileExists(AppData.SysDir+ 'ProteusVerboseLog');
 MainForm.Proteus.ProductName:= AppData.AppName;
 MainForm.Proteus.DefaultKey := cpCertificate.GenerateTrialCertificate(AppData.AppName, 1, 365).GenerateKeyString;
 MainForm.Proteus.Initialize;

 if MainForm.Proteus.CurCertif.Demo  then
   begin
     //MesajWarning('Trial expired!');
     MainForm.Caption:= MainForm.Caption+ ' Demo';
   end;
 chHardID.HDIDValid:= TRUE;
 
 { Splash screen }
 if NOT AppData.RunningFirstTime
 AND NOT AppData.BetaTesterMode           // It is confusing for the user to see this splash screen when the program starts for the first time AND the "setup" wizard is presented.
 then ShowSplashScreen(15292);


 { SHOW MAIN WINDOW }                     // Comes after LoadForm()
 if AppData.StartMinim
 then                                     // IF THE APPLICATION WAS MINIMIZED ON CLOSE THEN I MINIMIZE IT ON OPEN or IF IT IS SET IN USER PREFERENCES TO LEAVE MINIMIZED
  begin
    { SYS TRAY ICON }
    Application.Minimize;
    MainForm.TrayIcon.PutIconOnlyInTray;  // Put the correct icon in systray }
  end
 else
  begin
    MainForm.Show;
    MainForm.Update;                       // Let form refresh. Else I get nasty black stripes in playlist
    MainForm.TrayIcon.PutIconOnlyInTask;   // Restore the app, but don't automatically show its taskbar icon
  end;


 { FIRST RUN }
 if AppData.RunningFirstTime then
  begin
   { Preparation of the main form }
   AppData.MainFormCaption('Welcome...');
   CenterForm(MainForm);
   MainForm.pgCtrl.ActivePage:= MainForm.tabMain;            // Default page to show

   if NOT AppData.RunningHome then
     begin
       { Desktop shortcuts/Association }
       csShell.CreateShortcut(AppData.AppName, TRUE);        // OnDesktop
       csShell.CreateShortcut(AppData.AppName, FALSE);       // OnStartMenu

       AssociateWith('.LightSaber', AppData.AppName, FALSE, FALSE, TRUE);

      if NOT AppData.BetaTesterMode
      then csExecuteShell.ExecuteURL(AppData.ProductWelcome);// Welcome page
       FormUniversalEula.ShowEulaModal;                      // EULA
       TfrmSkinDisk.CreateFormModal;                         // Choose skin. There is a bug: Form losses modal attribute after applying skin. So, I can call this ONLY at the end of initialization procedure. Even though I can click the main form, the Skins form is still marked as modal.
     end;
  end;

 { Auto updater }
 AppData.MainFormCaption('Starting autoupdater...');
 Updater:= TUpdater.Create(UpdaterURL);
 Updater.URLDownload  := AppData.ProductHome;
 Updater.URLRelHistory:= AppData.ProductHome;
 Updater.CheckForNews;

 MainForm.FontSizeChanged;
 Winapi.ShellApi.DragAcceptFiles(MainForm.Handle, True);     // Accept the dropped files from Windows Explorer
 Application.OnHint:= MainForm.CanShowHint;
 AppData.MainFormCaption('');
end;


end.
