UNIT uInitialization;

INTERFACE

USES
   Winapi.ShellApi, Vcl.Forms, Vcl.Dialogs, Vcl.Controls;

CONST
   //Mutex        = 'Tester.SingleInstanceMutex';         { Do not change it. Must be the same as the one in Uninstaller }
   HomePage     = 'https://www.GabrielMoraru.com';
   UpdaterURL   = 'https://www.GabrielMoraru.com/updaterTest.bin';


procedure LateInitialization;

IMPLEMENTATION

USES 
  chHardID, cvIniFile, csShell, csExecuteShell, cmGuiSettings,
  cpProteusCertificate, cbAppData, cbCenterControl, cTranslate, ciUpdater,
  FormMain, FormUniversalEula, FormSkinsDisk, FormSettings, FormSplashScreen;


procedure LateInitialization;
begin
 Randomize;

 AppData.CompanyName:= 'SciVance';
 AppData.ProductHomePage:= 'http://www.Laboratories.tech';
 AppData.SupportPage:= HomePage;
 AppData.UninstReason:= HomePage;

 { Settings }
 GuiSettings:= TGuiSettings.Create;
 GuiSettings.Load;

 Winapi.ShellApi.DragAcceptFiles(MainForm.Handle, True); // Accept the dropped files from Windows Explorer
 Application.OnHint:= MainForm.CanShowHint;

 { Skins }
 if AppData.RunningFirstTime
 AND NOT AppData.RunningHome
 then LoadLastSkin('Light AmethystKamri.vsf') // 'Light AmethystKamri.vsf' too much blue
 else LoadLastSkin();

 { Translator }
 Translator:= TTranslator.Create;  // Initialize the translator
 if NOT AppData.RunningHome
 then Translator.LoadLastTranslation;   // Load last language and apply it to all existing forms

 { Splash screen }
 if NOT AppData.RunningFirstTime
 AND NOT AppData.BetaTesterMode  { It is confusing for the user to see this splash screen when the program starts for the first time AND the "setup" wizard is presented. }
 then ShowSplashScreen(15292);

 { LICENSE }
 { Load the log early othewise it will overwrite the existing text }
 MainForm.Proteus.ProductName:= AppData.AppName;
 MainForm.Proteus.DefaultKey := cpProteusCertificate.GenerateTrialCertificate(AppData.AppName, 1, 365).GenerateKeyString;
 MainForm.Proteus.Initialize;

 if MainForm.Proteus.CurCertif.Demo  then
  begin
   //MesajWarning('Trial expired!');
   MainForm.Caption:= MainForm.Caption+ ' Demo';
  end;
 chHardID.HDIDValid:= TRUE;


 { SHOW MAIN WINDOW }                     // Comes after LoadForm()
 if AppData.StartMinim
 then                                     // IF THE APPLICATION WAS MINIMIZED ON CLOSE THEN I MINIMIZE IT ON OPEN or IF IT IS SET IN USER PREFERENCES TO LEAVE MINIMIZED
  begin
    { SYS TRAY ICON }
    Application.Minimize;
    MainForm.TrayIcon.PutIconOnlyInTray;  { Put the correct icon in systray }
  end
 else
  begin
    MainForm.Show;
    MainForm.Update;                       { Let form refresh. Else I get nasty black stripes in playlist }
    MainForm.TrayIcon.PutIconOnlyInTask;   { Restore the app, but don't automatically show its taskbar icon }
  end;

 AppData.LogInfo(' ');
 AppData.LogInfo(' '+ AppData.AppName+ AppData.GetVersionInfoV);
 AppData.LogInfo(' ');

 { FIRST RUN }
 if AppData.RunningFirstTime then
  begin
   { Preparation of the main form }
   AppData.MainFormCaption('Welcome...');
   CenterForm(MainForm);
   MainForm.pgCtrl.ActivePage:= MainForm.tabMain;       { Default page to show }

   { Desktop shortcuts/Association }
   csShell.CreateShortcut(AppData.AppName, TRUE);       { OnDesktop }
   csShell.CreateShortcut(AppData.AppName, FALSE);      { OnStartMenu }
   ///AssociateWith('.MyFile', 'My app file', FALSE, FALSE, TRUE);

   if NOT AppData.BetaTesterMode
   then csExecuteShell.ExecuteURL(HomePage);            { Force show help page }

   { About }
   //FormAbout.ShowAboutParented(HomePage, MainForm.tabAbout);
   if NOT AppData.RunningHome
   then FormUniversalEula.ShowEulaModal;
  end;

 { Initialization end }
 Assert(Vcl.Dialogs.UseLatestCommonDialogs= TRUE);      { This is true anyway by defaul, but I check it to remember myself about it. Details: http://stackoverflow.com/questions/7944416/tfileopendialog-requires-windows-vista-or-later }

 { Auto updater }
    // For testing purposes
 CONST wwwBioniXWall     = 'http://www.BionixWallpaper.com/';
 CONST wwwUpdaterBinTest = 'downloads/Bionix%20Desktop%20Wallpaper%20Changer/OnlineNews.bin';

 
 Updater:= TUpdater.Create(wwwUpdaterBinTest); //wwwUpdaterURL
 Updater.URLDownload    := wwwBioniXWall+ '/downloads/index.html#soft'; // wwwDwnldPage;
 Updater.URLRelHistory  := wwwBioniXWall+ '/downloads/Bionix%20Desktop%20Wallpaper%20Changer/release-history.html#soft'; // wwwReleaseHistory;

 { Let user choose skin }
 if AppData.RunningFirstTime
 AND NOT AppData.RunningHome
 then TfrmSkinDisk.CreateFormModal;  // There is a bug: Form losses modal attribute after applying skin. So, I can call this ONLY at the end of initialization procedure. Even though I can click the main form, the Skins form is still marked as modal.

 MainForm.FontSizeChanged;
 AppData.MainFormCaption('');
end;


end.
