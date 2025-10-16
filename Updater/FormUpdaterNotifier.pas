UNIT FormUpdaterNotifier;
{-------------------------------------------------------------------------------------------------------------
   Show info and updates
   2022-04-10

   THIS FORM IS USED BY MULTIPLE PROGRAMS.
   Do not localize it for a specific program!

   How to use it:
       Create the form with ShowUpdater(False);
       Call
         TUpdater.CheckForNewsNow;
         TUpdater.CheckForNewsTry;
         TUpdater.CheckForNewsDelay(Seconds);  // automatically check for news 9 seconds after program startup

   Note:
        You don't have to manually created this form. The TUpdater object will create the form for you if there are new, or if there is a connection error.

   See "ciUpdater.pas" for details.
-------------------------------------------------------------------------------------------------------------}

INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  System.SysUtils, System.Classes, Vcl.Forms, LightVcl.Common.AppDataForm,Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, Vcl.ComCtrls,
  InternetLabel,
  LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.SystemTime, LightVcl.Common.Clipboard, ciUpdater, LightVcl.Visual.RichLog, LightVcl.Visual.RichLogTrack, FormUpdaterSettings, FormUpdaterRecEditor;

CONST
  UpdaterDemoURL = 'https://www.GabrielMoraru.com/uploads/OnlineNews_v2_TemplateApp.bin'; { For demo purposes }

TYPE
  TFrmUpdater = class(TLightForm)
    Log              : TRichLog;
    PageCtrl         : TPageControl;
    btnBinFile       : TButton;
    btnCheckDelay    : TButton;
    btnCheckManually : TButton;
    btnCheckToday    : TButton;
    btnIsTimeToCheck : TButton;
    btnNewVersFound  : TButton;
    btnSettings      : TButton;
    Button1          : TButton;
    inetWhatsNew     : TInternetLabel;
    lblDownload      : TInternetLabel;
    lblConnectError  : TLabel;
    lblStatus        : TLabel;
    lblVersion       : TLabel;
    Panel1           : TPanel;
    Panel2           : TPanel;
    Panel3           : TPanel;
    pnlBottom        : TPanel;
    tabDemo          : TTabSheet;
    tabNews          : TTabSheet;
    tabRecEditor     : TTabSheet;
    LogVerb: TRichLogTrckbr;
    procedure btnBinFileClick       (Sender: TObject);
    procedure btnCheckDelayClick    (Sender: TObject);
    procedure btnCheckManuallyClick (Sender: TObject);
    procedure btnCheckTodayClick    (Sender: TObject);
    procedure btnIsTimeToCheckClick (Sender: TObject);
    procedure btnNewVersFoundClick  (Sender: TObject);
    procedure btnSettingsClick      (Sender: TObject);
    procedure btnTestInternetClick  (Sender: TObject);
    procedure OnConnectError        (Sender: TObject; Msg: string);
    procedure OnHasNews             (Sender: TObject);
    procedure OnNoNews              (Sender: TObject);
    procedure OnUpdateEnd           (Sender: TObject);
    procedure OnUpdateStart         (Sender: TObject);
    procedure FormCreate            (Sender: TObject);
    procedure FormDestroy           (Sender: TObject);
    procedure FormClose             (Sender: TObject; var Action: TCloseAction);
  private
    Demo: Boolean;

    procedure PopulateNews;
  public
    class procedure CreateForm(Demo: Boolean = FALSE); static;
 end;


IMPLEMENTATION  {$R *.DFM}

USES
   LightVcl.Visual.RichLogUtils, LightVcl.Common.Colors, LightCore.AppData, LightVcl.Common.AppData, LightVcl.Common.CursorGuard, LightVcl.Common.System, LightVcl.Internet, LightCore.Internet;






{ Show updater form (modal) }
class procedure TFrmUpdater.CreateForm(Demo: Boolean = FALSE);
VAR
   Form: TFrmUpdater;
begin
  TAppData.RaiseIfStillInitializing;
  Assert(Updater <> NIL);

  AppData.CreateFormHidden(TFrmUpdater, Form);   { Freed by ShowModal }

  { Setup updater }
  Updater.OnUpdateStart := Form.OnUpdateStart;
  Updater.OnNoNews      := Form.OnNoNews;
  Updater.OnHasNews     := Form.OnHasNews;
  Updater.OnConnectError:= Form.OnConnectError;
  Updater.OnUpdateEnd   := Form.OnUpdateEnd;

  { Demo }
  Form.Demo:= Demo;
  Form.tabDemo.TabVisible:= Demo;
  Form.tabRecEditor.TabVisible:= Demo;
  Form.LogVerb.Visible:= Demo OR AppData.BetaTesterMode;
  if Demo
  then Form.LogVerb.Verbosity:= lvrVerbose;

  { GUI }
  Form.lblDownload .Link := Updater.URLDownload;
  Form.inetWhatsNew.Link := Updater.URLRelHistory;
  Form.lblConnectError.Visible:= Updater.ConnectionError;
  Form.PopulateNews;

  { Nest the Settings form }
  //Form.frmSettings:= TfrmUpdaterSettings.Create;

  { Closed by mrOk/mrCancel }
  if Form.Visible
  then Form.Show     { Cannot make a visible window modal! }
  else Form.ShowModal;
end;






{--------------------------------------------------------------------------------------------------
   FORM CONSTRUCTOR

   The caller needs to call manually LoadForm(FrmUpdater).
   This is necessary when I need to set the form as fsMDIChild. LoadForm needs to be called after fsMDIChild
--------------------------------------------------------------------------------------------------}
procedure TFrmUpdater.FormCreate(Sender: TObject);
begin
 Demo:= FALSE;
 Assert(lblVersion.Transparent= True);         { Needed for compatibility with VCL skins }
 Font:= Application.MainForm.Font;             { Themes }
end;


procedure TFrmUpdater.FormDestroy(Sender: TObject);
begin
 { Disconnect updated from the form }
 Updater.OnNoNews      := NIL;
 Updater.OnHasNews     := NIL;
 Updater.OnConnectError:= NIL;
 Updater.OnUpdateStart := NIL;
 Updater.OnUpdateEnd   := NIL;

 ///SaveForm;
end;


procedure TFrmUpdater.FormClose(Sender: TObject; VAR Action: TCloseAction);
begin
  Action:= caFree;
  //frmSettings.Container.Parent:= frmSettings;   { We need to move the container back on its original form, in order to let that form to correctly save its children }
end;







{--------------------------------------------------------------------------------------------------
   MANUALY CHECK FOR NEWS
--------------------------------------------------------------------------------------------------}
procedure TFrmUpdater.btnCheckManuallyClick(Sender: TObject);
begin
 Log.Clear;
 Log.AddVerb('Forced GetNews');

 Updater.Delay:= 1; { Delay is ignored! }
 btnCheckManually.Enabled:= FALSE;                 { Don't let user to close the form until the HTTP thread returns something (even an error). Else, I get an acces violation in THttpThread.Create }
 LightVcl.Common.System.CursorBusy;                                       { Check for news }
 TRY
   Updater.GetNews;
   PopulateNews;
 FINALLY
  btnCheckManually.Enabled:= TRUE;
  CursorNotBusy;
 END;
end;



{--------------------------------------------------------------------------------------------------
   SHOW "NEW VERSION FOUND"
--------------------------------------------------------------------------------------------------}

procedure TFrmUpdater.PopulateNews;
begin     //clr
 { Show version }
 lblVersion.Caption:= 'You are running version '+ TAppData.GetVersionInfo
                +CRLF+'Online version is '+ Updater.NewsRec.AppVersion;

 if Updater.NewsRec.CriticalUpd
 then lblVersion.Caption:= lblVersion.Caption+ CRLF+ 'CRITICAL UPDATE!';

 Log.AddMsg(Updater.NewsRec.NewsHeadline+ CRLF);
 Log.AddMsg(Updater.NewsRec.NewsBody);

 { Show new version in red }
 if Updater.NewVersionFound then
  begin
   { Show as label }
   lblVersion.Color:= clRedFade;
   lblVersion.StyleElements:= [];
   lblVersion.Transparent:= FALSE;
  end;
end;


{--------------------------------------------------------------------------------------------------
   DEMO
--------------------------------------------------------------------------------------------------}
procedure TFrmUpdater.btnCheckTodayClick(Sender: TObject);
begin
 PageCtrl.ActivePage:= tabNews;

 Log.Clear;
 Log.AddVerb('CheckForNews Today');

 Updater.Delay:= 1; { 1 seconds delay }
 Updater.When:= cwHours;

 if NOT Updater.IsTimeToCheckAgain
 then Log.AddInfo('Already checked for news today.');

 Updater.CheckForNews;
end;


procedure TFrmUpdater.btnIsTimeToCheckClick(Sender: TObject);
begin
 Log.Clear;
 Log.AddInfo('Last succesful check for updates: '+ DateTimeToStr(Updater.LastUpdate));
 PageCtrl.ActivePage:= tabNews;

 if Updater.IsTimeToCheckAgain
 then Log.AddInfo('IsTimeToCheckAgain: Yes')
 else Log.AddInfo('IsTimeToCheckAgain: No');
end;


procedure TFrmUpdater.btnNewVersFoundClick(Sender: TObject);
begin
 if Updater.NewVersionFound
 then Log.AddInfo('NewVersionFound')
 else Log.AddInfo('No NewVersionFound');

 PageCtrl.ActivePage:= tabNews;
end;


procedure TFrmUpdater.btnCheckDelayClick(Sender: TObject);
begin
 Log.Clear;
 Log.AddInfo('CheckForNews Delay: 3sec');
 PageCtrl.ActivePage:= tabNews;

 Updater.Delay:= 1; { 1 seconds delay }
 Updater.When:= cwStartUp;
 Updater.CheckForNews;
end;






{--------------------------------------------------------------------------------------------------
   EVENTS
--------------------------------------------------------------------------------------------------}
procedure TFrmUpdater.OnUpdateEnd(Sender: TObject);
begin
 Log.AddVerb('End updater');
end;


procedure TFrmUpdater.OnUpdateStart(Sender: TObject);
begin
 lblConnectError.Visible:= FALSE;
 lblStatus.Visible:= TRUE;
 lblStatus.Transparent:= FALSE;
 lblStatus.Color:= clOrange;                 { Change font color, not bkg color to keep it compatible with 'Skins' }
 lblStatus.Caption:= 'Checking for news...';
 lblStatus.Visible:= TRUE;
 lblStatus.Refresh;                          { A blocking operation follows so we need to update this control to let user know that the update started }
 Log.AddVerb(lblStatus.Caption);
end;


procedure TFrmUpdater.OnHasNews(Sender: TObject);
begin
 lblStatus.Visible:= TRUE;
 lblStatus.Transparent:= FALSE;
 lblStatus.Color:= clLimeDark;                     { Change font color, not bkg color to keep it compatible with 'Skins' }
 lblStatus.Font.Color:= clBlueNight;
 lblStatus.Caption:= 'We have news for you!';
 Log.AddImpo('We have news for you!');

 { Always show "new version available" if the user has an old version! }
 PopulateNews;

 if Updater.ForceNewsFound
 then lblStatus.Caption:= lblStatus.Caption+ ' (BT: Forced)';

 Assert(FormStyle<> fsMDIChild);

 { Announce that we check for news so the user will know why the GUI freezes }
 if Visible
 then Show     { Cannot make a visible window modal! }
 else ShowModal;
end;



procedure TFrmUpdater.OnNoNews(Sender: TObject);
begin
 lblStatus.Visible:= TRUE;
 lblStatus.Transparent:= TRUE;
 lblStatus.Caption:= 'No news today.';
end;



procedure TFrmUpdater.OnConnectError(Sender: TObject; Msg: string);
begin
 lblConnectError.Visible:= TRUE;
 lblStatus.Visible:= FALSE;
 Log.AddError(Msg);

 if Visible
 then Show     { Cannot make a visible window modal! }
 else ShowModal;
end;














{--------------------------------------------------------------------------------------------------
   GUI
--------------------------------------------------------------------------------------------------}
procedure TFrmUpdater.btnSettingsClick(Sender: TObject);
begin
  TfrmUpdaterSettings.CreateModal;
end;


procedure TFrmUpdater.btnTestInternetClick(Sender: TObject);
begin
  lblConnectError.Visible:=LightVcl.Internet.TestProgramConnection(TRUE) <= 0;
end;


procedure TFrmUpdater.btnBinFileClick(Sender: TObject);
begin
  TfrmRecEditor.CreateFormModal;   { Generate the BIN file that we upload on FTP }
end;


end.
