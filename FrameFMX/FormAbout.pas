UNIT FormAbout;

{=============================================================================================================
   2026.04.23
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Template "About" form — FMX version

   CROSS-PLATFORM: Yes (Windows, macOS, Android, iOS)
   PROTEUS: Windows-only (cpProteus uses Vcl.Controls — not yet ported to FMX)

--------------------------------------------------------------------------------------------------------------
   Reads data (program name, website, etc) from AppData.
   Shows Trial details (from Proteus). If you don't have Proteus, just ignore this part.
   Form closed with Escape or Enter.

   USAGE:
     1. Modal (no callback):
          TfrmAboutApp.CreateFormModal;

     2. Modal with close callback (needed on Android — ShowModal is non-blocking there):
          TfrmAboutApp.CreateFormModal(
            procedure
            begin
              // runs once after form closes
            end);

     3. Logo image (optional — imgLogo is empty by default):
          Drop a Logo.png in AppData.AppSysDir; FormCreate loads it automatically.

   Optional per-instance fields (Proteus, EULAURL, CreditsURL, CreditsText):
     CreateFormModal instantiates and shows the form internally, so it returns no reference.
     To use these, either extend CreateFormModal's signature or instantiate manually:
       AppData.CreateForm(TfrmAboutApp, Form, asNone);
       Form.EULAURL:= '...';
       AppData.ShowModal(Form);

   DON'T ADD IT TO ANY DPK!
=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON}
{.DEFINE USEPROTEUS}

USES
  System.SysUtils, System.Classes, System.UITypes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Objects, FMX.Layouts, FMX.Controls.Presentation,
  LightFmx.Common.AppData.Form, FMX.TabControl, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo
  {$IFDEF USEPROTEUS}
  , cpProteus
  {$ENDIF};

TYPE
  TfrmAboutApp = class(TLightForm)
    Container   : TLayout;
    imgLogo     : TImage;
    lblVersion  : TLabel;
    lblCompany  : TLabel;
    lblCredits  : TLabel;
    lblChildren : TLabel;
    lblExpire   : TLabel;
    inetEULA    : TLabel;
    btnEnterKey : TButton;
    btnOrderNow : TButton;
    TabControl: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    lblAppName: TLabel;
    Memo: TMemo;
    procedure FormCreate       (Sender: TObject);
    procedure FormKeyDown      (Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure btnEnterKeyClick (Sender: TObject);
    procedure btnOrderNowClick (Sender: TObject);
    procedure lblCompanyClick  (Sender: TObject);
    procedure inetEULAClick    (Sender: TObject);
    procedure lblCreditsClick  (Sender: TObject);
    procedure lblChildrenClick (Sender: TObject);
  private
    FProductHome       : string;
    FProductOrder      : string;
    FEULAURL           : string;
    FCreditsURL        : string;
    FChildrenTapCount  : Integer;
    procedure UpdateBetaTesterVisual;
  public
    {$IFDEF USEPROTEUS}
    Proteus: TProteus;
    {$ENDIF}
    EULAURL    : string;   // Optional. Set before showing. If empty, EULA label stays hidden.
    CreditsURL : string;   // Optional. Set before showing. If empty, Credits label stays hidden.
    CreditsText: string;   // Optional. Caption for credits label.

    class procedure CreateFormModal(AAfterClose: TProc = NIL); static;
  end;


IMPLEMENTATION {$R *.fmx}

USES
  {$IFDEF MSWINDOWS} WinApi.Windows, {$ENDIF}
  System.UIConsts, System.IOUtils,
  LightCore.AppData, LightCore.Internet, LightCore.IO, LightCore, LightCore.Reports, LightCore.TextFile,
  LightFmx.Common.AppData, LightFmx.Common.Dialogs, LightFmx.Common.Screen;




{ Creates and displays the About form modally.
  AAfterClose fires once in FormPreRelease. Needed on Android where ShowModal is non-blocking. }
class procedure TfrmAboutApp.CreateFormModal(AAfterClose: TProc);
var
  Form: TfrmAboutApp;
begin
  AppData.CreateForm(TfrmAboutApp, Form, asNone);
  Form.AfterClose:= AAfterClose;
  {$IFDEF USEPROTEUS}
  Form.btnOrderNow.Visible:= TRUE;
  Form.btnEnterKey.Visible:= TRUE;
  {$ENDIF}

  AppData.ShowModal(Form);
end;


{ Initializes the About form with application and license information. }
procedure TfrmAboutApp.FormCreate(Sender: TObject);
begin
  CloseOnEscape:= TRUE;

  FProductHome := AppData.ProductHome;
  FProductOrder:= AppData.ProductOrder;
  FEULAURL     := EULAURL;
  FCreditsURL  := CreditsURL;

  {$IFDEF USEPROTEUS}
  if Proteus <> NIL then
    begin
      btnOrderNow.Visible:= NOT Proteus.CurCertif.Platit;
      lblExpire.Visible  := TRUE;
      if Proteus.CurCertif.Trial
      then lblExpire.Text:= 'Lite edition'
      else lblExpire.Text:= 'Registered';
    end
  else
    begin
      btnOrderNow.Visible:= FALSE;
      btnEnterKey.Visible:= FALSE;
      lblExpire.Visible  := FALSE;
    end;
  {$ELSE}
  btnOrderNow.Visible:= FALSE;
  btnEnterKey.Visible:= FALSE;
  lblExpire.Visible  := FALSE;
  {$ENDIF}

  lblCompany.Text  := AppData.CompanyName;
  lblAppName.Text  := AppData.AppName;
  lblVersion.Text  := AppData.GetAppVersion;

  // Clickable company label — show only if URL available
  lblCompany.Visible := FProductHome <> '';

  // EULA link
  inetEULA.Visible:= FEULAURL <> '';

  // Credits link
  if FCreditsURL <> '' then
    begin
      lblCredits.Text   := CreditsText;
      lblCredits.Visible:= TRUE;
    end
  else
    lblCredits.Visible:= FALSE;

  if imgLogo.Bitmap.IsEmpty
  AND FileExists(AppData.AppSysDir+ 'Logo.png')
  then imgLogo.Bitmap.LoadFromFile(AppData.AppSysDir+ 'Logo.png');

  // Secret BetaTester toggle — tap lblChildren CHILDREN_TAPS_TO_TOGGLE times
  FChildrenTapCount:= 0;
  UpdateBetaTesterVisual;

  // Reports
  Memo.Lines.Clear;
  Memo.Lines.Add('=< CORE REPORT >=');
  Memo.Lines.Add(GenerateCoreReport);
  Memo.Lines.Add('');
  Memo.Lines.Add('=< SCREEN RESOLUTION >=');
  Memo.Lines.Add(GenerateScreenResolutionRep);   //   Tester: LightSaber\Demo\FMX\Demos\FMX_Demos.dpr
end;


{ Closes the form on Enter or Escape. }
procedure TfrmAboutApp.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key in [vkReturn, vkEscape] then Close;
end;


{ Displays the license key entry dialog. Requires Proteus. }
procedure TfrmAboutApp.btnEnterKeyClick(Sender: TObject);
begin
  {$IFDEF USEPROTEUS}
  Assert(Proteus <> NIL, 'Proteus not assigned! Set TfrmAboutApp.Proteus before showing.');
  if Proteus.ShowEnterKeyBox
  then MessageInfo('Key accepted. Please restart the program.')
  else MessageError('Key not accepted!');
  {$ENDIF}
end;


procedure TfrmAboutApp.btnOrderNowClick(Sender: TObject);
begin
  OpenURL(FProductOrder);
end;


procedure TfrmAboutApp.lblCompanyClick(Sender: TObject);
begin
  OpenURL(FProductHome);
end;


procedure TfrmAboutApp.inetEULAClick(Sender: TObject);
begin
  OpenURL(FEULAURL);
end;


procedure TfrmAboutApp.lblCreditsClick(Sender: TObject);
begin
  OpenURL(FCreditsURL);
end;


{ Paints lblChildren red when BetaTester mode is active — visual cue that survives form reopen.
  FontColor must be removed from StyledSettings so the style does not override our color. }
procedure TfrmAboutApp.UpdateBetaTesterVisual;
begin
  lblChildren.StyledSettings:= lblChildren.StyledSettings - [TStyledSetting.FontColor];
  if AppData.BetaTesterMode
  then lblChildren.TextSettings.FontColor:= claRed
  else lblChildren.TextSettings.FontColor:= claNull;   // claNull lets the style default paint it
end;


{ Secret trigger. Tap CHILDREN_TAPS_TO_TOGGLE times to toggle BetaTesterMode.
  BetaTesterMode is driven by a file on disk (see TAppDataCore.BetaTesterMode) — we create/delete it here.
  Why: gives support staff a way to enable developer features on a user's device without a rebuild. }
CONST
  TapsToToggleBetaMode = 8;

procedure TfrmAboutApp.lblChildrenClick(Sender: TObject);
begin
  // Activate beta tester mode
  Inc(FChildrenTapCount);
  if FChildrenTapCount < TapsToToggleBetaMode then EXIT;
  FChildrenTapCount:= 0;
  VAR BetaFile:= AppData.AppSysDir+ 'betatester';
  stringtofile(BetaFile, 'Beta mode activated via About form!', woOverwrite, wpAuto);
  UpdateBetaTesterVisual;

  {$IFDEF MSWINDOWS}
  WinApi.Windows.Beep(1200, 80);
  {$ENDIF}

  if AppData.BetaTesterMode
  then MessageInfo('BetaTester mode: ON')
  else MessageInfo('BetaTester mode: OFF');
end;


end.
