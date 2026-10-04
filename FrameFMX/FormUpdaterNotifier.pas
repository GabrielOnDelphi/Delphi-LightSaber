UNIT FormUpdaterNotifier;

{=============================================================================================================
   2026.10.03
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   FMX port of the VCL FormUpdaterNotifier (Updater\FormUpdaterNotifier.pas).

   Shows news/updates from the online updater (TUpdater in ciUpdater.pas) to the end user.

   CROSS-PLATFORM: Yes (Windows, macOS, iOS, Android)
   SCOPE: End-user notifier only. Demo/record-editor tabs and TestProgramConnectionMsg
          are intentionally omitted from this port — they live only in the VCL version.

   THIS FORM IS USED BY MULTIPLE PROGRAMS. Do not localize it for a specific program.

   How to use it:
     1. Modal (no callback):
          TfrmUpdater.CreateFormModal;

     2. Modal with after-close callback (needed on Android — ShowModal is non-blocking there):
          TfrmUpdater.CreateFormModal(
            procedure
            begin
              // runs once after form closes
            end);

   Note:
     TUpdater does NOT construct this form (ciUpdater has no reference to this unit).
     The host app must create it (CreateFormModal); until then TUpdater's events fire into
     unassigned handlers and are silently dropped.
     Mobile: TUpdater.GetNews runs THTTPClient synchronously and will freeze the UI for the
     duration of the download. A future improvement is to wrap it in a TTask.

   See "ciUpdater.pas" for the model behind this view.
=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON}

USES
  System.SysUtils, System.Classes, System.UITypes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  LightFmx.Common.AppData.Form,
  ciUpdater;


TYPE
  TfrmUpdater = class(TLightForm)
    Container       : TLayout;
    pnlTop          : TLayout;
    pnlBottom       : TLayout;
    lblVersion      : TLabel;
    lblStatus       : TLabel;
    lblConnectError : TLabel;
    Memo            : TMemo;
    lblDownload     : TLabel;
    lblRelHistory   : TLabel;
    btnSettings     : TButton;
    btnClose        : TButton;
    procedure FormCreate          (Sender: TObject);
    procedure FormClose           (Sender: TObject; var Action: TCloseAction);
    procedure btnSettingsClick    (Sender: TObject);
    procedure btnCloseClick       (Sender: TObject);
    procedure lblDownloadClick    (Sender: TObject);
    procedure lblRelHistoryClick  (Sender: TObject);
  private
    HostEvents: RUpdaterEvents;   { The handlers that the host program had set on the Updater before this form set its own. FormPreRelease gives them back. }
    procedure OnUpdateStart   (Sender: TObject);
    procedure OnUpdateEnd     (Sender: TObject);
    procedure OnHasNews       (Sender: TObject);
    procedure OnNoNews        (Sender: TObject);
    procedure OnConnectError  (Sender: TObject; Msg: string);
    procedure PopulateNews;
    procedure SetStatusColor  (AColor: TAlphaColor);
    procedure ResetStatusColor;
  public
    procedure FormPreRelease; override;
    class procedure CreateFormModal(AAfterClose: TProc = NIL); static;
  end;


IMPLEMENTATION {$R *.fmx}

USES
  LightCore, LightCore.AppData, LightCore.Internet,
  LightFmx.Common.AppData,
  FormUpdaterSettings;


{ The version of the running program, all four numbers on Windows (9.55.1.0).
  AppData.GetAppVersion returns only Major.Minor on Windows (FMX.Platform.Win.pas, TPlatformWin.GetVersionString),
  and CompareVersions counts the missing numbers as 0, so a running 9.55.1 compared as 9.55.0.0 and NewVersionFound
  said TRUE for the version the user already runs. Off Windows, or with no version resource: AppData.GetAppVersion. }
function RunningVersion: string;
begin
  Result:= GetFileVersionFull(ParamStr(0));
  if Result = ''
  then Result:= AppData.GetAppVersion;
end;



{ Build and show the notifier form modally.
  AAfterClose fires once in FormPreRelease — needed on Android where ShowModal is non-blocking. }
class procedure TfrmUpdater.CreateFormModal(AAfterClose: TProc);
VAR
  Form: TfrmUpdater;
begin
  Form:= NIL;
  AppData.CreateForm(TfrmUpdater, Form, asNone);
  Assert(Form <> NIL, 'CreateFormModal: Form was not created (called during initialization?).');
  Form.AfterClose:= AAfterClose;
  AppData.ShowModal(Form);
end;


procedure TfrmUpdater.FormCreate(Sender: TObject);
begin
  Assert(Updater <> NIL, 'TUpdater (Updater global) must be created before opening TfrmUpdater.');

  CloseOnEscape:= TRUE;

  { Wire updater events to this form. Keep the handlers of the host program, to give them back on close. }
  HostEvents.ReadFrom(Updater);
  Updater.OnUpdateStart  := OnUpdateStart;
  Updater.OnHasNews      := OnHasNews;
  Updater.OnNoNews       := OnNoNews;
  Updater.OnConnectError := OnConnectError;
  Updater.OnUpdateEnd    := OnUpdateEnd;

  { Initial GUI state. lblConnectError is shown ONLY when we have a real message
    to display — OnConnectError populates Text and Visible together. }
  lblConnectError.Text   := '';
  lblConnectError.Visible:= FALSE;
  lblStatus.Text         := '';

  lblDownload.Visible   := Updater.URLDownload   <> '';
  lblRelHistory.Visible := Updater.URLRelHistory <> '';

  PopulateNews;
end;


{ Free the form on close so reopening creates a fresh instance.
  Default FMX close action is caHide — without this the form would leak until app shutdown. }
procedure TfrmUpdater.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= TCloseAction.caFree;
end;


{ Guaranteed single-call cleanup (see LightFmx.Common.AppData.Form). Gives the Updater back the handlers of the
  host program, so the Updater never calls this (soon freed) form, and the host gets its own events again. }
procedure TfrmUpdater.FormPreRelease;
begin
  if Updater <> NIL
  then HostEvents.WriteTo(Updater);
  inherited FormPreRelease;
end;


{ Read current state from Updater.NewsRec and render into the version label + memo. }
procedure TfrmUpdater.PopulateNews;
begin
  lblVersion.Text:= 'You are running version ' + RunningVersion
              + CRLF + 'Online version is '   + Updater.NewsRec.AppVersion;

  if Updater.NewsRec.CriticalUpd
  then lblVersion.Text:= lblVersion.Text + CRLF + 'CRITICAL UPDATE!';

  Memo.Lines.Clear;
  if Updater.NewsRec.NewsHeadline <> ''
  then Memo.Lines.Add(Updater.NewsRec.NewsHeadline);

  if Updater.NewsRec.NewsBody <> '' then
    begin
      if Memo.Lines.Count > 0
      then Memo.Lines.Add('');
      Memo.Lines.Add(Updater.NewsRec.NewsBody);
    end;

  { Highlight version label when a newer build is online }
  if Updater.NewVersionFound(RunningVersion) then
    begin
      lblVersion.StyledSettings:= lblVersion.StyledSettings - [TStyledSetting.FontColor];
      lblVersion.TextSettings.FontColor:= TAlphaColors.Red;
    end;
end;


{ FMX labels don't have a Color property like VCL — we change FontColor instead.
  Strips FontColor from StyledSettings so the manual color wins over the active style. }
procedure TfrmUpdater.SetStatusColor(AColor: TAlphaColor);
begin
  lblStatus.StyledSettings:= lblStatus.StyledSettings - [TStyledSetting.FontColor];
  lblStatus.TextSettings.FontColor:= AColor;
end;


{ Neutral status: hand FontColor back to the active style so the text stays readable on dark skins too. }
procedure TfrmUpdater.ResetStatusColor;
begin
  lblStatus.StyledSettings:= lblStatus.StyledSettings + [TStyledSetting.FontColor];
end;


{--------------------------------------------------------------------------------------------------
   UPDATER EVENTS
--------------------------------------------------------------------------------------------------}
procedure TfrmUpdater.OnUpdateStart(Sender: TObject);
begin
  lblConnectError.Visible:= FALSE;
  lblStatus.Text:= 'Checking for news...';
  SetStatusColor(TAlphaColors.Orange);
  lblStatus.Visible:= TRUE;
end;


procedure TfrmUpdater.OnUpdateEnd(Sender: TObject);
begin
  { No-op: OnHasNews / OnNoNews / OnConnectError already updated the status. }
end;


procedure TfrmUpdater.OnHasNews(Sender: TObject);
begin
  lblStatus.Text:= 'We have news for you!';
  SetStatusColor(TAlphaColors.Green);
  lblStatus.Visible:= TRUE;

  PopulateNews;

  if Updater.ForceNewsFound
  then lblStatus.Text:= lblStatus.Text + ' (BT: Forced)';
end;


procedure TfrmUpdater.OnNoNews(Sender: TObject);
begin
  lblStatus.Text:= 'No news today.';
  ResetStatusColor;
  lblStatus.Visible:= TRUE;
end;


procedure TfrmUpdater.OnConnectError(Sender: TObject; Msg: string);
begin
  lblConnectError.Text:= Msg;
  lblConnectError.Visible:= TRUE;
  lblStatus.Visible:= FALSE;
end;


{--------------------------------------------------------------------------------------------------
   USER ACTIONS
--------------------------------------------------------------------------------------------------}
procedure TfrmUpdater.btnSettingsClick(Sender: TObject);
begin
  TfrmUpdaterSettings.CreateModal;
end;


procedure TfrmUpdater.btnCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TfrmUpdater.lblDownloadClick(Sender: TObject);
begin
  if Updater.URLDownload <> ''
  then OpenURL(Updater.URLDownload);
end;


procedure TfrmUpdater.lblRelHistoryClick(Sender: TObject);
begin
  if Updater.URLRelHistory <> ''
  then OpenURL(Updater.URLRelHistory);
end;


end.
