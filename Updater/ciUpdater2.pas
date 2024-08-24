INTERFACE

USES
  System.SysUtils, System.DateUtils, System.Classes, Vcl.ExtCtrls, ciUpdaterRec;

TYPE
  TCheckWhen = (cwNever, cwStartUp, cwHours);

  TUpdater = class(TObject)
  private
    Timer: TTimer;
    LocalNewsID: Integer;
    FUpdaterStart: TNotifyEvent;
    FUpdaterEnd: TNotifyEvent;
    FConnectError: TNotifyEvent;
    FHasNews: TNotifyEvent;
    FNoNews: TNotifyEvent;
    procedure GetNewsDelay;
    procedure TimerTimer(Sender: TObject);
    procedure Clear;
    function TooLongNoSee: Boolean;
  protected
    URLNewsFile: string;
    procedure HandleError(const ErrorMsg: string);
  public
    Delay: Integer;
    When: TCheckWhen;
    CheckEvery: Integer;
    ShowConnectFail: Boolean;
    ForceNewsFound: Boolean;
    URLDownload: string;
    URLRelHistory: string;
    NewsRec: RNews;
    HasNews: Boolean;
    LastUpdate: TDateTime;
    ConnectionError: Boolean;

    constructor Create(const aURLNewsFile: string);
    destructor Destroy; override;

    function NewVersionFound: Boolean;
    function IsTimeToCheckAgain: Boolean;
    procedure CheckForNews;
    function GetNews: Boolean;
    procedure LoadFrom(const FileName: string);
    procedure SaveTo(const FileName: string);
    procedure Load;
    procedure Save;

    property OnUpdateStart: TNotifyEvent read FUpdaterStart write FUpdaterStart;
    property OnHasNews: TNotifyEvent read FHasNews write FHasNews;
    property OnNoNews: TNotifyEvent read FNoNews write FNoNews;
    property OnConnectError: TNotifyEvent read FConnectError write FConnectError;
    property OnUpdateEnd: TNotifyEvent read FUpdaterEnd write FUpdaterEnd;
  end;

VAR
  Updater: TUpdater;

IMPLEMENTATION

USES
  FormAsyncMessage, ciDownload, ccINIFile, cmDebugger, cbAppData;

Const
  TooLongNoSeeInterval = 180;

constructor TUpdater.Create(const aURLNewsFile: string);
begin
  Assert(Updater = nil, 'Updater already created!');
  inherited Create;
  URLNewsFile := aURLNewsFile;

  Timer := TTimer.Create(nil);
  Timer.Enabled := False;
  Timer.OnTimer := TimerTimer;

  Clear;

  if AppData.RunningFirstTime then
    Delay := 300
  else
    Delay := 30;

  if FileExists(AppData.IniFile) then
    Load;
end;

procedure TUpdater.Clear;
begin
  NewsRec.Clear;

  When := cwHours;
  HasNews := False;
  LocalNewsID := 0;
  LastUpdate := 0;

  CheckEvery := 12;
  ForceNewsFound := False;
  ShowConnectFail := True;
end;

destructor TUpdater.Destroy;
begin
  FreeAndNil(Timer);

  try
    Save;
  except
    on E: Exception do
      cmDebugger.OutputDebugStr(E.Message);
  end;

  inherited Destroy;
end;

procedure TUpdater.CheckForNews;
begin
  case When of
    cwNever:
      if TooLongNoSee then
        GetNewsDelay;
    cwStartUp:
      GetNewsDelay;
    cwHours:
      if IsTimeToCheckAgain then
        GetNewsDelay;
  else
    raise Exception.Create('Unknown type in TCheckWhen');
  end;
end;

procedure TUpdater.GetNewsDelay;
begin
  if Delay = 0 then
    GetNews
  else
  begin
    Timer.Interval := Delay * 1000;
    Timer.Enabled := True;
  end;
end;

procedure TUpdater.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := False;
  GetNews;
end;

function UpdaterFileLocation: string;
begin
  Result := AppData.AppDataFolder + 'OnlineNews.bin';
end;

function TUpdater.GetNews: Boolean;
begin
  Timer.Enabled := False;
  HasNews := False;
  Assert(URLNewsFile <> '', 'Updater URLNewsFile is empty!');

  if Assigned(FUpdaterStart) then
    FUpdaterStart(Self);

  try
    Result := ciDownload.DownloadFile(URLNewsFile, '', UpdaterFileLocation, True);

    if Result then
    begin
      Result := NewsRec.LoadFrom(UpdaterFileLocation);

      if not Result then
        HandleError('The updater file seems to be invalid.');

      LastUpdate := Now;
      HasNews := (NewsRec.NewsID > LocalNewsID) or ForceNewsFound;
      LocalNewsID := NewsRec.NewsID;

      if HasNews and Assigned(FHasNews) then
        FHasNews(Self)
      else if not HasNews and Assigned(FNoNews) then
        FNoNews(Self);
    end
    else
      HandleError('Cannot check for news & updates!');
  except
    on E: Exception do
      HandleError(E.Message);
  end;

  if Assigned(FUpdaterEnd) then
    FUpdaterEnd(Self);
end;

procedure TUpdater.HandleError(const ErrorMsg: string);
begin
  ConnectionError := True;

  if Assigned(FConnectError) then
    FConnectError(Self);

  if ShowConnectFail then
    MesajAsync(ErrorMsg);
end;

function TUpdater.IsTimeToCheckAgain: Boolean;
begin
  Result := ForceNewsFound or
    ((CheckEvery > 0) and (System.DateUtils.HoursBetween(Now, LastUpdate) >= CheckEvery));

  if not Result and TooLongNoSee then
    Result := True;
end;

function TUpdater.TooLongNoSee: Boolean;
begin
  Result := System.DateUtils.DaysBetween(Now, LastUpdate) >= TooLongNoSeeInterval;
end;

function TUpdater.NewVersionFound: Boolean;
begin
  Result := (NewsRec.AppVersion <> '') and (NewsRec.AppVersion > AppData.GetVersionInfo);
end;

procedure TUpdater.Save;
begin
  SaveTo(AppData.IniFile);
end;

procedure TUpdater.Load;
begin
  LoadFrom(AppData.IniFile);
end;

procedure TUpdater.SaveTo(const FileName: string);
var
  IniFile: TIniFileEx;
begin
  IniFile := TIniFileEx.Create('Updater', FileName);
  try
    IniFile.WriteDateEx('LastUpdate_', LastUpdate);
    IniFile.Write('LocalCounter', LocalNewsID);
    IniFile.Write('When', Ord(When));
    IniFile.Write('CheckEvery', CheckEvery);
    IniFile.Write('ForceNewsFound', ForceNewsFound);
    IniFile.Write('ShowConnectFail', ShowConnectFail);
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure TUpdater.LoadFrom(const FileName: string);
var
  IniFile: TIniFileEx;
begin
  IniFile := TIniFileEx.Create('Updater', FileName);
  try
    LastUpdate := Now;
    LastUpdate := IniFile.ReadDateEx('LastUpdate_', 0);
    LocalNewsID := IniFile.Read('LocalCounter', 0);
    When := TCheckWhen(IniFile.Read('When', Ord(cwHours)));
    CheckEvery := IniFile.Read('CheckEvery', 12);
    ForceNewsFound := IniFile.Read('ForceNewsFound', False);
    ShowConnectFail := IniFile.Read('ShowConnectFail', True);
  finally
    FreeAndNil(IniFile);
  end;
end;

END.
