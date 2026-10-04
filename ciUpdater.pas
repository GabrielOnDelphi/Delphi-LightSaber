UNIT ciUpdater;

{=============================================================================================================
   2026.10.03
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Automatic program Updater & News announcer
   This updater checks if a new version (or news) is available online.

   Platform: Windows, macOS, Linux, iOS, Android (framework-neutral; works in both VCL and FMX projects).

   Features:
      You can target (show the news) only a group of customers or all (Paying customers/Trial users/Demo users/All).
      The library can check for news at start up, after a predefined seconds delay.
      This is useful because the program might freeze for some miliseconds
      (depending on how bussy is the server) while downloading the News file from the Internet.
      No personal user data is sent to the server.

--------------------------------------------------------------------------------------------------------------

   The Updater
      Checks the website every x hours to see if updates of the product (app) are available.

   The Announcer
      The online files keep information not only about the updates but also it keeps news (like "Discount available if you purchase by the end of the day").
      The program can retrieve and display the news to the user (only once).

--------------------------------------------------------------------------------------------------------------

   The online file
      The data is kept in an INI file. A graphic editor is available for this.
      See the RNews record.

   Example of usage:

      Updater:= TUpdater.Create(URL1);
      Updater.URLDownload:= URL2;
      Updater.CheckForNews;

      Projects\Testers\LightUpdater\Tester_Updater.dpr
==============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.DateUtils, System.Classes,
  {$IFDEF FRAMEWORK_FMX} FMX.Types {$ELSE} Vcl.ExtCtrls {$ENDIF},
  ciUpdaterRec, LightCore.Types;

TYPE
  TCheckWhen = (cwNever,
                cwStartUp,         // Force to check for news now (the value of Delay is taken into consideration)
                cwHours);          // Check for news but ONLY if the specified number of hours has passed

  TUpdater = class(TObject)
  private
    Timer          : TTimer;
    LocalNewsID    : Integer;                 { The online counter is saved to disk after we successfully read it from the online file }
    FUpdaterStart  : TNotifyEvent;
    FUpdaterEnd    : TNotifyEvent;
    FConnectError  : TNotifyMsgEvent;
    FHasNews       : TNotifyEvent;
    FNoNews        : TNotifyEvent;
    procedure GetNewsDelay;
    procedure TimerTimer(Sender: TObject);
    procedure Clear;
    function  TooLongNoSee: Boolean;
  protected
    URLNewsFile    : string;                  { The URL from where we download the news file containing the RNews record. Mandatory }
  public
    { Input parameters }
    Delay          : Integer;                 { In seconds. Set it to zero to get the news right away. }
    When           : TCheckWhen;
    CheckEvery     : Integer;                 { In hours. How often to check for news when When = cwHours. Zero or less: never by hours; only the 180-day check (TooLongNoSee) runs. To check at every start, set When = cwStartUp. }
    ShowConnectFail: Boolean;                 { If true, show error messages when the program fails to connect to the internet. }
    ForceNewsFound : Boolean;                 { For DEBUGGING. If true, the object will always say that it has found news }
    { URLs }
    URLDownload    : string;                  { URL from where the user can download the new update. Not mandatory }
    URLRelHistory  : string;                  { URL where the user can see the Release History. Not mandatory }
    { Outputs }
    NewsRec        : RNews;                   { Temporary record }
    HasNews        : Boolean;                 { Returns true if news were found }
    LastUpdate     : TDateTime;               { We signal with -1 that we don't know yet the value. We need to read it from disk, in this case (only once) }
    ConnectionError: Boolean;

    constructor Create(CONST aURLNewsFile: string);
    destructor Destroy; override;

    { One-shot init + first check for the global Updater singleton.
      Assigns the same URL to both URLDownload and URLRelHistory; callers that need them
      split should construct TUpdater manually. Defaults: Delay=10s, CheckEvery=24h,
      ShowConnectFail=FALSE, When=cwHours. }
    class procedure InitAndCheck(const aNewsURL, aProductURL: string; aOnConnectError: TNotifyMsgEvent); static;

    function  NewVersionFound(CONST AppVersion: string): Boolean;

    function  IsTimeToCheckAgain: Boolean;
    procedure CheckForNews;
    function  GetNews: Boolean;
    procedure LoadFrom(CONST FileName: string);
    procedure SaveTo  (CONST FileName: string);

    procedure Load;
    procedure Save;

    { Events }
    property  OnUpdateStart : TNotifyEvent    read FUpdaterStart  write FUpdaterStart;
    property  OnHasNews     : TNotifyEvent    read FHasNews       write FHasNews;
    property  OnNoNews      : TNotifyEvent    read FNoNews        write FNoNews;
    property  OnConnectError: TNotifyMsgEvent read FConnectError  write FConnectError;
    property  OnUpdateEnd   : TNotifyEvent    read FUpdaterEnd    write FUpdaterEnd;
  end;

  { The five event handlers of a TUpdater. A form that sets its own handlers reads the ones of the host program
    first and writes them back when it closes, so the host keeps its handlers. }
  RUpdaterEvents = record
    OnUpdateStart : TNotifyEvent;
    OnHasNews     : TNotifyEvent;
    OnNoNews      : TNotifyEvent;
    OnConnectError: TNotifyMsgEvent;
    OnUpdateEnd   : TNotifyEvent;
    procedure ReadFrom(Source: TUpdater);
    procedure WriteTo (Target: TUpdater);
  end;

VAR
   Updater: TUpdater; { Only one instance per app! }

function CompareVersions(const V1, V2: string): Integer;
function GetFileVersionFull(CONST FileName: string): string;

IMPLEMENTATION

USES
  {$IFDEF MSWINDOWS} LightCore.ExeVersion, {$ENDIF}
  LightCore, LightCore.TextFile, LightCore.IO, LightCore.Download, LightCore.INIFile, LightCore.AppData;

Const
  TooLongNoSeeInterval = 180;    { Force to check for updates every 180 days even if the updater is disabled }
  DefaultWhen          = cwHours;


{--------------------------------------------------------------------------------------------------
   CREATE
--------------------------------------------------------------------------------------------------}
constructor TUpdater.Create(CONST aURLNewsFile: string);
begin
  Assert(Updater = NIL, 'Updater already created!');
  inherited Create;
  URLNewsFile:= aURLNewsFile;

  Timer:= TTimer.Create(NIL);
  Timer.Enabled:= FALSE;
  Timer.OnTimer:= TimerTimer;

  Clear;    { Default settings }

  { Don't bother the user on first startup. Probably he has the latest version anyway. }
  if AppDataCore.RunningFirstTime
  then Delay:= 300
  else Delay:= 30;

  { URLDownload and URLRelHistory default to '' — caller must set them after Create (see comment in type declaration). }

  { Load user settings }
  if FileExists(AppDataCore.IniFile)
  then Load;
end;


class procedure TUpdater.InitAndCheck(const aNewsURL, aProductURL: string; aOnConnectError: TNotifyMsgEvent);
begin
  Updater:= TUpdater.Create(aNewsURL);
  Updater.URLDownload    := aProductURL;
  Updater.URLRelHistory  := aProductURL;
  Updater.Delay          := 10;
  Updater.When           := cwHours;
  Updater.CheckEvery     := 24;
  Updater.ShowConnectFail:= FALSE;
  Updater.OnConnectError := aOnConnectError;
  Updater.CheckForNews;
end;


{ Default parameters }
procedure TUpdater.Clear;
begin
  NewsRec.Clear;

  When            := DefaultWhen;
  HasNews         := FALSE;
  ConnectionError := FALSE;
  LocalNewsID     := 0;
  LastUpdate      := 0;               { We signal with -1 that we don't know yet the value. We need to read it from disk, in this case (only once) }

  { Parameters }
  CheckEvery      := 12;          { Hours. Default interval for checking updates. }
  ForceNewsFound  := FALSE;
  ShowConnectFail := TRUE;        { If true, show error messages when the program fails to connect to the internet. }
end;


destructor TUpdater.Destroy;
begin
  FreeAndNil(Timer);

  TRY
    Save;
  EXCEPT
    on E: Exception DO AppDataCore.LogError('Updater.Save failed: ' + E.Message);
  END;

  inherited Destroy;
end;





{--------------------------------------------------------------------------------------------------
   GET NEWS
--------------------------------------------------------------------------------------------------}

{ Main function.
  Set When = cwStartUp then call CheckForNews at program startup. }
procedure TUpdater.CheckForNews;
begin
  case When of
    cwNever  : if TooLongNoSee then GetNewsDelay;  { Still check if we haven't done it in 6 months }
    cwStartUp: GetNewsDelay;
    cwHours  : if IsTimeToCheckAgain               { This will check for news ONLY if the specified number of hours has passed }
               then GetNewsDelay;
    else
       Raise Exception.Create('Unknown type in TCheckWhen');
  end;
end;


{ Check for news few seconds later. We want to check for news some seconds after the program started so we don't freeze the program imediatelly after startup }
procedure TUpdater.GetNewsDelay;
begin
 if Delay = 0
 then GetNews
 else
  begin
   Timer.Interval:= Delay * 1000;
   Timer.Enabled:= TRUE;
  end;
end;


procedure TUpdater.TimerTimer(Sender: TObject);
begin
 Timer.Enabled:= FALSE;    { Disable automatic checking if we already checked once manually }
 GetNews;
end;


{ Where we store the News file locally }
function UpdaterFileLocation: string;
begin
 Result:= AppDataCore.AppDataFolder+ 'Online_v4.News';
end;


{ Download data from website right now.
  Returns TRUE when the news file was downloaded and read, FALSE on any error (no connection, HTTP error, an HTML page, a wrong format).
  Whether there is news is in HasNews. OnUpdateEnd fires at the end of every call, after an error too. }
function TUpdater.GetNews: Boolean;
VAR ErrorMsg: string;
begin
 Timer.Enabled:= FALSE;
 HasNews:= FALSE;
 Assert(URLNewsFile <> '', 'Updater URLNewsFile is empty!');

 if Assigned(FUpdaterStart)
 then FUpdaterStart(Self);

 { Download the news file }
 LightCore.Download.DownloadToFile(URLNewsFile, UpdaterFileLocation, ErrorMsg);    { Returns false if the Internet connection failed. If the URL is invalid, probably it will return the content of the 404 page (if the server automatically returns a 404 page). }
 Result:= ErrorMsg = '';

 { Parse the news file }
 if Result
 then
  begin
    Result:= NewsRec.LoadFrom(UpdaterFileLocation);

   if NOT Result then
    begin
     { Detect if the server returned an HTML page instead of the news file }
     VAR FileContent:= StringFromFile(UpdaterFileLocation);
     VAR FileSize:= LightCore.IO.GetFileSize(UpdaterFileLocation);
     VAR DetailMsg: string;

     if (FileSize < 25*KB)
     AND ( (  (PosInsensitive('<html', FileContent) > 0)
          AND (PosInsensitive('<body', FileContent) > 0))
         OR (PosInsensitive('<!doctype ', FileContent) > 0)
         OR (PosInsensitive('<meta name', FileContent) > 0))
     then DetailMsg:= 'Server returned an HTML page instead of the news file. The file URL may be invalid.'
     else DetailMsg:= 'The news file has an invalid format (version mismatch or corruption).';

     ConnectionError:= TRUE;  { Treat malformed/HTML responses as a connection-level failure for the UI }

     if Assigned(FConnectError)
     then FConnectError(Self, DetailMsg);

     if ShowConnectFail
     then AppDataCore.LogError(DetailMsg);

     if Assigned(FUpdaterEnd) then FUpdaterEnd(Self);   { Same as the other two exits: a listener that waits for the end of the check must get it }
     EXIT;
    end;

   { Success: clear any prior error state }
   ConnectionError:= FALSE;
   LastUpdate:= Now;  { Last SUCCESFUL update= now }

   { Compare local news with the online news }
   HasNews := (NewsRec.NewsID > LocalNewsID) OR ForceNewsFound;  { ForceNewsFound is for debugging }
   LocalNewsID:= NewsRec.NewsID;

   if HasNews AND Assigned(FHasNews)
   then FHasNews(Self);

   if NOT HasNews AND Assigned(FNoNews)
   then FNoNews(Self)
  end
 else
  begin
   ConnectionError:= TRUE;

   if Assigned(FConnectError)
   then FConnectError(Self, ErrorMsg);
  end;

 if Assigned(FUpdaterEnd) then FUpdaterEnd(Self);
end;






{--------------------------------------------------------------------------------------------------
   UTIL
--------------------------------------------------------------------------------------------------}

{ Returns true interval passed since the last check if higher than CheckEvery
  Still check for updates every 180 days, EVEN if the updater is disabled. }
function TUpdater.IsTimeToCheckAgain: Boolean;
begin
 Result:= ForceNewsFound
       OR (CheckEvery > 0) AND (System.DateUtils.HoursBetween(Now, LastUpdate) >= CheckEvery);

 if NOT Result
 AND TooLongNoSee
 then Result:= TRUE;
end;


{ Returns true if we haven't checked for updates in the last 180 days }
function TUpdater.TooLongNoSee: Boolean;
begin
  Result:= System.DateUtils.DaysBetween(Now, LastUpdate) >= TooLongNoSeeInterval;
end;


{ Returns true when the online version is higher than the local version }
function TUpdater.NewVersionFound(CONST AppVersion: string): boolean;  // Obtain AppVersion via GetFileVersionFull(ParamStr(0)): all four numbers
begin
  Result:= (NewsRec.AppVersion <> '?') AND (CompareVersions(NewsRec.AppVersion, AppVersion) > 0);
end;










{ Load/save object settings }
procedure TUpdater.Save;
begin
  SaveTo(AppDataCore.IniFile);
end;


procedure TUpdater.Load;
begin
  LoadFrom(AppDataCore.IniFile);
end;



procedure TUpdater.SaveTo(CONST FileName: string);
begin
 VAR IniFile:= TIniFileEx.Create('Updater', FileName);
 try
   { Internal state }
   IniFile.WriteDateTime('Updater', 'LastUpdate__', LastUpdate);   { WriteDateTime, not WriteDate: WriteDate writes DateToStr and loses the time of day (System.IniFiles.pas, TCustomIniFile.WriteDate). After a restart LastUpdate was then the midnight before the check, so the next check came up to 24 hours early. }
   IniFile.Write      ('LocalCounter',    LocalNewsID);

   { User settings }
   IniFile.Write      ('When',            Ord(When));
   IniFile.Write      ('CheckEvery',      CheckEvery);
   IniFile.Write      ('ForceNewsFound',  ForceNewsFound);
   IniFile.Write      ('ShowConnectFail', ShowConnectFail);
 finally
   FreeAndNil(IniFile);
 end;
end;


procedure TUpdater.LoadFrom(CONST FileName: string);
begin
 VAR IniFile := TIniFileEx.Create('Updater', FileName);
 try
   { Internal state}
   { Default 0 (epoch) — NOT Now — so that an INI missing this key triggers TooLongNoSee on the next IsTimeToCheckAgain. }
   LastUpdate      := IniFile.ReadDateTime('Updater', 'LastUpdate__', 0);   { Also reads the date-only values that WriteDate wrote before 2026.10.03 }
   LocalNewsID     := IniFile.Read('LocalCounter', 0);

   { User settings }
   { A value outside TCheckWhen (a corrupted INI file) would make CheckForNews raise 'Unknown type in TCheckWhen' }
   VAR WhenOrd: Integer:= IniFile.Read('When', Ord(DefaultWhen));
   if (WhenOrd >= Ord(Low(TCheckWhen))) AND (WhenOrd <= Ord(High(TCheckWhen)))
   then When:= TCheckWhen(WhenOrd)
   else
     begin
       AppDataCore.LogWarn('Updater: the value ' + IntToStr(WhenOrd) + ' of the key When in ' + FileName + ' is not a valid TCheckWhen. Using the default.');
       When:= DefaultWhen;
     end;

   CheckEvery      := IniFile.Read('CheckEvery', 12);
   ForceNewsFound  := IniFile.Read('ForceNewsFound',  FALSE);
   ShowConnectFail := IniFile.Read('ShowConnectFail', TRUE);
 finally
   FreeAndNil(IniFile);
 end;
end;



{ Compares two version strings segment by segment (e.g. "9.55.0.0" vs "9.9.0.0").
  Returns: >0 if V1>V2, 0 if equal, <0 if V1<V2.
  Missing segments are treated as 0. }
function CompareVersions(const V1, V2: string): Integer;
VAR
  Parts1, Parts2: TArray<string>;
  i, N1, N2: Integer;
begin
  Parts1:= V1.Split(['.']);
  Parts2:= V2.Split(['.']);

  VAR MaxLen:= Length(Parts1);
  if Length(Parts2) > MaxLen
  then MaxLen:= Length(Parts2);

  for i:= 0 to MaxLen-1 do
   begin
     N1:= 0;
     N2:= 0;
     if i < Length(Parts1)
     then TryStrToInt(Parts1[i], N1);
     if i < Length(Parts2)
     then TryStrToInt(Parts2[i], N2);

     if N1 <> N2
     then EXIT(N1 - N2);
   end;

  Result:= 0;
end;



{ The version of a program file with all four numbers (9.55.1.2): the form to give to NewVersionFound for the running program, GetFileVersionFull(ParamStr(0)).
  A shorter version compares as older, because CompareVersions counts a missing number as 0: a running 9.55.1.2 read as 9.55.1 made NewVersionFound say TRUE for the version the user already runs.
  Returns '' when the file has no version resource, and always off Windows, where LightCore.ExeVersion does not exist. The caller then uses the version that its framework reports.
  Raises on Windows when FileName is empty (GetVersionInfoFile). }
function GetFileVersionFull(CONST FileName: string): string;
{$IFDEF MSWINDOWS}
VAR Version: TFileVersion;
{$ENDIF}
begin
  Result:= '';
  {$IFDEF MSWINDOWS}
  if GetVersionInfoFile(FileName, Version)
  then Result:= IntToStr(Version.Major) + '.' + IntToStr(Version.Minor) + '.' + IntToStr(Version.Release) + '.' + IntToStr(Version.Build);
  {$ENDIF}
end;



{ RUpdaterEvents }

procedure RUpdaterEvents.ReadFrom(Source: TUpdater);
begin
  Assert(Source <> NIL, 'RUpdaterEvents.ReadFrom: no Updater');
  OnUpdateStart := Source.OnUpdateStart;
  OnHasNews     := Source.OnHasNews;
  OnNoNews      := Source.OnNoNews;
  OnConnectError:= Source.OnConnectError;
  OnUpdateEnd   := Source.OnUpdateEnd;
end;


procedure RUpdaterEvents.WriteTo(Target: TUpdater);
begin
  Assert(Target <> NIL, 'RUpdaterEvents.WriteTo: no Updater');
  Target.OnUpdateStart := OnUpdateStart;
  Target.OnHasNews     := OnHasNews;
  Target.OnNoNews      := OnNoNews;
  Target.OnConnectError:= OnConnectError;
  Target.OnUpdateEnd   := OnUpdateEnd;
end;



end.
