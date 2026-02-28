UNIT ciUpdater;

{=============================================================================================================
   2026.02
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Automatic program Updater & News announcer
   This updater checks if a new version (or news) is available online.

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
  System.SysUtils, System.DateUtils, System.Classes, Vcl.ExtCtrls,
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
    CheckEvery     : Integer;                 { In hours. How often to check for news. Set it to zero to check every time the program starts. }
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

VAR
   Updater: TUpdater; { Only one instance per app! }

function CompareVersions(const V1, V2: string): Integer;

IMPLEMENTATION

USES
  LightCore, LightCore.TextFile, LightCore.IO, LightCore.Download, LightCore.INIFile, LightVcl.Common.Debugger, LightCore.AppData;

Const
  TooLongNoSeeInterval = 180;    { Force to check for updates every 180 days even if the updater is disabled }


{--------------------------------------------------------------------------------------------------
   CREATE
--------------------------------------------------------------------------------------------------}
constructor TUpdater.Create(CONST aURLNewsFile: string);
begin
  Assert(Updater = NIL, 'Updater already created!');
  inherited Create;
  URLNewsFile:= aURLNewsFile;
  URLDownload  := AppDataCore.ProductHome;
  URLRelHistory:= AppDataCore.ProductHome;

  Timer:= TTimer.Create(NIL);
  Timer.Enabled:= FALSE;
  Timer.OnTimer:= TimerTimer;

  Clear;    { Default settings }

  if AppDataCore.RunningFirstTime
  then Delay      := 300          { Don't bother the user on first startup. Probably he has the latest version anyway. }
  else Delay      := 30;

  URLDownload    := '';
  URLRelHistory  := '';

  { Load user settings }
  if FileExists(AppDataCore.IniFile)
  then Load;
end;


{ Default parameters }
procedure TUpdater.Clear;
begin
  NewsRec.Clear;

  When        := cwHours;
  HasNews     := FALSE;
  LocalNewsID := 0;
  LastUpdate  := 0;               { We signal with -1 that we don't know yet the value. We need to read it from disk, in this case (only once) }

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
    on E: Exception DO LightVcl.Common.Debugger.OutputDebugStr(E.Message);
  END;

  inherited Destroy;
end;





{--------------------------------------------------------------------------------------------------
   GET NEWS
--------------------------------------------------------------------------------------------------}

{ Main function.
  Set When = cwToday then call CheckForNews at program startup. }
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
  Returns TRUE if we need to show the form (in case of error or news) and FALSE if no errors AND no news. }
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

     if Assigned(FConnectError)
     then FConnectError(Self, DetailMsg);

     if ShowConnectFail
     then AppDataCore.LogError(DetailMsg);

     EXIT;
    end;

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

   // if ShowConnectFail then TFrmUpdater.ShowUpdater;  // del MessageError('Cannot check for news & updates!'{+ CRLF+ ErrorMsg});
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
function TUpdater.NewVersionFound(CONST AppVersion: string): boolean;  // Obtain AppVersion via TAppData.GetVersionInfo
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
   IniFile.WriteDate  ('LastUpdate__',    LastUpdate);
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
   LastUpdate      := IniFile.ReadDate('LastUpdate__', Now);
   LocalNewsID     := IniFile.Read('LocalCounter', 0);

   { User settings }
   When            := TCheckWhen(IniFile.Read('When', Ord(cwHours)));
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



end.
