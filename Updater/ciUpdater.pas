UNIT ciUpdater;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
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
      The data is kept in a binary file. A graphic editor is available for this. The file is extensible (it can be easily expanded to accept new features).
      See the RNews recrod.

   Example of usage:

      Updater:= TUpdater.Create(URL1);
      Updater.URLDownload:= URL2;
      Updater.CheckForNews;

      LightSaber\Demo\LightUpdater\Tester_Updater.dpr
==============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.DateUtils, System.Classes, Vcl.ExtCtrls,
  ciUpdaterRec, LightCore;

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
    URLNewsFile    : string;                  { The URL from where we read the bin file containing the RNews record. Mandatory }
  public
    { Input parameters }
    Delay          : Integer;                 { In seconds. Set it to zero to get the news right away. }
    When           : TCheckWhen;
    CheckEvery     : Integer;                 { In hours. How often to check for news. Set it to zero to check every time the program starts. }
    ShowConnectFail: Boolean;                 { If true, the user doesn't want to see an error msg in case the program fails to connect to the internet. }
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

    function  NewVersionFound: Boolean;

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

IMPLEMENTATION

USES
  FormAsyncMessage, LightCore.Download, LightCore.INIFile, LightVcl.Common.Debugger, LightCore.AppData, LightVcl.Common.AppData;

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

  Timer:= TTimer.Create(NIL);
  Timer.Enabled:= FALSE;
  Timer.OnTimer:= TimerTimer;

  Clear;    { Default settings }

  if AppData.RunningFirstTime
  then Delay      := 300          { Don't bother the user on first startup. Probably he has the latest version anyway. }
  else Delay      := 30;

  URLDownload    := '';
  URLRelHistory  := '';

  { Load user settings }
  if FileExists(AppData.IniFile)
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
  ShowConnectFail := TRUE;        { If true, the user doesn't want to see an error msg in case the program fails to connect to the internet. }
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
 Result:= AppData.AppDataFolder+ 'Online_v2.News';
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

 { Download the Bin file }
 LightCore.Download.DownloadToFile(URLNewsFile, UpdaterFileLocation, ErrorMsg);    { Returns false if the Internet connection failed. If the URL is invalid, probably it will return the content of the 404 page (if the server automatically returns a 404 page). }
 Result:= ErrorMsg = '';

 { Parse the binary file }
 if Result
 then
  begin
   Result:= NewsRec.LoadFrom(UpdaterFileLocation);

   if NOT Result then
    begin
     if Assigned(FConnectError)
     then FConnectError(Self, 'Cannot load new record from disk!');

     if ShowConnectFail
     then MesajAsync('The updater file seems to be invalid!'); // This message also appears if the online does not exist and the server returns a 404 page

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
function TUpdater.NewVersionFound: boolean;
begin
  Result:= (NewsRec.AppVersion <> '?') AND (NewsRec.AppVersion > TAppData.GetVersionInfo);
end;










{ Load/save object settings }
procedure TUpdater.Save;
begin
  SaveTo(AppData.IniFile);
end;


procedure TUpdater.Load;
begin
  LoadFrom(AppData.IniFile);
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
   LastUpdate      := Now;
   LastUpdate      := IniFile.ReadDate('LastUpdate__', 0);
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


end.
