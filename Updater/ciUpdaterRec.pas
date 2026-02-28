UNIT ciUpdaterRec;

{-------------------------------------------------------------------------------------------------------------
   2026.02
   www.GabrielMoraru.com

   Online data holder for TUpdater.
   Holds: Version info, News Text

   File format: INI (human-editable). Pipe character (|) encodes line breaks in NewsBody.
-------------------------------------------------------------------------------------------------------------}

//See: https://stackoverflow.com/questions/64685038/how-to-traverse-the-enums-in-any-given-set

INTERFACE

USES
  System.SysUtils, System.Classes;

TYPE
  TTargetUser = (tuAll, tuRegistered, tuTrial, tuDemo); { For which user category is this annoucement } { Discern between paid/trial/demo license so it can display messages based on the license state (for example, only show PURCHASE NOW to the Demo/expired_trial users)}

  RNews = record
  private
    const
      Signature = 'LightUpdater';
      CurrentVersion = 4;
  public
    Comment     : string;
    { News }
    NewsHeadline: string;
    NewsBody    : string;
    NewsID      : Integer;      { Increment it each time we publish new news. This is how the program detects that a new announcement was published online }
    TargetUser  : TTargetUser;
    ShowCounter : Integer;      { How many times to show this to the user. Usually only one time. }
    { Program updates }
    AppVersion  : string;       { Version of the online Setup.exe file. Format: 9.55.0.0. If the counter in OnlineNews is higher than the counter in News then it means that the news file was Updater. }
    CriticalUpd : Boolean;      { If true, we need to force the user to upgrade }
    IsBetaVers  : Boolean;      { The online version is a beta version }
    function  LoadFrom(FileName: string): Boolean;
    procedure SaveTo  (FileName: string);
    procedure Clear;
  end;


IMPLEMENTATION

USES
  LightCore.INIFile;


{ Encode CRLF as pipe for INI storage }
function EncodePipe(const S: string): string;
begin
  Result:= StringReplace(S, #13#10, '|', [rfReplaceAll]);
end;

{ Decode pipe back to CRLF }
function DecodePipe(const S: string): string;
begin
  Result:= StringReplace(S, '|', #13#10, [rfReplaceAll]);
end;



procedure RNews.Clear;
begin
 Comment     := '';
 AppVersion  := '?'; // This is how we know that this is uninitialized. Don't change it.
 NewsID      := 0;
 NewsHeadline:= '';
 NewsBody    := '';
 TargetUser  := tuAll;
 CriticalUpd := FALSE;
 ShowCounter := 1;
end;


function RNews.LoadFrom(FileName: string): Boolean;
VAR
   IniHeader: TIniFileEx;
   IniNews: TIniFileEx;
   Sig: string;
   Ver: Integer;
begin
 Clear;

 IniHeader:= TIniFileEx.Create('Header', FileName);
 TRY
   Sig:= IniHeader.Read('Signature', '');
   Ver:= IniHeader.Read('Version', 0);
 FINALLY
   FreeAndNil(IniHeader);
 END;

 if (Sig <> Signature) OR (Ver <> CurrentVersion)
 then EXIT(FALSE);

 IniNews:= TIniFileEx.Create('News', FileName);
 TRY
   Comment     := IniNews.Read('Comment', '');
   AppVersion  := IniNews.Read('AppVersion', '?');
   NewsID      := IniNews.Read('NewsID', 0);
   NewsHeadline:= IniNews.Read('NewsHeadline', '');
   NewsBody    := DecodePipe(IniNews.Read('NewsBody', ''));
   TargetUser  := TTargetUser(IniNews.Read('TargetUser', 0));
   CriticalUpd := IniNews.Read('CriticalUpd', FALSE);
   ShowCounter := IniNews.Read('ShowCounter', 1);
   IsBetaVers  := IniNews.Read('IsBetaVers', FALSE);
 FINALLY
   FreeAndNil(IniNews);
 END;

 Result:= TRUE;
end;


procedure RNews.SaveTo(FileName: string);
VAR
   IniHeader: TIniFileEx;
   IniNews: TIniFileEx;
begin
 IniHeader:= TIniFileEx.Create('Header', FileName);
 TRY
   IniHeader.Write('Signature', Signature);
   IniHeader.Write('Version', CurrentVersion);
 FINALLY
   FreeAndNil(IniHeader);
 END;

 IniNews:= TIniFileEx.Create('News', FileName);
 TRY
   IniNews.Write('Comment',      Comment);
   IniNews.Write('AppVersion',   AppVersion);
   IniNews.Write('NewsID',       NewsID);
   IniNews.Write('NewsHeadline', NewsHeadline);
   IniNews.Write('NewsBody',     EncodePipe(NewsBody));
   IniNews.Write('TargetUser',   Ord(TargetUser));
   IniNews.Write('CriticalUpd',  CriticalUpd);
   IniNews.Write('ShowCounter',  ShowCounter);
   IniNews.Write('IsBetaVers',   IsBetaVers);
 FINALLY
   FreeAndNil(IniNews);
 END;
end;


end.
