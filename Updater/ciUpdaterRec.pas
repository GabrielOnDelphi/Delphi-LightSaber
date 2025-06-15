UNIT ciUpdaterRec;

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2023.06
   www.GabrielMoraru.com
   See Copyright file

   Online data holder for TUpdater.
   Holds: Version info, News Text
--------------------------------------------------------------------------------------------------}

//See: https://stackoverflow.com/questions/64685038/how-to-traverse-the-enums-in-any-given-set

INTERFACE

USES
  System.SysUtils, System.Classes, Vcl.StdCtrls;

TYPE
  TTargetUser = (tuAll, tuRegistered, tuTrial, tuDemo); { For which user category is this annoucement } { Discern between paid/trial/demo license so it can display messages based on the license state (for example, only show PURCHASE NOW to the Demo/expired_trial users)}

  RNews = record
  private
    const
      Signature = 'LightUpdater';
      CurrentVersion = 2;
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

procedure PopulateUsers(Combo: TComboBox);


IMPLEMENTATION

USES
  LightCore.StreamBuff2;





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
   Stream: TCubicBuffStream2;
begin
 Clear;
 Stream:= TCubicBuffStream2.CreateRead(FileName);
 TRY
   Result:= Stream.ReadHeader(Signature, CurrentVersion);
   if Result then
    begin
      Comment     := Stream.ReadString;
      AppVersion  := Stream.ReadString;
      NewsID      := Stream.ReadInteger;
      NewsHeadline:= Stream.ReadString;
      NewsBody    := Stream.ReadString;
      TargetUser  := TTargetUser(Stream.ReadByte);
      CriticalUpd := Stream.ReadBoolean;
      ShowCounter := Stream.ReadInteger;
      IsBetaVers  := Stream.ReadBoolean;

      Stream.ReadPaddingDef;
    end;

 FINALLY
   FreeAndNil(Stream);
 END;
end;


procedure RNews.SaveTo(FileName: string);
VAR
   Stream: TCubicBuffStream2;
begin
 Stream:= TCubicBuffStream2.CreateWrite(FileName);
 TRY
   Stream.WriteHeader(Signature, CurrentVersion);
   Stream.WriteString (Comment);
   Stream.WriteString (AppVersion);
   Stream.WriteInteger (NewsID);
   Stream.WriteString (NewsHeadline);
   Stream.WriteString (NewsBody);
   Stream.WriteByte(Ord(TargetUser));
   Stream.WriteBoolean (CriticalUpd);
   Stream.WriteInteger (ShowCounter);
   Stream.WriteBoolean (IsBetaVers);

   Stream.WritePaddingDef;
 FINALLY
   FreeAndNil(Stream);
 END;
end;



procedure PopulateUsers(Combo: TComboBox);
begin
 Combo.Items.Clear;
 Combo.Items.Add('All users');
 Combo.Items.Add('Registered users');
 Combo.Items.Add('Trial users');
 Combo.Items.Add('Demo users');
 Combo.ItemIndex:= 0;
end;


end.


