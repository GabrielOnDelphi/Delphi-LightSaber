UNIT ciUpdaterRec;

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2023.06
   See Copyright.txt

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
  ccStreamBuff;

CONST
  CurrentVersion = 1;




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
   IOStream: TCubicBuffStream;
begin
 Clear;
 IOStream:= TCubicBuffStream.Create(FileName, fmOpenRead);
 TRY
   IOStream.Position:= 0;
   IOStream.MagicNo:= 'CubicUpdater';
   Result:= IOStream.ReadMagicVer = CurrentVersion;      { Read MagicNo & version }

   if Result then
    begin
      Comment     := IOStream.ReadStringU;
      AppVersion  := IOStream.ReadStringU;
      NewsID      := IOStream.ReadInteger;
      NewsHeadline:= IOStream.ReadStringU;
      NewsBody    := IOStream.ReadStringU;
      TargetUser  := TTargetUser(IOStream.ReadByte);
      CriticalUpd := IOStream.ReadBoolean;
      ShowCounter := IOStream.ReadInteger;
      IsBetaVers  := IOStream.ReadBoolean;

      IOStream.ReadPadding(128);
    end;
 FINALLY
   FreeAndNil(IOStream);
 END;
end;


procedure RNews.SaveTo(FileName: string);
VAR
   IOStream: TCubicBuffStream;
begin
 IOStream:= TCubicBuffStream.Create(FileName, fmOpenWrite OR fmCreate);
 TRY
   IOStream.MagicNo:= 'CubicUpdater';
   IOStream.WriteMagicVer(CurrentVersion);      { MagicNo & version }
   IOStream.WriteStringU (Comment);
   IOStream.WriteStringU (AppVersion);
   IOStream.WriteInteger (NewsID);
   IOStream.WriteStringU (NewsHeadline);
   IOStream.WriteStringU (NewsBody);
   IOStream.WriteByte(Ord(TargetUser));
   IOStream.WriteBoolean (CriticalUpd);
   IOStream.WriteInteger (ShowCounter);
   IOStream.WriteBoolean (IsBetaVers);

   IOStream.WritePadding(128);
 FINALLY
   FreeAndNil(IOStream);
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


