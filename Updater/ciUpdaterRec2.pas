INTERFACE

USES
  System.SysUtils, System.Classes, Vcl.StdCtrls;

TYPE
  TTargetUser = (tuAll, tuRegistered, tuTrial, tuDemo); 

  RNews = record
  private
    const
      MagicNumber = 'CubicUpdater';
      CurrentVersion = 1;
    procedure ValidateEnumValue(AValue: Integer);
  public
    Comment     : string;
    NewsHeadline: string;
    NewsBody    : string;
    NewsID      : Integer;
    TargetUser  : TTargetUser;
    ShowCounter : Integer;
    AppVersion  : string; 
    CriticalUpd : Boolean;
    IsBetaVers  : Boolean;

    function LoadFrom(FileName: string): Boolean;
    procedure SaveTo(FileName: string);
    procedure Clear;
  end;

procedure PopulateUsers(Combo: TComboBox);

IMPLEMENTATION

USES
  ccStreamBuff;

{ RNews }

procedure RNews.Clear;
begin
  Comment     := '';
  AppVersion  := ''; // Now initialized as an empty string
  NewsID      := 0;
  NewsHeadline:= '';
  NewsBody    := '';
  TargetUser  := tuAll;
  CriticalUpd := False;
  ShowCounter := 1;
  IsBetaVers  := False;
end;

procedure RNews.ValidateEnumValue(AValue: Integer);
begin
  if (AValue < Ord(Low(TTargetUser))) or (AValue > Ord(High(TTargetUser))) then
    raise Exception.CreateFmt('Invalid TargetUser value: %d', [AValue]);
end;

function RNews.LoadFrom(FileName: string): Boolean;
var
  IOStream: TCubicBuffStream;
begin
  Clear;
  IOStream := TCubicBuffStream.Create(FileName, fmOpenRead);
  try
    IOStream.Position := 0;
    IOStream.MagicNo := MagicNumber;
    Result := IOStream.ReadMagicVer = CurrentVersion; 

    if Result then
    begin
      Comment     := IOStream.ReadStringU;
      AppVersion  := IOStream.ReadStringU;
      NewsID      := IOStream.ReadInteger;
      NewsHeadline:= IOStream.ReadStringU;
      NewsBody    := IOStream.ReadStringU;

      // Validate and assign enum value
      ValidateEnumValue(IOStream.ReadByte);
      TargetUser := TTargetUser(IOStream.ReadByte);

      CriticalUpd := IOStream.ReadBoolean;
      ShowCounter := IOStream.ReadInteger;
      IsBetaVers  := IOStream.ReadBoolean;

      IOStream.ReadPadding(128);
    end
    else
    begin
      raise Exception.Create('Magic number or version mismatch in the file.');
    end;
  finally
    FreeAndNil(IOStream);
  end;
end;

procedure RNews.SaveTo(FileName: string);
var
  IOStream: TCubicBuffStream;
begin
  IOStream := TCubicBuffStream.Create(FileName, fmOpenWrite or fmCreate);
  try
    IOStream.MagicNo := MagicNumber;
    IOStream.WriteMagicVer(CurrentVersion); 
    IOStream.WriteStringU(Comment);
    IOStream.WriteStringU(AppVersion);
    IOStream.WriteInteger(NewsID);
    IOStream.WriteStringU(NewsHeadline);
    IOStream.WriteStringU(NewsBody);
    IOStream.WriteByte(Ord(TargetUser));
    IOStream.WriteBoolean(CriticalUpd);
    IOStream.WriteInteger(ShowCounter);
    IOStream.WriteBoolean(IsBetaVers);

    IOStream.WritePadding(128);
  finally
    FreeAndNil(IOStream);
  end;
end;

procedure PopulateUsers(Combo: TComboBox);
begin
  Combo.Items.Clear;
  Combo.Items.Add('All users');
  Combo.Items.Add('Registered users');
  Combo.Items.Add('Trial users');
  Combo.Items.Add('Demo users');
  Combo.ItemIndex := 0;
end;

END.
