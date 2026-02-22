{-------------------------------------------------------------------------------------------------------------
   TDVDrivesCombo & TDVDrivesList 1.1
   Purpose: List disk drives (much better than Borland's control)
-------------------------------------------------------------------------------------------------------------   
   Updated: 2026.02 / Delphi 13
   Gabriel Moraru
-------------------------------------------------------------------------------------------------------------
   Original author: Jakub Dusek (1999) drivi@seznam.cz  drivi.misto.cz/delphi (offline)
   Copyright: This source code is not public domain and can be distributed only with this header and included readme file
-------------------------------------------------------------------------------------------------------------}

UNIT DiskDrives;

INTERFACE

USES
  WinApi.Windows, WinApi.Messages, WinApi.ShellApi,
  System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ImgList;

TYPE
  TDriveType = (dtUnknown, dtNonExisting, dtRemovable, dtFixed, dtRemote, dtCDROM, dtRamDisk);
  TViewDrivesType = set of TDriveType;
  TIconSize = (isSmall, isLarge);
  TTextCase = (tcLowerCase, tcUpperCase);

TDiskDriveCombo = class(TCustomComboBox)
  private
    FAutoUpdate:Boolean;
    FDisplayBrackets:Boolean;
    FDisplayIcons:Boolean;
    FDisplayLabel:Boolean;
    FDisplaySysName:Boolean;
    FDrive:char;
    FIconsSize:TIconSize;
    FTextCase:TTextCase;
    FViewDrivesType:TViewDrivesType;
    procedure SetDisplayBrackets(Value:Boolean);
    procedure SetDisplayIcons(Value:Boolean);
    procedure SetDisplayLabel(Value:Boolean);
    procedure SetDisplaySysName(Value:Boolean);
    procedure SetDrive(Value:char);
    procedure SetCase(Value:TTextCase);
    procedure SetIconsSize(Value:TIconSize);
    procedure SetViewDrivesType(Value:TViewDrivesType);
    procedure CMFontChanged(var Message: TMessage);message CM_FONTCHANGED;
  protected
    procedure DropDown;override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);override;
    procedure Change;override;
  public
    constructor Create(AOwner:TComponent);override;
    procedure CreateWnd; override;
    procedure Loaded; override;
  published
    property AutoUpdate:Boolean read FAutoUpdate write FAutoUpdate default true;
    property DisplayBrackets:Boolean read FDisplayBrackets write SetDisplayBrackets default True;
    property DisplayIcons:Boolean read FDisplayIcons write SetDisplayIcons default True;
    property DisplayLabel:Boolean read FDisplayLabel write SetDisplaylabel default True;
    property DisplaySysName:Boolean read FDisplaySysName write SetDisplaySysName default True;
    property Drive:Char read FDrive write SetDrive;
    property IconsSize:TIconSize read FIconsSize write SetIconsSize;
    property TextCase:TTextCase read FTextCase write SetCase default tcLowerCase;
    property ViewDrivesType:TViewDrivesType read FViewDrivesType write SetViewDrivesType default [dtRemovable,dtFixed, dtRemote, dtCDROM, dtRamDisk];
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    property ImeMode;
    property ImeName;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
    procedure UpdateDrives;
  end;


TDiskDriveList = class(TCustomListBox)
  private
    FDisplayBrackets:Boolean;
    FDisplayIcons:Boolean;
    FDisplayLabel:Boolean;
    FDisplaySysName:Boolean;
    FDrive:char;
    FIconsSize:TIconSize;
    FTextCase:TTextCase;
    FViewDrivesType:TViewDrivesType;
    procedure SetDisplayBrackets(Value:Boolean);
    procedure SetDisplayIcons(Value:Boolean);
    procedure SetDisplayLabel(Value:Boolean);
    procedure SetDisplaySysName(Value:Boolean);
    procedure SetDrive(Value:char);
    procedure SetCase(Value:TTextCase);
    procedure SetIconsSize(Value:TIconSize);
    procedure SetViewDrivesType(Value:TViewDrivesType);
    procedure CMFontChanged(var Message: TMessage);message CM_FONTCHANGED;
  protected
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);override;
    procedure Click;override;
  public
    constructor Create(AOwner:TComponent);override;
    procedure CreateWnd;override;
    procedure Loaded;override;
  published
    property DisplayBrackets:Boolean read FDisplayBrackets write SetDisplayBrackets default True;
    property DisplayIcons:Boolean read FDisplayIcons write SetDisplayIcons default True;
    property DisplayLabel:Boolean read FDisplayLabel write SetDisplaylabel default True;
    property DisplaySysName:Boolean read FDisplaySysName write SetDisplaySysName default True;
    property Drive:Char read FDrive write SetDrive;
    property IconsSize:TIconSize read FIconsSize write SetIconsSize;
    property TextCase:TTextCase read FTextCase write SetCase default tcLowerCase;
    property ViewDrivesType:TViewDrivesType read FViewDrivesType write SetViewDrivesType default [dtRemovable,dtFixed, dtRemote, dtCDROM, dtRamDisk];
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    property ImeMode;
    property ImeName;
    property BorderStyle;
    property Color;
    property Columns;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property IntegralHeight;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
    procedure UpdateDrives;
  end;

procedure Register;


IMPLEMENTATION

VAR
  Larges,Smalls: TImageList;


{ Returns the required item height for owner-drawn controls based on font metrics }
function GetItemHeight(Font: TFont): Integer;
var
  DC: HDC;
  Fnt: HFont;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    Fnt := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, Fnt);
  finally
    ReleaseDC(0, DC);
  end;
  Result := Metrics.tmHeight + 1;
end;

{ Creates the Smalls and Larges image lists on first use,
  loading the system icon lists from the Windows shell. }
procedure EnsureImageListsCreated;
var
  SFI: TSHFileInfo;
  SysIL: UInt;
begin
  if Smalls <> nil then Exit;

  Smalls:= TImageList.CreateSize(16,16);
  SysIL:= SHGetFileInfo('', 0, SFI, SizeOf(SFI), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  if SysIL <> 0 then
  begin
    Smalls.Handle:= SysIL;
    Smalls.ShareImages:= True;
    Smalls.drawingstyle:= dsTransparent;
  end;

  Larges:= TImageList.CreateSize(32,32);
  SysIL:= SHGetFileInfo('', 0, SFI, SizeOf(SFI), SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
  if SysIL <> 0 then
  begin
    Larges.Handle:= SysIL;
    Larges.ShareImages:= True;
    Larges.drawingstyle:= dsTransparent;
  end;
end;

{ Draws the shell icon for the given path at position (x,y) on the canvas.
  Uses SHGFI_SYSICONINDEX to get the icon index into the system image list
  without allocating an icon handle (SHGFI_ICON would leak a GDI handle per call). }
procedure DrawAssociatedImage(s:string;Cnvs:TCanvas;x,y:integer;Size:TIconSize);
var info:tshfileinfo;
begin
  EnsureImageListsCreated;
  fillchar(info,sizeof(tshfileinfo),0);
  shgetfileinfo(PChar(s),0,info,sizeof(tshfileinfo),SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  case Size of
    isSmall: Smalls.Draw(Cnvs,x,y,info.iIcon);
    isLarge: Larges.Draw(Cnvs,x,y,info.iIcon);
  end;
end;

{ Maps Windows.GetDriveType result to TDriveType enum.
  The enum values match the Windows constants (DRIVE_UNKNOWN=0..DRIVE_RAMDISK=6).
  Returns dtUnknown for any unexpected value from the OS. }
function GetDriveType (Drive : char) : TDriveType;
VAR WinType: UINT;
begin
  WinType:= Windows.GetDriveType(PChar(Drive + ':\'));
  if WinType <= Ord(High(TDriveType))
  then Result:= TDriveType(WinType)
  else Result:= dtUnknown;
end;

{ Populates List with single-character drive letters (A..Z) matching the given DrivesType filter.
  Uses Windows.GetLogicalDrives bitmask to enumerate available drives. }
procedure GetLogicalDriveList (List : TStrings; DrivesType:TViewDrivesType);
 var
   Num  : integer;
   Bits : set of 0..25;
 begin
   List.Clear;
   integer (Bits) := Windows.GetLogicalDrives;
   for Num := 0 to 25 do
     if Num in Bits then
      if GetDriveType(Char (Num + Ord('A'))) in DrivesType then
      List.Add (Char (Num + Ord('A')))
end;

{ Returns the volume label for the given drive letter.
  Uses SEM_FAILCRITICALERRORS to suppress "drive not ready" dialogs for removable media. }
function GetVolumeLabel(Drive:char): string;
var
    VolLab: array [0..MAX_PATH] of Char;
    MaxLength,SysFlag:DWord;
    OldErrorMode:UINT;
begin
  Result:= '';
  OldErrorMode:= SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    if GetVolumeInformation(PChar(drive+':\'), VolLab, Length(VolLab), NIL, MaxLength, SysFlag, nil, 0)
    then Result:= VolLab;
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

//TDVDrivesCombo

constructor TDiskDriveCombo.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Style:=csOwnerDrawFixed;
  FViewDrivesType:=[dtRemovable,dtFixed, dtRemote, dtCDROM, dtRamDisk];
  AutoUpdate:=True;
  DisplayIcons:=True;
  DisplayBrackets:=True;
  DisplayLabel:=True;
  DisplaySysName:=True;
end;

procedure TDiskDriveCombo.CreateWnd;
begin
  inherited CreateWnd;
  UpdateDrives;
end;

procedure TDiskDriveCombo.Loaded;
begin
  inherited Loaded;
  SetDrive(FDrive);
end;

procedure TDiskDriveCombo.DropDown;
begin
  if FAutoUpdate then UpdateDrives;
  inherited dropdown;
end;

procedure TDiskDriveCombo.SetIconsSize(Value:TIconSize);
begin
  FIconsSize:=Value;
  case Value of
    isSmall:
    begin
      if GetItemHeight(Font)<18 then itemheight:=18;
      Height:=itemheight;
    end;
    isLarge:
    begin
      if GetItemHeight(Font)<34 then itemheight:=34;
      Height:=itemheight;
    end;
  end;
  RecreateWnd;
  Invalidate;
end;

procedure TDiskDriveCombo.SetDisplayIcons(Value:Boolean);
begin
  FDisplayIcons:=Value;
  invalidate;
end;

procedure TDiskDriveCombo.UpdateDrives;
var s:string;
begin
  if (Items.Count > 0) AND (ItemIndex >= 0)
  then s:= Items[ItemIndex]
  else s:= '';
  Items.Clear;
  GetLogicalDriveList(Items,FViewDrivesType);
  if Items.Count<>0 then
  begin
    if Items.IndexOf(s)<>-1 then ItemIndex:=Items.IndexOf(s)
    else
      ItemIndex:=0;
    s:=Items[ItemIndex];
    FDrive:=s[1];
  end;
end;

procedure TDiskDriveCombo.Change;
var s:string;
begin
  if (Items.Count > 0) AND (ItemIndex >= 0) then
  begin
    s:= Items[ItemIndex];
    FDrive:= s[1];
  end;
  inherited Change;
end;

procedure TDiskDriveCombo.SetCase(Value:TTextCase);
begin
  FTextCase:=Value;
  Invalidate;
end;

procedure TDiskDriveCombo.SetViewDrivesType(Value:TViewDrivesType);
begin
  FViewDrivesType:=Value;
  UpdateDrives;
end;

procedure TDiskDriveCombo.SetDrive(Value:char);
begin
  if Items.indexof(Value)<>-1 then
  begin
    ItemIndex:=Items.IndexOf(Value);
    FDrive:=Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TDiskDriveCombo.SetDisplayBrackets(Value:Boolean);
begin
  FDisplayBrackets:=Value;
  Invalidate;
end;

procedure TDiskDriveCombo.SetDisplayLabel(Value:Boolean);
begin
  FDisplayLabel:=Value;
  Invalidate;
end;

procedure TDiskDriveCombo.SetDisplaySysName(Value:Boolean);
begin
  FDisplaySysName:=Value;
  Invalidate;
end;

procedure TDiskDriveCombo.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ItemHeight:=GetItemHeight(Font);
  SetIconsSize(FIconsSize);
end;

procedure TDiskDriveCombo.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var az,sr:string;
    x,y:integer;
    info:tshfileinfo;
begin
  x:= 0;
  y:= 0;
  az:=Items[Index]+': ';
  sr:=Items[Index]+':\';
  if not FDisplaySysName then
  begin
    if (FDisplayLabel) and (getdrivetype(az[1])<>dtRemovable) then
    begin
      if FDisplayBrackets then az:=az+'[';
      az:=az+GetVolumeLabel(az[1]);
      if FDisplayBrackets then az:=az+']';
    end;
    case FTextCase of
      tcLowerCase: az:=LowerCase(az);
      tcUpperCase: az:=UpperCase(az);
    end;
  end
  else
  begin
    fillchar(info,sizeof(tshfileinfo),0);
    shgetfileinfo(PChar(sr),0,info,sizeof(tshfileinfo),shgfi_displayname);
    az:=info.szDisplayName;
  end;
  Canvas.fillrect(rect);
  rect.top:=rect.Top+1;
  rect.bottom:=rect.Bottom-1;
  rect.Left:=rect.Left+2;
  if FDisplayIcons then
  begin
    case FIconsSize of
      isSmall:
      begin
        y:=rect.Top+(((rect.Bottom-rect.Top)-16) div 2);
        x:=20;
      end;
      isLarge:
      begin
        y:=rect.Top+(((rect.Bottom-rect.Top)-32) div 2);
        x:=36;
      end;
    end;
    DrawAssociatedImage(sr,Canvas,rect.Left,y,FIconsSize);
  end
  else
    x:=2;
  rect.Left:=rect.Left+x;
  DrawText(Canvas.Handle, PChar(az), Length(az), Rect,
           DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
end;

//TDVDrivesList

constructor TDiskDriveList.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Style:=lbOwnerDrawFixed;
  FViewDrivesType:=[dtRemovable,dtFixed, dtRemote, dtCDROM, dtRamDisk];
  DisplayIcons:=True;
  DisplayBrackets:=True;
  DisplayLabel:=True;
  DisplaySysName:=True;
end;

procedure TDiskDriveList.CreateWnd;
begin
  inherited CreateWnd;
  UpdateDrives;
end;

procedure TDiskDriveList.Loaded;
begin
  inherited Loaded;
  SetDrive(FDrive);
end;

procedure TDiskDriveList.SetIconsSize(Value:TIconSize);
begin
  FIconsSize:=Value;
  case Value of
    isSmall: if GetItemHeight(Font)<18 then itemheight:=18;
    isLarge: if GetItemHeight(Font)<34 then itemheight:=34;
  end;
  RecreateWnd;
  Invalidate;
end;

procedure TDiskDriveList.SetDisplayIcons(Value:Boolean);
begin
  FDisplayIcons:=Value;
  invalidate;
end;

procedure TDiskDriveList.UpdateDrives;
var s:string;
begin
  if (Items.Count > 0) AND (ItemIndex >= 0)
  then s:= Items[ItemIndex]
  else s:= '';
  Items.Clear;
  GetLogicalDriveList(Items,FViewDrivesType);
  if Items.Count<>0 then
  begin
    if Items.IndexOf(s)<>-1 then ItemIndex:=Items.IndexOf(s)
    else
      ItemIndex:=0;
    s:=Items[ItemIndex];
    FDrive:=s[1];
  end;
end;

procedure TDiskDriveList.Click;
var s:string;
begin
  if (Items.Count > 0) AND (ItemIndex >= 0) then
  begin
    s:= Items[ItemIndex];
    FDrive:= s[1];
  end;
  inherited Click;
end;

procedure TDiskDriveList.SetCase(Value:TTextCase);
begin
  FTextCase:=Value;
  Invalidate;
end;

procedure TDiskDriveList.SetViewDrivesType(Value:TViewDrivesType);
begin
  FViewDrivesType:=Value;
  UpdateDrives;
end;

procedure TDiskDriveList.SetDrive(Value:char);
begin
  if Items.indexof(Value)<>-1 then
  begin
    ItemIndex:=Items.IndexOf(Value);
    FDrive:=Value;
    if Assigned(OnClick) then OnClick(Self);
  end;
end;

procedure TDiskDriveList.SetDisplayBrackets(Value:Boolean);
begin
  FDisplayBrackets:=Value;
  Invalidate;
end;

procedure TDiskDriveList.SetDisplayLabel(Value:Boolean);
begin
  FDisplayLabel:=Value;
  Invalidate;
end;

procedure TDiskDriveList.SetDisplaySysName(Value:Boolean);
begin
  FDisplaySysName:=Value;
  Invalidate;
end;

procedure TDiskDriveList.CMFontChanged(var Message: TMessage);
begin
  inherited;
  itemheight:=GetItemHeight(Font);
  SetIconsSize(FIconsSize);
end;

procedure TDiskDriveList.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var az,sr:string;
    x,y:integer;
    info:tshfileinfo;
begin
  x:= 0;
  y:= 0;
  az:=Items[Index]+': ';
  sr:=Items[Index]+':\';
  if not FDisplaySysName then
   begin
     if (FDisplayLabel) and (getdrivetype(az[1])<>dtRemovable) then
     begin
       if FDisplayBrackets then az:=az+'[';
       az:=az+GetVolumeLabel(az[1]);
       if FDisplayBrackets then az:=az+']';
     end;
     case FTextCase of
       tcLowerCase: az:=LowerCase(az);
       tcUpperCase: az:=UpperCase(az);
     end;
   end
  else
   begin
     fillchar(info,sizeof(tshfileinfo),0);
     shgetfileinfo(PChar(sr),0,info,sizeof(tshfileinfo),shgfi_displayname);
     az:=info.szDisplayName;
   end;

  Canvas.fillrect(rect);
  rect.top:=rect.Top+1;
  rect.bottom:=rect.Bottom-1;
  rect.Left:=rect.Left+2;
  if FDisplayIcons then
   begin
     case FIconsSize of
       isSmall:
          begin
            y:=rect.Top+(((rect.Bottom-rect.Top)-16) div 2);
            x:=20;
          end;
       isLarge:
          begin
            y:=rect.Top+(((rect.Bottom-rect.Top)-32) div 2);
            x:=36;
          end;
     end;
     DrawAssociatedImage(sr,Canvas,rect.Left,y,FIconsSize);
   end
  else
    x:=2;

  rect.Left:=rect.Left+x;
  DrawText(Canvas.Handle, PChar(az), Length(az), Rect, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
end;


procedure Register;
begin
  RegisterComponents('3rd_party', [TDiskDriveCombo]);
  RegisterComponents('3rd_party', [TDiskDriveList]);
end;




finalization
  FreeAndNil(Larges);
  FreeAndNil(Smalls);

end.
