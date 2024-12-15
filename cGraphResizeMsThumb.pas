unit cGraphResizeMsThumb;

{=============================================================================================================
   Source:
      https://www.experts-exchange.com/questions/21330007/Thumbnail-in-TListView.html
      Prog.hu/tudastar/148932/fajlhoz-tartozo-windows-os-nezokep-lekerdezese#e3
--------------------------------------------------------------------------------------------------------------
   ISSUES:
      SIt seems it cannot generate images higher than 256px


   INPUT:
      Videos, Images.
      Note: If the input file is not supported it will return a bitmap with a system icon for that file type

   TESTER
      c:\MyProjects\Projects GRAPHICS Resamplers\GLOBAL Tester\TEST IMAGES\
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  Windows, SysUtils, ActiveX, ShellApi, CommCtrl, ShlObj,
  System.Classes, System.Math, Vcl.Graphics;

TYPE
  TFileThumb = class
  private
    FhImageList48: Cardinal;
    FWidth: Integer;
    FIconSize: Integer;
    FBmp: TBitmap;
    FFilePath: String;
    procedure SetFile(const Value: String);
    procedure SetSize(Value: Integer);
  public
    property Width: Integer read FWidth write SetSize;
    property ThumbBmp: TBitmap read FBmp;
    property FilePath: String read FFilePath write SetFile;

    procedure GenerateThumbnail;
    procedure GenerateThumbnail2;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

const
  MinSize = 52;
  MaxSize = 65535;
  ColorFormat: DWORD = 24;
  IEIFLAG_OFFLINE = 8;
  IEIFLAG_SCREEN = $20;

constructor TFileThumb.Create;
var
  hImagList16, hImagList32: Cardinal;
  ShInfo: TShFileInfo;
  IconHeight, IconWidth: Integer;
begin
  inherited;

  FWidth := 100;
  FFilePath := '';
  FBmp := TBitmap.Create;
  FBmp.PixelFormat := pf24Bit;

  // Retrieve system image list for icons
  hImagList32 := SHGetFileInfo(
    'file.txt', FILE_ATTRIBUTE_NORMAL, ShInfo, SizeOf(ShInfo),
    SHGFI_LARGEICON or SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES);

  hImagList16 := SHGetFileInfo(
    'file.txt', FILE_ATTRIBUTE_NORMAL, ShInfo, SizeOf(ShInfo),
    SHGFI_SMALLICON or SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES);

  FhImageList48 := hImagList16 + (hImagList16 - hImagList32);
  if ImageList_GetIconSize(FhImageList48, IconHeight, IconWidth) and (IconHeight = 48) then
    FIconSize := 48
  else
  begin
    FhImageList48 := hImagList32;
    if ImageList_GetIconSize(hImagList32, IconHeight, IconWidth) then
      FIconSize := IconHeight
    else
      FIconSize := 32;
  end;

  FBmp.SetSize(FWidth, FWidth);
  FBmp.Canvas.Brush.Color := GetSysColor(COLOR_WINDOW);
  FBmp.Canvas.FillRect(Rect(0, 0, FWidth, FWidth));
end;

destructor TFileThumb.Destroy;
begin
  FBmp.Free;
  inherited;
end;

procedure TFileThumb.SetFile(const Value: String);
begin
  if Value = FFilePath then Exit;
  FFilePath := Value;
end;

procedure TFileThumb.SetSize(Value: Integer);
begin
  if Value = FWidth then Exit;
  FWidth := EnsureRange(Value, MinSize, MaxSize);
  FBmp.SetSize(FWidth, FWidth);
end;


// This uses ImageFactory
procedure TFileThumb.GenerateThumbnail;
var
  ShellItem: IShellItem;
  ImageFactory: IShellItemImageFactory;
  hBmp: HBITMAP;
  Size1: TSize;
begin
  // Initialize the bitmap canvas
  FBmp.SetSize(FWidth, FWidth);
  FBmp.Canvas.Brush.Color := GetSysColor(COLOR_WINDOW);
  FBmp.Canvas.FillRect(Rect(0, 0, FWidth, FWidth));
  if (FFilePath = '') or not FileExists(FFilePath) then Exit;
  // Use IShellItemImageFactory for thumbnails
  if SHCreateItemFromParsingName(PChar(FFilePath), nil, IShellItem, ShellItem) <> S_OK then Exit;
  if ShellItem.QueryInterface(IShellItemImageFactory, ImageFactory) <> S_OK then Exit;
  // Request a thumbnail with the desired size
  Size1.cx := FWidth;
  Size1.cy := FWidth;
  if ImageFactory.GetImage(Size1, SIIGBF_RESIZETOFIT, hBmp) = S_OK
  then FBmp.Handle := hBmp; // Assign HBITMAP to TBitmap
end;



// This uses IExtractImg which seems to be limited to max 160 pixels
// I can delete this method
procedure TFileThumb.GenerateThumbnail2;
var
  Path, Name: String;
  FolderISF, DesktopISF: IShellFolder;
  IExtractImg: IExtractImage;
  Attrib, Eaten: DWORD;
  PItemIDL: PItemIDList;
  MemAlloc: IMalloc;
  CharBuf: array[0..2047] of WideChar;
  hBmp: HBITMAP;
  Size1: TSize;
  Priority, Flags: Cardinal;
  GLResult: HRESULT;
  Mid: Integer;
  ShInfo: TShFileInfo;
begin
  // Reset bitmap size and clear content
  FBmp.SetSize(FWidth, FWidth);
  FBmp.Canvas.Brush.Color := GetSysColor(COLOR_WINDOW);
  FBmp.Canvas.FillRect(Rect(0, 0, FWidth, FWidth));

  if (FFilePath = '') or not FileExists(FFilePath) then Exit;

  Path := ExtractFilePath(FFilePath);
  Name := ExtractFileName(FFilePath);
  Mid := (FWidth - FIconSize) div 2;

  SHGetFileInfo(PChar(FFilePath), FILE_ATTRIBUTE_NORMAL, ShInfo, SizeOf(ShInfo), SHGFI_LARGEICON or SHGFI_SYSICONINDEX);
  ImageList_Draw(FhImageList48, ShInfo.iIcon, FBmp.Canvas.Handle, Mid, Mid, ILD_TRANSPARENT);

  if not ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5)) then Exit;

  if (SHGetMalloc(MemAlloc) <> S_OK) or not Assigned(MemAlloc) or (SHGetDesktopFolder(DesktopISF) <> S_OK) then Exit;

  if DesktopISF.ParseDisplayName(0, nil, PWideChar(Path), Eaten, PItemIDL, Attrib) <> S_OK then Exit;

  DesktopISF.BindToObject(PItemIDL, nil, IShellFolder, FolderISF);
  MemAlloc.Free(PItemIDL);

  if FolderISF.ParseDisplayName(0, nil, PWideChar(Name), Eaten, PItemIDL, Attrib) <> S_OK then Exit;

  FolderISF.GetUIObjectOf(0, 1, PItemIDL, IExtractImage, nil, Pointer(IExtractImg));
  MemAlloc.Free(PItemIDL);

  if not Assigned(IExtractImg) then Exit;

  Size1.cx := FWidth;
  Size1.cy := FWidth;
  Flags := IEIFLAG_SCREEN or IEIFLAG_OFFLINE;
  Priority := 0;

  GLResult := IExtractImg.GetLocation(CharBuf, Length(CharBuf), Priority, Size1, ColorFormat, Flags);
  if (GLResult = S_OK) or (GLResult = E_PENDING) then
    if (IExtractImg.Extract(hBmp) = S_OK) and (hBmp <> 0)
    then FBmp.Handle := hBmp;
end;

end.
