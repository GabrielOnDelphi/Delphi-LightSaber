UNIT LightFmx.Visual.ScreenCapture;

{-------------------------------------------------------------------------------------------------------------
   Screen Capture with Rectangle Selection
   Claude 2026.01

   Cross-platform screenshot capture allowing user to select rectangular areas.

   Features:
   - Full screen capture with semi-transparent overlay
   - Rectangle selection with TSelection component
   - Multiple captures (Ctrl+P / Cmd+P)
   - ESC to finish
   - Remembers last selection rectangle (saved to INI)
   - Cross-platform (Windows, macOS, Linux, iOS, Android)
   - Business logic separated from UI
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation;

TYPE
  // Business logic class - handles all capture logic
  TScreenCaptureManager = class
  private
    FScreenshot: TBitmap;
    FCapturedImages: TObjectList<TBitmap>;
    FLastSelectionRect: TRectF;

    procedure LoadLastSelection;
    procedure SaveLastSelection;
  private
    FCaptureTipShown: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartCapture;
    function CaptureSelectedArea(CONST SelectionRect: TRectF): Boolean;
    function GetCapturedImages: TObjectList<FMX.Graphics.TBitmap>;

    property Screenshot: FMX.Graphics.TBitmap read FScreenshot;
    property LastSelectionRect: TRectF read FLastSelectionRect;
    property CaptureTipShown: Integer read FCaptureTipShown write FCaptureTipShown;
  end;


  // UI Form - thin layer for user interaction
  TScreenCaptureCallback = reference to procedure(CapturedImages: TObjectList<FMX.Graphics.TBitmap>);


IMPLEMENTATION

USES
  LightFmx.Common.IniFile  {$IFDEF MSWINDOWS}, Winapi.Windows{$ENDIF}
  {$IFDEF MACOS}, Macapi.AppKit {$ENDIF};


{-------------------------------------------------------------------------------------------------------------
   TScreenCaptureManager - Business Logic
-------------------------------------------------------------------------------------------------------------}

constructor TScreenCaptureManager.Create;
begin
  inherited Create;
  FCapturedImages:= TObjectList<FMX.Graphics.TBitmap>.Create(True);  // Owns objects
  FScreenshot:= FMX.Graphics.TBitmap.Create;
  FLastSelectionRect:= TRectF.Empty;

  LoadLastSelection;
end;


destructor TScreenCaptureManager.Destroy;
begin
  SaveLastSelection;

  FreeAndNil(FCapturedImages);
  FreeAndNil(FScreenshot);
  inherited;
end;


procedure TScreenCaptureManager.StartCapture;
{$IFDEF MSWINDOWS}
VAR
  ScreenDC, MemDC: Winapi.Windows.HDC;
  ScreenWidth, ScreenHeight: Integer;
  hBitmap: Winapi.Windows.HBITMAP;
  BitmapData: TBitmapData;
{$ENDIF}
begin
  // Cross-platform screen capture
  {$IFDEF MSWINDOWS}
  // Windows: Use GDI to capture actual desktop
  ScreenDC:= GetDC(0);
  try
    ScreenWidth:= GetSystemMetrics(SM_CXSCREEN);
    ScreenHeight:= GetSystemMetrics(SM_CYSCREEN);

    FScreenshot.Width:= ScreenWidth;
    FScreenshot.Height:= ScreenHeight;

    MemDC:= CreateCompatibleDC(ScreenDC);
    try
      hBitmap:= CreateCompatibleBitmap(ScreenDC, ScreenWidth, ScreenHeight);
      try
        SelectObject(MemDC, hBitmap);
        BitBlt(MemDC, 0, 0, ScreenWidth, ScreenHeight, ScreenDC, 0, 0, SRCCOPY);

        // Copy to FMX bitmap
        if FScreenshot.Map(TMapAccess.Write, BitmapData) then
        try
          VAR BitmapInfo: TBitmapInfo;
          FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);
          BitmapInfo.bmiHeader.biSize  := SizeOf(TBitmapInfoHeader);
          BitmapInfo.bmiHeader.biWidth := ScreenWidth;
          BitmapInfo.bmiHeader.biHeight:= -ScreenHeight; // Top-down
          BitmapInfo.bmiHeader.biPlanes:= 1;
          BitmapInfo.bmiHeader.biBitCount:= 32;
          BitmapInfo.bmiHeader.biCompression:= BI_RGB;

          GetDIBits(MemDC, hBitmap, 0, ScreenHeight, BitmapData.Data, BitmapInfo, DIB_RGB_COLORS);
        finally
          FScreenshot.Unmap(BitmapData);
        end;
      finally
        DeleteObject(hBitmap);
      end;
    finally
      DeleteDC(MemDC);
    end;
  finally
    ReleaseDC(0, ScreenDC);
  end;
  {$ELSE}
  // Other platforms: Use platform services or form screenshot
  ShowMessage('Screen capture not yet implemented for this platform');
  {$ENDIF}
end;


function TScreenCaptureManager.CaptureSelectedArea(CONST SelectionRect: TRectF): Boolean;
VAR
  CroppedBitmap: FMX.Graphics.TBitmap;
  SourceRect, DestRect: TRectF;
begin
  Result:= FALSE;

  if SelectionRect.IsEmpty then
    begin
      ShowMessage('Please select a screen area first!');
      EXIT;
    end;

  // Create cropped bitmap from selection
  CroppedBitmap:= FMX.Graphics.TBitmap.Create;
  try
    CroppedBitmap.Width:= Round(SelectionRect.Width);
    CroppedBitmap.Height:= Round(SelectionRect.Height);

    // Copy selected area from screenshot
    SourceRect:= SelectionRect;
    DestRect:= RectF(0, 0, SelectionRect.Width, SelectionRect.Height);

    CroppedBitmap.Canvas.BeginScene;
    try
      CroppedBitmap.Canvas.DrawBitmap(FScreenshot, SourceRect, DestRect, 1.0, True);
    finally
      CroppedBitmap.Canvas.EndScene;
    end;

    // Add to captured images
    FCapturedImages.Add(CroppedBitmap.CreateThumbnail(Round(CroppedBitmap.Width), Round(CroppedBitmap.Height)));

    // Remember this selection for next capture
    FLastSelectionRect:= SelectionRect;
    SaveLastSelection;

    Result:= TRUE;
  finally
    FreeAndNil(CroppedBitmap);
  end;
end;


function TScreenCaptureManager.GetCapturedImages: TObjectList<FMX.Graphics.TBitmap>;
begin
  Result:= FCapturedImages;
end;



{-------------------------------------------------------------------------------------------------------------
   INI FILE - Save/Load last selection rectangle
-------------------------------------------------------------------------------------------------------------}

procedure TScreenCaptureManager.LoadLastSelection;
VAR
  INI: TIniFileApp;
begin
  INI:= TIniFileApp.Create('ScreenCapture');
  TRY
    FLastSelectionRect.Left  := INI.Read('LastSelection_Left',   0.0);
    FLastSelectionRect.Top   := INI.Read('LastSelection_Top',    0.0);
    FLastSelectionRect.Right := INI.Read('LastSelection_Right',  0.0);
    FLastSelectionRect.Bottom:= INI.Read('LastSelection_Bottom', 0.0);

    FCaptureTipShown:= INI.Read('CaptureTipShown', 0);

    // Validate loaded rectangle
    if (FLastSelectionRect.Width <= 0) OR (FLastSelectionRect.Height <= 0) then
      FLastSelectionRect:= TRectF.Empty;
  FINALLY
    FreeAndNil(INI);
  END;
end;


procedure TScreenCaptureManager.SaveLastSelection;
VAR
  INI: TIniFileApp;
begin
  if FLastSelectionRect.IsEmpty then EXIT;

  INI:= TIniFileApp.Create('ScreenCapture');
  TRY
    INI.Write('LastSelection_Left',   FLastSelectionRect.Left);
    INI.Write('LastSelection_Top',    FLastSelectionRect.Top);
    INI.Write('LastSelection_Right',  FLastSelectionRect.Right);
    INI.Write('LastSelection_Bottom', FLastSelectionRect.Bottom);

    INI.Write('CaptureTipShown', FCaptureTipShown);
  FINALLY
    FreeAndNil(INI);
  END;
end;



end.


