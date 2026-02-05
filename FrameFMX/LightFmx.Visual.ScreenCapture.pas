UNIT LightFmx.Visual.ScreenCapture;

{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Screen Capture Manager - Business Logic Layer

   Handles the core screen capture functionality, separated from the UI layer (FormScreenCapture.pas).

   Features:
   - Full screen capture (Windows implemented, other platforms: stub)
   - Rectangular area selection and cropping
   - Multiple captures in a single session
   - Persists last selection rectangle to INI file
   - Persists tip display counter to INI file

   Usage:
     1. Create TScreenCaptureManager
     2. Call StartCapture to capture the desktop
     3. Call CaptureSelectedArea with a TRectF to crop and store a region
     4. Call GetCapturedImages to retrieve all captured regions

   Platform Support:
   - Windows: Full GDI-based screen capture
   - macOS: Core Graphics screen capture (requires Screen Recording permission in 10.15+)
   - Linux/iOS/Android: Not yet implemented (stubs only)

   See also: FormScreenCapture.pas for the UI layer
=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Types, System.Classes, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation;

TYPE
  TScreenCaptureManager = class
  private
    FScreenshot: TBitmap;                    // Full screen capture
    FCapturedImages: TObjectList<TBitmap>;   // User-selected regions (owned)
    FLastSelectionRect: TRectF;              // Persisted to INI for convenience
    FCaptureTipShown: Integer;               // Counter: hide instructions after N views

    procedure LoadLastSelection;
    procedure SaveLastSelection;
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


  // Callback type for receiving captured images (used by FormScreenCapture.pas)
  TScreenCaptureCallback = reference to procedure(CapturedImages: TObjectList<FMX.Graphics.TBitmap>);


IMPLEMENTATION

USES
  LightFmx.Common.IniFile  {$IFDEF MSWINDOWS}, Winapi.Windows{$ENDIF}
  {$IFDEF MACOS}, Macapi.CoreGraphics, Macapi.CoreFoundation, Macapi.CocoaTypes{$ENDIF};


{-------------------------------------------------------------------------------------------------------------
   TScreenCaptureManager
-------------------------------------------------------------------------------------------------------------}

constructor TScreenCaptureManager.Create;
begin
  inherited Create;
  FCapturedImages:= TObjectList<FMX.Graphics.TBitmap>.Create(TRUE);  // Owns objects
  FScreenshot:= FMX.Graphics.TBitmap.Create;
  FLastSelectionRect:= TRectF.Empty;
  FCaptureTipShown:= 0;

  LoadLastSelection;  // Overwrites FCaptureTipShown and FLastSelectionRect if INI exists
end;


destructor TScreenCaptureManager.Destroy;
begin
  SaveLastSelection;

  FreeAndNil(FCapturedImages);
  FreeAndNil(FScreenshot);
  inherited;
end;


{  Captures the entire primary screen/desktop into FScreenshot.

   Windows Implementation:
     Uses GDI functions to capture the desktop:
     1. GetDC(0) - Gets device context for the entire screen
     2. Creates a compatible memory DC and bitmap
     3. BitBlt copies screen contents to the memory bitmap
     4. Maps the FMX bitmap for direct pixel access
     5. GetDIBits transfers pixels from GDI bitmap to FMX bitmap

   macOS Implementation:
     Uses Core Graphics to capture the screen:
     1. CGWindowListCreateImage captures all on-screen windows
     2. Maps the FMX bitmap for direct pixel access
     3. Creates a CGBitmapContext pointing to FMX bitmap data
     4. CGContextDrawImage renders the screen image into the bitmap
     Note: Requires Screen Recording permission in macOS 10.15 Catalina and later.

   Note: Captures only the primary monitor. Multi-monitor support would
   require EnumDisplayMonitors (Windows) or multiple display handling (macOS).

   Other Platforms:
     Not yet implemented - shows a message and leaves FScreenshot empty.  }
procedure TScreenCaptureManager.StartCapture;
{$IFDEF MSWINDOWS}
VAR
  ScreenDC, MemDC: Winapi.Windows.HDC;
  ScreenWidth, ScreenHeight: Integer;
  hBitmap: Winapi.Windows.HBITMAP;
  BitmapData: TBitmapData;
{$ENDIF}
{$IFDEF MACOS}
VAR
  ScreenImage: CGImageRef;
  ScreenWidth, ScreenHeight: Integer;
  ColorSpace: CGColorSpaceRef;
  Context: CGContextRef;
  BitmapData: TBitmapData;
  ScreenRect: CGRect;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  // Windows: Use GDI to capture the desktop
  ScreenDC:= GetDC(0);
  try
    ScreenWidth:= GetSystemMetrics(SM_CXSCREEN);
    ScreenHeight:= GetSystemMetrics(SM_CYSCREEN);

    // Resize FMX bitmap to match screen dimensions
    FScreenshot.Width:= ScreenWidth;
    FScreenshot.Height:= ScreenHeight;

    // Create memory DC compatible with screen
    MemDC:= CreateCompatibleDC(ScreenDC);
    try
      // Create GDI bitmap to hold the screen capture
      hBitmap:= CreateCompatibleBitmap(ScreenDC, ScreenWidth, ScreenHeight);
      try
        SelectObject(MemDC, hBitmap);
        // Copy screen pixels to memory bitmap
        BitBlt(MemDC, 0, 0, ScreenWidth, ScreenHeight, ScreenDC, 0, 0, SRCCOPY);

        // Transfer pixels from GDI bitmap to FMX bitmap
        if FScreenshot.Map(TMapAccess.Write, BitmapData) then
        try
          VAR BitmapInfo: TBitmapInfo;
          FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);
          BitmapInfo.bmiHeader.biSize       := SizeOf(TBitmapInfoHeader);
          BitmapInfo.bmiHeader.biWidth      := ScreenWidth;
          BitmapInfo.bmiHeader.biHeight     := -ScreenHeight;  // Negative = top-down DIB
          BitmapInfo.bmiHeader.biPlanes     := 1;
          BitmapInfo.bmiHeader.biBitCount   := 32;
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

  {$ELSEIF DEFINED(MACOS)}
  // macOS: Use Core Graphics to capture the screen
  // Note: Requires Screen Recording permission in macOS 10.15+
  ScreenRect:= CGRectInfinite;  // Capture entire screen
  ScreenImage:= CGWindowListCreateImage(ScreenRect, kCGWindowListOptionOnScreenOnly, kCGNullWindowID, kCGWindowImageDefault);
  if ScreenImage <> nil then
  try
    ScreenWidth:= CGImageGetWidth(ScreenImage);
    ScreenHeight:= CGImageGetHeight(ScreenImage);

    // Resize FMX bitmap to match screen dimensions
    FScreenshot.Width:= ScreenWidth;
    FScreenshot.Height:= ScreenHeight;

    // Map FMX bitmap for direct pixel access
    if FScreenshot.Map(TMapAccess.Write, BitmapData) then
    try
      // Create color space and bitmap context to draw into FMX bitmap
      ColorSpace:= CGColorSpaceCreateDeviceRGB;
      try
        // Create context pointing directly to FMX bitmap data
        // FMX uses BGRA format, so we use kCGImageAlphaPremultipliedFirst with kCGBitmapByteOrder32Little
        Context:= CGBitmapContextCreate(
          BitmapData.Data,
          ScreenWidth,
          ScreenHeight,
          8,  // bits per component
          BitmapData.Pitch,
          ColorSpace,
          kCGImageAlphaPremultipliedFirst OR kCGBitmapByteOrder32Little
        );
        if Context <> nil then
        try
          // Draw the screen image into our bitmap context
          CGContextDrawImage(Context, CGRectMake(0, 0, ScreenWidth, ScreenHeight), ScreenImage);
        finally
          CGContextRelease(Context);
        end;
      finally
        CGColorSpaceRelease(ColorSpace);
      end;
    finally
      FScreenshot.Unmap(BitmapData);
    end;
  finally
    CGImageRelease(ScreenImage);
  end
  else
    ShowMessage('Screen capture failed. Please grant Screen Recording permission in System Preferences > Security & Privacy > Privacy.');

  {$ELSE}
  // Linux/iOS/Android: Not yet implemented
  ShowMessage('Screen capture not yet implemented for this platform');
  {$ENDIF}
end;


{  Captures the selected rectangular region from the screenshot.

   Parameters:
     SelectionRect - The rectangle defining the area to capture (in screen coordinates)

   Returns:
     TRUE if capture succeeded, FALSE if selection is empty or no screenshot available

   Side Effects:
     - Adds the cropped image to FCapturedImages list
     - Updates FLastSelectionRect for persistence
     - Saves selection to INI file  }
function TScreenCaptureManager.CaptureSelectedArea(CONST SelectionRect: TRectF): Boolean;
VAR
  CroppedBitmap: FMX.Graphics.TBitmap;
  SourceRect, DestRect: TRectF;
begin
  Result:= FALSE;

  // Validate screenshot exists
  if NOT Assigned(FScreenshot) OR FScreenshot.IsEmpty then
    begin
      ShowMessage('No screenshot available. Call StartCapture first.');
      EXIT;
    end;

  // Validate selection
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

    // Add to captured images (create a copy since CroppedBitmap will be freed)
    FCapturedImages.Add(CroppedBitmap.CreateThumbnail(Round(CroppedBitmap.Width), Round(CroppedBitmap.Height)));

    // Remember this selection for next capture
    FLastSelectionRect:= SelectionRect;
    SaveLastSelection;

    Result:= TRUE;
  finally
    FreeAndNil(CroppedBitmap);
  end;
end;


{  Returns the list of captured images.
   Note: The caller should NOT free the returned list - it's owned by this manager.  }
function TScreenCaptureManager.GetCapturedImages: TObjectList<FMX.Graphics.TBitmap>;
begin
  Result:= FCapturedImages;
end;



{-------------------------------------------------------------------------------------------------------------
   INI Persistence
   Saves/loads last selection rectangle and tip counter to user's INI file.
   Section: 'ScreenCapture'
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
  INI:= TIniFileApp.Create('ScreenCapture');
  TRY
    // Save selection rectangle (only if valid)
    if NOT FLastSelectionRect.IsEmpty then
      begin
        INI.Write('LastSelection_Left',   FLastSelectionRect.Left);
        INI.Write('LastSelection_Top',    FLastSelectionRect.Top);
        INI.Write('LastSelection_Right',  FLastSelectionRect.Right);
        INI.Write('LastSelection_Bottom', FLastSelectionRect.Bottom);
      end;

    // Always save tip counter (independent of selection rectangle)
    INI.Write('CaptureTipShown', FCaptureTipShown);
  FINALLY
    FreeAndNil(INI);
  END;
end;



end.


