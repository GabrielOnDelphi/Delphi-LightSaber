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
   - macOS/Linux/iOS/Android: Not yet implemented (stubs only)

   See also: FormScreenCapture.pas for the UI layer
=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Generics.Collections,
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
    function GetCapturedImages: TObjectList<TBitmap>;

    property Screenshot: TBitmap read FScreenshot;
    property LastSelectionRect: TRectF read FLastSelectionRect;
    property CaptureTipShown: Integer read FCaptureTipShown write FCaptureTipShown;
  end;


  // Callback type for receiving captured images (used by FormScreenCapture.pas)
  TScreenCaptureCallback = reference to procedure(CapturedImages: TObjectList<TBitmap>);


IMPLEMENTATION

USES
  LightFmx.Common.IniFile, LightCore.Types
  {$IFDEF MSWINDOWS}, Winapi.Windows{$ENDIF};


{-------------------------------------------------------------------------------------------------------------
   TScreenCaptureManager
-------------------------------------------------------------------------------------------------------------}

constructor TScreenCaptureManager.Create;
begin
  inherited Create;
  FCapturedImages:= TObjectList<TBitmap>.Create(OwnObjects);
  FScreenshot:= TBitmap.Create;
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

   Note: Captures only the primary monitor. Multi-monitor support would
   require EnumDisplayMonitors and combining captures.

   Other Platforms:
     Not yet implemented - shows a message and leaves FScreenshot empty.  }
procedure TScreenCaptureManager.StartCapture;
{$IFDEF MSWINDOWS}
VAR
  ScreenDC, MemDC: HDC;
  ScreenWidth, ScreenHeight: Integer;
  hBitmap: HBITMAP;
  BitmapData: TBitmapData;
  BitmapInfo: TBitmapInfo;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  // Get device context for the entire screen (0 = desktop)
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
  {$ELSE}
  // macOS/Linux/iOS/Android: Not yet implemented
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
  CroppedBitmap, CapturedCopy: TBitmap;
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
  CroppedBitmap:= TBitmap.Create;
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

    // Create a copy for the captured images list (CroppedBitmap will be freed)
    CapturedCopy:= TBitmap.Create;
    CapturedCopy.Assign(CroppedBitmap);
    FCapturedImages.Add(CapturedCopy);

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
function TScreenCaptureManager.GetCapturedImages: TObjectList<TBitmap>;
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


