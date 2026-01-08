UNIT FormScreenCapture;

{-------------------------------------------------------------------------------------------------------------
   Screen Capture with Rectangle Selection
   Claude 2026.01

   Cross-platform screenshot capture allowing user to select rectangular areas.
-------------------------------------------------------------------------------------------------------------}

//ToDo: show some glossy effect (for the covered image). FMX musth have some glossy effects.

INTERFACE

USES
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.StdCtrls, FMX.Controls.Presentation, LightFmx.Visual.ScreenCapture;

TYPE
  TfrmScreenCapture = class(TForm)
    layMain: TLayout;
    imgScreenshot: TImageControl;
    pbOverlay: TPaintBox;
    SelectionRect: TSelection;
    layInstructions: TLayout;
    lblInstructions: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pbOverlayPaint(Sender: TObject; Canvas: TCanvas);
    procedure SelectionRectChange(Sender: TObject);
  private
    FSCManager: TScreenCaptureManager;
    procedure UpdateInstructions;
    function IsCaptureKeyPressed(Key: Word; Shift: TShiftState): Boolean;
    procedure ShowCaptureFlash;
  public
    destructor Destroy; override;
    procedure StartCapture;
  end;


procedure ScreenCaptureNow(OnComplete: TScreenCaptureCallback);


IMPLEMENTATION {$R *.fmx}

USES
  {$IFDEF MSWINDOWS} Winapi.Windows{$ENDIF}
  {$IFDEF MACOS}, Macapi.AppKit {$ENDIF};






{-------------------------------------------------------------------------------------------------------------
   Utils
-------------------------------------------------------------------------------------------------------------}
procedure ScreenCaptureNow(OnComplete: TScreenCaptureCallback);
begin
  VAR FOverlay:= TfrmScreenCapture.Create(nil);
  try
    FOverlay.StartCapture;
    FOverlay.ShowModal;

    if Assigned(OnComplete)
    then OnComplete(FOverlay.FSCManager.GetCapturedImages);
  finally
    FOverlay.Close;
  end;
end;






{-------------------------------------------------------------------------------------------------------------
   Form
-------------------------------------------------------------------------------------------------------------}

procedure TfrmScreenCapture.FormCreate(Sender: TObject);
begin
  FSCManager:= TScreenCaptureManager.Create;
  layInstructions.Visible:= FSCManager.CaptureTipShown < 3;
end;


procedure TfrmScreenCapture.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Don't free FSCManager here - it's needed for the callback after ShowModal returns
  // FSCManager will be freed when the form is destroyed
  Action:= TCloseAction.caHide;   // Don't set caFree - form is manually freed by ScreenCaptureNow
end;


destructor TfrmScreenCapture.Destroy;
begin
  FreeAndNil(FSCManager);
  inherited;
end;


procedure TfrmScreenCapture.StartCapture;
begin
  // Hide form temporarily to capture clean screenshot
  Hide;
  Application.ProcessMessages;
  Sleep(100);

  // Capture screen
  FSCManager.StartCapture;

  // Display screenshot in image control (hidden, used only as source for painting)
  imgScreenshot.Bitmap.Assign(FSCManager.Screenshot);

  // Set last selection if available
  if NOT FSCManager.LastSelectionRect.IsEmpty
  then SelectionRect.BoundsRect:= FSCManager.LastSelectionRect;

  // Show form
  Show;
  UpdateInstructions;
end;


procedure TfrmScreenCapture.pbOverlayPaint(Sender: TObject; Canvas: TCanvas);
VAR
  FullRect, SelectRect, DrawRect: TRectF;
begin
  if NOT Assigned(FSCManager) OR NOT Assigned(FSCManager.Screenshot) then EXIT;

  FullRect:= pbOverlay.LocalRect;
  SelectRect:= SelectionRect.BoundsRect;

  Canvas.BeginScene;
  try
    // Draw the entire screenshot dimmed (fog effect)
    Canvas.DrawBitmap(FSCManager.Screenshot, FSCManager.Screenshot.BoundsF, FullRect, 0.3, True);

    // Draw the selected area at full brightness (lift the fog)
    if NOT SelectRect.IsEmpty then
      begin
        DrawRect:= SelectRect;
        DrawRect.Intersect(FullRect);
        Canvas.DrawBitmap(FSCManager.Screenshot, DrawRect, DrawRect, 1.0, True);
      end;
  finally
    Canvas.EndScene;
  end;
end;


procedure TfrmScreenCapture.SelectionRectChange(Sender: TObject);
begin
  // Repaint overlay when selection changes
  if Assigned(pbOverlay) then
    pbOverlay.Repaint;
end;


procedure TfrmScreenCapture.UpdateInstructions;
VAR Msg: string;
begin
  Msg:= '1. Select a rectangle on your screen' + #13#10;

  {$IFDEF MSWINDOWS}
  Msg:= Msg + '2. Press Ctrl+P to capture the selected area' + #13#10; {$ENDIF}
  {$IFDEF MACOS}
  Msg:= Msg + '2. Press Cmd+P to capture the selected area' + #13#10; {$ENDIF}
  {$IF DEFINED(IOS) OR DEFINED(ANDROID)}
  Msg:= Msg + '2. Tap to capture the selected area' + #13#10; {$ENDIF}

  Msg:= Msg + '3. Press ESC when done';

  lblInstructions.Text:= Msg;
end;


function TfrmScreenCapture.IsCaptureKeyPressed(Key: Word; Shift: TShiftState): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result:= (Key = Ord('P')) AND (ssCtrl in Shift); {$ENDIF}
  {$IFDEF MACOS}
  Result:= (Key = Ord('P')) AND (ssCommand in Shift);  {$ENDIF}
  {$IF DEFINED(IOS) OR DEFINED(ANDROID)}
  Result:= FALSE; {$ENDIF}
end;


procedure TfrmScreenCapture.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  // Hide instructions after first keypress
  if layInstructions.Visible then
    begin
      FSCManager.CaptureTipShown:= FSCManager.CaptureTipShown+1;
      layInstructions.Visible:= FSCManager.CaptureTipShown < 3;
    end;

  // ESC - finish capturing (use ModalResult for modal forms)
  if Key = vkEscape then
    begin
      ModalResult:= mrOk;
      EXIT;
    end;

  // Ctrl+P / Cmd+P - capture selected area
  if IsCaptureKeyPressed(Key, Shift) then
    begin
      if FSCManager.CaptureSelectedArea(SelectionRect.BoundsRect) then
        begin
          ShowCaptureFlash;  // Visual feedback
          UpdateInstructions;
        end;
      Key:= 0;
    end;
end;


procedure TfrmScreenCapture.ShowCaptureFlash;
begin
  // Visual feedback: briefly highlight the selection
  SelectionRect.Opacity:= 0.3;
  Application.ProcessMessages;
  Sleep(100);
  SelectionRect.Opacity:= 1.0;
  Application.ProcessMessages;  
  Sleep(100);
  SelectionRect.Opacity:= 0.3;
  Application.ProcessMessages;
  Sleep(100);
  SelectionRect.Opacity:= 1.0;  
end;

end.


