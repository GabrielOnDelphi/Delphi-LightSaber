UNIT LightVcl.Visual.CheckBox;

{=============================================================================================================
   Gabriel Moraru
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
  A checkbox that autoresizes exactly like TLabel
  It intercepts CMTextChanged where it recomputes the new Width

  Features:
      + property AutoSize ->  Autoresize width to fix the text inside

  Important:
    The control will NOT be automatically resized IF we call LoadForm(self) in FormCreate (canvas not ready)..
    We need to call LoadForm(self) in LateInitialize.

  Similar: https://stackoverflow.com/questions/9678029/automatically-resize-a-delphi-button

  Tester: c:\Myprojects\Project Testers\cubic VCL controls tester\

  Limitations:
    SysCheckWidth is hardcoded to 21 pixels. On high-DPI displays, this may need adjustment.
=============================================================================================================}

INTERFACE

{ $DEBUGINFO ON}

USES
  Winapi.Windows, Winapi.Messages,
  System.Classes, System.SysUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.StdCtrls;

TYPE
  TCubicCheckBox = class(TCheckBox)
  private
    FAutoSize: Boolean;
    procedure AdjustBounds;
    procedure setAutoSize(b: Boolean);  reintroduce;  // Don't overload. We want to hide original method. http://docwiki.embarcadero.com/RADStudio/Sydney/en/Methods_(Delphi)
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
     procedure Loaded; override;
  public
     constructor Create(AOwner: TComponent); override;
  published
     property AutoSize: Boolean read FAutoSize write setAutoSize stored TRUE;
  end;


procedure Register;

IMPLEMENTATION

CONST
  SysCheckWidth: Integer = 21;  // In theory this should be obtained from the "system"



constructor TCubicCheckBox.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FAutoSize:= FALSE;   { This must be false, otherwise AdjustBounds will be executed when we drop the control on a form and we don't have a Parent at that point. }
end;


procedure TCubicCheckBox.AdjustBounds;
VAR
   DC: HDC;
   Canvas: TCanvas;
begin
 if NOT (csReading in ComponentState) AND FAutoSize then
   begin
    // Don't use HandleAllocated. It will cause the control not to be updated when placed in the inactive tab of a PageControl.
    // https://stackoverflow.com/questions/59107255/autoresizing-tcheckbox-like-tlabel?noredirect=1#comment104450251_59107255
    // We need a canvas but this control has none:
    // TPageControl initializes only the page that is currently selected.
    // Other pages have no valid handle, so components placed on them have no handle either.
    // To fix: get DeviceContext from DesktopWindow.
    // https://stackoverflow.com/questions/59107255/autoresizing-tcheckbox-like-tlabel
    Canvas:= TCanvas.Create;
    DC:= GetDC(HWND_DESKTOP);
    TRY
      Canvas.Handle:= DC;
      Canvas.Font:= Font;
      Width:= Canvas.TextWidth(Caption) + SysCheckWidth + 4;
      Canvas.Handle:= 0;  { Detach handle before releasing DC }
    FINALLY
      ReleaseDC(HWND_DESKTOP, DC);  { Must release to the same window we got it from }
      FreeAndNil(Canvas);
    END;
   end;
end;


procedure TCubicCheckBox.setAutoSize(b: Boolean);
begin
  if FAutoSize <> b then
  begin
    FAutoSize := b;
    if b then AdjustBounds;
  end;
end;


procedure TCubicCheckBox.CMTextChanged(var Message:TMessage);
begin
  inherited;  { Call inherited to ensure proper base class behavior }
  Invalidate;
  AdjustBounds;
end;


procedure TCubicCheckBox.CMFontChanged(var Message:TMessage);
begin
  inherited;
  if AutoSize
  then AdjustBounds;
end;


procedure TCubicCheckBox.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;


procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TCubicCheckBox]);
end;



end.

