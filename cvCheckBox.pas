UNIT cvCheckBox;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
  A checkbox that autoresizes exactly like TLabel
  It incercepts CMTextChanged where it recomputes the new Width

  Features:
      + property AutoSize ->  Autoresize width to fix the text inside

  Important:
    The control will NOT be automatically resized IF we call LoadForm(self) in FormCreate (canvas not ready)..
    We need to call LoadForm(self) in LateInitialize.

  Similar: https://stackoverflow.com/questions/9678029/automatically-resize-a-delphi-button

  Tester: c:\Myprojects\Project Testers\cubic VCL controls tester\
=============================================================================================================}

INTERFACE

{ $DEBUGINFO ON}

USES
  Winapi.Windows, Winapi.Messages, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.StdCtrls;

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
    // if HandleAllocated then exit
    // Don't use HandleAllocated. It will cause the control not to be updated when placed in the inactive tab of a PageControl. https://stackoverflow.com/questions/59107255/autoresizing-tcheckbox-like-tlabel?noredirect=1#comment104450251_59107255
    // We need a canvas but this control has none:
    // TPageControl initializes only the page that is currently selected.
    // It means that another pages will have no valid handle. Since this, all components that are placed on them have no handle as well. This is a reason for which AdjustBounds method does not work at all.
    // To fix it: get DeviceContext from DesktopWindow.
    // https://stackoverflow.com/questions/59107255/autoresizing-tcheckbox-like-tlabel
    Canvas := TCanvas.Create;
    DC     := GetDC(HWND_DESKTOP);
    TRY
      Canvas.Handle := DC;
      Canvas.Font   := Font;
      Width := Canvas.TextWidth(Caption) + SysCheckWidth + 4;
      Canvas.Handle := 0;
    FINALLY
      ReleaseDC(Handle, DC);
      Canvas.Free;
    END;
   end;

  FAutoSize:= TRUE;
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
  Invalidate;
  AdjustBounds;
  // I NEED TO CALL INHERITED? https://www.delphipraxis.net/100017-message-parameter-fuer-cmtextchanged.html
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
  RegisterComponents('LightSaber', [TCubicCheckBox]);
end;



end.

