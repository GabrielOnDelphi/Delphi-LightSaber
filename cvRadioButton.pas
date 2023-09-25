UNIT cvRadioButton;

{--------------------------------------------------------------------------------------------------
  CubicDesign
  2019-07-10
  A radio button that autoresizes exactly like TLabel
  It incercepts CMTextChanged where it recomputes the new Width

  Features:
      + property AutoSize -> Autoresize width to fix the text inside

  Important:
    The control will NOT be automatically resized IF you call LoadForm(self) in FormCreate (canvas not ready)..
    You need to call LoadForm(self) in LateInitialize.

  Similar: https://stackoverflow.com/questions/9678029/automatically-resize-a-delphi-button

  Tester: c:\Myprojects\Project Testers\Cubic VCL tester GLOBAL\
  
  
  
  
  
  
  
  Issue: 
  https://stackoverflow.com/questions/47476603/major-flaw-radio-buttons-are-randomly-checked-when-showing-their-parent-form
  
--------------------------------------------------------------------------------------------------}







// issue: Set a huge font: it will not not resize its height to match the font height









INTERFACE

{$DEBUGINFO ON}

USES
  Winapi.Windows, Winapi.Messages, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.StdCtrls;

TYPE
  TCubicRadioButton = class(TRadioButton)
  private
    FAutoSize: Boolean;
    procedure AdjustBounds;
    procedure setAutoSize(b: Boolean);  reintroduce;
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
  SysCheckWidth: Integer = 20;  // In theory this should be obtained from the "system"



constructor TCubicRadioButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FAutoSize:= FALSE;   { This must be false, otherwise AdjustBounds will be executed when we drop the control on a form and we don't have a Parent at that point. }
end;










procedure TCubicRadioButton.AdjustBounds;
VAR
   DC: HDC;
   Canvas: TCanvas;
begin
 if not (csReading in ComponentState) and FAutoSize then
   begin
    //if HandleAllocated then exit;    // if HandleAllocated then  // Remove this line. It will cause the control not to be updated when placed in the inactive tab of a PageControl. https://stackoverflow.com/questions/59107255/autoresizing-tcheckbox-like-tlabel?noredirect=1#comment104450251_59107255
    // We need a canvas but this control has none. So we need to "produce" one.
    Canvas := TCanvas.Create;
    DC     := GetDC(Handle);
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


procedure TCubicRadioButton.setAutoSize(b: Boolean);
begin
  if FAutoSize <> b then
  begin
    FAutoSize := b;
    if b then AdjustBounds;
  end;
end;


procedure TCubicRadioButton.CMTextChanged(var Message:TMessage);
begin
  Invalidate;
  AdjustBounds;
end;


procedure TCubicRadioButton.CMFontChanged(var Message:TMessage);
begin
  inherited;
  if AutoSize
  then AdjustBounds;
end;


procedure TCubicRadioButton.Loaded;
begin
  inherited Loaded; 
  AdjustBounds;
end;


procedure Register;
begin
  RegisterComponents('Cubic', [TCubicRadioButton]);
end;


end.


