UNIT LightVcl.Visual.RichEditResize;

{=============================================================================================================
   Gabriel Moraru
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------

  This component resizes itself as we add lines to it (instead of showing the vert scrollbars)

  Tester:
    C:\Projects\LightSaber\Demo\Tester All Visual Controls

  Also see:
    LightVcl.Visual.RichEdit.pas
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, Winapi.Messages,
  System.Classes, System.SysUtils,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Controls;


TYPE
  TRichEditResize = class(TRichEdit)
  private
    FMinHeight: Integer;
    procedure ResizeRichEdit;
  protected
    procedure CreateWnd; override;
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MinHeight: Integer read FMinHeight write FMinHeight;
  end;

procedure Register;



IMPLEMENTATION


constructor TRichEditResize.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMinHeight:= 50;
end;


{ Disables scrollbars after window creation since this control auto-resizes to fit content. }
procedure TRichEditResize.CreateWnd;
begin
  inherited CreateWnd;
  ScrollBars:= ssNone;
end;


procedure TRichEditResize.Change;
begin
  inherited;
  ResizeRichEdit;
end;


{ Calculates the required height based on text content and resizes both
  the control and its parent container accordingly. }
procedure TRichEditResize.ResizeRichEdit;
const
  PADDING = 5;
var
  LineHeight, ContentHeight, ParentHeight: Integer;
  DC: HDC;
  TextMetric: TTextMetric;
  LineCount: Integer;
begin
  if (csDesigning in ComponentState) then EXIT;
  Assert(Parent <> NIL, 'TRichEditResize.ResizeRichEdit: Parent not assigned');

  { Calculate single line height from font metrics }
  DC:= GetDC(Handle);
  try
    GetTextMetrics(DC, TextMetric);
    LineHeight:= TextMetric.tmHeight - TextMetric.tmInternalLeading;
  finally
    ReleaseDC(Handle, DC);
  end;

  { Calculate number of lines using EM_LINEFROMCHAR with MaxInt to get the last line index }
  LineCount:= Perform(EM_LINEFROMCHAR, WPARAM(MaxInt), 0) + 1;

  { Calculate content height, respecting minimum height }
  ContentHeight:= LineHeight * LineCount;
  if ContentHeight < FMinHeight
  then ContentHeight:= FMinHeight;

  { Warn about incompatible parent configuration }
  Assert(NOT ((Parent is TPanel) AND TPanel(Parent).AutoSize),
    'TRichEditResize: Parent panel cannot be set to AutoSize');

  { Set the control height to match content }
  Height:= ContentHeight;

  { Resize parent to accommodate control position plus padding }
  ParentHeight:= Top + ContentHeight + PADDING;
  Parent.Height:= ParentHeight;
end;


procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TRichEditResize]);
end;



end.
