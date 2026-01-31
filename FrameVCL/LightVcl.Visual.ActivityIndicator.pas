UNIT LightVcl.Visual.ActivityIndicator;

{=============================================================================================================
   Gabriel Moraru
   2026-01-31
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------

   Features:
     Custom activity indicator that displays a triangle (play button style) when not animating.
     When Animate=True, displays the standard spinning dots animation.
     When Animate=False, draws a red triangle glyph as a visual placeholder.

   Usage:
     Drop on a form. Set Animate:=True to show spinning animation.
     The triangle glyph appears when animation is stopped.

   Important:
     Do NOT set 'Parent:= Owner' in the constructor.
     See: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create

   External dependencies:
     LightVcl.Graph.Util - for DrawTriangle function
=============================================================================================================}

INTERFACE

USES
  System.Classes, Vcl.Controls, Vcl.Graphics, Vcl.WinXCtrls;

TYPE
  { TActivityIndicatorC - Custom activity indicator with triangle glyph when stopped.
    Inherits from TActivityIndicator and overrides DrawFrame to show a triangle
    when not animating, providing visual feedback that animation is available. }
  TActivityIndicatorC = class(TActivityIndicator)
   protected
    { Overrides parent drawing to show triangle when Animate=False }
    procedure DrawFrame; override;
   published
    { These properties are already published in TActivityIndicator.
      Re-declared here for explicit visibility in Object Inspector. }
    property Align;
    property Visible;
    property OnClick;
  end;

procedure Register;

IMPLEMENTATION

USES LightVcl.Graph.Util;


{ Draws the activity indicator frame.
  When animating: delegates to parent's spinning dots animation.
  When stopped: draws a red triangle glyph as a visual placeholder,
  similar to a "play" button to indicate animation can be started. }
procedure TActivityIndicatorC.DrawFrame;
begin
 if Animate
 then inherited DrawFrame
 else
  if HandleAllocated then
   begin
     Canvas.Pen.Style  := psSolid;
     Canvas.Brush.Style:= bsClear;
     Canvas.Pen.Color  := clRed;
     LightVcl.Graph.Util.DrawTriangle(Canvas, ClientWidth, ClientHeight, 8);
   end
end;


procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TActivityIndicatorC]);
end;

end.
