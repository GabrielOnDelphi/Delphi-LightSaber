UNIT LightVcl.Visual.ActivityIndicator;

{--------------------------------------------------------------------------------------------------
   2020-11-28

   Features:

   Dont set 'Parent:= Owner' in constructor. See this for details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
--------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.Classes, Vcl.Controls, Vcl.Graphics, Vcl.WinXCtrls;

TYPE
  TActivityIndicatorC = class(TActivityIndicator)
   private
   protected
    procedure DrawFrame; override;
   public
   published
    property Align;
    property Visible;
    property OnClick;
  end;

procedure Register;

IMPLEMENTATION

USES LightVcl.Graph.Util;


procedure TActivityIndicatorC.DrawFrame;
begin
 if  Animate
 then inherited DrawFrame
 else
  if HandleAllocated then
   begin
     Canvas.Pen.Style   := psSolid;
     Canvas.Brush.Style := bsClear;
     Canvas.Pen.Color   := clRed;
     LightVcl.Graph.Util.DrawTriangle(Canvas, ClientWidth, ClientHeight, 8);
   end
end;


procedure Register;
begin
  RegisterComponents('LightSaber', [TActivityIndicatorC]);
end;








end.
