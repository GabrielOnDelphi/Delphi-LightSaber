UNIT cvActivityIndicator;

{--------------------------------------------------------------------------------------------------
   2020-11-28

   Features:

   Dont set 'Parent:= Owner' in constructor. See this for details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
--------------------------------------------------------------------------------------------------}

INTERFACE

USES
  Winapi.Windows, System.SysUtils, System.Types, System.Classes, Vcl.ExtCtrls, Vcl.Controls, Vcl.Graphics, vcl.Themes, Vcl.WinXCtrls;

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

USES cGraphUtil;


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
     cGraphUtil.DrawTriangle(Canvas, ClientWidth, ClientHeight, 8);
   end
end;


procedure Register;
begin
  RegisterComponents('LightSaber', [TActivityIndicatorC]);
end;








end.
