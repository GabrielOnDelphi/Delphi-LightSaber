UNIT VclUtilsExt;

{=============================================================================================================
   2026.04
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Utility functions for VCL components.
=============================================================================================================}

INTERFACE

USES
   Winapi.Windows,  Winapi.Messages,
   System.TypInfo, System.Classes, System.SysUtils,
   Vcl.StdCtrls, Vcl.Menus, Vcl.ActnList, Vcl.ComCtrls, Vcl.Controls, Vcl.Forms, Vcl.Graphics;

procedure CopyParentImage(Control: TControl; Dest: TCanvas);

IMPLEMENTATION



{ This procedure is copied from RxLibrary VCLUtils.
  It copies the background image of a control's parent, including any overlapping sibling graphic controls, onto a destination canvas?useful for rendering transparent or custom-painted controls in Delphi. }
TYPE
   TParentControl = class(TWinControl);

procedure CopyParentImage(Control: TControl; Dest: TCanvas);
var
  I, Count, X, Y, SaveIndex: Integer;
  DC: HDC;
  R, SelfR, CtlR: TRect;
  GraphicCtrl: TGraphicControl;
begin
  if Control = nil
  then raise Exception.Create('CopyParentImage: Control parameter cannot be nil');

  if Control.Parent = nil
  then EXIT;

  Count := Control.Parent.ControlCount;
  DC    := Dest.Handle;
  Control.Parent.ControlState := Control.Parent.ControlState + [csPaintCopy];

  TRY
    SelfR := Bounds(Control.Left, Control.Top, Control.Width, Control.Height);
    X := -Control.Left;
    Y := -Control.Top;

    { Copy parent control image }
    SaveIndex := SaveDC(DC);
    TRY
      SetViewportOrgEx(DC, X, Y, nil);
      IntersectClipRect(DC, 0, 0, Control.Parent.ClientWidth, Control.Parent.ClientHeight);
      TParentControl(Control.Parent).Perform(WM_ERASEBKGND, wParam(DC), 0);  { see: http://stackoverflow.com/questions/4072974/range-check-error-while-painting-the-canvas }
      TParentControl(Control.Parent).PaintWindow(DC);
    FINALLY
      RestoreDC(DC, SaveIndex);
    END;

    { Copy images of graphic controls }
    for I := 0 to Count - 1 do
    begin
      if Control.Parent.Controls[I] = Control
      then Break
      else
        if (Control.Parent.Controls[I] <> nil)
        AND (Control.Parent.Controls[I] is TGraphicControl) then
        begin
          GraphicCtrl := TGraphicControl(Control.Parent.Controls[I]);
          CtlR := Bounds(GraphicCtrl.Left, GraphicCtrl.Top, GraphicCtrl.Width, GraphicCtrl.Height);
          if Bool(IntersectRect(R, SelfR, CtlR)) AND GraphicCtrl.Visible then
          begin
            GraphicCtrl.ControlState := GraphicCtrl.ControlState + [csPaintCopy];
            SaveIndex := SaveDC(DC);
            TRY
              SetViewportOrgEx(DC, GraphicCtrl.Left + X, GraphicCtrl.Top + Y, nil);
              IntersectClipRect(DC, 0, 0, GraphicCtrl.Width, GraphicCtrl.Height);
              GraphicCtrl.Perform(WM_PAINT, wParam(DC), 0);  { see: http://stackoverflow.com/questions/4072974/range-check-error-while-painting-the-canvas }
            FINALLY
              RestoreDC(DC, SaveIndex);
              GraphicCtrl.ControlState := GraphicCtrl.ControlState - [csPaintCopy];
            END;
          end;
        end;
    end;
  FINALLY
    Control.Parent.ControlState := Control.Parent.ControlState - [csPaintCopy];
  END;
end;

end.
