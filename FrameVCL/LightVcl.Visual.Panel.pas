UNIT LightVcl.Visual.Panel;
{--------------------------------------------------------------------------------------------------
   2026.01

   Features:
      * Multi-line caption (word wrap). Caption:= '12345 67890';
      * Enumerate through child controls, by their position (top to bottom)

   From:
      https://stackoverflow.com/questions/1274518/make-a-delphi-tpanel-caption-wrap
--------------------------------------------------------------------------------------------------}

INTERFACE
{ $D-} { NoDebugInfo switch }

USES
  Winapi.Windows, System.SysUtils, Types, System.Classes, Vcl.ExtCtrls, Vcl.Controls, Vcl.Graphics, vcl.Themes;

TYPE
  TCubicPanel = class(TCustomPanel)
   private
    FWordWrap: Boolean;
    FGutter: Integer;
    FPrevCtrl: TControl;  // Used by NextControl enumeration
   protected
   public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    property DockManager;
    {}
    procedure ResetToFirstCtrl;
    function  FirstControl: TControl;
    function  NextControl: TControl;   { Returns the controls in phisycal order (sorted by .Top) }
    function  LastControl: TControl;
   published
    property WordWrap: Boolean read FWordWrap write FWordWrap default TRUE;
    property Gutter  : Integer read FGutter   write FGutter   default 0;     { Show red gutter on the right side and top. Used in c:\MyProjects\BioniX\MultiMonitor\Multimonitor env simulator }
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCaption;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property VerticalAlignment;
    property Visible;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

procedure Register;

IMPLEMENTATION




constructor TCubicPanel.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);  { Dont set 'Parent:= Owner' in constructor. See this for details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create }
 FWordWrap:= TRUE;
 FGutter:= 0;
 Alignment:= taLeftJustify;
 ControlStyle := ControlStyle - [csSetCaption]; // No caption
 //del Caption:= 'Split'+ CRLF+ 'caption';
end;








procedure TCubicPanel.Paint;
const
  Alignments: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
  VerticalAlignments: array[TVerticalAlignment] of Longint = (DT_TOP, DT_BOTTOM, DT_VCENTER);
var
  Rect: TRect;
  TopColor, BottomColor: TColor;
  Flags: Longint;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered
    then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered
    then BottomColor := clBtnHighlight;
  end;

begin
 if WordWrap
 then
  begin
   Rect := GetClientRect;
   if BevelOuter <> bvNone then
    begin
     AdjustColors(BevelOuter);
     Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
    end;

   if NOT (StyleServices.Enabled AND (csParentBackground in ControlStyle))
   then Frame3D(Canvas, Rect, Color, Color, BorderWidth)
   else InflateRect(Rect, -Integer(BorderWidth), -Integer(BorderWidth));

   if BevelInner <> bvNone then
    begin
     AdjustColors(BevelInner);
     Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
    end;

   if not StyleServices.Enabled or not ParentBackground then
    begin
     Canvas.Brush.Color := Color;
     Canvas.FillRect(Rect);
    end;

   if ShowCaption AND (Caption <> '') then
    begin
     Canvas.Brush.Style := bsClear;
     Canvas.Font := Self.Font;
     Flags := DT_NOPREFIX OR
              DT_WORDBREAK OR
              VerticalAlignments[VerticalAlignment] OR
              Alignments[Alignment];
     Flags := DrawTextBiDiModeFlags(Flags);
     DrawText(Canvas.Handle, PChar(Caption), Length(Caption), Rect, Flags);
    end;

  end
 else
    inherited;

 if FGutter> 0 then
  begin
   Canvas.Pen.Color:= clPurple;

   { Draw vertical line }
   Canvas.MoveTo(Gutter, 1);
   Canvas.LineTo(Gutter, Height-1);

   { Draw horizontal line }
   Canvas.MoveTo(1, Gutter);
   Canvas.LineTo(Width-1, Gutter);
  end;
end;

















{-------------------------------------------------------------------------------------------------------------
  Enumeration helpers: Returns child controls sorted by their vertical position (.Top).
  Usage:
    ResetToFirstCtrl;
    Ctrl:= NextControl;
    while Ctrl <> nil do begin
      // Process Ctrl
      Ctrl:= NextControl;
    end;
-------------------------------------------------------------------------------------------------------------}

{ Resets the control enumeration. Call before starting a new enumeration with NextControl. }
procedure TCubicPanel.ResetToFirstCtrl;
begin
  FPrevCtrl:= NIL;
end;


{ Returns controls in physical order (sorted by .Top).
  Returns NIL when there are no more controls.
  Note: The algorithm finds the next control whose top is greater than the previous one
  and that overlaps or is adjacent to the previous control's bottom edge. }
function TCubicPanel.NextControl: TControl;
VAR
   CurCtrl: TControl;
   i, BtmDist: Integer;
   LastBtmDist: Integer;
begin
 if ControlCount = 0
 then RAISE Exception.Create('Panel has no child controls!');

 Result:= NIL;
 LastBtmDist:= MaxInt;

 if FPrevCtrl = NIL
 then
  begin
   Result:= FirstControl;
   FPrevCtrl:= Result;
  end
 else
   for i:= 0 to ControlCount-1 DO
    begin
     CurCtrl:= Controls[i];

     { Don't compare to itself }
     if FPrevCtrl = CurCtrl
     then Continue;

     { CurCtrl is above FPrevCtrl }
     if CurCtrl.Top <= FPrevCtrl.Top
     then Continue;

     { Distance from bottom of previous control to top of current control }
     BtmDist:= (FPrevCtrl.Top + FPrevCtrl.Height) - CurCtrl.Top;

     if BtmDist < 0
     then Continue;

     if BtmDist < LastBtmDist then
      begin
       LastBtmDist:= BtmDist;
       Result:= CurCtrl;
       FPrevCtrl:= CurCtrl;
      end;
    end;
end;


{ Returns the topmost control (smallest .Top value). }
function TCubicPanel.FirstControl: TControl;
VAR
   LastTop, i: Integer;
begin
 Assert(FPrevCtrl = NIL, 'FirstControl: ResetToFirstCtrl was not called!');

 if ControlCount = 0
 then RAISE Exception.Create('Panel has no child controls!');

 Result:= Controls[0];
 LastTop:= MaxInt;

 for i:= 0 to ControlCount-1 DO
  if Controls[i].Top < LastTop then
    begin
     LastTop:= Controls[i].Top;
     Result:= Controls[i];
    end;
end;


{ Returns the bottommost control (largest .Top value). }
function TCubicPanel.LastControl: TControl;
VAR
   LastTop, i: Integer;
begin
 if ControlCount = 0
 then RAISE Exception.Create('Panel has no child controls!');

 Result:= Controls[0];
 LastTop:= 0;

 for i:= 0 to ControlCount-1 DO
    if Controls[i].Top > LastTop then
      begin
       LastTop:= Controls[i].Top;
       Result:= Controls[i];
      end;
end;





procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TCubicPanel]);
end;





end.
