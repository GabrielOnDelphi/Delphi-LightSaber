UNIT cvSplitter;
{
 In acest custom component pot sa setez MinSize la zero
 Necesar pt DNA Baser
}

{$S-,W-,R-,H+,X+}
{$DebugInfo OFF}

INTERFACE
USES
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Themes, Vcl.Forms, Vcl.Graphics, System.Types;


TYPE
  NaturalNumber = 1..High(Integer);

  TCanResizeEvent = procedure(Sender: TObject; var NewSize: Integer; var Accept: Boolean) of object;

  TResizeStyle = (rsNone, rsLine, rsUpdate, rsPattern);

  TCubicSplitter = class(TGraphicControl)
  private
    FActiveControl: TWinControl;
    FAutoSnap: Boolean;
    FBeveled: Boolean;
    FBrush  : TBrush;
    FControl: TControl;
    FDownPos: TPoint;
    FLineDC : HDC;
    FLineVisible: Boolean;
    FMinSize: NaturalNumber;
    //FMaxSize: Integer;
    FNewSize: Integer;
    FOldKeyDown: TKeyEvent;
    FOldSize: Integer;
    FPrevBrush: HBrush;
    FResizeStyle: TResizeStyle;
    FSplit: Integer;
    FOnCanResize: TCanResizeEvent;
    FOnMoved: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    procedure AllocateLineDC;
    procedure CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
    procedure DrawLine;
    function  FindControl: TControl;
    procedure FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ReleaseLineDC;
    procedure SetBeveled(Value: Boolean);
    procedure UpdateControlSize;
    procedure UpdateSize(X, Y: Integer);
  protected
    function  CanResize(var NewSize: Integer): Boolean; reintroduce;
    function  DoCanResize(var NewSize: Integer): Boolean; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure RequestAlign; override;
    procedure StopSizing; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property   Canvas;
  published
    property Align default alLeft;
    property AutoSnap: Boolean read FAutoSnap write FAutoSnap default True;
    property Beveled: Boolean read FBeveled write SetBeveled default False;
    property Color;
    property Cursor default crHSplit;
    property Constraints;
    property MinSize: NaturalNumber read FMinSize write FMinSize default 1;
    property ParentColor;
    property ResizeStyle: TResizeStyle read FResizeStyle write FResizeStyle default rsPattern;
    property Visible;
    property Width default 3;
    property OnCanResize: TCanResizeEvent read FOnCanResize write FOnCanResize;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  
procedure Register;

IMPLEMENTATION {$R *.res}








{ TCubicSplitter }

{$IF NOT DEFINED(CLR)}
type
  TWinControlAccess = class(TWinControl);
{$ENDIF}

constructor TCubicSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csGestures];
  FAutoSnap    := True;
  Height       := 100;
  Align        := alLeft;
  Width        := 3;
  Cursor       := crHSplit;
  FMinSize     := 1;                                                                                   { original aici era 30 }
  FResizeStyle := rsUpdate;
  FOldSize     := -1;
end;

destructor TCubicSplitter.Destroy;
begin
  FreeAndNil(FBrush);
  inherited Destroy;
end;







procedure TCubicSplitter.RequestAlign;
begin
  inherited RequestAlign;
  if (Cursor <> crVSplit) and (Cursor <> crHSplit) then Exit;
  if Align in [alBottom, alTop]
  then Cursor := crVSplit
  else Cursor := crHSplit;
end;


function TCubicSplitter.DoCanResize(var NewSize: Integer): Boolean;
begin
  Result := CanResize(NewSize);
  if Result and (NewSize <= MinSize) and FAutoSnap
  then NewSize := 0;
end;


function TCubicSplitter.CanResize(var NewSize: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCanResize)
  then FOnCanResize(Self, NewSize, Result);
end;



{===============================================================================
                                INPUT EVENTS
===============================================================================}
procedure TCubicSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//var I: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FControl := FindControl;
    FDownPos := Point(X, Y);
    if Assigned(FControl) then
    begin
      {
      if Align in [alLeft, alRight] then
      begin
        FMaxSize := Parent.ClientWidth - FMinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Visible and (Align in [alLeft, alRight]) then Dec(FMaxSize, Width);
        Inc(FMaxSize, FControl.Width);
      end
      else
       begin
        FMaxSize := Parent.ClientHeight- FMinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Align in [alTop, alBottom] then Dec(FMaxSize, Height);
        Inc(FMaxSize, FControl.Height);
       end;
       }
      UpdateSize(X, Y);
      AllocateLineDC;
      with ValidParentForm(Self) do
        if ActiveControl <> nil then
        begin
          FActiveControl := ActiveControl;
{$IF DEFINED(CLR)}
          FOldKeyDown := @FocusKeyDown;
          (FActiveControl as IControl).HookDelegate('OnKeyDown', @FOldKeyDown);
{$ELSE}
          FOldKeyDown := TWinControlAccess(FActiveControl).OnKeyDown;
          TWinControlAccess(FActiveControl).OnKeyDown := FocusKeyDown;
{$ENDIF}
        end;
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    end;
  end;
end;


procedure TCubicSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewSize, Split: Integer;
begin
  inherited;
  if (ssLeft in Shift) and Assigned(FControl) then
  begin
    CalcSplitSize(X, Y, NewSize, Split);
    if DoCanResize(NewSize) then
    begin
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
      FNewSize := NewSize;
      FSplit := Split;
      if ResizeStyle = rsUpdate then UpdateControlSize;
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    end;
  end;
end;


procedure TCubicSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState;  X, Y: Integer);
begin
  inherited;
  if Assigned(FControl) then
  begin
    if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    UpdateControlSize;
    StopSizing;
  end;
end;


procedure TCubicSplitter.FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    StopSizing
{$IF NOT DEFINED(CLR)}
  else if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
{$ENDIF}
end;



{===============================================================================

===============================================================================}

procedure TCubicSplitter.UpdateControlSize;
var
  OldPosition: Integer;
begin
  if FNewSize <> FOldSize then
  begin
    case Align of
      alLeft:
        begin
          OldPosition := FControl.Left;
          FControl.Width := FNewSize;
          FControl.Left := OldPosition;
        end;
      alTop:
        begin
          OldPosition := FControl.Top;
          FControl.Height := FNewSize;
          FControl.Top := OldPosition;
        end;
      alRight:
        begin
          Parent.DisableAlign;
          try
            FControl.Left := FControl.Left + (FControl.Width - FNewSize);
            FControl.Width := FNewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
      alBottom:
        begin
          Parent.DisableAlign;
          try
            FControl.Top := FControl.Top + (FControl.Height - FNewSize);
            FControl.Height := FNewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
    end;
    Update;
    if Assigned(FOnMoved) then FOnMoved(Self);
    FOldSize := FNewSize;
  end;
end;


procedure TCubicSplitter.CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
var
  S: Integer;
begin
  if Align in [alLeft, alRight] then
    Split := X - FDownPos.X
  else
    Split := Y - FDownPos.Y;
  S := 0;
  case Align of
    alLeft: S := FControl.Width + Split;
    alRight: S := FControl.Width - Split;
    alTop: S := FControl.Height + Split;
    alBottom: S := FControl.Height - Split;
  end;
  NewSize := S;
  if S < FMinSize then
    NewSize := FMinSize;
  {else if S > FMaxSize then NewSize := FMaxSize;}
  if S <> NewSize then
  begin
    if Align in [alRight, alBottom] then
      S := S - NewSize else
      S := NewSize - S;
    Inc(Split, S);
  end;
end;


procedure TCubicSplitter.UpdateSize(X, Y: Integer);
begin
  CalcSplitSize(X, Y, FNewSize, FSplit);
end;


procedure TCubicSplitter.SetBeveled(Value: Boolean);
begin
  FBeveled := Value;
  Repaint;
end;


procedure TCubicSplitter.StopSizing;
begin
  if Assigned(FControl) then
  begin
    if FLineVisible then DrawLine;
    FControl := nil;
    ReleaseLineDC;
{$IF DEFINED(CLR)}
    if Assigned(FActiveControl) and Assigned(FOldKeyDown) then
    begin
      (FActiveControl as IControl).UnhookDelegate('OnKeyDown', @FOldKeyDown);
      FActiveControl := nil;
      FOldKeyDown := nil;
    end;
{$ELSE}
    if Assigned(FActiveControl) then
    begin
      TWinControlAccess(FActiveControl).OnKeyDown := FOldKeyDown;
      FActiveControl := nil;
    end;
{$ENDIF}
  end;
  if Assigned(FOnMoved) then
    FOnMoved(Self);
end;


{===============================================================================
                                   PAINT
===============================================================================}
procedure TCubicSplitter.Paint;
const
  XorColor = $00FFD8CE;
var
  FrameBrush: HBRUSH;
  R: TRect;
begin
  R := ClientRect;
  if TStyleManager.IsCustomStyleActive then
    Canvas.Brush.Color := StyleServices.GetSystemColor(clBtnFace)
  else
    Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  if Beveled then
  begin
    if Align in [alLeft, alRight] then
      InflateRect(R, -1, 2) else
      InflateRect(R, 2, -1);
    OffsetRect(R, 1, 1);
    FrameBrush := CreateSolidBrush(ColorToRGB(StyleServices.GetSystemColor(clBtnHighlight)));
    FrameRect(Canvas.Handle, R, FrameBrush);
    DeleteObject(FrameBrush);
    OffsetRect(R, -2, -2);
    FrameBrush := CreateSolidBrush(ColorToRGB(StyleServices.GetSystemColor(clBtnShadow)));
    FrameRect(Canvas.Handle, R, FrameBrush);
    DeleteObject(FrameBrush);
  end;
  if csDesigning in ComponentState then
    { Draw outline }
    with Canvas do
    begin
      Pen.Style := psDot;
      Pen.Mode := pmXor;
      Pen.Color := XorColor;
      Brush.Style := bsClear;
      Rectangle(0, 0, ClientWidth, ClientHeight);
    end;
  if Assigned(FOnPaint) then FOnPaint(Self);
end;


procedure TCubicSplitter.AllocateLineDC;
begin
  FLineDC := GetDCEx(Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS
    or DCX_LOCKWINDOWUPDATE);
  if ResizeStyle = rsPattern then
  begin
    if FBrush = nil then
    begin
      FBrush := TBrush.Create;
      if TStyleManager.IsCustomStyleActive then
        with StyleServices do
          FBrush.Bitmap := AllocPatternBitmap(clBlack, GetStyleColor(scSplitter))
      else
        FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
    end;
    FPrevBrush := SelectObject(FLineDC, FBrush.Handle);
  end;
end;


procedure TCubicSplitter.DrawLine;
var
  P: TPoint;
begin
  FLineVisible := not FLineVisible;
  P := Point(Left, Top);
  if Align in [alLeft, alRight] then
    P.X := Left + FSplit else
    P.Y := Top + FSplit;
  with P do PatBlt(FLineDC, X, Y, Width, Height, PATINVERT);
end;


procedure TCubicSplitter.ReleaseLineDC;
begin
  if FPrevBrush <> 0 then
    SelectObject(FLineDC, FPrevBrush);
  ReleaseDC(Parent.Handle, FLineDC);
  if FBrush <> nil then
  begin
    FreeAndNil(FBrush);
  end;
end;


function TCubicSplitter.FindControl: TControl;
var
  P: TPoint;
  I: Integer;
  R: TRect;
begin
  Result := nil;
  P := Point(Left, Top);
  case Align of
    alLeft:
      if not AlignWithMargins then
        Dec(P.X)
      else
        Dec(P.X, Margins.Left + 1);
    alRight:
      if not AlignWithMargins then
        Inc(P.X, Width)
      else
        Inc(P.X, Width + Margins.Right + 1);
    alTop:
      if not AlignWithMargins then
        Dec(P.Y)
      else
        Dec(P.Y, Margins.Top + 1);
    alBottom:
      if not AlignWithMargins then
        Inc(P.Y, Height)
      else
        Inc(P.Y, Height + Margins.Bottom + 1);
  else
    Exit;
  end;

  for I := 0 to Parent.ControlCount - 1 do
  begin
    Result := Parent.Controls[I];
    if Result.Visible and Result.Enabled and
      (Result.Align in [alLeft, alRight, alTop, alBottom]) and
     ((Result.Align in [alLeft, alRight]) = (Align in [alLeft, alRight])) then
    begin
      R := Result.BoundsRect;
      if Result.AlignWithMargins then
      begin
        Inc(R.Right, Result.Margins.Right);
        Dec(R.Left, Result.Margins.Left);
        Inc(R.Bottom, Result.Margins.Bottom);
        Dec(R.Top, Result.Margins.Top);
      end;
      if (R.Right - R.Left) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Left)
        else
          Inc(R.Right);
      if (R.Bottom - R.Top) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Top)
        else
          Inc(R.Bottom);
      if R.Contains(P) then Exit;
    end;
  end;
  Result := nil;
end;


procedure Register;
begin
  RegisterComponents('LightSaber', [TCubicSplitter]);
end;


end.
