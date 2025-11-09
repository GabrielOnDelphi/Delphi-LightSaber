UNIT AniImg;
{-------------------------------------------------------------------------------------------------------------

  TAnimateImage v1.01
  by Kambiz R. Khojasteh

  kambiz@delphiarea.com
  http://www.delphiarea.com
  18 Nov 2008
  TAnimateImage is a windowed control similar to TImage for displaying images of an image list on a form. You only need to assign a TImageList ...
-------------------------------------------------------------------------------------------------------------}

// used by MsgDispatcher

INTERFACE

USES
  WinApi.Messages, System.SysUtils, System.Classes, VCL.Controls, VCL.Graphics,
  VCL.ExtCtrls, VCL.ImgList, System.UITypes;

TYPE
  TAnimateImage = class(TGraphicControl)
  private
    FActive          : Boolean;
    FCenter          : Boolean;
    FDoubleBuffered  : Boolean;
    FFrameIndex      : System.UITypes.TImageIndex;
    FImageChangeLink : TChangeLink;
    FImages          : TCustomImageList;
    FLoopCount       : Integer;
    FNumLoops        : Integer;
    FOnFrame         : TNotifyEvent;
    FOnMouseEnter    : TNotifyEvent;
    FOnMouseLeave    : TNotifyEvent;
    FOnWrap          : TNotifyEvent;
    FReverse         : Boolean;
    FStartFrame      : System.UITypes.TImageIndex;
    FStopFrame       : System.UITypes.TImageIndex;
    FTimer           : TTimer;
    FTransparent     : Boolean;
    function  GetInterval: Integer;
    procedure CheckTimer;
    procedure CMMouseEnter    (var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave    (var Message: TMessage); message CM_MOUSELEAVE;
    procedure ImageListChange (Sender: TObject);
    procedure SetActive       (Value: Boolean);
    procedure SetCenter       (Value: Boolean);
    procedure SetFrameIndex   (Value: System.UITypes.TImageIndex);
    procedure SetImages       (Value: TCustomImageList);
    procedure SetInterval     (Value: Integer);
    procedure SetStartFrame   (Value: System.UITypes.TImageIndex);
    procedure SetStopFrame    (Value: System.UITypes.TImageIndex);
    procedure SetTransparent  (Value: Boolean);
    procedure TimerExpired    (Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function  CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure Loaded; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    LoopCount: Integer read FLoopCount default 0;
  published
    property Active         : Boolean read FActive write SetActive default FALSE;
    property Center         : Boolean read FCenter write SetCenter default TRUE;
    property Height default 100;
    property Width  default 100;
    property FrameIndex     : TImageIndex  read FFrameIndex write SetFrameIndex default 0;
    property DoubleBuffered : Boolean      read FDoubleBuffered write FDoubleBuffered default FALSE;
    property Interval       : Integer      read GetInterval write SetInterval default 250;
    property NumLoops       : Integer      read FNumLoops write FNumLoops default 0;
    property Reverse        : Boolean      read FReverse write FReverse default FALSE;
    property StartFrame     : TImageIndex  read FStartFrame write SetStartFrame default 0;
    property StopFrame      : TImageIndex  read FStopFrame write SetStopFrame default 0;
    property Transparent    : Boolean      read FTransparent write SetTransparent default True;
    property OnFrame        : TNotifyEvent read FOnFrame write FOnFrame;
    property Images         : TCustomImageList read FImages write SetImages;    
    property OnMouseEnter   : TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave   : TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnWrap         : TNotifyEvent read FOnWrap write FOnWrap;
  end;


procedure Register;

IMPLEMENTATION
USES LightVcl.Common.VclUtils;





{ TAnimateImage }
constructor TAnimateImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  SetBounds(Left, Top, 100, 100);

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange:= ImageListChange;

  FActive         := FALSE;
  FTimer          := TTimer.Create(NIL);
  FTimer.Enabled  := False;
  FTimer.Interval := 150;
  FTimer.OnTimer  := TimerExpired;

  FTransparent := True;
  FCenter := TRUE;
  FLoopCount:= 1;
end;

destructor TAnimateImage.Destroy;
begin
  { Stop timer }
  FActive       := FALSE;
  FTimer.Enabled:= FALSE;
  FreeAndNil(FTimer);

  FreeAndNil(FImageChangeLink);
  inherited Destroy;
end;


procedure TAnimateImage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    SetImages(nil);
end;


function TAnimateImage.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if FImages <> nil then
  begin
    if Align in [alNone, alLeft, alRight] then
      NewWidth := FImages.Width;
    if Align in [alNone, alTop, alBottom] then
      NewHeight := FImages.Height;
  end;
end;


procedure TAnimateImage.Loaded;
begin
  inherited Loaded;
  CheckTimer;
end;


procedure TAnimateImage.Paint;

  procedure PaintFrame(C: TCanvas);
  begin
    if FTransparent then
      LightVcl.Common.VclUtils.CopyParentImage(Self, C)
    else
    begin
      C.Brush.Color := Color;
      C.FillRect(ClientRect);
    end;
    if (FImages <> nil) and (FFrameIndex >= 0) then
    begin
      if FCenter then
        FImages.Draw(C, (Width - FImages.Width) div 2, (Height - FImages.Height) div 2, FFrameIndex)
      else
        FImages.Draw(C, 0, 0, FFrameIndex);
    end;
  end;

  procedure PaintDoubleBuffered;
  var
    B: TBitmap;
  begin
    B := TBitmap.Create;
    try
      B.Width := Width;
      B.Height := Height;
      PaintFrame(B.Canvas);
      Canvas.Draw(0, 0, B);
    finally
      B.Free;
    end;
  end;

begin
  if FDoubleBuffered then
    PaintDoubleBuffered
  else
    PaintFrame(Canvas);
  if csDesigning in ComponentState then
  begin
    Canvas.Pen.Style := psDash;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0, 0, Width, Height);
  end;
end;


procedure TAnimateImage.ImageListChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
  begin
    FStartFrame := 0;
    if (FImages <> nil) and (FImages.Count > 1) then
      FStopFrame := FImages.Count - 1
    else
      FStopFrame := 0;
    if FFrameIndex < FStartFrame then
      FFrameIndex := FStartFrame
    else if FFrameIndex > FStopFrame then
      FFrameIndex := FStopFrame;
    if AutoSize then AdjustSize;
    Invalidate;
    CheckTimer;
  end;
end;


procedure TAnimateImage.TimerExpired(Sender: TObject);
var
  NewFrameIndex: Integer;
  Wrapped: Boolean;
begin
  Wrapped := False;
  if FReverse then
  begin
    NewFrameIndex := FFrameIndex - 1;
    if NewFrameIndex < FStartFrame then
    begin
      NewFrameIndex := FStopFrame;
      Wrapped := True;
    end;
  end
  else
  begin
    NewFrameIndex := FFrameIndex + 1;
    if NewFrameIndex > FStopFrame then
    begin
      NewFrameIndex := FStartFrame;
      Wrapped := True;
    end;
  end;
  if Wrapped then
  begin
    Inc(FLoopCount);
    if Assigned(FOnWrap) then
      FOnWrap(Self);
    if (FNumLoops <> 0) and (FLoopCount >= FNumLoops) then
      SetActive(False);
  end;
  if FActive then
  begin
    SetFrameIndex(NewFrameIndex);
    if Assigned(FOnFrame) then
      FOnFrame(Self);
  end;
end;

procedure TAnimateImage.CheckTimer;
VAR WasEnabled: Boolean;
begin
  if NOT (csLoading in ComponentState) AND (FTimer<> NIL) then
  begin
    WasEnabled := FTimer.Enabled;
    FTimer.Enabled := FActive and (FStopFrame - StartFrame > 0);
    if FTimer.Enabled AND NOT WasEnabled then
     begin
       FLoopCount := 0;
       if FReverse then
         SetFrameIndex(FStopFrame)
       else
         SetFrameIndex(FStartFrame);
     end;
  end;
end;

procedure TAnimateImage.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TAnimateImage.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TAnimateImage.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    CheckTimer;
  end;
end;

procedure TAnimateImage.SetCenter(Value: Boolean);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;
    Invalidate;
  end;
end;

procedure TAnimateImage.SetFrameIndex(Value: TImageIndex);
begin
  if Value < -1 then Value := -1;
  if not (csLoading in ComponentState) then
  begin
    if (FFrameIndex <> Value) and ((Value < 0) or
       ((Value >= FStartFrame) and (Value <= FStopFrame))) then
    begin
      FFrameIndex := Value;
      Invalidate;
    end;
  end
  else
    FFrameIndex := Value;
end;

procedure TAnimateImage.SetImages(Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    if FImages <> nil then
      FImages.UnRegisterChanges(FImageChangeLink);
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(Self);
    end;
    ImageListChange(nil);
  end;
end;

procedure TAnimateImage.SetStartFrame(Value: TImageIndex);
begin
  if Value < 0 then Value := 0;
  if not (csLoading in ComponentState) then
  begin
    if (FStartFrame <> Value) and (Value <= FStopFrame) then
    begin
      FStartFrame := Value;
      if (FFrameIndex < FStartFrame) and (FFrameIndex >= 0) then
      begin
        FFrameIndex := FStartFrame;
        Invalidate;
      end;
      CheckTimer;
    end;
  end
  else
    FStartFrame := Value;
end;

procedure TAnimateImage.SetStopFrame(Value: TImageIndex);
begin
  if Value < 0 then Value := 0;
  if not (csLoading in ComponentState) then
  begin
    if (FStopFrame <> Value) and (Value >= FStartFrame) and
       (FImages <> nil) and (Value < FImages.Count) then
    begin
      FStopFrame := Value;
      if FFrameIndex > FStopFrame then
      begin
        FFrameIndex := FStopFrame;
        Invalidate;
      end;
      CheckTimer;
    end;
  end
  else
    FStopFrame := Value;
end;

procedure TAnimateImage.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

function TAnimateImage.GetInterval: Integer;
begin
  Result := FTimer.Interval;
end;

procedure TAnimateImage.SetInterval(Value: Integer);
begin
  FTimer.Interval := Value;
end;




(*  moved to: LightVcl.Common.VclUtils.pas

{ This procedure is copied from RxLibrary VCLUtils.
  It copies the background image of a control's parent, including any overlapping sibling graphic controls,
  onto a destination canvas—useful for rendering transparent or custom-painted controls in Delphi. }
TYPE
  TParentControl = class(TWinControl);

procedure CopyParentImage(Control: TControl; Dest: TCanvas);
var
  I, Count, X, Y, SaveIndex: Integer;
  DC: HDC;
  R, SelfR, CtlR: TRect;
begin
  if (Control = nil) OR (Control.Parent = nil)
  then Exit;

  Count := Control.Parent.ControlCount;
  DC    := Dest.Handle;
  with Control.Parent
   DO ControlState := ControlState + [csPaintCopy];

  TRY
    with Control do
     begin
      SelfR := Bounds(Left, Top, Width, Height);
      X := -Left; Y := -Top;
     end;

    { Copy parent control image }
    SaveIndex := SaveDC(DC);
    TRY
      SetViewportOrgEx(DC, X, Y, nil);
      IntersectClipRect(DC, 0, 0, Control.Parent.ClientWidth, Control.Parent.ClientHeight);
      with TParentControl(Control.Parent) DO
       begin
        Perform(WM_ERASEBKGND, wParam(DC), 0);         { see: http://stackoverflow.com/questions/4072974/range-check-error-while-painting-the-canvas }
        PaintWindow(DC);
       end;
    FINALLY
      RestoreDC(DC, SaveIndex);
    END;

    { Copy images of graphic controls }
    for I := 0 to Count - 1 do begin
      if Control.Parent.Controls[I] = Control then Break
      else if (Control.Parent.Controls[I] <> nil) and
        (Control.Parent.Controls[I] is TGraphicControl) then
      begin
        with TGraphicControl(Control.Parent.Controls[I]) do begin
          CtlR := Bounds(Left, Top, Width, Height);
          if Bool(IntersectRect(R, SelfR, CtlR)) and Visible then
          begin
            ControlState := ControlState + [csPaintCopy];
            SaveIndex := SaveDC(DC);
            try
              SetViewportOrgEx(DC, Left + X, Top + Y, nil);
              IntersectClipRect(DC, 0, 0, Width, Height);
              Perform(WM_PAINT, wParam(DC), 0);                            { see: http://stackoverflow.com/questions/4072974/range-check-error-while-painting-the-canvas }
            finally
              RestoreDC(DC, SaveIndex);
              ControlState := ControlState - [csPaintCopy];
            end;
          end;
        end;
      end;
    end;
  FINALLY
    with Control.Parent DO
     ControlState := ControlState - [csPaintCopy];
  end;
end;  *)




procedure Register;
begin
  RegisterComponents('3rd_party', [TAnimateImage]);
end;


end.
