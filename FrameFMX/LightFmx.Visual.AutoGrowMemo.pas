unit LightFmx.Visual.AutoGrowMemo;

{=============================================================================================================
   2026-04-24
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   FMX TMemo that auto-grows (and shrinks) its parent layout height as the user types.

   Usage:
     Drop TAutoGrowMemo where you would use TMemo. Set MinHeight/MaxHeight in Object Inspector.
     The parent TLayout (or TLightLayout, Align=Bottom) will track the content height.

   Properties:
     MinHeight        — parent height when empty or text fits in one line (design-time height)
     MaxHeight        — parent height cap; content scrolls beyond this point
     InternalPadding  — compensates for style/platform internal memo padding
                        Default (21) calibrated for FMX default style at ~14pt on Windows/Android.
                        Tune if the 1-line height looks wrong on the target platform/style.
=============================================================================================================}

{  Loaded chains its handler onto the existing OnChangeTracking — any FMX-designer-assigned
   OnChangeTracking is preserved (called after AdjustParentHeight). OnChange is left alone. }

INTERFACE

USES
  System.Classes, System.SysUtils, System.Math, System.Types,
  FMX.Memo, FMX.Controls, FMX.TextLayout, FMX.Types;

type
  TAutoGrowMemo = class(TMemo)
  private
    FMinHeight             : Single;
    FMaxHeight             : Single;
    FInternalPadding       : Single;
    FUpdating              : Boolean;
    FSavedOnChangeTracking : TNotifyEvent;
    function  ComputeNeededHeight: Single;
    procedure AdjustParentHeight;
    procedure OnChangeHandler(Sender: TObject);
    procedure SetMinHeight      (const Value: Single);
    procedure SetMaxHeight      (const Value: Single);
    procedure SetInternalPadding(const Value: Single);
  protected
    procedure Resize; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MinHeight       : Single read FMinHeight       write SetMinHeight;
    property MaxHeight       : Single read FMaxHeight       write SetMaxHeight;
    property InternalPadding : Single read FInternalPadding write SetInternalPadding;
  end;

procedure Register;

IMPLEMENTATION


constructor TAutoGrowMemo.Create(AOwner: TComponent);
begin
  inherited;
  FMinHeight       := 44;
  FMaxHeight       := 130;
  FInternalPadding := 21;
end;


procedure TAutoGrowMemo.Loaded;
begin
  inherited;
  // OnChangeTracking fires per-keystroke; OnChange only fires on focus-loss/Enter
  // and would mean the memo never grows while typing.
  FSavedOnChangeTracking:= OnChangeTracking;
  OnChangeTracking:= OnChangeHandler;
  AdjustParentHeight;
end;


function TAutoGrowMemo.ComputeNeededHeight: Single;
var
  Layout   : TTextLayout;
  LayoutText: string;
begin
  if (Width <= 1) OR (FMaxHeight <= 0) then
    Exit(FMinHeight);

  // Use 'W' when empty so layout returns proper single-line height (empty string gives 0)
  if Text = ''
  then LayoutText := 'W'
  else LayoutText := Text;

  Layout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    Layout.BeginUpdate;
    Layout.MaxSize    := TPointF.Create(Max(1, Width - Padding.Left - Padding.Right), FMaxHeight * 2);
    Layout.Text       := LayoutText;
    Layout.Font.Assign(Font);
    Layout.WordWrap   := True;
    Layout.HorizontalAlign := TTextAlign.Leading;
    Layout.VerticalAlign   := TTextAlign.Leading;
    Layout.EndUpdate;
    // Result is assigned to Parent.Height (see AdjustParentHeight). The parent slot must fit
    // the memo PLUS its Margins: FMX ArrangeControl sizes an aligned child to Parent.Height
    // minus the child's own Margins (FMX.Types.ArrangeControl -> PaddingRect). So the memo's
    // exterior Margins ARE part of the height the parent must provide.
    // The memo's own interior Padding is NOT added separately: FInternalPadding is calibrated
    // for the default Padding=0, and is the single knob meant to absorb interior style insets.
    Result := Layout.TextHeight + Margins.Top + Margins.Bottom + FInternalPadding;
  finally
    FreeAndNil(Layout);
  end;

  Result := Max(FMinHeight, Min(FMaxHeight, Result));
end;


procedure TAutoGrowMemo.AdjustParentHeight;
var
  ParentCtrl: TControl;
  Needed    : Single;
begin
  if FUpdating OR NOT Assigned(Parent) OR NOT (Parent is TControl) then Exit;
  ParentCtrl:= Parent as TControl;
  Needed:= ComputeNeededHeight;
  // Short-circuit when no actual change — kills the 2-pass feedback loop
  // (parent.Height set → realign → child Resize → AdjustParentHeight again).
  if SameValue(Needed, ParentCtrl.Height, 0.5) then Exit;
  FUpdating:= True;
  try
    ParentCtrl.Height:= Needed;
  finally
    FUpdating:= False;
  end;
end;


procedure TAutoGrowMemo.OnChangeHandler(Sender: TObject);
begin
  AdjustParentHeight;
  if Assigned(FSavedOnChangeTracking)
  then FSavedOnChangeTracking(Sender);
end;


procedure TAutoGrowMemo.Resize;
begin
  inherited;
  AdjustParentHeight;  // width change → text reflows → re-measure
end;


{ Re-measure when a sizing property changes at run or design time.
  Skipped while csLoading: during streaming the setters fire before Loaded,
  and Loaded does the first AdjustParentHeight once the form is fully built. }
procedure TAutoGrowMemo.SetMinHeight(const Value: Single);
begin
  if SameValue(FMinHeight, Value) then Exit;
  FMinHeight := Value;
  if not (csLoading in ComponentState) then AdjustParentHeight;
end;


procedure TAutoGrowMemo.SetMaxHeight(const Value: Single);
begin
  if SameValue(FMaxHeight, Value) then Exit;
  FMaxHeight := Value;
  if not (csLoading in ComponentState) then AdjustParentHeight;
end;


procedure TAutoGrowMemo.SetInternalPadding(const Value: Single);
begin
  if SameValue(FInternalPadding, Value) then Exit;
  FInternalPadding := Value;
  if not (csLoading in ComponentState) then AdjustParentHeight;
end;


procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TAutoGrowMemo]);
end;


end.
