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

{  One limitation to be aware of: Loaded overwrites OnChange, so if someone sets OnChange on
  TAutoGrowMemo in the FMX file, it gets silently lost. Not an issue for the current memos (they only
  have OnKeyDown), but worth knowing for future USES. }

INTERFACE

USES
  System.Classes, System.SysUtils, System.Math, System.Types,
  FMX.Memo, FMX.Controls, FMX.TextLayout, FMX.Types;

type
  TAutoGrowMemo = class(TMemo)
  private
    FMinHeight      : Single;
    FMaxHeight      : Single;
    FInternalPadding: Single;
    FUpdating       : Boolean;
    function  ComputeNeededHeight: Single;
    procedure AdjustParentHeight;
    procedure OnChangeHandler(Sender: TObject);
  protected
    procedure Resize; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MinHeight       : Single read FMinHeight       write FMinHeight;
    property MaxHeight       : Single read FMaxHeight       write FMaxHeight;
    property InternalPadding : Single read FInternalPadding write FInternalPadding;
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
  OnChange := OnChangeHandler;
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
    Result := Layout.TextHeight + Margins.Top + Margins.Bottom + FInternalPadding;
  finally
    Layout.Free;
  end;

  Result := Max(FMinHeight, Min(FMaxHeight, Result));
end;


procedure TAutoGrowMemo.AdjustParentHeight;
begin
  if FUpdating OR NOT Assigned(Parent) then Exit;
  FUpdating := True;
  try
    (Parent as TControl).Height := ComputeNeededHeight;
  finally
    FUpdating := False;
  end;
end;


procedure TAutoGrowMemo.OnChangeHandler(Sender: TObject);
begin
  AdjustParentHeight;
end;


procedure TAutoGrowMemo.Resize;
begin
  inherited;
  AdjustParentHeight;  // width change → text reflows → re-measure
end;


procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TAutoGrowMemo]);
end;


end.
