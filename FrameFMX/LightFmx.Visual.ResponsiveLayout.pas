UNIT LightFmx.Visual.ResponsiveLayout;

{=============================================================================================================
   2026.03
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Responsive layout components for FMX.

   TLightCenteredLayout
     A layout that centers its content horizontally within the parent, capped at MaxWidth pixels.
     When the parent is narrower than MaxWidth, the layout shrinks to fit.
     Auto-sizes its height to fit children (when Align is Top/Bottom/MostTop/MostBottom).
     LabelWidth and RowHeight propagate to all child TResponsiveRowLayout controls.
     Children can be any control — centering, max-width capping, and auto-height apply to all.
     Only LabelWidth/RowHeight propagation is specific to TResponsiveRowLayout children.
     Recommended: set Align=Top. Place inside TVertScrollBox if content may exceed the form height.
     User Margins are fully available (centering is handled via SetBounds, not Margins).

   TResponsiveRowLayout
     Arranges a label + control pair:
       * Side-by-side when there is room for LabelWidth + Gap + Control.Width,
       * Stacked vertically (label on top) when narrower.
     BreakWidth: optional minimum threshold for side-by-side (0 = auto from content).
     Auto-sizes its own Height based on current arrangement (grows when stacked).
     Setting Height externally (e.g. in Object Inspector) syncs to RowHeight.
     Respects Padding (children are inset accordingly).
     Exactly 2 children. First child = label (Align=None). Second child = control.
       Ctrl.Align = Right → right-aligned in its row. Client → fills remaining width. None → after label.

   Example hierarchy:

     TVertScrollBox (Align=Client)                            <-- optional, for scrolling
       +-- TLightCenteredLayout                    (Align=Top, MaxWidth=500)
             +-- TResponsiveRowLayout            (Align=Top)
             |     +-- TLabel                    (Align=None)
             |     +-- TSpinBox                  (Align=None)
             +-- TResponsiveRowLayout            (Align=Top)
                   +-- TLabel                    (Align=None)
                   +-- TSpinBox                  (Align=None)
=============================================================================================================}

INTERFACE

USES
  System.Classes, System.Math,
  FMX.Types, FMX.Controls, FMX.Layouts;

TYPE
  TLightCenteredLayout = class(TLayout)
  private
    FMaxWidth: Single;
    FLabelWidth: Single;
    FRowHeight: Single;
    FRealigning: Boolean;
    procedure SetMaxWidth(const Value: Single);
    procedure SetLabelWidth(const Value: Single);
    procedure SetRowHeight(const Value: Single);
  protected
    procedure DoRealign; override;
  public
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
    constructor Create(AOwner: TComponent); override;
  published
    property MaxWidth:   Single read FMaxWidth   write SetMaxWidth;
    property LabelWidth: Single read FLabelWidth write SetLabelWidth;
    property RowHeight:  Single read FRowHeight  write SetRowHeight;
  end;


  TResponsiveRowLayout = class(TLayout)
  private
    FBreakWidth: Single;
    FLabelWidth: Single;
    FRowHeight: Single;
    FGap: Single;
    FRealigning: Boolean;
    FExpectedHeight: Single;  { Height last computed by DoRealign — used to detect user changes in SetBounds }
    procedure SetBreakWidth(const Value: Single);
    procedure SetLabelWidth(const Value: Single);
    procedure SetRowHeight(const Value: Single);
    procedure SetGap(const Value: Single);
  protected
    procedure DoRealign; override;
  public
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
    constructor Create(AOwner: TComponent); override;
  published
    property BreakWidth: Single read FBreakWidth write SetBreakWidth;
    property LabelWidth: Single read FLabelWidth write SetLabelWidth;
    property RowHeight:  Single read FRowHeight  write SetRowHeight;
    property Gap:        Single read FGap        write SetGap;
  end;

procedure Register;

IMPLEMENTATION


{-------------------------------------------------------------------------------------------------------------
   TLightCenteredLayout
-------------------------------------------------------------------------------------------------------------}

constructor TLightCenteredLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaxWidth:= 400;
  FLabelWidth:= 120;
  FRowHeight:= 28;
  Align:= TAlignLayout.Top;
end;


procedure TLightCenteredLayout.SetMaxWidth(const Value: Single);
begin
  if FMaxWidth <> Value then
  begin
    FMaxWidth:= Max(0, Value);
    Realign;
  end;
end;


procedure TLightCenteredLayout.SetLabelWidth(const Value: Single);
begin
  if FLabelWidth <> Value then
  begin
    FLabelWidth:= Max(0, Value);
    Realign;
  end;
end;


procedure TLightCenteredLayout.SetRowHeight(const Value: Single);
begin
  if FRowHeight <> Value then
  begin
    FRowHeight:= Max(1, Value);
    Realign;
  end;
end;


{ Intercepts positioning by the parent's alignment engine.
  When Align is Top/Bottom, the parent passes the full available width.
  We cap it to MaxWidth and shift X to center ourselves. }
procedure TLightCenteredLayout.SetBounds(X, Y, AWidth, AHeight: Single);
var
  DesiredW, NewX: Single;
begin
  if (FMaxWidth > 0) and (Align in [TAlignLayout.Top, TAlignLayout.Bottom,
                                     TAlignLayout.MostTop, TAlignLayout.MostBottom]) then
  begin
    DesiredW:= Min(FMaxWidth, AWidth);
    NewX:= X + (AWidth - DesiredW) / 2;
    inherited SetBounds(NewX, Y, DesiredW, AHeight);
  end
  else
    inherited SetBounds(X, Y, AWidth, AHeight);
end;


procedure TLightCenteredLayout.DoRealign;
var
  NewH, ChildBottom: Single;
  i: Integer;
begin
  if FRealigning then EXIT;
  FRealigning:= True;
  try
    { Propagate properties directly — setters would trigger Realign on each row.
      We force child re-layout explicitly after inherited (see below). }
    for i:= 0 to ControlsCount - 1 do
      if Controls[i] is TResponsiveRowLayout then
      begin
        TResponsiveRowLayout(Controls[i]).FLabelWidth:= FLabelWidth;
        TResponsiveRowLayout(Controls[i]).FRowHeight:= FRowHeight;
      end;

    inherited;

    { Force child rows to re-layout — inherited only triggers DoRealign on children
      whose bounds changed; property-only changes (LabelWidth, RowHeight) need
      an explicit nudge so they take effect at design time. }
    for i:= 0 to ControlsCount - 1 do
      if Controls[i] is TResponsiveRowLayout then
        TResponsiveRowLayout(Controls[i]).Realign;

    { Auto-size height to fit aligned children only. None-aligned controls are
      "floating" and don't participate — including them causes progressive drift
      because the layout doesn't manage their position. }
    if Align in [TAlignLayout.Top, TAlignLayout.Bottom,
                 TAlignLayout.MostTop, TAlignLayout.MostBottom] then
    begin
      NewH:= 0;
      for i:= 0 to ControlsCount - 1 do
        if Controls[i].Visible and (Controls[i].Align <> TAlignLayout.None) then
        begin
          ChildBottom:= Controls[i].Position.Y + Controls[i].Height + Controls[i].Margins.Bottom;
          if ChildBottom > NewH
          then NewH:= ChildBottom;
        end;
      NewH:= NewH + Padding.Bottom;

      if (NewH > 0) and (Abs(Height - NewH) > 0.1)
      then Height:= NewH;
    end;
  finally
    FRealigning:= False;
  end;
end;


{-------------------------------------------------------------------------------------------------------------
   TResponsiveRowLayout

   Place exactly two children inside: first = label, second = input control.
   Label must have Align = None (we manage its width).
   Control: Align = None/Left (after label), Right (right edge), Client (fills remaining width).

   When enough room:   [Label |Gap| Control    ]   (side by side, control at own width)
   When too narrow:    [Label.......................]   (stacked)
                       [Control    ]
-------------------------------------------------------------------------------------------------------------}

constructor TResponsiveRowLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBreakWidth     := 0;  { 0 = auto-calculate from LabelWidth + Gap + Ctrl.Width }
  FLabelWidth     := 200;
  FRowHeight      := 28;
  FGap            := 8;
  FExpectedHeight := FRowHeight;
  Height          := FRowHeight;
  Align           := TAlignLayout.Top;
end;


procedure TResponsiveRowLayout.SetBreakWidth(const Value: Single);
begin
  if FBreakWidth <> Value then
  begin
    FBreakWidth:= Max(0, Value);
    Realign;
  end;
end;


procedure TResponsiveRowLayout.SetLabelWidth(const Value: Single);
begin
  if FLabelWidth <> Value then
  begin
    FLabelWidth:= Max(0, Value);
    Realign;
  end;
end;


procedure TResponsiveRowLayout.SetRowHeight(const Value: Single);
begin
  if FRowHeight <> Value then
  begin
    FRowHeight:= Max(1, Value);
    Realign;
  end;
end;


procedure TResponsiveRowLayout.SetGap(const Value: Single);
begin
  if FGap <> Value then
  begin
    FGap:= Max(0, Value);
    Realign;
  end;
end;


{ When Height is changed externally (user in Object Inspector, code),
  sync it to RowHeight. We compare AHeight against FExpectedHeight (the last
  value DoRealign computed) rather than against current Height, because FMX
  updates Size.Height before calling SetBounds. }
procedure TResponsiveRowLayout.SetBounds(X, Y, AWidth, AHeight: Single);
var
  NewRowH: Single;
begin
  if (NOT FRealigning) and (Abs(AHeight - FExpectedHeight) > 0.1) then
  begin
    NewRowH:= Max(1, AHeight - Padding.Top - Padding.Bottom);
    if Abs(FRowHeight - NewRowH) > 0.1
    then FRowHeight:= NewRowH;  { Direct field access — no extra Realign }
  end;
  inherited SetBounds(X, Y, AWidth, AHeight);
end;


procedure TResponsiveRowLayout.DoRealign;
var
  Lbl, Ctrl: TControl;
  NewH, AvailW, StartX, StartY: Single;
  SideBySideOK: Boolean;
begin
  if FRealigning then EXIT;
  FRealigning:= True;
  try
    inherited;

    if ControlsCount < 2 then
    begin
      { No children pair — reset to single-row height }
      FExpectedHeight:= FRowHeight;
      if Abs(Height - FRowHeight) > 0.1
      then Height:= FRowHeight;
      EXIT;
    end;

    Lbl:=  Controls[0];
    Ctrl:= Controls[1];

    StartX:= Padding.Left;
    StartY:= Padding.Top;
    AvailW:= Max(0, Width - Padding.Left - Padding.Right);

    { BreakWidth > 0: use as explicit threshold. BreakWidth = 0: auto from content.
      Client-aligned controls fill remaining space, so only require room for label + gap. }
    if FBreakWidth > 0
    then SideBySideOK:= AvailW >= FBreakWidth
    else if Ctrl.Align = TAlignLayout.Client
    then SideBySideOK:= AvailW >= FLabelWidth + FGap
    else SideBySideOK:= AvailW >= FLabelWidth + FGap + Ctrl.Width;

    { Note: inherited (AlignObjects) already processed Ctrl based on its Align
      (Right → repositioned, Client → width stretched). Our SetBounds calls below
      override those positions/sizes, which is safe because all FMX alignment
      goes through DoRealign, which we control. }
    if SideBySideOK then
    begin
      { Side by side: label at fixed width, control in remaining space }
      Lbl.SetBounds(StartX, StartY, FLabelWidth, FRowHeight);
      if Ctrl.Align = TAlignLayout.Right
      then Ctrl.SetBounds(StartX + AvailW - Ctrl.Width, StartY, Ctrl.Width, FRowHeight)
      else if Ctrl.Align = TAlignLayout.Client
      then Ctrl.SetBounds(StartX + FLabelWidth + FGap, StartY, AvailW - FLabelWidth - FGap, FRowHeight)
      else Ctrl.SetBounds(StartX + FLabelWidth + FGap, StartY, Ctrl.Width, FRowHeight);
      NewH:= StartY + FRowHeight + Padding.Bottom;
    end
    else
    begin
      { Stacked: label on top, control below }
      Lbl.SetBounds(StartX, StartY, AvailW, FRowHeight);
      if Ctrl.Align = TAlignLayout.Right
      then Ctrl.SetBounds(StartX + AvailW - Ctrl.Width, StartY + FRowHeight + FGap, Ctrl.Width, FRowHeight)
      else if Ctrl.Align = TAlignLayout.Client
      then Ctrl.SetBounds(StartX, StartY + FRowHeight + FGap, AvailW, FRowHeight)
      else Ctrl.SetBounds(StartX, StartY + FRowHeight + FGap, Ctrl.Width, FRowHeight);
      NewH:= StartY + FRowHeight + FGap + FRowHeight + Padding.Bottom;
    end;

    FExpectedHeight:= NewH;
    if Abs(Height - NewH) > 0.1
    then Height:= NewH;
  finally
    FRealigning:= False;
  end;
end;






procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TLightCenteredLayout, TResponsiveRowLayout]);
end;


end.
