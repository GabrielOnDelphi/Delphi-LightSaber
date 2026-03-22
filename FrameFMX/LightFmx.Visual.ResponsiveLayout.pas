UNIT LightFmx.Visual.ResponsiveLayout;

{=============================================================================================================
   2026.03
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Responsive layout components for FMX.

   TCenteredMaxLayout
     A layout that centers its content horizontally within the parent, capped at MaxWidth pixels.
     When the parent is narrower than MaxWidth, the layout shrinks to fit.
     Auto-sizes its height to fit children (when Align is Top/Bottom/MostTop/MostBottom).
     LabelWidth and RowHeight propagate to all child TResponsiveRowLayout controls.
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
     Exactly 2 children. First child = label (Align=None). Second child = control (keeps its own width).

   Example hierarchy:

     TVertScrollBox (Align=Client)                            <-- optional, for scrolling
       +-- TCenteredMaxLayout                    (Align=Top, MaxWidth=500)
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
  TCenteredMaxLayout = class(TLayout)
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
   TCenteredMaxLayout
-------------------------------------------------------------------------------------------------------------}

constructor TCenteredMaxLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaxWidth:= 400;
  FLabelWidth:= 120;
  FRowHeight:= 28;
  Align:= TAlignLayout.Top;
end;


procedure TCenteredMaxLayout.SetMaxWidth(const Value: Single);
begin
  if FMaxWidth <> Value then
  begin
    FMaxWidth:= Max(0, Value);
    Realign;
  end;
end;


procedure TCenteredMaxLayout.SetLabelWidth(const Value: Single);
begin
  if FLabelWidth <> Value then
  begin
    FLabelWidth:= Max(0, Value);
    Realign;
  end;
end;


procedure TCenteredMaxLayout.SetRowHeight(const Value: Single);
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
procedure TCenteredMaxLayout.SetBounds(X, Y, AWidth, AHeight: Single);
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


procedure TCenteredMaxLayout.DoRealign;
var
  NewH, ChildBottom: Single;
  i: Integer;
begin
  if FRealigning then EXIT;
  FRealigning:= True;
  try
    { Propagate properties directly to avoid triggering redundant Realign on each row.
      The subsequent inherited call will give rows their correct Width and trigger DoRealign. }
    for i:= 0 to ControlsCount - 1 do
      if Controls[i] is TResponsiveRowLayout then
      begin
        TResponsiveRowLayout(Controls[i]).FLabelWidth:= FLabelWidth;
        TResponsiveRowLayout(Controls[i]).FRowHeight:= FRowHeight;
      end;

    inherited;

    { Auto-size height to fit all visible children. Only for stacking alignments;
      for Align=Client the parent controls our height. }
    if Align in [TAlignLayout.Top, TAlignLayout.Bottom,
                 TAlignLayout.MostTop, TAlignLayout.MostBottom] then
    begin
      NewH:= 0;
      for i:= 0 to ControlsCount - 1 do
        if Controls[i].Visible then
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
   Control keeps its own width — set Align = Left or None.

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

    { BreakWidth > 0: use as explicit threshold. BreakWidth = 0: auto from content. }
    if FBreakWidth > 0
    then SideBySideOK:= AvailW >= FBreakWidth
    else SideBySideOK:= AvailW >= FLabelWidth + FGap + Ctrl.Width;

    if SideBySideOK then
    begin
      { Side by side: label at fixed width, control after gap keeps its own width }
      Lbl.SetBounds (StartX, StartY, FLabelWidth, FRowHeight);
      Ctrl.SetBounds(StartX + FLabelWidth + FGap, StartY, Ctrl.Width, FRowHeight);
      NewH:= StartY + FRowHeight + Padding.Bottom;
    end
    else
    begin
      { Stacked: label on top at full width, control below keeps its own width }
      Lbl.SetBounds (StartX, StartY, AvailW, FRowHeight);
      Ctrl.SetBounds(StartX, StartY + FRowHeight + FGap, Ctrl.Width, FRowHeight);
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
  RegisterComponents('LightSaber FMX', [TCenteredMaxLayout, TResponsiveRowLayout]);
end;


end.
