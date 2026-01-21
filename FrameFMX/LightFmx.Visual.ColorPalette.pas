UNIT LightFmx.Visual.ColorPalette;

{=============================================================================================================
   2026
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   TColorPalette

   A visual component that displays a grid of color swatches.
   The user can click on a swatch to select a color. The OnColorSelected event is fired.

   Features:
     - Configurable swatch size and spacing
     - Automatic wrapping of swatches when the container is resized
     - Visual selection indicator (thicker border)
     - Smart border color (white for dark colors, black for light colors)

   Usage:
     ColorPalette.SetColors([TAlphaColorRec.Red, TAlphaColorRec.Green, TAlphaColorRec.Blue]);
     ColorPalette.OnColorSelected := MyHandler;

   Tester:
     c:\Projects\LightSaber\Demo\FMX\Demo TColorPalette\ProjectColorPalette.dpr
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Classes, System.UITypes, System.Generics.Collections, System.Math, System.Types,
  FMX.Types, FMX.Controls, FMX.Layouts, FMX.ScrollBox, FMX.Objects, FMX.Graphics;

TYPE
  TColorSelectedEvent = procedure(Sender: TObject; const AColor: TAlphaColor) of object;

  TColorPalette = class(TScrollBox)
  private
    FFlow            : TFlowLayout;
    FColors          : TList<TAlphaColor>;
    FItemSize        : Single;
    FItemSpacing     : Single;
    FSelectedShape   : TShape;              // Currently selected swatch
    FSelectedColor   : TAlphaColor;
    FHasSelection    : Boolean;
    FOnColorSelected : TColorSelectedEvent;
    FStrokeColorLight: TAlphaColor;         // Border color for dark swatches
    FStrokeColorDark : TAlphaColor;         // Border color for light swatches

    procedure Rebuild;
    procedure SetItemSize(const Value: Single);
    procedure SetItemSpacing(const Value: Single);
    procedure ApplySelectionVisual(AShape: TShape);
    procedure ClearSelectionVisual;
    procedure SwatchMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    function  CalcLuminance(Color: TAlphaColor): Single;
  protected
    procedure Resize; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetColors(const AColors: array of TAlphaColor);
    procedure AddColor(AColor: TAlphaColor);
    procedure RemoveColorAt(Index: Integer);
    procedure ClearColors;
    procedure SelectColor(AColor: TAlphaColor);
    procedure ClearSelection;

    property SelectedColor: TAlphaColor read FSelectedColor;
    property HasSelection: Boolean read FHasSelection;
  published
    property OnColorSelected  : TColorSelectedEvent read FOnColorSelected write FOnColorSelected;
    property ItemSize         : Single read FItemSize write SetItemSize;
    property ItemSpacing      : Single read FItemSpacing write SetItemSpacing;
    property StrokeColorLight : TAlphaColor read FStrokeColorLight write FStrokeColorLight;
    property StrokeColorDark  : TAlphaColor read FStrokeColorDark write FStrokeColorDark;
  end;

procedure Register;


IMPLEMENTATION

CONST
  DEFAULT_ITEM_SIZE    = 28;
  DEFAULT_ITEM_SPACING = 6;
  DEFAULT_PADDING      = 6;
  SELECTION_THICKNESS  = 3;
  NORMAL_THICKNESS     = 1;
  LUMINANCE_THRESHOLD  = 0.45;   // Below this, use light stroke; above, use dark stroke


{-------------------------------------------------------------------------------------------------------------
   CONSTRUCTOR / DESTRUCTOR
-------------------------------------------------------------------------------------------------------------}
constructor TColorPalette.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Initialize default values
  FItemSize         := DEFAULT_ITEM_SIZE;
  FItemSpacing      := DEFAULT_ITEM_SPACING;
  FStrokeColorLight := TAlphaColorRec.White;
  FStrokeColorDark  := TAlphaColorRec.Black;
  FSelectedShape    := NIL;
  FHasSelection     := FALSE;
  FSelectedColor    := 0;

  // Set padding around the content
  Padding.Rect  := TRectF.Create(DEFAULT_PADDING, DEFAULT_PADDING, DEFAULT_PADDING, DEFAULT_PADDING);

  // Create flow layout for automatic swatch arrangement
  FFlow         := TFlowLayout.Create(Self);
  FFlow.Align   := TAlignLayout.Client;
  FFlow.Stored  := FALSE;   // Don't stream to .fmx file
  FFlow.Parent  := Self.Content;

  // Color list
  FColors := TList<TAlphaColor>.Create;
end;


destructor TColorPalette.Destroy;
begin
  FreeAndNil(FColors);
  inherited;
end;


{-------------------------------------------------------------------------------------------------------------
   INITIALIZATION
-------------------------------------------------------------------------------------------------------------}
procedure TColorPalette.Loaded;
begin
  inherited;

  // If no colors were set by user, provide sensible defaults
  if FColors.Count = 0
  then SetColors([
      TAlphaColorRec.Black,     TAlphaColorRec.Darkgray,  TAlphaColorRec.Gray,
      TAlphaColorRec.Lightgray, TAlphaColorRec.White,     TAlphaColorRec.Red,
      TAlphaColorRec.Orange,    TAlphaColorRec.Yellow,    TAlphaColorRec.Lime,
      TAlphaColorRec.Cyan,      TAlphaColorRec.Blue,      TAlphaColorRec.Purple])
  else
    Rebuild;
end;


procedure TColorPalette.Resize;
begin
  inherited;

  // Update flow layout width so swatches wrap correctly
  if Assigned(FFlow)
  then FFlow.Width := Max(0, Width - Padding.Left - Padding.Right);
end;


{-------------------------------------------------------------------------------------------------------------
   COLOR MANAGEMENT
-------------------------------------------------------------------------------------------------------------}
procedure TColorPalette.SetColors(const AColors: array of TAlphaColor);
begin
  FColors.Clear;
  for VAR i := Low(AColors) to High(AColors) do
    FColors.Add(AColors[i]);
  Rebuild;
end;


procedure TColorPalette.AddColor(AColor: TAlphaColor);
begin
  FColors.Add(AColor);
  Rebuild;
end;


procedure TColorPalette.RemoveColorAt(Index: Integer);
begin
  if (Index < 0) OR (Index >= FColors.Count) then EXIT;

  // Clear selection if removing selected color
  if FHasSelection
  AND Assigned(FSelectedShape)
  AND (FSelectedShape.Fill.Color = FColors[Index]) then
    begin
      ClearSelectionVisual;
      FHasSelection := FALSE;
    end;

  FColors.Delete(Index);
  Rebuild;
end;


procedure TColorPalette.ClearColors;
begin
  FColors.Clear;
  ClearSelectionVisual;
  FHasSelection := FALSE;
  Rebuild;
end;


{-------------------------------------------------------------------------------------------------------------
   SELECTION
-------------------------------------------------------------------------------------------------------------}
procedure TColorPalette.SelectColor(AColor: TAlphaColor);
begin
  // Find the shape with this color and select it
  for VAR i := 0 to FFlow.ChildrenCount - 1 do
    if FFlow.Children[i] is TShape then
      begin
        VAR Shape := TShape(FFlow.Children[i]);
        if Shape.Fill.Color = AColor then
          begin
            ApplySelectionVisual(Shape);
            EXIT;
          end;
      end;
end;


procedure TColorPalette.ClearSelection;
begin
  ClearSelectionVisual;
end;


procedure TColorPalette.ClearSelectionVisual;
begin
  if Assigned(FSelectedShape) then
    begin
      FSelectedShape.Stroke.Thickness := NORMAL_THICKNESS;
      FSelectedShape.Stroke.Color     := FStrokeColorDark;
      FSelectedShape := NIL;
    end;
  FHasSelection := FALSE;
end;


procedure TColorPalette.ApplySelectionVisual(AShape: TShape);
begin
  // Clear previous selection
  if Assigned(FSelectedShape) AND (FSelectedShape <> AShape) then
    begin
      FSelectedShape.Stroke.Thickness := NORMAL_THICKNESS;
      FSelectedShape.Stroke.Color     := FStrokeColorDark;
    end;

  // Apply selection to new shape
  FSelectedShape := AShape;
  if Assigned(FSelectedShape) then
    begin
      FSelectedShape.Stroke.Thickness := SELECTION_THICKNESS;

      // Choose contrasting border color based on luminance
      if CalcLuminance(FSelectedShape.Fill.Color) < LUMINANCE_THRESHOLD
      then FSelectedShape.Stroke.Color := FStrokeColorLight
      else FSelectedShape.Stroke.Color := FStrokeColorDark;

      FHasSelection  := TRUE;
      FSelectedColor := FSelectedShape.Fill.Color;
    end
  else
    FHasSelection := FALSE;
end;


function TColorPalette.CalcLuminance(Color: TAlphaColor): Single;
begin
  // Extract RGB components and calculate relative luminance (ITU-R BT.709)
  VAR r := (Color and $FF) / 255.0;
  VAR g := ((Color shr 8) and $FF) / 255.0;
  VAR b := ((Color shr 16) and $FF) / 255.0;
  Result := 0.2126 * r + 0.7152 * g + 0.0722 * b;
end;


{-------------------------------------------------------------------------------------------------------------
   MOUSE HANDLING
-------------------------------------------------------------------------------------------------------------}
procedure TColorPalette.SwatchMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if NOT (Sender is TShape) then EXIT;

  VAR Shape := TShape(Sender);

  // Apply visual selection
  ApplySelectionVisual(Shape);

  // Fire event
  if Assigned(FOnColorSelected)
  then FOnColorSelected(Self, Shape.Fill.Color);
end;


{-------------------------------------------------------------------------------------------------------------
   PROPERTY SETTERS
-------------------------------------------------------------------------------------------------------------}
procedure TColorPalette.SetItemSize(const Value: Single);
begin
  if (Value <= 0) OR (FItemSize = Value) then EXIT;
  FItemSize := Value;
  Rebuild;
end;


procedure TColorPalette.SetItemSpacing(const Value: Single);
begin
  if (Value < 0) OR (FItemSpacing = Value) then EXIT;
  FItemSpacing := Value;
  Rebuild;
end;


{-------------------------------------------------------------------------------------------------------------
   REBUILD UI
-------------------------------------------------------------------------------------------------------------}
procedure TColorPalette.Rebuild;
VAR
  Swatch: TRectangle;
  HalfSpacing: Single;
begin
  if NOT Assigned(FFlow) then EXIT;

  HalfSpacing := FItemSpacing / 2;

  FFlow.BeginUpdate;
  TRY
    // Remove existing swatches
    for VAR i := FFlow.ChildrenCount - 1 downto 0 do
      FFlow.Children[i].Free;

    // Create new swatches as rectangles (squares)
    for VAR i := 0 to FColors.Count - 1 do
      begin
        Swatch := TRectangle.Create(Self);
        Swatch.Parent  := FFlow;
        Swatch.Stored  := FALSE;              // Don't stream to .fmx file
        Swatch.Width   := FItemSize;
        Swatch.Height  := FItemSize;
        Swatch.HitTest := TRUE;

        // Fill color
        Swatch.Fill.Kind  := TBrushKind.Solid;
        Swatch.Fill.Color := FColors[i];

        // Border
        Swatch.Stroke.Kind      := TBrushKind.Solid;
        Swatch.Stroke.Thickness := NORMAL_THICKNESS;
        Swatch.Stroke.Color     := FStrokeColorDark;

        // Spacing via margins
        Swatch.Margins.Rect := TRectF.Create(HalfSpacing, HalfSpacing, HalfSpacing, HalfSpacing);

        // Disable gestures to ensure mouse events work
        Swatch.Touch.InteractiveGestures := [];
        Swatch.CanParentFocus := FALSE;

        // Event handler
        Swatch.OnMouseDown := SwatchMouseDown;
      end;
  FINALLY
    FFlow.EndUpdate;
  END;

  // Restore selection if it was previously set
  if FHasSelection then
    for VAR i := 0 to FFlow.ChildrenCount - 1 do
      if FFlow.Children[i] is TShape then
        begin
          VAR Shape := TShape(FFlow.Children[i]);
          if Shape.Fill.Color = FSelectedColor then
            begin
              ApplySelectionVisual(Shape);
              BREAK;
            end;
        end;
end;


{-------------------------------------------------------------------------------------------------------------
   COMPONENT REGISTRATION
-------------------------------------------------------------------------------------------------------------}
procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TColorPalette]);
end;


end.
