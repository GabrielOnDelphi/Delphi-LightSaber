unit LightFmx.Visual.SpinBox;

{=============================================================================================================
   2026.06.10
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
TLabeledSpinBoxFMX combines a TLabel and TSpinBox, similar to TLabeledEdit
=============================================================================================================}

interface

uses
  System.SysUtils, System.Classes, System.Math,
  FMX.EditBox, FMX.Controls, FMX.Layouts, FMX.Types, FMX.StdCtrls, FMX.Text, FMX.SpinBox;

type
  TLabeledSpinBox = class(TLayout)
  private
    FLabel: TLabel;      // Owned by Self (Owner-managed) - freed automatically, no destructor needed
    FSpinBox: TSpinBox;  // Owned by Self (Owner-managed)
    function GetLabelText: string;
    procedure SetLabelText(const Value: string);
    function GetValue: Double;
    procedure SetValue(const Value: Double);
    function GetMax: Double;
    procedure SetMax(const Value: Double);
    function GetMin: Double;
    procedure SetMin(const Value: Double);
    function GetIncrement: Double;
    procedure SetIncrement(const Value: Double);
    function GetValueType: TNumValueType;
    procedure SetValueType(const AValue: TNumValueType);
    function GetSpinBox: TSpinBox;
    function GetLabel: TLabel;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    property SpinBox: TSpinBox read GetSpinBox;
    property LabelControl: TLabel read GetLabel;
  published
    property LabelText: string read GetLabelText write SetLabelText;
    { Declared BEFORE Value on purpose (same reason as the Min/Max note below), and routed through a
      setter that repairs an FMX defect - see SetValueType. }
    property ValueType: TNumValueType read GetValueType write SetValueType default TNumValueType.Float;
    { Min/Max/Increment MUST be declared BEFORE Value: the FMX streamer writes/reads properties in
      declaration order, and the internal TSpinBox clamps Value to Min..Max immediately on assignment
      (it is not in csLoading). With Value first, a design-time Value above the default Max (100)
      would be clamped on load (e.g. Max=500, Value=250 loaded back as 100). }
    property Min: Double read GetMin write SetMin;
    property Max: Double read GetMax write SetMax;
    property Increment: Double read GetIncrement write SetIncrement;
    property Value: Double read GetValue write SetValue;
    property Align;
    property Position;
    property Size;
    property Width;
    property Height;
  end;

procedure Register;

implementation


constructor TLabeledSpinBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Create Label
  FLabel:= TLabel.Create(Self);
  FLabel.Parent:= Self;
  FLabel.Stored:= False;  // Prevent streaming to FMX file (avoids duplicate children)
  FLabel.Position.X:= 0;
  FLabel.Position.Y:= 0;
  FLabel.Text:= 'Label:';
  FLabel.AutoSize:= True;

  // Create SpinBox
  FSpinBox:= TSpinBox.Create(Self);
  FSpinBox.Parent:= Self;
  FSpinBox.Stored:= False;  // Prevent streaming to FMX file (avoids duplicate children)
  FSpinBox.Position.X:= 0;
  FSpinBox.Position.Y:= FLabel.Height + 4;
  FSpinBox.Width:= 108;
  FSpinBox.Height:= 41;
  FSpinBox.Size.PlatformDefault:= False;
  FSpinBox.Min:= 0;
  FSpinBox.Max:= 100;
  FSpinBox.ValueType:= TNumValueType.Float;   // BEFORE Value - see SetValueType for why the order matters
  FSpinBox.Value:= 1.0;
  FSpinBox.Increment:= 0.1;
  FSpinBox.KeyboardType:= TVirtualKeyboardType.DecimalNumberPad;

  Height:= FLabel.Height + FSpinBox.Height + 4;
  Width:= System.Math.Max(FLabel.Width, FSpinBox.Width);
end;



function TLabeledSpinBox.GetLabel: TLabel;
begin
  Result := FLabel;
end;


function TLabeledSpinBox.GetLabelText: string;
begin
  Result := FLabel.Text;
end;


function TLabeledSpinBox.GetSpinBox: TSpinBox;
begin
  Result := FSpinBox;
end;


function TLabeledSpinBox.GetValue: Double;
begin
  Result := FSpinBox.Value;
end;


procedure TLabeledSpinBox.SetLabelText(const Value: string);
begin
  FLabel.Text := Value;
end;

procedure TLabeledSpinBox.SetValue(const Value: Double);
begin
  FSpinBox.Value := Value;
end;


function TLabeledSpinBox.GetMax: Double;
begin
  Result := FSpinBox.Max;
end;


function TLabeledSpinBox.GetMin: Double;
begin
  Result := FSpinBox.Min;
end;


function TLabeledSpinBox.GetIncrement: Double;
begin
  Result := FSpinBox.Increment;
end;


procedure TLabeledSpinBox.SetMax(const Value: Double);
begin
  FSpinBox.Max := Value;
end;

procedure TLabeledSpinBox.SetMin(const Value: Double);
begin
  FSpinBox.Min := Value;
end;

procedure TLabeledSpinBox.SetIncrement(const Value: Double);
begin
  FSpinBox.Increment := Value;
end;


function TLabeledSpinBox.GetValueType: TNumValueType;
begin
  Result := FSpinBox.ValueType;
end;


{ Changing ValueType MUST NOT change Value - but in FMX it does. Switching Float -> Integer
  multiplies the value by 10^DecimalDigits (x100 with the default 2 digits), then clamps it to Max.

  Traced through the Delphi 13 RTL:
     TEditBoxModel.SetValueType    FMX.EditBox.pas:577  FilterChar := '0123456789-+'   (no '.' , no ',')
     TCustomEditModel.SetFilterChar FMX.Edit.pas:2148    re-filters the EXISTING TEXT
                                                         -> '20.00' becomes '2000'
                                    FMX.Edit.pas:2153    text changed -> Change
     TCustomEditModel.Change        FMX.Edit.pas:1714    -> Validate -> DoValidate
     TEditBoxModel.DoValidate       FMX.EditBox.pas:516  ValueRange.Value := 2000  (Value re-read FROM the text)
     TCustomValueRange.IntChanged   FMX.StdActns.pas:979 clamps into Min..Max -> 200

  Measured 2026-08-01: a box left at the constructor defaults (Float, DecimalDigits=2) holding 20 with
  Max=200 came out as 200 the instant ValueType was set to Integer. It is not a streaming bug - a plain
  runtime assignment triggers it just as hard.

  Scope, stated precisely: the switch scales Value by 10^DecimalDigits (the decimals of the rendered
  text are promoted to units), then clamps the result into Min..Max - so it ends up ON Max for a large
  positive value, on Min for a large negative one, and at Value*10^DecimalDigits when the scaled number
  still fits. The switch is harmless only when that scaling is a no-op: Value=0 ('0.00' filters down to
  '000') or DecimalDigits=0 (the text then carries no separator, so the filter finds nothing to strip
  and no Change fires at all). Whether the caller NOTICES
  depends on whether it assigns Value afterwards - a later assignment repairs it by accident. The pattern
  that leaves the damage visible is "design-time Value in the .fmx, ValueType set in code afterwards",
  which used to be the ONLY possible pattern because ValueType was not published.

  The repair saves Value across the switch. Assigning it afterwards is enough: by then FilterChar is the
  integer set, so the text round-trip cannot fire a second time. Events are muted for the duration
  because changing the DISPLAY type is not a value change - without that, a consumer's OnChange would
  see the bogus 200 before the restore lands. The one case where the value legitimately DOES move is
  Float -> Integer over a fraction; that is rounded deliberately and reported with a single OnChange
  after the handlers are restored. See the two comments inside the procedure. }
procedure TLabeledSpinBox.SetValueType(const AValue: TNumValueType);
var
  SavedValue:    Double;
  NewValue:      Double;
  SavedChange:   TNotifyEvent;
  SavedTracking: TNotifyEvent;
begin
  if FSpinBox.ValueType = AValue then EXIT;

  SavedValue    := FSpinBox.Value;
  SavedChange   := FSpinBox.OnChange;
  SavedTracking := FSpinBox.OnChangeTracking;

  { An Integer box cannot hold a fraction, so drop it HERE instead of letting the model drop it behind
    our back. TEditBoxModel.GetValue rounds on the way OUT (FMX.EditBox.pas:531-537) while ValueRange
    keeps the fraction - so the box would report 12 while still holding 12.5, and switching back to
    Float would resurrect the 12.5 the caller was told had been discarded.
    Round (not Trunc) because that is what FMX itself uses everywhere (FMX.EditBox.pas:443,510,515,534). }
  if AValue = TNumValueType.Integer
  then NewValue:= Round(SavedValue)
  else NewValue:= SavedValue;

  FSpinBox.OnChange        := NIL;
  FSpinBox.OnChangeTracking:= NIL;
  try
    FSpinBox.ValueType:= AValue;
    FSpinBox.Value    := NewValue;
  finally
    FSpinBox.OnChange        := SavedChange;
    FSpinBox.OnChangeTracking:= SavedTracking;
  end;

  { Fired AFTER the handlers are back, so a consumer sees the settled value and never the bogus
    intermediate - and ONLY when the value really moved (Float 12.5 -> Integer 12). A switch that
    changes nothing but the display type stays silent, which is the contract. Sender is FSpinBox
    because that is what FMX passes: TCustomEditModel.DoChange calls FOnChange(Owner), FMX.Edit.pas:1871. }
  if (NOT SameValue(SavedValue, NewValue)) AND Assigned(SavedChange)
  then SavedChange(FSpinBox);
end;


procedure TLabeledSpinBox.Resize;
begin
  inherited;

  // Guard: During inherited Create, Resize may be called before children are created
  if (FLabel = NIL) OR (FSpinBox = NIL) then EXIT;

  // Vertical layout: label on top, spinbox below
  FLabel.Position.X:= 0;
  FLabel.Position.Y:= 0;

  FSpinBox.Position.X:= 0;
  FSpinBox.Position.Y:= FLabel.Height + 4;
  FSpinBox.Width:= Self.Width;
end;




procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TLabeledSpinBox]);
end;


end.