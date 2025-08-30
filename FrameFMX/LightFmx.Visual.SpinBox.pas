unit LightFmx.Visual.SpinBox;

// TLabeledSpinBoxFMX combines a TLabel and TSpinBox, similar to TLabeledEdit

interface

uses
  System.SysUtils, System.Classes, System.Math,
  FMX.EditBox, FMX.Controls, FMX.Layouts, FMX.Types, FMX.StdCtrls, FMX.Text, FMX.SpinBox;

type
  TLabeledSpinBox = class(TLayout)
  private
    FLabel: TLabel;
    FSpinBox: TSpinBox;
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
    function GetSpinBox: TSpinBox;
    function GetLabel: TLabel;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SpinBox: TSpinBox read GetSpinBox;
    property LabelControl: TLabel read GetLabel;
  published
    property LabelText: string read GetLabelText write SetLabelText;
    property Value: Double read GetValue write SetValue;
    property Min: Double read GetMin write SetMin;
    property Max: Double read GetMax write SetMax;
    property Increment: Double read GetIncrement write SetIncrement;
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
  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Position.X := 0;
  FLabel.Position.Y := 0;
  FLabel.Text := 'Label:';
  FLabel.AutoSize := True;

  // Create SpinBox
  FSpinBox := TSpinBox.Create(Self);
  FSpinBox.Parent := Self;
  FSpinBox.Position.X := 0;
  FSpinBox.Position.Y := FLabel.Height + 4;
  FSpinBox.Width := 108;
  FSpinBox.Height := 41;
  FSpinBox.Size.PlatformDefault := False;
  FSpinBox.Min := 0;
  FSpinBox.Max := 100;
  FSpinBox.Value := 1.0;
  FSpinBox.ValueType := TNumValueType.Float;
  FSpinBox.Increment := 0.1;
  FSpinBox.KeyboardType := TVirtualKeyboardType.DecimalNumberPad;

  Height := FLabel.Height + FSpinBox.Height + 4;
  Width  := System.Math.Max(FLabel.Width, FSpinBox.Width);
end;


destructor TLabeledSpinBox.Destroy;
begin
  FLabel.Free;
  FSpinBox.Free;
  inherited;
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


procedure TLabeledSpinBox.Resize;
begin
  inherited;
  // Vertical layout: label on top, spinbox below
  FLabel.Position.X := 0;
  FLabel.Position.Y := 0;

  FSpinBox.Position.X := 0;
  FSpinBox.Position.Y := FLabel.Height + 4;
  FSpinBox.Width := Self.Width;
end;




procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TLabeledSpinBox]);
end;


end.