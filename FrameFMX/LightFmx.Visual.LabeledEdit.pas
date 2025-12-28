unit LightFmx.Visual.LabeledEdit;

{=============================================================================================================
   www.GabrielMoraru.com
   2024.05
-------------------------------------------------------------------------------------------------------------
   Exactly like the one in VCL
=============================================================================================================}


interface

uses
  System.SysUtils, System.Classes, System.Math,
  FMX.Controls, FMX.Layouts, FMX.Edit, FMX.Types, FMX.StdCtrls;

type
  // TLabeledEditFMX simulates VCL TLabeledEdit in FMX
  TLabeledEdit = class(TLayout)
  private
    FLabel: TLabel;
    FEdit: TEdit;
    function  GetLabelText: string;
    procedure SetLabelText(const Value: string);
    function  GetEditText: string;
    procedure SetEditText(const Value: string);
    function  GetEdit: TEdit;
    function  GetLabel: TLabel;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Edit: TEdit read GetEdit;
    property LabelControl: TLabel read GetLabel;
  published
    property LabelText: string read GetLabelText write SetLabelText;
    property Text: string read GetEditText write SetEditText;
    property Align;
    property Position;
    property Size;
    property Width;
    property Height;
  end;

procedure Register;


IMPLEMENTATION


constructor TLabeledEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Create Label
  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Position.X := 0;
  FLabel.Position.Y := 0;
  FLabel.Text := 'Label:';

  // Create Edit
  FEdit := TEdit.Create(Self);
  FEdit.Parent := Self;
  FEdit.Position.X := 0;
  FEdit.Position.Y := FLabel.Height + 4; // Space between label and edit
  FEdit.Width := 120;
  FEdit.Text := '';

  Height := FLabel.Height + FEdit.Height + 4;
  Width  := System.Math.Max(FLabel.Width, FEdit.Width);
end;


destructor TLabeledEdit.Destroy;
begin
  FLabel.Free;
  FEdit.Free;
  inherited;
end;




function TLabeledEdit.GetEdit: TEdit;
begin
  Result := FEdit;
end;


function TLabeledEdit.GetEditText: string;
begin
  Result := FEdit.Text;
end;


function TLabeledEdit.GetLabel: TLabel;
begin
  Result := FLabel;
end;


function TLabeledEdit.GetLabelText: string;
begin
  Result := FLabel.Text;
end;


procedure TLabeledEdit.Resize;
begin
  inherited;
  // Simple vertical layout: label on top, edit below
  FLabel.Position.X := 0;
  FLabel.Position.Y := 0;
  FLabel.Width := Self.Width;

  FEdit.Position.X := 0;
  FEdit.Position.Y := FLabel.Height + 4;
  FEdit.Width := Self.Width;
end;


procedure TLabeledEdit.SetEditText(const Value: string);
begin
  FEdit.Text := Value;
end;


procedure TLabeledEdit.SetLabelText(const Value: string);
begin
  FLabel.Text := Value;
end;






procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TLabeledEdit]);
end;


end.