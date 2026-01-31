unit LightFmx.Visual.LabeledEdit;

{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   TLabeledEdit for FMX - Combines a TLabel and TEdit, similar to VCL's TLabeledEdit.
   The label is positioned above the edit control.
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

CONST
  LABEL_EDIT_SPACING = 4;  // Vertical gap between label and edit control


constructor TLabeledEdit.Create(AOwner: TComponent);
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

  // Create Edit
  FEdit:= TEdit.Create(Self);
  FEdit.Parent:= Self;
  FEdit.Stored:= False;  // Prevent streaming to FMX file (avoids duplicate children)
  FEdit.Position.X:= 0;
  FEdit.Position.Y:= FLabel.Height + LABEL_EDIT_SPACING;
  FEdit.Width:= 120;
  FEdit.Text:= '';

  Height:= FLabel.Height + FEdit.Height + LABEL_EDIT_SPACING;
  Width:= System.Math.Max(FLabel.Width, FEdit.Width);
end;


destructor TLabeledEdit.Destroy;
begin
  // FLabel and FEdit are owned by Self, so they're freed automatically
  inherited;
end;




function TLabeledEdit.GetEdit: TEdit;
begin
  Result:= FEdit;
end;


{ Nil check required: During FMX streaming/design-time, properties may be read before children are created }
function TLabeledEdit.GetEditText: string;
begin
  if FEdit = NIL
  then Result:= ''
  else Result:= FEdit.Text;
end;


function TLabeledEdit.GetLabel: TLabel;
begin
  Result:= FLabel;
end;


{ Nil check required: During FMX streaming/design-time, properties may be read before children are created }
function TLabeledEdit.GetLabelText: string;
begin
  if FLabel = NIL
  then Result:= ''
  else Result:= FLabel.Text;
end;


procedure TLabeledEdit.Resize;
begin
  inherited;

  // Guard: During inherited Create, Resize may be called before children are created
  if (FLabel = NIL) OR (FEdit = NIL) then EXIT;

  // Vertical layout: label on top, edit below
  FLabel.Position.X:= 0;
  FLabel.Position.Y:= 0;
  FLabel.Width:= Self.Width;

  FEdit.Position.X:= 0;
  FEdit.Position.Y:= FLabel.Height + LABEL_EDIT_SPACING;
  FEdit.Width:= Self.Width;
end;


{ Nil check required: During FMX streaming/design-time, properties may be set before children are created }
procedure TLabeledEdit.SetEditText(const Value: string);
begin
  if FEdit <> NIL
  then FEdit.Text:= Value;
end;


{ Nil check required: During FMX streaming/design-time, properties may be set before children are created }
procedure TLabeledEdit.SetLabelText(const Value: string);
begin
  if FLabel <> NIL
  then FLabel.Text:= Value;
end;






procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TLabeledEdit]);
end;


end.