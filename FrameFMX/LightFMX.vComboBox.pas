unit LightFMX.vComboBox;

{=============================================================================================================
   www.GabrielMoraru.com
   2024.05
-------------------------------------------------------------------------------------------------------------
  Features:
      + SelectedItem
      + DualItems

  DualItems:
    Allows user to add two strings separated by |.
    The format is: 'int_cmd|Nice Screen Name' stored in Items as: Names | Value
    The UI shows the second string (Value).
    The first string (Name) can be retrieved with: s := TCubicComboBox.SelectedDualItem;

  Tester: c:\MyProjects\Project support\Testers\CubicCombobox tester\Tester.dpr
=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes, System.Types,   // Added System.Types for TRectF
  FMX.Controls, FMX.ListBox, FMX.Graphics, FMX.Types;

type
  TLightComboBox = class(TComboBox)
  private
    FDualItem: Boolean;
    // Owner-draw event for the internal listbox
   // procedure DoDrawItem(const Canvas: TCanvas; const Bounds: TRectF; const Index: Integer; const State: TOwnerDrawStates);
  public
    constructor Create(AOwner: TComponent); override;

    { Set selection }
    function SelectedItem: string;            // Returns the selected item's internal command (if DualItem) or full text
    function SelectedItemSafe: string;        // Safe version, returns '' if no selection or empty
    function SelectedItemForce: string;       // Returns first item if none selected
    { Get Selection }
    function SelectItem(const ItemText: string): Integer;       // Select by text
    function SelectObject(AObject: TObject): Integer;           // Select by object

    function SelectedObject: TObject;         // Returns associated object
    function SelectedIndex: Integer;         // Returns current ItemIndex

    function SelectDualItem(const DualText: string): Integer;   // Select by dual text
    function SelectedDualItem: string;        // Returns screen name of selected dual item
  published
    property IsDualItem: Boolean read FDualItem write FDualItem default False;  // Enables dual item mode
  end;

procedure Register;

IMPLEMENTATION



constructor TLightComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items.NameValueSeparator := '|';            // Set separator for dual items
  //ListBox.OnDrawItem := DoDrawItem;           // Assign custom drawing handler on internal listbox
  FDualItem := False;                         // Default to single item mode
end;

{-----------------------------------------------------------------------------------------------------------------------
   DRAW
-----------------------------------------------------------------------------------------------------------------------}
{procedure TCubicComboBox.DoDrawItem(const Canvas: TCanvas; const Bounds: TRectF; const Index: Integer; const State: TOwnerDrawStates);
var
  s: string;
  TextRect: TRectF;
begin
  // Determine text to display: screen name if dual, full text otherwise
  if IsDualItem then
    s := Items.ValueFromIndex[Index]
  else
    s := Items[Index];

  // Set colors based on selection state
  if odSelected in State
  then
    begin
      Canvas.Fill.Color := TAlphaColorRec.Blue;    // Highlight background
      Canvas.Font.Color := TAlphaColorRec.White;   // Highlight text
    end
  else
    begin
      Canvas.Fill.Color := TAlphaColorRec.White;   // Default background
      Canvas.Font.Color := TAlphaColorRec.Black;   // Default text
    end;

  Canvas.FillRect(Bounds, 0, 0, AllCorners, 1);   // Draw background (no radius, full opacity)

  // Draw text with slight padding
  TextRect := Bounds;
  TextRect.Left := TextRect.Left + 2;  // Left padding
  Canvas.FillText(TextRect, s, False, 1, [], TTextAlign.Leading, TTextAlign.Center);  // Draw centered text
end;  }

{ Set Selection }

function TLightComboBox.SelectedItem: string;
begin
  if ItemIndex >= 0 then
    if IsDualItem then
      Result := Items.Names[ItemIndex] // Internal command
    else
      Result := Items[ItemIndex]     // Full text
  else
    Result := '';
end;

function TLightComboBox.SelectedItemSafe: string;
begin
  Result := SelectedItem;
  if Result = '' then
    Result := '';
end;

function TLightComboBox.SelectedItemForce: string;
begin
  if Items.Count = 0 then
    Result := ''
  else
  begin
    if ItemIndex < 0 then
      ItemIndex := 0;  // Force select first item
    Result := SelectedItem;
  end;
end;


function TLightComboBox.SelectedObject: TObject;
begin
  if ItemIndex >= 0 then
    Result := Items.Objects[ItemIndex]
  else
    Result := nil;
end;

function TLightComboBox.SelectedIndex: Integer;
begin
  Result := ItemIndex;
end;

{ Get Selection }

function TLightComboBox.SelectItem(const ItemText: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Items.Count - 1 do
    if Items[i] = ItemText then
    begin
      Result := i;
      Break;
    end;
  ItemIndex := Result;  // Set selection, -1 if not found
end;


function TLightComboBox.SelectDualItem(const DualText: string): Integer;
var
  i: Integer;
begin
  Assert(IsDualItem);
  Result := -1;
  for i := 0 to Items.Count - 1 do
    if Items.ValueFromIndex[i] = DualText then
    begin
      Result := i;
      Break;
    end;
  ItemIndex := Result;
end;


{ Dual Items }
function TLightComboBox.SelectObject(AObject: TObject): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Items.Count - 1 do
    if Items.Objects[i] = AObject then
    begin
      Result := i;
      Break;
    end;

  ItemIndex := Result;  // Set selection, -1 if not found
end;


function TLightComboBox.SelectedDualItem: string;
begin
  Assert(IsDualItem);
  if ItemIndex >= 0 then
    Result := Items.ValueFromIndex[ItemIndex]  // Screen name
  else
    Result := '';
end;


procedure Register;
begin
  RegisterComponents('LightSaber', [TLightComboBox]);
end;

end.
