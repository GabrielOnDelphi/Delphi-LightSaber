UNIT LightFmx.Visual.ComboBox;

{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

  FMX port of LightVcl.Visual.ComboBox (TCubicComboBox).

  Features:
      + SelectedItem - Get currently selected item
      + DualItems - Store two strings per item separated by |

  DualItems:
    Allows user to add two strings separated by |.
    The format is: 'int_cmd|Nice Screen Name' stored in Items as: Names | Value
    The UI shows the second string (Value).
    The first string (Name) can be retrieved with: s := TLightComboBox.SelectedItem;
    The screen name (Value) can be retrieved with: s := TLightComboBox.SelectedDualItem;

  Tester: c:\MyProjects\Project support\Testers\CubicCombobox tester\Tester.dpr
=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes,
  FMX.Controls, FMX.ListBox;

TYPE
  TLightComboBox = class(TComboBox)
  private
    FDualItem: Boolean;
  public
    constructor Create(AOwner: TComponent); override;

    { Get selection }
    function SelectedItem: string;            { Returns the selected item. Raises assertion if no item selected! Use SelectedItemSafe to avoid exceptions. }
    function SelectedItemSafe: string;        { Returns the selected item. Returns '' if no item selected }
    function SelectedItemForce: string;       { Returns the selected item. If no item is selected, selects the first item first }
    function SelectedObject: TObject;         { Returns the object associated with selected item, or NIL }

    { Set selection }
    function SelectItem(const ItemText: string): Integer;         { Select by text. Case insensitive. Returns index or -1 if not found }
    function SelectFirstItem: Boolean;                            { Select first item. Returns True if list is not empty }
    function SelectObject(AObject: TObject): Boolean;             { Select by object. Returns True if found }

    { Dual items }
    function SelectDualItem(const ScreenName: string): Integer;   { Select by screen name (Value). Returns index or -1 }
    function SelectedDualItem: string;                            { Returns screen name (Value) of selected dual item }
  published
    property IsDualItem: Boolean read FDualItem write FDualItem default False;  { Indicates if the items contained are 'dual items' (two strings per item, separated by '|' ) }
  end;

procedure Register;

IMPLEMENTATION


constructor TLightComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items.NameValueSeparator:= '|';   // Set separator for dual items
  FDualItem:= False;                // Default to single item mode
end;


{-----------------------------------------------------------------------------------------------------------------------
   SET SELECTION
-----------------------------------------------------------------------------------------------------------------------}

{ Selects the item matching the specified text. Case insensitive. Returns index or -1 if not found.
  In IsDualItem mode, searches by Name (internal command), not by Value (screen name). }
function TLightComboBox.SelectItem(const ItemText: string): Integer;
var
  i: Integer;
begin
  Result:= -1;
  if Count = 0 then EXIT;

  if IsDualItem
  then
    begin
      for i:= 0 to Items.Count-1 do
        if SameText(Items.Names[i], ItemText) then   // Search by internal command name
        begin
          Result:= i;
          Break;
        end;
    end
  else
    begin
      for i:= 0 to Items.Count-1 do
        if SameText(Items[i], ItemText) then
        begin
          Result:= i;
          Break;
        end;
    end;

  ItemIndex:= Result;  // Set selection, -1 deselects if not found
end;


function TLightComboBox.SelectObject(AObject: TObject): Boolean;
var
  i: Integer;
begin
  for i:= 0 to Items.Count-1 do
    if Items.Objects[i] = AObject then
    begin
      ItemIndex:= i;
      EXIT(True);
    end;
  Result:= False;
end;


function TLightComboBox.SelectFirstItem: Boolean;
begin
  Result:= Count > 0;
  if Result
  then ItemIndex:= 0;
end;



{-----------------------------------------------------------------------------------------------------------------------
   GET SELECTION
-----------------------------------------------------------------------------------------------------------------------}

{ Returns the selected item. Raises assertion if no item is selected - use SelectedItemSafe instead.
  If IsDualItem mode is active, returns the 'internal command' (Name) associated with this item. }
function TLightComboBox.SelectedItem: string;
begin
  Assert(ItemIndex >= 0, 'No item selected! Use SelectedItemSafe to avoid exceptions.');
  if IsDualItem
  then Result:= Items.Names[ItemIndex]
  else Result:= Items[ItemIndex];
end;


{ Safe version of SelectedItem. Returns '' if no item is selected or list is empty.
  If IsDualItem mode is active, returns the 'internal command' (Name) associated with this item. }
function TLightComboBox.SelectedItemSafe: string;
begin
  if (Count <= 0) OR (ItemIndex < 0)
  then Result:= ''
  else
    if IsDualItem
    then Result:= Items.Names[ItemIndex]
    else Result:= Items[ItemIndex];
end;


{ Returns the selected item. If no item is selected, selects the first item first.
  Returns '' if list is empty. }
function TLightComboBox.SelectedItemForce: string;
begin
  if Count = 0 then EXIT('');
  if ItemIndex < 0
  then ItemIndex:= 0;
  Result:= SelectedItem;
end;


function TLightComboBox.SelectedObject: TObject;
begin
  if ItemIndex >= 0
  then Result:= Items.Objects[ItemIndex]
  else Result:= NIL;
end;



{-----------------------------------------------------------------------------------------------------------------------
   DUAL ITEMS
-----------------------------------------------------------------------------------------------------------------------}

{ Returns the screen name (Value) of the selected dual item.
  Raises assertion if IsDualItem mode is not active.
  Returns '' if no item is selected. }
function TLightComboBox.SelectedDualItem: string;
begin
  Assert(IsDualItem, 'SelectedDualItem called but IsDualItem is False!');
  if ItemIndex < 0
  then Result:= ''
  else Result:= Items.ValueFromIndex[ItemIndex];
end;


{ Selects an item based on its screen name (Value). Case insensitive.
  Raises assertion if IsDualItem mode is not active.
  Returns index or -1 if not found. }
function TLightComboBox.SelectDualItem(const ScreenName: string): Integer;
var
  i: Integer;
begin
  Assert(IsDualItem, 'SelectDualItem called but IsDualItem is False!');
  if Count = 0 then EXIT(-1);

  Result:= -1;
  for i:= 0 to Items.Count-1 do
    if SameText(Items.ValueFromIndex[i], ScreenName) then
    begin
      Result:= i;
      Break;
    end;

  ItemIndex:= Result;  // If no item with this name is found, -1 deselects
end;



procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TLightComboBox]);
end;

end.
