UNIT cvComboBox;

{=============================================================================================================
   www.GabrielMoraru.com
   2024.05
--------------------------------------------------------------------------------------------------------------

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
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.StdCtrls;

TYPE
  TCubicComboBox = class(TComboBox)
   private
    FDualItem: Boolean;
   protected
   public
    constructor Create(AOwner : TComponent); override;

    { Set selection }
    function  SelectedItem: string;            { Returns the selected item }
    function  SelectedItemSafe: string;        { Returns the selected item. No exception if no item selected! }
    function  SelectedItemForce: string;       { Returns the selected item. If no item is selected the it selects the first item first. }
    function  SelectedObject: TObject;

    { Get selection }
    function  SelectItem(CONST ItemText: string): Integer;
    function  SelectFirstItem: Boolean;
    function  SelectObject(AObject: TObject): Boolean;

    { Dual items }
    procedure DrawItem (Index: Integer; Rect: TRect; State: TOwnerDrawState);  override;
    function  SelectDualItem (const ScreenName: string): Integer;
    function  SelectedDualItem: string;
   published
    property IsDualItem: Boolean read FDualItem write FDualItem default FALSE;         { Indicates if the items contained are 'dual items' (two strings per item, separated by '|' ) }
   end;

procedure Register;

IMPLEMENTATION



Constructor TCubicComboBox.Create(AOwner : TComponent);
begin
 inherited Create(AOwner);
 Style := csOwnerDrawFixed;
 Items.NameValueSeparator:= '|';
 // Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
end;




{-----------------------------------------------------------------------------------------------------------------------
   DRAW
-----------------------------------------------------------------------------------------------------------------------}
procedure TCubicComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
VAR s: string;
begin
 // if csCreating in ControlState then exit;

 if Index < 0 then
  begin
    inherited;
    EXIT;
  end;

 if IsDualItem
 then s:= Items.ValueFromIndex[Index]
 else s:= Items[Index];

 Canvas.FillRect(Rect);
 Canvas.TextOut(Rect.Left + 2, Rect.Top + (Rect.Height - Canvas.TextHeight(s)) DIV 2, s);
end;



{-----------------------------------------------------------------------------------------------------------------------
   SET SELECTION
-----------------------------------------------------------------------------------------------------------------------}

function TCubicComboBox.SelectItem(CONST ItemText: string): Integer;  { Selects the item containing the specified text. The search is case Insensitive. If IsDualItem mode is active, it returns the 'internal command' associated with this item }
VAR
   i: Integer;
begin
 Result:= -1;
 if ItemCount = 0 then EXIT;

 if IsDualItem
 then
   begin
    for i:= 0 to Items.Count-1 DO
     if SameText(Items.Names[i], ItemText) then        //Hint: Use Items.ValueFromIndex[ItemIndex] to get the string at ItemIndex
      begin
        Result:= i;
        Break;
      end;
   end
 else
   Result:= Items.IndexOf(ItemText);

 ItemIndex:= Result;  { If no item found, deselect all }
end;


function TCubicComboBox.SelectObject(aObject: TObject): Boolean;
begin
  VAR Index:= Items.IndexOfObject(aObject);
  Result:= Index >= 0;
  if Result
  then ItemIndex := Index;
end;


function TCubicComboBox.SelectFirstItem: Boolean;
begin
  Result:= Items.Count > 0;
  if Result
  then ItemIndex:= 0;
end;






{-----------------------------------------------------------------------------------------------------------------------
   GET SELECTION
-----------------------------------------------------------------------------------------------------------------------}

{ Returns the selected item.
  If IsDualItem mode is active, it returns the 'internal command' associated with this item. }
function TCubicComboBox.SelectedItem: string;
begin
 if IsDualItem
 then Result:= Items.Names[ItemIndex]
 else Result:= Items[ItemIndex];
end;


function TCubicComboBox.SelectedItemSafe: string;
begin
 if (ItemCount <= 0) OR (ItemIndex < 0)
 then Result:= ''
 else
   if IsDualItem
   then Result:= Items.Names[ItemIndex]
   else Result:= Items[ItemIndex];
end;


function TCubicComboBox.SelectedItemForce: string;
begin
 if ItemCount = 0 then EXIT('');
 if ItemIndex < 0
 then ItemIndex:= 0;
 Result:= SelectedItem;
end;


function TCubicComboBox.SelectedObject: TObject;
begin
 if ItemIndex >= 0
 then Result:= Items.Objects[ItemIndex]
 else Result:= NIL;
end;








{-----------------------------------------------------------------------------------------------------------------------
   DUAL ITEMS
-----------------------------------------------------------------------------------------------------------------------}
function TCubicComboBox.SelectedDualItem: string;
begin
 Assert(IsDualItem);
 if ItemIndex < 0
 then Result:= ''
 else Result:= Items.ValueFromIndex[ItemIndex];
end;


function TCubicComboBox.SelectDualItem(CONST ScreenName: string): Integer;  { Selects an item, based on its internal name }
VAR
   i: Integer;
begin
 Assert(IsDualItem);
 if ItemCount = 0 then EXIT(-1);

 Result:= -1;
 for i:= 0 to Items.Count-1 DO
   if SameText(Items.ValueFromIndex[i], ScreenName) then
    begin
     Result:= i;
     Break;
    end;

 ItemIndex:= Result;  { If no item with this name is found, the -1 item (no item) is selected }
end;






procedure Register;
begin
  RegisterComponents('LightSaber', [TCubicComboBox]);
end;

end.
