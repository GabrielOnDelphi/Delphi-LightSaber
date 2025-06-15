UNIT LightVcl.Visual.CheckListBox;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------

  Features:
      + SelectedItem
=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.CheckLst;      {$WARN GARBAGE OFF}

TYPE
  TCubicCheckListBox = class(TCheckListBox)
   private
   protected
   public
    constructor Create(AOwner : TComponent); override;
    function  SelectedItem: string;            { Returns the selected item }
    function  SelectedItemForce: string;       { Returns the selected item. If no item is selected the it selects the first item first. }
    function  SelectItem(CONST ItemText: string): Integer;
    function  FindItem (const ItemText: string): Integer;
    function  CheckItem(const ItemText: string; Checked: Boolean): Integer;       { Toggle the checkbox for the specified item }
//    procedure DrawItem (Index: Integer; Rect: TRect; State: TOwnerDrawState);  override;
   published
   end;

procedure Register;

IMPLEMENTATION








Constructor TCubicCheckListBox.Create(AOwner : TComponent);
begin
 inherited Create(AOwner);
 Items.NameValueSeparator:= '|';
 // Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
 // if csCreating in ControlState then exit;
end;







function TCubicCheckListBox.SelectItem(CONST ItemText: string): Integer;  { Selects the item containing the specified text. The search is case Insensitive. If IsDualItem mode is active, it returns the 'internal command' associated with this item }
begin
 if Count = 0 then EXIT(-1);
 Result:= Items.IndexOf(ItemText);
 ItemIndex:= Result;  { If no item with this name is found, the -1 item (no item) is selected }
end;


function TCubicCheckListBox.CheckItem(const ItemText: string; Checked: Boolean): Integer;
begin
 Result:= Items.IndexOf(ItemText);
 Self.Checked[Result]:= Checked;
end;


function TCubicCheckListBox.FindItem(CONST ItemText: string): Integer;
begin
 Result:= Items.IndexOf(ItemText);
end;



function TCubicCheckListBox.SelectedItem: string;   { Returns the selected item. If IsDualItem mode is active, it returns the 'internal command' associated with this item }
begin
 if Count = 0 then EXIT('');
 if Count < 0 then EXIT('');
 Result:= Items[ItemIndex];
end;


function TCubicCheckListBox.SelectedItemForce: string;   { Returns the selected item. If no item is selected the it selects the first item first. }
begin
 if Count = 0 then EXIT('');
 if Count < 0 then ItemIndex:= 0;
 Result:= SelectedItem;
end;










{-----------------------------------------------------------------------------------------------------------------------
   DUAL ITEMS
-----------------------------------------------------------------------------------------------------------------------} {
procedure TCubicCheckListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
 inherited;
end;  }










procedure Register;
begin
  RegisterComponents('LightSaber', [TCubicCheckListBox]);
end;




end.{===================================================================================================================








