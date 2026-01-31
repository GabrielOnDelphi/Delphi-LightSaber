UNIT LightVcl.Visual.CheckListBox;

{=============================================================================================================
   Gabriel Moraru
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
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
    function  SelectedItem: string;            { Returns the selected item text, or empty string if nothing is selected }
    function  SelectedItemForce: string;       { Returns the selected item. If no item is selected, selects the first item first }
    function  SelectItem(CONST ItemText: string): Integer;
    function  FindItem (const ItemText: string): Integer;
    function  CheckItem(const ItemText: string; Checked: Boolean): Integer;       { Sets the checkbox state for the item. Returns -1 if not found }
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







{ Selects the item matching the specified text. Returns the index or -1 if not found }
function TCubicCheckListBox.SelectItem(CONST ItemText: string): Integer;
begin
 if Count = 0 then EXIT(-1);
 Result:= Items.IndexOf(ItemText);
 ItemIndex:= Result;  { If no item with this name is found, ItemIndex becomes -1 (no selection) }
end;


{ Sets the checkbox state for the specified item. Returns the item index or -1 if not found }
function TCubicCheckListBox.CheckItem(const ItemText: string; Checked: Boolean): Integer;
begin
 Result:= Items.IndexOf(ItemText);
 if Result >= 0
 then Self.Checked[Result]:= Checked;
end;


function TCubicCheckListBox.FindItem(CONST ItemText: string): Integer;
begin
 Result:= Items.IndexOf(ItemText);
end;



{ Returns the selected item text, or empty string if nothing is selected }
function TCubicCheckListBox.SelectedItem: string;
begin
 if (Count = 0) OR (ItemIndex < 0)
 then EXIT('');
 Result:= Items[ItemIndex];
end;


{ Returns the selected item. If no item is selected, selects the first item first }
function TCubicCheckListBox.SelectedItemForce: string;
begin
 if Count = 0 then EXIT('');
 if ItemIndex < 0
 then ItemIndex:= 0;
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
  RegisterComponents('LightSaber VCL', [TCubicCheckListBox]);
end;




end.{===================================================================================================================








