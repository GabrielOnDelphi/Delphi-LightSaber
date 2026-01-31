UNIT LightVcl.Visual.ListBox;

{=============================================================================================================
   2026.01
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

  Features:
      OnChange event
      Load form file
      Save to file
      Remove empty lines
      Scroll At End
      Delete on 'Del' key
      Remove duplicates
      Remove empty lines
      Add new line but limit total number of lines

=============================================================================================================}

INTERFACE

USES
   Winapi.Windows, Winapi.Messages,
   System.SysUtils, System.Classes, System.Types, System.Math,
   Vcl.Controls, Vcl.Graphics, Vcl.StdCtrls, Vcl.Forms;

TYPE
  TLBNotifications = (lbAdded, lbInserted, lbDeleted);                                          // notifications about changes in listbox
  TLBNotifyEvent = procedure (Sender: TObject; Command: TLBNotifications) of object;            // event declaration

  TCubicListBox= class(TCustomListBox)                                                          // declarat in StdCtrls
   private
    FOnContentChange: TLBNotifyEvent;
    FDeleteKey : Boolean;
    FRClkSel   : Boolean;
    FHintPrefix: string;
    FHintItemAu: Boolean;
    FHintDef   : string;
   protected
    procedure Resize; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {}
    procedure LBADDSTRING(var M: TMessage); message LB_ADDSTRING;
    procedure LBDELETE(var M: TMessage); message LB_DELETESTRING;
    procedure LBINSERT(var M: TMessage); message LB_INSERTSTRING;

   public
    constructor Create(AOwner: TComponent); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetHeightAuto(MaxHeight: Integer; aForm: TControl);

    { Get selected }
    function  SelectedItemI: Integer;
    function  SelectedItemForce: string;                                 { Same as SelectedItem but if no item is selected then force the first item selected }
    function  SelectedItem: string;                                      { returns the item the cursor is on }
    function  SelectedItems: string;                                     { return all selected text }
    function  SelectedObject: TObject;
    function  SelCountEx: Integer;                                       { does the same thing as 'SelCount' but also works when MultiSelect=False. SelCount does not work if MultiSelect=False }

    { Set selected }
    procedure DeSelectAll;
    procedure SelectNext;
    procedure SelectFirstItem;
    procedure SelectLastItem;
    procedure SelectItemSafe(Index: Integer);                            { Try to select the specified item. If the index is invalid no error is thrown and the closes item is selected }
    procedure SelectItem(Obj: TObject);

    { Items }
    procedure SwapItems(x, y: Integer);
    procedure MoveUp;                                                    { Move current item up }
    procedure MoveDown;

    function  FindItem(const aText: string): Integer;

    { Delete items }
    procedure DeleteFirst(iCount: Integer);                              { Delete first x items }
    procedure DeleteSelected(FreeObject: Boolean= FALSE); reintroduce;
    function  RemoveDuplicates: Integer;
    function  RemoveEmptyLines: Integer;
    function  RemoveLongLines(CONST iLength: integer): Integer;          { Remove all lines that are longer than iLength }
    procedure Trim;

    { Visibility }
    procedure NeedScrollBar;                                             { Calculate whether the ScrollBar is needed or not.  Must call 'OnResize' 'OnAddItems' 'OnAssignItems 'OnChanged' }
    procedure ScrollAtTheEnd;
    function  VisibleItems: Integer;

    { I/O }
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile  (FileName: string);

    { Special }
    procedure MoveItemsTo(ListBox: TCubicListBox; iCount: Integer);      { Move the first x items from the curent listbox to the bottom of another  listbox }
    procedure AddLimit   (Msg: string);
   published
    property HintItemAuto     : Boolean read FHintItemAu  Write FHintItemAu default TRUE;       { Show the item under cursor as hint }
    property HintPrefix       : string  read FHintPrefix Write FHintPrefix;                     { User defined text to be added in front of the auto-generated hint }
    property HintDefault      : string  read FHintDef    Write FHintDef;                        { Hint to show when the cursor is not over an item (when the cursor is over item and ShowAutoHint is true the the program shows item's name as hint ) }

    property Respond2DeleteKey: Boolean read FDeleteKey  Write FDeleteKey default TRUE;         { If FALSE then it will behave normal like the original TListBox control. If TRUE it will activate the enhancement: it will move the cursor to the next line after 'DeleteSelected' procedure was called. }
    property MultiSelect                                                  default TRUE;
    property RightClickSelects: Boolean read FRClkSel    Write FRClkSel   default TRUE;         { Pressing the Right Mouse Button will toggle the selection of the item under cursor }
    property OnContentChange  : TLBNotifyEvent read FOnContentChange write FOnContentChange;    { Triggered when a new line is entered/removed }

    property Style;
    property AutoComplete;
    property AutoCompleteDelay;
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property Items;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollWidth;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Touch;
    property Visible;
    property StyleElements;
    property OnClick;
    property OnContextPopup;
    property OnData;
    property OnDataFind;
    property OnDataObject;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
 end;

procedure Register;

IMPLEMENTATION

USES
  LightVcl.Common.VclUtils, LightVcl.Common.Sound, LightCore, LightCore.Math, LightCore.TextFile;




constructor TCubicListBox.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);       // Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
 FDeleteKey := TRUE;
 MultiSelect:= TRUE;
 FHintItemAu:= TRUE;
 FRClkSel   := TRUE;                 { Pressing the Right Mouse Button will toggle the selection of the item under cursor }
end;


procedure TCubicListBox.Resize;
begin
 inherited;
 NeedScrollBar;
end;



{ Resize the height based on the number of rows in it, but never exceeds MaxHeight% of aForm.
  MaxHeight: Maximum height as percentage of aForm's height (e.g., 50 = 50%).
  aForm: Parent control used to calculate maximum allowed height. }
procedure TCubicListBox.SetHeightAuto(MaxHeight: Integer; aForm: TControl);
begin
  Assert(aForm <> NIL, 'SetHeightAuto requires a valid form reference!');
  Assert(MaxHeight > 0, 'MaxHeight must be greater than 0!');

  VAR MaxHeightPx := (aForm.Height * MaxHeight) DIV 100;
  VAR ItemCount   := Min(Items.Count, 10);  // Limit to 10 items for height calculation
  VAR iHeight     := ItemCount * ItemHeight;

  iHeight:= iHeight + 6;                    // Padding for border and potential scrollbar
  iHeight:= Max(Min(iHeight, MaxHeightPx), 100);  // Keep within bounds

  Height:= iHeight;
end;


{--------------------------------------------------------------------------------------------------
   Remove
--------------------------------------------------------------------------------------------------}

{ Removes duplicate entries (case-insensitive comparison).
  Returns the number of items removed.
  Uses a temporary TStringList to avoid slow UI refreshes during deletion.
  Note: O(n²) algorithm - may be slow for very large lists (10000+ items). }
function TCubicListBox.RemoveDuplicates: Integer;
VAR i1, i2, IsBreakTime: Integer;
    TSL: TStringList;
begin
 TSL:= TStringList.Create;
 TRY
  TSL.BeginUpdate;
  TSL.Assign(Items);     { Don't delete directly from Items because it will refresh and be too slow }
  Result:= 0;
  IsBreakTime:= 0;

  for i1:= TSL.Count-1 downto 1 DO
   begin
    { Refresh GUI periodically during long operations }
    inc(IsBreakTime);
    if IsBreakTime > 8000 then
     begin
      Update;
      IsBreakTime:= 0;
     end;

    for i2:= i1-1 downto 0 DO
     if SameText(TSL.Strings[i1], TSL.Strings[i2]) then
      begin
       TSL.Delete(i1);
       inc(Result);
       Break;
      end;
   end;

  TSL.EndUpdate;
  Items.Assign(TSL);
 FINALLY
   FreeAndNil(TSL);
 END;
end;


function TCubicListBox.RemoveEmptyLines: Integer;
VAR i: Integer;
begin
 Result:= 0;
 for i:= Items.Count-1 downto 0 DO
  if (Items[i]= '') OR (Items[i]= ' ')
  then begin
         Inc(Result);
         Items.Delete(i);
       end;
end;


function TCubicListBox.RemoveLongLines(CONST iLength: integer): Integer;                           { remove all lines that are longer than iLength }
VAR i: Integer;
    TSL: TStringList;
begin
 TSL:= TStringList.Create;
 TRY
  TSL.Assign(Items);    { don't delete directly from SELF because it will refresh and be too slow }
  Result:= 0;
  for i:= TSL.Count-1 downto 0 DO
    if length(TSL.Strings[i])> iLength then
     begin
      TSL.Delete(i);
      inc(Result);
     end;
  Items.Assign(TSL);
 FINALLY
  FreeAndNil(TSL);
 END;
end;




{--------------------------------------------------------------------------------------------------
   MOUSE
--------------------------------------------------------------------------------------------------}
procedure TCubicListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
VAR sTextUnderCursor: string;
    itm: integer;
begin
 inherited;

 if  HintItemAuto
 AND ShowHint
 AND (Items.Count> 0) then
  begin
    itm:= ItemAtPos(Point(X,Y), TRUE);
    if (itm< 0) then
     begin
      Hint:= HintDefault;
      if HintDefault<> ''
      then Application.CancelHint;
     end
    else
     begin
      sTextUnderCursor:= Items[itm];                                                               { Get text under cursor }
      if Hint<> sTextUnderCursor
      then
       begin
        Hint:= HintPrefix+ sTextUnderCursor;
        Application.CancelHint;                                                                    { similar:  http://delphi.newswhat.com/geoxml/forumhistorythread?groupname=borland.public.delphi.vcl.components.writing.win32&messageid=40812beb$1@newsgroups.borland.com     }
       end;
     end;
  end;
end;


procedure TCubicListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
VAR
   ItemUnderMouse: Integer;
   Point: TPoint;
begin
 { Pressing the Right Mouse Button will TOGGLE the selection of the item under cursor }
 if (Button= mbRight)
 AND RightClickSelects then
  begin
   LightVcl.Common.VclUtils.SetFocus(self);
   Point.X:= X;
   Point.Y:= Y;
   ItemUnderMouse:= ItemAtPos(Point, TRUE);

   if ItemUnderMouse> -1 then
    begin
     Selected[ItemUnderMouse]:= NOT Selected[ItemUnderMouse];
     ItemIndex:= ItemUnderMouse;
    end;
  end;

 inherited;                                                                                         { defaul behavior }
end;




{--------------------------------------------------------------------------------------------------
   SELECTION
--------------------------------------------------------------------------------------------------}
function TCubicListBox.SelCountEx: Integer;         { does the same thing as 'SelCount' but also works when MultiSelect=False. SelCount does not work if MultiSelect=False }
VAR i: Integer;
begin
 Result:= 0;
 for i:= 0 to (Items.Count- 1) DO
   if Selected[i]
   then inc(Result);
end;


{ Improvements:
   * After deleting the selected item(s), move the cursor down one position.
   * Supports multiple selection! }
procedure TCubicListBox.DeleteSelected(FreeObject: Boolean= FALSE);
VAR i, Current: Integer;
begin
 if SelCountEx> 0
 then
   begin
     Current:= -1;
     for i:= Items.Count-1 downto 0 DO
       if Selected[i] then
         begin
           Current:= i;                      { Remember the last item selected }
           VAR Obj:= Items.Objects[i];
           if FreeObject and (Obj <> nil) then Obj.Free;
           Items.Delete(i);
         end;

     if Items.Count = 0 then EXIT;

     { MOVE THE CURSOR DOWN }
     if Current < 0                          { If nothing was selected, I select the first item }
     then Current:= 0
     else
       if Current >= Items.Count             { If I can go down, move cursor to the next row... }
       then Current:= Items.Count-1;         { ...if not, select the last item. }

     Selected[Current]:= TRUE;
     ItemIndex:= Current;
   end
 else BipErrorShort;
end;


{ Deletes the first iCount items from the list.
  Note: Always delete at index 0 because items shift down after each deletion. }
procedure TCubicListBox.DeleteFirst(iCount: Integer);
VAR i: Integer;
begin
 if iCount <= 0 then EXIT;
 if iCount > Count then iCount:= Count;

 for i:= 1 to iCount DO
  Items.Delete(0);
end;


procedure TCubicListBox.KeyDown(VAR Key: Word; Shift: TShiftState);  { Delete the selected items if the user press the DELETE key }
begin
 if  Respond2DeleteKey
 AND (Key= VK_DELETE)
 then DeleteSelected;
 inherited;
end;



{--------------------------------------------------------------------------------------------------
   SET
--------------------------------------------------------------------------------------------------}

{ Does not crash if we try to select an item that does not exist }
procedure TCubicListBox.SelectItemSafe(Index: Integer);
begin
  if Count < 1 then EXIT;
  if Index < 0
  then ItemIndex:= 0
  else
     if Index > Count-1
     then ItemIndex:= Count-1
     else ItemIndex:= Index;

  Selected[ItemIndex]:= TRUE;
end;


procedure TCubicListBox.SelectItem(Obj: TObject);
begin
  for var i:= 0 to Count - 1 DO
   if Items.Objects[i] = Obj then
     begin
       ItemIndex:= i;
       Selected[i]:= TRUE;
       EXIT;
     end;
end;


procedure TCubicListBox.SelectFirstItem;
begin
  if Count= 0 then EXIT;
  ItemIndex:= 0;
  Selected[0]:= TRUE
end;


procedure TCubicListBox.SelectLastItem;
begin
  if Count= 0 then EXIT;
  ItemIndex:= Count-1;
  Selected[Count-1]:= TRUE;
end;


procedure TCubicListBox.SelectNext;
begin
  if ItemIndex+ 1 < Count
  then
   begin
    //Selected[ItemIndex]:= FALSE;  { Deselect cur item }
    ItemIndex:= ItemIndex+ 1;
    Selected[ItemIndex]:= TRUE  { Select next item }
   end
  else BipErrorShort;
end;


procedure TCubicListBox.DeselectAll;
VAR I: Integer;
begin
  for I:= 0 to Items.Count- 1
    DO Selected[I]:= FALSE;
end;



{--------------------------------------------------------------------------------------------------
   GET
--------------------------------------------------------------------------------------------------}

{ Returns the text of the first selected item, or empty string if nothing is selected. }
function TCubicListBox.SelectedItem: string;
VAR SelIdx: Integer;
begin
 SelIdx:= SelectedItemI;
 if SelIdx > -1
 then Result:= Items[SelIdx]
 else Result:= '';
end;


{ Returns the first selected item. Subsequent selected items are ignored. }
function TCubicListBox.SelectedItemI: Integer;
VAR i: Integer;
begin
 Result:= -1;
 //if Items.Count> 0 then
 for i:= 0 TO Items.Count-1 DO
   if Selected[i] then EXIT(i);
end;


{ Same as SelectedItem but if no item is selected then force the first item selected }
function TCubicListBox.SelectedItemForce: string;
begin
 if Count< 1 then EXIT('');

 if ItemIndex < 0
 then ItemIndex:= 0;

 Result:= SelectedItem;

 if Result= '' then
  begin
   Selected[0]:= TRUE;
   Result:= Items[0];
  end;
end;


function TCubicListBox.SelectedItems: string;   { Return all selected text }
VAR i: Integer;
begin
 Result:= '';
 if Items.Count> 0 then
 for I:= 0 TO Items.Count-1 DO
   if Selected[i]
   then Result:= Result+ Items[i]+ CRLFw;
end;


function TCubicListBox.SelectedObject: TObject;
begin
 if ItemIndex >= 0
 then Result:= Items.Objects[ItemIndex]
 else Result:= NIL;
end;




{--------------------------------------------------------------------------------------------------
   Swap/Move
--------------------------------------------------------------------------------------------------}
procedure TCubicListBox.MoveUp;   { Move current item up }
begin
 if ItemIndex < 0 then EXIT;           { No item selected }
 if ItemIndex = 0 then EXIT;           { We cannot get higher than this }
 SwapItems(ItemIndex, ItemIndex -1);
 ItemIndex:= ItemIndex- 1;
end;


procedure TCubicListBox.MoveDown;
begin
 if ItemIndex < 0 then EXIT;           { No item selected }
 if ItemIndex = Items.Count-1 then EXIT;  { We cannot get lower than this }
 SwapItems(ItemIndex, ItemIndex +1);
 ItemIndex:= ItemIndex+ 1;
end;


{ Swaps two items including their associated Objects. }
procedure TCubicListBox.SwapItems(x, y: Integer);
VAR
  TempStr: string;
  TempObj: TObject;
begin
 TempStr:= Items[x];
 TempObj:= Items.Objects[x];

 Items[x]:= Items[y];
 Items.Objects[x]:= Items.Objects[y];

 Items[y]:= TempStr;
 Items.Objects[y]:= TempObj;
end;













{--------------------------------------------------------------------------------------------------
   LOAD/SAVE to file
--------------------------------------------------------------------------------------------------}
procedure TCubicListBox.LoadFromFile(FileName: string);
begin
  if FileExists(FileName)
  then Items.Text:= StringFromFile(FileName);
end;


procedure TCubicListBox.SaveToFile(FileName: string);
begin
 StringToFile(FileName, Items.Text, woOverwrite);
end;




{ This MIGHT make the control slow when I add (via Self.Text:= xxx) with 100000+ lines. But I measured the time and I haven't seen it }
procedure TCubicListBox.LBADDSTRING(var M: TMessage);
begin
 inherited;
 if Assigned(FOnContentChange)
 then FOnContentChange(Self, lbAdded);
end;


procedure TCubicListBox.LBINSERT(var M: TMessage);
begin
 inherited;
 if Assigned(FOnContentChange)
 then FOnContentChange(Self, lbInserted);
end;


procedure TCubicListBox.LBDELETE(var M: TMessage);
begin
 inherited;
 if Assigned(FOnContentChange)
 then FOnContentChange(Self, lbDeleted);
end;




{ Add a new line. Make sure that the total no of line does go out of screen }
procedure TCubicListBox.AddLimit(Msg: string);
VAR Surplus: Integer;
begin
 Surplus:= Count - VisibleItems;  { Delete extra lines at the top }
 DeleteFirst(Surplus);
 Items.Add(Msg);
end;


{ Returns the number of fully visible items in the listbox. }
function TCubicListBox.VisibleItems: Integer;
begin
 if ItemHeight <= 0
 then Result:= 0
 else Result:= RoundDown(ClientHeight / ItemHeight) - 1;
end;


procedure TCubicListBox.ScrollAtTheEnd;
begin
 TopIndex:= Items.Count- 1;                                                                        { How to Jump to the Last Item in the TListBox }
end;


{ Moves the first iCount items from this listbox to the bottom of another listbox.
  Note: Always operates on index 0 since items shift down after each deletion. }
procedure TCubicListBox.MoveItemsTo(ListBox: TCubicListBox; iCount: Integer);
VAR i: Integer;
begin
 Assert(ListBox <> NIL, 'Target ListBox cannot be nil!');
 if iCount <= 0 then EXIT;
 if iCount > Count then iCount:= Count;

 for i:= 1 to iCount DO
  begin
   ListBox.Items.AddObject(Items[0], Items.Objects[0]);
   Items.Delete(0);
  end;
end;


{ Calculate whether the horizontal ScrollBar is needed based on the widest item.
  Note: ScrollWidth > ClientWidth shows the scrollbar, ScrollWidth <= ClientWidth hides it. }
Procedure TCubicListBox.NeedScrollBar;
VAR i, LastWidth, CurWidth: Integer;
begin
 LastWidth:= 0;
 for i:= 0 to (Items.Count - 1) DO
  begin
   CurWidth:= Canvas.TextWidth(Items[i]);
   if CurWidth > LastWidth
   then LastWidth:= CurWidth;
  end;

 ScrollWidth:= LastWidth + 6;
end;


function TCubicListBox.FindItem(const aText: string): Integer;    { Returns position of specified item }
VAR i: Integer;
begin
 Result:= -1;
 for i:= 0 to Items.Count-1 DO
  if SameText(aText, Items[i])
  then EXIT(i)
end;


procedure TCubicListBox.Trim;
VAR i: Integer;
begin
 for i:= 0 to Items.Count-1
   DO Items[i]:= System.SysUtils.trim(Items[i]);
end;









procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TCubicListBox]);
end;


end.
