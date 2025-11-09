UNIT LightVcl.Visual.FileListBox;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------

  Improvements:
     DelDeletesFile  - Delete item on "Del" key press (also see DelDeletesFile)
     DelShowConfirm  - Shows confirmation box before deleting a file
     DeselectAll
     SelectFirstItem
     SelectItemClick - Deselect all current items and select specified item then simulate a click on it
     SelectItem      - Deselect all current items and select specified item
     SelectedItem
     SimulateClickCurItem - Simulate a click on current item
     OnChangeEx      - Triggers whenever:
                           * A new file was selected
                           * The folder was changed.
                        Also returns as OUT variable the currently selected item and the previously selected item
}

{.$WARN UNIT_PLATFORM OFF}   {Silence the 'W1005 Unit Vcl.FileCtrl is specific to a platform' warning }

INTERFACE

USES
   Winapi.Windows,
   System.SysUtils, System.Classes, System.Types,
   Vcl.Controls, Vcl.Forms, Vcl.FileCtrl;

TYPE
  TOnSelChanged = procedure (Sender: TObject; OUT CurrentItem, PreviousItem: Integer) of object;

  TCubicFileList= class(TFileListBox)
   private
     FDeleteFile: Boolean;
     FDelConfirm: Boolean;
     FBeforeDelete: TNotifyEvent;  { UNUSED }
     FAfterDelete: TNotifyEvent;
     FHintItemAu: Boolean;
   protected
     FOnSelChanged : TOnSelChanged;
   protected
     procedure Change; override;
   public
     constructor Create(AOwner: TComponent); override;
     { Selection }
     function  MoveSelectionUp : Boolean;                                                               { Returns true if it succesfully moved the selection }
     function  MoveSelectionDwn: Boolean;
     procedure InvertSelection;
     procedure DeselectAll;
     procedure SelectFirstItem;
     procedure SelectItemClick(CONST ItemPos: Integer);                                                 { Deselect all current items and select specified item then simulate a click on it }
     procedure SimulateClickCurItem;                                                                    { Simulate a click on current item }
     procedure SimulateClickCurItemForce;                                                               { Simulate a click on current item. If no item is selected, then select the first on  }
     procedure SelectItem     (CONST ItemPos: Integer);    overload;                                    { Deselect all current items and select specified item }
     procedure SelectItem     (ItemName: string);          overload;                                    { Select specified item. The name must be incomplete }
     function  SelectionCount: Integer;                                                                 { Returns the number of selected items }
     function  SelectedItem: string;
     { Delete file }
     procedure DeleteCurrentFile;                                                                       { To Recycle Bin }
     procedure DeleteSelectedAndFocus;
     procedure KeyUp          (VAR Key: Word; Shift: TShiftState); override;
     procedure ReadFileNames;  override;
     procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
   published
     property HintItemAuto   : Boolean        read FHintItemAu    Write FHintItemAu default TRUE;       { show the item under cursor as hint }
     property OnChangeEx     : TOnSelChanged  read FOnSelChanged  write FOnSelChanged;                  { Takes place when the user selects a new file }
     property OnBeforeDelete : TNotifyEvent   read FBeforeDelete  write FBeforeDelete;
     property OnAfterDelete  : TNotifyEvent   read FAfterDelete   write FAfterDelete;
     property DelDeletesFile : Boolean        read FDeleteFile    write FDeleteFile default TRUE;       { daca TRUE, apasarea tastei Delete are ca efect stergerea fisierului de pe disk, daca nu, sterge doar itemul de pe ecran }
     property DelShowConfirm : Boolean        read FDelConfirm    write FDelConfirm default TRUE;       { Shows confirmation box before deleting a file }
  end;

procedure Register;



IMPLEMENTATION
USES
   LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.Dialogs, LightVcl.Common.IO;



constructor TCubicFileList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDeleteFile:= TRUE;
  FDelConfirm:= TRUE;
  FHintItemAu:= TRUE;
  Hint:= 'Press the "Delete" key to delete the selected file from disk.';
end;

procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TCubicFileList]);
end;




{--------------------------------------------------------------------------------------------------
   UPDATE
--------------------------------------------------------------------------------------------------}
procedure TCubicFileList.ReadFileNames;                                                            { This makes the control to keep the current file selected when updating }
VAR SelItem: string;
begin
 SelItem:= SelectedItem;
 inherited;                                                                                        { Refresh content }
 SelectItem(SelItem);                                                                              { Restore selection }
end;





{--------------------------------------------------------------------------------------------------
   DO SELECT ITEM
--------------------------------------------------------------------------------------------------}

procedure TCubicFileList.SelectFirstItem;
begin
 if Count> 0
 then SelectItem(0);
end;


procedure TCubicFileList.SelectItem(CONST ItemPos: Integer);                                       { Deselect all current items and select specified item }
begin
 Assert(ItemPos> -1);
 Assert(ItemPos< Count, 'TCubicFileList.SelectItem ItemPos< Count');
 //del  nu e bine asa ca la plecare itemindex=itempos: if ItemIndex= ItemPos then EXIT;                                               { If the item is already selected, ignore it }

 DeselectAll;
 if ItemPos< Count then
  begin
   Selected[ItemPos]:= TRUE;
   ItemIndex:= ItemPos;
   if Assigned(FOnChange) then FOnChange(Self);
  end;
end;


procedure TCubicFileList.SelectItem(ItemName: string);                                       { Select specified item. The name must be incomplete }
VAR i: Integer;
begin
 if Count<= 0 then EXIT;                                                                           { No files in list }

 ItemName:= ExtractFileName(ItemName);
 if SameText(SelectedItem, ItemName)                                                               { If the item is already selected, ignore it }
 then EXIT;

 for i:= 0 to Count-1 DO
  if SameText(Items[i], ItemName) then
   begin
     SelectItem(i);
     Break;
   end;
end;


procedure TCubicFileList.SelectItemClick(CONST ItemPos: Integer);                                  { Deselect all current items, select specified item then simulate a click on it }
begin
 if Count> 0 then
  begin
   SelectItem(ItemPos);
   Click;
  end;
end;


procedure TCubicFileList.SimulateClickCurItem;                                                     { Simulate a click on current item  }
begin
 if ItemIndex>= 0
 then Click;
end;


procedure TCubicFileList.SimulateClickCurItemForce;                                                { Simulate a click on current item. If no item is selected, then select the first on  }
begin
 if Count= 0 then EXIT;

 if SelectionCount= 0
 then SelectFirstItem;
 Click;
end;



{--------------------------------------------------------------------------------------------------
   SELECTION
--------------------------------------------------------------------------------------------------}
function TCubicFileList.SelectedItem: string;                                                      { Returns the selected item }
begin
 if (ItemIndex> -1) AND (Count> -1)
 then Result:= Items[ItemIndex]
 else Result:= '';
end;


procedure TCubicFileList.InvertSelection;
VAR i: Integer;
begin
 for i:= 0 to Count-1 DO
  Selected[i]:= NOT Selected[i];
end;


procedure TCubicFileList.DeselectAll;
VAR i: Integer;
begin
 for i:= 0 to Count-1 DO
  Selected[i]:= FALSE;
end;


function TCubicFileList.SelectionCount: Integer;                                                   { Returns the number of selected items }
VAR i: Integer;
begin
 Result:= 0;
 for i:= 0 to Count-1 DO
  if Selected[i] then Inc(Result);
end;





{--------------------------------------------------------------------------------------------------
   MOVE SELECTION
--------------------------------------------------------------------------------------------------}
function TCubicFileList.MoveSelectionUp : Boolean;                                                 { Returns true if it succesfully moved the selection }
VAR temp, PrevItem: Integer;
begin
 Result:= (Items.Count> 0) AND (ItemIndex> 0);                                                     { daca e deja selectat primul item, nu pot sa ma duc mai sus }
 if NOT Result then EXIT;

 PrevItem:= ItemIndex;
 SelectItem(ItemIndex-1);

 temp:= ItemIndex;
 if Assigned(FOnSelChanged)
 then FOnSelChanged(Self, temp, PrevItem);
end;


function TCubicFileList.MoveSelectionDwn: Boolean;
VAR temp, PrevItem: Integer;
begin
 Result:= (Items.Count> 0) AND (ItemIndex< Items.Count-1);                                         { The selection is already on the last row }
 if NOT Result then EXIT;

 PrevItem:= ItemIndex;
 SelectItem(ItemIndex+1);

 temp:= ItemIndex;
 if Assigned(FOnSelChanged)
 then FOnSelChanged(Self, temp, PrevItem);
end;




{--------------------------------------------------------------------------------------------------
   SELECTION CHANGED
--------------------------------------------------------------------------------------------------}
procedure TCubicFileList.Change;
VAR temp: Integer;
begin
  temp:= ItemIndex;

  if (Count>0)
  AND (FLastSel<> ItemIndex)
  AND Assigned(FOnSelChanged)
  then FOnSelChanged(Self, temp, FLastSel);
  inherited Change;
end;





(*
procedure TCubicFileList.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
VAR PrevItem: Integer;
begin
 PrevItem:= ItemIndex;
 inherited; x
 if (Count> 0)
 AND ( PrevItem<> ItemIndex )                                                                      { am dat click pe un item, altul fata de cel current selectat
 AND Assigned(FOnSelChanged)
 then FOnSelChanged(Self, PrevItem);
end;

procedure TCubicFileList.KeyDown(VAR Key: Word; Shift: TShiftState);
begin
 inherited KeyDown(Key, Shift);
 if (Count> 0) AND ( (Key= VK_DOWN) OR (Key= VK_UP) )                                              { am apasat tasta Sageata Sus/Jos ? }
 then
   if Assigned(FOnSelChanged)
   then FOnSelChanged(Self);
end; *)





{--------------------------------------------------------------------------------------------------
   DELETE ITEMS
--------------------------------------------------------------------------------------------------}
procedure TCubicFileList.KeyUp(VAR Key: Word; Shift: TShiftState);
begin
 if (Key= VK_DELETE)                                                            { User pressed DELETE key ? }
 AND DelDeletesFile
 then DeleteCurrentFile;

 inherited;
end;


{ Delete current item from disk without refreshing the listbox.
  This is useful because in case of a large folder, re-reading the files in the folder could take a while.
  Deletes to Recycle Bin }
procedure TCubicFileList.DeleteCurrentFile;
VAR CurItem: Integer;
begin
 if (Count < 1) then EXIT;

 if FileExistsMsg(FileName) then
  begin
    CurItem:= ItemIndex;

    { Event - Delete file }
    if Assigned(FBeforeDelete)   { UNUSED }
    then FBeforeDelete(Self);

    { Delete to recycle }
    if NOT RecycleItem(FileName, TRUE, DelShowConfirm)
    then MessageWarning('File not deleted.'+ CRLFw+ FileName);

    DeleteSelected;                                                                              { Delete list box item }

    { Move cursor to next file }
    if CurItem>= Count
    then CurItem:= Count-1;
    if Count > 0
    then Selected[CurItem]:= TRUE;                                             { pune cursorul pe urmatoarea linie }

    { Event - Delete file }
    if Assigned(FAfterDelete)
    then FAfterDelete(Self);
  end;
end;


procedure TCubicFileList.DeleteSelectedAndFocus;  { Delete selected item (not file) }
VAR CurItem: Integer;
begin
 if (Count < 1) then EXIT;

 CurItem:= ItemIndex;
 DeleteSelected;                                                                              { Delete list box item }

 { Move cursor to next file }
 if CurItem>= Count then CurItem:= Count-1;
 if Count > 0 then Selected[CurItem]:= TRUE;                                             { pune cursorul pe urmatoarea linie }
end;









procedure TCubicFileList.MouseMove(Shift: TShiftState; X, Y: Integer);
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
      if Hint<> ''
      then Application.CancelHint;
     end
    else
     begin
      sTextUnderCursor:= Items[itm];                                                               { Get text under cursor }
      if Hint<> sTextUnderCursor
      then
       begin
        Hint:= sTextUnderCursor;
        Application.CancelHint;                                                                    { CEVA ASEMANATOR  http://delphi.newswhat.com/geoxml/forumhistorythread?groupname=borland.public.delphi.vcl.components.writing.win32&messageid=40812beb$1@newsgroups.borland.com     }
       end;
     end;
  end;
end;



end.
