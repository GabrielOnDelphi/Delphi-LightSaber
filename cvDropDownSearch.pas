UNIT cvDropDownSearch;

{=============================================================================================================
   Gabriel Moraru
   2024.09
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

  Searchbox with auto-suggest.
  A dropdown box similar to the Help Insight in Delphi IDE.
  Displays a list of items. The list is filtered (gets smaller) as the user types in more characters into the searchbox.

  Can be closed with click, doubleclick, esc and enter.

  Tester:
    C:\Projects\LightSaber\Demo\Tester All Visual Controls

  Also see:
    https://blog.dummzeuch.de/2019/04/28/autocompletion-for-tedits-revisited/
    https://stackoverflow.com/questions/2012208/google-like-edit-combo-control-for-delphi

  Tester:
    C:\Projects\LightSaber\Demo\Tester All Visual Controls

=============================================================================================================}

//ToDo: Issue: the drop down does not respond to scroll

INTERFACE
USES
   Winapi.Windows, Winapi.Messages, System.Classes, System.Types, System.SysUtils, Vcl.WinXCtrls, Vcl.Controls,
   vcl.dialogs,
   cvListBox;

TYPE
  TSelectNotifyEvent = procedure (Sender: TObject; SelectedItem: TObject) of object;

  TDropDownSearchBox= class(TSearchBox)
   private
     FOnEndSearch: TSelectNotifyEvent;
     FFullItemList: TStringList;  // New field to store all items
     FCurrentIndex: Integer;
     FIsNavigating: Boolean;
     FCurrentFilter: string;
     procedure showDropDown;
     procedure endSearch(Sender: TObject);
     procedure FilterItems;
     procedure HandleArrowKeys(Key: Word);
     procedure WMKillFocus(var Message: TWMKillFocus); message WM_KillFocus;
   protected
     procedure SetParent(AParent: TWinControl); override;
     procedure KeyPress(var Key: Char); override;
     procedure Change; override;
     procedure KeyDown(var Key: Word; Shift: TShiftState); override;
     procedure Click; override;
   protected
     lbxSearch: TCubicListBox;
   public
     constructor Create(AOwner: TComponent); override;
     procedure  AfterConstruction; override;
     procedure  CreateWindowHandle(const Params: TCreateParams); override;
     destructor Destroy; override;
     procedure  Populate(Objects: TStringList);
     procedure  SetHostParent(aParent: TWinControl);

     function   SelectedString: string;
     function   SelectedObject: TObject;
   published
     property   OnEndSearch: TSelectNotifyEvent read FOnEndSearch write FOnEndSearch;    { Triggered when the user selected an item from the list }
  end;

procedure Register;



IMPLEMENTATION
USES
   ccCore;



{-------------------------------------------------------------------------------------------------------------
   CONSTRUCTOR
-------------------------------------------------------------------------------------------------------------}
constructor TDropDownSearchBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AlignWithMargins := True;
  TextHint         := 'Search...';    //ToDo: bug: this is not shown
  FFullItemList    := TStringList.Create;  // Initialize FFullItemList
  FCurrentIndex := -1;
  FIsNavigating := False;
  FCurrentFilter := '';

  ///OnInvokeSearch   := showDropDown;
end;


procedure TDropDownSearchBox.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited;

end;


procedure TDropDownSearchBox.AfterConstruction;
begin
  inherited;
  if Assigned(lbxSearch) then   //temporary
  begin
    ShowMessage('TDropDownSearchBox exists!');
    raise Exception.Create('Exists!');
  end;

  lbxSearch := TCubicListBox.Create(Self);
  lbxSearch.Name        := 'lbxSearch';
  lbxSearch.Parent      := Self;
  lbxSearch.Width       := Width;
  lbxSearch.Height      := 81;
  lbxSearch.MultiSelect := FALSE;
  lbxSearch.ItemHeight  := 13;
  lbxSearch.Sorted      := TRUE;
  lbxSearch.Visible     := FALSE;
  lbxSearch.OnClick     := EndSearch;
  lbxSearch.OnDblClick  := EndSearch;
  lbxSearch.IntegralHeight := FALSE;

  Text := '';
end;


{ This is triggered when we click on a different window/app.
  Another alternative could be TApplicationEvents.OnMessage but we cannot integrate that into this component. }
procedure TDropDownSearchBox.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  lbxSearch.Visible := FALSE;  // Close the dropdown when the control loses focus
end;


destructor TDropDownSearchBox.Destroy;
begin
  FFullItemList.Free;  // Free FFullItemList
  inherited;
end;

// This is called after lbxSearch was constructed, so we are ok.
procedure TDropDownSearchBox.SetParent(AParent: TWinControl);
begin
  inherited;
  SetHostParent(AParent);
end;


// This WAS called when I install/uninstall the package. So, I cannot raise an exception here.
//Fixed
procedure TDropDownSearchBox.SetHostParent(aParent: TWinControl);
begin
  if aParent = nil then EXIT;
  if (lbxSearch = nil) then EXIT;
  if (csDesigning in ComponentState) then EXIT;

  VAR Pos:= ClientToParent(Point(0, 0), aParent);

  lbxSearch.Parent := aParent;
  lbxSearch.Top    := Pos.Y+ Self.Height+ 2;
  lbxSearch.Left   := Pos.X;
  lbxSearch.Width  := Self.Width;
end;



{-------------------------------------------------------------------------------------------------------------
   XXX
-------------------------------------------------------------------------------------------------------------}
procedure TDropDownSearchBox.Change;
begin
  inherited;
  if not FIsNavigating then
  begin
    FCurrentFilter := Text;
    FilterItems;
    showDropDown;
  end;
end;


procedure TDropDownSearchBox.Click;
begin
  inherited;
  if lbxSearch.Visible
  then lbxSearch.Visible:= FALSE
  else
  begin
    FilterItems;
    showDropDown;
  end;
end;


procedure TDropDownSearchBox.showDropDown;
begin
  if (lbxSearch = NIL) OR (csDesigning in ComponentState) then EXIT;

  if lbxSearch.Items.Count > 0 then
  begin
    lbxSearch.Visible := TRUE;
    lbxSearch.BringToFront;
  end;
end;



// Test what happens if the text is not found so the user selects nothing
procedure TDropDownSearchBox.EndSearch(Sender: TObject);
begin
  if (lbxSearch.ItemIndex >= 0)
  //AND (lbxSearch.SelectedObject <> NIL)
  //AND (Sender = lbxSearch)
  then
    begin
     // This case handles when the user clicks an item in the list
      Self.Text := lbxSearch.SelectedItem;

      if Assigned(FOnEndSearch)
      then FOnEndSearch(Self, lbxSearch.SelectedObject);
    end;

  lbxSearch.Visible := FALSE;
end;


// Cancel search on Escape
procedure TDropDownSearchBox.KeyPress(var Key: Char);
var
  OriginalKey: Char;
begin
  OriginalKey := Key;  // Store the original key value before inherited is called
  inherited KeyPress(Key);  // This will change the Key to #0
  case OriginalKey of
    ESC:
      begin
        lbxSearch.Visible := False;
        FCurrentFilter := '';
        FilterItems;
        Key := #0;  // Consume the key
        Exit;
      end;
    #13:  // 'Enter' key
      if lbxSearch.Visible and (lbxSearch.ItemIndex >= 0) then
      begin
        EndSearch(Self);
        Key := #0;
      end;
    #9:  // 'Tab' key
      begin
        lbxSearch.Visible := False;
        FCurrentFilter := Text;
      end;
    else
      FIsNavigating := False;
  end;
end;


procedure TDropDownSearchBox.HandleArrowKeys(Key: Word);
begin
  if FFullItemList.Count = 0 then EXIT;

  FIsNavigating := True;
  try
    if FCurrentIndex = -1
    then FCurrentIndex := 0;

    case Key of
      VK_UP:   FCurrentIndex := (FCurrentIndex - 1 + FFullItemList.Count) mod FFullItemList.Count;
      VK_DOWN: FCurrentIndex := (FCurrentIndex + 1) mod FFullItemList.Count;
    end;

    Text := FFullItemList[FCurrentIndex];
    SelectAll;
    
    if not lbxSearch.Visible
    then showDropDown;

    // Update listbox selection
    lbxSearch.ItemIndex := lbxSearch.Items.IndexOf(Text);
  finally
    FIsNavigating := False;
  end;
end;


procedure TDropDownSearchBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_UP, VK_DOWN:
      begin
        HandleArrowKeys(Key);
        Key := 0; // Consume the key
      end;
  end;
end;


procedure TDropDownSearchBox.FilterItems;
var
  i: Integer;
  FilterText: string;
begin
  if (lbxSearch = NIL) OR (csDesigning in ComponentState) then EXIT;

  FilterText := LowerCase(FCurrentFilter);
  lbxSearch.Items.BeginUpdate;
  try
    lbxSearch.Items.Clear;
    for i := 0 to FFullItemList.Count - 1 do
      if (FilterText = '') OR (PosInsensitive(FilterText, LowerCase(FFullItemList[i])) > 0) then
        lbxSearch.Items.AddObject(FFullItemList[i], FFullItemList.Objects[i]);
  finally
    lbxSearch.Items.EndUpdate;
  end;

  lbxSearch.SelectFirstItem;
  FCurrentIndex := -1; // Reset current index when filtering
end;



{ Note: The TStrings object does not own the objects you add this way. Objects added to the TStrings object still exist even if the TStrings instance is destroyed. They must be explicitly destroyed by the application. }
procedure TDropDownSearchBox.Populate(Objects: TStringList);
begin 
  FFullItemList.Assign(Objects);  // Store all items in FFullItemList
  FilterItems;                    // Apply initial filtering

  lbxSearch.SetHeightAuto(300, Parent); //Resize it based on the number of rows in it, but never make it bigger than the 1/2 form
end;




{-------------------------------------------------------------------------------------------------------------
   ACCESS
-------------------------------------------------------------------------------------------------------------}
function TDropDownSearchBox.SelectedObject: TObject;
begin
  Result:= lbxSearch.SelectedObject;
end;


function TDropDownSearchBox.SelectedString: string;
begin
  if lbxSearch.ItemIndex >= 0
  then Result := lbxSearch.Items[lbxSearch.ItemIndex]
  else Result := '';
end;



procedure Register;
begin
  RegisterComponents('LightSaber', [TDropDownSearchBox]);
end;


end.


