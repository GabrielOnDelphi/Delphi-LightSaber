UNIT LightVcl.Visual.DropDownSearch;

{=============================================================================================================
   2025.03
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

  A search box with auto-suggest (VCL)
  When you type some text, a dropdown box similar to the Help Insight in Delphi IDE will appear.
  The list is filtered (gets smaller) as the user types in more characters into the searchbox.

  Can be closed with: click, double-click, escape, tab, and enter.
  The list can be navigated with arrow up/down.
  Use PopulateDictionary to add words to your list.

  Also see:
    https://blog.dummzeuch.de/2019/04/28/autocompletion-for-tedits-revisited/
    https://stackoverflow.com/questions/2012208/google-like-edit-combo-control-for-delphi

  Tester:
    C:\Projects\LightSaber\Demo\Tester All Visual Controls
=============================================================================================================}

INTERFACE
USES
   Winapi.Windows, Winapi.Messages,
   System.Classes, System.Types, System.SysUtils, System.Math,
   Vcl.WinXCtrls, Vcl.Controls, Vcl.Dialogs, Vcl.Forms,
   LightVcl.Visual.ListBox;

TYPE
  TSelectNotifyEvent = procedure (Sender: TObject; SelectedItem: TObject) of object;

  TDropDownSearchBox= class(TSearchBox)
   private
     FOnEndSearch: TSelectNotifyEvent;
     FWords: TStringList;  // Field to store all items
     FCurrentIndex: Integer;
     FIsNavigating: Boolean;      // True when the user is navigating through the List with the arrow keys
     FCurrentFilter: string;
     lbxSearch: TCubicListBox;    // Dropdown list
     FMaxDropHeight: Integer;
     procedure showDropDown;
     procedure endSearch(Sender: TObject);
     procedure FilterItems;
     procedure HandleArrowKeys(Key: Word);
     procedure WMKillFocus(var Message: TWMKillFocus); message WM_KillFocus;
     procedure SetHost;
   protected
     procedure KeyDown(var Key: Word; Shift: TShiftState); override;
     procedure Change; override;
     procedure Click; override;
   public
     constructor Create(AOwner: TComponent); override;
     procedure  AfterConstruction; override;
     destructor Destroy; override;

     procedure  PopulateDictionary(Words: TStringList);
     procedure  AddDemoStrings;

     function   SelectedString: string;
     function   SelectedObject: TObject;
   published
     property   OnEndSearch: TSelectNotifyEvent read FOnEndSearch write FOnEndSearch;       { Triggered when the user selected an item from the list }
     property   MaxDropHeight: Integer read FMaxDropHeight write FMaxDropHeight default 50; { In percents }
  end;

procedure Register;



IMPLEMENTATION
USES
   LightCore, LightCore.Time;



{-------------------------------------------------------------------------------------------------------------
   CONSTRUCTOR
-------------------------------------------------------------------------------------------------------------}
constructor TDropDownSearchBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Self
  AlignWithMargins := True;
  TextHint         := 'Search...';    //ToDo: bug: this is not shown

  // New properties
  FWords           := TStringList.Create;  
  FMaxDropHeight   := 50;
  FCurrentIndex    := -1;
  FIsNavigating    := False;
  FCurrentFilter   := '';
end;


procedure TDropDownSearchBox.AfterConstruction;
begin
  inherited;

  lbxSearch             := TCubicListBox.Create(Self);
  lbxSearch.Name        := 'lbxSearch';
  lbxSearch.MultiSelect := FALSE;
  lbxSearch.Sorted      := TRUE;
  //lbxSearch.ItemHeight  := 20;  
  lbxSearch.IntegralHeight := FALSE;  
  lbxSearch.Visible     := FALSE;
  lbxSearch.OnClick     := EndSearch;
  lbxSearch.OnDblClick  := EndSearch;

  Text := '';
end;


{ This is triggered when we click on a different window/app.
  Alternatively one could use TApplicationEvents.OnMessage but here we simply close the dropdown when the control loses focus. }
procedure TDropDownSearchBox.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  lbxSearch.Visible := FALSE;  // Close the dropdown when the control loses focus
end;


destructor TDropDownSearchBox.Destroy;
begin
  FWords.Free;
  inherited Destroy;
end;



{-------------------------------------------------------------------------------------------------------------
   Listbox position
-------------------------------------------------------------------------------------------------------------}
// Set lbxSearch's Parent to be the top-level form rather than the container (which might be too small).
// We compute the screen coordinates of Self and convert them into the host form's client coordinates.
procedure TDropDownSearchBox.SetHost;
const
  DROPDOWN_OFFSET = 2;
var
  Host: TWinControl;
  ScreenPos, HostPos: TPoint;
begin
  if (csDesigning in ComponentState) then EXIT;

  Host := GetParentForm(Self);  // Get the top-level form
  Assert(Host <> NIL, 'Cannot find hosting form!');
  if Host <> nil then
    begin
      // Use the bottom left corner of Self (point at 0,Self.Height) to calculate the position.
      ScreenPos := Self.ClientToScreen(Point(0, Self.Height));
      HostPos   := Host.ScreenToClient(ScreenPos);  // Converts screen coordinates to the form’s client coordinates, positioning the dropdown correctly below the edit box.

      lbxSearch.Parent := Host;
      lbxSearch.Left   := HostPos.X;
	  lbxSearch.Top    := HostPos.Y + DROPDOWN_OFFSET;
      lbxSearch.Width  := Self.Width;
    end;
end;


{-------------------------------------------------------------------------------------------------------------
   Event Handling
-------------------------------------------------------------------------------------------------------------}
procedure TDropDownSearchBox.Change;
begin
  inherited;
  if (csDesigning in ComponentState) then EXIT;

  if NOT FIsNavigating then  // True when the user is navigating through the List with the arrow keys
  begin
    FCurrentFilter := Text;  // Update filter with current text
    FilterItems;             // Filter the dropdown items
    showDropDown;            // Show the dropdown
  end;
end;


procedure TDropDownSearchBox.Click;
begin
  inherited;
  if (csDesigning in ComponentState) then EXIT;

  if lbxSearch.Visible
  then lbxSearch.Visible:= FALSE
  else
    begin
      SetHost;
      FilterItems;
      showDropDown;
    end;
end;


procedure TDropDownSearchBox.showDropDown;
begin
  if lbxSearch.Items.Count > 0
  then
    begin
      //SetHeightAuto(lbxSearch, MaxDropHeight); //Resize it based on the number of rows in it, but never make it bigger than the 1/2 form
      lbxSearch.Visible := TRUE;
      lbxSearch.BringToFront;
    end
  else
    lbxSearch.Visible := FALSE;  // Close the list box if user's text does not match any words.
end;



//todo: Test what happens if the text is not found so the user selects nothing
procedure TDropDownSearchBox.EndSearch(Sender: TObject);
begin
  if (lbxSearch.ItemIndex >= 0) then
    begin
      // This handles when the user clicks an item in the list
      Self.Text:= lbxSearch.SelectedItem;

      if Assigned(FOnEndSearch)
      then FOnEndSearch(Self, lbxSearch.SelectedObject);
    end;

  lbxSearch.Visible := FALSE;
end;


{-------------------------------------------------------------------------------------------------------------
   Key press
-------------------------------------------------------------------------------------------------------------}
procedure TDropDownSearchBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of

    VK_ESCAPE:   // Cancel search on Escape
      begin
        lbxSearch.Visible := False;
        FCurrentFilter := '';
        FilterItems;
        Key := 0;  // Consume the key
      end;

    VK_RETURN:
      if lbxSearch.Visible and (lbxSearch.ItemIndex >= 0) then
      begin
        EndSearch(Self);
        Key := 0;
      end;

    VK_UP, VK_DOWN:
      begin
        HandleArrowKeys(Key);
        Key := 0; // Consume the key
      end;

    VK_TAB:  // 'Tab' key
      begin
        lbxSearch.Visible := False;
        FCurrentFilter := Text;
      end;

    else
      FIsNavigating := FALSE;
  end;
end;


// Navigating only through the filtered list in lbxSearch.Items.
procedure TDropDownSearchBox.HandleArrowKeys(Key: Word);
var
  ItemCount: Integer;
begin
  ItemCount := lbxSearch.Items.Count;
  if ItemCount = 0 then EXIT;

  FIsNavigating := True;
  try
    // If no current index, try to use the listbox's current selection.
    if FCurrentIndex < 0
    then FCurrentIndex := max(lbxSearch.ItemIndex, 0);

    case Key of
      VK_UP:   FCurrentIndex := (FCurrentIndex - 1 + ItemCount) mod ItemCount;
      VK_DOWN: FCurrentIndex := (FCurrentIndex + 1) mod ItemCount;
    end;

    Text := lbxSearch.Items[FCurrentIndex];
    SelectAll;

    // If the dropdown list box was not open open it now
    showDropDown;

    // Update listbox selection to match new index.
    lbxSearch.ItemIndex := FCurrentIndex;
  finally
    FIsNavigating := False;
  end;
end;



{-------------------------------------------------------------------------------------------------------------
   Filtering
-------------------------------------------------------------------------------------------------------------}

{ Show only the items that match what the user typed }
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
    for i := 0 to FWords.Count - 1 do
      if (FilterText = '')
	  OR (PosInsensitive(FilterText, LowerCase(FWords[i])) > 0)
	  then lbxSearch.Items.Add(FWords[i]);
  finally
    lbxSearch.Items.EndUpdate;
  end;

  lbxSearch.SelectFirstItem;
  FCurrentIndex := -1; // Reset current index when filtering
end;




{-------------------------------------------------------------------------------------------------------------
   Population
-------------------------------------------------------------------------------------------------------------}

{ Note: The TStrings object does not own the objects you add this way. Objects added to the TStrings object still exist even if the TStrings instance is destroyed. They must be explicitly destroyed by the application. }
procedure TDropDownSearchBox.PopulateDictionary(Words: TStringList);
begin
  FWords.Assign(Words);    // Store all items in FFullItemList
end;


procedure TDropDownSearchBox.AddDemoStrings;
begin
  VAR TSL:= GetRockBands;
  TRY
    PopulateDictionary(TSL);
  FINALLY
    FreeAndNil(TSL);
  END;
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


{-------------------------------------------------------------------------------------------------------------
   Registration
-------------------------------------------------------------------------------------------------------------}

procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TDropDownSearchBox]);
end;


end.
