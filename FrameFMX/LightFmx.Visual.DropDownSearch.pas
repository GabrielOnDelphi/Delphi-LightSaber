unit LightFmx.Visual.DropDownSearch;

{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
  A search box with auto-suggest (FMX).
  When you type some text, a dropdown box similar to the Help Insight in Delphi IDE will appear.
  The list is filtered (gets smaller) as the user types in more characters into the searchbox.

  Can be closed with: click, double-click, escape, tab, and enter.
  The list can be navigated with arrow up/down.
  Use PopulateDictionary to add words to your list.
  Adapted from VCL to FMX, replacing TSearchBox with TEdit and TCubicListBox with TListBox.

  Demo:
    c:\Projects\LightSaber\Demo\FMX\Demo TDropDownSearchBox\FMX_Demo_SearchBoxes.dpr
=============================================================================================================}

INTERFACE
USES
  System.SysUtils, System.Classes, System.Types, System.Math,
  FMX.Controls, FMX.Edit, FMX.ListBox, FMX.Types, FMX.Forms,
  System.UITypes;

TYPE
  TSelectNotifyEvent = procedure (Sender: TObject; SelectedItem: TObject) of object;

  TDropDownSearchBox = class(TEdit)
   private
     FOnEndSearch  : TSelectNotifyEvent;
     FWords        : TStringList;            // Field to store all items
     FCurrentIndex : Integer;
     FIsNavigating : Boolean;                // True when the user is navigating through the List with the arrow keys
     FCurrentFilter: string;
     lbxSearch     : TListBox;               // Dropdown list
     FMaxDropHeight: Integer;
     procedure showDropDown;
     procedure FilterItems;
     procedure HandleArrowKeys(Key: Word);
     procedure endSearch (Sender: TObject);
     procedure EditExit  (Sender: TObject);
     procedure EditTyping(Sender: TObject);
     procedure SetHost;
     procedure SetHeightAuto(Box: TListBox; MaxHeightPercent: Integer);
   protected
     procedure KeyDown(var Key: Word; var KeyChar: Char; Shift: TShiftState); override;
     procedure Click; override;
   public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;

//     procedure ClearDictionary;
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
  Text                := '';
  OnTyping            := EditTyping;
  OnExit              := EditExit;              // Handle focus loss

  // New properties
  FWords           := TStringList.Create;  
  FMaxDropHeight   := 50;
  FCurrentIndex    := -1;
  FIsNavigating    := False;
  FCurrentFilter   := '';

  // List box
  lbxSearch             := TListBox.Create(Self);
  lbxSearch.Name        := 'lbxSearch';
  lbxSearch.MultiSelect := FALSE;
  lbxSearch.Sorted      := TRUE;
  lbxSearch.ItemHeight  := 20;
  lbxSearch.Visible     := FALSE;
  lbxSearch.OnClick     := EndSearch;
  lbxSearch.OnDblClick  := EndSearch;
end;


destructor TDropDownSearchBox.Destroy;
begin
  FreeAndNil(FWords);
  inherited Destroy;
end;

{-------------------------------------------------------------------------------------------------------------
   Listbox position
-------------------------------------------------------------------------------------------------------------}

{ Set lbxSearch's Parent to be the top-level form rather than the container (which might be too small).
  We compute the screen coordinates of Self and convert them into the host form's client coordinates. }
procedure TDropDownSearchBox.SetHost;
const
  DROPDOWN_OFFSET = 2;
var
  Host: TCommonCustomForm;
  ScreenPos, HostPos: TPointF;
begin
  if (csDesigning in ComponentState) then EXIT;

  Host:= Self.Root.GetObject as TCommonCustomForm;
  Assert(Host <> NIL, 'Cannot find hosting form!');

  // Convert screen coordinates to form's client coordinates, positioning the dropdown below the edit box
  ScreenPos:= Self.LocalToScreen(PointF(0, Self.Height));
  HostPos:= Host.ScreenToClient(ScreenPos);

  lbxSearch.Parent     := Host;
  lbxSearch.Position.X := HostPos.X;
  lbxSearch.Position.Y := HostPos.Y + DROPDOWN_OFFSET;
  lbxSearch.Width      := Self.Width;
end;


{-------------------------------------------------------------------------------------------------------------
   Event Handling
-------------------------------------------------------------------------------------------------------------}

procedure TDropDownSearchBox.EditTyping(Sender: TObject);
begin
  if (csDesigning in ComponentState) then EXIT;
  if NOT FIsNavigating then  // True when the user is navigating through the List with the arrow keys
  begin
    FCurrentFilter := Text;  // Update filter with current text
    FilterItems;             // Filter the dropdown items
    showDropDown;            // Show the dropdown
  end;
end;


//ToDo: move this to its own component!
{ Resize the height based on the number of rows, but never bigger than x% of the form.
  MaxHeightPercent is relative to the form height. }
procedure TDropDownSearchBox.SetHeightAuto(Box: TListBox; MaxHeightPercent: Integer);
var
  Form: TCommonCustomForm;
  MaxHeightPx, AvailableSpace, ItemHeightCalc: Single;
  ItemCount: Integer;
begin
  if NOT (Box.Parent is TCommonCustomForm) then EXIT;

  Form:= Box.Parent as TCommonCustomForm;

  // Calculate maximum height based on percentage (e.g., 50%)
  MaxHeightPx:= (Form.ClientHeight * MaxHeightPercent) / 100;

  // Calculate available space from listbox top to form bottom
  AvailableSpace:= Form.ClientHeight - Box.Position.Y - 4;

  // Calculate height based on item count (limit to 10 items max for display)
  ItemCount:= Min(Box.Items.Count, 10);
  ItemHeightCalc:= ItemCount * Box.ItemHeight + 6; // Include padding

  // Set height to the smallest of: item-based height, percentage max, or available space
  Box.Height:= Min(ItemHeightCalc, Min(MaxHeightPx, AvailableSpace));

  // Ensure a minimum height
  if Box.Height < 50
  then Box.Height:= 50;
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
      SetHeightAuto(lbxSearch, MaxDropHeight); //Resize it based on the number of rows in it, but never make it bigger than the 1/2 form
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
      Self.Text := lbxSearch.Items[lbxSearch.ItemIndex];

      if Assigned(FOnEndSearch)
      then FOnEndSearch(Self, lbxSearch.Items.Objects[lbxSearch.ItemIndex]);
    end;

  lbxSearch.Visible := FALSE;
end;


procedure TDropDownSearchBox.EditExit(Sender: TObject);
begin
  lbxSearch.Visible := False;  // Hide dropdown when focus is lost
end;


{-------------------------------------------------------------------------------------------------------------
   Key press
-------------------------------------------------------------------------------------------------------------}
procedure TDropDownSearchBox.KeyDown(var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  inherited;
  case Key of

    vkEscape:   // Cancel search on Escape
      begin
        lbxSearch.Visible := False;
        FCurrentFilter := '';
        FilterItems;
        Key := 0;  // Consume the key
      end;

    vkReturn:
      if lbxSearch.Visible and (lbxSearch.ItemIndex >= 0) then
      begin
        EndSearch(Self);
        Key := 0;
      end;

    vkUp, vkDown:
      begin
        HandleArrowKeys(Key);
        Key := 0; // Consume the key
      end;

    vkTab:
      begin
        lbxSearch.Visible := False;
        FCurrentFilter := Text;
      end;

    else
      FIsNavigating := FALSE;
  end;
end;


{ Navigate through the filtered list using arrow keys.
  Wraps around at the top/bottom of the list. }
procedure TDropDownSearchBox.HandleArrowKeys(Key: Word);
var
  ItemCount: Integer;
begin
  ItemCount:= lbxSearch.Items.Count;
  if ItemCount = 0 then EXIT;

  FIsNavigating:= True;
  try
    // If no current index, try to use the listbox's current selection
    if FCurrentIndex < 0
    then FCurrentIndex:= Max(lbxSearch.ItemIndex, 0);

    case Key of
      vkUp:   FCurrentIndex:= (FCurrentIndex - 1 + ItemCount) mod ItemCount;
      vkDown: FCurrentIndex:= (FCurrentIndex + 1) mod ItemCount;
    end;

    Text:= lbxSearch.Items[FCurrentIndex];
    SelectAll;

    // If the dropdown list box was not open, open it now
    showDropDown;

    // Update listbox selection to match new index
    lbxSearch.ItemIndex:= FCurrentIndex;
  finally
    FIsNavigating:= False;
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

  FilterText:= FCurrentFilter;
  lbxSearch.Items.BeginUpdate;
  try
    lbxSearch.Items.Clear;
    for i:= 0 to FWords.Count - 1 do
      if (FilterText = '')
      OR (PosInsensitive(FilterText, FWords[i]) > 0)  // PosInsensitive is already case-insensitive
      then lbxSearch.Items.Add(FWords[i]);
  finally
    lbxSearch.Items.EndUpdate;
  end;

  if lbxSearch.Items.Count > 0
  then lbxSearch.ItemIndex:= 0;

  FCurrentIndex:= -1; // Reset current index when filtering
end;




{-------------------------------------------------------------------------------------------------------------
   Population
-------------------------------------------------------------------------------------------------------------}

{ Populates the search dictionary with the provided words.
  Note: Assign clears the existing items first, then copies the new ones.
  Objects attached to strings are NOT owned by FWords - they must be freed by the caller. }
procedure TDropDownSearchBox.PopulateDictionary(Words: TStringList);
begin
  FWords.Assign(Words);  // Clears FWords first, then copies all items from Words
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
  if lbxSearch.ItemIndex >= 0 
  then Result := lbxSearch.Items.Objects[lbxSearch.ItemIndex]
  else Result := nil;
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
  RegisterComponents('LightSaber FMX', [TDropDownSearchBox]);
end;


end.
