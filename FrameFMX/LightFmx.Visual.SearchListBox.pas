UNIT LightFmx.Visual.SearchListBox;

{=============================================================================================================
   2026.01
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   A searchable listbox component with an integrated search filter.

   Features:
     - Edit field at top for entering search text
     - Listbox below that filters items as user types
     - Case-insensitive search
     - Remembers and restores selection when clearing search
     - Supports integer Tag values for enum mapping
     - Auto-selects first match when searching

   Usage:
     SearchListBox.Items.Add('Item 1');
     SearchListBox.Items.Add('Item 2');
     SearchListBox.SetItems(MyStringArray);  // Or use SetItems for bulk loading with tags

     // Get selected item
     SelectedText := SearchListBox.SelectedText;
     SelectedTag  := SearchListBox.SelectedTag;

   Events:
     OnSelectionChanged - Triggered when user selects an item

   Demo:
     c:\Projects\LightSaber\Demo\FMX\Demo TDropDownSearchBox\FMX_Demo_SearchBoxes.dpr

=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes, System.Types, System.UITypes,
  FMX.Controls, FMX.Layouts, FMX.Edit, FMX.ListBox, FMX.Types, FMX.StdCtrls, FMX.SearchBox;

TYPE
  TLightSearchListBox = class(TLayout)
  private
    FSearchEdit: TSearchBox;
    FListBox: TListBox;
    FAllItems: TStringList;          // Stores all items (unfiltered)
    FOnSelectionChanged: TNotifyEvent;

    function  GetSearchText: string;
    procedure SetSearchText(const Value: string);
    function  GetSelectedIndex: Integer;
    procedure SetSelectedIndex(const Value: Integer);
    function  GetSelectedText: string;
    function  GetSelectedTag: Integer;
    function  GetItemCount: Integer;

    procedure SearchEditChangeTracking(Sender: TObject);
    procedure ListBoxChange(Sender: TObject);
    procedure FilterItems;
    procedure CreateChildControls;
  protected
    procedure Resize; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure SetItems(const Items: array of string);
    procedure AddItem(const Text: string; Tag: Integer = 0);
    procedure Clear;

    { Select item by tag value (useful for enum selection) }
    function SelectByTag(Tag: Integer): Boolean;

    { Direct access to internal controls for advanced customization }
    property SearchEdit: TSearchBox read FSearchEdit;
    property ListBox: TListBox read FListBox;

    { Current search text }
    property SearchText: string read GetSearchText write SetSearchText;

    { Selected item properties }
    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex;
    property SelectedText: string read GetSelectedText;
    property SelectedTag: Integer read GetSelectedTag;

    { Number of visible (filtered) items }
    property ItemCount: Integer read GetItemCount;
  published
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property Align;
    property Anchors;
    property Position;
    property Size;
    property Width;
    property Height;
    property Visible;
    property Enabled;
  end;


procedure Register;


IMPLEMENTATION

CONST
  SEARCH_EDIT_HEIGHT = 24;  // Height of the search edit field
  SPACING = 4;              // Gap between search edit and listbox


{-------------------------------------------------------------------------------------------------------------
   CONSTRUCTOR / DESTRUCTOR
-------------------------------------------------------------------------------------------------------------}

constructor TLightSearchListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAllItems:= TStringList.Create;

  // Default size
  Width:= 200;
  Height:= 250;
end;


destructor TLightSearchListBox.Destroy;
begin
  FreeAndNil(FAllItems);
  inherited;
end;


{ Create child controls after component is fully loaded (design-time) or when first needed (runtime) }
procedure TLightSearchListBox.Loaded;
begin
  inherited;
  CreateChildControls;
end;


procedure TLightSearchListBox.CreateChildControls;
begin
  // Already created?
  if FSearchEdit <> NIL then EXIT;

  // Create search edit field
  FSearchEdit:= TSearchBox.Create(Self);
  FSearchEdit.Parent:= Self;
  FSearchEdit.Stored:= False;  // Prevent streaming to FMX file (avoids duplicate children)
  FSearchEdit.Align:= TAlignLayout.Top;
  FSearchEdit.Height:= SEARCH_EDIT_HEIGHT;
  FSearchEdit.OnChangeTracking:= SearchEditChangeTracking;

  // Create listbox
  FListBox:= TListBox.Create(Self);
  FListBox.Parent:= Self;
  FListBox.Stored:= False;  // Prevent streaming to FMX file (avoids duplicate children)
  FListBox.Align:= TAlignLayout.Client;
  FListBox.Margins.Top:= SPACING;
  FListBox.OnChange:= ListBoxChange;
end;


{-------------------------------------------------------------------------------------------------------------
   RESIZE
-------------------------------------------------------------------------------------------------------------}

procedure TLightSearchListBox.Resize;
begin
  inherited;

  // Guard against calls during design-time registration when controls aren't created yet
  if FSearchEdit = NIL then EXIT;

  // Layout is handled by Align properties, but we ensure minimum heights
  if FSearchEdit.Height < SEARCH_EDIT_HEIGHT
  then FSearchEdit.Height:= SEARCH_EDIT_HEIGHT;
end;


{-------------------------------------------------------------------------------------------------------------
   POPULATION
-------------------------------------------------------------------------------------------------------------}

{ Populate the listbox with an array of strings.  Each item's Tag is set to its array index (useful for enum mapping). }
procedure TLightSearchListBox.SetItems(const Items: array of string);
VAR i: Integer;
begin
  //CreateChildControls;  // Ensure controls exist for runtime use

  FAllItems.Clear;
  for i:= 0 to High(Items) do
    FAllItems.AddObject(Items[i], TObject(i));  // Store index as Tag

  FSearchEdit.Text:= '';  // Clear search to show all items
  FilterItems;
end;


{ Add a single item with optional tag value }
procedure TLightSearchListBox.AddItem(const Text: string; Tag: Integer = 0);
begin
  //CreateChildControls;  // Ensure controls exist for runtime use

  FAllItems.AddObject(Text, TObject(Tag));
  FilterItems;
end;


procedure TLightSearchListBox.Clear;
begin
  //CreateChildControls;  // Ensure controls exist for runtime use

  FAllItems.Clear;
  FListBox.Clear;
  FSearchEdit.Text:= '';
end;


{-------------------------------------------------------------------------------------------------------------
   FILTERING
   Filters the listbox based on search text while preserving selection.
-------------------------------------------------------------------------------------------------------------}

procedure TLightSearchListBox.FilterItems;
VAR
  SearchText: string;
  SavedTag: Integer;
  HadSelection: Boolean;
  i: Integer;
  Item: TListBoxItem;
begin
  // Guard - controls might not exist yet during design-time
  if (FSearchEdit = NIL) OR (FListBox = NIL) then EXIT;

  SearchText:= LowerCase(FSearchEdit.Text);

  // Remember current selection by Tag value (not by index, which changes during filtering)
  HadSelection:= FListBox.ItemIndex >= 0;
  if HadSelection
  then SavedTag:= FListBox.ListItems[FListBox.ItemIndex].Tag
  else SavedTag:= -1;

  FListBox.BeginUpdate;
  TRY
    FListBox.Clear;

    // Add items that match the search text
    for i:= 0 to FAllItems.Count - 1 do
      if (SearchText = '') OR (Pos(SearchText, LowerCase(FAllItems[i])) > 0)
      then
        begin
          Item:= TListBoxItem.Create(FListBox);
          Item.Text:= FAllItems[i];
          Item.Tag:= Integer(FAllItems.Objects[i]);  // Restore Tag from stored object
          FListBox.AddObject(Item);
        end;

    // Handle selection after filtering
    if SearchText <> ''
    then
      begin
        // When searching: auto-select first match
        if FListBox.Count > 0
        then FListBox.ItemIndex:= 0;
      end
    else
      begin
        // When clearing search: restore previous selection by Tag
        if HadSelection
        then SelectByTag(SavedTag);
      end;
  FINALLY
    FListBox.EndUpdate;
  END;
end;


{-------------------------------------------------------------------------------------------------------------
   EVENT HANDLERS
-------------------------------------------------------------------------------------------------------------}

procedure TLightSearchListBox.SearchEditChangeTracking(Sender: TObject);
begin
  FilterItems;
end;


procedure TLightSearchListBox.ListBoxChange(Sender: TObject);
begin
  if Assigned(FOnSelectionChanged)
  then FOnSelectionChanged(Self);
end;


{-------------------------------------------------------------------------------------------------------------
   SELECTION
-------------------------------------------------------------------------------------------------------------}

{ Select an item by its Tag value. Returns TRUE if found. }
function TLightSearchListBox.SelectByTag(Tag: Integer): Boolean;
VAR i: Integer;
begin
  Result:= FALSE;
  //if FListBox = NIL then EXIT;

  for i:= 0 to FListBox.Count - 1 do
    if FListBox.ListItems[i].Tag = Tag
    then
      begin
        FListBox.ItemIndex:= i;
        EXIT(TRUE);
      end;
end;


{-------------------------------------------------------------------------------------------------------------
   GETTERS / SETTERS
-------------------------------------------------------------------------------------------------------------}

function TLightSearchListBox.GetSearchText: string;
begin
  if FSearchEdit = NIL
  then Result:= ''
  else Result:= FSearchEdit.Text;
end;


procedure TLightSearchListBox.SetSearchText(const Value: string);
begin
  CreateChildControls;
  FSearchEdit.Text:= Value;
  // OnChangeTracking will trigger FilterItems automatically
end;


function TLightSearchListBox.GetSelectedIndex: Integer;
begin
  if FListBox = NIL
  then Result:= -1
  else Result:= FListBox.ItemIndex;
end;


procedure TLightSearchListBox.SetSelectedIndex(const Value: Integer);
begin
  //CreateChildControls;
  FListBox.ItemIndex:= Value;
end;


function TLightSearchListBox.GetSelectedText: string;
begin
  if (FListBox = NIL) OR (FListBox.ItemIndex < 0)
  then Result:= ''
  else Result:= FListBox.ListItems[FListBox.ItemIndex].Text;
end;


function TLightSearchListBox.GetSelectedTag: Integer;
begin
  if (FListBox = NIL) OR (FListBox.ItemIndex < 0)
  then Result:= -1
  else Result:= FListBox.ListItems[FListBox.ItemIndex].Tag;
end;


function TLightSearchListBox.GetItemCount: Integer;
begin
  if FListBox = NIL
  then Result:= 0
  else Result:= FListBox.Count;
end;


{-------------------------------------------------------------------------------------------------------------
   REGISTRATION
-------------------------------------------------------------------------------------------------------------}

procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TLightSearchListBox]);
end;


end.
