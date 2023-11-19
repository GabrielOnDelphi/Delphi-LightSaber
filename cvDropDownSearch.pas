UNIT cvDropDownSearch;

{-------------------------------------------------------------------------------------------------------------
  GabrielM
  2023.10.31

  Searchbox with auto-suggest
  A dropdown box similar to the Help Insight in Delphi IDE.
  Displays a list of items. The list is filtered (gets smaller) as the user types in more characters into the searchbox.

  Also see:
    https://blog.dummzeuch.de/2019/04/28/autocompletion-for-tedits-revisited/
    https://stackoverflow.com/questions/2012208/google-like-edit-combo-control-for-delphi

-------------------------------------------------------------------------------------------------------------}

//ToDo: Issue: the drop down does not respond to scroll
//ToDo: Issue: the drop down needs two clicks instead of one in order to be closed
//ToDo: When the drop down is focused, let user close the drop down with Enter

INTERFACE
USES
   System.Classes, Vcl.Controls, System.Types, Vcl.WinXCtrls,
   cvListBox;

TYPE
  TSelectNotifyEvent = procedure (Sender: TObject; SelectedItem: TObject) of object;

  TDropDownSearchBox= class(TSearchBox)
   private
    FOnEndSearch: TSelectNotifyEvent;
    procedure showDropDown;
    procedure endSearch(Sender: TObject);
   protected
    procedure KeyPress(var Key: Char); override;
    procedure Click; override;
    procedure Change; override;
   protected
     lbxSearch: TCubicListBox;
   public
     constructor Create(AOwner: TComponent); override;
     procedure  CreateWindowHandle(const Params: TCreateParams); override;
     procedure  SetParent(AParent: TWinControl); override;                               { SetParent is called during construction AND also during deconstruction with aParent=nil }
     destructor Destroy; override;
     procedure  Populate(Objects: TStringList);
     procedure  SetHostParent(aParent: TWinControl);
     function   SelectedObject: TObject;
   published
     property   OnEndSearch: TSelectNotifyEvent read FOnEndSearch write FOnEndSearch;    { Triggered when the user selected an item from the list }
  end;

procedure Register;



IMPLEMENTATION
USES
   ccCore;





constructor TDropDownSearchBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AlignWithMargins := True;
  TextHint         := 'Search...';
  OnInvokeSearch   := EndSearch;
  Text             := '';

  lbxSearch := TCubicListBox.Create(Self);
  lbxSearch.Visible := FALSE;
  lbxSearch.OnClick := EndSearch;
  lbxSearch.Parent  := Self;
end;


procedure TDropDownSearchBox.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited;
  lbxSearch.Name      := 'lbxSearch';
  lbxSearch.Width       := Width;
  lbxSearch.Height      := 81;
  lbxSearch.MultiSelect := False;
  lbxSearch.ItemHeight  := 13;
  lbxSearch.Sorted      := TRUE;
  lbxSearch.Visible     := FALSE;

  Text:= '';
end;


procedure TDropDownSearchBox.SetParent(AParent: TWinControl);
begin
  inherited;
end;


destructor TDropDownSearchBox.Destroy;
begin
  ///FreeAndNil(lbxSearch);  Destroyed by the Parent
  inherited;
end;






procedure TDropDownSearchBox.Change;
begin
  inherited;
  showDropDown;
end;


procedure TDropDownSearchBox.Click;
begin
  inherited;
  showDropDown;
end;


procedure TDropDownSearchBox.showDropDown;
begin
  lbxSearch.Visible:= TRUE;
end;


procedure TDropDownSearchBox.SetHostParent(aParent: TWinControl);
begin
  VAR Pos:= ClientToParent(Point(0, 0), aParent);

  lbxSearch.Parent := aParent;
  lbxSearch.Top    := Pos.Y+ Self.Height+ 2;
  lbxSearch.Left   := Pos.X;
  lbxSearch.Width  := Width;
end;


// Test what happens if the text is not found so the user selects nothing
procedure TDropDownSearchBox.EndSearch(Sender: TObject);
begin
  lbxSearch.Visible:= FALSE; // This must be at the end because of edtSearchReagent.Text:= ActionBl.Reagent.AsText

  if lbxSearch.SelectedObject = NIL then EXIT; // When it happens?

  Self.Text:= lbxSearch.SelectedItem;

  if Assigned(FOnEndSearch) then FOnEndSearch(Self, lbxSearch.SelectedObject);
end;


// Cancel search on Escape
procedure TDropDownSearchBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if key = ESC
  then lbxSearch.Visible:= FALSE;;    //ToDo: Allow the user move the cursor down to select from the list.
end;



procedure TDropDownSearchBox.Populate(Objects: TStringList);
begin
  lbxSearch.Clear;
  for VAR i:= 0 to Objects.Count-1 DO
    if (Text = '') OR (PosInsensitive(Text, Objects[i]) > 0)
    then lbxSearch.Items.AddObject(Objects[i], Objects.Objects[i]);
  lbxSearch.SelectFirstItem;

  lbxSearch.SetHeightAuto(300, Parent); //Resize it based on the number of rows in it, but never make it bigger than the 1/2 form
end;


function TDropDownSearchBox.SelectedObject: TObject;
begin
  Result:= lbxSearch.SelectedObject;
end;



procedure Register;
begin
  RegisterComponents('LightSaber', [TDropDownSearchBox]);
end;


end.


