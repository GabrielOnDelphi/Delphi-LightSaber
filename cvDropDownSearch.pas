UNIT cvDropDownSearch;

{-------------------------------------------------------------------------------------------------------------
  CubicDesign
  2020

  A dropdown box in which displayes a list of items that is filtered as the user type in more characters.
  Similar to the Help Insight in Delphi IDE.

-------------------------------------------------------------------------------------------------------------}

INTERFACE
USES
   Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, System.Types, Vcl.Forms, Vcl.FileCtrl, Vcl.WinXCtrls,
   cvListBox;

TYPE
  TSelectNotifyEvent = procedure (Sender: TObject; SelectedItem: TObject) of object;

  TDropDownSearchBox= class(TSearchBox)
   private
    FOnEndSearch: TSelectNotifyEvent;
    procedure ShowDrowDown(Sender: TObject);
    procedure EndSearch(Sender: TObject);
   protected
    procedure KeyPress(var Key: Char); override;
    procedure Click; override;
    procedure Change; override;
   protected
   public
     lbxSearch: TCubicListBox;
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     property OnEndSearch: TSelectNotifyEvent read FOnEndSearch write FOnEndSearch;    { Triggered when the user selected an item from the list }
   published
  end;

procedure Register;



IMPLEMENTATION
USES
   ccCore, ccIO;



constructor TDropDownSearchBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AlignWithMargins := True;
  TextHint         := 'Search...';
  OnInvokeSearch   := EndSearch;

  lbxSearch := TCubicListBox.Create(Self);
  //lbxSearch.Name := 'lbxSearch';
  lbxSearch.Parent := Self;
  lbxSearch.Width  := Width;
  lbxSearch.Height := 81;
  lbxSearch.MultiSelect := False;
  lbxSearch.ItemHeight  := 13;
  lbxSearch.ScrollWidth := 6;
  lbxSearch.Sorted  := True;
  lbxSearch.Visible := False;
  lbxSearch.OnClick := EndSearch;
end;


destructor TDropDownSearchBox.Destroy;
begin
  FreeAndNil(lbxSearch);
  inherited;
end;






procedure TDropDownSearchBox.Change;
begin
  inherited;
  ShowDrowDown(Self);
end;


procedure TDropDownSearchBox.Click;
begin
  inherited;
  ShowDrowDown(Self);
end;


procedure TDropDownSearchBox.ShowDrowDown(Sender: TObject);
begin
  //VAR X:= edtSearchReagent.ClientToParent(Point(edtSearchReagent.Left, edtSearchReagent.Top), Parent.Parent).X;

  lbxSearch.Parent := Self.Parent;
  lbxSearch.Top    := Self.Top + Self.Height;
  lbxSearch.Left   := Left + Self.Left;
  lbxSearch.Width  := Width;
  lbxSearch.Visible:= TRUE;

  //Populate(Self.Text);
end;


// Test what happens if the text is not found so the user selects nothing
procedure TDropDownSearchBox.EndSearch(Sender: TObject);
begin
  lbxSearch.Visible:= FALSE; // This must be at the end because of edtSearchReagent.Text:= ActionBl.Reagent.AsText

  if lbxSearch.SelectedObject = NIL then EXIT; // When it happens?

  Self.Text:= lbxSearch.SelectedItem;

  if Assigned(FOnEndSearch) then FOnEndSearch(Self, lbxSearch.SelectedObject);
end;

{
procedure TDropDownSearchBox.FillItems;
begin

end;    }

// Cancel search on Escape
procedure TDropDownSearchBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if key = ESC
  then lbxSearch.Visible:= FALSE;;    //ToDo: Allow the user move the cursor down to select from the list.
end;






procedure Register;
begin
  RegisterComponents('LightSaber', [TDropDownSearchBox]);
end;




end.
