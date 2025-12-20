UNIT MainForm;

{=============================================================================================================
   2025.12
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   This demo program lists all styles file delivered with Delphi and applies them (on click).

   To load the coreect style file for each platform do this:
     case TOSVersion.Platform of
       pfWindows: TStyleManager.SetStyleFromFile('styles/Windows/Jet.style');
       pfAndroid: TStyleManager.SetStyleFromFile('styles/Android/Jet.style');
       pfiOS:     TStyleManager.SetStyleFromFile('styles/iOS/Jet.style');
       pfMacOS:   TStyleManager.SetStyleFromFile('styles/MacOS/Jet.style');
     end;

    Note: If you call the TStyleManager.SetStyle function in the initialization section of a unit on the main project file, before Application.Initialize, then the style is applied to all forms.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.Classes, System.SysUtils, System.Types, System.UITypes, System.IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListView.Types, FMX.ListView.Appearances, FMX.Styles, FMX.ListView.Adapters.Base, FMX.ListView, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  LightFmx.Common.AppData.Form, FMX.Edit;

TYPE
  TfrmSimpleDemo = class(TLightForm)  // This form derives from TLightForm (which does all the magic)
    btnRefresh   : TButton;
    Button1      : TButton;
    chkSome      : TCheckBox;
    edtPath      : TEdit;
    GroupBox1    : TGroupBox;
    Layout1      : TLayout;
    Layout2      : TLayout;
    lblFolder    : TLabel;
    ListView     : TListView;
    Memo1        : TMemo;
    ProgressBar  : TProgressBar;
    RadioButton1 : TRadioButton;
    RadioButton2 : TRadioButton;
    StatusBar1   : TStatusBar;
    Timer        : TTimer;
    TrackBar1    : TTrackBar;
    procedure FormCreate  (Sender: TObject);
    procedure ListViewItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure TimerTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListViewUpdateObjects(const Sender: TObject; const AItem: TListViewItem);
    procedure btnRefreshClick(Sender: TObject);
  private
    LastStyle: string;
    CurItem: Integer;
    procedure LoadViaStyleManager(const AItem: TListViewItem);
    procedure LoadViaStyleBook(const AItem: TListViewItem);
    procedure InvalidateStyleResources(AControl: TFmxObject);
  public
  end;

VAR frmSimpleDemo: TfrmSimpleDemo;

IMPLEMENTATION
{$R *.fmx}

USES
   LightFmx.Common.AppData, LightCore, LightCore.IO, DataModule;


procedure TfrmSimpleDemo.FormCreate(Sender: TObject);
VAR StylesFolder: string;
begin
  // Set this in code so you don't have to rely on the IDE and to ensure it's always linked to the central source.
  //Self.StyleBook:= datMod.StyleBook;

  // Locate the default Styles folder
  if AppData.RunningFirstTime then
    if AppData.RunningHome
    then StylesFolder:= 'c:\Delphi\Styles & resources\FMX Styles\Win\'
    else
      begin
        StylesFolder:= TPath.Combine( GetEnvironmentVariable('ProgramFiles(x86)'), 'Embarcadero\Studio\37.0\Redist\styles\Fmx');
        if NOT TDirectory.Exists(StylesFolder)
        then StylesFolder := TPath.Combine(TPath.GetPublicPath, 'Documents\Embarcadero\Studio\37.0\Styles');    // If the folder doesn't exist (e.g. different Delphi version), fall back to user styles
      end;

  btnRefreshClick(Sender);
end;


// List all Style files
procedure TfrmSimpleDemo.btnRefreshClick(Sender: TObject);
var
  Files: TStringDynArray;
  FileName: string;
  StylesFolder: string;
  Item: TListViewItem;
begin
  StylesFolder:= edtPath.Text;
  edtPath.Text:= StylesFolder;
  lblFolder.Hint:= StylesFolder;

  if NOT DirectoryExists(StylesFolder) then
  begin
    Caption:= 'Folder not found! '+ StylesFolder;
    EXIT;
  end;

  ListView.BeginUpdate;
  try
    ListView.Items.Clear;
    Files:= TDirectory.GetFiles(StylesFolder, '*.style');
    for FileName in Files do
    begin
      if PosInsensitive('metropolis', FileName) > 0 then Continue;   // We skip all Metropolis styles because they are fugly.
      Item:= ListView.Items.Add;
      Item.Text     := ExtractFileName(FileName);
      Item.Detail   := 'Text format';
      Item.TagString:= FileName;
    end;

    Files := TDirectory.GetFiles(StylesFolder, '*.fsf');
    for FileName in Files do
    begin
      if PosInsensitive('metropolis', FileName) > 0 then Continue;  // We skip all Metropolis styles because they are fugly.
      Item:= ListView.Items.Add;
      Item.Text := ExtractFileName(FileName);
      Item.Detail := 'Binary format';
      Item.TagString := FileName;
    end;
  finally
    ListView.EndUpdate;
  end;
end;


procedure TfrmSimpleDemo.ListViewItemClick(const Sender: TObject; const AItem: TListViewItem);
begin
  if (AItem = nil) or (AItem.TagString = '') then Exit;
  CurItem:= AItem.Index;

  //LoadViaStyleBook(AItem);
  LoadViaStyleManager(AItem);

  {
  // Fix for the ListView Focus issue.
  //  After applying a style the selection in ListView is lost because that control is fully reconstrcuted. So, we cannot rely on ListView.Selected.
  //  Use TThread.ForceQueue to allow the UI to finish the style change before re-focusing
  // But the problem does not seems to be from here. Looks like it is related to "TStyleManager.SetStyleFromFile is inconsistent"
  TThread.ForceQueue(nil, procedure
    begin
      ListView.ItemIndex := AItem.Index;
    end); }
end;


(*
// this works!
procedure TfrmSimpleDemo.LoadViaStyleBook(const AItem: TListViewItem);
begin
  var NewStyle: TStyleBook;
  NewStyle := TStyleBook.Create(Self);
  NewStyle.LoadFromFile(AItem.TagString);

  TStyleManager.SetStyle(NIL);    // alten globalen Style sauber deaktivieren
  StyleBook := NIL;    // Formular vom alten Style trennen

  // neuen Style setzen wenn nicht "NONE"
  StyleBook := NewStyle;
  TStyleManager.SetStyle(NewStyle);
end;  *)


procedure TfrmSimpleDemo.LoadViaStyleBook(const AItem: TListViewItem);
var
  StylePath: string;
begin
  if (AItem = nil) or (AItem.TagString = '') then EXIT;
  StylePath := AItem.TagString;

  // Create and load new style
  var NewStyle := TStyleBook.Create(Self);
  try
    NewStyle.LoadFromFile(StylePath);
    // Force complete refresh of all controls
    InvalidateStyleResources(Self);
  except
    NewStyle.Free;
    raise;
  end;

  // CRITICAL: Clear the style manager first
  //doc: To set the active style to the native system style, for example Windows theme, pass nil as the Style parameter.
  TStyleManager.SetStyle(nil);
  Self.StyleBook := nil;  // Disconnect form from old style

  // Set new style
  Self.StyleBook := NewStyle;
  TStyleManager.SetStyle(NewStyle); //doc: Sets the active style to the instance specified by the Style parameter.

  // Force all controls to release style resources
  InvalidateStyleResources(Self);
end;


// Helper procedure to recursively invalidate style resources
procedure TfrmSimpleDemo.InvalidateStyleResources(AControl: TFmxObject);
var
  I: Integer;
  Child: TFmxObject;
begin
  if AControl is TStyledControl then
    TStyledControl(AControl).ApplyStyleLookup;

  for I := 0 to AControl.ChildrenCount - 1 do
  begin
    Child := AControl.Children[I];
    InvalidateStyleResources(Child);
  end;
end;

(*
//todo: this does not work, but the code above (in comments works)
procedure TfrmSimpleDemo.LoadViaStyleBook(const AItem: TListViewItem);
var
  NewStyleBook: TStyleBook;
  StylePath: string;
begin
  if (AItem = nil) or (AItem.TagString = '') then Exit;
  StylePath := AItem.TagString;


   StyleBook1.FileName := StylePath;

    // 2. Force the Form to use this StyleBook
    Self.StyleBook := StyleBook1;


  {
  var NewStyle: TStyleBook;
  NewStyle := TStyleBook.Create(Self);
  NewStyle.LoadFromFile(StylePath);

  // alten globalen Style sauber deaktivieren
  TStyleManager.SetStyle(nil);

  // Formular vom alten Style trennen
  StyleBook := NIL;

  // neuen Style setzen wenn nicht "NONE"
  StyleBook := NewStyle;
  TStyleManager.SetStyle(NewStyle);
        }

   EXIT;



  // Create the StyleBook in memory (don't assign to Form yet)
  NewStyleBook:= TStyleBook.Create(Self);

  // Try to load the file
  // Note: TStyleBook.LoadFromFile still uses the same engine, so we still wrap it to catch platform incompatibility exceptions.
  try
    NewStyleBook.LoadFromFile(StylePath);
    FreeAndNil(Self.StyleBook);           // SUCCESS: Swap the old StyleBook for the new one Free the old one if it exists to save memory
    Self.StyleBook := NewStyleBook;     // Assign the new style to this form

    Caption := 'Applied via TStyleBook: ' + AItem.Text;
  except
    on E: Exception do
    begin
      NewStyleBook.Free; // Clean up the failed object
      ShowMessage('Incompatible Style: ' + AItem.Text + sLineBreak + E.Message);
    end;
  end;
end;     *)


{ Bug?
  Behavior of TStyleManager.SetStyleFromFile is inconsistent.
  I have all style files in a folder listed in a ListView.
  When I click the PuertoRico.style, the current clicked item is highlighted in the ListView.
  Then if I click the Air.style and then go back to PuertoRico or any other style, the highlight never appears back.
  The highlight is lost until application restart.

  So, it looks like we have to use LoadViaStyleBook. }
procedure TfrmSimpleDemo.LoadViaStyleManager(const AItem: TListViewItem);
var
  StylePath: string;
  StyleObj: TFmxObject;
  Description: TStyleDescription;
  IsCompatible: Boolean;
  PlatformIs: string;
begin
  if (AItem = nil) or (AItem.TagString = '') then EXIT;

  StylePath  := AItem.TagString;
  IsCompatible:= False;

  if SameText(StylePath, LastStyle) then EXIT;
  PlatformIs := 'unknown';

  // Probe the style file without applying it globally yet
  StyleObj := TStyleStreaming.LoadFromFile(StylePath);
  try
    if StyleObj <> nil then
    begin
      Description := TStyleManager.FindStyleDescriptor(StyleObj);
      LastStyle:= StylePath;

      if Description <> nil then
      begin
        // Check if current platform is supported in the PlatformTarget string
        {$IFDEF MSWINDOWS}
        IsCompatible:= Description.PlatformTarget.Contains('[MSWINDOWS]')   //example: [MSWINDOWS][MODERN][DEFINEFONTSTYLES]
           OR Description.PlatformTarget.Contains('[ANY]')
           OR (Description.PlatformTarget = '');
         if IsCompatible then PlatformIs := 'win';
        {$ENDIF}

        {$IFDEF MACOS}
        IsCompatible:= Description.PlatformTarget.Contains('[MACOS]');
        if IsCompatible then PlatformIs := 'mac';
        {$ENDIF}
      end
      else
        IsCompatible:= True;   // If no description exists, it's usually an older "universal" style
    end;
  finally
    StyleObj.Free;
  end;

  // 2. Apply the style only if compatible
  if IsCompatible then
    begin
      try
        if TStyleManager.SetStyleFromFile(StylePath)
        then Caption := 'Applied: ' + AItem.Text
        else ShowMessage('Failed to apply style.');
      except
        on E: Exception do ShowMessage('Error: ' + E.Message);
      end;
    end
  else
    ShowMessage('Style "' + AItem.Text + '" is not designed for this Platform. '+ PlatformIs);
end;


procedure TfrmSimpleDemo.Button1Click(Sender: TObject);
begin
  if CurItem = ListView.Items.Count
  then CurItem:= 0;

  ListViewItemClick(Sender, ListView.Items[CurItem]);
  ListView.ItemIndex:= CurItem;

  Inc(CurItem);
end;


// tries to set the font to white for dark themes.
procedure TfrmSimpleDemo.ListViewUpdateObjects(const Sender: TObject; const AItem: TListViewItem);
var
  TextObj: TListItemText;
  DetailObj: TListItemText;
  BackgroundObj: TListItemDrawable;
  BgColor: TAlphaColor;
  Brightness: Byte;
begin
  EXIT;

  // Get the background drawable of the item (usually 'background' or 'itembackground')
  BackgroundObj := AItem.Objects.FindDrawable('background');
  if not Assigned(BackgroundObj)
  then BackgroundObj := AItem.Objects.FindDrawable('itembackground');  // fallback name in some styles
  {
  if TStyleManager.IsCurrentStyleDark then
    TextObj.TextColor := TAlphaColorRec.WhiteSmoke  // softer white
  else
    TextObj.TextColor := TAlphaColorRec.DarkSlateGray;  // darker than black
      }
  // Calculate brightness (luminance) of the background
  Brightness := round(
                TAlphaColorRec(BgColor).R * 0.299 +
                TAlphaColorRec(BgColor).G * 0.587 +
                TAlphaColorRec(BgColor).B * 0.114);

  // Main text
  TextObj := AItem.Objects.FindObjectT<TListItemText>('text');
  if Assigned(TextObj) then
  begin
    if Brightness < 128 then  // Dark background
      TextObj.TextColor := TAlphaColorRec.White
    else
      TextObj.TextColor := TAlphaColorRec.Black;  // or leave as is: TextObj.RestoreDefaults;
  end;

  // Detail text (if used)
  DetailObj := AItem.Objects.FindObjectT<TListItemText>('detail');
  if Assigned(DetailObj) then
  begin
    if Brightness < 128 then
      DetailObj.TextColor := TAlphaColorRec.White
    else
      DetailObj.TextColor := TAlphaColorRec.Black;
  end;
end;




procedure TfrmSimpleDemo.TimerTimer(Sender: TObject);
begin
  ProgressBar.Value:= ProgressBar.Value+ 1;
  if ProgressBar.Value > 99
  then ProgressBar.Value:= 0;
end;


end.











(* this works!
procedure TfrmSimpleDemo.LoadViaStyleBook(const AItem: TListViewItem);
begin
  var NewStyle: TStyleBook;
  NewStyle := TStyleBook.Create(nil);
  NewStyle.LoadFromFile(AItem.TagString);

  // alten globalen Style sauber deaktivieren
  TStyleManager.SetStyle(nil);

  // Formular vom alten Style trennen
  StyleBook := NIL;

  // neuen Style setzen wenn nicht "NONE"
  StyleBook := NewStyle;
  TStyleManager.SetStyle(NewStyle); }
end; *)

//I have a TStyleBook on the main form. The main form and the secondary form link to this stylebook (in Object Inspector)
procedure TfrmSimpleDemo.LoadViaStyleBook(const AItem: TListViewItem);
var
  NewStyleBook: TStyleBook;
  OldStyleBook: TStyleBook;
  StylePath: string;
begin
  if (AItem = nil) or (AItem.TagString = '') then Exit;
  StylePath := AItem.TagString;

  if not TFile.Exists(StylePath) then Exit;

  // 1. Create the new style object
  NewStyleBook := TStyleBook.Create(Self);
  try
    NewStyleBook.LoadFromFile(StylePath);
  except
    on E: Exception do
    begin
      NewStyleBook.Free;
      ShowMessage('Error loading style: ' + E.Message);
      Exit;
    end;
  end;

  // 2. Capture the old StyleBook reference
  OldStyleBook := Self.StyleBook;

  // 3. SWAP (The order here is critical for FMX)
  TStyleManager.SetStyle(nil);
  Self.StyleBook := nil;

  Self.StyleBook := NewStyleBook;
  TStyleManager.SetStyle(NewStyleBook);

  // 4. CRASH FIX: Defer destruction
  // Do NOT Free the old style book immediately.
  // ForceQueue ensures it is freed only after the current Windows/Event messages finish.
  if Assigned(OldStyleBook) and (OldStyleBook <> NewStyleBook) then
  begin
    TThread.ForceQueue(nil,
      procedure
      begin
        OldStyleBook.Free;
      end);
  end;

  // 5. LISTVIEW HIGHLIGHT FIX
  // Since the style change recreates the ListView's internal presentation,
  // we must re-apply the index for it to glow/highlight correctly.
  var SavedIndex := AItem.Index;
  TThread.ForceQueue(nil,
    procedure
    begin
      ListView.ItemIndex := -1;
      ListView.ItemIndex := SavedIndex;
    end);
end;
