UNIT FormStylesMain;

{=============================================================================================================
   2026.02
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Demo/test program for the FMX style loading bug reported to Embarcadero.
   Lists all style files delivered with Delphi and applies them on click.

   BUG:
     TStyleManager.SetStyleFromFile is inconsistent.
     After loading certain styles (e.g. Air.style), the ListView selection highlight
     is permanently lost. Switching to any other style won't restore it.
     The highlight only comes back after restarting the application.
     Not all styles trigger this bug - only specific ones like Air.style.

     Forum thread: https://en.delphipraxis.net/topic/14848-inconsistent-behavior-when-loading-styles/

     Workaround (default)
       Use TStyleBook instead of TStyleManager.SetStyleFromFile.
       LoadViaStyleBook avoids the bug by managing styles through TStyleBook.
       Check the "Use buggy SetStyleFromFile" checkbox to switch to the buggy method.

   TIPS:
     1. TControl.IsLightStyleColor detects light/dark styles (VCL only, not available in FMX)
     2. If you call TStyleManager.SetStyle before Application.Initialize, the style is applied to all forms.
     3. To load the correct style file for each platform:
         case TOSVersion.Platform of
           pfWindows: TStyleManager.SetStyleFromFile('styles/Windows/Jet.style');
           pfAndroid: TStyleManager.SetStyleFromFile('styles/Android/Jet.style');
           pfiOS:     TStyleManager.SetStyleFromFile('styles/iOS/Jet.style');
           pfMacOS:   TStyleManager.SetStyleFromFile('styles/MacOS/Jet.style');
         end;
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.Classes, System.SysUtils, System.Types, System.UITypes, System.IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListView.Types, FMX.ListView.Appearances, FMX.Styles, FMX.ListView.Adapters.Base, FMX.ListView, FMX.Dialogs, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Edit,
  LightFmx.Common.AppData.Form;

TYPE
  TfrmSimpleDemo = class(TLightForm)
    btnRefresh   : TButton;
    btnNext: TButton;
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
    procedure btnNextClick(Sender: TObject);
    procedure ListViewUpdateObjects(const Sender: TObject; const AItem: TListViewItem);
    procedure btnRefreshClick(Sender: TObject);
    procedure edtPathClick(Sender: TObject);
  private
    LastStyle: string;
    CurItem: Integer;
    procedure LoadViaStyleManager(const AItem: TListViewItem);
    procedure LoadViaStyleBook(const AItem: TListViewItem);
  public
    procedure AfterConstruction; override;
  end;


VAR frmSimpleDemo: TfrmSimpleDemo;

IMPLEMENTATION
{$R *.fmx}

USES
   LightFmx.Common.AppData, LightCore, LightCore.IO, LightFmx.Common.Styles;



procedure TfrmSimpleDemo.AfterConstruction;
begin
  inherited;
  // On first run, set default path. On subsequent runs, edtPath.Text is restored by TLightForm state saving.
  if AppData.RunningFirstTime then
    if AppData.BetaTesterMode
    then edtPath.Text:= 'c:\Delphi\Styles & resources\FMX Styles\Win\'
    else
      begin
        edtPath.Text:= TPath.Combine(GetEnvironmentVariable('ProgramFiles(x86)'), 'Embarcadero\Studio\37.0\Redist\styles\Fmx');
        if NOT TDirectory.Exists(edtPath.Text)
        then edtPath.Text:= TPath.Combine(TPath.GetPublicPath, 'Documents\Embarcadero\Studio\37.0\Styles');
      end;

  btnRefreshClick(Self);
end;


{ List all .style and .fsf files from the specified folder into the ListView }
procedure TfrmSimpleDemo.btnRefreshClick(Sender: TObject);
var
  Files: TStringDynArray;
  FileName: string;
  StylesFolder: string;
  Item: TListViewItem;
begin
  StylesFolder:= edtPath.Text;
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
      if PosInsensitive('metropolis', FileName) > 0 then Continue;   // Skip Metropolis styles
      Item:= ListView.Items.Add;
      Item.Text     := ExtractFileName(FileName);
      Item.Detail   := 'Text format';
      Item.TagString:= FileName;
    end;

    Files := TDirectory.GetFiles(StylesFolder, '*.fsf');
    for FileName in Files do
    begin
      if PosInsensitive('metropolis', FileName) > 0 then Continue;
      Item:= ListView.Items.Add;
      Item.Text     := ExtractFileName(FileName);
      Item.Detail   := 'Binary format';
      Item.TagString:= FileName;
    end;
  finally
    ListView.EndUpdate;
  end;
end;


procedure TfrmSimpleDemo.ListViewItemClick(const Sender: TObject; const AItem: TListViewItem);
begin
  if (AItem = nil) or (AItem.TagString = '') then Exit;
  CurItem:= AItem.Index;

  if chkSome.IsChecked
  then LoadViaStyleManager(AItem)   // Buggy: demonstrates the SetStyleFromFile highlight bug
  else LoadViaStyleBook(AItem);     // Workaround: avoids the bug via TStyleBook
end;


{ LoadStyle
  HAS PROBLEMS IF WE TRY TO LOAD AN INVALID STYLE FILE!
  Demonstrates the TStyleManager.SetStyleFromFile bug.
  After loading in invalid styles (e.g. Air.style), the ListView selection highlight is permanently lost and won't come back until the application is restarted.

  This is still the recommended method IF you don't have any invalid Style files. }
procedure TfrmSimpleDemo.LoadViaStyleManager(const AItem: TListViewItem);
var
  StylePath: string;
begin
  if (AItem = nil) or (AItem.TagString = '') then EXIT;
  StylePath := AItem.TagString;
  if SameText(StylePath, LastStyle) then EXIT;
  LastStyle:= StylePath;

  if NOT IsStyleCompatible(StylePath) then
  begin
    ShowMessage('Style "' + AItem.Text + '" is not compatible with this platform.');
    EXIT;
  end;

  try
    if TStyleManager.SetStyleFromFile(StylePath)
    then Caption := 'Applied (StyleManager): ' + AItem.Text
    else ShowMessage('Failed to apply style.');
  except
    on E: Exception do ShowMessage('Error: ' + E.Message);
  end;
end;


{ WORKS
  Uses TStyleBook to avoid the SetStyleFromFile highlight bug.
  Unfortunately we need to manually skin existing forms and all new (dynamically created forms).

  Leak: Old StyleBooks are NOT freed manually. Setting Self.StyleBook := nil causes FMX to internally release/free
  some of the StyleBook's child objects (animations, etc.), but the StyleBook doesn't know they're gone.
  Calling OldStyleBook.Free afterwards causes "Invalid Pointer Operation" in TAnimation.Destroy because it tries to free already-freed children.
  Instead, we let the form own all StyleBooks (Create(Self))and they get cleaned up when the form is destroyed. }
procedure TfrmSimpleDemo.LoadViaStyleBook(const AItem: TListViewItem);
var
  StylePath: string;
begin
  if (AItem = nil) or (AItem.TagString = '') then Exit;
  StylePath := AItem.TagString;
  if not TFile.Exists(StylePath) then Exit;

  if NOT IsStyleCompatible(StylePath) then
  begin
    ShowMessage('Style "' + AItem.Text + '" is not compatible with this platform.');
    EXIT;
  end;

  // Don't free it. See comment above. Each style switch leaks one TStyleBook (minus its freed children).
  var NewStyleBook:= TStyleBook.Create(Self);
  try
    NewStyleBook.LoadFromFile(StylePath);
  except
    on E: Exception do
    begin
      FreeAndNil(NewStyleBook);
      ShowMessage('Error loading style: ' + E.Message);
      Exit;
    end;
  end;

  // Apply to ALL open forms. Assign directly - don't call TStyleManager.SetStyle(nil)
  // because destroying the old style triggers TMainMenu.RecreateOSMenu on secondary forms,
  // which tries to look up styles from the half-destroyed StyleBook → AV crash.
  for var i := 0 to Screen.FormCount - 1 do
    Screen.Forms[i].StyleBook := NewStyleBook;

  Caption:= 'Applied (StyleBook): ' + AItem.Text;

  // Re-apply ListView selection after style change recreates internal presentation
  var SavedIndex := AItem.Index;
  TThread.ForceQueue(nil,
    procedure
    begin
      ListView.ItemIndex := -1;
      ListView.ItemIndex := SavedIndex;
    end);
end;


{ Cycle to next style in the list }
procedure TfrmSimpleDemo.btnNextClick(Sender: TObject);
begin
  if CurItem >= ListView.Items.Count
  then CurItem:= 0;

  ListViewItemClick(Sender, ListView.Items[CurItem]);
  ListView.ItemIndex:= CurItem;

  Inc(CurItem);
end;


{ Note: FMX styles already set proper text colors for ListView items.
  Unlike VCL (which has TControl.IsLightStyleColor), FMX has no built-in
  dark style detection. Don't override text colors here - let the style handle it. }
procedure TfrmSimpleDemo.ListViewUpdateObjects(const Sender: TObject; const AItem: TListViewItem);
var
  TextObj: TListItemText;
  DetailObj: TListItemText;
  IsDark: Boolean;
begin
  exit;
  ///IsDark:= TStyleManager.IsCurrentStyleDark;
  IsDark:= false;

  TextObj := AItem.Objects.FindObjectT<TListItemText>('text');
  if Assigned(TextObj) then
  begin
    if IsDark
    then TextObj.TextColor := TAlphaColorRec.WhiteSmoke
    else TextObj.TextColor := TAlphaColorRec.Black;
  end;

  DetailObj := AItem.Objects.FindObjectT<TListItemText>('detail');
  if Assigned(DetailObj) then
  begin
    if IsDark
    then DetailObj.TextColor := TAlphaColorRec.WhiteSmoke
    else DetailObj.TextColor := TAlphaColorRec.Black;
  end;
end;


procedure TfrmSimpleDemo.TimerTimer(Sender: TObject);
begin
  ProgressBar.Value:= ProgressBar.Value+ 1;
  if ProgressBar.Value > 99
  then ProgressBar.Value:= 0;
end;


procedure TfrmSimpleDemo.edtPathClick(Sender: TObject);
begin
  btnRefreshClick(Sender);
end;


end.
