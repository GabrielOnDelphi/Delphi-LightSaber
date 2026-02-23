{=============================================================================================================
  FMX STYLES - HOW IT WORKS
  =============================================================================================================

  There are two mechanisms for applying styles in FireMonkey:

  1. TStyleManager (from FMX.Styles) - Application-wide, global approach
     - TStyleManager.SetStyleFromFile loads a .style and applies it to ALL forms at once.
     - TStyleManager.SetStyle(nil) reverts to the default platform look.
     - No visual component needed on any form.
     - Best used ONCE in the .dpr, before Application.Initialize, to set a fixed app-wide style.
     - NOT recommended for runtime switching: some .style files (e.g. Air.style) have incomplete
       style definitions (missing lookups like "listviewstyle") which permanently corrupt the
       style engine until the app is restarted.
       See: https://en.delphipraxis.net/topic/14848-inconsistent-behavior-when-loading-styles/

  2. TStyleBook (visual component) - Per-form approach (RECOMMENDED for runtime switching)
     - Create a NEW TStyleBook each time you switch styles. Load via LoadFromFile.
     - Assign Form.StyleBook for ALL open forms (loop Screen.Forms).
       (UseStyleManager := True is supposed to propagate to all forms, but it does NOT
       work reliably for secondary forms — assign StyleBook explicitly to each form.)
     - Do NOT free old StyleBooks! When FMX detaches a StyleBook (Form.StyleBook := NewOne),
       it internally frees some of the old StyleBook's children (animations, etc.) without
       telling the StyleBook. Calling OldStyleBook.Free then crashes with "Invalid Pointer
       Operation" in TAnimation.Destroy (double-free).
       Instead, create StyleBooks with Create(Self) so the form owns them. They accumulate
       harmlessly and get freed when the form is destroyed.
     - Do NOT reuse a single StyleBook with Clear + LoadFromFile for the same reason:
       after detachment, the old StyleBook's internals may be partially destroyed.

  This demo uses approach #2 (new TStyleBook per style switch).

  Refs:
     https://docwiki.embarcadero.com/Libraries/Athens/en/FMX.Controls.TStyleBook.LoadFromFile
     https://blogs.embarcadero.com/changing-a-firemonkey-style-at-runtime/
     https://en.delphipraxis.net/topic/14848-inconsistent-behavior-when-loading-styles/
=============================================================================================================}

unit HeaderFooterTemplate;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.IOUtils,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ListBox, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView;

type
  THeaderFooterForm = class(TForm)
    Header: TToolBar;
    Footer: TToolBar;
    HeaderLabel: TLabel;
    cmbStyles: TComboBox;
    StyleBook1: TStyleBook;  { Design-time placeholder; not used at runtime }
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    ListView: TListView;
    Button1: TButton;
    GroupBox1: TGroupBox;
    procedure FormCreate(Sender: TObject);
    procedure cmbStylesChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    StylesPath: string;
  public
  end;

var
  HeaderFooterForm: THeaderFooterForm;

implementation

{$R *.fmx}

uses
  Form2;

procedure THeaderFooterForm.FormCreate(Sender: TObject);
var
  Files: TStringDynArray;
  FileName: string;
begin
  StylesPath := 'c:\Delphi\Styles & resources\FMX Styles\Win\';

  { Populate combo with available .style files }
  cmbStyles.Items.Add('(Default)');
  Files := TDirectory.GetFiles(StylesPath, '*.style');
  for FileName in Files
    do cmbStyles.Items.Add(TPath.GetFileNameWithoutExtension(FileName));
  cmbStyles.ItemIndex := 0;
end;


procedure THeaderFooterForm.cmbStylesChange(Sender: TObject);
var
  StyleFile: string;
  NewStyleBook: TStyleBook;
  i: Integer;
begin
  if cmbStyles.ItemIndex <= 0 then
  begin
    { Revert to default platform style by assigning nil to all forms.
      Don't free old StyleBooks - see documentation header. }
    for i := 0 to Screen.FormCount - 1 do
      Screen.Forms[i].StyleBook := nil;
    Exit;
  end;

  { Create a NEW StyleBook each time. Old ones are owned by Self and freed on form destroy.
    Do NOT reuse/clear old StyleBooks - FMX partially destroys their internals on detach. }
  NewStyleBook := TStyleBook.Create(Self);
  try
    StyleFile := StylesPath + cmbStyles.Selected.Text + '.style';
    NewStyleBook.LoadFromFile(StyleFile);
  except
    on E: Exception do
    begin
      FreeAndNil(NewStyleBook);
      Caption := 'Error loading style: ' + E.Message;
      Exit;
    end;
  end;

  { Apply to ALL open forms }
  for i := 0 to Screen.FormCount - 1 do
    Screen.Forms[i].StyleBook := NewStyleBook;
end;


procedure THeaderFooterForm.Button1Click(Sender: TObject);
begin
  frm2.Show;
end;

end.
