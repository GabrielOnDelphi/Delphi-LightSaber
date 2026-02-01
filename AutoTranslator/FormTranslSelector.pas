UNIT FormTranslSelector;

{=============================================================================================================
   2026.01.29
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   LANGUAGE SELECTOR FORM

   Allows users to choose a language/translation file from available options.
   Works with LightVcl.Common.Translate.pas translation engine.

   USAGE:
     Call TfrmTranslSelector.ShowSelector to display the language selection dialog.
     The TTranslator instance must be created before calling this.

   FEATURES:
     - Lists all .ini translation files from the Lang folder
     - Remembers and pre-selects the last used language
     - Shows translator credits after selection
     - Links to TfrmTranslEditor for creating new translations

   DEFAULT LANGUAGE:
     The English.ini file can be empty! In this case the program uses design-time strings.
     Note: A program restart may be necessary when switching to default English.

   NOTES:
     Don't add dependencies to CubicVisualControls here!

   RELATED:
     - LightVcl.Common.Translate.pas - Translation engine
     - FormTranslEditor.pas - Translation editor form
=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON}

USES
  System.SysUtils, System.Classes,
  Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Controls,
  LightVcl.Visual.AppDataForm, LightVcl.Translate;

TYPE
  TfrmTranslSelector = class(TLightForm)
    btnApplyLang: TButton;
    btnRefresh  : TButton;
    btnTranslate: TButton;
    grpChoose   : TGroupBox;
    lblAuthors  : TLabel;
    ListBox     : TListBox;
    Panel2      : TPanel;
    procedure ListBoxDblClick  (Sender: TObject);
    procedure btnApplyLangClick(Sender: TObject);
    procedure btnRefreshClick  (Sender: TObject);
    procedure btnTranslateClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
  private
    function GetSelectedFileName: string;
    function GetSelectedFilePath: string;
    procedure ApplyLanguage;
  public
    function IsEnglish: Boolean;
    function PopulateLanguageFiles: Boolean;
    class procedure ShowSelector; static;
  end;




IMPLEMENTATION {$R *.dfm}

USES
  LightCore.AppData, LightVcl.Visual.AppData, LightVcl.Common.Dialogs, LightCore.IO, LightVcl.Common.IO, FormTranslEditor;


{ Shows the language selector form as a modal dialog.
  Translator must be initialized before calling this method. }
class procedure TfrmTranslSelector.ShowSelector;
var
  frmSelector: TfrmTranslSelector;
begin
  Assert(Translator <> NIL, 'Translator must be initialized before showing selector');

  AppData.CreateFormHidden(TfrmTranslSelector, frmSelector);
  try
    { Hide translation editor button on first run (no settings yet) }
    frmSelector.btnTranslate.Visible:= NOT AppData.RunningFirstTime;
    frmSelector.btnRefresh.Visible:= frmSelector.btnTranslate.Visible;
    frmSelector.PopulateLanguageFiles;
    frmSelector.ShowModal;
  finally
    FreeAndNil(frmSelector);
  end;
end;


procedure TfrmTranslSelector.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= TCloseAction.caFree;
end;







{-------------------------------------------------------------------------------------------------------------
   APPLY LANGUAGE
-------------------------------------------------------------------------------------------------------------}

procedure TfrmTranslSelector.ListBoxDblClick(Sender: TObject);
begin
  ApplyLanguage;
end;


{ Applies the selected language file to the application.
  Shows translator credits after successful application. }
procedure TfrmTranslSelector.ApplyLanguage;
begin
  if ListBox.ItemIndex < 0 then EXIT;

  if GetSelectedFileName = ''
  then MessageInfo('Please select a language!')
  else
    if FileExistsMsg(GetSelectedFilePath)
    then
      begin
        Translator.CurLanguage:= Translator.GetLangFolder + GetSelectedFileName;
        lblAuthors.Caption:= 'Translated by: ' + Translator.Authors;
        lblAuthors.Visible:= TRUE;
      end
    else
      MessageWarning('Cannot load language file!');
end;


procedure TfrmTranslSelector.btnApplyLangClick(Sender: TObject);
begin
  ApplyLanguage;
  Close;
end;







{-------------------------------------------------------------------------------------------------------------
   UTILS
-------------------------------------------------------------------------------------------------------------}

{ Returns the full path to the selected language file, or empty string if none selected }
function TfrmTranslSelector.GetSelectedFilePath: string;
begin
  if ListBox.ItemIndex < 0
  then Result:= ''
  else Result:= Translator.GetLangFolder + GetSelectedFileName;
end;


{ Returns the file name (with .ini extension) of the selected language, or empty string if none selected }
function TfrmTranslSelector.GetSelectedFileName: string;
begin
  if ListBox.ItemIndex < 0
  then Result:= ''
  else Result:= ListBox.Items.Strings[ListBox.ItemIndex] + '.ini';
end;


{ Returns True if the selected language is English }
function TfrmTranslSelector.IsEnglish: Boolean;
begin
  if ListBox.ItemIndex < 0
  then Result:= TRUE
  else Result:= SameText('English', ListBox.Items.Strings[ListBox.ItemIndex]);
end;


{ Populates the listbox with available language files from the Lang folder.
  Pre-selects the previously used language if available, otherwise selects the first one.
  Returns False if no language files were found. }
function TfrmTranslSelector.PopulateLanguageFiles: Boolean;
var
  iLastLang: Integer;
  Files: TStringList;
begin
  ListBox.Clear;

  { Read all .ini files in Lang folder }
  Files:= ListFilesOf(Translator.GetLangFolder, '*.ini', True, FALSE);
  try
    if Files.Count = 0
    then AppData.LogWarn('No language files detected in ' + Translator.GetLangFolder);

    { Remove path & extension, then populate the list box }
    for var s in Files do
      ListBox.Items.Add(ExtractOnlyName(s));
  finally
    FreeAndNil(Files);
  end;

  { Pre-select appropriate language }
  if ListBox.Items.Count > 0 then
    begin
      iLastLang:= ListBox.Items.IndexOf(ExtractOnlyName(Translator.CurLanguageName));
      if iLastLang < 0
      then
        begin
          { Pre-select the first language in the list }
          ListBox.Selected[0]:= True;  //ToDo: Pre-select the English lang preferentially
          ListBox.ItemIndex:= 0;
        end
      else
        begin
          { Pre-select the last language used }
          ListBox.Selected[iLastLang]:= True;
          ListBox.ItemIndex:= iLastLang;
        end;
    end;

  Result:= ListBox.Items.Count > 0;
end;



procedure TfrmTranslSelector.btnRefreshClick(Sender: TObject);
begin
  PopulateLanguageFiles;
end;


procedure TfrmTranslSelector.FormActivate(Sender: TObject);
begin
  PopulateLanguageFiles;
end;


procedure TfrmTranslSelector.btnTranslateClick(Sender: TObject);
begin
  TfrmTranslEditor.ShowEditor;
end;



end.
