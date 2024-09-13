UNIT FormSelectLang;

{-------------------------------------------------------------------------------------------------------------
   2021.03.18
   Let user choose a language file
   See Copyright.txt
-------------------------------------------------------------------------------------------------------------
   How to use it:
      Instantiate the TTranslator class
      The path where the translation files are stored is returned by GetLangFolder
      Call PopulateLanguageFiles to populate a listbox with all discovered language files

   Default language
      The English.ini file can be empty! BioniX will load the default (design time) values.
      If the language is English this message will appear because if the En file is empty the current GUI strings will not be overwritten: 'A program restart may be necessary to apply this language...'

   Coupling warning:
      Don't add dependencies to CubicVisualControls here!
-------------------------------------------------------------------------------------------------------------}

INTERFACE
{.$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Controls, cTranslate;

TYPE
  TfrmLanguage = class(TForm)
    btnApplyLang: TButton;
    btnRefresh  : TButton;
    btnTranslate: TButton;
    grpChoose   : TGroupBox;
    lblAuthors  : TLabel;
    lblHint     : TLabel;
    ListBox     : TListBox;
    Panel2      : TPanel;
    procedure FormDestroy      (Sender: TObject);
    procedure ListBoxDblClick  (Sender: TObject);
    procedure btnApplyLangClick(Sender: TObject);
    procedure btnRefreshClick  (Sender: TObject);
    procedure btnTranslateClick(Sender: TObject);
    procedure FormKeyPress     (Sender: TObject; var Key: Char);
  private
    function GetSelectedFileName: string;
    function GetSelectedFilePath: string;
    procedure ApplyLanguage;
  public
    function IsEnglish: Boolean;  //unused
    function PopulateLanguageFiles: Boolean;
  end;

VAR
   frmLanguage: TfrmLanguage;


procedure ShowSelectLanguage;


IMPLEMENTATION {$R *.dfm}
USES
  cbAppData, cbINIFile, cbDialogs, ccIO, ccTextFile, cmIO, FormTranslator;



procedure ShowSelectLanguage;
begin
 Assert(Translator <> NIL);
 ForceDirectories(Translator.GetLangFolder);   { Make sure that the folders exists }

 VAR frmLanguage:= TfrmLanguage.Create(Application);
 TRY
   frmLanguage.Font:= Application.MainForm.Font;
   frmLanguage.btnTranslate.Visible:= NOT Appdata.RunningFirstTime;
   frmLanguage.btnRefresh.Visible:= frmLanguage.btnTranslate.Visible;
   LoadForm(frmLanguage);
   frmLanguage.PopulateLanguageFiles;    { Populate the ListBox with languages we found in 'Lang' folder }
   frmLanguage.ShowModal;
 FINALLY
   FreeAndNil(frmLanguage);
 END;
end;


procedure TfrmLanguage.FormDestroy(Sender: TObject);
begin
  SaveForm(Self);  //Localization warning: Don't add dependencies to CubicVisualControls here!
end;


procedure TfrmLanguage.FormKeyPress(Sender: TObject; var Key: Char);
begin
 if Ord(Key) = VK_ESCAPE then Close;
end;










{---------------------------------------------------------------------------
   LOAD EXISTING LANGUAGE FILE
---------------------------------------------------------------------------}
procedure TfrmLanguage.ListBoxDblClick(Sender: TObject);
begin
 ApplyLanguage;
end;


procedure TfrmLanguage.ApplyLanguage;
begin
  if ListBox.ItemIndex = 0 then EXIT;

  if GetSelectedFileName = ''
  then MesajInfo('Please select a language!')
  else
    if FileExistsMsg(GetSelectedFilePath)
    then
     begin
       Translator.CurLanguage:= GetSelectedFileName;
       Translator.LoadTranslationAllForms(GetSelectedFilePath, TRUE);

       //Caption := 'Loaded: '+ GetSelectedFilePath;
       lblAuthors.Caption:= 'Translated by: '+ Translator.Authors;
       lblAuthors.Visible:= TRUE;
     end
    else
      MesajWarning('Cannot load language file!');
end;


procedure TfrmLanguage.btnApplyLangClick(Sender: TObject);
begin
  ApplyLanguage;
  Close;
end;







{---------------------------------------------------------------------------
   UTILS
---------------------------------------------------------------------------}
function TfrmLanguage.GetSelectedFilePath: string;
begin
  if ListBox.ItemIndex < 0
  then Result:= ''
  else Result:= Translator.GetLangFolder+ GetSelectedFileName;
end;


function TfrmLanguage.GetSelectedFileName: string;
begin
  if ListBox.ItemIndex < 0
  then Result:= ''
  else Result:= ListBox.Items.Strings[ListBox.ItemIndex]+ '.ini';
end;


{ Reurns True if the selected language is English }
function TfrmLanguage.IsEnglish: Boolean;
begin
  if ListBox.ItemIndex < 0
  then Result:= TRUE
  else Result:= SameText('English', ListBox.Items.Strings[ListBox.ItemIndex]);
end;


{ Returns false if no language file was found }
function TfrmLanguage.PopulateLanguageFiles: Boolean;
VAR
   iLastLang: Integer;
   Files: TStringList;
begin
  ListBox.Clear;

  { Read all files in folder }
  Files:= ListFilesOf(Translator.GetLangFolder, '*.ini', True, FALSE);
  TRY
   if Files.Count = 0
   then AppData.LogWarn('No language files detected in '+ Translator.GetLangFolder);

   { Remove path & ext, then populate the list box }
   for var s in Files DO
     ListBox.Items.Add(ExtractOnlyName(ExtractFileName(s)));
  FINALLY
    FreeAndNil(Files);
  END;

  { Select lang }
  if ListBox.Items.Count > 0 then
   begin
     iLastLang:= ListBox.Items.IndexOf(ExtractOnlyName(Translator.CurLanguage));
     if iLastLang < 0
     then
      begin
       // Pre-select the first language in the list
       ListBox.Selected[0];  //ToDo: Pre-select the english lang (preferentially)
       ListBox.ItemIndex:= 0;
      end
     else
       begin
        // Pre-select the last language used
        ListBox.Selected[iLastLang];
        ListBox.ItemIndex:= iLastLang;
       end;
   end;

 Result:= ListBox.Items.Count > 0;
end;



procedure TfrmLanguage.btnRefreshClick(Sender: TObject);
begin
 PopulateLanguageFiles;
end;


procedure TfrmLanguage.btnTranslateClick(Sender: TObject);
begin
 VAR frmTranslator:= TfrmTranslator.Create(Application);
 frmTranslator.Show;
end;



end.
