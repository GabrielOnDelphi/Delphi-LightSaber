UNIT FormTranslSelector;

{-------------------------------------------------------------------------------------------------------------
   www.GabrielMoraru.com
   2025.01

   Let user choose a language file.

--------------------------------------------------------------------------------------------------------------
   How to use it:
      Instantiate the TTranslator class
      The path where the translation files are stored is returned by GetLangFolder
      Call PopulateLanguageFiles to populate a listbox with all discovered language files

   Default language
      The English.ini file can be empty! In this cases the program will use the default (design time) strings.
      But in this case, a program restart will be necessary to apply that language.

   Coupling warning:
      Don't add dependencies to CubicVisualControls here!
-------------------------------------------------------------------------------------------------------------}

INTERFACE
{.$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  System.SysUtils, System.Classes,
  Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Controls,
  LightCom.AppDataForm, LightCom.Translate;

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
    function IsEnglish: Boolean;  //unused
    function PopulateLanguageFiles: Boolean;
    class procedure ShowSelector; static;
  end;




IMPLEMENTATION {$R *.dfm}
USES
  ccAppData, LightCom.AppData
, LightCom.Dialogs, ccIO, LightCom.IO, FormTranslEditor;



class procedure TfrmTranslSelector.ShowSelector;
VAR
   frmSelector: TfrmTranslSelector;
begin
  Assert(Translator <> NIL);

  AppData.CreateFormHidden(TfrmTranslSelector, frmSelector);
  TRY
    frmSelector.btnTranslate.Visible:= NOT AppData.RunningFirstTime;
    frmSelector.btnRefresh.Visible:= frmSelector.btnTranslate.Visible;
    frmSelector.PopulateLanguageFiles;    { Populate the ListBox with languages we found in 'Lang' folder }
    frmSelector.ShowModal;
  FINALLY
    FreeAndNil(frmSelector);
  END;
end;


procedure TfrmTranslSelector.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= TCloseAction.caFree;
end;







{---------------------------------------------------------------------------
   LOAD EXISTING LANGUAGE FILE
---------------------------------------------------------------------------}
procedure TfrmTranslSelector.ListBoxDblClick(Sender: TObject);
begin
  ApplyLanguage;
end;


procedure TfrmTranslSelector.ApplyLanguage;
begin
  if ListBox.ItemIndex < 0 then EXIT;

  if GetSelectedFileName = ''
  then MessageInfo('Please select a language!')
  else
    if FileExistsMsg(GetSelectedFilePath)
    then
      begin
        Translator.CurLanguage:= Translator.GetLangFolder+ GetSelectedFileName;

        lblAuthors.Caption:= 'Translated by: '+ Translator.Authors;
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







{---------------------------------------------------------------------------
   UTILS
---------------------------------------------------------------------------}
function TfrmTranslSelector.GetSelectedFilePath: string;
begin
  if ListBox.ItemIndex < 0
  then Result:= ''
  else Result:= Translator.GetLangFolder+ GetSelectedFileName;
end;


function TfrmTranslSelector.GetSelectedFileName: string;
begin
  if ListBox.ItemIndex < 0
  then Result:= ''
  else Result:= ListBox.Items.Strings[ListBox.ItemIndex]+ '.ini';
end;


{ Reurns True if the selected language is English }
function TfrmTranslSelector.IsEnglish: Boolean;
begin
  if ListBox.ItemIndex < 0
  then Result:= TRUE
  else Result:= SameText('English', ListBox.Items.Strings[ListBox.ItemIndex]);
end;


{ Returns false if no language file was found }
function TfrmTranslSelector.PopulateLanguageFiles: Boolean;
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
     ListBox.Items.Add(ExtractOnlyName(s));
  FINALLY
    FreeAndNil(Files);
  END;

  { Select lang }
  if ListBox.Items.Count > 0 then
   begin
     iLastLang:= ListBox.Items.IndexOf(ExtractOnlyName(Translator.CurLanguageName));
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
