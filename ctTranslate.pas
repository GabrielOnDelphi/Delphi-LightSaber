unit ctTranslate;
{========================================================================================  
   Gabriel Moraru / Heracle BioSoft SRL  
   2022-09  
   See Copyright.txt  
   RTTI-based automatic language translator engine.  
  
-----------------------------------------------------------------------------------------  

   **DESCRIPTION**  
     This class will translate all GUI strings (for example TLabel.Caption, TLabel.Hint, TMenuItem.Caption)  
     for all live forms in your application.  
     The text is stored in an INI file which can be sent to DeepL or GoogleTranslate for translation.  
     This library is now part of the [LightSaber Core](https://github.com/GabrielOnDelphi/Delphi-LightSaber-CoreLib) library.
     
-----------------------------------------------------------------------------------------  
  
   **Advantages** 
     Changing language is life without the need to restart the app  
     The text translated can be seen live, as we translate it  
     The user can create his own translations easily (with the included utility)  
     Full Unicode support  
     All text is automatically saved to a file.  
        The user does not have to collect the text manually from the screen.  
        No room for errors (missed/hidden text).  
     Instant/automatic translations via Google Translate (not fully implemented yet)  
        Press the "Translate to xyz language" button, then 3 seconds later the GUI switches to that language.  
     RTTI-based - Supports everything  
        Support for multiple properties (Caption, TextHint, or Hint).  
        Support for ANY classic Delphi control (TButton, TLable, TCheckbox, etc) that implements those properties.  
        Support for ANY classic Delphi component (TMenu, TAction, etc)  
        Support for ANY custom-made/3rd party Delphi component  
     Easy to integrate.  
        Only 4 lines of code are required.  
        Low dependency  
     Clean code, documented, tested (mostly) & Demo app  
  
   **Limitations**  
   
       1. Cannot translate forms that are not live (naturally)  
          Proposed solution: Add a flag like TTranslator.LiveTranslation. When a new form is created it will check the flag. If the flag is true, the form will save itself to the English INI file.  
  
       2. Strings into the source code will not be translated automatically (naturally)  
          Proposed solution: Add a function that can read strings from the INI file. Add support for something similar to the Format(%s %f Fd) Delphi function.  
          Proposed solution: If the strings are embedded as resources, we can translate them live.  
  
--------------------------------------------------------------------------------------------------------------  

   **HOW TO USE IT**  
  
    1. To load a language into all LIVE forms in your app:  
  
       procedure TMainForm.LateInitialize;  
       begin  
        LoadForm(Self);  
        Translator:= TTranslator.Create;  
  
        // If no translation was selected (program's first start) the "Choose Language" dialog will appear.  
        Translator.LoadLastTranslation;  
       end;  
  
  
    2. To load a language into a form that is not live (we do it on the form's creation):  
  
       procedure TDynamicForm.Initialize;  
       begin  
        // Call LoadFormTranlation() for DYNAMICALLY created forms, when they are created  
        Translator.LoadFormTranlation(DynamicForm);  
       end;  
  
  
    3. Save all live forms to the INI file to be translated:  
  
       a. Call Translator.SaveTranslation;  //Note: Only currently existing/live forms will be saved to disk.  
       b. Manually translate the text (or use www.DeepL.com). There is a helper tool in FormTranslator.pas.  
       c. Place the newly translated file in the 'Lang' folder. The program will discover the new file when the ShowSelectLanguage procedure is called (no need to restart).  
  
  
    4. Let the user choose/switch a language:  
  
       a. Call FormSelectLang.pas ShowSelectLanguage  
       b. Languages are switched 'Live'. No need to restart the program.  
  
--------------------------------------------------------------------------------------------------------------  
  
    **INFO**  
       The path where the translation files are stored is returned by GetLangFolder().  
       If no translation is found in the INI file for a GUI control, its text will be left untranslated.  
       Mark controls that should not be translated (for ex, labels that will have their caption changed dynamically) with Tag=128  
  
    **WARNING**  
       Make sure you make your GUI controls (labels, buttons, etc) large enough. Some languages require more space (more characters) than others.  
  
    TESTER  
       c:\Myprojects\Packages\LightSaber\Demo\Translation system\TranslatorTester.dpr  
--------------------------------------------------------------------------------------------------------------

ToDo 1:
  Add support for strings in PAS files.
  Ideas:
     1. Have a tool to parse all PAS files for ShowMessage.
        We can know where the text resides (which unit) by using TObject.UnitName. https://stackoverflow.com/questions/73892045/can-a-routine-know-the-unit-where-it-runs
     2. Save the strings as Const in a single PAS file.
        Have a PAS file for each language.
        Issue: requires program recompilation!
     3. Save the strings as static resources (stringresource).
        Have a tool to parse all PAS files and extract the strings.
     4!
     Const
       Unique_IDx: TLangID = 'Unique_str_as_ID';  // 'Real text string' so the human transaltor can understand it + instructions for human.  //#TextChanged directive

       Call:
          ShowMessage(Trs(Unique_str_as_ID));
          or
          Mesaj(Unique_str_as_ID);

       'Unique_str_as_ID' is then loaded from a INI or CSV files (one per language) in the following format: Unique_IDx = 'some text'.
       If the string is not found in the INI file the const is used and an warning is logged.
        Create a function Trs(TextID) that will take the ID and return the text based on the installed language.
        Pass the Const identifier.
        The Trs function can be integrated directly into an overloaded variant of the Mesaj() procedure.
      5.
         Variation on the (4) idea.
         Put the Unique_str_as_ID in an external ini file (EN): 57 = 'String to translate.'
         An external tool will pickup the numbers and generate other ini files (DE, CH, RO, ES).
         At runt time, at language selection, we read the appropriate file and load all the strings in a TDictionary, so locating the string this way will be very fast.
         I can have a comment to indicate that a line was changed so the translator can see it. Once the translation is updated, the comment will be deleted by the tool.
         What will happen wit conflicting numbers?

//ToDo 1: Make program work even if there is no default (EN) file in the Lang folder
//todo 3: I need special labels, with two fields: one that is static and needs to be translated, and one that is dynamic and does not appear in the INI file. Example: "Battery status: 30%" . The 30% is the dynamic part
//ToDo 4: When the program starts, the default selected language should be system's language.
//todo 4: don't save internet labels: inetCodecFFD.Hint=https://sourceforge.net/projects/ffdshow-tryout/files/latest/download
//ToDo 5: Should I save forms individualy or all in one INI file?
//ToDo 5: With BioniX I might reach the 64KB limit of the INI file (50KB for Sp, 46KB for En). Use own format.


{
BUG:
  The translation for the caption holding the version of BioniX (in AutoUpdater form) was loaded from the translator.
  Details: Now I load the translation later, after the forms have been fully initializaed,
           in TTranslator.LoadTranslation(FileName) via uInitialization.LateInitialization

   Dirty fix:
      * Make sure I load the translation EARLY before I do any initialization to the form where I might change the Caption of labels.
      * Call TTranslator.LoadTranslation every time I create a form

   Full fix:
      * Set control's Tag to 128 so it will be excluded from the translation. Problem: it will remain permanently untranslated.
      * Set control's caption via GetLangStr() dynamically.
}


INTERFACE
USES
  System.Classes, System.SysUtils, System.IniFiles, System.TypInfo, Vcl.Forms, Vcl.Menus, Vcl.ExtCtrls;

TYPE
 TTranslator = class(TObject)
  private
    LastLanguage: string;  // Used to skip translating a form when it is already in the chousen language.
    procedure WriteComponent(Component: TComponent; Section: string; Ini: TMemIniFile);
    procedure ReadComponent (Component: TComponent; Section: string; Ini: TMemIniFile);
    procedure WriteProperty (Component: TComponent; ParentName, PropertyType, Section: string; Ini: TMemIniFile);
    procedure ReadProperty  (Component: TComponent; ParentName, PropertyType, Section: string; Ini: TMemIniFile);
  protected
    procedure _saveFormTranlation (Form: TForm; Ini: TMemIniFile);
  public
    Authors: string;               // Credits the author of the current translation file
    CurLanguage: string;           // The language currently used for translation. Example: FileName.ini (only filename, no folder/path)
    DontSaveEmpty: Boolean;        // Don't save text properties that are empty
    ParseCtrlsWithAction: boolean; // If true, save to ini all controls even if they have an assigned action. If false, we let the associated action to set control's cation/hint

    constructor Create;
    destructor Destroy; override;
    function GetLangFolder: string;

    procedure LoadDefaultTranslation;
    procedure LoadLastTranslation;
    procedure LoadFormTranlation      (Form: TForm; ForceLoad: Boolean= FALSE);
    procedure LoadTranslationAllForms (CONST FileName: string; ForceLoad: Boolean= FALSE);
    procedure SaveTranslation         (CONST FileName: string; Overwrite: Boolean= TRUE);
 end;

function trs(CONST s: string): string;


VAR
   Translator: TTranslator;

IMPLEMENTATION

USES
   FormSelectLang, cmVclUtils, ccCore, ccIO, cmINIFileQuick, ccAppData;


CONST
   DefaultLang= 'English.ini';
   NoLanguage = 'No_Language';



constructor TTranslator.Create;
begin
  inherited Create;
  DontSaveEmpty:= TRUE;
  ForceDirectories(GetLangFolder);  // Make sure that the folders exists
  CurLanguage:= ReadString('Last_Language', NoLanguage); { English is the default }
end;


destructor TTranslator.Destroy;
begin
  WriteString('Last_Language', CurLanguage);
  inherited Destroy;
end;




{--------------------------------------------------------------------------------
  Main functions
--------------------------------------------------------------------------------}
procedure TTranslator.LoadDefaultTranslation;
begin
 CurLanguage:= DefaultLang;
 LoadLastTranslation;
end;


{ This will show the 'Choose language' dlg if this is the first time when the application starts. }
procedure TTranslator.LoadLastTranslation;
begin
 if CurLanguage = NoLanguage
 then FormSelectLang.ShowSelectLanguage
 else
   if FileExistsMsg(GetLangFolder+ CurLanguage)
   then LoadTranslationAllForms(GetLangFolder+ CurLanguage)
   else CurLanguage:= DefaultLang;   { Could not load the last language file. We go back to default (En) }

 //MesajInfo(TTranslator.UnitName);
end;


{ Load translation for ALL live forms }
procedure TTranslator.LoadTranslationAllForms(CONST FileName: string; ForceLoad: Boolean= FALSE);
begin
 VAR IniContainer:= TMemIniFile.Create(FileName);
 TRY
   Authors:= IniContainer.ReadString('Authors', 'Name', 'CubicDesign');

   // FormCount       = forms currently displayed on the screen
   // CustomFormCount = forms or property pages currently displayed on the screen
   for VAR i:= 0 to Screen.CustomFormCount - 1 DO
      LoadFormTranlation(Screen.Forms[I], ForceLoad);

   LastLanguage:= CurLanguage;
 FINALLY
   FreeAndNil(IniContainer);
 END;
end;



{ Load translation for specified form.
   Call this for forms that were not alive when the translation was applied (during the initialization of the app) }
procedure TTranslator.LoadFormTranlation(Form: TForm; ForceLoad: Boolean= FALSE);
VAR
   i: Integer;
   Section: string;
   FileName: string;
begin
 if FileExistsMsg(GetLangFolder+ CurLanguage)
 then FileName:=  GetLangFolder+ CurLanguage
 else EXIT;

 if ForceLoad
 then
 else
    if LastLanguage = CurLanguage
    then EXIT;   { We remain to english }

 VAR IniContainer:= TMemIniFile.Create(FileName);
 TRY
   Section:= Form.Name;   // Name of the form that hosts all the contols
   if Form.Tag = DontTranslate then EXIT;
   ReadComponent(Form, Section, IniContainer);   // Read form

   // Enumerate children of container
   for i:= 0 to Form.ComponentCount-1 do
      ReadComponent(Form.Components[i], Section, IniContainer);
 FINALLY
   FreeAndNil(IniContainer);
 END;
end;



procedure TTranslator.SaveTranslation(CONST FileName: string; Overwrite: Boolean= true);
VAR
   i: Integer;
   CurForm: TForm;
begin
 if Overwrite
 then DeleteFile(FileName);

 VAR IniContainer:= TMemIniFile.Create(FileName);
 TRY
  IniContainer.WriteString('Authors', 'Name', Authors);

  //
  // QUESTION: What is the condition for a form to appear in this list? Do the forms created with TForm.Create(Nil) appear here? Test it
  for i:= 0 to Screen.FormCount - 1 DO
   begin
     CurForm:= Screen.Forms[I];
     _saveFormTranlation(CurForm, IniContainer);
   end;

   IniContainer.UpdateFile;
 FINALLY
   FreeAndNil(IniContainer);
 END;
end;


procedure TTranslator._saveFormTranlation(Form: TForm; Ini: TMemIniFile);
var
   i: Integer;
   Section: string;
begin
  Section:= Form.Name;    // Name of the form that hosts all the contols
  if Form.Tag = DontTranslate then EXIT;
  WriteComponent(Form, Section, Ini);    // Set caption to self (form)

  // Enumerate children of container
  for i:= 0 to Form.ComponentCount-1 do
     WriteComponent(Form.Components[i], Section, Ini);

   Ini.UpdateFile;
end;






{------------------------------------------------------------------------------
   Load translation from INI for the specified component
------------------------------------------------------------------------------}

{ Read translation for this component from INI file }
procedure TTranslator.ReadComponent(Component: TComponent; Section: string; Ini: TMemIniFile);
VAR
   HasAct: Boolean;
begin
  if Component.Tag = DontTranslate then EXIT;

  // If an action is assigned, then we don't read this control. We let the action to set its caption/hint.
  HasAct:= cmVclUtils.HasAction(Component);

  if NOT HasAct
  OR (HasAct AND ParseCtrlsWithAction) then
   begin
    ReadProperty(Component, '', 'Hint',     Section, Ini);
    ReadProperty(Component, '', 'Caption',  Section, Ini);
    ReadProperty(Component, '', 'TextHint', Section, Ini);
    if Component.InheritsFrom(TLabeledEdit)
    then ReadProperty(TLabeledEdit(Component).EditLabel, Component.Name, 'Caption', Section, Ini);
   end;
end;


{ Write the text (caption, hint, etc) of this component to INI file }
procedure TTranslator.WriteComponent(Component: TComponent; Section: string; Ini: TMemIniFile);
VAR
   HasAct: boolean;
begin
 if Component.Tag = DontTranslate then EXIT;

  // If an action is assigned, then we don't read this control. We let the action to set its caption/hint.
  HasAct:= cmVclUtils.HasAction(Component);

  if NOT HasAct
  OR (HasAct AND ParseCtrlsWithAction) then
   begin
    {Note: We don't write here TEdit.Text. Maybe we should? The idea is that in most cases, this text will be filled by the program at run time }

    WriteProperty(Component, '', 'Hint'    , Section, Ini);
    WriteProperty(Component, '', 'Caption' , Section, Ini);
    WriteProperty(Component, '', 'TextHint', Section, Ini);

    { Composite controls }
    if Component.InheritsFrom(TLabeledEdit)
    then WriteProperty(TLabeledEdit(Component).EditLabel, Component.Name, 'Caption', Section, Ini);
   end;
end;





{-------------------------------------------------------------------------------
   Read/write strings to disk
   A "property" is a Caption, hint or text hint
//------------------------------------------------------------------------------}
procedure TTranslator.ReadProperty(Component: TComponent; ParentName, PropertyType, Section: string; Ini: TMemIniFile);
VAR
  Translation: string;
  Ident: string;
begin
 if IsPublishedProp(Component, PropertyType) then
   begin
    { Find name }
    if ParentName = ''
    then Ident:=                 Component.Name+ '.'+ PropertyType
    else Ident:= ParentName+'.'+ Component.Name+ '.'+ PropertyType;

    { Read }
    Translation:= Ini.ReadString(Section, Ident, '');
    Translation:= CRLFToEnter(Translation);           // We cannot write the ENTER character into a INI file
    if Translation > ''
    then SetStrProp(Component, PropertyType, Translation );
   end;

  // todo: else if Component is TComboBox then (Component as TComboBox).Text:= Translation;
end;


procedure TTranslator.WriteProperty(Component: TComponent; ParentName, PropertyType, Section: string; Ini: TMemIniFile);
VAR PropertyValue: string;
begin
 if IsPublishedProp(Component, PropertyType) then
   begin
    PropertyValue:= GetStrProp(Component, PropertyType);

    if PropertyValue.IsEmpty
    AND DontSaveEmpty then EXIT;    //why is it still saves the hint?

    // Don't save if this is a menu separator
    if (Component is TMenuItem)
    AND (Trim(PropertyValue) = '-')
    then EXIT;

    // Don't save items marked by programmer as special (text starts with @monkeytail)
    if FirstChar(PropertyValue) = '@'
    then EXIT;

    // We cannot write the ENTER character into a INI file. We replace the enters with the CRLF string
    if Pos(CR, PropertyValue) > 0
    then PropertyValue:= EnterToCRLF(PropertyValue);

    // Remove solitary CRs and LFs
    PropertyValue:= ReplaceLonellyCR(PropertyValue, '');
    PropertyValue:= ReplaceLonellyLF(PropertyValue, '');

    // Remove accelerators
    if SameStr(PropertyType, 'Caption')
    then PropertyValue:= ReplaceString(PropertyValue, '&', '');

    // Finally write to INI
    VAR Ident: string;
    if ParentName > ''
    then Ident:= ParentName+'.' + Component.Name+'.'+PropertyType
    else Ident:=                  Component.Name+'.'+PropertyType;
    Ini.WriteString(Section, Ident, PropertyValue);
   end;

  // todo: else if Component is TComboBox then  PropertyValue:= (Component as TComboBox).Text;
end;










{---------------------------------------------------------------------------
   UTILS
---------------------------------------------------------------------------}
function TTranslator.GetLangFolder: string;
begin
  Result:= AppData.CurFolder + 'Lang\';
end;


function trs(CONST s: string): string;
begin
 Result:= s;
end;


end.

