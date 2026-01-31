unit LightVcl.Common.Translate;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com

--------------------------------------------------------------------------------------------------------------
   AUTOMATIC LANGUAGE TRANSLATOR ENGINE FOR DELPHI [LightSaber]

   RTTI-based translation system for VCL applications.

-----------------------------------------------------------------------------------------

   **DESCRIPTION**

     This class translates all components (on a form) that have a text-like property
     (for example: TLabel.Caption, TLabel.Hint, TMenuItem.Caption).
     It does this automatically (no code necessary) for all forms created with AppData.CreateForm().
     The text is stored in an INI file.
     In the future the INI file will be sent to DeepL or GoogleTranslate (via API) for automatic translation.

-----------------------------------------------------------------------------------------

   **Features**

     Changes the GUI language live, without the need to restart the app.

     Embedded tools:
        It offers a "translation editor" form. See: FormTranslEditor.pas
        It offers a "translation selector" form. See: FormTranslSelector.pas
        The user can create their own translations easily (with the above mentioned utility).
        The text translated can be seen live, as we translate it (press Apply).

     Instant translations:
        TO BE IMPLEMENTED!
        Press the "Translate to xyz language" button. The GUI text is sent to "Google Translate" (API)
        and seconds later the GUI switches to that new language.

     RTTI-based
        Supports everything:
          Support for multiple properties (Caption, TextHint, or Hint).
          Support for ANY classic Delphi control (TButton, TLabel, TCheckbox, etc) that implements those properties.
          Support for ANY classic Delphi component (TMenu, TAction, etc)
          Support for ANY custom-made/3rd party Delphi component

     Programmer perspective
        Easy to integrate
        Zero lines of code necessary!
        Low dependency
        Full Unicode support
        Clean code, documented, tested (mostly) & Demo app

     User perspective
        Easy to use
        All text is automatically saved to an INI file.
        The user does not have to collect the text manually from the screen.
        No room for errors (missed/hidden text).
        The LightSaber\Demo\Tester AutoTranslator\Lang\How to translate.rtf file explain to the end user how to create his own translations.


   **Limitations**  
   
       1. Cannot translate forms that are not live (naturally)  
          Proposed solution: Add a flag like TTranslator.LiveTranslation.
          When a new form is created it will check the flag.
          If the flag is true, the form will save itself to the English INI file.
  
       2. Strings into the source code will not be translated automatically (naturally)  
          Proposed solution: Add a function that can read strings from the INI file. Add support for something similar to the Format(%s %f Fd) Delphi function.  
          Proposed solution: If the strings are embedded as resources, we can translate them live.  
  
--------------------------------------------------------------------------------------------------------------

   **HOW TO USE IT**

    * Automatically

         AppData will automatically create the Translator object.
         Create a new form with AppData.CreateForm(). This will take care of everything. Seriously!

         Check the LightCore.AppData, LightVcl.Visual.AppData
.pas for more info.

    * Manually

         procedure TForm1.FormCreate;
         begin
          Translator:= TTranslator.Create;
          Translator.LoadTranslation(Form1);
          Translator.Free;
         end;

    * To save all live forms to the INI file to be translated:

        a. Call Translator.SaveTranslation;                    //Note: Only currently existing/live forms will be saved to disk.
        b. Manually translate the text (or use www.DeepL.com). There is a helper tool in FormTranslEditor.pas.  https://github.com/DeveloppeurPascal/DeepL4Delphi
        c. Place the newly translated file in the 'Lang' folder. The program will discover the new file when the  New -> TfrmTranslSelector.ShowSelector procedure is called (no need to restart).


    * Let the user switch the language:

        a. Call TfrmTranslSelector.ShowSelector
        b. Languages are switched 'Live'. No need to restart the program.


--------------------------------------------------------------------------------------------------------------  
  
    **INFO**  
       The path where the translation files are stored, is returned by GetLangFolder().
       If no translation is found in the INI file for a GUI control, its text will be left untranslated.  
       Mark controls that should not be translated (for ex, labels that will have their caption changed dynamically) with Tag=128  
  
    **WARNING**  
       Make sure you make your GUI controls (labels, buttons, etc) large enough. Some languages require more space (more characters) than others.  
  
    **TESTER**
       LightSaber\Demo\Tester AutoTranslator\AutoTranslatorDemo.dpr

-------------------------------------------------------------------------------------------------------------}

{ToDo 1:
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
          MessageInfo(Unique_str_as_ID);

       'Unique_str_as_ID' is then loaded from a INI or CSV files (one per language) in the following format: Unique_IDx = 'some text'.
       If the string is not found in the INI file the const is used and a warning is logged.
        Create a function Trs(TextID) that will take the ID and return the text based on the installed language.
        Pass the Const identifier.
        The Trs function can be integrated directly into an overloaded variant of the MessageInfo() procedure.
      5.
         Variation on the (4) idea.
         Put the Unique_str_as_ID in an external ini file (EN): 57 = 'String to translate.'
         An external tool will pickup the numbers and generate other ini files (DE, CH, RO, ES).
         At runt time, at language selection, we read the appropriate file and load all the strings in a TDictionary, so locating the string this way will be very fast.
         I can have a comment to indicate that a line was changed so the translator can see it. Once the translation is updated, the comment will be deleted by the tool.
         What will happen wit conflicting numbers?}


//ToDo 1: Make program work even if there is no default (EN) file in the Lang folder
//todo 3: I need special labels, with two fields: one that is static and needs to be translated, and one that is dynamic and does not appear in the INI file. Example: "Battery status: 30%" . The 30% is the dynamic part
//ToDo 4: When the program starts, the default selected language should be system's language.
//todo 4: don't save internet labels: inetCodecFFD.Hint=https://sourceforge.net/projects/ffdshow-tryout/files/latest/download
//ToDo 5: Should I save forms individually or all in one INI file?
//ToDo 5: With BioniX I might reach the 64KB limit of the INI file (50KB for Sp, 46KB for En). Use own format.


{
BUG:
  The translation for the caption holding the version of BioniX (in AutoUpdater form) was loaded from the translator.
  Details: Now I load the translation later, after the forms have been fully initialized,
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
  System.Classes, System.SysUtils, System.IniFiles, System.TypInfo,
  Vcl.Forms, Vcl.Menus, Vcl.ExtCtrls,
  LightCore.AppData;

{ Bitwise constants for TControl.Tag }
CONST
   DontTranslate= 128; { 128 = binary 1000 0000 } {Note: in Delphi11 we can write it directly as a binary literal: %10000000 }

TYPE
 TTranslator = class(TObject)
  private
    FCurLanguage: string;           // The language currently used for translation. Full path. Example: c:\AppName\Lang\German.ini
    LastLanguage: string;           // Used to skip translating a form when it is already in the chosen language.  THIS IS A SHORT NAME
    FOnTranslationLoaded: TNotifyEvent;
    FAppData: TAppDataCore;
    procedure WriteComponent(Component: TComponent; CONST Section: string; Ini: TMemIniFile);
    procedure ReadComponent (Component: TComponent; CONST Section: string; Ini: TMemIniFile);
    procedure WriteProperty (Component: TComponent; CONST ParentName, PropertyType, Section: string; Ini: TMemIniFile);
    procedure ReadProperty  (Component: TComponent; CONST ParentName, PropertyType, Section: string; Ini: TMemIniFile);

    procedure setCurLanguage(const Value: string);
    function  getCurLanguage: string;
    procedure saveFormTranslation (Form: TForm; Ini: TMemIniFile);
    function  ReadString(CONST Identifier, DefaultVal: string): string;
    procedure WriteString(const Identifier, s: string);
  protected
    function  DefaultLang: string;
  public
    Authors: string;                // Credits the author of the current translation file
    DontSaveEmpty: Boolean;         // Don't save text properties that are empty
    ParseCtrlsWithAction: boolean;  // If true, save to ini all controls even if they have an assigned action. If false, we let the associated action to set control's caption/hint

    constructor Create(aAppData: TAppDataCore);
    destructor Destroy; override;


    procedure LoadDefaultTranslation;
    procedure LoadLastTranslation;
    procedure LoadTranslation (Form: TForm; ForceLoad: Boolean= FALSE);            overload;
    procedure LoadTranslation (ForceLoad: Boolean= FALSE);                         overload;
    procedure SaveTranslation (CONST FileName: string; Overwrite: Boolean= TRUE);

    function  GetLangFolder  : string;
    property  CurLanguageName: string read getCurLanguage;                        // Only name. Example: FileName.ini
    property  CurLanguage    : string read FCurLanguage write setCurLanguage;     // The language currently used for translation. Example: c:\AppName\Lang\FileName.ini (full path)

    property  OnTranslationLoaded: TNotifyEvent read FOnTranslationLoaded write FOnTranslationLoaded;
 end;


function trs(CONST s: string): string;


VAR
   Translator: TTranslator;


IMPLEMENTATION

USES
   LightVcl.Common.IniFile, LightVcl.Common.VclUtils, LightCore, LightCore.IO;

CONST
   NoLanguage = 'No_Language';



constructor TTranslator.Create(aAppData: TAppDataCore);
begin
  inherited Create;
  if aAppData = NIL
  then raise Exception.Create('TTranslator.Create: aAppData parameter cannot be nil');

  FAppData:= aAppData;
  DontSaveEmpty:= TRUE;
  FCurLanguage:= GetLangFolder + ReadString('Last_Language', DefaultLang); // Set the last used language. If none, default to English
end;


destructor TTranslator.Destroy;
begin
  WriteString('Last_Language', CurLanguageName);
  inherited Destroy;
end;





{--------------------------------------------------------------------------------
  Main functions
--------------------------------------------------------------------------------}

{ Sets the current language and immediately reloads translations for all live forms.
  Value must be a full path to the language INI file (e.g., 'C:\App\Lang\German.ini'). }
procedure TTranslator.setCurLanguage(const Value: string);
begin
  Assert(Pos(PathDelim, Value) > 0, 'CurLanguage var must contain the full path!');

  FCurLanguage:= Value;
  LoadTranslation;
end;


{ Returns only the filename portion of the current language (e.g., 'German.ini'). }
function TTranslator.getCurLanguage: string;
begin
  Result:= ExtractFileName(FCurLanguage);
end;


{ Returns the default language filename (just the filename, not full path).
  Used as default value in ReadString when no Last_Language is saved. }
function TTranslator.DefaultLang: string;
begin
  Result:= 'English.ini';
end;


procedure TTranslator.LoadDefaultTranslation;
begin
  CurLanguage:= GetLangFolder + DefaultLang;
end;


{ This will show the 'Choose language' dlg if this is the first time when the application starts. }
procedure TTranslator.LoadLastTranslation;
begin
 if CurLanguageName = NoLanguage
 then //FormTranslSelector. New -> TfrmTranslSelector.ShowSelector
 else
   if FileExists(CurLanguage)
   then LoadTranslation
   else LoadDefaultTranslation;   { Could not load the last language file. We go back to default (En) }
end;


{ Load translation for ALL live forms }
procedure TTranslator.LoadTranslation(ForceLoad: Boolean= FALSE);
begin
 VAR IniContainer:= TMemIniFile.Create(CurLanguage);
 TRY
   Authors:= IniContainer.ReadString('Authors', 'Name', 'CubicDesign');

   // FormCount       = count of TForm descendants currently displayed on the screen
   // CustomFormCount = count of TCustomForm descendants (includes property pages)
   // Note: Screen.Forms[] only contains TForm descendants (FormCount items), not TCustomForm descendants.
   for VAR i:= 0 to Screen.FormCount - 1 DO
      LoadTranslation(Screen.Forms[i], ForceLoad);

   LastLanguage:= CurLanguageName;
 FINALLY
   FreeAndNil(IniContainer);
 END;

 if Assigned(FOnTranslationLoaded)
 then FOnTranslationLoaded(Self);
end;



{ Load translation for specified form.
   Call this for forms that were not alive when the translation was applied (during the initialization of the app) }
procedure TTranslator.LoadTranslation(Form: TForm; ForceLoad: Boolean= FALSE);
VAR
   i: Integer;
   Section: string;
begin
  Assert(Form <> NIL, 'TTranslator.LoadTranslation: Form parameter cannot be nil');

  if NOT FileExists(CurLanguage)
  then EXIT;

  if (LastLanguage = CurLanguageName)
  AND NOT ForceLoad
  then EXIT;

  if Form.Tag = DontTranslate
  then EXIT;  // Check early to avoid unnecessary INI file I/O

  VAR IniContainer:= TMemIniFile.Create(CurLanguage);
  TRY
    Section:= Form.Name;   // Name of the form that hosts all the controls
    ReadComponent(Form, Section, IniContainer);   // Read form's own properties

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
  ForceDirectories(GetLangFolder); // Ensure the folder exists. But don't do it sooner than necessary.

  if Overwrite
  then DeleteFile(FileName);

  VAR IniContainer:= TMemIniFile.Create(FileName, TEncoding.UTF8);
  TRY
   IniContainer.WriteString('Authors', 'Name', Authors);

   // QUESTION: What is the condition for a form to appear in this list? Do the forms created with TForm.Create(Nil) appear here? Test it
   for i:= 0 to Screen.FormCount - 1 DO
    begin
      CurForm:= Screen.Forms[I];
      saveFormTranslation(CurForm, IniContainer);
    end;

    IniContainer.UpdateFile;
  FINALLY
    FreeAndNil(IniContainer);
  END;
end;


procedure TTranslator.saveFormTranslation(Form: TForm; Ini: TMemIniFile);
var
   i: Integer;
   Section: string;
begin
  Assert(Form <> NIL, 'TTranslator.saveFormTranslation: Form parameter cannot be nil');
  Assert(Ini <> NIL, 'TTranslator.saveFormTranslation: Ini parameter cannot be nil');

  if Form.Tag = DontTranslate
  then EXIT;

  Section:= Form.Name;    // Name of the form that hosts all the controls
  WriteComponent(Form, Section, Ini);    // Write form's own properties

  // Enumerate children of container
  for i:= 0 to Form.ComponentCount-1 do
     WriteComponent(Form.Components[i], Section, Ini);

  Ini.UpdateFile;
end;






{------------------------------------------------------------------------------
   Load translation from INI for the specified component
------------------------------------------------------------------------------}

{ Read translation for this component from INI file }
procedure TTranslator.ReadComponent(Component: TComponent; CONST Section: string; Ini: TMemIniFile);
VAR
   HasAct: Boolean;
begin
  if Component.Tag = DontTranslate then EXIT;

  // If an action is assigned, then we don't read this control. We let the action to set its caption/hint.
  HasAct:= LightVcl.Common.VclUtils.HasAction(Component);

  if NOT HasAct
  OR (HasAct AND ParseCtrlsWithAction) then
   begin
    ReadProperty(Component, '', 'Hint',     Section, Ini);
    //ReadProperty(Component, '', 'Caption',  Section, Ini);  //Important: don't load form caption via translator because it can show obsolete information such as version number.
    ReadProperty(Component, '', 'TextHint', Section, Ini);
    if Component.InheritsFrom(TLabeledEdit)
    then ReadProperty(TLabeledEdit(Component).EditLabel, Component.Name, 'Caption', Section, Ini);
   end;
end;


{ Write the text (caption, hint, etc) of this component to INI file }
procedure TTranslator.WriteComponent(Component: TComponent; CONST Section: string; Ini: TMemIniFile);
VAR
   HasAct: boolean;
begin
 if Component.Tag = DontTranslate then EXIT;

  // If an action is assigned, then we don't write this control. We let the action to set the control's caption/hint.
  HasAct:= LightVcl.Common.VclUtils.HasAction(Component);

  if NOT HasAct
  OR (HasAct AND ParseCtrlsWithAction) then
   begin
    {Note: We don't write here TEdit.Text. Maybe we should? The idea is that in most cases, this text will be filled by the program at run time }

    WriteProperty(Component, '', 'Hint'    , Section, Ini); 
    //WriteProperty(Component, '', 'Caption' , Section, Ini);
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
procedure TTranslator.ReadProperty(Component: TComponent; CONST ParentName, PropertyType, Section: string; Ini: TMemIniFile);
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

    { Read from INI and convert CRLF markers back to actual line breaks }
    Translation:= Ini.ReadString(Section, Ident, '');
    Translation:= CRLFToEnter(Translation);  { Convert 'CRLF' text marker back to actual CR+LF characters }
    if Translation <> ''
    then SetStrProp(Component, PropertyType, Translation);
   end;

  // todo: else if Component is TComboBox then (Component as TComboBox).Text:= Translation;
end;


procedure TTranslator.WriteProperty(Component: TComponent; CONST ParentName, PropertyType, Section: string; Ini: TMemIniFile);
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

    { Finally write to INI }
    VAR Ident: string;
    if ParentName <> ''
    then Ident:= ParentName + '.' + Component.Name + '.' + PropertyType
    else Ident:= Component.Name + '.' + PropertyType;
    Ini.WriteString(Section, Ident, PropertyValue);
   end;

  // todo: else if Component is TComboBox then  PropertyValue:= (Component as TComboBox).Text;
end;










{---------------------------------------------------------------------------
   UTILS
---------------------------------------------------------------------------}
function TTranslator.GetLangFolder: string;
begin
  Result:= FAppData.AppFolder + 'Lang\';
end;


{ Translate a string (placeholder function for future implementation).
  This function is intended to translate strings embedded in source code (not GUI controls).
  Currently returns the input string unchanged.
  Future implementation will lookup the string in a language-specific INI or dictionary. }
function trs(CONST s: string): string;
begin
  Result:= s;  //ToDo: Implement translation lookup from language-specific INI file
end;



// Remember the last used language
function TTranslator.ReadString(CONST Identifier, DefaultVal: string): string;
begin
  VAR IniFile:= TIniFileApp.Create(FAppData.AppName, FAppData.IniFile);
  TRY
    Result:= IniFile.ReadString('Translator', Identifier, DefaultVal);
  FINALLY
    FreeAndNil(IniFile);
  END;
end;

procedure TTranslator.WriteString(CONST Identifier, s: string);
begin
  VAR IniFile:= TIniFileApp.Create(FAppData.AppName, FAppData.IniFile);
  TRY
    IniFile.WriteString('Translator', Identifier, s)
  FINALLY
    FreeAndNil(IniFile);
  END;
end;



end.
