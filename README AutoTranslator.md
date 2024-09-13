# DelphiLightSaber-AutoTranslator  
RTTI-based automatic language translator engine.  
Offcial page: https://gabrielmoraru.com/delphi-rtti-based-automatic-language-translator-engine/

Translate your application with only two lines of code.
![Translator Code](https://github.com/GabrielOnDelphi/DelphiLightSaber-AutoTranslator/assets/31410401/5e2722e2-93cf-4628-8995-e76f5214b1e4)



![TranslationEn](https://github.com/GabrielOnDelphi/DelphiLightSaber-AutoTranslator/assets/31410401/758c0f96-b4bc-4ac3-b54b-d34d013e1083)


![TranslationDe](https://github.com/GabrielOnDelphi/DelphiLightSaber-AutoTranslator/assets/31410401/576b8cb8-5c82-480c-9965-762c7bedc578)


![TranslationCn](https://github.com/GabrielOnDelphi/DelphiLightSaber-AutoTranslator/assets/31410401/28583504-fbc3-400c-b6c9-cfe0777a1af3)

  
  
{===============================================================  
   Gabriel Moraru / Heracle BioSoft SRL  
   2022-09  
   See Copyright.txt  
   RTTI-based automatic language translator engine.  
  
----------------------------------------------------------------     

   **DESCRIPTION**    
     This class will translate all GUI strings (for example TLabel.Caption, TLabel.Hint, TMenuItem.Caption)  
     for all live forms in your application.  
     The text is stored in an INI file which can be sent to DeepL or GoogleTranslate for translation. 


**Downloading the code**  

This repository is only a placeholder and source of information. It is not updated since 01.2023.
The code should be downloaded now from the [LightSaber Core](https://github.com/GabrielOnDelphi/Delphi-LightSaber) repository.

_________________
     
  
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
       c:\Myprojects\Packages\CubicCommonControls\Demo\Translation system\TranslatorTester.dpr  
       
--------------------------------------------------------------------------------------------------------------  
  
