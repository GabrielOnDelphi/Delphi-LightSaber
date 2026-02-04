# DelphiLightSaber-AutoTranslator

RTTI-based automatic language translator engine.  
Official page: https://gabrielmoraru.com/automatic-language-translator-engine-for-delphi-guis-update-2026/

Let your application self-translate to any language with only two lines of code!

---

## Overview

This library translates all GUI strings (TLabel.Caption, TLabel.Hint, TMenuItem.Caption, etc.) for all live forms in your application. The text is stored in INI files which can be sent to DeepL for automatic translation.

**Downloading the code**

This repository is only a placeholder and source of information.
The code should be downloaded from the [LightSaber Core](https://github.com/GabrielOnDelphi/Delphi-LightSaber) repository.

---

## Features

- **Live language switching** - Change language at runtime without restart
- **Live preview** - See translated text as you edit in the built-in editor
- **User-created translations** - Users can create their own translations with the included editor
- **Full Unicode support** - UTF-8 encoding for all languages
- **DeepL integration** - Automatic translation via DeepL API (free and pro tiers)
- **RTTI-based** - Supports ANY component with Caption/Hint/TextHint properties:
  - Classic Delphi controls (TButton, TLabel, TCheckbox, etc.)
  - Classic Delphi components (TMenu, TAction, etc.)
  - Works even with custom/3rd party components
- **Easy integration** - Only two lines of code required (zero when using my LightSaber framework)
- **Clean code** - Documented, tested & includes demo app.

## Cost Information (Free DeepL account)

**DeepL API Free:**
- Cost: Free
- Limit: 500,000 characters/month
- Endpoint: `https://api-free.deepl.com`

Login into your DeepL account. You will see three plans. The first one is free BUT you need to give your credit card. I gave mine. They haven't charged me but I went through the full payment process (verification). They say is to prevent abuse (people that generate fake accounts to get free keys).
The look for "API key". In that page, press "Generate new key". Copy the key into the program.

**DeepL API Pro:**
- Base fee: $5.49/month + $25 per 1,000,000 characters
- Unlimited characters
- Endpoint: `https://api.deepl.com`

**Typical usage:** 
A translation INI file is ~500-2000 characters. Free tier allows ~250-1000 file translations per month.


---

## How does it work?

The program uses RTTI to find components that have text properties like Caption and Hint. It save these properties as text, in an INI file that is translated to the desiered langauge (manually or automatically).

## Quick Start

### Automatic (Recommended)

If you are already using my LightSaber AppData framework, the translation is fully automatic. You don't have to write any code!

```pascal
// In DPR file - AppData handles everything
AppData := TAppData.Create('My Cool App');
AppData.CreateMainForm(TMainForm, MainForm, TRUE, TRUE, asFull);
AppData.Run;
```

---

### Manual Integration

If you don't use my LightSaber AppData framework, you need to create the object and call LoadLastTranslation.

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  Translator := TTranslator.Create(AppData);   // If no translation was selected (first start), the "Choose Language" dialog appears
  Translator.LoadLastTranslation;             
end;
```

All live forms (instantiated forms) will be translated.


### Dynamically created forms

If you create a form dynamically, you need to call LoadTranslation for that form in its constructor:

```pascal
procedure TDynamicForm.FormCreate(Sender: TObject);
begin
  // Call LoadTranslation() for forms created after app initialization
  Translator.LoadTranslation(DynamicForm);
end;
```

### Starting a new translation

When you want to translate your app, you call Translator.SaveTranslation(IniFileName). 
This will save all strings in all LIVE forms to the specified INI file.
If you want to include a form 
```pascal
// Save all live forms to INI file
Translator.SaveTranslation('c:\App\Lang\English.ini');
```

Then:
- Translate the file manually, or use the built-in editor, or use DeepL API
- Place the translated file in the `Lang` folder
- The program discovers new files automatically (no restart needed)

### Let users choose/switch language

```pascal
// Show language selector dialog
TfrmTranslSelector.ShowSelector;

// Or set language directly in code
Translator.CurLanguage := 'c:\App\Lang\German.ini';
```

### Responding to language changes

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
begin
  Translator.OnTranslationLoaded := TranslationLoaded;
end;

procedure TMainForm.TranslationLoaded(Sender: TObject);
begin
  Caption := 'Current language is: ' + Translator.CurLanguageName;
  //Do other updates here
end;
```

---

## INI File Format

```ini
[Authors]
Name=YourName

[MainForm]
btnOK.Caption=OK
btnOK.Hint=Click to confirm
btnCancel.Caption=Cancel
lblTitle.Caption=Welcome
LabeledEdit1.EditLabel.Caption=Enter value
```

**Structure:**
- Section = Form name
- Key = `ComponentName.PropertyType`
- For composite controls: `ParentName.ComponentName.PropertyType`
- Multiline text: Use `CRLF` marker for line breaks

**Common component prefixes:**
| Prefix | Component Type |
|--------|---------------|
| `lbl`  | Label         |
| `btn`  | Button        |
| `grp`  | GroupBox      |
| `chk`  | Checkbox      |
| `rad`  | RadioButton   |
| `frm`  | Form (window) |

---

## Translation Tips

### Accelerators

Menu items and buttons may contain the `&` character (e.g., "Sa&ve file"). This creates a keyboard accelerator - pressing the underlined letter triggers the action. You can:
- Keep the `&` in the same position
- Move it to a more appropriate character for your language
- Remove it entirely

### Preserving Spaces

If you find multiple spaces in a string, preserve them in your translation. These spaces are placeholders for dynamic values:
```
Change wallpaper after               minutes.
```

### Resetting to Original Strings

To reset the program to its original (untranslated) text:
1. Load the English translation
2. Delete the English.ini file
3. Restart the program

The program will warn that the file wasn't found, then continue with default strings.

### Updating Translations After Program Updates

#### Manually 

When a new version of your form(s) adds strings, use a diff tool (like BeyondCompare) to compare your translation with the updated English.ini. Only translate the new entries.

### Automatically 

Just press "Edit current translation". The program will translate only the new componentes in the GUI.

---

## Excluding Controls from Translation

Set `Tag = 128` on any control to exclude it from translation:

```pascal
CONST
  DontTranslate = 128;

// In form designer or code:
lblDynamicValue.Tag := DontTranslate;
```

Alternatively, prefix the caption with `@` at design time:  
```pascal

 lblTime.Caption := '@Runtime';  // Will not be saved to INI
```

Use exclusions for:
- Labels displaying dynamic values (e.g., "Monitor resolution: 1920x1200")
- Controls with programmatically set text

**For translators:** 
If you see dynamic values in the INI file (like resolution numbers, file paths), delete the entire line. The program will fill these at runtime.

---

## DeepL API Integration

The library includes `TDeepLTranslator` for automatic translation via DeepL API.

### Usage

```pascal
var
  Translator: TDeepLTranslator;
begin
  Translator := TDeepLTranslator.Create;
  try
    Translator.ApiKey := 'your-api-key';
    Translator.UseFreeAPI := True;  // Use free tier endpoint

    // Translate single text
    Result := Translator.TranslateText('Hello', 'DE');

    // Translate batch (up to 50 texts per request)
    Results := Translator.TranslateBatch(['Hello', 'World'], 'DE');

    // Translate entire INI file
    Translator.TranslateINIFile('English.ini', 'German.ini', 'DE');
  finally
    FreeAndNil(Translator);
  end;
end;
```

### Supported Languages

| Language   | Code | Language   | Code |
|------------|------|------------|------|
| English    | EN   | Polish     | PL   |
| German     | DE   | Russian    | RU   |
| French     | FR   | Japanese   | JA   |
| Spanish    | ES   | Chinese    | ZH   |
| Italian    | IT   | Korean     | KO   |
| Portuguese | PT   | Romanian   | RO   |
| Dutch      | NL   | And more...|      |

### Error Handling

```pascal
Translator.TranslateText('Hello', 'DE');
if Translator.LastError <> '' then
  ShowMessage('Error: ' + Translator.LastError);
```

Common HTTP errors:
- `401` - Invalid API key
- `429` - Rate limit exceeded
- `456` - Quota exceeded (free tier)

### API Limits

- Max 50 texts per batch request
- Max 128 KiB request size
- Source language auto-detected if not specified

---

## Limitations

1. **Only live forms** - Cannot translate forms that haven't been created yet
   - Proposed solution: Flag to auto-save new forms to English INI

2. **Source code strings** - Strings in `.pas` files are not translated automatically
   - Use `trs()` function placeholder for future support
   - Proposed solution: Parse PAS files for ShowMessage strings
   - Note that Delphi introduced recently a translation framework for strings in Delphi code.

3. **INI file limits:**
   - ~64KB per file (relevant for very large applications)
   - ~8KB per section (per form)

---

## Important Notes

- By default, the translation files are stored in `AppFolder\Lang\`. Use `Translator.GetLangFolder()` to obtain this path.
- If no translation found for a control, text remains unchanged.
- Make GUI controls large enough - some languages need more space.

---

## Demo Application

Located at: `LightSaber\Demo\VCL\Demo AutoTranslator\`

---

## Source Files

| File | Description |
|------|-------------|
| `LightVcl.Common.Translate.pas` | Main TTranslator engine |
| `LightVcl.Common.TranslatorAPI.pas` | DeepL API integration |
| `FormTranslEditor.pas` | Built-in translation editor |
| `FormTranslSelector.pas` | Language selector dialog |
| `FormTranslDeepL.pas` | DeepL translation dialog |

---

## Contributing Translations

If you translate an application using this library to your language, please consider sending the translation file to the application author for inclusion in future releases.

---

## Author

Gabriel Moraru
2026
