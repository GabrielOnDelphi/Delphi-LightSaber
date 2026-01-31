# DelphiLightSaber-AutoTranslator

RTTI-based automatic language translator engine.
Official page: https://gabrielmoraru.com/delphi-rtti-based-automatic-language-translator-engine/

Translate your application with only two lines of code.
![Translator Code](https://github.com/GabrielOnDelphi/DelphiLightSaber-AutoTranslator/assets/31410401/5e2722e2-93cf-4628-8995-e76f5214b1e4)
![TranslationEn](https://github.com/GabrielOnDelphi/DelphiLightSaber-AutoTranslator/assets/31410401/758c0f96-b4bc-4ac3-b54b-d34d013e1083)
![TranslationDe](https://github.com/GabrielOnDelphi/DelphiLightSaber-AutoTranslator/assets/31410401/576b8cb8-5c82-480c-9965-762c7bedc578)

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
- **Automatic text collection** - All translatable text is automatically saved to INI file
- **DeepL integration** - Automatic translation via DeepL API (free and pro tiers)
- **RTTI-based** - Supports ANY component with Caption/Hint/TextHint properties:
  - Classic Delphi controls (TButton, TLabel, TCheckbox, etc.)
  - Classic Delphi components (TMenu, TAction, etc.)
  - Custom/3rd party components
- **Easy integration** - Zero lines of code when using AppData.CreateForm()
- **Clean code** - Documented, tested & includes demo app

---

## Quick Start

### Automatic (Recommended)

When using the LightSaber AppData framework, translation is fully automatic:

```pascal
// In DPR file - AppData handles everything
AppData := TAppData.Create('MyApp');
AppData.CreateMainForm(TMainForm, MainForm, TRUE, TRUE, asFull);
AppData.Run;
```

### Manual Integration

```pascal
procedure TForm1.FormCreate;
begin
  Translator := TTranslator.Create(AppData);   // If no translation was selected (first start), the "Choose Language" dialog appears
  Translator.LoadLastTranslation;
end;
```

---

## How To Use

 


Load translation for dynamically created forms

```pascal
procedure TDynamicForm.Initialize;
begin
  // Call LoadTranslation() for forms created after app initialization
  Translator.LoadTranslation(DynamicForm);
end;
```

### 3. Save all live forms to INI file

```pascal
// Save all live forms to INI file
Translator.SaveTranslation('c:\App\Lang\English.ini');
```

Then:
- Translate manually, or use the built-in editor, or use DeepL API
- Place the translated file in the `Lang` folder
- The program discovers new files automatically (no restart needed)

### 4. Let users choose/switch language

```pascal
// Show language selector dialog
TfrmTranslSelector.ShowSelector;

// Or set language directly
Translator.CurLanguage := 'c:\App\Lang\German.ini';
```

### 5. Responding to language changes

```pascal
procedure TMainForm.FormPostInitialize;
begin
  Translator.OnTranslationLoaded := TranslationLoaded;
end;

procedure TMainForm.TranslationLoaded(Sender: TObject);
begin
  lblCurLang.Caption := 'Language: ' + Translator.CurLanguageName;
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

When a new version adds strings, use a diff tool (like BeyondCompare) to compare your translation with the updated English.ini. Only translate the new entries

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
- Forms that should remain untranslated

**For translators:** If you see dynamic values in the INI file (like resolution numbers, file paths), delete the entire line. The program will fill these at runtime

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

### Cost Information (January 2026)

**DeepL API Free:**
- Cost: Free
- Limit: 500,000 characters/month
- Endpoint: `https://api-free.deepl.com`

**DeepL API Pro:**
- Base fee: $5.49/month + $25 per 1,000,000 characters
- Unlimited characters
- Endpoint: `https://api.deepl.com`


**Typical usage:** A translation INI file is ~500-2000 characters. Free tier allows ~250-1000 file translations per month.

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

3. **INI file limits:**
   - ~64KB per file (relevant for very large applications)
   - ~8KB per section (per form)

---

## Important Notes

- Translation files are stored in `AppFolder\Lang\`
- Path returned by `Translator.GetLangFolder()`
- If no translation found for a control, text remains unchanged
- Make GUI controls large enough - some languages need more space

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
See Copyright.txt
