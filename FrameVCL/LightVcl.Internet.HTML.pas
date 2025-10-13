UNIT LightVcl.Internet.HTML;

//This hould be moved to core

{-------------------------------------------------------------------------------------------------------------
   2023.06
   www.GabrielMoraru.com
   See Copyright file

   HTML Parsing

   Also see: c:\MyProjects\Packages\Third party packages\uHTMLBuilder.pas
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
   MsHtml,
   System.SysUtils, System.AnsiStrings, System.StrUtils, System.Classes, System.Math,
   LightCore.StringList, LightCore, LightCore.Time;

{--------------------------------------------------------------------------------------------------
   HTML MANIPULATION
   Forms & fields
--------------------------------------------------------------------------------------------------}
 procedure SetFieldValue   (aForm: IHTMLFormElement; const fieldName: string; const newValue: string; CONST Instance: integer=0);
 function  GetFormByNumber (Document: IHTMLDocument2; formNumber: integer): IHTMLFormElement;


IMPLEMENTATION

USES
  LightCore.IO, LightCore.Internet, LightCore.TextFile;


procedure SetFieldValue(aForm: IHTMLFormElement; const fieldName: string; const newValue: string; const instance: integer=0);
{ Example how to use it:  BML auto login\ }
VAR
  field: IHTMLElement;
  inputField: IHTMLInputElement;
  selectField: IHTMLSelectElement;
  textField: IHTMLTextAreaElement;
begin
  field := aForm.Item(fieldName,instance) as IHTMLElement;
  if Assigned(field) then
   begin
    if field.tagName = 'INPUT' then
     begin
       inputField := field as IHTMLInputElement;
       if (inputField.type_ <> 'radio')
       AND (inputField.type_ <> 'checkbox')
       then inputField.value := newValue
       else inputField.checked := (newValue = 'checked');
     end
    else
     if field.tagName = 'SELECT' then
      begin
       selectField := field as IHTMLSelectElement;
       selectField.value := newValue;
      end
    else
     if field.tagName = 'TEXTAREA' then
      begin
       textField := field as IHTMLTextAreaElement;
       textField.value := newValue;
      end;
   end;
end;



function GetFormByNumber(document: IHTMLDocument2; formNumber: integer): IHTMLFormElement;         { Example how to use it: BML auto login\ }
VAR forms: IHTMLElementCollection;
begin
  forms := document.Forms as IHTMLElementCollection;
  if formNumber < forms.Length
  then result := forms.Item(formNumber,'') as IHTMLFormElement
  else result := nil;
end;


 end.
