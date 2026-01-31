UNIT LightVcl.Internet.HTML;

// This should be moved to core

{-------------------------------------------------------------------------------------------------------------
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt

   HTML Parsing

   Also see: c:\MyProjects\Packages\Third party packages\uHTMLBuilder.pas
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
   MsHtml,
   System.SysUtils;

{--------------------------------------------------------------------------------------------------
   HTML MANIPULATION
   Forms & fields
--------------------------------------------------------------------------------------------------}
 procedure SetFieldValue   (aForm: IHTMLFormElement; const fieldName: string; const newValue: string; const Instance: integer= 0);
 function  GetFormByNumber (Document: IHTMLDocument2; formNumber: integer): IHTMLFormElement;


IMPLEMENTATION


procedure SetFieldValue(aForm: IHTMLFormElement; const fieldName: string; const newValue: string; const instance: integer=0);
{ Example: BML auto login }
VAR
  field: IHTMLElement;
  inputField: IHTMLInputElement;
  selectField: IHTMLSelectElement;
  textField: IHTMLTextAreaElement;
begin
  Assert(aForm <> NIL, 'SetFieldValue: aForm is nil');
  field:= aForm.Item(fieldName, instance) as IHTMLElement;
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



{ Returns the form at the specified index, or nil if index is out of bounds.
  Example: BML auto login }
function GetFormByNumber(document: IHTMLDocument2; formNumber: integer): IHTMLFormElement;
VAR forms: IHTMLElementCollection;
begin
  Assert(document <> NIL, 'GetFormByNumber: document is nil');
  forms:= document.Forms as IHTMLElementCollection;
  if formNumber < forms.Length
  then Result:= forms.Item(formNumber, '') as IHTMLFormElement
  else Result:= NIL;
end;


end.
