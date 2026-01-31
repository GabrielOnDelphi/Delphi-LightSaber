unit LightFmx.Common.Helpers;

{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   FMX helper functions for form traversal and clipboard operations.
=============================================================================================================}

interface

uses
  System.Classes,
  FMX.Types, FMX.Forms, FMX.Controls, FMX.Platform;

function GetParentForm(Control: TControl; TopForm: Boolean = True): TCommonCustomForm;
function FindImmediateParentForm(Obj: TFmxObject): TCommonCustomForm;

function CopyToClipboard(CONST s: string): Boolean;



IMPLEMENTATION

USES
  LightFmx.Common.Dialogs;


{ Traverses up the parent hierarchy to find the closest TCommonCustomForm.
  Returns nil if Obj is nil or has no parent form. }
function FindImmediateParentForm(Obj: TFmxObject): TCommonCustomForm;
begin
  while (Obj <> nil)
  AND NOT (Obj is TCommonCustomForm)
    DO Obj:= Obj.Parent;

  if Obj is TCommonCustomForm
  then Result := TCommonCustomForm(Obj)
  else Result := nil;
end;


{ Returns the parent form of a control.
  If TopForm is True, returns the topmost form in the hierarchy.
  In design mode, TopForm is automatically set to False to return the immediate parent. }
function GetParentForm(Control: TControl; TopForm: Boolean = True): TCommonCustomForm;
var
  Form: TCommonCustomForm;
begin
  Assert(Control <> NIL, 'GetParentForm: Control cannot be nil');

  // In design mode, override "TopForm" to return the immediate parent form
  if csDesigning in Control.ComponentState
  then TopForm:= False;
  
  // Find the immediate parent form
  Form := FindImmediateParentForm(Control);

  // If TopForm is True, traverse up to the topmost form
  if (Form <> NIL) and TopForm then
  begin
    while (Form.Parent <> nil) and (Form.Parent is TCommonCustomForm) do
      Form := TCommonCustomForm(Form.Parent);
  end;
  
  Result := Form;
end;


{ Copies text to clipboard using FMX platform services.
  Returns True if successful, False if clipboard service is unavailable.
  Shows an error message to the user if the service is not available. }
function CopyToClipboard(CONST s: string): Boolean;
VAR
   ClipboardService: IFMXClipboardService;
begin
  Result:= TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipboardService);
  if Result
  then ClipboardService.SetClipboard(s)
  else messageError('Clipboard service not available.');
end;


end.
