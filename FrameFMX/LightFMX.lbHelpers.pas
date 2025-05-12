unit LightFMX.lbHelpers;

interface

uses
  System.Classes,
  FMX.Types, FMX.Forms, FMX.Controls, FMX.Platform;

function GetParentForm(Control: TControl; TopForm: Boolean = True): TCommonCustomForm;
function FindImmediateParentForm(Obj: TFmxObject): TCommonCustomForm;

function CopyToClipboard(CONST s: string): Boolean;



IMPLEMENTATION

USES
  LightFMX.lbDialogs;


function FindImmediateParentForm(Obj: TFmxObject): TCommonCustomForm;
begin
  while (Obj <> nil)
  AND NOT (Obj is TCommonCustomForm)
    DO Obj := Obj.Parent;

  if Obj is TCommonCustomForm
  then Result := TCommonCustomForm(Obj)
  else Result := nil;
end;


function GetParentForm(Control: TControl; TopForm: Boolean = True): TCommonCustomForm;
var
  Form: TCommonCustomForm;
begin
  // If the control is in design mode, override the "TopForm" parameter
  if csDesigning in Control.ComponentState then
    TopForm := False;
  
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


function CopyToClipboard(CONST s: string): Boolean;
VAR
   ClipboardService: IFMXClipboardService;
begin
  Result:= TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipboardService);
  if Result
  then ClipboardService.SetClipboard(s)
  else messageError('Clipboard service not available.'); // Optional user feedback
end;


end.
