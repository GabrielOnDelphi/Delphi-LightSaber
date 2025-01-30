UNIT csKbShortcuts;

{=============================================================================================================
   SYSTEM - Keyboard Shortcuts
   2023.01
   See Copyright.txt
==============================================================================================================

   A tool that lists all keyboard shortcuts assigned to all actions/menus in an application.

   A tool somehow simmilar to mine:
       https://scotthollows.com/2016/10/12/list-of-delphi-controls-on-a-form-hierarchical-and-flat-list-vcl/
   Discussion about the gory details of TActionList:
       https://stackoverflow.com/questions/1852976/how-can-i-prevent-shortcuts-from-colliding-interacting-in-delphi

   In this group:
     * csShell.pas
     * csSystem.pas
     * csWindow.pas
     * csWindowMetrics.pas
     * csExecuteProc.pas
     * csExecuteShell.pas

   Tester:
       c:\MyProjects\Project Testers\Shortcut lister\Tester.dpr

=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Forms, cbAppDataForm,vcl.StdCtrls, vcl.Menus, Vcl.ActnList,
  ccCore;

 procedure ShowConflicts;

 function  ShiftState2Modifier(CONST Shift: TShiftState): Word;

 function  GetShortCutKey     (ShortCut: TShortCut):Word;
 function  GetShortCutModifier(ShortCut: TShortCut):Word;

 function  RegisterHotShortCut(CONST hHandle: THandle; CONST Atom: integer; CONST ShortCut: TShortCut):Boolean;
 procedure ShowShortcutsInButtons(aForm: TForm);   // Show keyboard shortcuts for all buttons in a form that have an action assigned to them



IMPLEMENTATION
USES cbAppData;



{--------------------------------------------------------------------------------------------------
   SYSTEM WIDE SHORTCUTS
   For example about hot to use it, see BioniX.Form_Settings
--------------------------------------------------------------------------------------------------}
function ShiftState2Modifier(CONST Shift: TShiftState): Word;
begin
  Result := 0;
  if ssShift in Shift then Result:= Result or MOD_SHIFT;
  if ssAlt   in Shift then Result:= Result or MOD_ALT;
  if ssCtrl  in Shift then Result:= Result or MOD_CONTROL;
end;


function GetShortCutKey(ShortCut: TShortCut): Word;
VAR shift: TShiftState;
begin
  Vcl.Menus.ShortCutToKey(ShortCut,Result,shift);
end;


function GetShortCutModifier(ShortCut: TShortCut): Word;
VAR key: Word;
    shift: TShiftState;
begin
  Vcl.Menus.ShortCutToKey(ShortCut,key,shift);
  Result:= ShiftState2Modifier(shift);
end;


function RegisterHotShortCut(CONST hHandle: THandle; CONST Atom: integer; CONST ShortCut: TShortCut): Boolean;
VAR key : Word;
    Shift: TShiftState;
begin
  WinApi.Windows.UnregisterHotKey(hHandle, Atom);
  ShortCutToKey(ShortCut, key, shift);
  Result:= RegisterHotKey(hHandle, Atom, ShiftState2Modifier(Shift), key);
end;


// Show keyboard shortcuts for all buttons in a form that have an action assigned to them
procedure ShowShortcutsInButtons(aForm: TForm);
VAR
   i: Integer;
   comp: TComponent;
   act: TAction;
begin
 for i:= 0 to aForm.ComponentCount-1 DO
  begin
   comp:= aForm.Components[i];
   if comp is TButton
   AND (TButton(comp).Action <> NIL) then
     begin
      act:= TButton(comp).Action as TAction;
      if act.Shortcut > 0
      then TButton(comp).Caption:= TButton(comp).Caption+ #13#10 +'('+ VCL.Menus.ShortCutToText(act.Shortcut)+ ')';
     end;
  end;
end;






procedure ShowConflictsIn(Form: TForm);
VAR
  i: Integer;
  Component: TComponent;
  Menu: TMenuItem;
  Action: TAction;
begin
 AppData.LogMsg('Form: '+ Form.Name);
 for i:= 0 to Form.ComponentCount-1 DO   {Note: Iterating over Components[]: that just yields the components that are OWNED by the form. You will miss any components that are added dynamically, and not owned by the form, or components that are owned by frames }
   begin
    { List actions }
    Component:= Form.Components[i];
    if Component is TAction
    then
     begin
      Action:= TAction(Component);
      AppData.LogInfo(' '+ Component.Name+ Tab
                    + IntToStr(Action.ShortCut)+ Tab
                    + Vcl.Menus.ShortCutToText(Action.ShortCut)+ Tab
                    + Form.Name+ Tab
                    + Action.Caption); { List all actions including those that have no shortcuts so we can see them and assign a shortcut to them }
     end
    else
      { List menus }
      if (Component is TMenuItem) then
       begin
        Menu:= TMenuItem(Component);
        Action:= Menu.Action as TAction;

        { We only list menu items that have no action assigned to them }
        if Action = NIL
        then
          begin
            if Menu.ShortCut <> 0
            then AppData.LogInfo(' '+ Menu.Name+ Tab+ IntToStr(Menu.ShortCut));
          end
        else
          { We only list menus that have a different shortcut than their associated action }
          if Menu.ShortCut <> Action.ShortCut
          then AppData.LogWarn(' Shortcut for menu '+ Menu.Name+ ' is different than shortcut for its associated action! '+ IntToStr(Action.ShortCut))
       end;
   end;

 AppData.LogEmptyRow;
 AppData.LogEmptyRow;
end;


procedure ShowConflicts;
VAR
  i: Integer;
  Form: TForm;
begin
  for i:= 0 to Screen.FormCount-1 do
  begin
    Form:= Screen.Forms[I];
    ShowConflictsIn(Form);
   end;
end;



end.
