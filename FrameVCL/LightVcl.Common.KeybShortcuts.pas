UNIT LightVcl.Common.KeybShortcuts;

{=============================================================================================================
   SYSTEM - Keyboard Shortcuts
   2023.01
   www.GabrielMoraru.com
==============================================================================================================

   A tool that lists all keyboard shortcuts assigned to all actions/menus in an application.

   A tool somehow similar to mine:
       https://scotthollows.com/2016/10/12/list-of-delphi-controls-on-a-form-hierarchical-and-flat-list-vcl/
   Discussion about the gory details of TActionList:
       https://stackoverflow.com/questions/1852976/how-can-i-prevent-shortcuts-from-colliding-interacting-in-delphi

   In this group:
     * LightVcl.Common.Shell.pas
     * csSystem.pas
     * csWindow.pas
     * LightVcl.Common.WindowMetrics.pas
     * LightVcl.Common.ExecuteProc.pas
     * LightVcl.Common.ExecuteShell.pas

   Tester:
       c:\MyProjects\Project Testers\Shortcut lister\Tester.dpr

=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Forms, vcl.StdCtrls, vcl.Menus, Vcl.ActnList,
  LightCore;

 procedure ShowConflicts;

 function  ShiftState2Modifier(CONST Shift: TShiftState): Word;

 function  GetShortCutKey     (ShortCut: TShortCut):Word;
 function  GetShortCutModifier(ShortCut: TShortCut):Word;

 function  RegisterHotShortCut(CONST hHandle: THandle; CONST Atom: integer; CONST ShortCut: TShortCut):Boolean;
 procedure ShowShortcutsInButtons(aForm: TForm);   // Show keyboard shortcuts for all buttons in a form that have an action assigned to them



IMPLEMENTATION
USES LightCore.AppData;



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


{ Show keyboard shortcuts for all buttons in a form that have an action assigned to them }
procedure ShowShortcutsInButtons(aForm: TForm);
VAR
   i: Integer;
   comp: TComponent;
   act: TAction;
begin
 if aForm = NIL
 then raise Exception.Create('ShowShortcutsInButtons: aForm parameter cannot be nil');

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
 if Form = NIL
 then raise Exception.Create('ShowConflictsIn: Form parameter cannot be nil');

 AppDataCore.LogMsg('Form: '+ Form.Name);

 { Note: Components[] yields only components OWNED by the form.
   Components added dynamically without ownership or owned by frames will be missed. }
 for i:= 0 to Form.ComponentCount-1 DO
   begin
    Component:= Form.Components[i];

    { List actions }
    if Component is TAction then
      begin
        Action:= TAction(Component);
        AppDataCore.LogInfo(' '+ Component.Name+ Tab
                      + IntToStr(Action.ShortCut)+ Tab
                      + Vcl.Menus.ShortCutToText(Action.ShortCut)+ Tab
                      + Form.Name+ Tab
                      + Action.Caption); { List all actions including those without shortcuts }
      end
    else
      { List menus }
      if Component is TMenuItem then
        begin
          Menu:= TMenuItem(Component);

          { Check if menu has an associated action that is a TAction }
          if (Menu.Action <> NIL) AND (Menu.Action is TAction) then
            begin
              Action:= TAction(Menu.Action);
              { Report if menu shortcut differs from action shortcut }
              if Menu.ShortCut <> Action.ShortCut
              then AppDataCore.LogWarn(' Shortcut for menu '+ Menu.Name+ ' is different than shortcut for its associated action! '+ IntToStr(Action.ShortCut));
            end
          else
            begin
              { Menu has no action - list it if it has a shortcut }
              if Menu.ShortCut <> 0
              then AppDataCore.LogInfo(' '+ Menu.Name+ Tab+ IntToStr(Menu.ShortCut));
            end;
        end;
   end;

 AppDataCore.LogEmptyRow;
 AppDataCore.LogEmptyRow;
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
