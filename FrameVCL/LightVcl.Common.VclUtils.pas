UNIT LightVcl.Common.VclUtils;

{=============================================================================================================
   2025.03
   www.GabrielMoraru.com
==============================================================================================================
   Utility functions that we can apply to VCL components

   See also:
      csWindow.pas
      LightVcl.Common.VclUtils.pas

   Tester:
      c:\MyProjects\My books\Building cross-platform applications\Demo projects\Show VCL inheritance tree\
=============================================================================================================}

INTERFACE
{ $I Frameworks.inc}
USES
   Winapi.Windows, System.TypInfo, System.Classes, System.SysUtils,
   Vcl.StdCtrls, Vcl.Menus, Vcl.ActnList, Vcl.ComCtrls, Vcl.Controls, Vcl.Forms;


TYPE
  { Interposer: Control that exposes the Canvas property.
    It can be used to access TPanel's canvas property for example }
  TCustomControlEx= class(TCustomControl)
   public
      property Canvas;
   end;


{=============================================================================================================
   MENUS & ACTIONS
=============================================================================================================}
 procedure MenuVisibility    (Item: TMenuItem;       Enabled, Visible: Boolean);
 procedure SetChildVisibility(ParentMenu: TMenuItem; Enabled, Visible: Boolean); overload; { Change the visibility for all children of ParentMenu }
 procedure SetChildVisibility(ParentMenu: TMenuItem; Visible: Boolean);          overload;

 function  AddSubMenu        (ParentMenu: TMenuItem; Caption: string; Event: TNotifyEvent): TMenuItem;  { Add a sub-menu item to a menu item. Also returns a pointer to that menu. I don't have to free it. The owner will free it. }
 procedure RemoveSubmenus    (ParentMenu: TMenuItem);

 { Actions }
 function  HasAction         (Component: TComponent): Boolean;                   { Returns true if this component (TMenuItem, TButton), etc has an action assigned to it. }
 procedure ActionVisibility  (Item: TAction; Show: Boolean);


{=============================================================================================================
   FORM
=============================================================================================================}
 procedure AlignCaptionToLeft(Handle: HWND);                                     { Align caption of the specified control to left. Example of usage: AlignCaptionToLeft(Button1.Handle)) }
 procedure ScrollAppTitle    (DirectionLeft: Boolean);                           { use it in a timer set it at 250ms }
 procedure ScrollFormCaption (Form: TForm);                                      { use it in a timer set it at 250ms }


{=============================================================================================================
   TControl
=============================================================================================================}
 procedure BlinkControl      (Control: TControl);                                { Makes the specified control to blink 5 times, to attract user's attention }
 function  CreateControl     (ControlClass: TControlClass; const ControlName: string; Parent: TWinControl; X, Y, W, H: Integer): TControl;
 procedure DoubleBuffer      (Control: TComponent; Enable: Boolean);             { Activate/deactivate double buffering for all controls owned by the specified control. aControl can be a form, panel, box, etc }
 procedure EnableDisable     (Control: TWinControl; Enable: Boolean);            { Enable/disable all controls in the specified control }
 {}
 function  FindControlAtPos  (ScreenPos: TPoint): TControl;
 function  FindSubcontrolAtPos(Control: TControl; ScreenPos, AClientPos: TPoint): TControl;
 procedure PushControlDown   (BottomCtrl, TopControl: TControl);                 { Makes sure that BottomCtrl is under the TopControl control. Useful to set splitters under their conected controls }    { old name: SetCtrlUnder }
 { Focus }
 function  CanFocus          (Control: TWinControl): Boolean;
 procedure SetFocus          (Control: TWinControl);

 procedure RefreshNow(Ctrl: TControl);


{=============================================================================================================
   DESIGN TIME DEBUGGING
=============================================================================================================}
 function ShowComponentState (Component: TComponent): string;
 function ShowControlState   (Control: TControl): string;
 function ShowInheritanceTree(Control: TControl): string;

 {}
 function  SetActivePage     (PageControl: TPageControl; CONST PageName: string): TTabSheet;   { Set the active tab for the specified PageControl, but instead of using an index we use a string }
 procedure ToggleCheckbox    (CheckBox: TCheckBox; BasedOn: TButtonControl);     { Disable and uncheck CheckBox if BasedOn is checked }


IMPLEMENTATION
USES
   LightVcl.Common.Dialogs;



{-------------------------------------------------------------------------------------------------------------
  Returns the control at the specified position (can be used with Mouse.CursorPos)

  This is a better alternative to FindVCLWindow and ControlAtPos because:
    FindVCLWindow doesn't search disabled controls (Enabled=False).
    TWinControl.ControlAtPos doesn't search controls if they are disabled indirectly (for example if Button.Enabled=True, but Button.Parent.Enabled=False).
  Source:
    https://stackoverflow.com/questions/6719620/how-do-i-get-the-control-that-is-under-the-cursor-in-delphi
-------------------------------------------------------------------------------------------------------------}
function FindControlAtPos(ScreenPos: TPoint): TControl;  { Recursive }
VAR
  i: Integer;
  f, m: TForm;
  p: TPoint;
  r: TRect;
begin
  Result := NIL;
  for i := Screen.FormCount-1 downto 0 DO
    begin
      f := Screen.Forms[i];
      if f.Visible
      AND (f.Parent= NIL)
      AND (f.FormStyle <> fsMDIChild)
      AND TRect.Create(f.Left, f.Top, f.Left+f.Width, f.Top+f.Height).Contains(ScreenPos)
      then Result := f;
    end;
  Result := FindSubcontrolAtPos(Result, ScreenPos, ScreenPos);

  if (Result is TForm)
  AND (TForm(Result).ClientHandle<>0) then
   begin
    WinAPI.Windows.GetWindowRect(TForm(Result).ClientHandle, r);
    p := TPoint.Create(ScreenPos.X-r.Left, ScreenPos.Y-r.Top);
    m := NIL;
    for i := TForm(Result).MDIChildCount-1 downto 0 do
     begin
      f := TForm(Result).MDIChildren[i];
      if TRect.Create(f.Left, f.Top, f.Left+f.Width, f.Top+f.Height).Contains(p)
      then m := f;
     end;

    if m <> NIL
    then Result := FindSubcontrolAtPos(m, ScreenPos, p);
   end;
end;


function FindSubcontrolAtPos(Control: TControl; ScreenPos, AClientPos: TPoint): TControl;    { Recursive }
VAR
  i: Integer;
  C: TControl;
begin
  Result := NIL;
  C := Control;
  if (C= nil) OR NOT C.Visible
  OR NOT TRect.Create(C.Left, C.Top, C.Left+C.Width, C.Top+C.Height).Contains(AClientPos) then Exit;

  Result := Control;
  if Control is TWinControl then
    for i := 0 to TWinControl(Control).ControlCount-1 do
    begin
      C := FindSubcontrolAtPos(TWinControl(Control).Controls[i], ScreenPos, Control.ScreenToClient(ScreenPos));
      if C <> NIL
      then Result := C;
    end;
end;








{-------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------}

{ Shows the whole inheritance tree, down to TObject, for the specified control }
function ShowInheritanceTree(Control: TControl): string;
var
  ClassRef: TClass;
begin
  Result:= '';
  ClassRef := Control.ClassType;

  WHILE ClassRef <> NIL do
   begin
    Result:= Result+ ' '+ ClassRef.ClassName + #13#10;
    ClassRef := ClassRef.ClassParent;
   end;
end;


function ShowControlState(Control: TControl): string;
begin
 Result:= 'ControlState: ';
 if csLButtonDown      in Control.ControlState then Result:= Result+ 'csLButtonDown, ';
 if csClicked          in Control.ControlState then Result:= Result+ 'csClicked, ';
 if csPalette          in Control.ControlState then Result:= Result+ 'csPalette, ';
 if csReadingState     in Control.ControlState then Result:= Result+ 'csReadingState, ';
 if csAlignmentNeeded  in Control.ControlState then Result:= Result+ 'csAlignmentNeeded, ';
 if csFocusing         in Control.ControlState then Result:= Result+ 'csFocusing, ';
 if csCreating         in Control.ControlState then Result:= Result+ 'csCreating, ';
 if csPaintCopy        in Control.ControlState then Result:= Result+ 'csPaintCopy, ';
 if csCustomPaint      in Control.ControlState then Result:= Result+ 'csCustomPaint, ';
 if csDestroyingHandle in Control.ControlState then Result:= Result+ 'csDestroyingHandle, ';
 if csDocking          in Control.ControlState then Result:= Result+ 'csDocking, ';
 if csDesignerHide     in Control.ControlState then Result:= Result+ 'csDesignerHide, ';
 if csPanning          in Control.ControlState then Result:= Result+ 'csPanning, ';
 if csRecreating       in Control.ControlState then Result:= Result+ 'csRecreating, ';
 if csAligning         in Control.ControlState then Result:= Result+ 'csAligning, ';
 if csGlassPaint       in Control.ControlState then Result:= Result+ 'csGlassPaint, ';
 if csPrintClient      in Control.ControlState then Result:= Result+ 'csPrintClient, ';
end;


function ShowComponentState(Component: TComponent): string;
begin
 Result:= 'ComponentState: ';
 if csLoading          in Component.ComponentState then Result:= Result+ 'csLoading, ';
 if csReading          in Component.ComponentState then Result:= Result+ 'csReading, ';
 if csWriting          in Component.ComponentState then Result:= Result+ 'csWriting, ';
 if csDesigning        in Component.ComponentState then Result:= Result+ 'csDesigning, ';
 if csAncestor         in Component.ComponentState then Result:= Result+ 'csAncestor, ';
 if csUpdating         in Component.ComponentState then Result:= Result+ 'csUpdating, ';
 if csFixups           in Component.ComponentState then Result:= Result+ 'csFixups, ';
 if csFreeNotification in Component.ComponentState then Result:= Result+ 'csFreeNotification, ';
 if csInline           in Component.ComponentState then Result:= Result+ 'csInline, ';
 if csDesignInstance   in Component.ComponentState then Result:= Result+ 'csDesignInstance, ';
end;


{ Makes the specified control to blink 5 times, to attract user's attention }
procedure BlinkControl(Control: TControl);
begin
 //Application.ProcessMessages;    // https://blog.dummzeuch.de/2018/09/29/calling-application-processmessages-in-a-delphi-program/
 Control.Visible:= FALSE;
 Control.Update;
 Sleep(100);
 Control.Visible:= TRUE;
 Control.Update;
 Sleep(100);
 Control.Visible:= FALSE;
 Control.Update;
 Sleep(100);
 Control.Visible:= TRUE;
 Sleep(100);
 Control.Visible:= FALSE;
 Control.Update;
 Sleep(100);
 Control.Visible:= TRUE;
end;


{ Align caption of the specified control to left. Example of usage: AlignCaptionToLeft(Button1.Handle)) }
procedure AlignCaptionToLeft(Handle: HWND);
VAR Style: DWORD;
begin
 Style := GetWindowLongPtr(Handle, GWL_STYLE);                { getWindowLong_ was replaced with getWindowLongPtr for 64 bit compatibility. Details: http://docwiki.embarcadero.com/RADStudio/Seattle/en/Converting_32-bit_Delphi_Applications_to_64-bit_Windows }
 SetLastError(0);
 if SetWindowLongptr(Handle, GWL_STYLE, Style OR bs_left)= 0  { setWindowLong_ was replaced with getWindowLongPtr for 64 bit compatibility. Details: http://docwiki.embarcadero.com/RADStudio/Seattle/en/Converting_32-bit_Delphi_Applications_to_64-bit_Windows }
 then MessageWarning('Cannot align caption!');
end;



{ Activate/deactivate double buffering for all controls owned by the specified control. aControl can be a form, panel, box, etc }
procedure DoubleBuffer(Control: TComponent; Enable: Boolean);   //todo 5: use this instead: http://stackoverflow.com/questions/8058745/tlabel-and-tgroupbox-captions-flicker-on-resize !!!!!!!
VAR i : integer;
begin
 {Parent}
 if Control is TWinControl
 then TWinControl(Control).DoubleBuffered:= Enable;

 {Childs}
 for i := 0 to Control.ComponentCount-1 DO
  if (Control.Components[i] is TWinControl)
  AND NOT (Control.Components[i] is Vcl.ComCtrls.TRichEdit)
  AND NOT (Control.Components[i] is THotKey)
  then TWinControl(Control.Components[i]).DoubleBuffered:= Enable;
end;





{--------------------------------------------------------------------------------------------------
   VCL
--------------------------------------------------------------------------------------------------}

{ The CanFocus VCL function is totally flawed and unreliable. Use this instead.
  https://stackoverflow.com/questions/41016976 }
function CanFocus(Control: TWinControl): Boolean;
begin
 Result:= Control.CanFocus AND Control.Enabled AND Control.Visible;
 if Result
 AND NOT Control.InheritsFrom(TForm)
 then
   { Recursive call:
     This control might be hosted by a panel which could be also invisible/disabled. So, we need to check all the parents down the road. We stop when we encounter the parent Form.
     Also see: GetParentForm }
   Result:= CanFocus(Control.Parent);
end;


procedure SetFocus(Control: TWinControl);
begin
 if CanFocus(Control)
 then Control.SetFocus;
end;


{ Enable/disable all controls in the specified control }
procedure EnableDisable(Control: TWinControl; Enable: Boolean);
VAR i: Integer;
begin
 Control.Enabled:= Enable;                               { Disable self }
 for i:= 0 to Control.ControlCount-1
  DO TWinControl(Control.Controls[i]).Enabled:= Enable;  { Disable children }
end;


{ Disable and uncheck CheckBox if BasedOn is checked. TButtonControl is the base class for a push button, check box, or radio button }
procedure ToggleCheckbox(CheckBox: TCheckBox; BasedOn: TButtonControl);
VAR Checked: Boolean;
begin
   if BasedOn is TCheckBox
   then Checked:= TCheckBox(BasedOn).Checked
   else
     if BasedOn is TRadioButton
     then Checked:= TRadioButton(BasedOn).Checked
     else RAISE Exception.Create('Unknown control in ToggleCheckbox: '+ BasedOn.Name);

 CheckBox.Enabled:= NOT Checked;
 if NOT CheckBox.Enabled                 { If disabled, aslo uncheck it }
 then CheckBox.Checked:= FALSE;
end;


{ Makes sure that BottomCtrl is under the TopControl control. Useful to set splitters under their conected controls }
procedure PushControlDown(BottomCtrl, TopControl: TControl);      { old name: SetCtrlUnder }
begin
 BottomCtrl.Top:= TopControl.Top+ TopControl.Height- 1;
end;


{ Set the active tab for the specified PageControl, but instead of using an index we use a string }
function SetActivePage(PageControl: TPageControl; CONST PageName: string): TTabSheet;
begin
 Result:= NIL;
 for VAR i:= 0 to PageControl.PageCount-1 DO
   if SameText(PageControl.Pages[i].Name, PageName)
   AND PageControl.Pages[i].TabVisible then
     begin
      PageControl.ActivePageIndex:= i;
      Result:= PageControl.Pages[i];
      Break;
     end;
end;


{ Force repainting of this control }
procedure RefreshNow(Ctrl: TControl);
begin
  Ctrl.Refresh;
end;









{--------------------------------------------------------------------------------------------------
   Caption effects
--------------------------------------------------------------------------------------------------}
procedure ScrollAppTitle(DirectionLeft: Boolean);                                        { use it in a timer set it at 250ms }
begin
 if DirectionLeft
 then Application.title:= system.COPY(Application.Title, 2, Length(Application.title)) + Application.title[1]   { left direction }
 else Application.title := Application.title[Length(Application.title)] + system.COPY(Application.title, 1, Length(Application.title) - 1);    { right direction }
end;


procedure ScrollFormCaption(Form: TForm);                                                { use it in a timer set it at 250ms }
begin
  Form.Caption:= system.COPY(Form.Caption, 2, Length(Form.Caption))+ Form.Caption[1];    //left direction
end;







{--------------------------------------------------------------------------------------------------
   ACTIONS / MENUS
   See also: LightVcl.Common.KeybShortcuts.pas
--------------------------------------------------------------------------------------------------}

{ Returns true if this component (TMenuItem, TButton), etc has an action assigned to it. }
function HasAction(Component: TComponent): Boolean;
VAR Action: TObject;
begin
  if  System.TypInfo.IsPublishedProp(Component, 'Action') then
    begin
     Action:= GetObjectProp(Component, 'Action');
     Result:= Action <> nil;
    end
  else
     Result:= FALSE;
end;


procedure ActionVisibility(Item: TAction; Show: Boolean);
begin
 TAction(Item).Enabled:= Show;                                           { Disabling the action will also disabled the menu  }
 TAction(Item).Visible:= Show;
end;


procedure MenuVisibility(Item: TMenuItem; Enabled, Visible: Boolean);
begin
 if Item.Action<> NIL                                                    { This menu has action associated? }
 then
   begin
    TAction(Item.Action).Enabled:= Enabled;                              { Disabling the action will also disable the menu  }
    TAction(Item.Action).Visible:= Visible;
   end
 else
   begin
    Item.Enabled:= Enabled;
    Item.Visible:= Visible;
   end;
end;


procedure SetChildVisibility(ParentMenu: TMenuItem; Enabled, Visible: Boolean);    { Change the visibility for all children of ParentMenu }
VAR I: Integer;

 procedure Parse(SubItem: TMenuItem);
 VAR I: Integer;
 begin
  if SubItem.Count> 0 then
   for I:= 0 TO SubItem.Count-1 DO
    begin
     MenuVisibility(SubItem.Items[I], Enabled, Visible);
     Parse(SubItem.Items[I]);
    end;
 end;

begin
 { The parent itsself }
 MenuVisibility(ParentMenu, Enabled, Visible);

 { All items in the Parent menu }
 for I:= 0 TO ParentMenu.Count-1 DO
  begin
   MenuVisibility(ParentMenu.Items[I], Enabled, Visible);
   Parse(ParentMenu.Items[I]);
  end;
end;


procedure SetChildVisibility(ParentMenu: TMenuItem; Visible: Boolean);
begin
 SetChildVisibility(ParentMenu, Visible, Visible);
end;


function AddSubMenu(ParentMenu: TMenuItem; Caption: string; Event: TNotifyEvent): TMenuItem;       { Add a sub-menu item to a menu item. Also returns a pointer to that menu. I don't have to free it. The owner will free it. }
begin
   Result := TMenuItem.Create(ParentMenu);
   TRY                                                                                             { This inserts a menu item after  }
     Result.AutoCheck  := FALSE;
     Result.AutoHotkeys:= maManual;
     Result.OnClick    := Event;
     Result.Caption    := Caption;
     ParentMenu.Add(Result);
   EXCEPT
     FreeAndNil( Result );
     RAISE;
   END;
end;


procedure RemoveSubmenus(ParentMenu: TMenuItem);                                                   { Remove all submenus of a menu }
VAR
   i: Integer;
   SubItem: TMenuItem;
begin
 for i:= ParentMenu.Count-1 downto 0 DO
  begin
    SubItem:= ParentMenu.Items[i];
    ParentMenu.Remove(SubItem);
    FreeAndNil(SubItem);
  end;
end;




{-------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------}
 // Example: CreateControl(TEdit, 'Edit1', 10, 10, 100, 20);
 function CreateControl(ControlClass: TControlClass; const ControlName: string; Parent: TWinControl; X, Y, W, H: Integer): TControl;
 begin
   Result := ControlClass.Create(Parent);
   Result.Parent := Parent;
   Result.Name   := ControlName;
   Result.SetBounds(X, Y, W, H);
   Result.Visible:= True;
 end;



end.
