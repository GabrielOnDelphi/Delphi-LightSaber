UNIT LightVcl.Common.PopUp;

{=============================================================================================================
   2023.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================

  Display the popup menu at coordinates above the cursor instead of the classical way (under the cursor).

  Example of usage:
   procedure TForm16.Button1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
   VAR P: TPoint;
   begin
    GetCursorPos(P);
    PopupMenu.Popup(P.X, P.Y);
   end;

  See also:
     LightVcl.Common.KeybShortcuts.pas
     LightVcl.Common.VclUtils.pas (the Menu section)

=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, Winapi.Messages, System.Classes, Vcl.Menus;

TYPE
  TPopupUpMenu = class(Vcl.Menus.TPopupMenu)
  public
    procedure Popup(X, Y: Integer); override;
  end;


IMPLEMENTATION


procedure TPopupUpMenu.Popup(X, Y: Integer);
CONST
  Flags  : array[Boolean, TPopupAlignment] of Word = ((TPM_LEFTALIGN,  TPM_RIGHTALIGN, TPM_CENTERALIGN), (TPM_RIGHTALIGN, TPM_LEFTALIGN, TPM_CENTERALIGN));
  Buttons: array[TTrackButton] of Word = (TPM_RIGHTBUTTON, TPM_LEFTBUTTON);
VAR
  AFlags: Integer;
begin
  PostMessage(PopupList.Window, WM_CANCELMODE, 0, 0);
  inherited;
  AFlags := Flags[UseRightToLeftAlignment, Alignment]
         OR Buttons[TrackButton]
         OR TPM_BOTTOMALIGN
         OR (Byte(MenuAnimation) shl 10);

  TrackPopupMenu(Items.Handle, AFlags, X, Y, 0 { reserved }, PopupList.Window, nil);
end;


end.
