UNIT LightVcl.Common.PopUp;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================
   TPopupUpMenu - Popup Menu That Appears Above the Cursor

   This component extends TPopupMenu to display the menu ABOVE the click point
   instead of the standard behavior (below the cursor). This is useful for
   toolbars at the bottom of the screen or contexts where upward menus are preferred.

   The key difference from TPopupMenu is the use of TPM_BOTTOMALIGN flag which
   causes Windows to position the menu so its bottom edge aligns with the Y coordinate.

   Example usage:
     procedure TForm1.Button1MouseDown(Sender: TObject; Button: TMouseButton;
       Shift: TShiftState; X, Y: Integer);
     var
       P: TPoint;
     begin
       GetCursorPos(P);
       PopupUpMenu1.Popup(P.X, P.Y);
     end;

   See also:
     - LightVcl.Common.KeybShortcuts.pas
     - LightVcl.Common.VclUtils.pas (the Menu section)
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


{ Displays the popup menu with its bottom edge at the specified Y coordinate,
  effectively showing the menu above the cursor position.
  Note: We don't call inherited because TPopupMenu.Popup already calls TrackPopupMenu.
  Instead, we replicate the necessary initialization and call TrackPopupMenu with TPM_BOTTOMALIGN. }
procedure TPopupUpMenu.Popup(X, Y: Integer);
CONST
  Flags: array[Boolean, TPopupAlignment] of Word =
    ((TPM_LEFTALIGN, TPM_RIGHTALIGN, TPM_CENTERALIGN),
     (TPM_RIGHTALIGN, TPM_LEFTALIGN, TPM_CENTERALIGN));
  Buttons: array[TTrackButton] of Word = (TPM_RIGHTBUTTON, TPM_LEFTBUTTON);
VAR
  AFlags: Integer;
begin
  DoPopup(Self);   { Fire OnPopup event - same as inherited would do }
  PostMessage(PopupList.Window, WM_CANCELMODE, 0, 0);
  AdjustBiDiBehavior;   { Handle right-to-left alignment if needed }

  AFlags := Flags[UseRightToLeftAlignment, Alignment]
         OR Buttons[TrackButton]
         OR TPM_BOTTOMALIGN   { Key difference: align bottom of menu to Y coordinate }
         OR (Byte(MenuAnimation) shl 10);

  TrackPopupMenu(Items.Handle, AFlags, X, Y, 0 { reserved }, PopupList.Window, nil);
end;


end.
