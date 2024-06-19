UNIT cmWinPlatform;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

   Functions that are available only for Windows

=============================================================================================================}

INTERFACE

USES
   System.SysUtils, Generics.Collections, Vcl.Themes, Vcl.Forms, Vcl.Dialogs;



IMPLEMENTATION

USES ccCore, cbDialogs;

{============================================================================================================
  There is a problem with TTaskDialog. It doesn't break long strings! So it is not good for showing long strings/debugging.

  When you have to display longer strings, MessageBox is much better than TTaskDialog because TTaskDialog will wrap your strings too soon (its width tends to be much smaller than MessageBox's width).     So, don't use TTaskDialog for 'normal' messages!
  But you can use TTaskDialog.cxWidth to customize the size of the dialog -> https://stackoverflow.com/questions/33302622/how-can-i-make-the-showmessage-dialog-wider-so-it-fits-the-text

  Documentation about TTaskDialog: http://stackoverflow.com/questions/4979556/how-to-use-the-ttaskdialog
============================================================================================================}
procedure MesajTaskDLG(CONST MessageText, Title: string);
VAR
   s: string;
   Dlg: TTaskDialog;
begin
 if MessageText= '' then EXIT;

 if (Win32MajorVersion >= 6)
 AND UseLatestCommonDialogs
 AND StyleServices.Enabled
 then
  begin
   Dlg:= TTaskDialog.Create(Application);
   TRY
     if Title= ''
     then s := Application.Title
     else s := Title;
     Dlg.Caption:= s;
     Dlg.Title  := s;
     Dlg.Text   := CRLFToEnter(MessageText);
     Dlg.CommonButtons := [tcbOk];
     Dlg.Execute;
   FINALLY
     FreeAndNil(Dlg);
   END
  end
 else
   MesajInfo(MessageText, Title);
end;


end.
