UNIT FormAsyncMessage;

{=============================================================================================================
   2026.01.29
   www.GabrielMoraru.com

--------------------------------------------------------------------------------------------------------------
   A non-blocking (modeless) message box.

   Unlike standard ShowMessage/MessageDlg which block the calling thread until dismissed,
   these async message boxes allow the application to continue running while displayed.
   Useful for background tasks that need to notify the user without interrupting workflow.

   USAGE:
     MessageInfoAsync('Operation completed successfully');
     MessageErrorAsync('Failed to save file', MainForm);
     MessageWarnAsync('Low disk space warning');

   The form auto-frees when closed (caFree).
   No external dependencies - uses only standard VCL.
=============================================================================================================}

INTERFACE
{.$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  System.Classes,
  Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Controls;

type
  TfrmShowMsgAsync = class(TForm)
    lblMessage: TLabel;
    Panel1: TPanel;
    btnOK: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  end;

{ Core async message function - creates and shows a modeless message dialog }
procedure MesajAsync       (CONST Msg: string; CONST Caption: string= ''; PopupParent: TCustomForm= NIL);
procedure MessageAsync     (CONST Msg: string; CONST Caption: string= ''; PopupParent: TCustomForm= NIL);  // English alias

{ Convenience functions with predefined captions }
procedure MessageInfoAsync (CONST Msg: string; PopupParent: TCustomForm= NIL);
procedure MessageWarnAsync (CONST Msg: string; PopupParent: TCustomForm= NIL);
procedure MesajWarnAsync   (CONST Msg: string; PopupParent: TCustomForm= NIL);   // Legacy alias
procedure MessageErrorAsync(CONST Msg: string; PopupParent: TCustomForm= NIL);


IMPLEMENTATION {$R *.dfm}


{ Creates and displays a non-blocking message dialog.
  Parameters:
    Msg         - The message text to display
    Caption     - Window title (defaults to empty)
    PopupParent - Parent form for proper Z-ordering (optional)
  Note: The form auto-frees when closed via caFree in FormClose. }
procedure MesajAsync(CONST Msg: string; CONST Caption: string= ''; PopupParent: TCustomForm= NIL);
var
  frm: TfrmShowMsgAsync;
  Owner: TComponent;
begin
  // Determine appropriate owner - use MainForm if available, otherwise Application
  if Application.MainForm <> NIL
  then Owner:= Application.MainForm
  else Owner:= Application;

  frm:= TfrmShowMsgAsync.Create(Owner);
  frm.Caption:= Caption;
  frm.lblMessage.Caption:= Msg;
  frm.BorderStyle:= bsDialog;
  frm.KeyPreview:= TRUE;  // Enable keyboard shortcuts

  // Set PopupParent for proper window stacking
  if PopupParent <> NIL
  then frm.PopupParent:= PopupParent
  else
    if Application.MainForm <> NIL
    then frm.PopupParent:= Application.MainForm;

  frm.Show;
end;


{ English alias for MesajAsync }
procedure MessageAsync(CONST Msg: string; CONST Caption: string= ''; PopupParent: TCustomForm= NIL);
begin
  MesajAsync(Msg, Caption, PopupParent);
end;


{ Shows an informational message with "Info" caption }
procedure MessageInfoAsync(CONST Msg: string; PopupParent: TCustomForm= NIL);
begin
  MesajAsync(Msg, 'Info', PopupParent);
end;


{ Shows a warning message with "Warning" caption }
procedure MessageWarnAsync(CONST Msg: string; PopupParent: TCustomForm= NIL);
begin
  MesajAsync(Msg, 'Warning', PopupParent);
end;


{ Legacy alias for MessageWarnAsync }
procedure MesajWarnAsync(CONST Msg: string; PopupParent: TCustomForm= NIL);
begin
  MessageWarnAsync(Msg, PopupParent);
end;


{ Shows an error message with "Error" caption }
procedure MessageErrorAsync(CONST Msg: string; PopupParent: TCustomForm= NIL);
begin
  MesajAsync(Msg, 'Error', PopupParent);
end;





{ Closes the dialog when OK button is clicked }
procedure TfrmShowMsgAsync.btnOKClick(Sender: TObject);
begin
  Close;
end;


{ Allows closing the dialog with Enter or Escape keys }
procedure TfrmShowMsgAsync.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then Close;  // Enter
  if Key = #27 then Close;  // Escape
end;


{ Auto-free the form when closed.
  Note: Related Delphi bug: https://quality.embarcadero.com/browse/RSP-33140 }
procedure TfrmShowMsgAsync.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
end;


end.
