UNIT FormAsyncMessage;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
  A non-blocking message box.
  No dependences.
=============================================================================================================}

INTERFACE
{.$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  System.Classes, Vcl.Forms, cbAppDataForm,Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Controls;

type
  TfrmShowMsgAsync = class(TLightForm)
    lblMessage: TLabel;
    Panel1: TPanel;
    btnOK: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
  end;

procedure MesajAsync     (CONST Msg: string; CONST Caption: string= ''; PopupParent: TCustomForm= NIL);
procedure MesajInfoAsync (CONST Msg: string; PopupParent: TCustomForm= NIL);
procedure MesajWarnAsync (CONST Msg: string; PopupParent: TCustomForm= NIL);
procedure MesajErrorAsync(CONST Msg: string; PopupParent: TCustomForm= NIL);


IMPLEMENTATION {$R *.dfm}


procedure MesajAsync(CONST Msg: string; CONST Caption: string= ''; PopupParent: TCustomForm= NIL);
VAR frm: TfrmShowMsgAsync;
begin
 frm:= TfrmShowMsgAsync.Create(Application.MainForm);
 frm.Caption:= Caption;
 frm.lblMessage.Caption:= Msg;
 frm.BorderStyle:= bsDialog;
 frm.PopupParent:= PopupParent;
 frm.Show;
end;


procedure MesajInfoAsync(CONST Msg: string; PopupParent: TCustomForm= NIL);
begin
 MesajAsync(Msg, 'Info', PopupParent);
end;

procedure MesajWarnAsync(CONST Msg: string; PopupParent: TCustomForm= NIL);
begin
 MesajAsync(Msg, 'Warning', PopupParent);
end;

procedure MesajErrorAsync(CONST Msg: string; PopupParent: TCustomForm= NIL);
begin
 MesajAsync(Msg, 'Error', PopupParent);
end;





procedure TfrmShowMsgAsync.btnOKClick(Sender: TObject);
begin
 Close;
end;

procedure TfrmShowMsgAsync.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 // WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!
 Action:= caFree;
 {Action:= caFree; Delphi bug: https://quality.embarcadero.com/browse/RSP-33140 }
end;

end.
