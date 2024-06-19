UNIT FormSkinsRes;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Universal skin loader. Loads skins from internal resource (from exe).

   DON'T ADD IT TO ANY DPK!

   To use it:
      Application.ShowMainForm:= FALSE;   // Necessary so the form won't flicker during skin loading at startup
      MainForm.Visible:= FALSE;
      LoadLastSkin  (during application initialization)
      MainForm.Show;

      Call ShowSkinForm to show this form.

-------------------------------------------------------------------------------------------------------------
  Skins folder:
         c:\MyProjects\Packages\VCL Styles utils\Styles\
         c:\Users\Public\Documents\Embarcadero\Studio\20.0\Styles\
         c:\Users\Public\Documents\Embarcadero\Studio\21.0\Styles\

  Tester:
     c:\MyProjects\Packages\VCL Styles Tools\FrmSkins tester\

  More:
     https://subscription.packtpub.com/book/application_development/9781783559589/1/ch01lvl1sec10/changing-the-style-of-your-vcl-application-at-runtime

  KNOWN BUGS:

     XE7
       TStyleManager.IsValidStyle always fails if Vcl.Styles is not in the USES list!!
       http://stackoverflow.com/questions/30328644/how-to-check-if-a-style-file-is-already-loaded

     caFree
       procedure Tfrm.FormClose(Sender: TObject; var Action: TCloseAction);
       begin
        Action:= caFree;
        Delphi bug: Don't use caFree: https://quality.embarcadero.com/browse/RSP-33140
       end;

     Solution:
       https://stackoverflow.com/questions/70840792/how-to-patch-vcl-forms-pas
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, System.SysUtils, System.Classes,
  Vcl.Styles, Vcl.Themes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls;

TYPE
  TfrmSkinRes = class(TForm)
    lBox: TListBox;
    lblTop: TLabel;
    pnlBottom: TPanel;
    pnlBtm: TPanel;
    btnOK: TButton;
    btnSkinEditor: TButton;
    lblMoreSkinsTrial: TLabel;
    procedure FormCreate  (Sender: TObject);
    procedure FormDestroy (Sender: TObject);
    procedure lBoxClick   (Sender: TObject);
    procedure lblTopClick (Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure btnSkinEditorClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOKClick(Sender: TObject);
  private
    procedure PopulateSkins;
  public
    class procedure ShowEditor; static;
    class procedure ShowEditorNonModal; static;
 end;


procedure LoadLastSkin(CONST DefaultSkin: string= '');  { On first run, set the DefaultSkin to an existing file (no path) like: 'Graphite Green.vsf'. Leave it empty if you want the default Windows theme to load }



IMPLEMENTATION {$R *.dfm}

USES
   uLinks, csExecuteShell, cbAppData, cTranslate,
   cmINIFileQuick, ccINIFile, cvIniFile;   {VCL.Styles is mandatory}

CONST
  DefWinTheme= 'Windows default theme';

VAR
  LastSkin: string;



{-----------------------------------------------------------------------------------------------------------------------
   UTILS
-----------------------------------------------------------------------------------------------------------------------}


procedure LoadLastSkin(CONST DefaultSkin: string= '');
begin
 LastSkin:= cmINIFileQuick.ReadString('LastSkin', DefaultSkin);   { This is a relative path so the skin can still be loaded when the application is moved to a different folder }

 if LastSkin = ''
 then LastSkin:= DefaultSkin;

 if (LastSkin > '')
 AND (LastSkin <> DefWinTheme)              { DefWinTheme represents the default Windows theme/skin. In other words don't load any skin file. Let Win skin the app }
 then TStyleManager.SetStyle(LastSkin);
end;










{-----------------------------------------------------------------------------------------------------------------------
   SHOW EDITOR
-----------------------------------------------------------------------------------------------------------------------}

{ THERE IS A BUG THAT CRASHES THE PROGRAM WHEN I CLOSE THIS WINDOW (after applying a skin) }
class procedure TfrmSkinRes.ShowEditor;
begin
 VAR frmEditor:= TfrmSkinRes.Create(NIL);

 WITH frmEditor DO
 begin
   LoadForm(frmEditor, TRUE);            { Position form }
   if Translator <> NIL
   then Translator.LoadFormTranlation(frmEditor);
   Font:= Application.MainForm.Font;     { Themes }
 end;

 { Closed by mrOk/mrCancel }
 frmEditor.ShowModal;      { Bug: IF I use ShowModal, after applying a new skin, the window will loose its 'modal' attribute! }
 FreeAndNil(frmEditor);    { We need to free the form because the Close will only hide the form! }
end;


{ Show non modal, for testing against bug.
  Remember to call FreeAndNil(frmEditor) }
class procedure TfrmSkinRes.ShowEditorNonModal;
begin
 var frmEditor:= TfrmSkinRes.Create(Application);

 WITH frmEditor DO
 begin
   LoadForm(frmEditor, TRUE);            { Position form }
   if Translator <> NIL
   then Translator.LoadFormTranlation(frmEditor);
   Font:= Application.MainForm.Font;     { Themes }
 end;

 frmEditor.Show;      { Bug: IF I use ShowModal, after applying a new skin, the window will loose its 'modal' attribute! }
end;


{
procedure ShowSkinForm;
VAR
   frmSkins: TfrmSkinRes;
begin
 frmSkins:= TfrmSkinRes.Create(NIL);
 frmSkins.ShowModal;
 FreeAndNil(frmSkins);
end;  }



{-----------------------------------------------------------------------------------------------------------------------
   CREATE
-----------------------------------------------------------------------------------------------------------------------}
procedure TfrmSkinRes.FormCreate(Sender: TObject);
begin
 LoadForm(Self);
 PopulateSkins;    
end;


procedure TfrmSkinRes.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action:= caFree;
 {Action:= caFree; Delphi bug: https://quality.embarcadero.com/browse/RSP-33140. Fixed in delphi 11 }
end;


procedure TfrmSkinRes.FormDestroy(Sender: TObject);
begin
 SaveForm(Self);
 if NOT AppData.Initializing
 then cmINIFileQuick.WriteString ('LastSkin', LastSkin);   { We don't save anything if the start up was improper! }
end;


procedure TfrmSkinRes.btnOKClick(Sender: TObject);
begin
 Close; // Not needed when I show the form modal
end;





{-------------------------------------------------------------------------------------------------------------
   Populate skins
-------------------------------------------------------------------------------------------------------------}

procedure TfrmSkinRes.lblTopClick(Sender: TObject);
begin
 PopulateSkins;
end;


procedure TfrmSkinRes.PopulateSkins;
VAR
   StyleName: string;
begin
  lBox.Clear;
 /// lBox.Items.Add(DefWinTheme); // ?needed?   { This corresponds to Windows' default theme }
 ///
  { Retrieve all the styles linked in the executable }
  for StyleName in TStyleManager.StyleNames DO
    lBox.Items.Add(StyleName);

 lBox.ItemIndex := lBox.Items.IndexOf(TStyleManager.ActiveStyle.Name);
end;


procedure TfrmSkinRes.lboxClick(Sender: TObject);
begin
 if lBox.ItemIndex < 0 then EXIT;

 lBox.Enabled:= FALSE;   { Prevent user to double click (because of Application.ProcessMessages below) }
 TRY
  LastSkin:= lBox.Items[lBox.ItemIndex];

  if LastSkin= DefWinTheme then
   begin
    TStyleManager.SetStyle('Windows');
    LastSkin:= DefWinTheme;
   end
  else
    begin
     TStyleManager.SetStyle(LastSkin);

     { Bug fix for: http://stackoverflow.com/questions/30328924/form-losses-modal-attribute-after-changing-app-style?noredirect=1#comment48752692_30328924
       Seems to have been fixed in Alexandria (from which I copied the patch. }
     Application.ProcessMessages;
     BringToFront;
    end;
 FINALLY
  lBox.Enabled:= TRUE;
 END;
end;


procedure TfrmSkinRes.btnSkinEditorClick(Sender: TObject);
begin
 if FileExists(AppData.SysDir+ 'SkinDesigner.exe')
 then ExecuteShell(AppData.SysDir+ 'SkinDesigner.exe')
 else ExecuteURL(wwwSkinDesinger);
end;


procedure TfrmSkinRes.FormKeyPress(Sender: TObject; var Key: Char);
begin
 if Ord(Key) = VK_RETURN then Close;
 if Ord(Key) = VK_ESCAPE then Close;
end;


end.
