UNIT FormSkinsDisk;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Universal skin loader. Loads skins from disk (vsf file)

   DON'T ADD IT TO ANY DPK!

   To use it:
      Application.ShowMainForm:= FALSE;   // Necessary so the form won't flicker during skin loading at startup
      MainForm.Visible:= FALSE;
      LoadLastSkin  (during application initialization)
      MainForm.Show;
      Skins should be present in the 'System\skins' folder
      On first run, set the DefaultSkin to an existing file (no path) like: 'Graphite Green.vsf'. Leave it empty if you want the default Windows theme to load 

      Call TfrmSkinDisk.ShowEditor to show this form.
	  
   Info:  
      If you want to make your code aware of the selected VCL style, you can use StyleServices.GetStyleColor, StyleServices.GetStyleFontColor, and StyleServices.GetSystemColor in Vcl.Themes unit.
      It does not work with MDI forms (the MDI children are not correctly painted). However, it works if I apply the skins directly from the IDE.

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
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  Winapi.Windows, System.SysUtils, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, System.Classes, Vcl.Forms;

TYPE
  TfrmSkinDisk = class(TForm)
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
    FOnDefaultSkin: TNotifyEvent;
    procedure PopulateSkins;
  public
    class procedure ShowEditor; static;
    class procedure ShowEditorNonModal(Nottify: TNotifyEvent); static;
  published
    property OnDefaultSkin: TNotifyEvent read FOnDefaultSkin write FOnDefaultSkin;
 end;


procedure LoadLastSkin(CONST DefaultSkin: string= '');  { On first run, set the DefaultSkin to an existing file (no path) like: 'Graphite Green.vsf'. Leave it empty if you want the default Windows theme to load }



IMPLEMENTATION {$R *.dfm}

USES
   ctTranslate, cmINIFileQuick, uLinks, Vcl.Themes, ccAppdata, csExecuteShell,
   cGraphUtil, cmSound, cmVclUtils, ccCenterControl, cvIniFile, IOUtils, ccIO, ccCore;   { VCL.Styles is mandatory here}

CONST
  DefWinTheme= 'Windows default theme';

VAR
  LastSkin: string;                                              { Disk short file name (not full path) for the current loaded skin }



{-----------------------------------------------------------------------------------------------------------------------
   UTILS
-----------------------------------------------------------------------------------------------------------------------}
function GetSkinDir: string;
begin
 Result:= AppData.SysDir+ 'skins\';
end;


function LoadSkinFromFile(CONST DiskShortName: string): Boolean;
VAR  Style : TStyleInfo;
begin
 Result:= FileExists(GetSkinDir+ DiskShortName);

 if Result then
  if TStyleManager.IsValidStyle(GetSkinDir+ DiskShortName, Style)
  then
    if NOT TStyleManager.TrySetStyle(Style.Name, FALSE)
    then
      begin
       TStyleManager.LoadFromFile(GetSkinDir+ DiskShortName);
       TStyleManager.SetStyle(Style.Name);
      end
    else Result:= FALSE
  else
     MesajError('Style is not valid: '+ GetSkinDir+ DiskShortName);
end;


procedure LoadLastSkin(CONST DefaultSkin: string= '');
begin
 LastSkin:= cmINIFileQuick.ReadString('LastSkin', DefaultSkin);   { This is a relative path so the skin can still be loaded when the application is moved to a different folder }

 if LastSkin = ''
 then LastSkin:= DefaultSkin;

 if (LastSkin > '')
 AND (LastSkin <> DefWinTheme)              { DefWinTheme represents the default Windows theme/skin. In other words don't load any skin file. Let Win skin the app }
 then LoadSkinFromFile(LastSkin);
end;










{-----------------------------------------------------------------------------------------------------------------------
   SHOW EDITOR
-----------------------------------------------------------------------------------------------------------------------}

{ THERE IS A BUG THAT CRASHES THE PROGRAM WHEN I CLOSE THIS WINDOW (after applying a skin) }
class procedure TfrmSkinDisk.ShowEditor;
begin
 VAR frmEditor:= TfrmSkinDisk.Create(NIL);

 WITH frmEditor DO
 begin
   LoadForm(frmEditor, TRUE);            { Position form }
   if Translator <> NIL then Translator.LoadFormTranlation(frmEditor);
   Font:= Application.MainForm.Font;     { Themes }
 end;

 { Closed by mrOk/mrCancel }
 frmEditor.ShowModal;      { Bug: IF I use ShowModal, after applying a new skin, the window will loose its 'modal' attribute! }
 FreeAndNil(frmEditor);    { We need to free the form because the Close will only hide the form! }
end;


{ Show non modal, for testing against bug: Crash when closing the "Skins" form. Fixed by keeping the form open
  Remember to call FreeAndNil(frmEditor) }
class procedure TfrmSkinDisk.ShowEditorNonModal(Nottify: TNotifyEvent);
begin
 var frmEditor:= TfrmSkinDisk.Create(Application);

 WITH frmEditor DO
 begin
   OnDefaultSkin:= Nottify;
   LoadForm(frmEditor, TRUE);            { Position form }
   if Translator <> NIL then Translator.LoadFormTranlation(frmEditor);
   Font:= Application.MainForm.Font;     { Themes }
 end;

 frmEditor.Show;      { Bug: IF I use ShowModal, after applying a new skin, the window will loose its 'modal' attribute! }
end;



{-----------------------------------------------------------------------------------------------------------------------
   CREATE
-----------------------------------------------------------------------------------------------------------------------}
procedure TfrmSkinDisk.FormCreate(Sender: TObject);
begin
 LoadForm(Self);
 PopulateSkins;    
 lblTop.Hint:= 'Skin files are located in '+ GetSkinDir;
end;


procedure TfrmSkinDisk.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action:= caFree;
 {Action:= caFree; Delphi bug: https://quality.embarcadero.com/browse/RSP-33140. Fixed in delphi 11 }
end;


procedure TfrmSkinDisk.FormDestroy(Sender: TObject);
begin
 SaveForm(Self);
 if NOT AppData.Initializing
 then cmINIFileQuick.WriteString ('LastSkin', LastSkin);   { We don't save anything if the start up was improper! }
end;


procedure TfrmSkinDisk.btnOKClick(Sender: TObject);
begin
 Close; // Not needed when I show the form modal
end;





{-------------------------------------------------------------------------------------------------------------
   Populate skins
-------------------------------------------------------------------------------------------------------------}

procedure TfrmSkinDisk.lblTopClick(Sender: TObject);
begin
 PopulateSkins;
end;


procedure TfrmSkinDisk.PopulateSkins;
VAR
   s, FullFileName: string;
begin
 lBox.Clear;
 lBox.Items.Add(DefWinTheme);    { This corresponds to Windows' default theme }
 lblTop.Hint:= GetSkinDir;

 if NOT DirectoryExists(GetSkinDir) then
  begin
   lblTop.Caption:= 'The skin directory could not be located! '+ GetSkinDir+ CRLF+ 'Add skins then click here to refresh the list.';
   lblTop.Color:= clRedBright;
   lblTop.Transparent:= FALSE;
   EXIT;
  end;

 { Display all *.vsf files }
 for FullFileName in TDirectory.GetFiles(GetSkinDir, '*.vsf') DO
  begin
   s:= ExtractFileName(FullFileName);
   lBox.Items.Add(s);
  end;

 lBox.ItemIndex := lBox.Items.IndexOf(TStyleManager.ActiveStyle.Name);
end;


procedure TfrmSkinDisk.lBoxClick(Sender: TObject);
begin
 if lBox.ItemIndex < 0 then EXIT;

 lBox.Enabled:= FALSE;   { Prevent user to double click (because of Application.ProcessMessages below) }
 TRY
  LastSkin:= lBox.Items[lBox.ItemIndex];

  if LastSkin= DefWinTheme then
   begin
    TStyleManager.SetStyle('Windows');
    LastSkin:= DefWinTheme;
    if Assigned(FOnDefaultSkin) then FOnDefaultSkin(Self);
   end
  else
   if LoadSkinFromFile(LastSkin) then
    begin
     { Bug fix for: http://stackoverflow.com/questions/30328924/form-losses-modal-attribute-after-changing-app-style?noredirect=1#comment48752692_30328924
       Seems to have been fixed in Alexandria (from which I copied the patch. }
     Application.ProcessMessages;
     BringToFront;
    end;
 FINALLY
  lBox.Enabled:= TRUE;
 END;
end;


procedure TfrmSkinDisk.btnSkinEditorClick(Sender: TObject);
begin
 if FileExists(AppData.SysDir+ 'SkinDesigner.exe')
 then ExecuteShell(AppData.SysDir+ 'SkinDesigner.exe')
 else ExecuteURL(wwwSkinDesinger);
end;


procedure TfrmSkinDisk.FormKeyPress(Sender: TObject; var Key: Char);
begin
 if Ord(Key) = VK_RETURN then Close;
 if Ord(Key) = VK_ESCAPE then Close;
end;


end.
