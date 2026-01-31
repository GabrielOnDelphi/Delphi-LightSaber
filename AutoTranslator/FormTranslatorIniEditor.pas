UNIT FormTranslatorIniEditor;

{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   INI TRANSLATION FILE EDITOR

   A simple text editor for viewing and editing translation INI files.
   Shown after creating or auto-translating, or when user wants to manually edit.

   USAGE:
     TfrmTranslatorIniEditor.ShowEditor(FileName);
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.IOUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  LightCore, LightVcl.Translate, LightCore.TextFile, LightVcl.Common.Clipboard;

TYPE
  TfrmTranslatorIniEditor = class(TForm)
    CubicGroupBox1: TGroupBox;
    Splitter1: TSplitter;
    pnlRight: TPanel;
    mmoValues: TMemo;
    Panel2: TPanel;
    btnCopyRight: TButton;
    Panel3: TPanel;
    mmoLangEditor: TMemo;
    Panel1: TPanel;
    btnSaveEditor: TButton;
    btnApplyEdits: TButton;
    btnCopy: TButton;
    btnValues: TButton;
    procedure btnSaveEditorClick(Sender: TObject);
    procedure btnApplyEditsClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnCopyRightClick(Sender: TObject);
    procedure btnValuesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FCurLangFile: string;
    procedure SaveEditor;
  public
    class procedure ShowEditor(const FileName: string); static;
    procedure LoadFile(const FileName: string);
  end;


IMPLEMENTATION {$R *.dfm}


class procedure TfrmTranslatorIniEditor.ShowEditor(const FileName: string);
var
  Editor: TfrmTranslatorIniEditor;
begin
  Editor:= TfrmTranslatorIniEditor.Create(Application);
  Editor.LoadFile(FileName);
  Editor.Show;
end;


procedure TfrmTranslatorIniEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= TCloseAction.caFree;
end;


procedure TfrmTranslatorIniEditor.LoadFile(const FileName: string);
begin
  FCurLangFile:= FileName;

  if FileName <> ''
  then Caption:= 'Editor: ' + ExtractFileName(FileName)
  else Caption:= 'No file loaded';

  btnSaveEditor.Enabled:= FileName <> '';
  btnApplyEdits.Enabled:= FileName <> '';

  if FileExists(FileName)
  then mmoLangEditor.Text:= StringFromFile(FileName);
end;


procedure TfrmTranslatorIniEditor.SaveEditor;
begin
  Assert(FCurLangFile <> '', 'No file to save');
  Assert(System.IOUtils.TPath.HasValidFileNameChars(ExtractFileName(FCurLangFile), FALSE));

  StringToFile(FCurLangFile, mmoLangEditor.Text, woOverwrite, wpOn);
  Caption:= 'Editor: ' + ExtractFileName(FCurLangFile) + ' (saved)';
end;


procedure TfrmTranslatorIniEditor.btnSaveEditorClick(Sender: TObject);
begin
  SaveEditor;
end;


procedure TfrmTranslatorIniEditor.btnApplyEditsClick(Sender: TObject);
begin
  SaveEditor;
  Translator.CurLanguage:= FCurLangFile;
  Translator.LoadTranslation(TRUE);  { Force reload even if same language }
end;


procedure TfrmTranslatorIniEditor.btnCopyClick(Sender: TObject);
begin
  StringToClipboard(mmoLangEditor.Text);
end;


procedure TfrmTranslatorIniEditor.btnCopyRightClick(Sender: TObject);
begin
  StringToClipboard(mmoValues.Text);
end;


procedure TfrmTranslatorIniEditor.btnValuesClick(Sender: TObject);
VAR
  s, Output: string;
begin
  Output:= '';
  for s in mmoLangEditor.Lines DO
    if Pos('=', s) > 0
    then Output:= Output + CopyFrom(s, '=', Length(s), FALSE) + CRLF
    else Output:= Output + s + CRLF;

  mmoValues.Text:= Output;
  pnlRight.Visible:= NOT pnlRight.Visible;
end;


end.
