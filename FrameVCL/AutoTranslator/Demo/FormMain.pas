UNIT FormMain;

INTERFACE

USES
  System.SysUtils, System.Classes, Vcl.StdCtrls, Vcl.Forms, Vcl.Controls, Vcl.ExtCtrls, Vcl.Mask,
  LightVcl.Visual.AppDataForm;

TYPE
 TMainForm = class(TLightForm)
    btnEditManual: TButton;
    btnSelector : TButton;
    lblCurLang  : TLabel;
    pnlRight    : TPanel;
    btnEditAuto: TButton;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    LabeledEdit: TLabeledEdit;
    Memo: TMemo;
    btnAddNewAuto: TButton;
    procedure btnSelectorClick(Sender: TObject);
    procedure btnEditManualClick(Sender: TObject);
    procedure btnEditAutoClick(Sender: TObject);
    procedure btnAddNewAutoClick(Sender: TObject);
  protected
  private
    procedure TranslationLoaded(Sender: TObject);
  public
    procedure FormPostInitialize; override; // Called after the main form was fully created
 end;

VAR
   MainForm: TMainForm;

IMPLEMENTATION  {$R *.dfm}

USES
  LightVcl.Common. Translate,
  FormTranslEditor,
  FormTranslatorIniEditor,
  FormTranslSelector;




{--------------------------------------------------------------------------------------------------
   APP START/CLOSE
--------------------------------------------------------------------------------------------------}
procedure TMainForm.FormPostInitialize;
begin
  inherited FormPostInitialize;

  Translator.OnTranslationLoaded:= TranslationLoaded;  // Event handler
  TranslationLoaded(Self);                             // Show current loaded translation
  MainFormCaption('');
end;




{--------------------------------------------------------------------------------------------------
   RIGHT PANEL
   SELECT/ADD/EDIT
--------------------------------------------------------------------------------------------------}

procedure TMainForm.btnSelectorClick(Sender: TObject);
begin
  TfrmTranslSelector.ShowSelector;
end;


procedure TMainForm.btnAddNewAutoClick(Sender: TObject);
begin
  TfrmTranslEditor.ShowEditor;
end;


procedure TMainForm.btnEditAutoClick(Sender: TObject);
begin
  TfrmTranslEditor.ShowEditor(ChangeFileExt(Translator.CurLanguageName, ''), TRUE);
end;


procedure TMainForm.btnEditManualClick(Sender: TObject);
begin
  TfrmTranslatorIniEditor.ShowEditor(Translator.CurLanguage);
end;




{--------------------------------------------------------------------------------------------------
   GUI UPDATE
--------------------------------------------------------------------------------------------------}

// Show current loaded translation
procedure TMainForm.TranslationLoaded(Sender: TObject);
begin
  lblCurLang.Caption:= 'Current translation file: '+ Translator.CurLanguageName;
end;


end.
