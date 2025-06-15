UNIT FormMain;

INTERFACE

USES

  System.SysUtils, System.Classes, Vcl.StdCtrls, Vcl.Forms, Vcl.Controls, Vcl.ExtCtrls,
  LightCom.AppDataForm, Vcl.Mask;

TYPE
 TMainForm = class(TLightForm)
    btnEditor   : TButton;
    btnSelector : TButton;
    CheckBox1   : TCheckBox;
    LabeledEdit : TLabeledEdit;
    lblCurLang  : TLabel;
    Memo        : TMemo;
    pnlRight    : TPanel;
    procedure btnSelectorClick(Sender: TObject);
    procedure btnEditorClick(Sender: TObject);
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
  LightCom.Translate,
  FormTranslEditor,
  FormTranslSelector;




{--------------------------------------------------------------------------------------------------
   APP START/CLOSE
--------------------------------------------------------------------------------------------------}
procedure TMainForm.FormPostInitialize;
begin
  inherited FormPostInitialize;

  Translator.OnTranslationLoaded:= TranslationLoaded;  // Event handler
  TranslationLoaded(Self);                             // Show current loaded translation
end;




{--------------------------------------------------------------------------------------------------
   STUFF
--------------------------------------------------------------------------------------------------}
procedure TMainForm.btnEditorClick(Sender: TObject);
begin
  TfrmTranslEditor.ShowEditor;
end;


procedure TMainForm.btnSelectorClick(Sender: TObject);
begin
  TfrmTranslSelector.ShowSelector;
end;


// Show current loaded translation
procedure TMainForm.TranslationLoaded(Sender: TObject);
begin
  lblCurLang.Caption:= 'Current translation file: '+ Translator.CurLanguageName;
end;


end.
