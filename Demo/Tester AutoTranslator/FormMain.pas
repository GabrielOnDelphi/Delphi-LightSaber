UNIT FormMain; 

INTERFACE

USES
  WinApi.Messages, System.SysUtils, System.Classes, Vcl.StdCtrls, VCL.Forms, Vcl.Controls, Vcl.ExtCtrls,
  cbAppData, cvINIFile, Vcl.Mask;

TYPE
 TfrmTester = class(TForm)
    pnlRight: TPanel;
    btnShowTranslator: TButton;
    CheckBox1: TCheckBox;
    Memo1: TMemo;
    LabeledEdit: TLabeledEdit;
    btnHelper: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnShowTranslatorClick(Sender: TObject);
    procedure btnHelperClick(Sender: TObject);
  protected
  private
    procedure LateInitialize(VAR Msg: TMessage); message MSG_LateFormInit; // Called after the main form was fully created
  public
 end;

VAR
   frmTester: TfrmTester;

IMPLEMENTATION  {$R *.dfm}

USES
  cTranslate,
  FormTranslator,
  FormSelectLang;




{--------------------------------------------------------------------------------------------------
   APP START/CLOSE
--------------------------------------------------------------------------------------------------}
procedure TfrmTester.FormCreate(Sender: TObject);
begin
  //
end;


procedure TfrmTester.LateInitialize;
begin
 LoadForm(Self);

 Translator:= TTranslator.Create;  // Initialize the translator
 Translator.LoadLastTranslation;   // Load last language
end;


procedure TfrmTester.FormDestroy(Sender: TObject);
begin
 FreeAndNil(Translator);
 SaveForm(Self);
end;


procedure TfrmTester.btnHelperClick(Sender: TObject);
begin
 VAR frmTranslator:= TfrmTranslator.Create(Application);
 frmTranslator.Show;
end;


procedure TfrmTester.btnShowTranslatorClick(Sender: TObject);
begin
 ShowSelectLanguage;
end;





end.
