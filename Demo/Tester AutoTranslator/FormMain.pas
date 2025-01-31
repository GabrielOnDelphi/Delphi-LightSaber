UNIT FormMain; 

INTERFACE

USES
  WinApi.Messages, System.SysUtils, System.Classes, Vcl.StdCtrls, Vcl.Forms, Vcl.Controls, Vcl.ExtCtrls,
  cbAppData, cvINIFile, Vcl.Mask, cbAppDataForm;

TYPE
 TfrmTester = class(TLightForm)
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
    procedure FormInitialize; {don't forget inherited LateInitialize!} override; // Called after the main form was fully created
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


procedure TfrmTester.FormInitialize;
begin
 inherited FormInitialize;

 Translator:= TTranslator.Create;  // Initialize the translator
 Translator.LoadLastTranslation;   // Load last language
end;


procedure TfrmTester.FormDestroy(Sender: TObject);
begin
 FreeAndNil(Translator);
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
