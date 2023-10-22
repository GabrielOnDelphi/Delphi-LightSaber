UNIT TesterForm;

INTERFACE

USES
  WinApi.Windows, Winapi.ShellAPI, WinApi.Messages, System.SysUtils, System.Classes, Vcl.StdCtrls, VCL.Forms, Vcl.Controls, Vcl.Samples.Spin,
  Vcl.ComCtrls, ccCore, ccINIFile, Vcl.ExtCtrls, FormLog, ccRichLog, Vcl.Mask;

const
 MSG_LateInitialize= WM_APP + 4711;

TYPE
 TfrmTester = class(TForm)
    pnlRight: TPanel;
    btnShowTranslator: TButton;
    CheckBox1: TCheckBox;
    Memo1: TMemo;
    LabeledEdit: TLabeledEdit;
    btnHelper: TButton;
    //Log: TRichLog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnShowTranslatorClick(Sender: TObject);
    procedure btnHelperClick(Sender: TObject);
  protected
  private
    procedure LateInitialize(VAR message: TMessage);  message MSG_LateInitialize;
  public
 end;

VAR
   frmTester: TfrmTester;

IMPLEMENTATION  {$R *.dfm}

USES
  cvIniFile,
  ctTranslate,
  ccAppData,
  FormTranslator, FormSelectLang;




{--------------------------------------------------------------------------------------------------
   APP START/CLOSE
--------------------------------------------------------------------------------------------------}
procedure TfrmTester.FormCreate(Sender: TObject);
begin
 //AppLog:= Log;
 Winapi.ShellAPI.DragAcceptFiles(Self.Handle, True);                                             { Accept the dropped files from Windows Explorer }
 PostMessage(Self.Handle, MSG_LateInitialize, 0, 0);                        { This will call LateInitialize }
end;


procedure TfrmTester.LateInitialize;
begin
 LoadForm(Self);

  // Translator
 Translator:= TTranslator.Create;
 Translator.LoadLastTranslation; // Load last language

 // Translator helper tool
 {frmTranslator:= TfrmTranslator.Create(Application);
 frmTranslator.Show;  }

 AppData.Initializing:= FALSE;
end;


procedure TfrmTester.FormDestroy(Sender: TObject);
begin
 FreeAndNil(Translator);
 SaveForm(Self);
 FreeAndNil(AppData-);
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
