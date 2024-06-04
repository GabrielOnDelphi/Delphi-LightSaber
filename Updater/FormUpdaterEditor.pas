unit FormUpdaterEditor;
{-------------------------------------------------------------------------------------------------------------
   GUI for changing TUpdater settings

   2022-01-29
   THIS FORM IS USED BY MULTIPLE PROGRAMS.
   Do not localize it for a specific program! Instead link to this OR create a copy in project's dir.

   CubicCommonControls\Demo\LightUpdater\Tester_Updater.dpr
-------------------------------------------------------------------------------------------------------------}

INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.Samples.Spin, cvSpinEdit, cvCheckBox, Vcl.ExtCtrls,
  ciUpdater;

TYPE
  TfrmUpdaterEditor = class(TForm)
    btnTestInternet    : TButton;
    chkForceNewsFound  : TCubicCheckBox;
    chkConnectFail     : TCubicCheckBox;
    GroupBox1          : TGroupBox;
    grpInterval        : TGroupBox;
    lblDayHours        : TLabel;
    Container          : TPanel;
    spnInterval        : TCubicSpinEdit;
    lblPrivacy: TLabel;
    cmbWhen: TComboBox;
    chkEveryStart: TLabel;
    pnlBtm: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    procedure btnOKClick   (Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure FormDestroy  (Sender: TObject);
    procedure FormKeyPress (Sender: TObject; var Key: Char);
    procedure btnTestInternetClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure cmbWhenChange(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
  private
    procedure GuiFromObject;
    procedure ObjectFromGUI;
  public
    procedure Apply;
    class procedure ShowEditor(Modal: Boolean); static;
    class function  GetEditor: TfrmUpdaterEditor;
  end;

IMPLEMENTATION {$R *.dfm}

USES
   ccCore, ccAppData, ciInternet, cTranslate, ccINIFileVCL;



{--------------------------------------------------------------------------------------------------
   CREATE/DESTROY
--------------------------------------------------------------------------------------------------}

{ Show the editor }
class function TfrmUpdaterEditor.GetEditor: TfrmUpdaterEditor;
begin
 TAppData.RaiseIfStillInitializing;

 Result:= TfrmUpdaterEditor.Create(Application);  { Freed by ShowModal }
 WITH Result DO
 begin
   Assert(cmbWhen.Items.Count = Ord(High(TCheckWhen))+1, 'cmbWhen.Count <> TCheckWhen');

   { GUI }
   LoadForm(Result, TRUE);            { Position form }
   if Translator <> NIL then Translator.LoadFormTranlation(Result);
   Font:= Application.MainForm.Font;     { Themes }
   chkForceNewsFound.Visible:= AppData.BetaTesterMode;   { BT }
   if NOT chkForceNewsFound.Visible
   then chkForceNewsFound.Checked:= FALSE;       { Uncheck it if we are not in HomeMode }

   GuiFromObject;
 end;
end;


{ Show the editor }
class procedure TfrmUpdaterEditor.ShowEditor(Modal: Boolean);
begin
 VAR frmEditor:= GetEditor;

 if Modal
 then frmEditor.ShowModal   { Closed by mrOk/mrCancel }
 else frmEditor.Show;

 if Modal
 then FreeAndNil(frmEditor);    { We need to free the form because the Close will only hide the form! }
end;


procedure TfrmUpdaterEditor.FormDestroy(Sender: TObject);
begin
  Container.Parent:= Self;    { We need to move the container back on its original form, in order to let that form to correctly save its children }
  SaveForm(Self, TRUE);
end;






procedure TfrmUpdaterEditor.GuiFromObject;
begin
 //Caption:= IntToStr(length(TCheckWhen));
 cmbWhen.ItemIndex         := Ord(Updater.When);
 spnInterval.Value         := Updater.CheckEvery;
 chkForceNewsFound.Checked := Updater.ForceNewsFound;
 chkConnectFail.Checked    := Updater.ShowConnectFail;
end;


procedure TfrmUpdaterEditor.ObjectFromGUI;
begin
 Updater.When               := TCheckWhen(cmbWhen.ItemIndex);
 Updater.CheckEvery         := spnInterval.Value;
 Updater.ForceNewsFound     := chkForceNewsFound.Checked;
 Updater.ShowConnectFail    := chkConnectFail.Checked;
end;




{--------------------------------------------------------------------------------------------------
   BUTTONS
--------------------------------------------------------------------------------------------------}
procedure TfrmUpdaterEditor.btnStartClick(Sender: TObject);
begin
  ObjectFromGUI;
end;


procedure TfrmUpdaterEditor.btnTestInternetClick(Sender: TObject);
begin
 Caption:= ProgramConnect2InternetS;
 mesaj(Caption);
end;


procedure TfrmUpdaterEditor.cmbWhenChange(Sender: TObject);
begin
 spnInterval.Enabled:= TCheckWhen(cmbWhen.ItemIndex) = cwPerDay;
 lblDayHours.Enabled:= TCheckWhen(cmbWhen.ItemIndex) = cwPerDay;

 if TCheckWhen(cmbWhen.ItemIndex) = cwNever then
  begin
    lblPrivacy.Visible:= TRUE;
    MesajWarning('Are you really really sure you want to disable the updater? CRLF You will miss not only information about new features and discounts, but also information about critical updates!');
    if spnInterval.Value> 24
    then spnInterval.Value:= 24;    { We trick the user in using the standard value (if he disabled the updater) }
  end;
end;





{--------------------------------------------------------------------------------------------------
   CLOSE
--------------------------------------------------------------------------------------------------}
procedure TfrmUpdaterEditor.btnCancelClick(Sender: TObject);
begin
 Close;
end;


procedure TfrmUpdaterEditor.Apply;
begin
 { We trick the user in using the standard value (if he disabled the updater) }
 if (TCheckWhen(cmbWhen.ItemIndex) = cwNever)
 AND (spnInterval.Value> 24)
 then spnInterval.Value:= 24;

 { Save }
 ObjectFromGUI;
 Updater.SaveTo(AppData.IniFile);
end;


procedure TfrmUpdaterEditor.btnApplyClick(Sender: TObject);
begin
 Apply;
end;


procedure TfrmUpdaterEditor.btnOKClick(Sender: TObject);
begin
 Apply;
 Close;
end;


procedure TfrmUpdaterEditor.FormKeyPress(Sender: TObject; var Key: Char);
begin
 if Ord(Key) = VK_RETURN then btnOkClick(Sender);
 if Ord(Key) = VK_ESCAPE then Close;
end;




end.
