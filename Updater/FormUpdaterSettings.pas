unit FormUpdaterSettings;
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
  Winapi.Windows, System.Classes, Vcl.Controls, Vcl.Forms, cbAppDataForm,Vcl.StdCtrls, Vcl.Samples.Spin, cvSpinEdit, cvCheckBox, Vcl.ExtCtrls,
  ciUpdater;

TYPE
  TfrmUpdaterSettings = class(TLightForm)
    btnTestInternet    : TButton;
    chkForceNewsFound  : TCubicCheckBox;
    chkConnectFail     : TCubicCheckBox;
    GroupBox1          : TGroupBox;
    grpInterval        : TGroupBox;
    lblHours           : TLabel;
    Container          : TPanel;
    spnHours           : TCubicSpinEdit;
    lblPrivacy         : TLabel;
    cmbWhen            : TComboBox;
    chkEveryStart      : TLabel;
    pnlBtm: TPanel;
    btnOK: TButton;
    btnApply: TButton;
    procedure btnApplyClick        (Sender: TObject);
    procedure btnStartClick        (Sender: TObject);
    procedure btnTestInternetClick (Sender: TObject);
    procedure cmbWhenChange        (Sender: TObject);
    procedure FormClose            (Sender: TObject; var Action: TCloseAction);
    procedure FormCreate           (Sender: TObject);
    procedure FormDestroy          (Sender: TObject);
    procedure FormKeyPress         (Sender: TObject; var Key: Char);
  private
    procedure GuiFromObject;
    procedure ObjectFromGUI;
    procedure UpdateVisibility;
  public
    procedure Apply;
    class function CreateParented(Container: TWinControl): TfrmUpdaterSettings; static;
    class procedure CreateModal; static;
  end;

IMPLEMENTATION {$R *.dfm}

USES
   cbDialogs, ccAppData, cbAppDataVCL
, ciInternet; //, cvINIFile;



{--------------------------------------------------------------------------------------------------
   CREATE/DESTROY
--------------------------------------------------------------------------------------------------}

// Show in a container (parented)
class function TfrmUpdaterSettings.CreateParented(Container: TWinControl): TfrmUpdaterSettings;
begin
  TAppData.RaiseIfStillInitializing;
  AppData.CreateFormHidden(TfrmUpdaterSettings, Result);
  Result.Container.Parent:= Container;
  Result.btnApply.Visible:= TRUE;
  Result.btnOK.Visible:= FALSE;
  Result.Align:= alClient;
end;



// Show as form
class procedure TfrmUpdaterSettings.CreateModal;
begin
  TAppData.RaiseIfStillInitializing;
  AppData.CreateFormModal(TfrmUpdaterSettings);
end;



procedure TfrmUpdaterSettings.FormCreate(Sender: TObject);
begin
  chkForceNewsFound.Visible:= AppData.BetaTesterMode;   { BT }
  if NOT chkForceNewsFound.Visible
  then chkForceNewsFound.Checked:= FALSE;       { Uncheck it if we are not in HomeMode }
  Assert(cmbWhen.Items.Count = Ord(High(TCheckWhen))+1, 'cmbWhen.Count <> TCheckWhen');
  GuiFromObject;
end;


procedure TfrmUpdaterSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
end;


procedure TfrmUpdaterSettings.FormDestroy(Sender: TObject);
begin
  Assert(Container.Parent = Self);    { We need to move the container back on its original form, in order to let that form to correctly save its children }
  Apply;                              { Reparenting MUST be before Apply }
  //SaveForm(Self); now called automatically
end;







procedure TfrmUpdaterSettings.GuiFromObject;
begin
  //Caption:= IntToStr(length(TCheckWhen));
  cmbWhen.ItemIndex         := Ord(Updater.When);
  spnHours.Value            := Updater.CheckEvery;
  chkForceNewsFound.Checked := Updater.ForceNewsFound;
  chkConnectFail.Checked    := Updater.ShowConnectFail;

  UpdateVisibility;
end;


procedure TfrmUpdaterSettings.ObjectFromGUI;
begin
 Updater.When            := TCheckWhen(cmbWhen.ItemIndex);
 Updater.CheckEvery      := spnHours.Value;
 Updater.ForceNewsFound  := chkForceNewsFound.Checked;
 Updater.ShowConnectFail := chkConnectFail.Checked;
end;



{--------------------------------------------------------------------------------------------------
   BUTTONS
--------------------------------------------------------------------------------------------------}
procedure TfrmUpdaterSettings.btnStartClick(Sender: TObject);
begin
  ObjectFromGUI;
end;


procedure TfrmUpdaterSettings.btnTestInternetClick(Sender: TObject);
begin
 Caption:= 'Checking.......';
 Update;
 Refresh;
 Caption:= ProgramConnect2InternetS;
 mesaj(Caption);
end;


procedure TfrmUpdaterSettings.UpdateVisibility;
begin
 spnHours.Enabled:= TCheckWhen(cmbWhen.ItemIndex) = cwHours;
 lblHours.Enabled:= TCheckWhen(cmbWhen.ItemIndex) = cwHours;

 if TCheckWhen(cmbWhen.ItemIndex) = cwNever then
  begin
    lblPrivacy.Visible:= TRUE;
    if spnHours.Value> 24
    then spnHours.Value:= 24;    { We set the standard value (if the user disabled the updater) }
  end;
end;


procedure TfrmUpdaterSettings.cmbWhenChange(Sender: TObject);
begin
 UpdateVisibility;

 if TCheckWhen(cmbWhen.ItemIndex) = cwNever
 then MesajWarning('Are you really really sure you want to disable the updater? CRLF You will miss not only information about new features and discounts, but also information about critical updates!');
end;


procedure TfrmUpdaterSettings.FormKeyPress(Sender: TObject; var Key: Char);
begin
 if (Ord(Key) = VK_RETURN)
 OR (Ord(Key) = VK_ESCAPE) then Close;
end;





{--------------------------------------------------------------------------------------------------
   APPLY SETTINGS
--------------------------------------------------------------------------------------------------}
procedure TfrmUpdaterSettings.Apply;
begin
  UpdateVisibility;
  ObjectFromGUI;
  Updater.Save;
end;


procedure TfrmUpdaterSettings.btnApplyClick(Sender: TObject);
begin
  Apply;
end;




end.
