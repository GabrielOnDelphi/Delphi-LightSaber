UNIT FormUpdaterSettings;

{=============================================================================================================
   2026.05.18
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   FMX port of the VCL FormUpdaterSettings (Updater\FormUpdaterSettings.pas).

   GUI for changing TUpdater settings (when to check, how often, behaviour flags).

   CROSS-PLATFORM: Yes (Windows, macOS, iOS, Android)
   SCOPE: End-user settings only. The VCL CreateParented (embedded) path and the
          TestProgramConnection button are intentionally omitted.

   THIS FORM IS USED BY MULTIPLE PROGRAMS. Do not localize it for a specific program.

   How to use it:
       TfrmUpdaterSettings.CreateModal;

   See "ciUpdater.pas" for the model behind this view.
=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON}

USES
  System.SysUtils, System.Classes, System.UITypes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ListBox, FMX.EditBox, FMX.SpinBox,
  LightFmx.Common.AppData.Form, LightFmx.Visual.CheckBox,
  ciUpdater;


TYPE
  TfrmUpdaterSettings = class(TLightForm)
    Container         : TLayout;
    grpInterval       : TGroupBox;
    cmbWhen           : TComboBox;
    lblHours          : TLabel;
    spnHours          : TSpinBox;
    GroupBox1         : TGroupBox;
    chkForceNewsFound : TLightCheckBox;
    chkConnectFail    : TLightCheckBox;
    lblPrivacy        : TLabel;
    pnlBtm            : TLayout;
    btnOK             : TButton;
    btnApply          : TButton;
    procedure FormCreate     (Sender: TObject);
    procedure FormClose      (Sender: TObject; var Action: TCloseAction);
    procedure cmbWhenChange  (Sender: TObject);
    procedure btnOKClick     (Sender: TObject);
    procedure btnApplyClick  (Sender: TObject);
  private
    procedure GuiFromObject;
    procedure ObjectFromGUI;
    procedure UpdateVisibility;
  public
    procedure Apply;
    class procedure CreateModal; static;
  end;


IMPLEMENTATION {$R *.fmx}

USES
  LightCore, LightFmx.Common.AppData, LightFmx.Common.Dialogs;


{--------------------------------------------------------------------------------------------------
   CREATE
--------------------------------------------------------------------------------------------------}

class procedure TfrmUpdaterSettings.CreateModal;
begin
  AppData.CreateFormModal(TfrmUpdaterSettings);
end;


procedure TfrmUpdaterSettings.FormCreate(Sender: TObject);
begin
  Assert(Updater <> NIL, 'TUpdater (Updater global) must be created before opening TfrmUpdaterSettings.');

  CloseOnEscape:= TRUE;

  { Populate cmbWhen if the FMX designer didn't (the .fmx ships with the items, but be defensive
    in case a future edit blanks them). The combo order matches TCheckWhen ordinal values. }
  if cmbWhen.Items.Count = 0 then
    begin
      cmbWhen.Items.Add('Never');
      cmbWhen.Items.Add('At startup');
      cmbWhen.Items.Add('Every N hours');
    end;
  Assert(cmbWhen.Items.Count = Ord(High(TCheckWhen)) + 1, 'cmbWhen item count <> TCheckWhen size');

  chkForceNewsFound.Visible:= AppData.BetaTesterMode;
  if NOT chkForceNewsFound.Visible
  then chkForceNewsFound.IsChecked:= FALSE;

  GuiFromObject;
end;


{--------------------------------------------------------------------------------------------------
   GUI <-> OBJECT
--------------------------------------------------------------------------------------------------}
procedure TfrmUpdaterSettings.GuiFromObject;
begin
  cmbWhen.ItemIndex          := Ord(Updater.When);
  spnHours.Value             := Updater.CheckEvery;
  chkForceNewsFound.IsChecked:= Updater.ForceNewsFound;
  chkConnectFail.IsChecked   := Updater.ShowConnectFail;

  UpdateVisibility;
end;


procedure TfrmUpdaterSettings.ObjectFromGUI;
begin
  { Skip cmbWhen write when no item is selected — keeps Updater.When unchanged rather than corrupting it with an out-of-range cast }
  if cmbWhen.ItemIndex >= 0
  then Updater.When     := TCheckWhen(cmbWhen.ItemIndex);
  Updater.CheckEvery      := Round(spnHours.Value);
  Updater.ForceNewsFound  := chkForceNewsFound.IsChecked;
  Updater.ShowConnectFail := chkConnectFail.IsChecked;
end;


procedure TfrmUpdaterSettings.UpdateVisibility;
VAR W: TCheckWhen;
begin
  { Guard against ItemIndex = -1 (no selection) — casting it to TCheckWhen would yield an out-of-range enum }
  if cmbWhen.ItemIndex < 0
  then W:= cwHours
  else W:= TCheckWhen(cmbWhen.ItemIndex);

  spnHours.Enabled:= W = cwHours;
  lblHours.Enabled:= W = cwHours;

  if W = cwNever then
    begin
      lblPrivacy.Visible:= TRUE;
      if spnHours.Value > 24
      then spnHours.Value:= 24;
    end
  else
    lblPrivacy.Visible:= FALSE;
end;


procedure TfrmUpdaterSettings.cmbWhenChange(Sender: TObject);
begin
  UpdateVisibility;

  if (cmbWhen.ItemIndex >= 0) AND (TCheckWhen(cmbWhen.ItemIndex) = cwNever)
  then MessageWarning(
    'Are you really really sure you want to disable the updater?' + CRLF +
    'You will miss not only information about new features and discounts, ' +
    'but also information about critical updates!');
end;


{ Free the form on close so reopening creates a fresh instance.
  Default FMX close action is caHide — without this the form would leak until app shutdown. }
procedure TfrmUpdaterSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= TCloseAction.caFree;
end;


{--------------------------------------------------------------------------------------------------
   APPLY
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


procedure TfrmUpdaterSettings.btnOKClick(Sender: TObject);
begin
  Apply;
  Close;
end;


end.
