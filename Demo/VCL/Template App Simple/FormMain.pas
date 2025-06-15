UNIT FormMain;

{=============================================================================================================
   2025.05
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Use this as a template when you start a new simple application
=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes, System.Actions,
  VCL.Menus, Vcl.AppEvnts, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Forms, Vcl.Controls, Vcl.ExtCtrls, Vcl.ActnList,
  LightVcl.Common.SystemTime, LightVcl.Common.Clipboard, LightVcl.Common.AppDataForm;

TYPE
  TMainForm = class(TLightForm)
    Actions     : TActionList;
    MainMenu    : TMainMenu;
    mmo         : TMemo;
    pgCtrl      : TPageControl;
    pnlRight    : TPanel;
    tabMain     : TTabSheet;
    btnStart    : TButton;
    tabSecondary: TTabSheet;
    CheckBox1: TCheckBox;
    procedure btnSTARTClick (Sender: TObject);
    procedure FormClose     (Sender: TObject; var Action: TCloseAction);
    procedure FormCreate    (Sender: TObject);
  protected
  private
  public
    procedure FormPostInitialize; override;
    procedure FormPreRelease; override;
 end;

VAR
   MainForm: TMainForm;

IMPLEMENTATION {$R *.dfm}

USES
   LightVcl.Common.Shell, LightVcl.Common.System, LightVcl.Common.ExecuteShell, ccAppData, LightVcl.Common.AppData, LightVcl.Common.CenterControl;



{--------------------------------------------------------------------------------------------------
   APP START/CLOSE
--------------------------------------------------------------------------------------------------}
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Too early to do initialization here!
  // Initialization code is better done in LateInitialize which takes place AFTER the form was properly constructed!
end;


procedure TMainForm.FormPostInitialize;
begin
  inherited FormPostInitialize;

  // Application
  AppData.CompanyName:= 'SciVance Technologies';

  // First run
  if AppData.RunningFirstTime
  then
   begin
    // Preparation of the main form
    AppData.MainFormCaption('Welcome...');
    CenterForm(MainForm);
    MainForm.pgCtrl.ActivePage:= MainForm.tabMain;            // Default page to show

    if NOT AppData.RunningHome then
      begin
        // Desktop shortcuts
        LightVcl.Common.Shell.CreateShortcut(AppData.AppName, TRUE);        // OnDesktop
        LightVcl.Common.Shell.CreateShortcut(AppData.AppName, FALSE);       // OnStartMenu

        // File association
        AssociateWith('.LightSaber', AppData.AppName, FALSE, FALSE, TRUE);

        // Welcome page
        if NOT AppData.BetaTesterMode  // The program is in BetaTester mode if a file called "betatester" is found in the "System" folder.
        then LightVcl.Common.ExecuteShell.ExecuteURL(AppData.ProductWelcome);
      end;
   end
  else
    AppData.MainFormCaption('');

  btnStartClick(self);
  Show;
end;




{--------------------------------------------------------------------------------------------------
   CLOSE
--------------------------------------------------------------------------------------------------}
procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;


procedure TMainForm.FormPreRelease;
begin
  inherited;
end;





{--------------------------------------------------------------------------------------------------
   MAIN CODE
--------------------------------------------------------------------------------------------------}
procedure TMainForm.btnSTARTClick(Sender: TObject);
begin
  CursorBusy;
  TRY
    Caption:= 'Started...';
  FINALLY
    CursorNotBusy;
  END;
end;



end.
