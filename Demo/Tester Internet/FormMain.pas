UNIT FormMain;

{=============================================================================================================
   Gabriel Moraru
   2025.01
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   Can be used as template for future applications.
--------------------------------------------------------------------------------------------------------------

=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes, System.Actions,
  VCL.Menus, Vcl.AppEvnts, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Forms, cbAppDataForm,Vcl.Controls, Vcl.ExtCtrls, Vcl.ActnList,
  csSystem, cbAppDataForm;

TYPE
  TMainForm = class(TLightForm)
    Actions     : TActionList;
    AppEvents   : TApplicationEvents;
    MainMenu    : TMainMenu;
    mmo         : TMemo;
    pgCtrl      : TPageControl;
    pnlRight    : TPanel;
    tabMain     : TTabSheet;
    btnIntConnection: TButton;
    tabSecondary: TTabSheet;
    btnGetExtIp: TButton;
    procedure btnIntConnectionClick (Sender: TObject);
    procedure FormClose     (Sender: TObject; var Action: TCloseAction);
    procedure btnGetExtIpClick(Sender: TObject);
  protected
    procedure BeforeRelease; override;
  private
  public
    procedure FormInitialize; {don't forget inherited LateInitialize!} override;
 end;

VAR
   MainForm: TMainForm;

IMPLEMENTATION {$R *.dfm}

USES
   csShell, csExecuteShell, cbAppData, cbCenterControl, ciInternet;



{--------------------------------------------------------------------------------------------------
   APP START/CLOSE
--------------------------------------------------------------------------------------------------}
procedure TMainForm.FormInitialize; inherited FormInitialize;
begin
  { Application }
  AppData.CompanyName:= 'SciVance Technologies';

  { FIRST RUN }
  if AppData.RunningFirstTime then
   begin
    { Preparation of the main form }
    AppData.MainFormCaption('Welcome...');
    CenterForm(MainForm);
    MainForm.pgCtrl.ActivePage:= MainForm.tabMain;            // Default page to show

    if NOT AppData.RunningHome then
      begin
        { Desktop shortcuts/Association }
        csShell.CreateShortcut(AppData.AppName, TRUE);        // OnDesktop
        csShell.CreateShortcut(AppData.AppName, FALSE);       // OnStartMenu

        AssociateWith('.LightSaber', AppData.AppName, FALSE, FALSE, TRUE);

       if NOT AppData.BetaTesterMode
       then csExecuteShell.ExecuteURL(AppData.ProductWelcome);// Welcome page
      end;
   end;

  AppData.MainFormCaption('');

  btnGetExtIpClick(self);
  Show;
end;



{--------------------------------------------------------------------------------------------------
 CLOSE
--------------------------------------------------------------------------------------------------}
procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;


{ It is enough to put SaveBeforeExit in thse two places only: OnCloseQueryand & OnDestroy.
  Details: https://groups.google.com/forum/#!msg/borland.public.delphi.objectpascal/82AG0_kHonU/ft53lAjxWRMJ }
procedure TMainForm.BeforeRelease;
begin
  if NOT Saved then
   begin
     inherited BeforeRelease;
   end;
end;




{--------------------------------------------------------------------------------------------------
   MAIN
--------------------------------------------------------------------------------------------------}
procedure TMainForm.btnIntConnectionClick(Sender: TObject);
begin
  CursorBusy;
  TRY
    Caption:= 'Connecting...';
    ciInternet.TestProgramConnection(TRUE);
  FINALLY
    CursorNotBusy;
  END;
end;


procedure TMainForm.btnGetExtIpClick(Sender: TObject);
begin
  mmo.Text:= GenerateInternetRep;
end;

end.
