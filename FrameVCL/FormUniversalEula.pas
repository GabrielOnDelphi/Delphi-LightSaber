UNIT FormUniversalEula;

{=============================================================================================================
   2026.01.29
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   UNIVERSAL EULA FORM

   Displays the End User License Agreement (EULA) in a modal dialog.
   The license text can be loaded from an external file or uses the built-in default text.

   USAGE:
     Call ShowEulaModal to display the EULA dialog.
     The form stays on top and must be acknowledged before continuing.

   EXTERNAL EULA FILE:
     Place 'Eula.txt' in AppData.AppSysDir to override the built-in license text.

   NOTES:
     - Form closes on OK button, Enter key, or Escape key
     - DON'T ADD IT TO ANY DPK!

   See: Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON}

USES
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, LightVcl.Visual.AppDataForm,Vcl.StdCtrls;

TYPE
  TfrmEULA = class(TLightForm)
    btnOK: TButton;
    mmoLicense: TMemo;
    procedure btnOKClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
  end;

procedure ShowEulaModal;

IMPLEMENTATION {$R *.dfm}

USES
   LightCore.TextFile, LightCore.AppData, LightVcl.Visual.AppData;

CONST
  EULA_FILENAME = 'Eula.txt';


{ Displays the EULA form as a modal dialog.
  If an external Eula.txt file exists in AppSysDir, it loads that text.
  Otherwise uses the built-in license text from the form. }
procedure ShowEulaModal;
var
  frmEULA: TfrmEULA;
  EulaPath: string;
begin
  frmEULA:= TfrmEULA.Create(NIL);
  try
    Assert(frmEULA.FormStyle = fsStayOnTop, 'EULA form must be fsStayOnTop!');
    Assert(frmEULA.Visible = FALSE, 'Form Visible must be False for ShowModal to work!');

    { Load external EULA text if available }
    EulaPath:= AppData.AppSysDir + EULA_FILENAME;
    if FileExists(EulaPath)
    then frmEULA.mmoLicense.Text:= StringFromFile(EulaPath);

    frmEULA.ShowModal;
  finally
    FreeAndNil(frmEULA);
  end;
end;


procedure TfrmEULA.btnOKClick(Sender: TObject);
begin
  Close;
end;


procedure TfrmEULA.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= TCloseAction.caFree;
end;


{ Closes the form when user presses Escape or Enter }
procedure TfrmEULA.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) OR (Key = VK_RETURN) then Close;
end;


end.
