program Uninstaller;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  UninstallerForm in 'UninstallerForm.pas' {frmMain},
  Vcl.Themes,
  Vcl.Styles,
  cbAppDataVCL,
  cvCountDown in '..\..\LightSaber\cvCountDown.pas',
  csShell in '..\..\LightSaber\csShell.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Uninstaller'{, WindowClassName});   // stackoverflow.com/questions/75449673/is-it-ok-to-create-an-object-before-application-initialize
  UninstallerForm.CreateCopy;
  AppData.CreateMainForm(TfrmMain, frmMain, TRUE);
  AppData.Run;
end.


