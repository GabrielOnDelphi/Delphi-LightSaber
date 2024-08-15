program Uninstaller;

uses
  FastMM4,
  Forms,
  UninstallerForm in 'UninstallerForm.pas' {frmMain},
  Vcl.Themes,
  Vcl.Styles,
  cbAppData in '..\..\Packages\LightSaber\cbAppData.pas',
  cvCountDown in '..\..\Packages\LightSaber\cvCountDown.pas',
  csShell in '..\..\Packages\LightSaber\csShell.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Uninstaller'{, WindowClassName});   // stackoverflow.com/questions/75449673/is-it-ok-to-create-an-object-before-application-initialize
  UninstallerForm.CreateCopy;
  AppData.CreateMainForm(TfrmMain, frmMain, TRUE);
  Application.Run;
end.


