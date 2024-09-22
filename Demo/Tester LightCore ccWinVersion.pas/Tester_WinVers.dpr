program Tester_WinVers;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  cbAppData,
  Forms,
  FormMain in 'FormMain.pas' {frmTester},
  FormRamLog in '..\..\FormRamLog.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light Tester WinVer');
  AppData.CreateMainForm(TfrmTester, frmTester, TRUE);
  TfrmRamLog.CreateFormAppData;
  Application.Run;
end.
