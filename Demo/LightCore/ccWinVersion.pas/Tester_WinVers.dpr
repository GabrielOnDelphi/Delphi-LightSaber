program Tester_WinVers;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  cbAppData,
  Forms,
  FormMain in 'FormMain.pas' {frmTester};

{$R *.res}

begin
  Application.Initialize;

  AppData:= TAppData.Create('Light Tester WinVer');
  AppData.CreateMainForm(TfrmTester, frmTester, TRUE);
  Application.Run;
end.
