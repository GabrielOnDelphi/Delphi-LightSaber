program Tester_WinVers;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  ccAppData,
  Forms,
  FormMain in 'FormMain.pas' {frmTester};

{$R *.res}

begin
  Application.Initialize_;

  AppData:= TAppData.Create('Cubic Tester WinVer');
  AppData.CreateMainForm(TfrmTester, frmTester, TRUE);
  Application.Run;
end.
