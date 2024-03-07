program Tester_Translator;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  Forms,
  FormMain in 'FormMain.pas' {frmTester},
  FormTranslator in '..\..\Packages\LightSaber\FormTranslator.pas',
  ccAppData in '..\..\ccAppData.pas';

{$R *.res}

begin
  Application.Initialize_;
  AppData:= TAppData.Create('Cubic Translator Tester');
  AppData.CreateMainForm(TfrmTester, frmTester, TRUE);
  Application.Run;
end.
