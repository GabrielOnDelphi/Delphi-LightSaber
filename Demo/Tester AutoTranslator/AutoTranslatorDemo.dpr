program AutoTranslatorDemo;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  Forms,
  FormMain in 'FormMain.pas' {frmTester},
  cbAppData in '..\..\LightSaber\cbAppData.pas',
  cTranslate in '..\cTranslate.pas',
  FormSelectLang in '..\FormSelectLang.pas',
  FormTranslator in '..\FormTranslator.pas';

{$R *.res}

begin
  Application.Initialize;
  AppData:= TAppData.Create('Light Automatic Translator');
  AppData.CreateMainForm(TfrmTester, frmTester, TRUE);
  Application.Run;
end.
