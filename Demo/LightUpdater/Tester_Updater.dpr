program Tester_Updater;

uses
  FastMM4,
  Forms,
  MainForm in 'MainForm.pas' {frmDemoStarter},
  Vcl.Themes,
  Vcl.Styles,
  cbAppData,
  ciUpdater in '..\..\Updater\ciUpdater.pas',
  ciUpdaterRec in '..\..\Updater\ciUpdaterRec.pas',
  cTranslate in '..\..\..\LightAutoTranslator\cTranslate.pas',
  FormSelectLang in '..\..\..\LightAutoTranslator\FormSelectLang.pas',
  FormTranslator in '..\..\..\LightAutoTranslator\FormTranslator.pas',
  FormUpdaterNotifier in '..\..\Updater\FormUpdaterNotifier.pas',
  FormUpdaterRecEditor in '..\..\Updater\FormUpdaterRecEditor.pas',
  FormUpdaterSettings in '..\..\Updater\FormUpdaterSettings.pas',
  ciDownload in '..\..\ciDownload.pas';

{$R *.res}

begin
  Application.Initialize;
  AppData:= TAppData.Create('Light Updater Demo');
  AppData.CreateMainForm(TfrmDemoStarter, frmDemoStarter, TRUE);
  Application.Run;
end.



