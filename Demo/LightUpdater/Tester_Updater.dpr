program Tester_Updater;

uses
  FastMM4,
  Forms,
  MainForm in 'MainForm.pas' {frmDemoStarter},
  Vcl.Themes,
  Vcl.Styles,
  BxConstants in '..\..\..\..\BioniX\SourceCode\BioniX VCL\BxConstants.pas',
  ccAppData in '..\..\ccAppData.pas',
  FormLog in '..\..\FormLog.pas',
  ciUpdater in '..\..\Updater\ciUpdater.pas',
  ciUpdaterRec in '..\..\Updater\ciUpdaterRec.pas',
  FormUpdaterEditor in '..\..\Updater\FormUpdaterEditor.pas',
  FormUpdaterNotifier in '..\..\Updater\FormUpdaterNotifier.pas',
  FormUpdaterRecEditor in '..\..\Updater\FormUpdaterRecEditor.pas';

{$R *.res}

begin
  Application.Initialize_;
  AppData:= TAppData.Create('Cubic Updater Demo');
  AppData.CreateMainForm(TfrmDemoStarter, frmDemoStarter, TRUE);
  Application.Run;
end.



