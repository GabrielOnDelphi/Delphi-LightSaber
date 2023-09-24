program Tester_Updater;

uses
  FastMM4,
  Forms,
  MainForm in 'MainForm.pas' {frmDemoStarter},
  Vcl.Themes,
  Vcl.Styles,
  ciUpdater in '..\..\ciUpdater.pas',
  ciUpdaterRec in '..\..\ciUpdaterRec.pas',
  FormUpdaterEditor in '..\..\FormUpdaterEditor.pas',
  FormUpdaterNotifier in '..\..\FormUpdaterNotifier.pas',
  FormUpdaterRecEditor in '..\..\FormUpdaterRecEditor.pas',
  BxConstants in '..\..\..\..\BioniX\SourceCode\BioniX VCL\BxConstants.pas',
  ccAppData in '..\..\ccAppData.pas',
  FormLog in '..\..\FormLog.pas';

{$R *.res}

begin
  Application.Initialize;
  AppData:= TAppDataEx.Create('Cubic Updater Demo');
  Application.MainFormOnTaskbar := True;    { If true, a taskbar button represents the application's main form and displays its caption. All child forms will stay on top of the MainForm (bad)! If False, a taskbar button represents the application's (hidden) main window and bears the application's Title. Must be True to use Windows (Vista) Aero effects (ive taskbar thumbnails, Dynamic Windows, Windows Flip, Windows Flip 3D). https://stackoverflow.com/questions/66720721/ } 
  Application.CreateForm(TfrmDemoStarter, frmDemoStarter);
  frmDemoStarter.Show;
  Application.Run;
end.
