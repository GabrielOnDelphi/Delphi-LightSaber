program Tester_Translator;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  Forms,
  TesterForm in 'TesterForm.pas' {frmTester},
  FormTranslator in '..\..\Packages\CubicCommonControls\FormTranslator.pas',
  ccAppData in '..\..\ccAppData.pas';

{$R *.res}

begin
  Application.Initialize;
  AppData:= TAppData.Create('Cubic Translator Tester');
  Application.MainFormOnTaskbar := FALSE;                 { If true, a taskbar button represents the application's main form and displays its caption. All child forms will stay on top of the MainForm (bad)! If False, a taskbar button represents the application's (hidden) main window and bears the application's Title. Must be True to use Windows (Vista) Aero effects (ive taskbar thumbnails, Dynamic Windows, Windows Flip, Windows Flip 3D). https://stackoverflow.com/questions/66720721/ }
  Application.CreateForm(TfrmTester, frmTester);
  frmTester.Show;
  Application.Run;
end.
