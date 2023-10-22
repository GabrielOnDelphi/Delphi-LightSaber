program Tester_SaveGUI;

uses
  fastmm4,
  Forms,
  MainForm in 'MainForm.pas' {frmTester},
  SecondForm in 'SecondForm.pas' {frmContainer},
  FormLog in '..\..\FormLog.pas',
  ccAppData in '..\..\ccAppData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;                 { If true, a taskbar button represents the application's main form and displays its caption. All child forms will stay on top of the MainForm (bad)! If False, a taskbar button represents the application's (hidden) main window and bears the application's Title. Must be True to use Windows (Vista) Aero effects (ive taskbar thumbnails, Dynamic Windows, Windows Flip, Windows Flip 3D). https://stackoverflow.com/questions/66720721/ }
  AppData:= TAppDataEx.Create('Cubic IniFileEx Tester'); { Absolutelly critical if you use the SaveForm/LoadForm functionality. This string will be used as the name of the INI file. }
  Application.CreateForm(TfrmTester, frmTester);
  frmTester.Show;
  Application.Run;
end.
