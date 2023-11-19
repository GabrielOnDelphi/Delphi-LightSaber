program Tester_SaveGUI;

uses
  FastMM4,
  System.SysUtils,
  Forms,
  MainForm in 'MainForm.pas' {frmTester},
  SecondForm in 'SecondForm.pas' {frmContainer},
  FormLog in '..\..\FormLog.pas',
  ccAppData in '..\..\ccAppData.pas';

{$R *.res}

procedure Main;
 begin
  AppData:= TAppData.Create('Cubic IniFileEx Tester');

  { Properly installed? }
  //if NOT AppData.CheckSysDir then EXIT;

  if AppData.InstanceRunning
  then
    { Send command line to the already running instance and restore (bring to front) that instance }
    AppData.ResurectInstance(Trim(ParamStr(1))) //ToDo: I need to send the URestore message because I need to call RestoreBioniX (to remove icon fromsystray) on that side (BX first instance)
  else
   begin
     AppData.CreateMainForm(TfrmTester, frmTester, TRUE, TRUE);
     Application.Run;
   end;
 end;

begin
  Main;
end.
