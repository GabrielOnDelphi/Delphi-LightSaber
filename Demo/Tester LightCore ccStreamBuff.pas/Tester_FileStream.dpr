program Tester_FileStream;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  cbAppDataVCL,
  FormMain in 'FormMain.pas' {MainForm},
  FormRamLog in '..\..\FormRamLog.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('BigSearch');
  AppData.CreateMainForm(TMainForm, MainForm, True, True);    // Main form
  TfrmRamLog.CreateGlobalLog;
  AppData.Run;
end.

