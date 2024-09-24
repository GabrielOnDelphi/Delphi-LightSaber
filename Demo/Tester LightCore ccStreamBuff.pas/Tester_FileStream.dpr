program Tester_FileStream;

uses
  FastMM4,
  VCL.Forms,
  cbAppData,
  FormMain in 'FormMain.pas' {MainForm},
  FormRamLog in '..\..\FormRamLog.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('BigSearch');
  AppData.CreateMainForm(TMainForm, MainForm, True, True);    // Main form
  TfrmRamLog.CreateGlobalLog;
  Application.Run;
end.

