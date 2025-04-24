program Demo_FileStream;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  cbAppDataVCL,
  FormMain in 'FormMain.pas' {MainForm},
  FormRamLog in '..\..\FrameVCL\FormRamLog.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('BigSearch');
  AppData.CreateMainForm(TMainForm, MainForm, True, True);    // Main form
  TfrmRamLog.CreateGlobalLog !Remove this!;
  AppData.Run;
end.

