program TemplateSimple;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  WinApi.Windows,
  VCL.Forms,
  FormMain in 'FormMain.pas' {MainForm},
  cbAppData in '..\..\cbAppData.pas',
  FormRamLog in '..\..\FormRamLog.pas';

{$R *.res}

begin
  CONST
     MultiThreaded= FALSE;                  // True => Only if we need to use multithreading in the Log.
  CONST
     AppName= 'Light Demo App Template Simple';  // Absolutelly critical if you use the SaveForm/LoadForm functionality. This string will be used as the name of the INI file.

  AppData:= TAppData.Create(AppName, '', True, MultiThreaded);
  AppData.CreateMainForm(TMainForm, MainForm, True, True);
  TfrmRamLog.CreateGlobalLog;
  
  Application.Run;
end.
