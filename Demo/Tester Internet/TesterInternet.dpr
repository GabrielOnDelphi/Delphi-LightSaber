program TesterInternet;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  WinApi.Windows,
  VCL.Forms,
  FormMain in 'FormMain.pas' {MainForm},
  cbAppDataVCL in '..\..\FrameVCL\cbAppData.pas',
  FormRamLog in '..\..\FrameVCL\FormRamLog.pas',
  ciInetDonwIndy in '..\..\FrameVCL\ciInetDonwIndy.pas',
  ciInternet in '..\..\FrameVCL\ciInternet.pas';

{$R *.res}

begin
  CONST
     MultiThreaded= FALSE;                  // True => Only if we need to use multithreading in the Log.
  CONST
     AppName= 'Light Demo App Template Simple';  // Absolutelly critical if you use the SaveForm/LoadForm functionality. This string will be used as the name of the INI file.

  AppData:= TAppData.Create(AppName, '', True, MultiThreaded);
  AppData.CreateMainForm(TMainForm, MainForm, True, True);
  TfrmRamLog.CreateGlobalLog;
  
  AppData.Run;
end.
