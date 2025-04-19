program Demo_Internet;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  WinApi.Windows,
  VCL.Forms,
  FormMain in 'FormMain.pas' {MainForm},
  cbAppDataVCL in '..\..\FrameVCL\cbAppDataVCL.pas',
  FormRamLog in '..\..\FrameVCL\FormRamLog.pas',
  ciInetDonwIndy in '..\..\FrameVCL\ciInetDonwIndy.pas',
  ciInternet in '..\..\FrameVCL\ciInternet.pas',
  cbAppDataForm in '..\..\FrameVCL\cbAppDataForm.pas';

{$R *.res}

begin
  CONST AppName= 'Light Demo Internet';  // Absolutelly critical if you use the SaveForm/LoadForm functionality. This string will be used as the name of the INI file.

  AppData:= TAppData.Create(AppName);
  AppData.CreateMainForm(TMainForm, MainForm, True, True);
  TfrmRamLog.CreateGlobalLog;
  
  AppData.Run;
end.
