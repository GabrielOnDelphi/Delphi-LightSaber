program Demo_Internet;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  WinApi.Windows,
  VCL.Forms,
  FormMain in 'FormMain.pas' {MainForm},
  LightCom.AppData in '..\..\FrameVCL\LightCom.AppData.pas',
  LightVcl.LogForm in '..\..\FrameVCL\LightVcl.LogForm.pas',
  ciInetDonwIndy in '..\..\FrameVCL\ciInetDonwIndy.pas',
  ciInternet in '..\..\FrameVCL\ciInternet.pas',
  LightCom.AppDataForm in '..\..\FrameVCL\LightCom.AppDataForm.pas',
  ciDownloadThread in '..\..\FrameVCL\ciDownloadThread.pas',
  ciDownload_WinInet in '..\..\FrameVCL\ciDownload_WinInet.pas',
  ccDownload in '..\..\ccDownload.pas',
  ccAppData in '..\..\ccAppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo Internet'); // This name is absolutelly critical if you use the SaveForm/LoadForm functionality. This string will be used as the name of the INI file.
  AppData.CreateMainForm(TMainForm, MainForm, True, True, asFull);
  AppData.Run;
end.
