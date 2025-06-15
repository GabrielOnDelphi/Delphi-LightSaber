program VCL_Demo_SystemReport;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  LighCore.AppData in '..\..\..\LighCore.AppData.pas',
  LightVcl.Common.AppDataForm in '..\..\..\FrameVCL\LightCom.AppDataForm.pas',
  LightVcl.Common.IniFile in '..\..\..\FrameVCL\LightCom.IniFile.pas',
  LightVcl.Common.AppData in '..\..\..\FrameVCL\LightCom.AppData.pas',
  LightVcl.LogForm in '..\..\..\FrameVCL\LightVcl.LogForm.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo System Report');
  AppData.CreateMainForm(TfrmMain, frmMain, True, True, asFull);
  AppData.Run;
end.
