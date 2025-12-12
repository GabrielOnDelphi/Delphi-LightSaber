program VCL_Demo_SystemReport;

uses
  FastMM4,
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  LightVcl.Visual.AppDataForm in '..\..\..\FrameVCL\LightVcl.Visual.AppDataForm.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightVcl.Common.Debugger in '..\..\..\FrameVCL\LightVcl.Common.Debugger.pas',
  LightCore.Platform in '..\..\..\LightCore.Platform.pas',
  LightVcl.Common.Reports in '..\..\..\FrameVCL\LightVcl.Common.Reports.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light Demo System Report');
  AppData.CreateMainForm(TfrmMain, True, True, asFull);
  AppData.Run;
end.
