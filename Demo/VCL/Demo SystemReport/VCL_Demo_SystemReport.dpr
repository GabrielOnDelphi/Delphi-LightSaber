program VCL_Demo_SystemReport;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  LightVcl.Visual.AppDataForm in '..\..\..\FrameVCL\LightVcl.Visual.AppDataForm.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo System Report');
  AppData.CreateMainForm(TfrmMain, frmMain, True, True, asFull);
  AppData.Run;
end.
