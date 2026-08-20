program VCL_Demo_VisualControls;

uses
  {$IFDEF DEBUG} FastMM4, {$ENDIF}
  MainForm in 'MainForm.pas' {frmMain},
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  LightVcl.Visual.AppDataForm in '..\..\..\FrameVCL\LightVcl.Visual.AppDataForm.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  Vcl.Forms;

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo Visual Controls');
  Application.MainFormOnTaskbar:= TRUE;
  AppData.CreateMainForm(TfrmMain, frmMain, asFull);
  AppData.Run;
end.
