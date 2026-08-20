program VCL_Demo_cGraphText;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  LightVcl.Visual.AppDataForm in '..\..\..\FrameVCL\LightVcl.Visual.AppDataForm.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  FormDemo in 'FormDemo.pas' {frmDemoShadow},
  Vcl.Forms;

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;
  
  AppData:= TAppData.Create('Light Demo cGraphText');
  Application.MainFormOnTaskbar:= TRUE;
  AppData.CreateMainForm(TfrmDemoShadow, asFull);
  AppData.Run;
end.
