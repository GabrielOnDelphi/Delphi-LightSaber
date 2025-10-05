program VCL_Demo_cGraphText;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  LightVcl.Common.AppData in '..\..\..\FrameVCL\LightVcl.Common.AppData.pas',
  LightVcl.Common.AppDataForm in '..\..\..\FrameVCL\LightVcl.Common.AppDataForm.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  FormDemo in 'FormDemo.pas' {frmDemoShadow};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;
  
  AppData:= TAppData.Create('Light Demo cGraphText');
  AppData.CreateMainForm(TfrmDemoShadow, TRUE, TRUE, asFull);
  AppData.Run;
end.
