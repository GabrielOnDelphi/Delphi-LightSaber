program VCL_Demo_WinVers;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  FormMain in 'FormMain.pas' {frmTester},
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  Vcl.Forms;

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Tester WinVer');
  Application.MainFormOnTaskbar:= TRUE;
  AppData.CreateMainForm(TfrmTester, asFull);
  AppData.Run;
end.
