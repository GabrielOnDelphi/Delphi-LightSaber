program Demo_AutoTranslator;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  LightCom.AppData,
  FormMain in 'FormMain.pas' {MainForm},
  FormTranslEditor in '..\..\FrameVCL\FormTranslEditor.pas' {frmTranslEditor},
  FormTranslSelector in '..\..\FrameVCL\FormTranslSelector.pas' {frmTranslSelector},
  ccAppData in '..\..\ccAppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo Translator');
  AppData.CreateMainForm(TMainForm, MainForm, TRUE, TRUE, asFull);
  AppData.Run;
end.
