program Demo_Log;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  FormMain in 'FormMain.pas' {MainForm},
  ccINIFile in '..\..\ccINIFile.pas',
  llRichLog in '..\..\FrameVCL\llRichLog.pas',
  LightCom.AppData in '..\..\FrameVCL\LightCom.AppData.pas',
  ccAppData in '..\..\ccAppData.pas';

{$R *.res}
{$WARN DUPLICATE_CTOR_DTOR OFF}    {Silence the: W1029 Duplicate constructor with identical parameters will be inacessible from C++. See: https://marc.durdin.net/2012/05/delphi-xe2s-hidden-hints-and-warnings-options/ }

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo Logging System');
  AppData.CreateMainForm(TMainForm, MainForm, TRUE, TRUE, asFull);
  AppData.Run;
end.
