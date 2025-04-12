program DemoLog;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  cbAppDataVCL,
  FormMain in 'FormMain.pas' {MainForm},
  cvLog in '..\..\FrameVCL\cvLog.pas',
  ccINIFile in '..\..\FrameVCL\ccINIFile.pas';

{$R *.res}
{$WARN DUPLICATE_CTOR_DTOR OFF}    {Silence the: W1029 Duplicate constructor with identical parameters will be inacessible from C++. See: https://marc.durdin.net/2012/05/delphi-xe2s-hidden-hints-and-warnings-options/ }

begin
  AppData:= TAppData.Create('Light Commercial Template');
  AppData.CreateMainForm(TMainForm, MainForm, TRUE, TRUE, asFull);
  AppData.Run;
end.
