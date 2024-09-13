program DemoLog;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  WinApi.Windows,
  VCL.Forms,
  cbAppData in '..\..\cbAppData.pas',
  Unit3 in 'Unit3.pas' {MainForm},
  FormLog in '..\..\FormLog.pas',
  cvINIFile in '..\..\cvINIFile.pas',
  cbDialogs in '..\..\cbDialogs.pas',
  cbIniFile in '..\..\cbIniFile.pas',
  Log in '..\..\VisLog\Log.pas',
  ccINIFile in '..\..\ccINIFile.pas',
  ccIO in '..\..\ccIO.pas',
  ccBinary in '..\..\ccBinary.pas',
  ccColors in '..\..\ccColors.pas',
  ccCompiler in '..\..\ccCompiler.pas',
  ccCore in '..\..\ccCore.pas',
  ccStreamBuff in '..\..\ccStreamBuff.pas',
  ccStreamBuff2 in '..\..\ccStreamBuff2.pas',
  ccStreamFile in '..\..\ccStreamFile.pas',
  ccStreamMem in '..\..\ccStreamMem.pas';

{$R *.res}
{$WARN DUPLICATE_CTOR_DTOR OFF}    {Silence the: W1029 Duplicate constructor with identical parameters will be inacessible from C++. See: https://marc.durdin.net/2012/05/delphi-xe2s-hidden-hints-and-warnings-options/ }

begin
  AppData:= TAppData.Create('Light Commercial Template');
  AppData.CreateMainForm(TMainForm, MainForm, TRUE);
  Application.Run;
end.
