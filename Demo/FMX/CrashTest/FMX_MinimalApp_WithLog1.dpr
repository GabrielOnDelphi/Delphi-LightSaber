program FMX_MinimalApp_WithLog1;

uses
  {$IF Defined(MSWINDOWS)}
   {$IFDEF DEBUG}
    FastMM4,
   {$ENDIF }
  {$ENDIF }
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  LightCore.ExceptionLogger in '..\..\..\LightCore.ExceptionLogger.pas',
  LightCore.AppData         in '..\..\..\LightCore.AppData.pas',
  LightFmx.Common.AppData   in '..\..\..\FrameFMX\LightFmx.Common.AppData.pas',
  LightFmx.Common.IniFile   in '..\..\..\FrameFMX\LightFmx.Common.IniFile.pas';

{$R *.res}

begin
  InstallExceptionLogger('CrashTest-Exceptions.log');   // FIRST line: every raise -> Documents\ (Android: run-as <pkg> cat files\CrashTest-Exceptions.log)

  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('CrashTest');
  AppData.CreateMainForm(TForm1, Form1, asFull);
  AppData.Run;
end.
