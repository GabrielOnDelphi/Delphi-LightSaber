program FMX_Demo_Log;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  System.StartUpCopy,
  FMX.Forms,
  FormMain in 'FormMain.pas' {MainForm},
  LightFmx.Common.AppData in '..\..\..\FrameFMX\LightFmx.Common.AppData.pas',
  LightFmx.Common.IniFile in '..\..\..\FrameFMX\LightFmx.Common.IniFile.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo FMX Visual Log');
  AppData.CreateMainForm(TMainForm, MainForm, TRUE);
  AppData.Run;
end.
