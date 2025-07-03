program FMX_Demo_Log;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  System.StartUpCopy,
  FMX.Forms,
  LightCore.INIFile in '..\..\LightCore.INIFile.pas',
  LightCore.AppData in '..\..\LightCore.AppData.pas',

  FormMain in 'FormMain.pas' {MainForm},
  LightFmx.Common.AppData in '..\..\..\FrameFMX\LightFmx.Common.AppData.pas',
  LightFmx.Common.IniFile in '..\..\..\FrameFMX\LightFmx.Common.IniFile.pas',
  LightFmx.Common.AppData.Form in '..\..\..\FrameFMX\LightFmx.Common.AppData.Form.pas',
  LightFmx.Common.LogFilter in '..\..\..\FrameFMX\LightFmx.Common.LogFilter.pas',
  LightFmx.Common.LogViewer in '..\..\..\FrameFMX\LightFmx.Common.LogViewer.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo FMX Visual Log');
  AppData.CreateMainForm(TMainForm, MainForm, TRUE);
  AppData.Run;
end.
