program FMX_Demo_Log;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  LightFmx.Common.AppData in '..\..\..\FrameFMX\LightFmx.Common.AppData.pas',
  LightFmx.Common.IniFile in '..\..\..\FrameFMX\LightFmx.Common.IniFile.pas',
  LightFmx.Common.AppData.Form in '..\..\..\FrameFMX\LightFmx.Common.AppData.Form.pas',
  LightFmx.Common.LogFilter in '..\..\..\FrameFMX\LightFmx.Common.LogFilter.pas',
  LightFmx.Common.LogViewer in '..\..\..\FrameFMX\LightFmx.Common.LogViewer.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo Visual Log');
  AppData.CreateMainForm(TForm1, Form1, TRUE);
  AppData.Run;
end.
