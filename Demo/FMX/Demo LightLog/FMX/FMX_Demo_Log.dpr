program FMX_Demo_Log;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  LightFMX.lbAppData in '..\..\..\FrameFMX\LightFMX.lbAppData.pas',
  LightFMX.lbIniFile in '..\..\..\FrameFMX\LightFMX.lbIniFile.pas',
  LightFMX.lbAppData.Form in '..\..\..\FrameFMX\LightFMX.lbAppData.Form.pas',
  LightFMX.lbLogFilter in '..\..\..\FrameFMX\LightFMX.lbLogFilter.pas',
  LightFmx.lbLogViewer in '..\..\..\FrameFMX\LightFmx.lbLogViewer.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo Visual Log');
  AppData.CreateMainForm(TForm1, Form1, TRUE);
  AppData.Run;
end.
