program FMX_TemplateMicro;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  LightFmx.Common.AppData in '..\..\..\FrameFMX\LightFmx.Common.AppData.pas',
  LightFmx.Common.IniFile in '..\..\..\FrameFMX\LightFmx.Common.IniFile.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Template FMX micro');
  AppData.CreateMainForm(TForm1, Form1, TRUE);
  AppData.Run;
end.
