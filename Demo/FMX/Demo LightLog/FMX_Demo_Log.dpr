program FMX_Demo_Log;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  System.StartUpCopy,
  FMX.Forms,
  FormMain in 'FormMain.pas' {MainForm},
  LightFmx.Common.AppData in '..\..\..\FrameFMX\LightFmx.Common.AppData.pas',
  LightFmx.Common.IniFile in '..\..\..\FrameFMX\LightFmx.Common.IniFile.pas',
  LightCore.INIFile in '..\..\..\LightCore.INIFile.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo FMX Visual Log');
  AppData.CreateMainForm(TMainForm, MainForm, asFull);
  AppData.Run;
end.
