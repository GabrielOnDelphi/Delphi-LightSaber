program FMX_MinimalApp_WithLog2;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightFmx.Common.AppData in '..\..\..\FrameFMX\LightFmx.Common.AppData.pas',
  LightFmx.Common.IniFile in '..\..\..\FrameFMX\LightFmx.Common.IniFile.pas',
  LightFmx.LogForm in '..\..\..\FrameFMX\LightFmx.LogForm.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Template FMX mini Log2');
  AppData.CreateMainForm(TForm1, Form1, asFull);  // Change AutoState from asFull to asNone if you don't want to save form's state to disk.
  AppData.Run;
end.
