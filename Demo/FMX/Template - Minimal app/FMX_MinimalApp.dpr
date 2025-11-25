program FMX_MinimalApp;

uses
  {$IF Defined(MSWINDOWS)}
   {$IFDEF DEBUG}
    FastMM4,
   {$ENDIF }
  {$ENDIF }
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {frmSimpleDemo},
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightFmx.Common.AppData in '..\..\..\FrameFMX\LightFmx.Common.AppData.pas',
  LightFmx.Common.IniFile in '..\..\..\FrameFMX\LightFmx.Common.IniFile.pas',
  LightFmx.Common.AppData.Form in '..\..\..\FrameFMX\LightFmx.Common.AppData.Form.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Template FMX micro');
  AppData.CreateMainForm(TfrmSimpleDemo, frmSimpleDemo, asFull); // Change AutoState from asFull to asNone if you don't want to save form's state to disk.
  AppData.Run;
end.
