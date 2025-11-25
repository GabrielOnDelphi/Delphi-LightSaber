program FMX_Demo_MessageBoxes;

uses
  {$IF Defined(MSWINDOWS)}
   {$IFDEF DEBUG}
    FastMM4,
   {$ENDIF }
  {$ENDIF }
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  LightFmx.Common.AppData in '..\..\..\FrameFMX\LightFmx.Common.AppData.pas',
  LightFmx.Common.IniFile in '..\..\..\FrameFMX\LightFmx.Common.IniFile.pas',
  LightCore.INIFile in '..\..\..\LightCore.INIFile.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo FMX Messages');
  AppData.CreateMainForm(TForm1, Form1, asPosOnly);
  AppData.Run;
end.
