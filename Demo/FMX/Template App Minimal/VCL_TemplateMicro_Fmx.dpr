program VCL_TemplateMicro_Fmx;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  LightFMX.lbAppData in '..\..\..\FrameFMX\LightFMX.lbAppData.pas',
  LightFMX.lbIniFile in '..\..\..\FrameFMX\LightFMX.lbIniFile.pas',
  LightFMX.lbAppData.Form in '..\..\..\FrameFMX\LightFMX.lbAppData.Form.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Template FMX micro');
  AppData.CreateMainForm(TForm1, Form1, TRUE);
  AppData.Run;
end.
