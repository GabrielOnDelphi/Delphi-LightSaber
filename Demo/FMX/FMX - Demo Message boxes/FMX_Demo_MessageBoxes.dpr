program FMX_Demo_MessageBoxes;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  LightFMX.lbAppData in '..\..\..\FrameFMX\LightFMX.lbAppData.pas',
  LightFMX.lbAppData.Form in '..\..\..\FrameFMX\LightFMX.lbAppData.Form.pas',
  LightFMX.lbIniFile in '..\..\..\FrameFMX\LightFMX.lbIniFile.pas',
  ccINIFile in '..\..\..\ccINIFile.pas',
  LightFMX.lbDialogs in '..\..\..\FrameFMX\LightFMX.lbDialogs.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo FMX Messages');
  AppData.CreateMainForm(TForm1, Form1, TRUE);
  AppData.Run;
end.
