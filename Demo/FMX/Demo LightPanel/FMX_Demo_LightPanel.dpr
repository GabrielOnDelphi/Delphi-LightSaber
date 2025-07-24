program FMX_Demo_LightPanel;

uses
  System.StartUpCopy,
  FMX.Forms,
  FormTest in 'FormTest.pas' {Form2},
  LightFmx.Common.AppData in '..\..\..\FrameFMX\LightFmx.Common.AppData.pas',
  LightFmx.Common.IniFile in '..\..\..\FrameFMX\LightFmx.Common.IniFile.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
