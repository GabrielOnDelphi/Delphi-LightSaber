program FMX_Demo_LightPanel;

USES
  {$IF Defined(MSWINDOWS)}
   {$IFDEF DEBUG}
    FastMM4,
   {$ENDIF }
  {$ENDIF }
  System.StartUpCopy,
  FMX.Forms,
  FormTest in 'FormTest.pas' {Form2},
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightFmx.Common.AppData in '..\..\..\FrameFMX\LightFmx.Common.AppData.pas',
  LightFmx.Common.IniFile in '..\..\..\FrameFMX\LightFmx.Common.IniFile.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
