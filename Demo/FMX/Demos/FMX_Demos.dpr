program FMX_Demos;

uses
  {$IF Defined(MSWINDOWS)}
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  {$ENDIF }
  System.StartUpCopy,
  FMX.Forms,
  FormTest in 'FormTest.pas' {Form2},
  FormSkinsDisk in '..\..\..\FrameFMX\FormSkinsDisk.pas' {frmStyleDisk},
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightFmx.Common.AppData in '..\..\..\FrameFMX\LightFmx.Common.AppData.pas',
  LightFmx.Common.IniFile in '..\..\..\FrameFMX\LightFmx.Common.IniFile.pas',
  LightFmx.Visual.SvgFlatButton in '..\..\..\FrameFMX\LightFmx.Visual.SvgFlatButton.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('FMX_Demos');
  LoadLastStyle;
  AppData.CreateMainForm(TForm2, Form2, asPosOnly);
  AppData.Run;
end.
