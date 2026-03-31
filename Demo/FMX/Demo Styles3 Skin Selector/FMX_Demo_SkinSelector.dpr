program FMX_Demo_SkinSelector;

uses
  {$IF Defined(MSWINDOWS)}
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  {$ENDIF }
  System.StartUpCopy,
  FMX.Forms,
  FormStylesMain in 'FormStylesMain.pas' {frmSimpleDemo},
  FormSecondary in 'FormSecondary.pas' {frmSecondary},
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightFmx.Common.AppData in '..\..\..\FrameFMX\LightFmx.Common.AppData.pas',
  LightFmx.Common.IniFile in '..\..\..\FrameFMX\LightFmx.Common.IniFile.pas',
  LightFmx.Common.AppData.Form in '..\..\..\FrameFMX\LightFmx.Common.AppData.Form.pas',
  LightFmx.Common.Styles in '..\..\..\FrameFMX\LightFmx.Common.Styles.pas',
  FormSkinsDisk in '..\..\..\FrameFMX\FormSkinsDisk.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light FMX SkinSelector Demo 2');
  AppData.CreateMainForm(TfrmSimpleDemo, frmSimpleDemo, asPosOnly); // Change AutoState from asFull to asNone if you don't want to save form's state to disk.
  AppData.CreateForm(TfrmSecondary, frmSecondary, asPosOnly);
  LoadLastStyle('');
  AppData.Run;
end.
