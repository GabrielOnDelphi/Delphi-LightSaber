program FMX_Demo_Styles;

uses
  {$IF Defined(MSWINDOWS)}
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  {$ENDIF }
  System.StartUpCopy,
  FMX.Forms,
  FormStylesMain in 'FormStylesMain.pas' {frmSimpleDemo},
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightFmx.Common.AppData in '..\..\..\FrameFMX\LightFmx.Common.AppData.pas',
  LightFmx.Common.IniFile in '..\..\..\FrameFMX\LightFmx.Common.IniFile.pas',
  LightFmx.Common.AppData.Form in '..\..\..\FrameFMX\LightFmx.Common.AppData.Form.pas',
  FormSecondary in 'FormSecondary.pas' {frmSecondary},
  DataModule in 'DataModule.pas' {datMod: TDataModule},
  LightFmx.Common.Styles in '..\..\..\FrameFMX\LightFmx.Common.Styles.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light FMX Styles Demo');
  AppData.CreateMainForm(TfrmSimpleDemo, frmSimpleDemo, asFull); // Change AutoState from asFull to asNone if you don't want to save form's state to disk.
  AppData.CreateForm(TfrmSecondary, frmSecondary, asFull);
  AppData.Run;
end.
