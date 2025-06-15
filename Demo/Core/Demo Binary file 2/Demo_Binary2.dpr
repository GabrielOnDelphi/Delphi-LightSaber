program Demo_Binary2;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  LightVcl.Common.AppData,
  ccINIFile,
  MainForm2 in 'MainForm2.pas' {frmMain},
  uSoldier_v2 in 'uSoldier_v2.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light Demo Binary 2');
  AppData.CreateMainForm(TfrmMain);
  AppData.Run;
end.
