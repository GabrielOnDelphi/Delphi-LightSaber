program DemoBinary1;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  cbAppDataVCL,
  ccINIFile,
  MainForm1 in 'MainForm1.pas' {frmMain},
  uSoldier_v1 in 'uSoldier_v1.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light Demo Binary 1');
  AppData.CreateMainForm(TfrmMain);
  AppData.Run;
end.
