program Demo_Binary3;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  LightCom.AppData,
  ccINIFile,
  MainForm3 in 'MainForm3.pas' {frmMain},
  uSoldier_v3 in 'uSoldier_v3.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light Demo Binary 2');
  AppData.CreateMainForm(TfrmMain);
  AppData.Run;
end.
