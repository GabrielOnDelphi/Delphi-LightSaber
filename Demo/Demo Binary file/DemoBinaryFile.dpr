program DemoBinaryFile;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  cbAppDataVCL,
  ccINIFile,
  MainForm in 'MainForm.pas' {frmMain},
  uSoldier in 'uSoldier.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light Demo Binary File');
  AppData.CreateMainForm(TfrmMain, frmMain, TRUE, TRUE, asPosOnly);
  AppData.Run;
end.
