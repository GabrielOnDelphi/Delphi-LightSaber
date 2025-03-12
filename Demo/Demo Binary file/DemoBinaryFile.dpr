program DemoBinaryFile;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  cbAppData,
  ccINIFile,
  uSoldier in 'uSoldier.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light Demo Binary File');
  AppData.CreateMainForm(TfrmMain, frmMain, TRUE, TRUE, asPosOnly);
  AppData.Run;
end.
