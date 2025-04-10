program Demo_SystemReport;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  cbAppDataVCL in '..\..\cbAppData.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Orinoco Reader', 'Orinoco', FALSE);
  AppData.CreateMainForm(TfrmMain, frmMain, True, True);
  //TfrmRamLog.CreateGlobalLog;
  AppData.Run;
end.
