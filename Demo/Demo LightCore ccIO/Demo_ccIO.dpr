program Demo_ccIO;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  MainForm in 'MainForm.pas' {frmTestIO},
  cbAppDataVCL;

{$R *.res}

begin
  AppData:= TAppData.Create('Light Tester IO');
  AppData.CreateMainForm(TfrmTestIO, frmTestIO, TRUE);
  AppData.Run;
end.
