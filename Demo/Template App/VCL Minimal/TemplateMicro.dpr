program TemplateMicro;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  MainForm in 'MainForm.pas' {frmMain},
  cbAppDataVCL,
  ccINIFile;

{$R *.res}

begin
  AppData:= TAppData.Create('Light Template Micro');
  AppData.CreateMainForm(TfrmMain, TRUE, TRUE, asFull);
  AppData.Run;
end.
