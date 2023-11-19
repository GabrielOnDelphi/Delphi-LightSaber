program Tester_ccIO;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form6},
  ccAppData in '..\..\..\ccAppData.pas',
  ccBinary in '..\..\..\ccBinary.pas',
  ccCompiler in '..\..\..\ccCompiler.pas',
  ccCore in '..\..\..\ccCore.pas',
  ccINIFile in '..\..\..\ccINIFile.pas',
  ccIniFileVcl in '..\..\..\ccIniFileVcl.pas',
  ccIO in '..\..\..\ccIO.pas',
  ccStreamBuff in '..\..\..\ccStreamBuff.pas',
  ccWinVersion in '..\..\..\ccWinVersion.pas';

{$R *.res}

begin
  Application.Initialize;
  AppData:= TAppData.Create('Cubic Tester IO');
  AppData.CreateMainForm(TForm6, Form6, TRUE);
  Application.Run;
end.
