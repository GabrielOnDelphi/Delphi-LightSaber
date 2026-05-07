program VCL_Demo_PlatformDetection;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form3},
  LightCore.IOPlatformFile in '..\..\..\LightCore.IOPlatformFile.pas',
  PlatformTest in '..\PlatformTest.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  LightVcl.Visual.AppDataForm in '..\..\..\FrameVCL\LightVcl.Visual.AppDataForm.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light Demo VCL PlatformDetection');
  AppData.CreateMainForm(TForm3, Form3, TRUE, TRUE, asPosOnly);
  AppData.Run;
end.
