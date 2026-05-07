program VCL_Demo_CreationOrder;

uses
  VCL.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  LightVcl.Visual.CreationOrderTester in '..\..\..\FrameVCL\LightVcl.Visual.CreationOrderTester.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  LightVcl.Visual.AppDataForm in '..\..\..\FrameVCL\LightVcl.Visual.AppDataForm.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light Demo Creation Order');
  AppData.CreateMainForm(TForm1, Form1, TRUE, TRUE, asPosOnly);
  AppData.Run;
end.
