program VCL_Demo_CreationOrder;

uses
  VCL.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  LightVcl.Visual.CreationOrderTester in '..\..\..\FrameVCL\LightVcl.Visual.CreationOrderTester.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;    { If true, a taskbar button represents the application's main form and displays its caption. All child forms will stay on top of the MainForm (bad)! If False, a taskbar button represents the application's (hidden) main window and bears the application's Title. Must be True to use Windows (Vista) Aero effects (ive taskbar thumbnails, Dynamic Windows, Windows Flip, Windows Flip 3D). https://stackoverflow.com/questions/66720721/ } 
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
