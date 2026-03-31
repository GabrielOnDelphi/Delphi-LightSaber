program FMX_Demo_Styles2.dproj;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {HeaderFooterForm},
  Form2 in 'Form2.pas' {frm2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(THeaderFooterForm, HeaderFooterForm);
  Application.CreateForm(Tfrm2, frm2);
  Application.Run;
end.
