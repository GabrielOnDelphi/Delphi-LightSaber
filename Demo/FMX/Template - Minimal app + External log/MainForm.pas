unit MainForm;

interface

uses
  System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Controls.Presentation, FMX.StdCtrls, FMX.DialogService, FMX.Grid,
  LightFmx.Common.AppData.Form, LightFmx.Common.LogViewer;

TYPE
  TForm1 = class(TLightForm)
    btnShowErr: TButton;
    chkSome: TCheckBox;
    RadioButton1: TRadioButton;
    lblInfoTop: TLabel;
    lblIniFile: TLabel;
    procedure FormCreate  (Sender: TObject);
    procedure btnShowErrClick(Sender: TObject);
  private
  public
    procedure FormPostInitialize; override;
    procedure FormPreRelease;     override;
  end;

VAR
  Form1: TForm1;

IMPLEMENTATION
{$R *.fmx}

USES
   LightCore, LightCore.Time, LightCore.Types, LightCore.AppData, LightCore.LogTypes, LightCore.LogRam, LightFmx.Common.AppData;



procedure TForm1.FormCreate(Sender: TObject);
begin
  // FormPostInitialize is a much better place to initialize your code!
end;


procedure TForm1.FormPostInitialize;
begin
  inherited; // Let the parent to its own initialization

  lblIniFile.Text:= 'Your INI file is here: ' + CRLF+ AppData.IniFile;
  lblIniFile.Position.Y:= 999999999999;  // Move it to the bottom

  // Now we can show something in the log...
  AppData.LogError('Existing error');   //.. but it will not be shown until we have a visual control to show the data in the RAM Log.
end;


procedure TForm1.FormPreRelease;
begin
  Caption:= 'Bye Bye!'; // This place is a better than FormClose/FormDestroy to deinitialize code.
  inherited;
end;


procedure TForm1.btnShowErrClick(Sender: TObject);
begin
  VAR FormLog:= AppData.FormLog;   // This first call to the form, creates the form
  FormLog.Caption:= 'Logger';      // ...then we can do whatever we want with the form

  AppData.LogError('New error');

  //AppData.RamLog.ShowOnError:= FALSE;   // Set this to False if you don't want the Log Form to auto pop-up
end;

end.





