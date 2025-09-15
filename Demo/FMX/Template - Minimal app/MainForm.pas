unit MainForm;

interface

uses
  System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Controls.Presentation, FMX.StdCtrls, FMX.DialogService, FMX.Grid,
  LightFmx.Common.AppData.Form, LightFmx.Common.LogViewer;

TYPE
  TForm1 = class(TLightForm)
    chkSome: TCheckBox;
    RadioButton1: TRadioButton;
    lblInfoTop: TLabel;
    lblIniFile: TLabel;
    procedure FormCreate  (Sender: TObject);
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
  inherited; // This will load the form's state from disk
  
  // Now we can show something in the log by sending the message directly to AppData object
  // The information will not be shown on the screeen until we assign a visual log to show it, as demonstrated in the next demo app.
  AppData.LogError('Some error.');
  lblIniFile.Text:= 'Your INI file is here: ' + CRLF+ AppData.IniFile;  
end;


procedure TForm1.FormPreRelease;
begin
  Caption:= 'Bye Bye!'; // This place is a better than FormClose/FormDestroy to deinitialize code.
  inherited;
end;


end.
