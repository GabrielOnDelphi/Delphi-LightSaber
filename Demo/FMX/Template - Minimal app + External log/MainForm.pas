UNIT MainForm;

{=============================================================================================================
   2025.09
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   By deriving your forms from TLightForm they gain the ability to save to disk their:
     * size
     * position
     * controls (checkboxes, radiobuttons, etc)

   When the application starts again, all the above properties are restored.
   As you can see this is done automagically. The user has to write zero code for that!

--------------------------------------------------------------------------------------------------------------
   More documentation in LightSaber\FrameFMX\LightFmx.Common.AppData.Form.pas
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Controls.Presentation, FMX.StdCtrls, FMX.DialogService, 
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
    procedure FormPreRelease;  override;
  end;

VAR
  Form1: TForm1;

IMPLEMENTATION
{$R *.fmx}

USES
   LightCore, LightCore.Time, LightCore.Types, LightCore.AppData, LightCore.LogTypes, LightCore.LogRam, LightFmx.Common.AppData;



procedure TForm1.FormCreate(Sender: TObject);
begin
  lblIniFile.Text:= 'Your form''s settings are saved here: ' + CRLF+ AppData.IniFile;
  lblIniFile.Position.Y:= 999999999999;  // Move it to the bottom

  // Show something in the log by sending the message directly to AppData object. The information will not be shown on the screeen until we assign a visual log to show it.
  AppData.LogWarn('Warning! A magic application is starting now!');
end;


procedure TForm1.FormPreRelease;
begin
  Caption:= 'Bye Bye!'; // This place is a better than FormClose/FormDestroy to do some clean up. It is guaranteed to be called only once!
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





