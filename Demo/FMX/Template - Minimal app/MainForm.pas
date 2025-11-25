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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Controls.Presentation, FMX.StdCtrls,
  LightFmx.Common.AppData.Form;

TYPE
  TfrmSimpleDemo = class(TLightForm)  // This form derives from TLightForm (which does all the magic)
    chkSome: TCheckBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    lblInfoTop: TLabel;
    lblIniFile: TLabel;
    procedure FormCreate  (Sender: TObject);
  private
  public
  end;


VAR frmSimpleDemo: TfrmSimpleDemo;  // Unfortunatelly we cannot get rid of this global var under FMX

IMPLEMENTATION
{$R *.fmx}

USES
   LightFmx.Common.AppData;


procedure TfrmSimpleDemo.FormCreate(Sender: TObject);
begin
  lblIniFile.Text:= 'Your form''s settings are saved here: ' + AppData.IniFile;  // Indeed they are...
end;


end.
