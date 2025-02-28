unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  cbAppDataFmxForm, ccIniFile;

TYPE
  TForm1 = class(TLightForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
  private
  public
   procedure FormInitialize; override;
  end;

VAR
  Form1: TForm1;

IMPLEMENTATION
{$R *.fmx}


procedure TForm1.FormInitialize;
begin
  AutoState:= asFull;  // Must be before inherited!
  inherited;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SaveForm;
end;

end.
