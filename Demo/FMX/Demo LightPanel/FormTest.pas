UNIT FormTest;

{=============================================================================================================
   2025.04
   www.GabrielMoraru.com
==============================================================================================================

   In FMX, unlike the VCL, the Visible property affects both design time and runtime when set in the Object Inspector.
   That’s why setting Visible:= False in FMX hides components in the Form Designer, which is a big inconvenience.

   Solution:
     In the Form Designer, my TLightPanel component is always visible so you can edit it,
     regardless of the VisibleAtRuntime value.
     Set VisibleAtRuntime to false to set the Visible to False at runtime (making the component invisible).

=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Types, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  LightFmx.Visual.Panel;

TYPE
  TForm2 = class(TForm)
    LightPanel1: TLightPanel;
    LightPanel2: TLightPanel;
    lblVisible: TLabel;
    Label1: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  public
  end;

var
  Form2: TForm2;

IMPLEMENTATION
{$R *.fmx}


procedure TForm2.FormCreate(Sender: TObject);
begin
  Caption:= 'LightPanel2.Visible is '+ BoolToStr(LightPanel2.Visible, TRUE);
end;


procedure TForm2.Button1Click(Sender: TObject);
begin
  LightPanel2.Visible:= TRUE;
end;


end.
