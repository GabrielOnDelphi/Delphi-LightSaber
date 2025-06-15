unit MainForm;

{=============================================================================================================
   2025.04
   www.GabrielMoraru.com
==============================================================================================================

   Demo for c:\Projects\LightSaber\LightCore.PlatformFile.pas
   You will see the results of this program during compilation in the "Messages" panel.

=============================================================================================================}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  LightFMX.lbAppData.Form, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm3 = class(TLightForm)
    Label1: TLabel;
  private
  public
  end;

var
  Form3: TForm3;

implementation {$R *.fmx}

USES
   LightCore.PlatformFile;

end.
