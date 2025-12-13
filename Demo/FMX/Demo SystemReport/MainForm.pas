UNIT MainForm;

{=============================================================================================================
   2025.12
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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Controls.Presentation,
  LightFmx.Common.AppData.Form, FMX.ScrollBox, FMX.Memo, FMX.Memo.Types;

TYPE
  TfrmSimpleDemo = class(TLightForm)
    Memo: TMemo;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;


VAR frmSimpleDemo: TfrmSimpleDemo;  // Unfortunatelly we cannot get rid of this global var under FMX

IMPLEMENTATION
{$R *.fmx}

USES
   LightCore.Reports;


procedure TfrmSimpleDemo.FormCreate(Sender: TObject);
begin
  Memo.Lines.Clear;
  Memo.Lines.Add('=< CORE REPORT >=');
  Memo.Lines.Add(GenerateCoreReport);
end;

end.
