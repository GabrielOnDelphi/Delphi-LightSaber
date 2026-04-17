UNIT FormSystemReport;

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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Memo.Types,
  LightFmx.Common.AppData.Form;

TYPE
  TfrmSystemReport = class(TLightForm)
    Memo: TMemo;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;


VAR frmSystemReport: TfrmSystemReport;  // Unfortunatelly we cannot get rid of this global var under FMX

IMPLEMENTATION
{$R *.fmx}

USES
   LightCore.Reports,
   LightFmx.Common.Platform;


procedure TfrmSystemReport.FormCreate(Sender: TObject);
begin
  Memo.Lines.Clear;
  Memo.Lines.Add('=< CORE REPORT >=');
  Memo.Lines.Add(GenerateCoreReport);
  Memo.Lines.Add('');
  Memo.Lines.Add('=< SCREEN RESOLUTION >=');
  Memo.Lines.Add(GenerateScreenResolutionRep);
end;

end.
