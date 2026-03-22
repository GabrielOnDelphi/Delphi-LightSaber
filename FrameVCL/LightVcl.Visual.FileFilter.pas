UNIT LightVcl.Visual.FileFilter;

{=============================================================================================================
   2026.03
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Allows access to Change method which I need to call after I load the controls from IniFile
-----------------------------------------------------------------------------------------------------------------------}

INTERFACE  {$WARN UNIT_PLATFORM OFF}

USES
   System.Classes, vcl.filectrl;

TYPE
  TLightFilterBox = class(TFilterComboBox)
  public
    procedure Change; override;
  end;

procedure Register;



IMPLEMENTATION



procedure TLightFilterBox.Change;
begin
  inherited;
end;


procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TLightFilterBox]);
end;


end.
