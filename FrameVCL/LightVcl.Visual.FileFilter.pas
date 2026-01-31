UNIT LightVcl.Visual.FileFilter;

{=============================================================================================================
   Gabriel Moraru
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------

  Allows access to Change method which I need to call after I load the controls from IniFile
-----------------------------------------------------------------------------------------------------------------------}

INTERFACE  {.$WARN UNIT_PLATFORM OFF}

USES
   System.Classes, vcl.filectrl;

TYPE
  TCubicFilterBox = class(TFilterComboBox)
  public
    procedure Change; override;
  end;

procedure Register;




IMPLEMENTATION




procedure TCubicFilterBox.Change;
begin
  inherited;
end;


procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TCubicFilterBox]);
end;


end.
