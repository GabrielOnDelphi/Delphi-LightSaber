UNIT SharedUninstaller;

{--------------------------------------------------------------------------------------------------
  HeracleBioSoft
  2023.03.02
  Utility functions for c:\Projects\LightSaber\Demo\VCL\Template Universal Uninstaller\VCL_Uninstaller.dpr

  Note:
    Do not add application specific dependinces/units to the Uses clause because you won't be able to compile the Uninstaller
--------------------------------------------------------------------------------------------------}

{done: the send feedback button should also take user to http://www.Bionixwallpaper.com/help/install/uninstall-reason.html#soft}

INTERFACE
USES
  Winapi.Windows;



IMPLEMENTATION

USES LightCore.Win.Registry, LightCore.AppData, LightVcl.Visual.AppData
;


{ Moved to TAppData.RegisterUninstaller (LightVcl.Visual.AppData.pas) }



end.


