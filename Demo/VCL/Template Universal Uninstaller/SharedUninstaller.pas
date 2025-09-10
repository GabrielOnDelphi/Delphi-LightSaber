UNIT SharedUninstaller;

{--------------------------------------------------------------------------------------------------
  HeracleBioSoft
  2023.03.02
  Utility functions for c:\MyProjects\Project support\Universal Uninstaller\Uninstaller.dpr

  Note:
    Do not add application specific dependinces/units to the Uses clause because you won't be able to compile the Uninstaller
--------------------------------------------------------------------------------------------------}

{done: the send feedback button should also take user to http://www.Bionixwallpaper.com/help/install/uninstall-reason.html#soft}

INTERFACE
USES
  Winapi.Windows;



IMPLEMENTATION

USES LightVcl.Common.Registry, LightCore.AppData, LightVcl.Common.AppData
;


{moved to

TAppData.AddUninstallerToCtrlPanel
TAppData.RegisterUninstaller;

  }



end.


