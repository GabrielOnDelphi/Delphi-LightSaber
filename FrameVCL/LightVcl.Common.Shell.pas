UNIT LightVcl.Common.Shell;

{=============================================================================================================
   2026.09.30
   www.GabrielMoraru.com

--------------------------------------------------------------------------------------------------------------

   Shell utilities:
     * Show the file properties dialog of Windows Explorer
     * Start menu tools
     * Install an INF file (Windows may then ask the user to restart the computer)
     * Extract the first icon of an exe, DLL or icon file

   See also:
     LightCore.Shell.pas          - the taskbar, the program associated with a file type, the list of recent files of the taskbar button, IsApiFunctionAvailable
     LightCore.Win.Shell.pas      - file associations, shortcuts (.lnk), the context menu, the 'Show Desktop' file, the uninstaller entry

   In this group:
     * LightVcl.Common.Shell.pas
     * LightVcl.Common.System.pas
     * LightVcl.Common.Window.pas
     * LightVcl.Common.WindowMetrics.pas
     * LightVcl.Common.ExecuteProc.pas
     * LightVcl.Common.ExecuteShell.pas
     * LightCore.Process.pas

=============================================================================================================}

INTERFACE  
USES
   Winapi.Windows, Winapi.ShellAPI, Winapi.Messages,
   Vcl.Forms;
 

{--------------------------------------------------------------------------------------------------
   CONTEXT MENU
-------------------------------------------------------------------------------------------------}
 procedure InvokePropertiesDialog(CONST FileName: string);                                          { Shows the standard file properties dialog like in Windows Explorer }


{--------------------------------------------------------------------------------------------------
   START MENU
--------------------------------------------------------------------------------------------------}
 procedure InvokeStartMenu(Form: TForm);                                                            { Activate Windows Start button from code  }


 function  InstallINF    (CONST PathName: string; hParent: HWND): Boolean;                          { Example: InstallINF('C:\driver.inf', 0) }


{--------------------------------------------------------------------------------------------------
   SHELL API
--------------------------------------------------------------------------------------------------}
 function  ExtractIconFromFile(IcoFileName: String): THandle;                                       { Extract icon from file  }


 
IMPLEMENTATION

 



{--------------------------------------------------------------------------------------------------
   SHELL INVOKE
--------------------------------------------------------------------------------------------------}

{ Displays the standard Windows file properties dialog (same as right-click > Properties in Explorer) }
procedure InvokePropertiesDialog(CONST FileName: string);
VAR sei: TShellExecuteInfo;
begin
  Assert(FileName <> '', 'FileName cannot be empty!');

  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize:= SizeOf(sei);
  sei.lpFile:= PChar(FileName);
  sei.lpVerb:= 'properties';
  sei.fMask := SEE_MASK_INVOKEIDLIST;
  ShellExecuteEx(@sei);
end;


{ Programmatically opens the Windows Start menu }
procedure InvokeStartMenu(Form: TForm);
begin
 Assert(Form <> NIL, 'Form cannot be nil!');
 SendMessage(Form.Handle, WM_SYSCOMMAND, SC_TASKLIST, 0);
end;




{ Installs a Windows INF file using the setup API.
  PathName: Full path to the .inf file.
  hParent: Parent window handle (use 0 for no parent).
  Returns TRUE if installation was initiated successfully. }
function InstallINF(CONST PathName: string; hParent: HWND): Boolean;
VAR instance: HINST;
begin
 Assert(PathName <> '', 'PathName cannot be empty!');

 instance:= ShellExecute(hParent, PChar('open'), PChar('rundll32.exe'), PChar('setupapi,InstallHinfSection DefaultInstall 132 ' + PathName), NIL, SW_HIDE);
 Result:= instance > 32;
end;


{--------------------------------------------------------------------------------------------------
   ICON
--------------------------------------------------------------------------------------------------}
{ Extracts the first icon from an executable, DLL, or icon file.
  IcoFileName: Path to the file containing the icon.
  Returns: Handle to the icon, or 0 if extraction failed.

  Usage example:
    var MyIcon: TIcon;
    if ExtractIconFromFile('app.exe') > 0 then
    begin
      MyIcon:= TIcon.Create;
      MyIcon.Handle:= ExtractIconFromFile('app.exe');
      Image1.Picture.Icon:= MyIcon;
      FreeAndNil(MyIcon);
    end; }
function ExtractIconFromFile(IcoFileName: String): THandle;
begin
 Assert(IcoFileName <> '', 'IcoFileName cannot be empty!');
 Result:= ExtractIcon(Application.Handle, PChar(IcoFilename), Word(0));  { Index 0 = first icon }
end;



end.
