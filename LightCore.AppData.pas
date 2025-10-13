UNIT LightCore.AppData;

{=============================================================================================================
   2025.03
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   FEATURES

     TAppDataCore:
       - Get application's %appdata% folder (the folder where you save temporary, app-related and ini files)
       - Get application's command line parameters
       - Force single instance (allow only one instance of your program to run). Second inst sends its command line to the first inst then shuts down
       - Detect if the application is running for the first time on this computer
       - Application self-restart
       - Application self-delete

    Cross-platform ready: stamped 2025.10
 ____________________________________________________________________________________________________________

   HOW TO USE IT

     In the DPR file of your console app replace the code with:

       program MyConsole;
       uses
         FastMM4,
         LightCore.AppData;
       begin
         AppDataCore:= TAppDataCore.Create('MyCollApp');
         AppDataCore.Run;
       end.

     For GUI projects see LightVcl.Common.AppData.pas

=============================================================================================================}

INTERFACE

USES
   System.IOUtils, System.AnsiStrings, System.SysUtils,
   LightCore, LightCore.Types, LightCore.INIFile, LightCore.LogRam;

TYPE
  THintType = (htOff,                      // Turn off the embedded help system
               htTooltips,                 // Show help as tool-tips
               htStatBar);                 // Show help in status bar

TYPE
  TAutoState = (asUndefined,
                asNone,                    // Don't save the form automatically.
                asPosOnly,                 // Restore form position
                asFull);                   // Restore form position and GUI elements

  TAppDataCore= class(TObject)
  private
    FShowOnError: Boolean;                 // Automatically show the visual log form when warnings/errors are added to the log. This way the user is informed about the problems.
    FHideHint   : Integer;
    FLastFolder : string;
    FSingleInstClassName: string;          // Used by the Single Instance mechanism.   {Old name: AppWinClassName }
    FRunningFirstTime: Boolean;
    CONST
      Signature: AnsiString= 'AppDataSettings'; { Do not change it! }
      DefaultHomePage= 'https://www.GabrielMoraru.com';
    class VAR FCreated: Boolean;           // Sanity check. Make sure the user did not created this obj twice
    class VAR FAppName: string;
    function  getLastUsedFolder: string;
    procedure setShowOnError(const Value: Boolean);
  protected
    FHintType: THintType;                // Turn off the embedded help system
    procedure setHideHint(const Value: Integer); virtual;
    procedure loadSettings;     virtual;
    procedure saveSettings;     virtual;
    procedure defaultSettings;  virtual;
    procedure setHintType(const Value: THintType); virtual; abstract;
  public
    RamLog: TRamLog;

    { Product details }
    CompanyName    : string;    // Optional. Used by the 'About' form
    CompanyHome    : WebURL;    // Company's home page
    ProductHome    : WebURL;    // Product's home page
    ProductOrder   : WebURL;    // Product's order page
    ProductSupport : WebURL;    // Product's user manual
    ProductWelcome : WebURL;    // Product's welcome page (gives a short introduction on how the product works).
    ProductUninstal: WebURL;    // Ask user why they uninstalled the product (feedback)

   {--------------------------------------------------------------------------------------------------
      App Single Instance
   --------------------------------------------------------------------------------------------------}
    property  SingleInstClassName: string read FSingleInstClassName;
    {}
    class VAR Initializing: Boolean;                 // See documentation at the top of the file

    constructor Create(CONST aAppName: string; CONST WindowClassName: string= ''; MultiThreaded: Boolean= FALSE); virtual;
    procedure  AfterConstruction; override;
    destructor Destroy; override;                    // This is called automatically by "Finalization" in order to call it as late as possible }

   {--------------------------------------------------------------------------------------------------
      User settings
   --------------------------------------------------------------------------------------------------}
    var
      UserPath     : string;           // User defined path where to save (large) files. Useful when the program needs to save large amounts of data that we don't want to put on a SSD drive.
      AutoStartUp  : Boolean;          // Start app at Windows startup

    var
      // These are here so they can be shared with the VCL/FMX variant of AppData
      StartMinim   : Boolean;          // Start minimized. Remmbers application's last state (it was minimized or not)
      Minimize2Tray: Boolean;          // Minimize to tray
      Opacity      : Integer;          // Form opacity

   {--------------------------------------------------------------------------------------------------
      App path/name
   --------------------------------------------------------------------------------------------------}
    function ExeFolder: string;        // The folder where the exe file is located. Old name: AppDir
    function SysDir: string;
    function CheckSysDir: Boolean;
    class function IniFile: string;
    class function AppDataFolder(ForceDir: Boolean= FALSE): string;
    class function AppDataFolderAllUsers(ForceDir: Boolean = FALSE): string;

    function AppShortName:  string;
    property LastUsedFolder: string read getLastUsedFolder write FLastFolder;

    class property AppName:  string read FAppName;
    property HideHint: Integer read FHideHint write setHideHint;       // Hide hint after x ms. Does nothing here. The child class mush override this
    property HintType: THintType  read FHintType write setHintType;         // Turn off the embedded help system

   {--------------------------------------------------------------------------------------------------
      App Control
   --------------------------------------------------------------------------------------------------}
    procedure Minimize; virtual; abstract;
    property  RunningFirstTime: Boolean read FRunningFirstTime;    // Returns true if the application is running for the first time on this computer.

   {--------------------------------------------------------------------------------------------------
      BetaTester tools
   --------------------------------------------------------------------------------------------------}
    class function RunningHome: Boolean;
    function  BetaTesterMode: Boolean;
    function  IsHardCodedExp(Year, Month, Day: word): Boolean;

   {--------------------------------------------------------------------------------------------------
      App Log
   --------------------------------------------------------------------------------------------------}
    procedure LogEmptyRow;
    procedure LogBold  (CONST Msg: string);
    procedure LogError (CONST Msg: string);
    procedure LogHint  (CONST Msg: string);
    procedure LogImpo  (CONST Msg: string);
    procedure LogInfo  (CONST Msg: string);
    procedure LogMsg   (CONST Msg: string);
    procedure LogVerb  (CONST Msg: string);
    procedure LogWarn  (CONST Msg: string);
    procedure LogClear;

    procedure PopUpLogWindow;

    property  ShowLogOnError: Boolean read FShowOnError write setShowOnError;    // Automatically show the visual log form when warnings/errors are added to the log. This way the user is informed about the problems.

    procedure MainFormCaption(const Caption: string); virtual; abstract;
  end;



{-------------------------------------------------------------------------------------------------
   Command line
--------------------------------------------------------------------------------------------------}
function  CommandLinePath: string;
procedure ExtractPathFromCmdLine(MixedInput: string; OUT Path, Parameters: string);
function  FindCmdLineSwitch(const Switch: string; IgnoreCase: Boolean): Boolean; deprecated 'Use System.SysUtils.FindCmdLineSwitch';
function  ExeName: string;

VAR
  AppDataCore: TAppDataCore;    // The global var "AppData" takes over this one in TAppData.Create. This obj is automatically freed on app shutdown (via FINALIZATION)



IMPLEMENTATION

USES
  LightCore.IO, LightCore.TextFile;

{-------------------------------------------------------------------------------------------------------------
 Parameters

    AppName
       The AppName is the central part of this class. It is used by IniFile/MessageBox/etc.
       Should contain only I/O-safe characters (so no ? <> * % /).
       It is used to generate the INI file that will store the form position/size.

    WindowClassName - [Optional]
       Used in InstanceRunning to detect if the application is already running, by looking to a form with this Class Name.
       Needed ONLY if you use the Single Instance functionality.
       This string must be unique in the whole computer! No other app is allowed to have this ID.
       If you leave it empty, the aAppName is used. But AppName might not be that unique, or you might want to change it over time.
-------------------------------------------------------------------------------------------------------------}
constructor TAppDataCore.Create(CONST aAppName: string; CONST WindowClassName: string= ''; MultiThreaded: Boolean= FALSE);
begin
  inherited Create;
  Initializing:= True;                            // Used in cv_IniFile.pas. Set it to false once your app finished initializing.

  { Sanity check via class var }
  if FCreated
  then RAISE Exception.Create('Error! AppDataCore already constructed!')
  else FCreated:= TRUE;

  { App name }
  Assert(System.IOUtils.TPath.HasValidPathChars(aAppName, FALSE), 'Invalid characters in AppName'+ aAppName);
  Assert(aAppName > '', 'AppName is empty!');
  FAppName:= aAppName;

  { App single instance }
  if WindowClassName = ''
  then FSingleInstClassName:= aAppName
  else FSingleInstClassName:= WindowClassName;    // We use FSingleInstClassName to identify the window/instance (when we check for an existing instance)

  AppDataFolder(TRUE);               // Force the creation of AppData folder.

  { App first run }
  FRunningFirstTime:= NOT FileExists(IniFile);

  { Log }
  { Warning: We cannot use Application.CreateForm here because this will make the Log the main form! }
  Assert(RamLog = NIL, 'Log already created!');  // Call this as soon as possible so it can catch all Log messages generated during app start up. A good place might be in your DPR file before Application.CreateForm(TMainForm, frmMain)
  RamLog:= TRamLog.Create(FShowOnError, NIL, MultiThreaded);

  { App settings }
  if FileExists(IniFile)
  then LoadSettings
  else defaultSettings; // Hint: Use LightSaber\Demo\Template App\Full\FormSettings.pas to give user access to these settings }

  { Product details }
  CompanyName    := 'SciVance';
  CompanyHome    := DefaultHomePage;
  ProductHome    := DefaultHomePage;
  ProductOrder   := DefaultHomePage;
  ProductSupport := DefaultHomePage;
  ProductWelcome := DefaultHomePage;
  ProductUninstal:= DefaultHomePage;
end;

procedure TAppDataCore.AfterConstruction;
begin
  inherited AfterConstruction;
end;

{ This destructor is called automatically from the "Finalization" section.

  Warning:
    At this point the AppData var is already nil because FreeAndNil (called in the initialization section), first puts the var to Nil, then calls the .Free.
    So we need to destroy the Log form here, and not in the Finalization section! }
destructor TAppDataCore.Destroy;
begin
  SaveSettings;
  FreeAndNil(RamLog); // Call this as late as possible
  inherited;
end;







{-------------------------------------------------------------------------------------------------------------
   APP PATH
-------------------------------------------------------------------------------------------------------------}

{ Returns the folder where the EXE file resides.
  The path ended with backslash. Works with UNC paths. Example: c:\Program Files\MyCoolApp\ }       // Oldname: CurFolder
function TAppDataCore.ExeFolder: string;
begin
  Result:= ExtractFilePath(ExeName);
end;


{ Returns the folder where the EXE file resides plus one extra folder called 'System'
  The path ended with backslash. Works with UNC paths. Example: c:\Program Files\MyCoolApp\System\ }
function TAppDataCore.SysDir: string;
begin
  Result:= ExeFolder+ Trail('System');
end;


{ Check if the System folder exists. If not, we should kill the program! }
function TAppDataCore.CheckSysDir: Boolean;
begin
  Result:= DirectoryExists(SysDir);

  if NOT Result
  then LogError('The program was not properly installed! The "System" folder is missing.');
end;


{ Returns ONLY the name of the app (exe name without extension) }
function TAppDataCore.AppShortName: string;
begin
  Result:= ExtractOnlyName(ExeName);
end;


{ Returns the last folder used when the user opened a LoadFile/SaveFile dialog box }
//ToDo: save this to a INI file
function TAppDataCore.getLastUsedFolder: string;
begin
  if FLastFolder = ''
  then Result:= GetMyDocuments
  else Result:= FLastFolder;
end;


{ Returns the path to current user's AppData folder on Windows, and to the current user's home directory on Mac OS X.
  Cannot be used at design-time because AppName is not set!

  Windows XP       C:\Documents and Settings\UserName\Application Data
  Windows Vista+   C:\Users\UserName\AppData\Roaming
  OS X             /Users/UserName
  iOS Device       /private/var/mobile/Containers/Data/Application/Application_ID
  iOS Simulator    /Users/UserName/Library/Developer/CoreSimulator/Devices/Device_ID/data/Containers/Data/Application/Application_ID
  Android          /data/data/Application_ID/files
  Linux            /home/UserName

  If ForceDir, then it creates the folder (full path) where the INI file will be written. }
class function TAppDataCore.AppDataFolder(ForceDir: Boolean = FALSE): string;
begin
  Assert(AppName > '', 'AppName is empty!');
  Assert(System.IOUtils.TPath.HasValidFileNameChars(AppName, FALSE), 'Invalid chars in AppName: '+ AppName);

  Result := Trail(TPath.Combine(TPath.GetHomePath, AppName));

  if ForceDir
  then ForceDirectories(Result);
end;



{ Example:
   Windows XP      C:\Documents and Settings\All Users\Application Data
   Windows Vista+  C:\ProgramData
   OS X            /Users/<username>/Public
   iOS Device      N/A
   iOS Simulator   /Users/<username>/Library/Developer/CoreSimulator/Devices/<Device ID>/data/Containers/Data/Application/<application ID>/Public
   Android         /storage/emulated/0/Android/data/<application ID>/files
}
class function TAppDataCore.AppDataFolderAllUsers(ForceDir: Boolean = FALSE): string;
begin
  Assert(AppName > '', 'AppName is empty!');
  Assert(TPath.HasValidFileNameChars(AppName, FALSE), 'Invalid chars in AppName: '+ AppName);

  Result := Trail(TPath.Combine(TPath.GetPublicPath, AppName));

  if ForceDir
  then ForceDirectories(Result);
end;


{ Returns the name of the INI file (where we will write application's settings).
  It is based on the name of the application. Example: c:\Documents and Settings\MyName\Application Data\MyApp\MyApp.ini }
class function TAppDataCore.IniFile: string;
begin
  Assert(AppName > '', 'AppName is empty!');
  Assert(TPath.HasValidFileNameChars(AppName, FALSE), 'Invalid chars in AppName: '+ AppName);

  Result := TPath.Combine(AppDataFolder, AppName + '.ini');
end;






{-----------------------------------------------------------------------------------------------------------------------
   APP UTILS
-----------------------------------------------------------------------------------------------------------------------}

{ Returns true if the application is "home" (in the computer where it was created). This is based on the presence of a DPR file that has the same name as the exe file. }
class function TAppDataCore.RunningHome: Boolean;
begin
  Result:= FileExists(ChangeFileExt(ExeName, '.dpr'));
end;


{ Returns true if a file called 'betatester' exists in application's folder or in application's system folder. }
function TAppDataCore.BetaTesterMode: Boolean;
begin
  Result:= FileExists(SysDir+ 'betatester') OR FileExists(ExeFolder+ 'betatester');
end;


{ Check if today is past the specified (expiration) date.
  If a file called 'dvolume.bin' exists, then the check is overridden.
  Good for checking exiration dates. }
function TAppDataCore.IsHardCodedExp(Year, Month, Day: word): Boolean;
VAR
   s: string;
   HardCodedDate: TDateTime;
begin
 if FileExists(ExeFolder+ 'dvolume.bin')         { If file exists, ignore the date passed as parameter and use the date written in file }
 then
   begin
     s:= StringFromFile(ExeFolder+ 'dvolume.bin');
     HardCodedDate:= StrToInt64Def(s, 0);
     Result:= round(HardCodedDate- Date) <= 0;     { For example: 2016.07.18 is 3678001053146ms. One day more is: 3678087627949 }
   end
 else
   begin
     HardCodedDate:= EncodeDate(Year, Month, Day);
     Result:= round(HardCodedDate- Date) <= 0;
   end;
end;







{-------------------------------------------------------------------------------------------------------------
   TOOLS
   APP COMMAND LINE
-------------------------------------------------------------------------------------------------------------}

{ Returns the path sent as command line param. }
function CommandLinePath: string;
begin
 if ParamCount > 0
 then Result:= Trim(ParamStr(1))     { Do we have parameter into the command line? }
 else Result := '';
end;


{ Recieves a full path and returns the path and the parameters separately }
procedure ExtractPathFromCmdLine(MixedInput: string; OUT Path, Parameters: string);
VAR i: Integer;
begin
 Assert(Length(MixedInput) > 0, 'Command line length is 0!');

 Path:= '';
 Parameters:= '';
 MixedInput:= Trim(MixedInput);

  // Check if the first character is a double quote
 if MixedInput[1]<> '"'
 then Path:= MixedInput
 else
   { Copy all between ""}
   for i:= 2 to Length(MixedInput) DO                                                   { This supposes that " is on the first position }
    if MixedInput[i]= '"' then                                                          { Find next " character }
     begin
      // ToDo: use LightCore.ExtractTextBetween
      Path:= CopyTo(MixedInput, 1+1, i-1);   // Exclude the double quotes               { +1 si -1 because we want to exclude "" }
      Parameters:= Trim(System.COPY(MixedInput, i+1, MaxInt));
      Break;
     end;
end;   { See also: http://delphi.about.com/od/delphitips2007/qt/parse_cmd_line.htm }


function FindCmdLineSwitch(const Switch: string; IgnoreCase: Boolean): Boolean;
begin
  Result:= System.SysUtils.FindCmdLineSwitch(Switch, IgnoreCase);
end;




{-------------------------------------------------------------------------------------------------------------
   LOG
   ---
   SEND MESSAGES DIRECTLY TO LOG WND
-------------------------------------------------------------------------------------------------------------}
procedure TAppDataCore.LogVerb(CONST Msg: string);
begin
  RamLog.AddVerb(Msg);   // This will call NotifyLogObserver
end;


procedure TAppDataCore.LogHint(CONST Msg: string);
begin
  RamLog.AddHint(Msg);    // This will call NotifyLogObserver
end;


procedure TAppDataCore.LogInfo(CONST Msg: string);
begin
  RamLog.AddInfo(Msg);    // This will call NotifyLogObserver
end;


procedure TAppDataCore.LogImpo(CONST Msg: string);
begin
  RamLog.AddImpo(Msg);    // This will call NotifyLogObserver
end;


procedure TAppDataCore.LogWarn(CONST Msg: string);
begin
  RamLog.AddWarn(Msg);    // This will call NotifyLogObserver
end;


procedure TAppDataCore.LogError(CONST Msg: string);
begin
  RamLog.AddError(Msg);   // This will call NotifyLogObserver
end;


procedure TAppDataCore.LogMsg(CONST Msg: string);  { Always show this message, no matter the verbosity of the log. Equivalent to Log.AddError but the msg won't be shown in red. }
begin
  RamLog.AddMsg(Msg);     // This will call NotifyLogObserver
end;


procedure TAppDataCore.LogBold(CONST Msg: string);
begin
  RamLog.AddBold(Msg);    // This will call NotifyLogObserver
end;


procedure TAppDataCore.LogClear;
begin
  RamLog.clear;
end;


procedure TAppDataCore.LogEmptyRow;
begin
  RamLog.AddEmptyRow;
end;


procedure TAppDataCore.setShowOnError(const Value: Boolean);
begin
  FShowOnError:= Value;
  RamLog.ShowonError:= Value;
end;


procedure TAppDataCore.PopUpLogWindow;
begin
  RamLog.PopUpWindow;
end;












{--------------------------------------------------------------------------------------------------
   Global settings
--------------------------------------------------------------------------------------------------}

// Hide hint after 'Value' ms. Does nothing here. The child class must override this
procedure TAppDataCore.setHideHint(const Value: Integer);
begin
  FHideHint := Value;
end;


procedure TAppDataCore.SaveSettings;
begin
  VAR IniFile := TIniFileEx.Create('AppData Settings', IniFile);
  try
    IniFile.Write('AutoStartUp'   , AutoStartUp);

    IniFile.Write('StartMinim'    , StartMinim);
    IniFile.Write('Minimize2Tray' , Minimize2Tray);
    IniFile.Write('Opacity'       , Opacity);
    IniFile.Write('ShowOnError'   , FShowOnError);
    IniFile.Write('HintType'      , Ord(HintType));
    IniFile.Write('HideHint'      , HideHint);
  finally
    FreeAndNil(IniFile);
  end;
end;


procedure TAppDataCore.LoadSettings;
begin
  VAR IniFile := TIniFileEx.Create('AppData Settings', IniFile);
  try
    AutoStartUp   := IniFile.Read('AutoStartUp'        , False);
    StartMinim    := IniFile.Read('StartMinim'         , False);
    Minimize2Tray := IniFile.Read('Minimize2Tray'      , False);
    Opacity       := IniFile.Read('Opacity'            , 255);
    FShowOnError  := IniFile.Read('ShowOnError'        , True);
    HintType      := THintType(IniFile.Read('HintType' , 0));
    HideHint      := IniFile.Read('HideHint'           , 2500);
  finally
    FreeAndNil(IniFile);
  end;
end;


{ Default program settings
  Hint: Use LightSaber\Demo\Template App\Full\FormSettings.pas to give user access to these settings }
procedure TAppDataCore.DefaultSettings;
begin
  HideHint     := 4000;                      // Hide hint after x ms.
  AutoStartUp  := FALSE;
  StartMinim   := FALSE;
  Minimize2Tray:= TRUE;                      // Minimize to tray
  HintType     := htTooltips;                // Turn off the embeded help system
  Opacity      := 250;
  UserPath     := AppDataFolder;
end;


function ExeName: string;
begin
  Result:= ParamStr(0);   //  Application.ExeName is available only on VCL
end;


end.
