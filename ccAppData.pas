UNIT ccAppData;

{=============================================================================================================
   CubicDesign
   2022.12.04
   See Copyright.txt
  ============================================================================================================

   Via class object you can:
      - Get application's appdata folder (the folder where you save temporary, app-related and ini files)
      - Get application's command line parameters
      - Detect if the application is running for the firs this in a computer
      - Application self-restart
      - Application self-delete
      - Set the font for all running forms
      - Create a new form and set its font to be the same as main forms' font.
      - etc

   The global AppData var will store the object (app wide).
   The AppName variable is the central part of this class. It is used by App/Ini file/MesageBox/etc. You set in in the constructor.

   It is CRITICAL to create the AppData object as soon as the application starts. Prefferably in the DPR file before creating the main form!
     Example: AppData:= TAppData.Create('MyCollApp');
   Even better you can created it in the Initialization/Finaliztion section.

   Note: TAppDataEx class also creates the Log form. See: FormLog.pas

   Tester app: c:\Myprojects\Packages\CubicCommonControls\Demo\CubicCore\GUI Autosave\DemoCore.dpr
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, Winapi.ShlObj, Winapi.ShellAPI, System.Win.Registry, System.IOUtils, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms;

TYPE
  TAppData= class(TObject)
  private
    FLastFolder: string;
    FRunningFirstTime: Boolean;
    FFont: TFont;
    class VAR FCreated: Boolean;
    class VAR FAppName: string;
    function getLastUsedFolder: string;
    procedure setFont(aFont: TFont);
   public
    class VAR Initializing: Boolean;                      { Set it to false once your app finished initializing (usually after you finished creating all forms). Used in cvIniFile.pas. }
    constructor Create(CONST aAppName: string); virtual;

   {--------------------------------------------------------------------------------------------------
      App path/name
   --------------------------------------------------------------------------------------------------}
    function CurFolder: string; //Old name: AppDir
    function SysDir: string;
    function IniFile: string;

    function AppDataFolder(ForceDir: Boolean= FALSE): string;
    function AppDataFolderAllUsers: string;

    function AppShortName: string;
    property LastUsedFolder: string read getLastUsedFolder write FLastFolder;
    class property AppName: string read FAppName;

   {--------------------------------------------------------------------------------------------------
      App Control
   --------------------------------------------------------------------------------------------------}
    property  RunningFirstTime: Boolean read FRunningFirstTime;        { Returns true if the application is running for the first time in this computer }
    procedure Restore;
    procedure Restart;
    procedure SelfDelete;
    function  RunSelfAtWinStartUp(Active: Boolean): Boolean;
    function  RunFileAtWinStartUp(FilePath: string; Active: Boolean): Boolean;
    class procedure CreateForm(aClass: TFormClass; OUT Reference; Show: Boolean = TRUE);
    class procedure CreateFormModal(aClass: TFormClass; OUT Reference);
    procedure SetMaxPriority;

    property Font: TFont read FFont write setFont;

    {-------------------------------------------------------------------------------------------------
      APPLICATION Version
   --------------------------------------------------------------------------------------------------}
    class function GetVersionInfo(ShowBuildNo: Boolean= False): string;
    function  GetVersionInfoV      : string;                            { MAIN. Returns version without Build number. Example: v1.0.0 }
    function  GetVersionInfoMajor: Word;
    function  GetVersionInfoMinor: Word;
    function  GetVersionInfo_: string;

   {--------------------------------------------------------------------------------------------------
      BetaTester tools
   --------------------------------------------------------------------------------------------------}
    class function RunningHome: Boolean;
    function  BetaTesterMode: Boolean;
    function  IsHardCodedExp(Year, Month, Day: word): Boolean;

    class procedure RaiseIfStillInitializing;
  end;




{-------------------------------------------------------------------------------------------------
   Command line
--------------------------------------------------------------------------------------------------}
function  CommandLinePath: string;
procedure ExtractPathFromCmdLine(MixedInput: string; OUT Path, Parameters: string);
function  FindCmdLineSwitch(const Switch: string; IgnoreCase: Boolean): Boolean; deprecated 'Use System.SysUtils.FindCmdLineSwitch';


{-------------------------------------------------------------------------------------------------
   STUFF
--------------------------------------------------------------------------------------------------}
function  GetVersionFixedInfo(CONST FileName: string; VAR FixedInfo: TVSFixedFileInfo): Boolean;


VAR
   AppData: TAppData;   //ToDo: make sure it is unique (make Singleton)


IMPLEMENTATION

USES
  ccCore, ccIO;


{ It must contain only I/O-safe characters (so no question marks). }
constructor TAppData.Create(CONST aAppName: string);
begin
  inherited Create;
  Initializing:= True;                            { Used in cvIniFile.pas. Set it to false once your app finished initializing. }

  if FCreated
  then RAISE Exception.Create('Error! AppData aready constructed!')
  else FCreated:= TRUE;

  Assert(System.IOUtils.TPath.HasValidPathChars(aAppName, FALSE), 'Invalid characters in AppName'+ aAppName);
  Assert(aAppName > '', 'AppName is empty!');
  FAppName:= aAppName;
  Application.Title:= aAppName;

  FRunningFirstTime:= NOT FileExists(IniFile);
  ForceDirectories(AppDataFolder);

  //ToDo: !!! CreateLogForm; But this will create dependencies on the Log!
end;


{ Returns the folder where the EXE file resides
  The path ended with backslash. Works with UNC paths.
  Example: c:\Program Files\MyCoolApp\ }
function TAppData.CurFolder: string;
begin
 Result:= ExtractFilePath(Application.ExeName);
end;


{ Returns the folder where the EXE file resides plus one extra folder called 'System'
  The path ended with backslash. Works with UNC paths.
  Example: c:\Program Files\MyCoolApp\System\ }
function TAppData.SysDir: string;
begin
 Result:= CurFolder+ 'system\';
end;



{ Returns the name of the INI file (where we will write application's settings).
  It is based on the name of the application.
  Example: c:\Documents and Settings\Bere\Application Data\MyApp\MyApp.ini }
function TAppData.IniFile: string;
begin
 Assert(AppName > '', 'AppName is empty!');
 Assert(TPath.HasValidFileNameChars(AppName, FALSE), 'Invalid chars in AppName: '+ AppName);

 Result:= AppDataFolder+ AppName+ '.ini';
end;



{ Returns ONLY the name of the app (exe name without extension) }
function TAppData.AppShortName: string;
begin
 Result:= ExtractOnlyName(Application.ExeName);
end;


{ Returns the last folder used when the user opened a LoadFile/SaveFile dialog box }
//ToDo: save this to a INI file
function TAppData.getLastUsedFolder: string;
begin
 if FLastFolder = ''
 then Result:= GetMyDocuments
 else Result:= FLastFolder;
end;








{ Returns the path to current user's AppData folder on Windows, and to the current user's home directory on Mac OS X.
  Example: c:\Documents and Settings\UserName\Application Data\AppName\
  if ForceDir then it creates the folder (full path) where the INI file will be written.
}
function TAppData.AppDataFolder(ForceDir: Boolean = FALSE): string;
begin
 Assert(AppName > '', 'AppName is empty!');
 Assert(System.IOUtils.TPath.HasValidFileNameChars(AppName, FALSE), 'Invalid chars in AppName: '+ AppName);

 Result:= Trail(Trail(TPath.GetHomePath)+ AppName);

 if ForceDir
 then ForceDirectories(Result);
end;


{ Example: 'C:\Documents and Settings\All Users\Application Data\AppName' }
function TAppData.AppDataFolderAllUsers: string;
begin
 Assert(AppName > '', 'AppName is empty!');
 Assert(System.IOUtils.TPath.HasValidFileNameChars(AppName, FALSE), 'Invalid chars in AppName: '+ AppName);

 Result:= Trail(GetSpecialFolder(CSIDL_COMMON_APPDATA)+ AppName);

 if NOT DirectoryExists(Result)
 then ForceDirectories(Result);
end;












{-----------------------------------------------------------------------------------------------------------------------
   APP UTILS
-----------------------------------------------------------------------------------------------------------------------}

{ Returns true if the application is "home" (in the computer where it was created). This is based on the presence of a DPR file that has the same name as the exe file. }
class function TAppData.RunningHome: Boolean;
begin
 Result:= FileExists(ChangeFileExt(Application.ExeName, '.dpr'));
end;


{ Returns true if a file called 'betatester' exists in application's folder or in application's system folder. }
function TAppData.BetaTesterMode: Boolean;
begin
 Result:= FileExists(SysDir+ 'betatester') OR FileExists(CurFolder+ 'betatester');
end;


{ Check if today is past the specified (expiration) date.
  If a file called 'dvolume.bin' exists, then the check is overridden.
  Good for checking exiration dates. }
function TAppData.IsHardCodedExp(Year, Month, Day: word): Boolean;
VAR
   s: string;
   HardCodedDate: TDateTime;
begin
 if FileExists(CurFolder+ 'dvolume.bin')         { If file exists, ignore the date passed as parameter and use the date written in file }
 then
  begin
   s:= StringFromFile(CurFolder+ 'dvolume.bin');
   HardCodedDate:= StrToInt64Def(s, 0);
   Result:= round(HardCodedDate- Date) <= 0;     { For example: 2016.07.18 is 3678001053146ms. One day more is: 3678087627949 }
   //todayInMilliseconds := round((Now+1) * SecsPerDay * 1000);
  end
 else
  begin
   HardCodedDate:= EncodeDate(Year, Month, Day);
   Result:= round(HardCodedDate- Date) <= 0;
  end;
end;













{--------------------------------------------------------------------------------------------------
   APPLICATION / WIN START UP
--------------------------------------------------------------------------------------------------}

{ Run the specified application at Windows startup }
function TAppData.RunFileAtWinStartUp(FilePath: string; Active: Boolean): Boolean;                             { Porneste anApp odata cu windows-ul }
VAR Reg: TRegistry;
begin
 Result:= FALSE;
 TRY
  Reg:= TRegistry.Create;
  TRY
   Reg.LazyWrite:= TRUE;
   Reg.RootKey:= HKEY_CURRENT_USER;                                                                { This is set by default by the constructor }
   if Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run', TRUE) then
     begin
      if Active
      then Reg.WriteString(ExtractOnlyName(FilePath),'"'+ FilePath + '"')                                { I got once an error here: ERegistryexception-Failed to set data for 'App-Name2'. Probably cause by an anti-virus }
      else Reg.DeleteValue(ExtractOnlyName(FilePath));
      Reg.CloseKey;
      Result:= TRUE;
     end;
  FINALLY
    FreeAndNil(Reg);
  END;
 except
   Result:= FALSE;                                                                                 { To catch possible issues caused by antivirus programs that won't let the program write to 'autorun' section }
 END;
end;


{ Run THIS application at Windows startup }
function TAppData.RunSelfAtWinStartUp(Active: Boolean): Boolean;
begin
 Result:= RunFileAtWinStartUp(ParamStr(0), Active);
end;








{--------------------------------------------------------------------------------------------------
   APPLICATION Control
--------------------------------------------------------------------------------------------------}
procedure TAppData.Restart;                                                                              { Source: About.com }
VAR PAppName : PChar;
begin
  PAppName:= PChar(Application.ExeName);
  Winapi.ShellAPI.ShellExecute( {Handle}0, 'open', PAppName, nil, nil, SW_SHOWNORMAL);   { Handle does not work. Replaced with 0. }
  Application.Terminate;
end;


{ Very dirty! It creates a BAT that deletes the EXE. An nativirus might block this behavior. }
procedure TAppData.SelfDelete;
CONST
   cBatCode = ':delete_exe' + CRLF +'del "%s"' + CRLF +'if exist "%s" goto delete_exe' + CRLF +'del "%s"';
VAR
 List : TStringList;
 BatPath : string;
 S : string;
 PI : TProcessInformation;
 SI : TStartupInfo;
begin
 BatPath:= GetTempFolder+ ChangeFileExt(AppShortName, '.BAT');   // make it in temp
 List := TStringList.Create;
 TRY
  S := Format(cBatCode, [Application.ExeName, Application.ExeName, BatPath]);
  List.Text := S;
  List.SaveToFile(BatPath);
 FINALLY
  FreeAndNil(List);
 END;

 FillChar(SI, SizeOf(SI), 0);
 SI.dwFlags := STARTF_USESHOWWINDOW;
 SI.wShowWindow := SW_HIDE;

 if CreateProcess( NIL, PChar(BatPath), nil, nil, False, IDLE_PRIORITY_CLASS, nil, nil, SI, PI) then
  begin
   CloseHandle(PI.hThread);
   CloseHandle(PI.hProcess);
  end;

 Application.Terminate;                                                                            { This is mandatory }
end;


{ Bring the application back to screen (if minimized, in background, hidden) }
procedure TAppData.Restore;
begin
  Application.MainForm.Visible:= TRUE;
  if Application.MainForm.WindowState = wsMinimized
  then Application.MainForm.WindowState:= TWindowState.wsNormal;
  //Use Restore to restore the application to its previous size before it was minimized. When the user restores the application to normal size, Restore is automatically called.
  //Note: Don't confuse the Restore method, which restores the entire application, with restoring a form or window to its original size. To minimize, maximize, and restore a window or form, change the value of its WindowState property.
  Application.Restore;
  SetForegroundWindow(Application.MainForm.Handle);
  Application.BringToFront;
end;


{ Set this process to maximum priority. Usefull when measuring time }
procedure TAppData.SetMaxPriority;
begin
 SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS); //  https://stackoverflow.com/questions/13631644/setthreadpriority-and-setpriorityclass
end;









{--------------------------------------------------------------------------------------------------
   VERSION INFO
--------------------------------------------------------------------------------------------------}

{ TVSFixedFileInfo returns all kind of more or less info about an executable file.
   Source: JCL }
function getVersionFixedInfo(CONST FileName: string; VAR FixedInfo: TVSFixedFileInfo): Boolean;
var
  InfoSize, FixInfoLen: DWORD;
  DummyHandle: DWORD;
  Buffer: string;
  FixInfoBuf: PVSFixedFileInfo;
begin
  Result := False;
  InfoSize := GetFileVersionInfoSize(PChar(FileName), DummyHandle);
  if InfoSize > 0 then
   begin
    FixInfoLen := 0;
    FixInfoBuf := Nil;

    SetLength(Buffer, InfoSize);
    if  GetFileVersionInfo(PChar(FileName), DummyHandle, InfoSize, Pointer(Buffer))    { The DummyHandle parameter is ignored by GetFileVersionInfo }
    AND VerQueryValue(Pointer(Buffer), '\', Pointer(FixInfoBuf), FixInfoLen)
    AND (FixInfoLen = SizeOf(TVSFixedFileInfo)) then
     begin
      Result := True;
      FixedInfo := FixInfoBuf^;
     end;
  end;
end;


function TAppData.GetVersionInfoMajor: Word;
VAR FixedInfo: TVSFixedFileInfo;
begin
 if GetVersionFixedInfo(Application.ExeName, FixedInfo)
 then Result:= HiWord(FixedInfo.dwFileVersionMS)
 else Result:= 0;
end;


function TAppData.GetVersionInfoMinor: Word;
VAR FixedInfo: TVSFixedFileInfo;
begin
 if GetVersionFixedInfo(Application.ExeName, FixedInfo)
 then Result:= LoWord(FixedInfo.dwFileVersionMS)
 else Result:= 0;
end;



{ Returns version with/without build number.
  Example:
     1.0.0.999
     1.0.0

  See also: CheckWin32Version }
class function TAppData.GetVersionInfo(ShowBuildNo: Boolean= False): string;
VAR FixedInfo: TVSFixedFileInfo;
begin
  FixedInfo.dwSignature:= 0;
  if GetVersionFixedInfo(Application.ExeName, FixedInfo)
  then
     begin
      Result:= IntToStr(HiWord(FixedInfo.dwFileVersionMS))+'.'+ IntToStr(LoWord(FixedInfo.dwFileVersionMS))+'.'+ IntToStr(HiWord(FixedInfo.dwFileVersionLS));
      if ShowBuildNo
      then Result:= Result+ '.'+ IntToStr(LoWord(FixedInfo.dwFileVersionLS));
     end
  else Result:= '0';
end;


{ Returns version without build number. Example: v1.0.0.
  Automatically adds the v in front of the number. }
function TAppData.GetVersionInfoV: string;
begin
 Result:= ' v'+ GetVersionInfo(False);
end;


{ Yet another alternative.
  Seems to have issues on Vista }
function TAppData.GetVersionInfo_: string;
const
  InfoStr: array[1..2] of string = ('FileVersion', 'InternalName');
var
  S: string;
  InfoSize, Len, VZero: DWORD;
  Buf: PChar;      //ok
  FixInfoBuf: PChar;
begin
  Result := '';
  S:= Application.ExeName;
  InfoSize:= GetFileVersionInfoSize(PChar(S), VZero);    // https://docs.microsoft.com/en-us/windows/win32/api/winver/nf-winver-getfileversioninfosizea
  if InfoSize > 0 then
  begin
    Buf:= AllocMem(InfoSize);
    GetFileVersionInfo(PChar(S), 0, InfoSize, Buf);

     if VerQueryValue(Buf, PChar('StringFileInfo\040904E4\' + InfoStr[1]), Pointer(FixInfoBuf), Len)
     then Result:= FixInfoBuf;    {  <---- AV here, on Vista 64bit }

    FreeMem(Buf, InfoSize);
  end
end;










{----







{-----------------------------------------------------------------------------------------------------------------------
   APP COMMAND LINE
-----------------------------------------------------------------------------------------------------------------------}
{ Returns the path sent as command line param. Tested ok. }
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

 { I don't have paramters }
 if MixedInput[1]<> '"'
 then
    Path:= MixedInput
 else
   { Copy all between ""}
   for i:= 2 to Length(MixedInput) DO                                                                { This supposes that " is on the first position }
    if MixedInput[i]= '"' then                                                                       { Find next " character }
     begin
      // ToDo: use ccCore.ExtractTextBetween
      Path:= CopyTo(MixedInput, 1+1, i-1);                                                           { +1 si -1 because we want to exclude "" }
      Parameters:= System.COPY(MixedInput, i+1, Length(MixedInput));
      Break;
     end;
end;   { See also: http://delphi.about.com/od/delphitips2007/qt/parse_cmd_line.htm }


function FindCmdLineSwitch(const Switch: string; IgnoreCase: Boolean): Boolean;
begin
  Result:= System.SysUtils.FindCmdLineSwitch(Switch, IgnoreCase);
end;






{ 1. Create the form ONLY it does not exist already
  2. Set the font of the new form to be the same as the font of the MainForm
  3. Show it }
class procedure TAppData.CreateForm(aClass: TFormClass; OUT Reference; Show: Boolean = TRUE);
begin
  Application.CreateForm(aClass, Reference);
  if TForm(Reference) <> Application.MainForm
  then TForm(Reference).Font:= Application.MainForm.Font;

  if Show then TForm(Reference).Show;
end;


class procedure TAppData.CreateFormModal(aClass: TFormClass; OUT Reference);
begin
  Application.CreateForm(aClass, Reference);
  if TForm(Reference) <> Application.MainForm
  then TForm(Reference).Font:= Application.MainForm.Font;
  TForm(Reference).Visible:= FALSE;   //This happens with frmFloor in Cassa2.exe
  TForm(Reference).ShowModal;
end;



// Question: Does FormCount also count invisible forms? Answer: Yes.
// Question: Does FormCount also count forms created TFrom.Create(Nil). Answer: Yes.
procedure TAppData.setFont(aFont: TFont);
begin
  FFont:= aFont;
  for VAR i:= 0 to Screen.CustomFormCount - 1 DO    // FormCount => forms currently displayed on the screen. CustomFormCount = as FormCount but also includes the property pages
    Screen.Forms[i].Font:= aFont;
end;


class procedure TAppData.RaiseIfStillInitializing;
CONST
   AppStillInitializingMsg = 'Application not properly initialized.'+#13#10#13#10+ 'PLEASE REPORT the steps necessary to reproduce this bug and restart the application.';
begin
 if AppData.Initializing
 then RAISE Exception.Create(AppStillInitializingMsg);
end;


 {
initialization

finalization  }

end.

