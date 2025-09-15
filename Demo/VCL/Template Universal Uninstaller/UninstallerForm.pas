UNIT UninstallerForm;

{--------------------------------------------------------------------------------------------------
  GabrielMoraru.com
  2023.02.14
  Universal Uninstaller

  How to use it:
     Add SharedUninstaller.pas to your project.
     Define the UninstalledApp const
     At application's first run cal:
        SharedUninstaller.RegisterUninstaller;

---------------------------------------------------------------------------------------------------
  How the uninstaller works:
   1. Creates copy of itself in the Temp folder
   2. Launches the copy
   3. Shuts down original (self)
   4. Lets the second copy to uninstall the app

  Why it had to be implemented it this way?
    The uninstaller is distributed in application's folder,
    so we cannot delete that folder because it is locked by the uninstaller.
--------------------------------------------------------------------------------------------------}

{ http://stackoverflow.com/questions/4014268/how-do-i-delete-a-folder-that-another-process-has-open }

INTERFACE

USES
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Controls,
  Vcl.ExtCtrls, System.Classes, SysUtils, Vcl.Forms, System.IOUtils,
  LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.SystemTime, LightVcl.Common.Clipboard, LightVcl.Common.Dialogs, LightCore.IO, LightCore.TextFile, LightVcl.Common.IO, LightVcl.Visual.PathEdit, LightVcl.Visual.CountDown,
  InternetLabel, LightVcl.Visual.RichLog, Vcl.Imaging.pngimage, LightVcl.Common.AppDataForm;

TYPE
  TfrmMain = class(TLightForm)
    imgLogo      : TImage;
    lblVersion   : TLabel;
    Panel1       : TPanel;
    mmoLog       : TRichLog;
    edtPath      : TCubicPathEdit;
    btnUninstall : TButton;
    btnResetIni  : TButton;
    btnFeedback  : TButton;
    btn1         : TButton;
    lblDiscount  : TLabel;
    inetDiscount : TInternetLabel;
    CountDown    : TCountDown;
    procedure btnUninstallClick (Sender: TObject);
    procedure btnFeedbackClick  (Sender: TObject);
    procedure FormCreate        (Sender: TObject);
    procedure imgLogoClick      (Sender: TObject);
    procedure btn1Click         (Sender: TObject);
    procedure edtPathPathChanged(Sender: TObject);
    procedure btnResetIniClick  (Sender: TObject);
    procedure CountDownTimesUp  (Sender: TObject);
  private
    FeedbackSent: Boolean;
    function DeleteItem(Item: string): Boolean;
  public
 end;


VAR
  frmMain: TfrmMain;

procedure CreateCopy;


IMPLEMENTATION {$R *.dfm}


{/$DEFINE BIONIX}
{/$DEFINE BASER}

USES
   LightVcl.Common.Window, LightVcl.Common.ExecuteShell, LightVcl.Common.Shell, LightCore.AppData, LightVcl.Common.AppData;

   {$IFDEF BioniX}BxConstants;{$ENDIF}
   {$IFDEF BASER} BaserConst, BxConstants;{$ENDIF}


{$IFDEF BioniX}
CONST
   //UninstalledApp   = 'BioniX Wallpaper';
   //UninstReasonPage = BxConstants.wwwUninstallFeedback;
   //SupportEmail     = BxConstants.ProductSupport;
   //WinClassName     = BxConstants.WindowClassName;
{$ENDIF}

{$IFDEF BASER}
CONST
   //UninstalledApp   = 'DNA Baser v5';
   //UninstReasonPage = wwwBaserUninstReason;
   //SupportEmail     = EmailBaser_Contact;
   //WinClassName     = 'Baser_v5.SingleInstance';
{$ENDIF}




procedure CreateCopy;  { Used in DPR file }
VAR SelfCopy: string;
begin
 { Let the program run from inside the IDE }
 if AppData.RunningHome then EXIT;

 { If not parameters are passed in the command line create a copy of self in temp, run that copy and kill self }
 if ParamCount= 0 then
  begin
   SelfCopy:= GetTempFolder+ ExtractFileName(Application.ExeName);
   FileCopyQuick(Application.ExeName, GetTempFolder);
   if FileExists(SelfCopy)
   then ExecuteShellEx(SelfCopy, 'uninstall')           { Doesn't matter what I pass to the copy. I just need to pass something. Anything. So the ParamCount gets > 0 }
   else MessageError('Cannot create temporary copy! Most probable cause: your antivirus is blocking it.');
   Application.Terminate;
  end
end;



procedure CheckValidAppName;  { This TEMPORARY check is here because I got bug reports from users saying "Cannot create file" }
begin
 if NOT System.IOUtils.TPath.HasValidFileNameChars(AppData.AppName, FALSE)
 then RAISE Exception.Create(AppData.AppName+ ' filename is invalid!');
end;




procedure TfrmMain.FormCreate(Sender: TObject);
VAR DetectedFolder: string;
begin
 lblVersion.Caption:= 'Uninstaller version: '+ TAppData.GetVersionInfo;
 /// inetDiscount.Link:= BxConstants.wwwUninstallDiscount; { Give user a 50% discount so he won't uninstall. }

 if FileExists(AppData.ExeFolder+ 'DebugMode')
 then Caption:= Application.ExeName
 else Caption:= AppData.AppName+ ' uninstaller - This program is provided "as is"';
 mmoLog.AddEmptyRow;

 DetectedFolder:= AppData.ReadInstallationFolder(AppData.AppName);

 if DetectedFolder= ''
 then
  begin
    frmMain.Show;
    mmoLog.AddEmptyRow;
    edtPath.Path:= 'Cannot detect installation folder! Please enter the path here manually.';
    mmoLog.AddWarn('Cannot detect installation folder!');
    mmoLog.AddWarn('Unless changed by user, the default path should be "C:\BioniX Wallpaper\"');
  end
 else
  begin
   edtPath.Path:= DetectedFolder;
   btnUninstall.Enabled:= FALSE;   { This will be enabled after 7 seconds, by the timer }

   if NOT DirectoryExists(DetectedFolder) then
    begin
      frmMain.Show;
      mmoLog.AddEmptyRow;
      MessageError('Original installation folder does not exist. CRLF Probably moved or deleted manually.'+ CRLF+ 'Folder: '+ DetectedFolder);
      mmoLog.AddWarn('Folder to be entered manually by user.');
    end;
   CountDown.Start;
  end;

 CheckValidAppName;
end;



function TfrmMain.DeleteItem(Item: string): Boolean;  { Delete to RecycleBin }
begin
  { This creates problems with RamDisk. The folder gets locked after BioniX deletes it }
  Result:= RecycleItem(Item, TRUE, FALSE);
  if Result
  then mmoLog.AddInfo('Folder '+Item+' deleted to RecycleBin.')
  else mmoLog.AddWarn('Folder '+Item+' not deleted. Probably another application is locking the folder.');
end;



procedure TfrmMain.btnUninstallClick(Sender: TObject);
VAR
   AppFolder: string;
   AddDataFolder: string;
begin
 AppFolder:= edtPath.Path;

 if FileExists(AppData.ExeFolder+ 'DebugMode')
 then AddDataFolder:= 'Protected while uninstaller running in debugg mode'
 else AddDataFolder:= AppData.ReadAppDataFolder(AppData.AppName);

 mmoLog.Clear;
 mmoLog.AddInfo('Uninstall process started...');

 { PATH EXISTS }
 if NOT DirectoryExistMsg(AppFolder) then
  begin
   mmoLog.AddWarn('Folder does not exist: '+ AppFolder);
   mmoLog.AddInfo('Uninstall process aborted.');
   EXIT;
  end;

 { ARE YOU SURE? }

 if MesajYesNo('Are you sure you want to uninstall '+ AppData.AppName+'?') { FUCKING IMPORTANT! Always check for mrYes, never for mrNo. This is why: http://capnbry.net/blog/?p=46 }
 then mmoLog.AddInfo('User confirmation obtained.')
 else
  begin
   mmoLog.AddInfo('Uninstall process aborted by user.');
   EXIT;
  end;

 { Give user a 50% discount so he won't uninstall. }
 lblDiscount.Visible:= TRUE;
 inetDiscount.Visible:= TRUE;
 Refresh;
 Sleep(3000);

 { APP STILL RUNNING? }
 Assert(AppData.SingleInstClassName <> '');

 WHILE RestoreWindowByName(AppData.SingleInstClassName) DO
   begin
    mmoLog.Lines.Add(AppData.AppName+ ' is running. Please shut it down.');
    MessageWarning(AppData.AppName+ ' is still running. Please shut it down NOW in order to uninstall it properly!'+
                 LBRK+ 'Note: If the application is not responding you can use the TaskManager to kill it.'+
                 LBRK+ 'When ready press OK to continue the uninstall process.');
   end;

  { Confirmation }
  if MesajYesNo('Do you approve deletion of the following items?'
           +CRLF+ ' '+ AppFolder
           +CRLF+ ' '+ AddDataFolder
           +CRLF+ '  Desktop icon'
           +CRLF+ '  Start menu icon'
           +CRLF
           +CRLF+ 'If you are not sure, you can also delete them manually.')
  then
   begin
    mmoLog.AddEmptyRow;
    mmoLog.AddInfo('The user approved deletion of: ');
    mmoLog.AddInfo('   '+ AppFolder);
    mmoLog.AddInfo('   '+ AddDataFolder);
   end
  else
   begin
    mmoLog.AddMsg('Uninstall process aborted by user.');
    EXIT;
   end;

 { Delete app's folder }
 if FolderIsSpecial(AppFolder)
 then
  begin
   mmoLog.AddEmptyRow;
   mmoLog.AddInfo('Deleting '+ AppFolder);
   mmoLog.AddWarn('  You are trying to delete a special folder!');
   mmoLog.AddInfo('   To protect the integrity of your data, the current operation was aborted.');
  end
 else
   if NOT DeleteItem(AppFolder) then
    begin
     MessageInfo(AppFolder+ CRLF+ 'This folder is locked by a 3rd party application. Please try to delete it manually.');
     mmoLog.AddMsg('The user will manually delete the folder: '+ AppFolder);
     ExecuteShell(AppFolder);
    end;

 { Delete app data folder }
 if DirectoryExists(AddDataFolder)
 then
   if FolderIsSpecial(AddDataFolder)
   then
    begin
     mmoLog.AddEmptyRow;
     mmoLog.AddInfo('Deleting '+ AddDataFolder);
     mmoLog.AddWarn('  You are trying to delete a special folder!');
     mmoLog.AddInfo('  To protect the integrity of your data, the current operation was aborted.');
    end
   else
     DeleteItem(AddDataFolder);

 { DELETE DESKTOP SHORTCUT }
 if DeleteDesktopShortcut  (AppData.AppName) then mmoLog.Lines.Add('Desktop icon removed.');
 if DeleteStartMenuShortcut(AppData.AppName) then mmoLog.Lines.Add('Start menu icon removed.');

 mmoLog.AddEmptyRow;

 { DELETE INSTALLATION PATH FROM REGISTRY
   - let it there is case the user want to run the uninstaller again
 if RegDeleteKey(HKEY_CURRENT_USER, RegKey+ UninstalledApp)
 then mmoLog.AddVerb('Path information cleaned'); }

 { Remove Baser file association }
 {$IFDEF BASER}
 if PosInsensitive('baser', AppData.AppName) > 0 then {DEL}
  begin
   mmoLog.AddInfo('Reseting file association.');
   AssociationReset('.scf', FALSE);
   AssociationReset('.abi', FALSE);
   AssociationReset('.ab1', FALSE);
   AssociationReset('.ab' , FALSE);
   AssociationReset('.fasta', FALSE);
  end;
 {$ENDIF}

 mmoLog.AddEmptyRow;
 mmoLog.AddInfo('Files deleted to Recycle Bin.');
 mmoLog.AddInfo('Uninstall done.');
 mmoLog.AddEmptyRow;
 CheckValidAppName;
 TRY
  mmoLog.SaveAsRtf(GetDesktopFolder+ AppData.AppName+ ' - uninstall confirmation '+ LightCore.IO.TimeToStr_IO(Now)+'.RTF');
 EXCEPT
   on EFCreateError do MesajGeneric('The uninstaller was blocked from creating the uninstall log on your desktop! Some overzealous antivirus programs misbehave by blocking legitimate applications.')
   { This is here because I got bug reports from users saying "Cannot create file".
     However, there is ultimate evidence that some antivirus programs will stop BioniX from creating a file on desktop! }
 END;

 if NOT FeedbackSent then
  begin
   MessageInfo('Please tell us why you uninstalled the program so we can make it better. CRLF CRLF CONSTRUCTIVE feedback is always appreciated. CRLF CRLF Thank you.');
   ExecuteShell(AppData.ProductUninstal);
  end;

 edtPath.Path:= '';

 AppData.SelfDelete;
end;



procedure TfrmMain.CountDownTimesUp(Sender: TObject);
begin
 btnUninstall.Enabled:= TRUE;
end;


procedure TfrmMain.btn1Click(Sender: TObject);
begin
 //DeleteItem('c:\test1');
 edtPath.Path:= 'c:\test1';
end;


procedure TfrmMain.btnFeedbackClick(Sender: TObject);
begin
 ExecuteShell('mailto:'+ AppData.ProductSupport);
 ExecuteShell(AppData.ProductUninstal);
 FeedbackSent:= TRUE;
end;


procedure TfrmMain.btnResetIniClick(Sender: TObject);
begin
 ///ExecuteShell(BxConstants.wwwResetToFactory);  put it back
end;


procedure TfrmMain.edtPathPathChanged(Sender: TObject);
begin
 btnUninstall.Enabled:= edtPath.PathIsValid= '';
end;


procedure TfrmMain.imgLogoClick(Sender: TObject);
begin
 ExecuteShell(AppData.ProductUninstal);
end;



end.     

