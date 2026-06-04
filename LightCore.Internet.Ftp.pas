UNIT LightCore.Internet.Ftp;

{-------------------------------------------------------------------------------------------------------------
   FTP support (platform-agnostic).
   Date: 2026-06-03

   This unit is deliberately NOT part of any LightSaber package: it pulls in Indy (IdFTP), and LightSaber stays
   Indy-free. It lives in the LightSaber folder only so projects on the LightSaber search path can reach it.
   Don't forget to let the program through the firewall.
--------------------------------------------------------------------------------------------------------------
   TFTPDetails - FTP account configuration record
     Holds one FTP deployment target: server, credentials, remote directory, the site's public URL, and an
     AccountName for shared-hosting subfolders (e.g. 000webhost). Serialized with TLightStream.
     Backward compatibility is mandatory: do NOT reorder the fields or change the serialization layout, because
     callers persist the record (e.g. Stormy's .thunder file).

   TFtpUploader - operations over an already-connected TIdFTP
     UploadFolder     - upload LocalDir (and all its files and subfolders) into RemoteDir, creating it if missing.
     ChangeDirForce   - change into a sub-folder of the current folder, creating it if missing.
     NavigateTo       - walk into a 'deep path' (one or more folders), creating each folder along the way.
     DirectoryExists  - report whether a directory exists in the current FTP folder.

     The uploader borrows the TIdFTP - it does NOT own it; the caller keeps full control of the connection
     lifecycle (Connect / Disconnect / status). Before UploadFolder, the caller must have the TIdFTP connected
     with TransferType = ftBinary.

     Errors propagate as exceptions (Indy's own EId* on a failed FTP command, or EFtpError) rather than
     showing a dialog, so the unit needs no GUI. The caller turns the exception into whatever UI it wants.
     Assign OnProgress to receive textual progress.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
   System.SysUtils, System.Classes, IdFTP, LightCore.StreamBuff;

TYPE
 TFTPDetails= record
   Server    : string;
   RemoteDir : string;
   User      : string;
   Parola    : string;
   Passive   : Boolean;
   OnlineAdr : string;
   AccountName: string;      { The name of the sub-folder where website is stored for each 000webhost user (using the Demo 000webhost account). If '' then no account was yet created }
   procedure Clear;
   procedure Read (Stream: TLightStream);
   procedure Write(Stream: TLightStream);
  end;

 PFTPDetails= ^TFTPDetails;


 TFtpProgressEvent = procedure(const Msg: string) of object;

 EFtpError = class(Exception);


 TFtpUploader = class(TObject)
  private
    FFtp: TIdFTP;                                         { Borrowed, NOT owned }
    FOnProgress: TFtpProgressEvent;
    procedure DoProgress(const Msg: string);
  public
    constructor Create(aFtp: TIdFTP);
    procedure UploadFolder    (const LocalDir, RemoteDir, Filter: string);   {todo 1: report a percentage via OnProgress, not just per-file text }
    procedure ChangeDirForce  (const SubDir: string);
    procedure NavigateTo      (const RemotePath: string);
    function  DirectoryExists (const Dir: string): Boolean;
    property  Ftp: TIdFTP read FFtp;
    property  OnProgress: TFtpProgressEvent read FOnProgress write FOnProgress;
  end;



IMPLEMENTATION

USES
   IdFTPCommon, IdFTPList, LightCore, LightCore.IO;



{-------------------------------------------------------------------------------------------------------------
   TFtpUploader
-------------------------------------------------------------------------------------------------------------}

constructor TFtpUploader.Create(aFtp: TIdFTP);
begin
 inherited Create;
 Assert(aFtp <> NIL, 'TFtpUploader needs a valid TIdFTP!');
 FFtp:= aFtp;
end;


procedure TFtpUploader.DoProgress(const Msg: string);
begin
 if Assigned(FOnProgress)
 then FOnProgress(Msg);
end;


function TFtpUploader.DirectoryExists(const Dir: string): Boolean;
VAR
  k: Integer;
  Item: TIdFTPListItem;
begin
 { One listing, read name + type from the SAME parsed record. The old code issued two separate listings
   (LIST/MLSD + NLST) and matched them by index - but those are independent server commands with no shared
   order or membership (MLSD returns './..', NLST usually does not), so the dir/file flag and the name could
   describe different entries. Indy's DirectoryListing carries both FileName and ItemType per item; List()
   clears it before filling, so no stale entries. }
 Result := False;
 FFtp.List('', True);
 for k := 0 to FFtp.DirectoryListing.Count - 1 do
  begin
   Item := FFtp.DirectoryListing.Items[k];
   if  (Item.ItemType = ditDirectory)
   and (Item.FileName <> '.')
   and (Item.FileName <> '..')
   and SameText(Item.FileName, Dir)                     { case-insensitive, matching the old TStringList.IndexOf default }
   then EXIT(True);
  end;
end;


procedure TFtpUploader.ChangeDirForce(const SubDir: string);
begin
 if NOT DirectoryExists(SubDir)
 then FFtp.MakeDir(SubDir);

 FFtp.ChangeDir(SubDir);                                { ChangeDir issues CWD and raises on any non-2xx reply, so a failed change cannot pass silently - no extra verify needed }
end;


procedure TFtpUploader.NavigateTo(const RemotePath: string);   { RemotePath may be many folders deep, e.g. 'public_html/user/sitename/' }
VAR SubDir, Dir: string;
begin
 Dir := Convert2LinuxPath(RemotePath);
 if Dir <> '' then
  begin
   if Dir[1] = '/'                                       { Dir is non-empty here (guarded above). Drop the leading '/' so the first Copy() below grabs a folder name, not '' }
   then Delete(Dir, 1, 1);

   Dir:= TrailLinuxPath(Dir);

   { Enter each folder in turn, peeling it off the front of Dir }
   WHILE Pos('/', Dir) > 0 DO
    begin
     SubDir:= System.Copy(Dir, 1, Pos('/', Dir) - 1);
     ChangeDirForce(SubDir);
     Delete(Dir, 1, Pos('/', Dir));                       { 'your/directory/subdir/' --> 'directory/subdir/' }
    end;
  end;
end;


procedure TFtpUploader.UploadFolder(const LocalDir, RemoteDir, Filter: string);

   procedure uploadDir(dir: string);
   VAR
      sFile, SubDir: string;
      List: TStringList;
   begin
     { # Subfolders }
     List:= ListDirectoriesOf(dir, FALSE, FALSE);
     TRY
       for SubDir in List DO
        begin
         DoProgress('Uploading files from '+ dir+ SubDir+ PathDelim);
         ChangeDirForce(SubDir);
         TRY
           uploadDir(dir + SubDir + PathDelim);                      { Descend locally and remotely in lock-step. PathDelim (not literal '\') so the local path stays valid on POSIX, where ListDirectoriesOf/ListFilesOf return '/'-separated paths }
         FINALLY
           FFtp.ChangeDirUp;                                         { Always climb back out, even if the recursion raised, so the connection is left at the same depth we entered. Otherwise a caller that reuses the (borrowed) TIdFTP after a failed upload would build a corrupted remote tree. }
         END;
        end;
     FINALLY
       FreeAndNil(List);                                             { try/finally: ChangeDirForce/ChangeDirUp can raise mid-loop }
     END;

     { # Files }
     List:= ListFilesOf(dir, Filter, FALSE, FALSE);
     TRY
       for sFile in List DO
        begin
          Assert(sFile > '', 'File name should not be empty!');
          if FileExists(dir + sFile)
          then
           begin
            DoProgress('Uploading file: '+ sFile+ ' - '+ GetFileSizeFormat(dir + sFile));
            FFtp.Put(dir + sFile, sFile);
           end
          else DoProgress('File not found! '+ sFile);
        end;
     FINALLY
       FreeAndNil(List);                                             { try/finally: FFtp.Put can raise an Indy exception mid-loop }
     END;
   end;


VAR
   SubDir, LDir, RDir: string;
begin
 Assert(FFtp.TransferType= ftBinary, 'FTP TransferType is not Binary!');

 { # Remote dir }
 RDir := StringReplace(RemoteDir, '\', '/', [rfReplaceAll]);   { Accept either 'dir\dir' or 'dir/dir' from the caller }
 if RDir <> '' then
  begin
   if RDir[1] = '/' then Delete(RDir, 1, 1);
   if RDir[Length(RDir)] <> '/' then RDir := RDir + '/';       { Force a trailing '/' so the loop below sees every segment }

   DoProgress('Navigating to '+ RDir);
   WHILE Pos('/', RDir) > 0 DO
    begin
     SubDir:= System.Copy(RDir, 1, Pos('/', RDir) - 1);
     ChangeDirForce(SubDir);
     Delete(RDir, 1, Pos('/', RDir));                          { 'your/directory/subdir/' --> 'directory/subdir/' }
    end;
  end;

 { # Local dir }
 LDir := LocalDir;
 if (Length(LDir) > 0) AND (LDir[Length(LDir)] <> PathDelim)
 then LDir := LDir + PathDelim;                                { PathDelim (not literal '\') so this stays correct on POSIX }

 { # Upload }
 uploadDir(LDir);
end;



{-------------------------------------------------------------------------------------------------------------
   TFTPDetails
-------------------------------------------------------------------------------------------------------------}

procedure TFTPDetails.Clear;
begin
 Server     := '';
 RemoteDir  := '';
 User       := '';
 Parola     := '';
 OnlineAdr  := '';
 AccountName:= '';
 Passive    := FALSE;
end;


{ Wire format (must match Write): Server, RemoteDir, User, Parola, Passive, OnlineAdr, AccountName,
  then 96 reserved bytes for future fields. }
procedure TFTPDetails.Read(Stream: TLightStream);
CONST CtMaxField= 65536;                               { 64KB. ReadString defaults to a 1KB SafetyLimit; these config fields can legitimately be longer (long URLs/paths), and a too-low cap would raise mid-read and desync the whole file. 64KB still bounds a corrupted stream. }
begin
 Server     := Stream.ReadString(CtMaxField);
 RemoteDir  := Stream.ReadString(CtMaxField);
 User       := Stream.ReadString(CtMaxField);
 Parola     := Stream.ReadString(CtMaxField);          {todo 5: encrypt password}
 Passive    := Stream.ReadBoolean;
 OnlineAdr  := Stream.ReadString(CtMaxField);
 AccountName:= Stream.ReadString(CtMaxField);
 Stream.ReadPadding0(96);                              { Reserved space - lets new fields be added later without breaking old files }
end;


{ Wire format (must match Read): see TFTPDetails.Read. }
procedure TFTPDetails.Write(Stream: TLightStream);
begin
 Stream.WriteString(Server);
 Stream.WriteString(RemoteDir);
 Stream.WriteString(User);
 Stream.WriteString(Parola);                           {todo 5: encrypt password}
 Stream.WriteBoolean(Passive);
 Stream.WriteString(OnlineAdr);
 Stream.WriteString(AccountName);
 Stream.WritePadding0(96);                              { Keep in sync with Read's ReadPadding0(96) }
end;



end.
