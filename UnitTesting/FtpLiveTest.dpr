program FtpLiveTest;

{=============================================================================================================
   GUARDED live test for LightCore.Internet.Ftp (TFtpUploader) against the Hostinger shared production server.
   Target for this run: tahionic.com.

   Lives in UnitTesting\ so it inherits the same search paths (Indy + LightSaber) as the other test projects.

   TWO PHASES - the default is READ-ONLY. Writing needs an explicit flag.
     PROBE  (default):  FtpLiveTest.exe "<parent>"
        Connect, print the login directory (RetrieveCurrentDir), LIST the login dir, then walk into <parent>
        one plain ChangeDir at a time and LIST it. NO MakeDir, NO Put. This tells us how this quirky server
        (see Hostinger.Parole) actually reports paths after login BEFORE we trust any of it for a write.
     UPLOAD (opt-in):   FtpLiveTest.exe "<parent>" --upload
        Everything PROBE does, then create ClaudeTemp under <parent> via UploadFolder and Put test1.html.

   Why a probe first: Hostinger.Parole documents that this server rejects direct paths ("550 Can't change
   directory") and needs curl's //double-slash for absolutes. So we must NOT assume CWD / works, nor assume
   how RetrieveCurrentDir reports the path after a possible chroot. We look first, write second.

   SAFETY (server hosts 5 live sites - BioniX, Tahionic, DNABaser, gabrielmoraru.com, scivance.tech):
     - Navigation is plain TIdFTP.ChangeDir per segment - never ChangeDirForce - so walking the existing tree
       can never create a stray folder. A missing segment raises and we abort.
     - The ONLY write is UploadFolder(local, 'ClaudeTemp'), creating one brand-new, unique folder. TFtpUploader
       has no delete path: worst case is one trivially-removable folder.

   Credentials are read from c:\Projects-www\Hostinger.Parole at run time - NOT compiled in, NOT on argv.
=============================================================================================================}

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.TypInfo,                                         { GetEnumName / TypeInfo - to print the dir-item type }
  IdFTP,
  IdFTPCommon,
  IdFTPList,                                              { TIdDirItemType, TIdFTPListItem }
  IdAllFTPListParsers,                                    { Register every server list-format parser so List() works on Hostinger }
  LightCore.Internet.Ftp in '..\LightCore.Internet.Ftp.pas';

const
  CRED_FILE  = 'c:\Projects-www\Hostinger.Parole';
  LOCAL_DIR  = 'c:\AI\Claude Code\Temp\FtpLiveTest\ClaudeTemp';   { Holds test1.html }
  REMOTE_SUB = 'ClaudeTemp';                                       { New folder to create under the parent }


{ # Credential parsing }
{ Reads 'Host & port:', 'Username:', 'Psw:' from the .Parole file, tolerant of the surrounding comment lines. }
procedure LoadCreds(out Host, User, Psw: string);
var
  Lines: TStringList;
  Line, LowLine: string;
  function ValueAfterColon(const S: string): string;
  begin
    Result := Trim(System.Copy(S, Pos(':', S) + 1, MaxInt));
  end;
begin
  Host := ''; User := ''; Psw := '';
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(CRED_FILE);
    for Line in Lines do
     begin
      if (Trim(Line) = '') or (Trim(Line)[1] = '#') then Continue;
      LowLine := LowerCase(Line);
      if      Pos('host', LowLine)     = 1 then Host := ValueAfterColon(Line)
      else if Pos('username', LowLine) = 1 then User := ValueAfterColon(Line)
      else if Pos('psw', LowLine)      = 1 then Psw  := ValueAfterColon(Line);
     end;
  finally
    FreeAndNil(Lines);
  end;

  if Pos(':', Host) > 0                                   { 'IP' or 'IP:port' -> keep only the host part }
  then Host := Trim(System.Copy(Host, 1, Pos(':', Host) - 1));

  if (Host = '') or (User = '') or (Psw = '')
  then raise Exception.Create('Could not parse Host/Username/Psw from ' + CRED_FILE);
end;


type
  { TFtpProgressEvent is 'of object', so the handler must be a method of an instance - a standalone
    procedure (and an anonymous method) is not assignment-compatible. This 1-method class supplies it. }
  TProgressSink = class
    procedure Progress(const Msg: string);                { matches TFtpProgressEvent }
  end;

procedure TProgressSink.Progress(const Msg: string);
begin
  Writeln('  ', Msg);
end;


{ Dump the current remote directory listing (read-only). }
procedure ListHere(Ftp: TIdFTP; const Tag: string);
var i: Integer;
begin
  Writeln('  --- LIST ', Tag, ' (', Ftp.RetrieveCurrentDir, ') ---');
  Ftp.List('', True);                                     { Populates Ftp.DirectoryListing (no TStrings dest needed) }
  for i := 0 to Ftp.DirectoryListing.Count - 1 do
    Writeln('    ', Ftp.DirectoryListing.Items[i].FileName,
            '   [', GetEnumName(TypeInfo(TIdDirItemType), Ord(Ftp.DirectoryListing.Items[i].ItemType)), ']');
  Writeln('  --- end (', Ftp.DirectoryListing.Count, ' entries) ---');
end;


{ Case-insensitive path compare, ignoring a trailing '/'. }
function SamePath(const A, B: string): Boolean;
  function Norm(const S: string): string;
  begin
    Result := LowerCase(ReplaceStr(S, '\', '/'));
    while (Result <> '') and (Result[Length(Result)] = '/') do SetLength(Result, Length(Result) - 1);
  end;
begin
  Result := Norm(A) = Norm(B);
end;


{ # Safe navigation (read-only) }
{ The login dir is chrooted deep into ONE site (e.g. /domains/cristinamoraru.net/public_html), but the target
  path /domains/tahionic.com/public_html lives ABOVE it. So we must reach the FTP root first, then walk DOWN.
  We try the documented-quirky steps read-only and report what this server actually accepts. Never MakeDir. }
procedure WalkIntoParent(Ftp: TIdFTP; const ParentPath: string);
var Walk, Seg, Target: string;
begin
  Writeln('Login dir: ', Ftp.RetrieveCurrentDir);         { Where the server dropped us - confirms the chroot }
  ListHere(Ftp, 'login-dir');

  Target := ReplaceStr(ParentPath, '\', '/');

  { # Try absolute ChangeDir first }
  { Parole warns this server may reject direct paths; test it read-only rather than assume. }
  Writeln('');
  Writeln('Attempt 1: absolute ChangeDir -> ', Target);
  try
    Ftp.ChangeDir(Target);
    Writeln('  OK. Now in: ', Ftp.RetrieveCurrentDir);
    ListHere(Ftp, 'target(absolute)');
    Exit;                                                 { Absolute worked - done, we are positioned }
  except
    on E: Exception do
      Writeln('  absolute ChangeDir rejected: ', E.ClassName, ': ', E.Message);
  end;

  { # Fall back: climb to root, then walk down segment by segment }
  Writeln('');
  Writeln('Attempt 2: climb to FTP root with ChangeDirUp, then walk down.');
  while Ftp.RetrieveCurrentDir <> '/' do
   begin
    Seg := Ftp.RetrieveCurrentDir;
    Ftp.ChangeDirUp;
    Writeln('  up -> ', Ftp.RetrieveCurrentDir);
    if Ftp.RetrieveCurrentDir = Seg then Break;           { ChangeDirUp made no progress - stop, do not loop forever }
   end;
  Writeln('  at root: ', Ftp.RetrieveCurrentDir);
  ListHere(Ftp, 'root');

  Walk := Target;
  while (Walk <> '') and (Walk[1] = '/') do Delete(Walk, 1, 1);
  if (Walk <> '') and (Walk[Length(Walk)] <> '/') then Walk := Walk + '/';

  while Pos('/', Walk) > 0 do
   begin
    Seg := System.Copy(Walk, 1, Pos('/', Walk) - 1);
    Delete(Walk, 1, Pos('/', Walk));
    if Seg = '' then Continue;

    Writeln('ChangeDir -> ', Seg);
    Ftp.ChangeDir(Seg);                                   { Plain ChangeDir: a missing segment raises -> we abort. No MakeDir. }
    Writeln('  now in: ', Ftp.RetrieveCurrentDir);
    ListHere(Ftp, Seg);
   end;
end;


{ # Main }
var
  Ftp: TIdFTP;
  Up: TFtpUploader;
  Sink: TProgressSink;
  ParentPath, Host, User, Psw, CurDir: string;
  DoUpload, DoCheckDir: Boolean;
  i: Integer;
begin
  try
    if ParamCount < 1 then
     begin
      Writeln('USAGE: FtpLiveTest.exe "<absolute-parent-path>" [--upload] [--checkdir]');
      Writeln('  default        : READ-ONLY probe (connect + LIST).');
      Writeln('  --checkdir     : READ-ONLY - call DirectoryExists for an existing + a bogus name (tests the fix).');
      Writeln('  --upload       : create ClaudeTemp + Put test1.html.');
      Halt(2);
     end;
    ParentPath := ParamStr(1);
    DoUpload   := False;
    DoCheckDir := False;
    for i := 2 to ParamCount do
     begin
      if SameText(ParamStr(i), '--upload')   then DoUpload   := True;
      if SameText(ParamStr(i), '--checkdir') then DoCheckDir := True;
     end;

    LoadCreds(Host, User, Psw);
    Writeln('Host: ', Host, '   User: ', User);
    Writeln('Parent path: ', ParentPath);
    Writeln('Mode: ', IfThen(DoUpload, 'UPLOAD (will create ClaudeTemp + Put test1.html)', 'PROBE / read-only'));
    Writeln('');

    Ftp := TIdFTP.Create(nil);
    try
      Ftp.Host     := Host;
      Ftp.Username := User;
      Ftp.Password := Psw;
      Ftp.Passive  := False;                              { Hostinger.Parole: "Pasive mode: no" }

      Writeln('Connecting...');
      Ftp.Connect;
      try
        Ftp.TransferType := ftBinary;

        WalkIntoParent(Ftp, ParentPath);                  { Read-only: walk + LIST. Aborts on a missing segment. }

        { # DirectoryExists check (read-only) }
        { Exercise the FIXED DirectoryExists against the live server: an existing dir must report True, a
          bogus one False. This is the exact scenario the old two-list code mishandled. }
        if DoCheckDir then
         begin
          Up := TFtpUploader.Create(Ftp);
          try
            Writeln('');
            Writeln('DirectoryExists("', REMOTE_SUB, '")        = ', BoolToStr(Up.DirectoryExists(REMOTE_SUB), True), '   (expect True if ClaudeTemp was uploaded)');
            Writeln('DirectoryExists("ZZ_NoSuchDir_QWE") = ', BoolToStr(Up.DirectoryExists('ZZ_NoSuchDir_QWE'), True), '   (expect False)');
          finally
            FreeAndNil(Up);
          end;
         end;

        if DoUpload then
         begin
          { # Final guard before ANY write }
          { We must be exactly in the requested parent - not still in the chrooted login site, not anywhere
            unexpected. Compare current dir against the parent (abs or end-matched). Abort otherwise. }
          CurDir := Ftp.RetrieveCurrentDir;
          if NOT (SamePath(CurDir, ParentPath)
               OR SamePath(CurDir, '/' + ReplaceStr(ParentPath, '\', '/'))) then
           begin
            Writeln('ABORT: not in the requested parent. Refusing to write.');
            Writeln('  wanted : ', ParentPath);
            Writeln('  current: ', CurDir);
            Halt(3);
           end;

          Writeln('');
          Writeln('UPLOAD: creating "', REMOTE_SUB, '" under ', CurDir, ' ...');
          Sink := TProgressSink.Create;
          Up := TFtpUploader.Create(Ftp);
          try
            Up.OnProgress := Sink.Progress;               { method pointer - matches TFtpProgressEvent }
            Up.UploadFolder(LOCAL_DIR, REMOTE_SUB, '*.*'); { The ONLY write: one new folder + test1.html }
            Writeln('Upload returned without error. Final dir: ', Ftp.RetrieveCurrentDir);
          finally
            FreeAndNil(Up);
            FreeAndNil(Sink);
          end;
         end
        else
          Writeln('PROBE complete - nothing written. Re-run with --upload once the listing above looks right.');

      finally
        Ftp.Disconnect;
      end;
    finally
      FreeAndNil(Ftp);
    end;

    Writeln('');
    Writeln('DONE - SUCCESS.');
  except
    on E: Exception do
     begin
      Writeln('FAILED: ', E.ClassName, ': ', E.Message);
      Halt(1);
     end;
  end;
end.
