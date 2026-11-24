UNIT LightCore.INIFileMem;

{=============================================================================================================
   2026.11.22
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
  TMemIniFileEx - an INI file kept in memory and written to disk once, at Free or at UpdateFile.

  What it is for:
     A program that writes many values to one INI file in one go, for example all its settings at close.
     TIniFile on Windows writes the file at every WriteString (one WritePrivateProfileString call per value),
     and each such write costs about 4 ms in a folder that the antivirus scans
     (measured with Windows Defender: C:\Projects\LightSaber\ToDo\Verdict - slow INI write fix.md, finding F1).
     TMemIniFileEx writes the file once for all its values.
     It has the Read/Write overloads of TIniFileEx (LightCore.INIFile.pas): the section name is given only to the constructor.

  When to use it:
     Where many values are written and nobody needs to read them from the file before the object is freed.
     The values must really change: TIniFileEx.WriteString already skips a value the file holds,
     so a program that rewrites mostly unchanged values (DNA Baser at close: 283 writes, 264 unchanged) stays on TIniFileEx.
     Use TIniFileEx instead when another object or program must see each value at once,
     or when the file holds comment lines that must stay.

  How it saves:
     Several objects may be open on the same file at the same time.
     So the save does not write the memory copy over the file: it reloads the file from disk,
     applies only the changes THIS object made (WriteString, DeleteKey, EraseSection, in their original order), then writes it.
     Changes that other objects saved in the meantime survive.
     An object that changed nothing never writes the file.
     WriteThrough = TRUE saves each change at once, for an object that stays open for a long time.

  The save on Windows - a failed or interrupted save leaves the old file whole:
     1. The new content goes to a temp file <FileName>.<GUID>.tmp in the same folder (ReplaceFile needs one volume), and FlushFileBuffers puts it on the disk.
        Every save picks a new GUID, so two saves at the same moment never write into one temp file.
        A temp file left by a crash stays in the folder: no later save deletes it.
     2. No file yet (first save): the temp file is renamed to FileName.
     3. Otherwise ReplaceFile swaps the temp file in and keeps the old file as <FileName>.TMemIniFileEx-backup, which is deleted after the swap.
        A backup left by an earlier save is deleted before the swap.
     4. A failed write or swap deletes the temp file and raises EIniFileException. The old file stays under its name,
        or, after ReplaceFile error 1177, under the backup name, from where it is renamed back.
     5. A crash between the two renames inside ReplaceFile leaves the old file only as <FileName>.TMemIniFileEx-backup.
        The constructor and UpdateFile rename it back when FileName is missing.
     ReplaceFile, measured on Windows 11 build 26300 (script and output: C:\Projects\LightSaber\Docs\ReplaceFile - measured behaviour\):
     error 2 when the file to replace does not exist, error 5 when it is read-only, error 1175 when the backup is read-only,
     error 32 when another handle holds the file with share mode READ only; an existing backup is overwritten.
     Errors 1176 and 1177 are from Microsoft's page: https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-replacefilew
     A power loss during the save was never tested.
     On the other platforms the save is the plain TMemIniFile.UpdateFile, which empties the file before it writes the new content.

  Limits:
     - A value is on disk only after UpdateFile or Free. Another object or program reads the old value until then.
     - Lines starting with ';' (comments) and blank lines are dropped at the first save (TMemIniFile.SetStrings skips them).
     - Quotes around a value are kept: Key="abc" reads as "abc".
       TIniFile (the Windows API) strips them and reads abc (https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-getprivateprofilestringw).
     - A failed save keeps this object's changes: the next UpdateFile, or Free, saves them again.
       Free tries a failed save 3 times, 50 ms apart, then gives only a log line (TAppDataCore.LogError): the destructor never raises.
       A caller that must react to a failed save calls UpdateFile before Free, inside its own try..except.
     - The file is read in the constructor and again at each save.
       A file that cannot be read (locked, or not valid in its encoding) gives the defaults and a warning in the log.
     - Clear and SetStrings (not virtual in TMemIniFile) are not recorded, so they are never saved.
     - Leading and trailing spaces of a value are trimmed when the file is loaded. A value cannot hold a line break.
     - No lock guards the save (reload, replay, write, swap).
       Two saves of the same file at the same moment (two threads, or two programs) can lose the changes of one of them.
     - On Windows, a program that deletes the INI file to reset its settings must delete <FileName>.TMemIniFileEx-backup too:
       the next TMemIniFileEx renames a leftover backup back to FileName.
     - On Windows the save creates its temp file in the folder of FileName, and ReplaceFile needs DELETE access on FileName.
       So a file that the user may write, in a folder where the user may not create or delete files, cannot be saved. TIniFile can.

  Encoding:
     The file's BOM decides (UTF-8, UTF-16), at the constructor and again at each save. An encoding set through the Encoding property is kept.
     A file without BOM, and a new file, are TEncoding.Default: ANSI on Windows (the same as WritePrivateProfileString writes), UTF-8 on POSIX (System.SysUtils.pas, TEncoding.GetDefault).
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.IniFiles,
   LightCore.INIFile;     { FontStruct }

TYPE
 TMemIniFileEx = class(TMemIniFile)
  private
    type
      TIniChangeKind = (ckWrite, ckDeleteKey, ckEraseSection);
      RIniChange = record
        Kind   : TIniChangeKind;
        Section: string;
        Ident  : string;
        Value  : string;
      end;
    VAR
      FChanges    : array of RIniChange;   { The changes this object made, in order. Replayed over a fresh copy of the file at save time }
      FChangeCount: Integer;
      FEncodingFromFile: TEncoding;            { The encoding the last load found in the file. NIL = no file yet }
    procedure addChange(Kind: TIniChangeKind; CONST Section, Ident, Value: string);
    procedure reloadFile;
    {$IFDEF MSWINDOWS}
    procedure writeTempFile(CONST TempName: string);
    procedure swapInTempFile(CONST TempName: string; FileWasLoaded: Boolean);
    procedure saveViaTempFile(FileWasLoaded: Boolean);
    {$ENDIF}
  protected
    FSection: string;
  public
    { TRUE = every WriteString/DeleteKey/EraseSection is saved to disk at once (one UpdateFile per change).
      For an object that stays open for a long time, so its values are on disk without waiting for Free,
      and it sees what other objects saved meanwhile. Slow: do not use it for a batch of writes. }
    WriteThrough: Boolean;

    constructor Create (CONST SectionName, FileName: string);                    reintroduce; virtual;
    destructor  Destroy; override;

    procedure WriteString (CONST Section, Ident, Value: String);                 override;
    procedure DeleteKey   (CONST Section, Ident: String);                        override;
    procedure EraseSection(CONST Section: string);                               override;
    procedure UpdateFile;                                                        override;

    function  ValueExists(CONST Ident: string): Boolean;                         reintroduce; overload;

    { Data/Time }
    function  ReadDate   (CONST Ident: string; Default: TDateTime): TDateTime;   reintroduce; overload;
    procedure WriteDate  (CONST Ident: string;   Value: TDateTime);              reintroduce; overload;

    { String }
    function  Read       (CONST Ident: string; Default: string): string;         overload;
    procedure Write      (const Ident, Value: String);                           overload;

    { Integer }
    function  Read       (const Ident: string; Default: Integer= 0): Integer;    overload;
    procedure Write      (const Ident: string;   Value: Integer);                overload;

    { Bool }
    function  Read       (CONST Ident: string; Default: Boolean= TRUE): Boolean; overload;
    procedure Write      (const Ident: string;   Value: Boolean);                overload;

    function  ReadFont   (CONST Ident: string): FontStruct;
    procedure WriteFont  (CONST Ident: string; Font: FontStruct);

    { Float }
    function  Read       (const Ident: string; Default: Double): Double;         overload;
    procedure Write      (const Ident: string;   Value: Double);                 overload;
  end;


IMPLEMENTATION

USES
   {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
   System.Classes, System.UITypes, System.IOUtils,
   LightCore.AppData;



{$IFDEF MSWINDOWS}
CONST
  ERROR_UNABLE_TO_MOVE_REPLACEMENT_2 = 1177;   { Not declared in Winapi.Windows. The value is from the ReplaceFile page }

{ A name that no person or other tool picks by chance: a user's own <FileName>.bak is never touched }
function backupNameOf(CONST IniName: string): string;
begin
 Result:= IniName+ '.TMemIniFileEx-backup';
end;


{ A new name at every call, in the folder of IniName: two saves at the same moment never write into one temp file.
  A temp file that a crash left behind stays in the folder: no later save picks its name, and none deletes it. }
function newTempNameOf(CONST IniName: string): string;
begin
 Result:= IniName+ '.'+ TGUID.NewGuid.ToString+ '.tmp';
end;


{ A crash between the two renames inside ReplaceFile leaves the old file only under the backup name.
  When IniName is missing and its backup exists, this renames the backup back to IniName.
  The same two files are also seen, for a moment, while another program is between the two renames of its own ReplaceFile.
  So it waits SettleTime and looks again: a ReplaceFile finishes in that time (measured: about 10 ms per write and swap
  with Windows Defender scanning, C:\Projects\LightSaber\Docs\ReplaceFile - measured behaviour\), a backup left by a crash stays.
  RenameFile (MoveFile) never overwrites, so an IniName that appears meanwhile stays as it is.
  A failed rename raises EIniFileException: a save over the missing file would hold only this object's values,
  and the swap of the save after it would overwrite the backup, the only copy of the old values. }
procedure restoreBackup(CONST IniName: string);
CONST
  SettleTime = 100;   { ms }
begin
 VAR Backup:= backupNameOf(IniName);
 if FileExists(IniName) OR NOT FileExists(Backup) then EXIT;

 Sleep(SettleTime);
 if FileExists(IniName) OR NOT FileExists(Backup) then EXIT;

 if NOT RenameFile(Backup, IniName) then
   begin
     VAR RenameError:= GetLastError;
     if FileExists(IniName) then EXIT;   { Another object restored it first }
     raise EIniFileException.Create('Cannot restore the INI file '+ IniName+ ' from '+ Backup+ '. '+ SysErrorMessage(RenameError));
   end;

 TAppDataCore.LogWarn('TMemIniFileEx: '+ IniName+ ' was missing. Restored it from '+ Backup);
end;
{$ENDIF}




{-------------------------------------------------------------------------------------------------------------
   MAIN
-------------------------------------------------------------------------------------------------------------}
{ A file that cannot be read (locked by another program, or not valid in its encoding) gives an empty memory copy, so every Read returns its default.
  That is what TIniFile does. The file itself is safe: the save reloads it from disk before it writes. }
constructor TMemIniFileEx.Create(CONST SectionName, FileName: string);
begin
 VAR Dir:= ExtractFilePath(FileName);
 if Dir <> ''
 then TDirectory.CreateDirectory(Dir);  // Make sure the path exists otherwise we get an error. Empty Dir = relative filename (current dir) - nothing to create; CreateDirectory('') would raise.

 {$IFDEF MSWINDOWS}
 TRY
   restoreBackup(FileName);
 EXCEPT
   on E: EIniFileException do
     TAppDataCore.LogError(E.Message);   { The load below then gives the defaults, and every save raises until the backup is back }
 END;
 {$ENDIF}

 TRY
   inherited Create(FileName);  { Nil encoding: the BOM decides; no BOM = TEncoding.Default (ANSI on Windows, UTF-8 on POSIX) }
 EXCEPT
   on E: EStreamError do
     TAppDataCore.LogWarn('TMemIniFileEx: cannot read '+ FileName+ ' - using default values. '+ E.Message);
   on E: EEncodingError do
     TAppDataCore.LogWarn('TMemIniFileEx: cannot read '+ FileName+ ' - using default values. '+ E.Message);
 END;
 FEncodingFromFile:= Encoding;

 FSection:= SectionName;
end;


{ The memory copy is written to disk only if this object changed something. AutoSave stays FALSE, so TMemIniFile.Destroy does not save a second time.
  A lock on the file often lasts only milliseconds (another object reads it), so a failed save is tried SaveTries times, RetryPause apart.
  A failure that lasts costs (SaveTries-1) * RetryPause ms, once, plus the wait of restoreBackup at each try when FileName is missing and its backup exists.
  The last failure is logged as an error, not raised.
  A caller that must handle the failure calls UpdateFile itself, before Free. }
destructor TMemIniFileEx.Destroy;
CONST
  SaveTries  = 3;
  RetryPause = 50;   { ms }
begin
 for VAR Attempt:= 1 to SaveTries do
   begin
     if FChangeCount = 0 then BREAK;
     TRY
       UpdateFile;
     EXCEPT
       on E: EIniFileException do
         if Attempt < SaveTries
         then Sleep(RetryPause)
         else TAppDataCore.LogError(E.Message+ ' (tried '+ IntToStr(SaveTries)+ ' times)');
     END;
   end;

 inherited Destroy;
end;


procedure TMemIniFileEx.addChange(Kind: TIniChangeKind; CONST Section, Ident, Value: string);
begin
 if FChangeCount = Length(FChanges)
 then SetLength(FChanges, 64 + 2*Length(FChanges));

 FChanges[FChangeCount].Kind   := Kind;
 FChanges[FChangeCount].Section:= Section;
 FChanges[FChangeCount].Ident  := Ident;
 FChanges[FChangeCount].Value  := Value;
 Inc(FChangeCount);
end;


{ Every Write* of TCustomIniFile (WriteInteger, WriteBool, WriteFloat, WriteDate, WriteBinaryStream...) ends here. }
procedure TMemIniFileEx.WriteString(CONST Section, Ident, Value: String);
begin
 inherited WriteString(Section, Ident, Value);
 addChange(ckWrite, Section, Ident, Value);
 if WriteThrough then UpdateFile;
end;


procedure TMemIniFileEx.DeleteKey(CONST Section, Ident: String);
begin
 inherited DeleteKey(Section, Ident);
 addChange(ckDeleteKey, Section, Ident, '');
 if WriteThrough then UpdateFile;
end;


procedure TMemIniFileEx.EraseSection(CONST Section: string);
begin
 inherited EraseSection(Section);
 addChange(ckEraseSection, Section, '', '');
 if WriteThrough then UpdateFile;
end;


{ Reloads the file from disk, applies this object's changes over it, and writes it.
  So the values that other objects saved to the same file in the meantime survive.
  If the reload fails, nothing is written and EIniFileException is raised. On Windows the write goes through a temp file (saveViaTempFile).
  The change record is emptied only after a successful save. A failed save keeps it, so the next UpdateFile (or Free) saves it again. }
procedure TMemIniFileEx.UpdateFile;
begin
 if FChangeCount = 0 then EXIT;

 TRY
   {$IFDEF MSWINDOWS}
   restoreBackup(FileName);  { Raises EIniFileException when the backup cannot be put back }

   { LoadValues does not say whether it found the file. Looked at before and after it, so a file that is missing for a moment
     (another program between the two renames of its ReplaceFile) counts as not loaded. A window of microseconds stays. }
   VAR FileWasLoaded:= FileExists(FileName);
   reloadFile;
   FileWasLoaded:= FileWasLoaded AND FileExists(FileName);
   {$ELSE}
   reloadFile;
   {$ENDIF}

   for VAR i:= 0 to FChangeCount-1 do
     case FChanges[i].Kind of
       ckWrite       : inherited WriteString (FChanges[i].Section, FChanges[i].Ident, FChanges[i].Value);
       ckDeleteKey   : inherited DeleteKey   (FChanges[i].Section, FChanges[i].Ident);
       ckEraseSection: inherited EraseSection(FChanges[i].Section);
     end;

   {$IFDEF MSWINDOWS}
   saveViaTempFile(FileWasLoaded);
   {$ELSE}
   inherited UpdateFile;
   {$ENDIF}
 EXCEPT
   on E: EStreamError do
     raise EIniFileException.Create('Cannot save INI file: '+ FileName+ '. '+ E.Message);
   on E: EEncodingError do
     raise EIniFileException.Create('Cannot save INI file: '+ FileName+ '. '+ E.Message);
 END;

 FChangeCount:= 0;
 FChanges:= NIL;
end;


{ Reloads the file from disk. On a read error, LoadValues raises before it clears the memory copy.
  TMemIniFile.LoadValues keeps the encoding of the first load for every later load (System.IniFiles.pas, TMemIniFile.LoadValues),
  so a file that gained a BOM meanwhile would be misread: read as ANSI, the UTF-8 BOM glues to the first section header, and that section is lost.
  So the BOM of the file decides again at each reload, as it did in the constructor.
  An encoding the caller set through the Encoding property is kept. }
procedure TMemIniFileEx.reloadFile;
begin
 VAR Detect:= (Encoding = FEncodingFromFile);
 if Detect then Encoding:= NIL;
 TRY
   Rename(FileName, TRUE);
 FINALLY
   if Detect then
     begin
       if Encoding = NIL
       then Encoding:= FEncodingFromFile;   { No file, or a failed load: the encoding of the last load stays }
       FEncodingFromFile:= Encoding;
     end;
 END;
end;


{$IFDEF MSWINDOWS}
{ The temp file sits in the folder of FileName: ReplaceFile needs all its files on one volume.
  On every failure the temp file is deleted and the old file is left as it was. }
procedure TMemIniFileEx.saveViaTempFile(FileWasLoaded: Boolean);
begin
 VAR TempName:= newTempNameOf(FileName);
 VAR Swapped:= FALSE;
 TRY
   writeTempFile(TempName);
   swapInTempFile(TempName, FileWasLoaded);
   Swapped:= TRUE;
 FINALLY
   if NOT Swapped
   then System.SysUtils.DeleteFile(TempName);
 END;
 Modified:= FALSE;
end;


{ Writes the memory copy to TempName as TMemIniFile.UpdateFile writes it to FileName: the same strings, the same Encoding and BOM.
  FlushFileBuffers puts the bytes on the disk before the swap (https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-flushfilebuffers).
  The stream is freed before the swap: ReplaceFile opens the new file with no sharing mode. }
procedure TMemIniFileEx.writeTempFile(CONST TempName: string);
begin
 VAR List:= TStringList.Create;
 TRY
   GetStrings(List);
   VAR Stream:= TFileStream.Create(TempName, fmCreate);
   TRY
     List.SaveToStream(Stream, Encoding);
     if NOT FlushFileBuffers(Stream.Handle) then
       begin
         VAR FlushError:= GetLastError;
         raise EIniFileException.Create('Cannot save INI file: '+ FileName+ '. FlushFileBuffers: '+ SysErrorMessage(FlushError));
       end;
   FINALLY
     FreeAndNil(Stream);
   END;
 FINALLY
   FreeAndNil(List);
 END;
end;


{ Puts TempName in the place of FileName.
  No FileName yet (first save): a rename. ReplaceFile cannot do it: it returns error 2 when the file to replace does not exist (measured).
  Otherwise ReplaceFile keeps the old file as the backup, which is deleted after the swap.
  A backup left by an earlier save is deleted before the swap. ReplaceFile would overwrite one that is not read-only (measured),
  so a backup that another object makes between that delete and the swap does no harm.
  On failure the old file keeps its name (Microsoft's page: error 1176 with a backup name, and every error other than 1177),
  or, after error 1177, it is under the backup name and is renamed back. Then EIniFileException is raised.
  FileWasLoaded FALSE and a FileName that exists now: the file appeared after the reload, so the memory copy holds only this object's changes.
  A swap would replace every other value of the file with nothing, so it raises instead, and the next save merges over the file. }
procedure TMemIniFileEx.swapInTempFile(CONST TempName: string; FileWasLoaded: Boolean);
begin
 if NOT FileExists(FileName) then
   begin
     if RenameFile(TempName, FileName) then EXIT;
     VAR RenameError:= GetLastError;
     raise EIniFileException.Create('Cannot save INI file: '+ FileName+ '. Rename of the temp file: '+ SysErrorMessage(RenameError));
   end;

 if NOT FileWasLoaded
 then raise EIniFileException.Create('Cannot save INI file: '+ FileName+ '. The file appeared during the save. Nothing was written: try again.');

 { A backup left by an earlier save that stopped after its swap. ReplaceFile cannot overwrite a read-only one (error 1175, measured),
   so it is deleted first. Safe: FileName exists, so the backup is not the only copy of the old values. }
 VAR Backup:= backupNameOf(FileName);
 if FileExists(Backup) then
   begin
     FileSetReadOnly(Backup, FALSE);
     if NOT System.SysUtils.DeleteFile(Backup) then
       begin
         VAR DeleteError:= GetLastError;
         raise EIniFileException.Create('Cannot save INI file: '+ FileName+ '. Cannot delete the old backup '+ Backup+ '. '+ SysErrorMessage(DeleteError));
       end;
   end;

 VAR Replaced:=ReplaceFile(PChar(FileName), PChar(TempName), PChar(Backup), REPLACEFILE_IGNORE_MERGE_ERRORS, NIL, NIL);
 VAR ReplaceError:= GetLastError;   { Read at once: the next API call overwrites it }

 if Replaced then
   begin
     if NOT System.SysUtils.DeleteFile(Backup) then
       begin
         VAR DeleteError:= GetLastError;
         TAppDataCore.LogWarn('TMemIniFileEx: saved '+ FileName+ ' but cannot delete its backup '+ Backup+ '. '+ SysErrorMessage(DeleteError));
       end;
     EXIT;
   end;

 if (ReplaceError = ERROR_UNABLE_TO_MOVE_REPLACEMENT_2)
 AND NOT FileExists(FileName)
 AND NOT RenameFile(Backup, FileName)
 then TAppDataCore.LogError('TMemIniFileEx: the old '+ FileName+ ' is now '+ Backup+ '. The next TMemIniFileEx created on this file restores it.');

 raise EIniFileException.Create('Cannot save INI file: '+ FileName+ '. ReplaceFile: '+ SysErrorMessage(ReplaceError));
end;
{$ENDIF}




{---------------
   FONT
---------------}

{ Reads font settings from INI file. Returns defaults if font entry not found. }
function TMemIniFileEx.ReadFont(CONST Ident: string): FontStruct;
begin
 if ValueExists(FSection, Ident+ 'Name')
 then
   begin
     Result.Name  := ReadString (FSection, Ident+ 'Name',  'Arial');
     Result.Color := TColor     (ReadInteger(FSection, Ident+ 'Color', 0));
     Result.Size  := ReadInteger(FSection, Ident+ 'Size',  8);
     Result.Style := TFontStyles(Byte(ReadInteger(FSection, Ident+ 'Style', 0)));
   end
 else
   begin
     { Return sensible defaults }
     Result.Name  := 'Arial';
     Result.Color := TColor(0);   { clBlack }
     Result.Size  := 8;
     Result.Style := [];
   end;
end;

{ Writes font settings to INI file.
  Note: Writes empty string for Ident so TIniFileVCL.ReadFont can find the font by identifier. }
procedure TMemIniFileEx.WriteFont(CONST Ident: string; Font: FontStruct);
begin
  WriteString (FSection, Ident,            '');
  WriteString (FSection, Ident + 'Name',   Font.Name);
  WriteInteger(FSection, Ident + 'Color',  Font.Color);
  WriteInteger(FSection, Ident + 'Size',   Font.Size);
  WriteInteger(FSection, Ident + 'Style',  Byte(Font.Style));
end;



{---------------
   DATE
---------------}
function TMemIniFileEx.ReadDate(const Ident: string; Default: TDateTime): TDateTime;
begin
  Result:= inherited ReadDate(FSection, Ident, Default);
end;

procedure TMemIniFileEx.WriteDate(const Ident: string; Value: TDateTime);
begin
  inherited WriteDate(FSection, Ident, Value);
end;




{----------------------
   DEFAULT OVERLOADS
----------------------}

function TMemIniFileEx.ValueExists(const Ident: string): Boolean;
begin
 Result:= inherited ValueExists(FSection, Ident);
end;



function TMemIniFileEx.Read(const Ident: string; Default: Boolean= TRUE): Boolean;
begin
 Result:= inherited ReadBool(FSection, Ident, Default);
end;

procedure TMemIniFileEx.Write(const Ident: string; Value: Boolean);
begin
 inherited WriteBool(FSection, Ident, Value);
end;



function TMemIniFileEx.Read(CONST Ident: string; Default: Integer= 0): Integer;
begin
 Result:= inherited ReadInteger(FSection, Ident, Default);
end;

procedure TMemIniFileEx.Write(CONST Ident: string; Value: Integer);
begin
  inherited WriteInteger(FSection, Ident, Value);
end;




function TMemIniFileEx.Read(CONST Ident: string; Default: string): string;
begin
 Result:= inherited ReadString(FSection, Ident, Default);
end;

procedure TMemIniFileEx.Write(const Ident, Value: String);
begin
 WriteString(FSection, Ident, Value);   { Not 'inherited': TMemIniFile.WriteString would skip the change record }
end;



function TMemIniFileEx.Read(const Ident: string; Default: Double): Double;
begin
 Result:= inherited ReadFloat(FSection, Ident, Default);
end;


procedure TMemIniFileEx.Write(const Ident: string; Value: Double);
begin
 inherited WriteFloat(FSection, Ident, Value);
end;



end.
