unit Test.LightCore.INIFileMem;

{=============================================================================================================
   Unit tests for LightCore.INIFileMem.pas
   TMemIniFileEx saves once, at Free or UpdateFile, merging its own changes over the file on disk.
   On Windows the save goes through a temp file <FileName>.<random>.tmp and ReplaceFile.

   The class deletes files (its temp file, <FileName>.TMemIniFileEx-backup), so every test works in a folder of its own under C:\test delete\
   (rule: C:\Projects\.claude\rules\delphi-code.md, section "Unit Testing").
   TearDown deletes only the files directly inside that folder (the class makes no subfolders), then the folder.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.IniFiles,
  LightCore.INIFileMem;

type
  { How TMemIniFileEx saves: once, at Free, merging its own changes over the file on disk. }
  [TestFixture]
  TTestMemIniFileExSave = class
  private
    FFolder: string;
    FFile  : string;
    function NewIni(CONST Section: string = 'S'): TMemIniFileEx;
    function ReadBack(CONST Section, Ident: string): string;
    function TempFilesLeft: Integer;
    function BackupFile: string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure FileIsNotWrittenBeforeFree;
    [Test]
    procedure ValuesSurviveFree;
    [Test]
    procedure TwoObjectsOpenAtOnce_KeepBothSets;
    [Test]
    procedure ChangesOfAnObjectFreedEarlier_Survive;
    [Test]
    procedure SameKey_LastFreedWins;
    [Test]
    procedure DeleteKey_IsReplayed;
    [Test]
    procedure EraseSection_IsReplayed;
    [Test]
    procedure ObjectThatOnlyReads_DoesNotRewriteFile;
    [Test]
    procedure LongValue_Over2047Chars_RoundTrips;
    [Test]
    procedure Save400Values_Under50ms;
    [Test]
    procedure Utf16File_StaysUtf16;
    [Test]
    procedure NewFile_IsAnsiWithoutBOM;
    [Test]
    procedure UpdateFile_ThenFree_WritesOnce;
    [Test]
    procedure LockedFile_UpdateFileRaises_FreeSavesAfterLockIsGone;
    [Test]
    procedure LockedFile_FreeWithChanges_DoesNotRaise;
    [Test]
    procedure FreeRetriesAFailedSave;
    [Test]
    procedure FreeGivesUpAfterAFewTries;
    [Test]
    procedure FileGainsUtf8Bom_FirstSectionSurvives;
    [Test]
    procedure LockedFile_Create_GivesDefaults;
    [Test]
    procedure WriteThrough_ValueOnDiskBeforeFree;
    [Test]
    procedure WriteThrough_DeleteKeyAndEraseSection_OnDiskBeforeFree;
    [Test]
    procedure ReplayOrder_SameKeyTwice_LastWriteWins;
    [Test]
    procedure ReplayOrder_EraseSectionThenWrite_KeepsNewKey;
    [Test]
    procedure InvalidUtf8File_CreateGivesDefaults_SaveRaisesAndKeepsBytes;
    [Test]
    procedure Font_RoundTrip_AndValueExists;
    [Test]
    procedure DateAndDouble_RoundTrip;

    { The save through a temp file }
    [Test]
    procedure FirstSave_NoFile_CreatesOnlyTheIniFile;
    {$IFDEF MSWINDOWS}
    [Test]
    procedure FailedSave_ReadOnlyFile_KeepsOldBytes;
    [Test]
    procedure WriteFailsHalfway_KeepsOldBytes;
    [Test]
    procedure MissingFile_BackupIsRestoredAtCreate;
    [Test]
    procedure MissingFile_BackupIsRestoredAtSave;
    [Test]
    procedure LeftoverTmp_DoesNotStopSave_LeftoverBakIsGone;
    [Test]
    procedure ManySaves_LeaveOnlyTheIniFile;
    [Test]
    procedure MissingFile_BackupLocked_SaveRaisesAndKeepsBackup;
    [Test]
    procedure ShareReadLockDuringSave_ChangesSurviveNextSave;
    [Test]
    procedure UsersOwnBakFile_SurvivesSave;
    [Test]
    procedure FileAppearsDuringSave_RaisesAndKeepsItsValues;
    [Test]
    procedure ReadOnlyLeftoverBackup_DoesNotBlockSave;
    [Test]
    procedure LockedLeftoverBackup_ErrorNamesTheBackup;
    [Test]
    procedure TwoSaves_UseDifferentTempNames;
    {$ENDIF}
  end;

implementation

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.Diagnostics, System.UITypes,
  LightCore.INIFile,    { FontStruct, TIniFileEx }
  LightCore.AppData;

CONST
  TestRoot  = 'C:\test delete\';
  RunPrefix = 'TestMemIniFileEx_';


{$IFDEF MSWINDOWS}
TYPE
  { An encoding whose GetBytes raises EWriteError, the exception TStream.WriteBuffer raises when the disk is full (System.Classes.pas, TStream.WriteBuffer).
    Reading through it works, so the reload at the start of the save succeeds and the write of the new content fails. }
  TWriteFailsEncoding = class(TMBCSEncoding)
  strict protected
    function GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer; override;
  end;

function TWriteFailsEncoding.GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer;
begin
  raise EWriteError.Create('Simulated write error (disk full)');
end;


TYPE
  { An encoding that, the first time it writes, creates the INI file with a value of another program.
    The save calls it while it writes the temp file: after the reload, before the swap.
    So the file is missing at the reload and present at the swap, as when another program is between the two renames of its ReplaceFile. }
  TCreatesIniEncoding = class(TMBCSEncoding)
  strict protected
    function GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer; override;
  public
    IniToCreate: string;
    Created: Boolean;
  end;

function TCreatesIniEncoding.GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer;
begin
  if NOT Created then
    begin
      Created:= TRUE;
      TFile.WriteAllText(IniToCreate, '[S]'#13#10'Other=Kept'#13#10, TEncoding.ANSI);
    end;
  Result:= inherited GetBytes(Chars, CharCount, Bytes, ByteCount);
end;


TYPE
  { An encoding that, each time it writes, records the temp file the save is writing.
    The save calls it while the temp file is open: TFileStream.Create(fmCreate) has already made the file. }
  TRecordsTempNameEncoding = class(TMBCSEncoding)
  strict protected
    function GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer; override;
  public
    IniName : string;
    TempSeen: string;    { The temp file in the folder at the last write. '' = none, '<several>' = more than one }
  end;

function TRecordsTempNameEncoding.GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer;
begin
  TempSeen:= '';
  for VAR FileName in TDirectory.GetFiles(ExtractFilePath(IniName)) do
    if FileName.StartsWith(IniName + '.', TRUE) AND FileName.EndsWith('.tmp', TRUE) then
      if TempSeen = ''
      then TempSeen:= FileName
      else TempSeen:= '<several>';
  Result:= inherited GetBytes(Chars, CharCount, Bytes, ByteCount);
end;
{$ENDIF}


TYPE
  { Its UpdateFile fails FailsLeft times with EIniFileException, the exception a locked file gives, then saves for real.
    OnUpdateFile runs at every UpdateFile, so a test can count how often the destructor tried. }
  TFailingSaveIni = class(TMemIniFileEx)
  public
    FailsLeft: Integer;
    OnUpdateFile: TProc;
    procedure UpdateFile; override;
  end;

procedure TFailingSaveIni.UpdateFile;
begin
  if Assigned(OnUpdateFile) then OnUpdateFile();
  if FailsLeft > 0 then
    begin
      Dec(FailsLeft);
      raise EIniFileException.Create('Simulated save failure (file locked for a moment)');
    end;
  inherited UpdateFile;
end;


function SameBytes(CONST A, B: TBytes): Boolean;
begin
  Result:= (Length(A) = Length(B))
       AND ((Length(A) = 0) OR CompareMem(@A[0], @B[0], Length(A)));
end;

{ For the message of a failed byte comparison }
function BytesToHex(CONST Bytes: TBytes): string;
begin
  Result:= '';
  for VAR B in Bytes do
    Result:= Result + IntToHex(B, 2) + ' ';
end;




procedure TTestMemIniFileExSave.Setup;
begin
  FFolder:= TestRoot + RunPrefix + TGUID.NewGuid.ToString;
  TDirectory.CreateDirectory(FFolder);
  FFile:= TPath.Combine(FFolder, 'Test.ini');
end;

{ Deletes only the files directly inside the folder this run created, then the folder }
procedure TTestMemIniFileExSave.TearDown;
begin
  if NOT FFolder.StartsWith(TestRoot + RunPrefix, TRUE)
  OR NOT TDirectory.Exists(FFolder)
  then EXIT;

  VAR Prefix:= IncludeTrailingPathDelimiter(FFolder);
  for VAR FileName in TDirectory.GetFiles(FFolder) do
    if FileName.StartsWith(Prefix, TRUE) then
      begin
        {$IFDEF MSWINDOWS}
        SetFileAttributes(PChar(FileName), FILE_ATTRIBUTE_NORMAL);   { One test makes the INI file read-only }
        {$ENDIF}
        System.SysUtils.DeleteFile(FileName);
      end;
  RemoveDir(FFolder);
end;

function TTestMemIniFileExSave.NewIni(CONST Section: string = 'S'): TMemIniFileEx;
begin
  Result:= TMemIniFileEx.Create(Section, FFile);
end;

{ Reads a value through a NEW object, so it comes from the file on disk }
function TTestMemIniFileExSave.ReadBack(CONST Section, Ident: string): string;
begin
  VAR Ini:= NewIni;
  TRY
    Result:= Ini.ReadString(Section, Ident, '<missing>');
  FINALLY
    FreeAndNil(Ini);
  END;
end;

{ How many temp files of a save are in the folder: every file named <FileName>.*.tmp.
  Each save picks a random temp name, so the test cannot know the name in advance. }
function TTestMemIniFileExSave.TempFilesLeft: Integer;
begin
  Result:= 0;
  for VAR FileName in TDirectory.GetFiles(FFolder) do
    if FileName.StartsWith(FFile + '.', TRUE) AND FileName.EndsWith('.tmp', TRUE)
    then Inc(Result);
end;

{ The name the class uses for the old file during a swap. Not <FileName>.bak: a user's own copy of that name must survive (UsersOwnBakFile_SurvivesSave) }
function TTestMemIniFileExSave.BackupFile: string;
begin
  Result:= FFile + '.TMemIniFileEx-backup';
end;


{ RED on TIniFile: WritePrivateProfileString creates the file at the first write }
procedure TTestMemIniFileExSave.FileIsNotWrittenBeforeFree;
begin
  VAR Ini:= NewIni;
  TRY
    Ini.Write('A', 'One');
    Assert.IsFalse(FileExists(FFile), 'The file must not exist before Free');
  FINALLY
    FreeAndNil(Ini);
  END;
  Assert.IsTrue(FileExists(FFile), 'Free must write the file');
  Assert.AreEqual('One', ReadBack('S', 'A'));
end;

procedure TTestMemIniFileExSave.ValuesSurviveFree;
begin
  VAR Ini:= NewIni;
  TRY
    Ini.Write('Str', 'Hello');
    Ini.Write('Int', 42);
    Ini.Write('Bool', FALSE);
    Ini.WriteFloat('S', 'Dbl', 1.5);
  FINALLY
    FreeAndNil(Ini);
  END;

  Ini:= NewIni;
  TRY
    Assert.AreEqual('Hello', Ini.Read('Str', ''));
    Assert.AreEqual(42,      Ini.Read('Int', 0));
    Assert.IsFalse(Ini.Read('Bool', TRUE));
    Assert.AreEqual(1.5, Ini.ReadFloat('S', 'Dbl', 0), 0.0001);
  FINALLY
    FreeAndNil(Ini);
  END;
end;

{ Each form opens its own object on the one INI file of the program. A snapshot save of B would delete what A wrote. }
procedure TTestMemIniFileExSave.TwoObjectsOpenAtOnce_KeepBothSets;
begin
  VAR IniA:= NewIni('FormA');
  VAR IniB:= NewIni('FormB');
  TRY
    IniA.Write('KeyA', 'ValueA');
    IniB.Write('KeyB', 'ValueB');
    IniB.WriteString('FormA', 'KeyB2', 'ValueB2');   { Same section as A, other key }
  FINALLY
    FreeAndNil(IniA);
    FreeAndNil(IniB);
  END;

  Assert.AreEqual('ValueA',  ReadBack('FormA', 'KeyA'));
  Assert.AreEqual('ValueB',  ReadBack('FormB', 'KeyB'));
  Assert.AreEqual('ValueB2', ReadBack('FormA', 'KeyB2'));
end;

{ Like LightCore.INIFileQuick: a short-lived object writes and is freed while a long-lived one is still open }
procedure TTestMemIniFileExSave.ChangesOfAnObjectFreedEarlier_Survive;
begin
  VAR Outer:= NewIni;
  TRY
    Outer.Write('Outer', 'O');

    VAR Inner:= NewIni;
    TRY
      Inner.Write('Inner', 'I');
    FINALLY
      FreeAndNil(Inner);
    END;
    Assert.AreEqual('I', ReadBack('S', 'Inner'), 'Inner must be on disk once freed');
  FINALLY
    FreeAndNil(Outer);
  END;

  Assert.AreEqual('O', ReadBack('S', 'Outer'));
  Assert.AreEqual('I', ReadBack('S', 'Inner'), 'Outer''s save must keep Inner''s value');
end;

procedure TTestMemIniFileExSave.SameKey_LastFreedWins;
begin
  VAR IniA:= NewIni;
  VAR IniB:= NewIni;
  TRY
    IniA.Write('K', 'FromA');
    IniB.Write('K', 'FromB');
    FreeAndNil(IniB);
  FINALLY
    FreeAndNil(IniA);
    FreeAndNil(IniB);
  END;
  Assert.AreEqual('FromA', ReadBack('S', 'K'));
end;

procedure TTestMemIniFileExSave.DeleteKey_IsReplayed;
begin
  VAR Ini:= NewIni;
  TRY
    Ini.Write('K1', '1');
    Ini.Write('K2', '2');
  FINALLY
    FreeAndNil(Ini);
  END;

  VAR IniA:= NewIni;
  TRY
    IniA.DeleteKey('S', 'K1');
    VAR IniB:= NewIni;
    TRY
      IniB.Write('K3', '3');
    FINALLY
      FreeAndNil(IniB);
    END;
  FINALLY
    FreeAndNil(IniA);
  END;

  Assert.AreEqual('<missing>', ReadBack('S', 'K1'));
  Assert.AreEqual('2', ReadBack('S', 'K2'));
  Assert.AreEqual('3', ReadBack('S', 'K3'));
end;

procedure TTestMemIniFileExSave.EraseSection_IsReplayed;
begin
  VAR Ini:= NewIni;
  TRY
    Ini.WriteString('Gone', 'K', '1');
    Ini.WriteString('Kept', 'K', '2');
  FINALLY
    FreeAndNil(Ini);
  END;

  Ini:= NewIni;
  TRY
    Ini.EraseSection('Gone');
  FINALLY
    FreeAndNil(Ini);
  END;

  Assert.AreEqual('<missing>', ReadBack('Gone', 'K'));
  Assert.AreEqual('2', ReadBack('Kept', 'K'));
end;

{ TMemIniFile drops ';' comment lines when it saves. An object that only reads must leave the file as it is. }
procedure TTestMemIniFileExSave.ObjectThatOnlyReads_DoesNotRewriteFile;
CONST
  Content = '; a comment'#13#10'[S]'#13#10'K=V'#13#10;
begin
  TFile.WriteAllText(FFile, Content, TEncoding.ANSI);
  VAR Before:= TFile.ReadAllBytes(FFile);

  VAR Ini:= NewIni;
  TRY
    Assert.AreEqual('V', Ini.Read('K', ''));
  FINALLY
    FreeAndNil(Ini);
  END;

  Assert.IsTrue(SameBytes(Before, TFile.ReadAllBytes(FFile)), 'File content changed');
end;

{ TIniFile.ReadString cuts values at 2047 chars (fixed buffer in System.IniFiles.pas). TMemIniFile reads whole lines. }
procedure TTestMemIniFileExSave.LongValue_Over2047Chars_RoundTrips;
begin
  VAR Long:= StringOfChar('x', 5000) + 'END';
  VAR Ini:= NewIni;
  TRY
    Ini.Write('Long', Long);
  FINALLY
    FreeAndNil(Ini);
  END;
  Assert.AreEqual(Long, ReadBack('S', 'Long'));
end;

{ A program that saves its settings at close writes hundreds of values. With one WritePrivateProfileString per value this costs about 4 ms each in a folder the antivirus scans. }
procedure TTestMemIniFileExSave.Save400Values_Under50ms;
begin
  VAR Watch:= TStopwatch.StartNew;
  VAR Ini:= NewIni;
  TRY
    for VAR i:= 1 to 400 do
      Ini.WriteString('Section'+ IntToStr(i mod 40), 'Key'+ IntToStr(i), 'Value'+ IntToStr(i));
  FINALLY
    FreeAndNil(Ini);
  END;
  Watch.Stop;

  Assert.IsTrue(Watch.ElapsedMilliseconds < 50, 'Took '+ IntToStr(Watch.ElapsedMilliseconds)+ ' ms');

  { All 400, not only the last: the change record grows past its first 64 entries here, and a growth bug would drop the older ones }
  Ini:= NewIni;
  TRY
    for VAR i:= 1 to 400 do
      Assert.AreEqual('Value'+ IntToStr(i), Ini.ReadString('Section'+ IntToStr(i mod 40), 'Key'+ IntToStr(i), '<missing>'), 'Key'+ IntToStr(i));
  FINALLY
    FreeAndNil(Ini);
  END;
end;

procedure TTestMemIniFileExSave.Utf16File_StaysUtf16;
begin
  TFile.WriteAllText(FFile, '[S]'#13#10'Old=Ąș'#13#10, TEncoding.Unicode);   { WriteAllText writes the BOM FF FE }

  VAR Ini:= NewIni;
  TRY
    Ini.Write('New', 'Țară');
  FINALLY
    FreeAndNil(Ini);
  END;

  VAR Bytes:= TFile.ReadAllBytes(FFile);
  Assert.IsTrue(Length(Bytes) > 2);
  Assert.AreEqual(Byte($FF), Bytes[0], 'BOM byte 1');
  Assert.AreEqual(Byte($FE), Bytes[1], 'BOM byte 2');
  Assert.AreEqual('Ąș',   ReadBack('S', 'Old'));
  Assert.AreEqual('Țară', ReadBack('S', 'New'));
end;

{ WritePrivateProfileString creates a new file as ANSI. Other programs that read the file with TIniFile expect that.
  The value holds a non-ASCII character, so a file saved as UTF-8 or UTF-16 (with or without BOM) gives other bytes.
  The expected bytes come from the RTL's ANSI encoding (the system code page), not from the class under test.
  TMemIniFile.GetStrings ends each section with an empty line (System.IniFiles.pas, TMemIniFile.GetStrings). }
procedure TTestMemIniFileExSave.NewFile_IsAnsiWithoutBOM;
CONST
  eAcute = #$00E9;
begin
  {$IFDEF MSWINDOWS}
  if (GetACP = CP_UTF8) OR (Length(TEncoding.ANSI.GetBytes(eAcute)) <> 1) then
    begin
      Assert.Pass('The system code page '+ IntToStr(GetACP)+ ' has no single-byte "é": this PC cannot tell ANSI from UTF-8');
      EXIT;
    end;
  {$ENDIF}

  VAR Ini:= NewIni;
  TRY
    Ini.Write('K', eAcute);
  FINALLY
    FreeAndNil(Ini);
  END;

  VAR Expected:= TEncoding.ANSI.GetBytes('[S]'#13#10'K='+ eAcute+ #13#10#13#10);
  VAR Actual  := TFile.ReadAllBytes(FFile);
  Assert.IsTrue(SameBytes(Expected, Actual), 'A new file must be ANSI, no BOM. Expected: '+ BytesToHex(Expected)+ ' Actual: '+ BytesToHex(Actual));
end;

{ UpdateFile writes and empties the change record, so Free writes nothing more: a change made on disk in between survives }
procedure TTestMemIniFileExSave.UpdateFile_ThenFree_WritesOnce;
begin
  VAR Ini:= NewIni;
  TRY
    Ini.Write('K', 'Mine');
    Ini.UpdateFile;
    Assert.AreEqual('Mine', ReadBack('S', 'K'), 'UpdateFile must write');

    TFile.WriteAllText(FFile, '[S]'#13#10'K=Changed on disk'#13#10, TEncoding.ANSI);
  FINALLY
    FreeAndNil(Ini);
  END;
  Assert.AreEqual('Changed on disk', ReadBack('S', 'K'));
end;

CONST
  LockedContent = '[S]'#13#10'K=OnDisk'#13#10;

{ A file held open with fmShareExclusive (another program): the save cannot reload it.
  The failed UpdateFile keeps the change record, so Free, after the lock is gone, saves K=Mine.
  A lock often lasts only milliseconds (another object that reads the file): dropping the changes would lose the whole batch. }
procedure TTestMemIniFileExSave.LockedFile_UpdateFileRaises_FreeSavesAfterLockIsGone;
begin
  TFile.WriteAllText(FFile, LockedContent, TEncoding.ANSI);
  VAR Ini:= NewIni;
  TRY
    Ini.Write('K', 'Mine');
    VAR Lock:= TFileStream.Create(FFile, fmOpenReadWrite or fmShareExclusive);
    TRY
      Assert.WillRaise(procedure begin Ini.UpdateFile; end, EIniFileException);
    FINALLY
      FreeAndNil(Lock);
    END;
    Assert.WillNotRaise(procedure begin FreeAndNil(Ini); end);
  FINALLY
    FreeAndNil(Ini);
  END;
  Assert.AreEqual('Mine', ReadBack('S', 'K'), 'Free after a failed UpdateFile must save the kept changes');
end;

{ Free must log the failure, not raise it }
procedure TTestMemIniFileExSave.LockedFile_FreeWithChanges_DoesNotRaise;
begin
  TFile.WriteAllText(FFile, LockedContent, TEncoding.ANSI);
  VAR Ini:= NewIni;
  VAR Lock:= TFileStream.Create(FFile, fmOpenReadWrite or fmShareExclusive);
  TRY
    Ini.Write('K', 'Mine');
    Assert.WillNotRaise(procedure begin FreeAndNil(Ini); end);
  FINALLY
    FreeAndNil(Lock);
    FreeAndNil(Ini);
  END;
  Assert.AreEqual('OnDisk', ReadBack('S', 'K'));
end;

{ A save at close that meets a lock of a few milliseconds: Free tries again, so the changes reach the disk }
procedure TTestMemIniFileExSave.FreeRetriesAFailedSave;
begin
  VAR Ini:= TFailingSaveIni.Create('S', FFile);
  TRY
    Ini.FailsLeft:= 1;
    Ini.Write('K', 'Mine');
    Assert.WillNotRaise(procedure begin FreeAndNil(Ini); end);
  FINALLY
    FreeAndNil(Ini);
  END;
  Assert.AreEqual('Mine', ReadBack('S', 'K'), 'Free must try the save again after one failure');
end;

{ A failure that lasts (a read-only file): Free stops after a few tries, does not raise, and writes nothing.
  The count lives outside the object, because the object is gone when Free returns. }
procedure TTestMemIniFileExSave.FreeGivesUpAfterAFewTries;
CONST
  NeverSaves = 1000;
begin
  VAR Calls: Integer:= 0;
  VAR Ini:= TFailingSaveIni.Create('S', FFile);
  TRY
    Ini.FailsLeft:= NeverSaves;
    Ini.OnUpdateFile:= procedure begin Inc(Calls); end;
    Ini.Write('K', 'Mine');
    Assert.WillNotRaise(procedure begin FreeAndNil(Ini); end);
  FINALLY
    FreeAndNil(Ini);
  END;
  Assert.IsTrue(Calls > 1,   'Free must try again after a failed save. Tries: '+ IntToStr(Calls));
  Assert.IsTrue(Calls <= 10, 'Free must give up after a few tries. Tries: '+ IntToStr(Calls));
  Assert.IsFalse(FileExists(FFile), 'A save that always fails must write nothing');
end;

{ An object created on a file without BOM. Then an editor saves the file with a UTF-8 BOM.
  The reload at save time must see the BOM: read as ANSI, the 3 BOM bytes glue to "[S]", the first section header is lost,
  and the save writes the file without the values of that section (System.IniFiles.pas, TMemIniFile.SetStrings). }
procedure TTestMemIniFileExSave.FileGainsUtf8Bom_FirstSectionSurvives;
begin
  TFile.WriteAllText(FFile, '[S]'#13#10'K=1'#13#10, TEncoding.ANSI);

  VAR Ini:= NewIni;
  TRY
    TFile.WriteAllText(FFile, '[S]'#13#10'K=1'#13#10'Other=2'#13#10, TEncoding.UTF8);   { WriteAllText writes the BOM EF BB BF }
    VAR Bytes:= TFile.ReadAllBytes(FFile);
    Assert.IsTrue((Length(Bytes) > 3) AND (Bytes[0] = $EF) AND (Bytes[1] = $BB) AND (Bytes[2] = $BF), 'Setup: the file must start with a UTF-8 BOM');

    Ini.Write('New', '3');
  FINALLY
    FreeAndNil(Ini);
  END;

  Assert.AreEqual('2', ReadBack('S', 'Other'), 'The values the editor saved in the first section must survive');
  Assert.AreEqual('3', ReadBack('S', 'New'));
end;

{ A long-lived object sets WriteThrough, so each value is on disk at once,
  and a value another object saved meanwhile survives its own later saves }
procedure TTestMemIniFileExSave.WriteThrough_ValueOnDiskBeforeFree;
begin
  VAR Ini:= NewIni;
  TRY
    Ini.WriteThrough:= TRUE;
    Ini.Write('A', 'One');
    Assert.AreEqual('One', ReadBack('S', 'A'), 'WriteThrough: the value must be on disk before Free');

    VAR Other:= NewIni;
    TRY
      Other.Write('B', 'Two');
    FINALLY
      FreeAndNil(Other);
    END;

    Ini.Write('C', 'Three');
    Assert.AreEqual('Two', Ini.Read('B', ''), 'The reload at each save must show the value of the other object');
  FINALLY
    FreeAndNil(Ini);
  END;
  Assert.AreEqual('Two',   ReadBack('S', 'B'));
  Assert.AreEqual('Three', ReadBack('S', 'C'));
end;

{ TIniFile cannot raise in Create. TMemIniFileEx must not either: it logs and gives the defaults }
procedure TTestMemIniFileExSave.LockedFile_Create_GivesDefaults;
begin
  TFile.WriteAllText(FFile, LockedContent, TEncoding.ANSI);
  VAR Lock:= TFileStream.Create(FFile, fmOpenReadWrite or fmShareExclusive);
  TRY
    VAR Ini: TMemIniFileEx:= NIL;
    TRY
      Assert.WillNotRaise(procedure begin Ini:= NewIni; end);
      Assert.AreEqual('Default', Ini.Read('K', 'Default'));
    FINALLY
      FreeAndNil(Ini);
    END;
  FINALLY
    FreeAndNil(Lock);
  END;
end;


{ WriteThrough must save a DeleteKey and an EraseSection at once too, not only a WriteString }
procedure TTestMemIniFileExSave.WriteThrough_DeleteKeyAndEraseSection_OnDiskBeforeFree;
begin
  TFile.WriteAllText(FFile, '[S]'#13#10'A=1'#13#10'B=2'#13#10'[Gone]'#13#10'K=3'#13#10, TEncoding.ANSI);
  Assert.AreEqual('1', ReadBack('S', 'A'),    'Setup: A must be on disk');
  Assert.AreEqual('3', ReadBack('Gone', 'K'), 'Setup: Gone.K must be on disk');

  VAR Ini:= NewIni;
  TRY
    Ini.WriteThrough:= TRUE;

    Ini.DeleteKey('S', 'A');
    Assert.AreEqual('<missing>', ReadBack('S', 'A'), 'WriteThrough: the deleted key must be gone from disk before Free');

    Ini.EraseSection('Gone');
    Assert.AreEqual('<missing>', ReadBack('Gone', 'K'), 'WriteThrough: the erased section must be gone from disk before Free');
    Assert.AreEqual('2', ReadBack('S', 'B'), 'The other key must stay');
  FINALLY
    FreeAndNil(Ini);
  END;
end;


{ The save replays the changes in the order they were made: the second Write of the same key wins }
procedure TTestMemIniFileExSave.ReplayOrder_SameKeyTwice_LastWriteWins;
begin
  VAR Ini:= NewIni;
  TRY
    Ini.Write('K', '1');
    Ini.Write('K', '2');
  FINALLY
    FreeAndNil(Ini);
  END;
  Assert.AreEqual('2', ReadBack('S', 'K'));
end;


{ EraseSection, then a Write into the same section: the section ends up holding only the new key.
  Replayed in the wrong order, the new key would be erased; with the erase not replayed, Old would survive. }
procedure TTestMemIniFileExSave.ReplayOrder_EraseSectionThenWrite_KeepsNewKey;
begin
  TFile.WriteAllText(FFile, '[S]'#13#10'Old=1'#13#10, TEncoding.ANSI);

  VAR Ini:= NewIni;
  TRY
    Ini.EraseSection('S');
    Ini.Write('K', 'New');
  FINALLY
    FreeAndNil(Ini);
  END;

  Assert.AreEqual('<missing>', ReadBack('S', 'Old'), 'The erase must be replayed');
  Assert.AreEqual('New',       ReadBack('S', 'K'),   'The Write after the erase must survive');
end;


{ A file with a UTF-8 BOM and an invalid UTF-8 byte ($FF never occurs in UTF-8).
  TEncoding.UTF8 is created with MB_ERR_INVALID_CHARS, so the load raises EEncodingError (System.SysUtils.pas, TUTF8Encoding.Create and TEncoding.GetString).
  The file is not locked, so only the failed reload can stop the save from replacing it. }
procedure TTestMemIniFileExSave.InvalidUtf8File_CreateGivesDefaults_SaveRaisesAndKeepsBytes;
CONST
  Content: array[0..12] of Byte = ($EF, $BB, $BF, Ord('['), Ord('S'), Ord(']'), 13, 10, Ord('K'), Ord('='), $FF, 13, 10);
begin
  VAR Before: TBytes;
  SetLength(Before, Length(Content));
  Move(Content[0], Before[0], Length(Content));
  TFile.WriteAllBytes(FFile, Before);

  VAR Ini: TMemIniFileEx:= NIL;
  TRY
    Assert.WillNotRaise(procedure begin Ini:= NewIni; end, NIL, 'Create must not raise on a file that is not valid UTF-8');
    Assert.AreEqual('Default', Ini.Read('K', 'Default'), 'Create must give the defaults');

    Ini.Write('K', 'New');
    Assert.WillRaise(procedure begin Ini.UpdateFile; end, EIniFileException);
  FINALLY
    FreeAndNil(Ini);
  END;

  Assert.IsTrue(SameBytes(Before, TFile.ReadAllBytes(FFile)), 'A failed reload must leave the file as it was');
  Assert.AreEqual(0, TempFilesLeft, 'Temp file left behind');
end;


{ ReadFont, WriteFont and ValueExists(Ident), read back through a new object, so from the file }
procedure TTestMemIniFileExSave.Font_RoundTrip_AndValueExists;
begin
  VAR Font: FontStruct;
  Font.Name := 'Consolas';
  Font.Size := 13;
  Font.Style:= [TFontStyle.fsBold, TFontStyle.fsItalic];   { System.UITypes declares TFontStyle with SCOPEDENUMS ON }
  Font.Color:= TColor($123456);

  VAR Ini:= NewIni;
  TRY
    Ini.WriteFont('Font', Font);
  FINALLY
    FreeAndNil(Ini);
  END;

  { The raw keys on disk. Style is the set as a byte: fsBold = bit 0, fsItalic = bit 1 (System.UITypes.pas, TFontStyle) }
  Assert.AreEqual('Consolas', ReadBack('S', 'FontName'));
  Assert.AreEqual('13',       ReadBack('S', 'FontSize'));
  Assert.AreEqual('3',        ReadBack('S', 'FontStyle'));
  Assert.AreEqual('1193046',  ReadBack('S', 'FontColor'));   { $123456 }

  Ini:= NewIni;
  TRY
    Assert.IsTrue (Ini.ValueExists('FontName'), 'ValueExists must see a key that is on disk');
    Assert.IsFalse(Ini.ValueExists('NoSuchKey'), 'ValueExists must not see a missing key');

    VAR Back:= Ini.ReadFont('Font');
    Assert.AreEqual('Consolas', Back.Name);
    Assert.AreEqual(13, Back.Size);
    Assert.IsTrue(Back.Style = [TFontStyle.fsBold, TFontStyle.fsItalic], 'Style');
    Assert.AreEqual(Integer($123456), Integer(Back.Color));

    { A missing font gives the same defaults as TIniFileEx, the class TMemIniFileEx copies its Read/Write overloads from }
    VAR Missing:= Ini.ReadFont('NoFont');
    VAR Ref:= TIniFileEx.Create('S', FFile);
    TRY
      VAR RefMissing:= Ref.ReadFont('NoFont');
      Assert.AreEqual(RefMissing.Name, Missing.Name);
      Assert.AreEqual(RefMissing.Size, Missing.Size);
      Assert.IsTrue(RefMissing.Style = Missing.Style, 'Default style');
      Assert.AreEqual(Integer(RefMissing.Color), Integer(Missing.Color));
    FINALLY
      FreeAndNil(Ref);
    END;
  FINALLY
    FreeAndNil(Ini);
  END;
end;


{ ReadDate, WriteDate and the Double overloads of Read and Write, read back through a new object.
  The Double default must be typed: a plain 0 binds to the Integer overload. }
procedure TTestMemIniFileExSave.DateAndDouble_RoundTrip;
begin
  VAR Day  : TDateTime:= EncodeDate(2026, 3, 15);
  VAR Dbl  : Double:= 2.25;
  VAR NoDbl: Double:= -1;

  VAR Ini:= NewIni;
  TRY
    Ini.WriteDate('Date', Day);
    Ini.Write('Dbl', Dbl);
  FINALLY
    FreeAndNil(Ini);
  END;

  Ini:= NewIni;
  TRY
    Assert.AreEqual(Double(EncodeDate(2026, 3, 15)), Double(Ini.ReadDate('Date', 0)), 0.0001, 'Date');
    Assert.AreEqual(2.25, Ini.Read('Dbl', NoDbl), 0.0001, 'Double');
    Assert.AreEqual(-1.0, Ini.Read('NoSuchDbl', NoDbl), 0.0001, 'A missing Double must give the default');
  FINALLY
    FreeAndNil(Ini);
  END;
end;




{-------------------------------------------------------------------------------------------------------------
   THE SAVE THROUGH A TEMP FILE
-------------------------------------------------------------------------------------------------------------}

{ ReplaceFile returns error 2 when the file to replace does not exist (measured), so the first save renames the temp file into place }
procedure TTestMemIniFileExSave.FirstSave_NoFile_CreatesOnlyTheIniFile;
begin
  Assert.IsFalse(FileExists(FFile), 'Setup must start with no INI file');
  VAR Ini:= NewIni;
  TRY
    Ini.Write('K', 'First');
  FINALLY
    FreeAndNil(Ini);
  END;

  Assert.AreEqual('First', ReadBack('S', 'K'));
  Assert.AreEqual(0, TempFilesLeft,      'Temp file left behind');
  Assert.IsFalse(FileExists(BackupFile), 'Backup left behind');
  Assert.AreEqual(1, Integer(Length(TDirectory.GetFiles(FFolder))), 'Only the INI file must be in the folder');
end;


{$IFDEF MSWINDOWS}
{ ReplaceFile fails with error 5 on a read-only file (measured). The save must raise and keep the old bytes, with no temp file left }
procedure TTestMemIniFileExSave.FailedSave_ReadOnlyFile_KeepsOldBytes;
CONST
  Content = '[S]'#13#10'K=Old'#13#10;
begin
  TFile.WriteAllText(FFile, Content, TEncoding.ANSI);
  VAR Before:= TFile.ReadAllBytes(FFile);
  Assert.IsTrue(SetFileAttributes(PChar(FFile), FILE_ATTRIBUTE_READONLY), 'Cannot make the INI file read-only');
  TRY
    VAR Ini:= NewIni;
    TRY
      Ini.Write('K', 'New');
      Assert.WillRaise(procedure begin Ini.UpdateFile; end, EIniFileException);
    FINALLY
      FreeAndNil(Ini);
    END;
  FINALLY
    SetFileAttributes(PChar(FFile), FILE_ATTRIBUTE_NORMAL);
  END;

  Assert.IsTrue(SameBytes(Before, TFile.ReadAllBytes(FFile)), 'The old bytes must survive a failed save');
  Assert.AreEqual(0, TempFilesLeft,      'Temp file left behind');
  Assert.IsFalse(FileExists(BackupFile), 'Backup left behind');
end;


{ A write that fails halfway (disk full) must not touch the old file.
  TStrings.SaveToFile opens the file with fmCreate, which empties it before the first byte is written (System.Classes.pas, TStrings.SaveToFile). }
procedure TTestMemIniFileExSave.WriteFailsHalfway_KeepsOldBytes;
CONST
  Content = '[S]'#13#10'K=Old'#13#10'Other=Kept'#13#10;
begin
  TFile.WriteAllText(FFile, Content, TEncoding.ANSI);
  VAR Before:= TFile.ReadAllBytes(FFile);

  VAR Enc:= TWriteFailsEncoding.Create;
  TRY
    VAR Ini:= NewIni;
    TRY
      Ini.Encoding:= Enc;
      Ini.Write('K', 'New');
      Assert.WillRaise(procedure begin Ini.UpdateFile; end, EIniFileException);
    FINALLY
      FreeAndNil(Ini);
    END;
  FINALLY
    FreeAndNil(Enc);
  END;

  Assert.IsTrue(SameBytes(Before, TFile.ReadAllBytes(FFile)), 'The old bytes must survive a write that fails halfway');
  Assert.AreEqual(0, TempFilesLeft, 'The half-written temp file must be deleted');
end;


{ A crash between the two renames inside ReplaceFile leaves the old file only under the backup name }
procedure TTestMemIniFileExSave.MissingFile_BackupIsRestoredAtCreate;
begin
  TFile.WriteAllText(BackupFile, '[S]'#13#10'K=FromBackup'#13#10, TEncoding.ANSI);
  Assert.IsFalse(FileExists(FFile), 'Setup must start with no INI file');

  VAR Ini:= NewIni;
  TRY
    Assert.AreEqual('FromBackup', Ini.Read('K', '<missing>'), 'The constructor must load the restored file');
  FINALLY
    FreeAndNil(Ini);
  END;

  Assert.IsTrue (FileExists(FFile),      'The backup must be back under the INI name');
  Assert.IsFalse(FileExists(BackupFile), 'The backup name must be free again');
end;


{ An object opened before such a crash must not save its own values alone over the missing file:
  the swap of the save after it would overwrite the backup, the only copy of the old values }
procedure TTestMemIniFileExSave.MissingFile_BackupIsRestoredAtSave;
begin
  TFile.WriteAllText(FFile, '[S]'#13#10'Old=Kept'#13#10, TEncoding.ANSI);

  VAR Ini:= NewIni;
  TRY
    Ini.Write('New', 'Added');
    Assert.IsTrue(RenameFile(FFile, BackupFile), 'Cannot rename the INI file');   { The state that a crash inside ReplaceFile leaves }
    Ini.UpdateFile;
  FINALLY
    FreeAndNil(Ini);
  END;

  Assert.AreEqual('Kept',  ReadBack('S', 'Old'), 'The old values must come back from the backup');
  Assert.AreEqual('Added', ReadBack('S', 'New'));
  Assert.IsFalse(FileExists(BackupFile), 'Backup left behind');
end;


{ A crash can leave a temp file (during the write) and the backup (after the swap, before the backup is deleted).
  The next save deletes the leftover backup before its swap.
  It never touches the leftover temp file: that name may belong to a save of another object that is running now. }
procedure TTestMemIniFileExSave.LeftoverTmp_DoesNotStopSave_LeftoverBakIsGone;
begin
  VAR StrayTemp:= FFile + '.' + TGUID.NewGuid.ToString + '.tmp';
  TFile.WriteAllText(FFile,      '[S]'#13#10'Old=Kept'#13#10,  TEncoding.ANSI);
  TFile.WriteAllText(StrayTemp,  'half-written garbage',       TEncoding.ANSI);
  TFile.WriteAllText(BackupFile, '[S]'#13#10'Old=Stale'#13#10, TEncoding.ANSI);
  VAR StrayBefore:= TFile.ReadAllBytes(StrayTemp);

  VAR Ini:= NewIni;
  TRY
    Ini.Write('New', 'Added');
    Assert.WillNotRaise(procedure begin Ini.UpdateFile; end, NIL, 'A leftover temp file must not stop the save');
  FINALLY
    FreeAndNil(Ini);
  END;

  Assert.AreEqual('Kept',  ReadBack('S', 'Old'));
  Assert.AreEqual('Added', ReadBack('S', 'New'));
  Assert.IsFalse(FileExists(BackupFile), 'Backup left behind');
  Assert.AreEqual(1, TempFilesLeft, 'Only the leftover temp file may stay');
  Assert.IsTrue(SameBytes(StrayBefore, TFile.ReadAllBytes(StrayTemp)), 'The save changed a temp file that is not its own');
end;


{ Each save deletes the backup right after the swap, while the antivirus may still be scanning the new files.
  C:\test delete\ is scanned by Windows Defender on the development PC (measured: a swap costs about 10 ms there, 1 ms under C:\Projects). }
procedure TTestMemIniFileExSave.ManySaves_LeaveOnlyTheIniFile;
CONST
  Saves = 30;
begin
  VAR Ini:= NewIni;
  TRY
    Ini.WriteThrough:= TRUE;
    for VAR i:= 1 to Saves do
      Ini.Write('K', i);
  FINALLY
    FreeAndNil(Ini);
  END;

  Assert.AreEqual(IntToStr(Saves), ReadBack('S', 'K'));
  Assert.AreEqual(1, Integer(Length(TDirectory.GetFiles(FFolder))), 'Only the INI file must be in the folder');
end;


{ The INI file is missing and its backup cannot be renamed back (held open with no share mode, so MoveFile fails).
  Create must log the failure and give the defaults. UpdateFile must raise and write nothing:
  a save over the missing file would hold only this object's values, and the swap of the next save would overwrite the backup. }
procedure TTestMemIniFileExSave.MissingFile_BackupLocked_SaveRaisesAndKeepsBackup;
begin
  TFile.WriteAllText(BackupFile, '[S]'#13#10'K=FromBackup'#13#10, TEncoding.ANSI);
  VAR Before:= TFile.ReadAllBytes(BackupFile);
  Assert.IsFalse(FileExists(FFile), 'Setup must start with no INI file');
  Assert.IsTrue(Assigned(AppDataCore) AND Assigned(AppDataCore.RamLog), 'The test runner must create AppDataCore and its log');

  VAR Lock:= TFileStream.Create(BackupFile, fmOpenReadWrite or fmShareExclusive);
  TRY
    VAR Ini: TMemIniFileEx:= NIL;
    TRY
      Assert.WillNotRaise(procedure begin Ini:= NewIni; end, NIL, 'Create must log a failed restore, not raise it');
      Assert.AreEqual('Default', Ini.Read('K', 'Default'), 'Create must give the defaults');
      Assert.IsTrue(Pos(BackupFile, AppDataCore.RamLog.GetAsText) > 0, 'Create must log the failed restore of '+ BackupFile);

      Ini.Write('K', 'Mine');
      Assert.WillRaise(procedure begin Ini.UpdateFile; end, EIniFileException);
    FINALLY
      FreeAndNil(Ini);
    END;
  FINALLY
    FreeAndNil(Lock);
  END;

  Assert.IsFalse(FileExists(FFile),    'The failed save must not write the INI file over the missing one');
  Assert.AreEqual(0, TempFilesLeft,    'Temp file left behind');
  Assert.IsTrue(SameBytes(Before, TFile.ReadAllBytes(BackupFile)), 'The backup must stay whole');
end;


{ Another object that reads the file holds it open with share mode READ only (TStrings.LoadFromFile: fmOpenRead or fmShareDenyWrite).
  ReplaceFile needs DELETE access, so the save fails with error 32 (measured, Docs\ReplaceFile - measured behaviour\, CASE 6).
  The changes must stay in the change record, so the next save writes them. }
procedure TTestMemIniFileExSave.ShareReadLockDuringSave_ChangesSurviveNextSave;
begin
  TFile.WriteAllText(FFile, '[S]'#13#10'Old=Kept'#13#10, TEncoding.ANSI);
  VAR Ini:= NewIni;
  TRY
    Ini.Write('A', 'One');
    Ini.Write('B', 'Two');

    VAR Reader:= TFileStream.Create(FFile, fmOpenRead or fmShareDenyWrite);
    TRY
      Assert.WillRaise(procedure begin Ini.UpdateFile; end, EIniFileException);
    FINALLY
      FreeAndNil(Reader);
    END;

    Ini.UpdateFile;
    Assert.AreEqual('One',  ReadBack('S', 'A'), 'The second UpdateFile must save the changes of the failed one');
    Assert.AreEqual('Two',  ReadBack('S', 'B'));
    Assert.AreEqual('Kept', ReadBack('S', 'Old'));
  FINALLY
    FreeAndNil(Ini);
  END;
end;


{ A user or another tool keeps its own copy, <FileName>.bak, beside the INI file. The save must not overwrite it or delete it. }
procedure TTestMemIniFileExSave.UsersOwnBakFile_SurvivesSave;
begin
  VAR UsersCopy:= FFile + '.bak';
  TFile.WriteAllText(FFile,     '[S]'#13#10'Old=Kept'#13#10,      TEncoding.ANSI);
  TFile.WriteAllText(UsersCopy, '[S]'#13#10'Old=UsersCopy'#13#10, TEncoding.ANSI);
  VAR Before:= TFile.ReadAllBytes(UsersCopy);

  VAR Ini:= NewIni;
  TRY
    Ini.Write('New', 'Added');
  FINALLY
    FreeAndNil(Ini);
  END;

  Assert.AreEqual('Added', ReadBack('S', 'New'));
  Assert.IsTrue(FileExists(UsersCopy), 'The save deleted the user''s own '+ UsersCopy);
  Assert.IsTrue(SameBytes(Before, TFile.ReadAllBytes(UsersCopy)), 'The save changed the user''s own '+ UsersCopy);
end;


{ The file is missing at the reload and present at the swap (another program between the two renames of its ReplaceFile).
  A swap then would replace the other program's file with only this object's changes: every other value would be lost.
  The save must raise instead, keep its changes, and the next save must merge them over the file. }
procedure TTestMemIniFileExSave.FileAppearsDuringSave_RaisesAndKeepsItsValues;
begin
  Assert.IsFalse(FileExists(FFile), 'Setup must start with no INI file');
  VAR Enc:= TCreatesIniEncoding.Create;
  TRY
    Enc.IniToCreate:= FFile;
    VAR Ini:= NewIni;
    TRY
      Ini.Encoding:= Enc;
      Ini.Write('K', 'Mine');
      Assert.WillRaise(procedure begin Ini.UpdateFile; end, EIniFileException, 'The file appeared during the save: the save must not replace it');
      Assert.IsTrue(Enc.Created, 'Setup: the encoding must have created the file during the save');
      Assert.AreEqual('Kept', ReadBack('S', 'Other'), 'The value of the other program must survive the failed save');

      Ini.UpdateFile;
    FINALLY
      FreeAndNil(Ini);
    END;
  FINALLY
    FreeAndNil(Enc);
  END;

  Assert.AreEqual('Kept', ReadBack('S', 'Other'), 'The next save must keep the value of the other program');
  Assert.AreEqual('Mine', ReadBack('S', 'K'),     'The next save must write the kept change');
end;


{ A leftover backup that is read-only makes ReplaceFile fail with error 1175 (measured, CASE 7), at every save.
  The INI file exists, so the backup is not the only copy of the old values: the save may delete it. }
procedure TTestMemIniFileExSave.ReadOnlyLeftoverBackup_DoesNotBlockSave;
begin
  TFile.WriteAllText(FFile,      '[S]'#13#10'Old=Kept'#13#10,  TEncoding.ANSI);
  TFile.WriteAllText(BackupFile, '[S]'#13#10'Old=Stale'#13#10, TEncoding.ANSI);
  Assert.IsTrue(SetFileAttributes(PChar(BackupFile), FILE_ATTRIBUTE_READONLY), 'Cannot make the backup read-only');

  VAR Ini:= NewIni;
  TRY
    Ini.Write('New', 'Added');
    Assert.WillNotRaise(procedure begin Ini.UpdateFile; end, NIL, 'A read-only leftover backup must not block the save');
  FINALLY
    FreeAndNil(Ini);
  END;

  Assert.AreEqual('Added', ReadBack('S', 'New'));
  Assert.AreEqual('Kept',  ReadBack('S', 'Old'));
  Assert.IsFalse(FileExists(BackupFile), 'Backup left behind');
end;


{ A leftover backup that cannot be deleted (held open by another program) blocks the save.
  The error message must name the backup, so a person knows which file to remove. }
procedure TTestMemIniFileExSave.LockedLeftoverBackup_ErrorNamesTheBackup;
begin
  TFile.WriteAllText(FFile,      '[S]'#13#10'Old=Kept'#13#10,  TEncoding.ANSI);
  TFile.WriteAllText(BackupFile, '[S]'#13#10'Old=Stale'#13#10, TEncoding.ANSI);
  VAR Before:= TFile.ReadAllBytes(FFile);

  VAR Lock:= TFileStream.Create(BackupFile, fmOpenReadWrite or fmShareExclusive);
  TRY
    VAR Ini:= NewIni;
    TRY
      Ini.Write('New', 'Added');
      VAR Msg:= '';
      TRY
        Ini.UpdateFile;
      EXCEPT
        on E: EIniFileException do
          Msg:= E.Message;
      END;
      Assert.IsTrue(Msg <> '', 'The save must raise EIniFileException');
      Assert.IsTrue(Pos(BackupFile, Msg) > 0, 'The message must name the backup '+ BackupFile+ '. Message: '+ Msg);
    FINALLY
      FreeAndNil(Ini);
    END;
  FINALLY
    FreeAndNil(Lock);
  END;

  Assert.IsTrue(SameBytes(Before, TFile.ReadAllBytes(FFile)), 'The INI file must stay as it was');
end;


{ Two saves at the same moment (two threads, or two programs) must not write into one temp file.
  So every save picks a temp name of its own. Seen from inside the save: the encoding looks at the folder while the temp file is open. }
procedure TTestMemIniFileExSave.TwoSaves_UseDifferentTempNames;
begin
  VAR Enc:= TRecordsTempNameEncoding.Create;
  TRY
    Enc.IniName:= FFile;
    VAR Ini:= NewIni;
    TRY
      Ini.Encoding:= Enc;

      Ini.Write('K', 'First');
      Ini.UpdateFile;
      VAR FirstTemp:= Enc.TempSeen;

      Ini.Write('K', 'Second');
      Ini.UpdateFile;
      VAR SecondTemp:= Enc.TempSeen;

      Assert.IsTrue((FirstTemp <> '') AND (FirstTemp <> '<several>'), 'Setup: the first save must write one temp file beside the INI file. Seen: '+ FirstTemp);
      Assert.IsTrue((SecondTemp <> '') AND (SecondTemp <> '<several>'), 'Setup: the second save must write one temp file beside the INI file. Seen: '+ SecondTemp);
      Assert.AreNotEqual(FirstTemp, SecondTemp, 'Two saves used the same temp file');
    FINALLY
      FreeAndNil(Ini);
    END;
  FINALLY
    FreeAndNil(Enc);
  END;

  Assert.AreEqual('Second', ReadBack('S', 'K'));
  Assert.AreEqual(0, TempFilesLeft, 'Temp file left behind');
end;
{$ENDIF}


initialization
  TDUnitX.RegisterTestFixture(TTestMemIniFileExSave);

end.
