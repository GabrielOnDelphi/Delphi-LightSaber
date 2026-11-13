unit Test.LightCore.LogFile;

{=============================================================================================================
   2026.11.12
   Unit tests for LightCore.LogFile.pas (TLogDailyFile: the daily text file of a TRamLog)
   Every test works in a folder of its own under the TEMP folder, made in Setup and removed in TearDown.
   The writer's Clock is replaced, so "today" is a fixed date and the tests never depend on the real date.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  System.DateUtils,
  LightCore.LogFile;

type
  [TestFixture]
  TTestLogDailyFile = class
  private
    FTestDir: string;
    FToday  : TDateTime;
    FErrors : Integer;
    FLastError: string;
    function  NewWriter: TLogDailyFile;
    function  At(Day: TDateTime; H, M, S: Word): TDateTime;
    procedure MakeFile(CONST Path, Content: string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure FileNameOf_IsPrefixDateExt;
    [Test]
    procedure Flush_EmptyBuffer_WritesNothing;
    [Test]
    procedure Flush_WritesTimeAndMessage;
    [Test]
    procedure Flush_MultiLineMessage_IsFlattened;
    [Test]
    procedure Flush_SecondFlush_Appends;
    [Test]
    procedure Destroy_FlushesTheBuffer;
    [Test]
    procedure Inactive_AddIsIgnored;
    [Test]
    procedure Flush_LinesOfTwoDays_GoToTwoFiles;
    [Test]
    procedure DaysToKeep_DeletesOnlyOldFilesOfThisLog;
    [Test]
    procedure DayChange_DeletesTheFileThatBecameTooOld;
    [Test]
    procedure BufferFull_CountsLostLines_WritesOneLine;
    [Test]
    procedure SizeCap_WritesOneClosingLine_ThenNothing;
    [Test]
    procedure Restart_FileClosedEarlier_NoSecondClosingLine;
    [Test]
    procedure Restart_JustUnderTheCap_OneClosingLine;
    [Test]
    procedure Restart_FileOverTheCap_WritesNothing;
    [Test]
    procedure SizeCap_NextDay_StartsANewFile;
    [Test]
    procedure WriteError_ReportedOncePerStreak;
  end;

implementation

CONST
  CRLF       = #13#10;
  TestPrefix = 'Srv ';


{ Number of times Sub occurs in S }
function CountOf(CONST Sub, S: string): Integer;
VAR Offset: Integer;
begin
  Result:= 0;
  Offset:= Pos(Sub, S);
  while Offset > 0 do
    begin
      Inc(Result);
      Offset:= Pos(Sub, S, Offset + Length(Sub));
    end;
end;


function ReadText(CONST Path: string): string;
begin
  Result:= TFile.ReadAllText(Path, TEncoding.UTF8);
end;


procedure TTestLogDailyFile.Setup;
begin
  FTestDir:= TPath.Combine(TPath.GetTempPath, 'LogFileTest_' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTestDir);
  FToday:= EncodeDate(2026, 3, 15);
  FErrors:= 0;
  FLastError:= '';
end;


{ Removes only the folder that Setup made in this run }
procedure TTestLogDailyFile.TearDown;
begin
  if (FTestDir <> '')
  AND FTestDir.StartsWith(TPath.Combine(TPath.GetTempPath, 'LogFileTest_'), TRUE)
  AND TDirectory.Exists(FTestDir)
  then TDirectory.Delete(FTestDir, TRUE);
end;


function TTestLogDailyFile.NewWriter: TLogDailyFile;
begin
  Result:= TLogDailyFile.Create(FTestDir, TestPrefix);
  Result.Clock:= function: TDateTime
                 begin
                   Result:= FToday;
                 end;
  Result.OnWriteError:= procedure(CONST Msg: string)
                        begin
                          Inc(FErrors);
                          FLastError:= Msg;
                        end;
end;


function TTestLogDailyFile.At(Day: TDateTime; H, M, S: Word): TDateTime;
begin
  Result:= Trunc(Day) + EncodeTime(H, M, S, 0);
end;


procedure TTestLogDailyFile.MakeFile(CONST Path, Content: string);
begin
  TFile.WriteAllText(Path, Content, TEncoding.ASCII);   { ASCII: no BOM }
end;


{-------------------------------------------------------------------------------------------------------------
   NAME AND FORMAT
-------------------------------------------------------------------------------------------------------------}
procedure TTestLogDailyFile.FileNameOf_IsPrefixDateExt;
VAR Writer: TLogDailyFile;
begin
  Writer:= NewWriter;
  try
    Assert.AreEqual(IncludeTrailingPathDelimiter(FTestDir) + 'Srv 2026-03-05.log', Writer.FileNameOf(EncodeDate(2026, 3, 5) + 0.75));
  finally
    FreeAndNil(Writer);
  end;
end;


procedure TTestLogDailyFile.Flush_EmptyBuffer_WritesNothing;
VAR
  Writer: TLogDailyFile;
  SubDir: string;
begin
  SubDir:= TPath.Combine(FTestDir, 'Logs');
  Writer:= TLogDailyFile.Create(SubDir, TestPrefix);
  try
    Writer.Flush;
    Assert.IsFalse(TDirectory.Exists(SubDir), 'A flush with no lines must not even create the folder');
  finally
    FreeAndNil(Writer);
  end;
  Assert.IsFalse(TDirectory.Exists(SubDir), 'Destroy with no lines must write nothing');
end;


procedure TTestLogDailyFile.Flush_WritesTimeAndMessage;
VAR Writer: TLogDailyFile;
begin
  Writer:= NewWriter;
  try
    Writer.Add(At(FToday, 14, 7, 9), 'Hello');
    Writer.Add(At(FToday, 14, 7, 10), 'Licence check: ok');
    Writer.Flush;
    Assert.AreEqual('14:07:09  Hello' + CRLF + '14:07:10  Licence check: ok' + CRLF, ReadText(Writer.FileNameOf(FToday)));
  finally
    FreeAndNil(Writer);
  end;
end;


procedure TTestLogDailyFile.Flush_MultiLineMessage_IsFlattened;
VAR Writer: TLogDailyFile;
begin
  Writer:= NewWriter;
  try
    Writer.Add(At(FToday, 1, 2, 3), 'Line A' + CRLF + 'Line B');
    Writer.Flush;
    Assert.AreEqual('01:02:03  Line A Line B' + CRLF, ReadText(Writer.FileNameOf(FToday)), 'One log entry must stay one line in the file');
  finally
    FreeAndNil(Writer);
  end;
end;


procedure TTestLogDailyFile.Flush_SecondFlush_Appends;
VAR Writer: TLogDailyFile;
begin
  Writer:= NewWriter;
  try
    Writer.Add(At(FToday, 10, 0, 0), 'First');
    Writer.Flush;
    Writer.Add(At(FToday, 10, 0, 5), 'Second');
    Writer.Flush;
    Writer.Flush;    { Empty: adds nothing }
    Assert.AreEqual('10:00:00  First' + CRLF + '10:00:05  Second' + CRLF, ReadText(Writer.FileNameOf(FToday)));
  finally
    FreeAndNil(Writer);
  end;
end;


procedure TTestLogDailyFile.Destroy_FlushesTheBuffer;
VAR
  Writer: TLogDailyFile;
  Path: string;
begin
  Writer:= NewWriter;
  try
    Path:= Writer.FileNameOf(FToday);
    Writer.Add(At(FToday, 23, 59, 59), 'Last words');
  finally
    FreeAndNil(Writer);
  end;
  Assert.AreEqual('23:59:59  Last words' + CRLF, ReadText(Path), 'Destroy must write what is still in the buffer');
end;


procedure TTestLogDailyFile.Inactive_AddIsIgnored;
VAR Writer: TLogDailyFile;
begin
  Writer:= NewWriter;
  try
    Writer.Add(At(FToday, 8, 0, 0), 'Kept');
    Writer.Active:= FALSE;
    Writer.Add(At(FToday, 8, 0, 1), 'Ignored');
    Writer.Flush;
    Assert.AreEqual('08:00:00  Kept' + CRLF, ReadText(Writer.FileNameOf(FToday)), 'A line added while Active is FALSE must not reach the file; one added before must');
  finally
    FreeAndNil(Writer);
  end;
end;


{-------------------------------------------------------------------------------------------------------------
   DAYS
-------------------------------------------------------------------------------------------------------------}
procedure TTestLogDailyFile.Flush_LinesOfTwoDays_GoToTwoFiles;
VAR
  Writer: TLogDailyFile;
  Tomorrow: TDateTime;
begin
  Tomorrow:= FToday + 1;
  Writer:= NewWriter;
  try
    Writer.Add(At(FToday,   23, 59, 58), 'Before midnight');
    Writer.Add(At(Tomorrow,  0,  0,  1), 'After midnight');
    Writer.Flush;
    Assert.AreEqual('23:59:58  Before midnight' + CRLF, ReadText(Writer.FileNameOf(FToday)));
    Assert.AreEqual('00:00:01  After midnight'  + CRLF, ReadText(Writer.FileNameOf(Tomorrow)));
  finally
    FreeAndNil(Writer);
  end;
end;


{ DaysToKeep = 3 keeps today and the two days before. Files with another name are never touched. }
procedure TTestLogDailyFile.DaysToKeep_DeletesOnlyOldFilesOfThisLog;
VAR
  Writer: TLogDailyFile;
  Dir: string;
begin
  Dir:= IncludeTrailingPathDelimiter(FTestDir);
  Writer:= NewWriter;
  try
    MakeFile(Writer.FileNameOf(FToday),     'x');
    MakeFile(Writer.FileNameOf(FToday - 2), 'x');
    MakeFile(Writer.FileNameOf(FToday - 3), 'x');
    MakeFile(Writer.FileNameOf(FToday - 40),'x');
    MakeFile(Dir + 'Srv notes.log',         'x');   { Same prefix, not a date }
    MakeFile(Dir + 'Srv 2026-02-30.log',    'x');   { Not a real date }
    MakeFile(Dir + 'Other 2020-01-01.log',  'x');   { Another log }
    MakeFile(Dir + 'Srv 2020-01-01.txt',    'x');   { Another extension }

    Writer.DaysToKeep:= 3;

    Assert.IsTrue (FileExists(Writer.FileNameOf(FToday)),      'Today must be kept');
    Assert.IsTrue (FileExists(Writer.FileNameOf(FToday - 2)),  'The oldest day of the 3 must be kept');
    Assert.IsFalse(FileExists(Writer.FileNameOf(FToday - 3)),  'The day before the 3 must be deleted');
    Assert.IsFalse(FileExists(Writer.FileNameOf(FToday - 40)), 'A file 40 days old must be deleted');
    Assert.IsTrue (FileExists(Dir + 'Srv notes.log'),          'A file whose name is not a date must stay');
    Assert.IsTrue (FileExists(Dir + 'Srv 2026-02-30.log'),     'A file whose date is not real must stay');
    Assert.IsTrue (FileExists(Dir + 'Other 2020-01-01.log'),   'Another log''s file must stay');
    Assert.IsTrue (FileExists(Dir + 'Srv 2020-01-01.txt'),     'A file with another extension must stay');
    Assert.AreEqual(0, FErrors, 'No delete may fail');
  finally
    FreeAndNil(Writer);
  end;
end;


{ The server runs for days: at midnight the file that just became too old goes, without a restart }
procedure TTestLogDailyFile.DayChange_DeletesTheFileThatBecameTooOld;
VAR
  Writer: TLogDailyFile;
  OldestKept: string;
begin
  Writer:= NewWriter;
  try
    Writer.DaysToKeep:= 3;
    OldestKept:= Writer.FileNameOf(FToday - 2);
    MakeFile(OldestKept, 'x');

    Writer.Add(At(FToday, 23, 59, 59), 'Today');
    Writer.Flush;
    Assert.IsTrue(FileExists(OldestKept), 'Before midnight the file is still inside the 3 days');

    FToday:= FToday + 1;       { Midnight passes }
    Writer.Add(At(FToday, 0, 0, 1), 'Tomorrow');
    Writer.Flush;
    Assert.IsFalse(FileExists(OldestKept), 'After midnight the file is 3 days old and must be deleted');
    Assert.IsTrue (FileExists(Writer.FileNameOf(FToday - 1)), 'Yesterday''s file must stay');
  finally
    FreeAndNil(Writer);
  end;
end;


{-------------------------------------------------------------------------------------------------------------
   LIMITS
-------------------------------------------------------------------------------------------------------------}
procedure TTestLogDailyFile.BufferFull_CountsLostLines_WritesOneLine;
VAR
  Writer: TLogDailyFile;
  Text: string;
  i: Integer;
begin
  Writer:= NewWriter;
  try
    Writer.MaxPending:= 3;
    for i:= 1 to 5 do
      Writer.Add(At(FToday, 9, 0, i), 'Line ' + IntToStr(i));
    FToday:= At(FToday, 9, 0, 6);    { The clock gives the time of the "not written" line }
    Writer.Flush;

    Text:= ReadText(Writer.FileNameOf(FToday));
    Assert.AreEqual('09:00:01  Line 1' + CRLF
                  + '09:00:02  Line 2' + CRLF
                  + '09:00:03  Line 3' + CRLF
                  + '09:00:06  2 log lines were not written to the file: more than 3 lines arrived between two writes.' + CRLF, Text);

    { The count starts again after a flush }
    Writer.Add(At(FToday, 9, 0, 7), 'Line 6');
    Writer.Flush;
    Assert.AreEqual(1, CountOf('not written', ReadText(Writer.FileNameOf(FToday))), 'A flush with no lost lines must not write a second "not written" line');
  finally
    FreeAndNil(Writer);
  end;
end;


{ Adds lines of about 1 KB, all at 12:00:00, until they pass a cap of 1 MB, then flushes }
procedure FillPastOneMB(Writer: TLogDailyFile; Day: TDateTime);
VAR
  Line: string;
  i: Integer;
begin
  Line:= StringOfChar('a', 1000);
  for i:= 1 to 1100 do                    { 1100 x 1012 bytes = 1.06 MB }
    Writer.Add(Trunc(Day) + EncodeTime(12, 0, 0, 0), Line);
  Writer.Flush;
end;


procedure TTestLogDailyFile.SizeCap_WritesOneClosingLine_ThenNothing;
VAR
  Writer: TLogDailyFile;
  Path, Text: string;
  SizeAfterCap: Int64;
begin
  Writer:= NewWriter;
  try
    Writer.MaxFileMB:= 1;
    Path:= Writer.FileNameOf(FToday);
    FillPastOneMB(Writer, FToday);

    Text:= ReadText(Path);
    Assert.AreEqual(1, CountOf(TLogDailyFile.SizeLimitText, Text), 'Exactly one closing line');
    Assert.IsTrue(Text.EndsWith('12:00:00  ' + TLogDailyFile.SizeLimitText + CRLF), 'The closing line must be the last line, with the time of the line that did not fit');
    SizeAfterCap:= TFile.GetSize(Path);
    Assert.IsTrue(SizeAfterCap <= 1024*1024 + 100, 'The file may pass the cap only by the closing line. Size: ' + IntToStr(SizeAfterCap));

    Writer.Add(At(FToday, 13, 0, 0), 'After the cap');
    Writer.Flush;
    Assert.AreEqual(SizeAfterCap, TFile.GetSize(Path), 'Nothing more may go into a closed file that day');
  finally
    FreeAndNil(Writer);
  end;
end;


procedure TTestLogDailyFile.Restart_FileClosedEarlier_NoSecondClosingLine;
VAR
  Writer: TLogDailyFile;
  Path: string;
  SizeAfterCap: Int64;
begin
  Writer:= NewWriter;
  try
    Writer.MaxFileMB:= 1;
    Path:= Writer.FileNameOf(FToday);
    FillPastOneMB(Writer, FToday);
  finally
    FreeAndNil(Writer);
  end;
  SizeAfterCap:= TFile.GetSize(Path);

  { The program restarts the same day, with a larger cap: the file was closed, so it stays closed }
  Writer:= NewWriter;
  try
    Writer.MaxFileMB:= 2;
    Writer.Add(At(FToday, 14, 0, 0), 'After the restart');
    Writer.Flush;
    Assert.AreEqual(SizeAfterCap, TFile.GetSize(Path), 'A file that ends with the closing line must get nothing more');
    Assert.AreEqual(1, CountOf(TLogDailyFile.SizeLimitText, ReadText(Path)), 'No second closing line');
  finally
    FreeAndNil(Writer);
  end;
end;


{ A restart counts the real size of the file: a file 20 bytes under the cap gets the closing line at the first line that does not fit }
procedure TTestLogDailyFile.Restart_JustUnderTheCap_OneClosingLine;
VAR
  Writer: TLogDailyFile;
  Path, Text: string;
begin
  Writer:= NewWriter;
  try
    Writer.MaxFileMB:= 1;
    Path:= Writer.FileNameOf(FToday);
    MakeFile(Path, StringOfChar('b', 1024*1024 - 20));

    Writer.Add(At(FToday, 15, 0, 0), 'ok');                                  { 14 bytes: fits }
    Writer.Add(At(FToday, 15, 0, 1), 'This line is too long to fit');        { Does not fit }
    Writer.Add(At(FToday, 15, 0, 2), 'x');
    Writer.Flush;

    Text:= ReadText(Path);
    Assert.IsTrue(Text.EndsWith('b' + '15:00:00  ok' + CRLF + '15:00:01  ' + TLogDailyFile.SizeLimitText + CRLF), 'The line that fits, then the closing line, then nothing');
    Assert.AreEqual(1, CountOf(TLogDailyFile.SizeLimitText, Text));
  finally
    FreeAndNil(Writer);
  end;

  { A third run on the same day adds nothing }
  Writer:= NewWriter;
  try
    Writer.MaxFileMB:= 1;
    Writer.Add(At(FToday, 16, 0, 0), 'Third run');
    Writer.Flush;
    Assert.AreEqual(1, CountOf(TLogDailyFile.SizeLimitText, ReadText(Path)), 'No second closing line after a restart');
    Assert.AreEqual(0, CountOf('Third run', ReadText(Path)));
  finally
    FreeAndNil(Writer);
  end;
end;


{ A file already at the cap without the closing line (an older program, or a smaller cap now): nothing is added, not even the closing line }
procedure TTestLogDailyFile.Restart_FileOverTheCap_WritesNothing;
VAR
  Writer: TLogDailyFile;
  Path: string;
begin
  Writer:= NewWriter;
  try
    Writer.MaxFileMB:= 1;
    Path:= Writer.FileNameOf(FToday);
    MakeFile(Path, StringOfChar('c', 1024*1024 + 5));
    Writer.Add(At(FToday, 17, 0, 0), 'Too late');
    Writer.Flush;
    Assert.AreEqual(Int64(1024*1024 + 5), TFile.GetSize(Path), 'A full file must not grow');
  finally
    FreeAndNil(Writer);
  end;
end;


procedure TTestLogDailyFile.SizeCap_NextDay_StartsANewFile;
VAR Writer: TLogDailyFile;
begin
  Writer:= NewWriter;
  try
    Writer.MaxFileMB:= 1;
    FillPastOneMB(Writer, FToday);
    FToday:= FToday + 1;
    Writer.Add(At(FToday, 0, 0, 3), 'New day');
    Writer.Flush;
    Assert.AreEqual('00:00:03  New day' + CRLF, ReadText(Writer.FileNameOf(FToday)), 'The cap of yesterday must not close today''s file');
  finally
    FreeAndNil(Writer);
  end;
end;


{-------------------------------------------------------------------------------------------------------------
   ERRORS
-------------------------------------------------------------------------------------------------------------}

{ A locked file: the first failing flush reports, the next failing ones do not; after a good write a new failure reports again }
procedure TTestLogDailyFile.WriteError_ReportedOncePerStreak;
VAR
  Writer: TLogDailyFile;
  Locker: TFileStream;
  Path: string;
begin
  Writer:= NewWriter;
  try
    Path:= Writer.FileNameOf(FToday);
    Locker:= TFileStream.Create(Path, fmCreate OR fmShareExclusive);
    try
      Writer.Add(At(FToday, 18, 0, 0), 'Lost 1');
      Writer.Flush;
      Writer.Add(At(FToday, 18, 0, 1), 'Lost 2');
      Writer.Flush;
      Assert.AreEqual(1, FErrors, 'Two failing flushes in a row must report once');
      Assert.Contains(FLastError, Path, 'The message must name the file');
    finally
      FreeAndNil(Locker);
    end;

    Writer.Add(At(FToday, 18, 0, 2), 'Written');
    Writer.Flush;
    Assert.AreEqual('18:00:02  Written' + CRLF, ReadText(Path), 'The write must work again once the file is free');
    Assert.AreEqual(1, FErrors, 'A good write must not report');

    Locker:= TFileStream.Create(Path, fmOpenReadWrite OR fmShareExclusive);
    try
      Writer.Add(At(FToday, 18, 0, 3), 'Lost 3');
      Writer.Flush;
      Assert.AreEqual(2, FErrors, 'A new failure after a good write must report again');
    finally
      FreeAndNil(Locker);
    end;
  finally
    FreeAndNil(Writer);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLogDailyFile);

end.
