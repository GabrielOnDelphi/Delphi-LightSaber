UNIT LightCore.LogFile;

{=============================================================================================================
   2026.11.12
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   TLogDailyFile: writes every line of a TRamLog to a plain text file, one file per day.
   An option of TRamLog (TRamLog.CreateDailyFile), off by default.

   How a line travels
     Add (any thread)  puts the line into a buffer in RAM, under a short lock. No disk access.
     Flush             takes the whole buffer and appends it to the file of each line's day. The owner calls it from a timer every FlushSeconds seconds, on one thread (normally the main thread).
     Destroy           flushes what is left.
     An empty buffer writes nothing: no file is opened.
     A crash loses the lines of at most one flush interval.

   Limits
     MaxPending  The lines the buffer holds between two flushes (default 20,000).
                 Past it, the lines are counted, and the next flush writes one line that says how many were not written.
     MaxFileMB   The size of one day file (default 100 MB).
                 When the next line would pass it, one closing line (SizeLimitText) is written, and nothing more goes into that file that day. The lines still go into the RAM log.
                 The size is the real size of the file on disk, so a restart on the same day appends and keeps counting. A file that already ends with the closing line gets no second one.
     DaysToKeep  The files of the last DaysToKeep days, today included, are kept. Older files are deleted each time DaysToKeep is set, and at each day change.
                 Only files whose name is exactly Prefix + yyyy-mm-dd + '.log' are touched.

   File format
     UTF-8 without BOM. One line per entry: 'hh:nn:ss  <message>' + CRLF. A multi-line message is flattened (LightCore.FlattenLineBreaks).
     The day of a line is the local date of its time, so the file changes at local midnight.

   Thread safety
     Add and Active: any thread.
     Flush, DeleteOldFiles and the property setters: one thread at a time (the owner's main thread).

   Tests: LightSaber\UnitTesting\Test.LightCore.LogFile.pas
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes, System.SyncObjs;

TYPE
  TLogClock          = reference to function: TDateTime;
  TLogFileErrorEvent = reference to procedure(CONST Msg: string);

  TLogDailyFile = class(TObject)
  private
    TYPE
      RPendingLine = record
        Time: TDateTime;
        Msg : string;
      end;
    VAR
      FLock        : TCriticalSection;        { Guards FPending, FPendingCount, FLost }
      FPending     : TArray<RPendingLine>;
      FPendingCount: Integer;
      FLost        : Integer;                 { Lines refused since the last flush because the buffer was full }
      FFolder      : string;                  { With a trailing path delimiter }
      FPrefix      : string;
      FDaysToKeep  : Integer;
      FMaxFileMB   : Integer;
      FMaxPending  : Integer;
      FLastDay     : Integer;                 { Day number of the last line written. 0 = none yet. A change means midnight passed. }
      FCheckedDay  : Integer;                 { Day whose file was opened and checked for the closing line in this run }
      FFullDay     : Integer;                 { Day whose file reached MaxFileMB. 0 = none }
      FErrorReported: Boolean;                { One OnWriteError per failure streak, not one per flush }
    procedure setDaysToKeep(Value: Integer);
    procedure setMaxFileMB (Value: Integer);
    procedure setMaxPending(Value: Integer);
    procedure writeDay(Day: Integer; CONST Texts: TArray<string>);
    procedure reportError(CONST Msg: string);
    function  dayOfFileName(CONST FileName: string; out Day: Integer): Boolean;
  public
    CONST
      FlushSeconds      = 5;          { How often the owner should call Flush }
      DefaultMaxPending = 20000;      { About 65 seconds of a flood of 305 lines per second; 13 flush intervals }
      DefaultMaxFileMB  = 100;
      DefaultDaysToKeep = 30;
      FileExt           = '.log';
      SizeLimitText     = 'Log file size limit reached: the rest of this day is not written to the file';
    VAR
      Active      : Boolean;              { FALSE = Add ignores the lines. What is already in the buffer is still written by the next Flush. }
      Clock       : TLogClock;            { Gives "today" for DeleteOldFiles and the time of the "not written" line. Now by default; tests replace it. }
      OnWriteError: TLogFileErrorEvent;   { Called once when a write or a delete fails, again only after a write succeeded. Called on the thread that calls Flush. }
    constructor Create(CONST Folder, Prefix: string);
    destructor Destroy; override;

    procedure Add(Time: TDateTime; CONST Msg: string);
    procedure Flush;
    procedure DeleteOldFiles;
    function  FileNameOf(Day: TDateTime): string;         { Full path of the file of that day }

    property Folder    : string  read FFolder;
    property Prefix    : string  read FPrefix;
    property DaysToKeep: Integer read FDaysToKeep write setDaysToKeep;   { Setting it runs DeleteOldFiles }
    property MaxFileMB : Integer read FMaxFileMB  write setMaxFileMB;
    property MaxPending: Integer read FMaxPending write setMaxPending;
  end;


IMPLEMENTATION

USES
   {$IFDEF POSIX}
   Posix.Unistd,   { Lets the compiler inline System.SysUtils.DeleteFile on Linux64 (hint H2443 without it) }
   {$ENDIF}
   LightCore;

CONST
  CRLF = #13#10;


constructor TLogDailyFile.Create(CONST Folder, Prefix: string);
begin
  inherited Create;
  Assert(Folder <> '', 'TLogDailyFile: Folder cannot be empty');
  FLock      := TCriticalSection.Create;
  FFolder    := IncludeTrailingPathDelimiter(Folder);
  FPrefix    := Prefix;
  FDaysToKeep:= DefaultDaysToKeep;
  FMaxFileMB := DefaultMaxFileMB;
  FMaxPending:= DefaultMaxPending;
  Active     := TRUE;
  Clock      := function: TDateTime
                begin
                  Result:= Now;
                end;
end;


destructor TLogDailyFile.Destroy;
begin
  if FLock <> NIL      { NIL when the constructor raised }
  then Flush;
  FreeAndNil(FLock);
  inherited;
end;


function TLogDailyFile.FileNameOf(Day: TDateTime): string;
begin
  Result:= FFolder + FPrefix + FormatDateTime('yyyy"-"mm"-"dd', Day, TFormatSettings.Invariant) + FileExt;
end;


{-------------------------------------------------------------------------------------------------------------
   SETTINGS
-------------------------------------------------------------------------------------------------------------}
procedure TLogDailyFile.setDaysToKeep(Value: Integer);
begin
  Assert(Value > 0, 'TLogDailyFile.DaysToKeep must be > 0');
  FDaysToKeep:= Value;
  DeleteOldFiles;
end;


procedure TLogDailyFile.setMaxFileMB(Value: Integer);
begin
  Assert(Value > 0, 'TLogDailyFile.MaxFileMB must be > 0');
  FMaxFileMB:= Value;
  FFullDay:= 0;          { Check again: a larger cap may reopen today's file }
  FCheckedDay:= 0;
end;


procedure TLogDailyFile.setMaxPending(Value: Integer);
begin
  Assert(Value > 0, 'TLogDailyFile.MaxPending must be > 0');
  FLock.Enter;
  try
    FMaxPending:= Value;
  finally
    FLock.Leave;
  end;
end;


{-------------------------------------------------------------------------------------------------------------
   ADD (any thread)
-------------------------------------------------------------------------------------------------------------}
procedure TLogDailyFile.Add(Time: TDateTime; CONST Msg: string);
VAR NewLen: Integer;
begin
  if NOT Active then EXIT;

  FLock.Enter;
  try
    if FPendingCount >= FMaxPending then
      begin
        if FLost < MaxInt
        then Inc(FLost);
        EXIT;
      end;

    if FPendingCount >= Length(FPending) then
      begin
        NewLen:= 2 * Length(FPending);
        if NewLen < 256        then NewLen:= 256;
        if NewLen > FMaxPending then NewLen:= FMaxPending;
        SetLength(FPending, NewLen);
      end;

    FPending[FPendingCount].Time:= Time;
    FPending[FPendingCount].Msg := Msg;
    Inc(FPendingCount);
  finally
    FLock.Leave;
  end;
end;


{-------------------------------------------------------------------------------------------------------------
   FLUSH (one thread)
-------------------------------------------------------------------------------------------------------------}
procedure TLogDailyFile.Flush;
VAR
  Lines: TArray<RPendingLine>;
  Count, Lost, i, First, Day: Integer;
  Texts: TArray<string>;
  LostTime: TDateTime;
begin
  { Take the buffer; the disk work runs outside the lock }
  FLock.Enter;
  try
    Lines:= FPending;
    Count:= FPendingCount;
    Lost := FLost;
    FPending:= NIL;
    FPendingCount:= 0;
    FLost:= 0;
  finally
    FLock.Leave;
  end;

  if (Count = 0) AND (Lost = 0) then EXIT;

  { The "not written" line goes after the lines that were taken: the lost lines arrived after them }
  if Lost > 0 then
    begin
      LostTime:= Clock();
      if Length(Lines) <= Count
      then SetLength(Lines, Count + 1);
      Lines[Count].Time:= LostTime;
      Lines[Count].Msg := IntToStr(Lost) + ' log lines were not written to the file: more than ' + IntToStr(FMaxPending) + ' lines arrived between two writes.';
      Inc(Count);
    end;

  { One write per day: a flush that spans midnight writes two files }
  First:= 0;
  while First < Count do
    begin
      Day:= Trunc(Lines[First].Time);
      i:= First;
      while (i < Count) AND (Trunc(Lines[i].Time) = Day) do
        Inc(i);

      SetLength(Texts, i - First);
      for VAR k:= First to i - 1 do
        Texts[k - First]:= FormatDateTime('hh":"nn":"ss', Lines[k].Time, TFormatSettings.Invariant) + '  ' + FlattenLineBreaks(Lines[k].Msg);

      writeDay(Day, Texts);
      First:= i;
    end;
end;


{ TRUE when Stream (positioned anywhere) ends with the closing line. Moves the position. }
function endsWithLimitLine(Stream: TStream): Boolean;
VAR
  Tail, Got: TBytes;
begin
  Tail:= TEncoding.UTF8.GetBytes(TLogDailyFile.SizeLimitText + CRLF);
  if Stream.Size < Length(Tail) then EXIT(FALSE);

  SetLength(Got, Length(Tail));
  Stream.Position:= Stream.Size - Length(Tail);
  Stream.ReadBuffer(Got, Length(Got));
  Result:= CompareMem(@Got[0], @Tail[0], Length(Tail));
end;


{ Appends Texts (one line each, without CRLF) to the file of Day, up to MaxFileMB }
procedure TLogDailyFile.writeDay(Day: Integer; CONST Texts: TArray<string>);
VAR
  Path: string;
  Stream: TFileStream;
  Buffer: TBytesStream;
  Bytes: TBytes;
  MaxBytes, FileSize: Int64;
  i: Integer;
begin
  { Midnight passed: a new file starts, and the oldest one may now be too old }
  if (FLastDay <> 0) AND (Day > FLastDay)
  then DeleteOldFiles;
  if Day > FLastDay
  then FLastDay:= Day;

  if Day = FFullDay then EXIT;    { This day's file is closed }

  Path:= FileNameOf(Day);
  MaxBytes:= Int64(FMaxFileMB) * 1024 * 1024;
  TRY
    ForceDirectories(FFolder);
    if FileExists(Path)
    then Stream:= TFileStream.Create(Path, fmOpenReadWrite OR fmShareDenyWrite)
    else Stream:= TFileStream.Create(Path, fmCreate OR fmShareDenyWrite);
    TRY
      FileSize:= Stream.Size;

      { First time this run sees the day's file: a file from an earlier run may already be closed }
      if Day <> FCheckedDay then
        begin
          FCheckedDay:= Day;
          if (FileSize >= MaxBytes) OR endsWithLimitLine(Stream) then
            begin
              FFullDay:= Day;
              EXIT;
            end;
        end;

      Buffer:= TBytesStream.Create;
      TRY
        for i:= 0 to High(Texts) do
          begin
            Bytes:= TEncoding.UTF8.GetBytes(Texts[i] + CRLF);
            if FileSize + Buffer.Size + Length(Bytes) > MaxBytes then
              begin
                Bytes:= TEncoding.UTF8.GetBytes(Copy(Texts[i], 1, 10) + SizeLimitText + CRLF);   { 'hh:nn:ss  ' of the line that did not fit }
                Buffer.WriteBuffer(Bytes, Length(Bytes));
                FFullDay:= Day;
                BREAK;
              end;
            Buffer.WriteBuffer(Bytes, Length(Bytes));
          end;

        Stream.Position:= FileSize;
        Stream.WriteBuffer(Buffer.Bytes, Buffer.Size);
      FINALLY
        FreeAndNil(Buffer);
      END;
    FINALLY
      FreeAndNil(Stream);
    END;
    FErrorReported:= FALSE;
  EXCEPT
    on E: EStreamError do     { EFCreateError, EFOpenError, EWriteError, EReadError: a locked file, a full disk, a folder without write rights }
      reportError('The log file ' + Path + ' could not be written (' + IntToStr(Length(Texts)) + ' lines lost): ' + E.Message);
  END;
end;


procedure TLogDailyFile.reportError(CONST Msg: string);
begin
  if FErrorReported then EXIT;
  FErrorReported:= TRUE;
  if Assigned(OnWriteError)
  then OnWriteError(Msg);
end;


{-------------------------------------------------------------------------------------------------------------
   OLD FILES
-------------------------------------------------------------------------------------------------------------}

{ TRUE when FileName (no folder) is exactly Prefix + yyyy-mm-dd + FileExt; Day is then that date }
function TLogDailyFile.dayOfFileName(CONST FileName: string; out Day: Integer): Boolean;
VAR
  DatePart: string;
  Y, M, D: Integer;
  Parsed: TDateTime;
begin
  Day:= 0;
  if Length(FileName) <> Length(FPrefix) + 10 + Length(FileExt) then EXIT(FALSE);
  if NOT SameText(Copy(FileName, 1, Length(FPrefix)), FPrefix) then EXIT(FALSE);
  if NOT SameText(Copy(FileName, Length(FPrefix) + 11, MaxInt), FileExt) then EXIT(FALSE);

  DatePart:= Copy(FileName, Length(FPrefix) + 1, 10);
  if (DatePart[5] <> '-') OR (DatePart[8] <> '-') then EXIT(FALSE);
  if NOT TryStrToInt(Copy(DatePart, 1, 4), Y)
  OR NOT TryStrToInt(Copy(DatePart, 6, 2), M)
  OR NOT TryStrToInt(Copy(DatePart, 9, 2), D)
  OR NOT TryEncodeDate(Y, M, D, Parsed)
  then EXIT(FALSE);

  Day:= Trunc(Parsed);
  Result:= TRUE;
end;


{ Deletes the files of this log older than DaysToKeep days. Runs on the caller's thread. }
procedure TLogDailyFile.DeleteOldFiles;
VAR
  SR: TSearchRec;
  Day, FirstKept: Integer;
  Failed: string;
begin
  if NOT DirectoryExists(FFolder) then EXIT;

  FirstKept:= Trunc(Clock()) - FDaysToKeep + 1;
  Failed:= '';
  if FindFirst(FFolder + FPrefix + '*' + FileExt, faAnyFile, SR) = 0 then
    TRY
      REPEAT
        if (SR.Attr AND faDirectory = 0)
        AND dayOfFileName(SR.Name, Day)
        AND (Day < FirstKept)
        AND NOT System.SysUtils.DeleteFile(FFolder + SR.Name)
        then Failed:= Failed + ' ' + SR.Name;
      UNTIL FindNext(SR) <> 0;
    FINALLY
      FindClose(SR);
    END;

  if Failed <> ''
  then reportError('Old log files could not be deleted from ' + FFolder + ':' + Failed);
end;


end.
