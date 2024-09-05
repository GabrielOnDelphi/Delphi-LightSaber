UNIT clRamLog;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

   A simple but effective log (non-visual).
   Its data can be displayed by TRichLog, but it can also work alone without being connected to a TRichLog.
   More details in llLogUtils.pas

   Future plans:
     TRamLog even though fully functional, it is a bit awkwardly written (uses text to keep track of verbosity level of the messages).
     The plan is to be replaced with VisLogRam.

   Tester:
     c:\Myprojects\Packages\LightSaber\Demo\LightLog\
=============================================================================================================}

//Note: use clVisLogRam is you want support for timestamp

INTERFACE

USES
   System.SysUtils, System.Classes, ccStreamBuff, llRichLog, llLogUtils, ccStreamMem;

TYPE
  TRamLog = class(TObject)
   private
     procedure setVerbosity(const Value: TLogVerb);
     procedure addNewMessage(const Level, Mesaj: string);
     function  getContent(Line: string): string;
   protected
     Indent: Integer;                                         { Intend each new added line with x spaces }
     FVerbosity: TLogVerb;
     RawLines: TStringList;                                   { Stores the actual log message and its associated verbosity }
     function  getLog(MsgType: Char): string;                 { UNUSED }
     function  getText: string;
     procedure setText(Value: string);
   public
     RichLog: TRichLog;                                       { Visual representation for the log. It may be NIL for objects that doesn't need a visual log }
     HasWarnings: Boolean;                                    { Indicates that at least an error/warning was stored in log }
     constructor Create;
     destructor Destroy;  override;
     procedure Clear(ClearVisualLog: Boolean);
     procedure Update;
     function  Count: Integer;

     { Export (old) }
     procedure importRawData(const RamLogRawLines: string);   { Access to the unformated lines of the log } { One ore more lines of data in TRamLog format (#1# text_text_text). Used by Baser to restore Log from disk }

     { Export (new) }
     procedure LoadFromStream(Stream: TCubicBuffStream);   overload;
     procedure LoadFromStream(Stream: TCubicMemStream);    overload;
     procedure SaveToStream  (Stream: TCubicBuffStream);   overload;
     procedure SaveToStream  (Stream: TCubicMemStream);    overload;

     { Add single-line message }
     procedure AddBold   (CONST Mesaj: string);
     procedure AddMsg    (CONST Mesaj: string);
     procedure AddVerb   (CONST Mesaj: string);
     procedure AddHint   (CONST Mesaj: string);
     procedure AddInfo   (CONST Mesaj: string);
     procedure AddImpo   (CONST Mesaj: string);
     procedure AddWarn   (CONST Mesaj: string);
     procedure AddError  (CONST Mesaj: string);
     {}
     procedure AddMsgInt (CONST Mesaj: string; i: Integer);
     procedure AddMsgLvl (CONST Mesaj: string; MsgType: TLogVerb);
     procedure AddEmptyRow;
     {}
     procedure Append   (RamLog: TRamLog);
     procedure ExportTo (aRichLog: TRichLog);
     procedure SaveAsTextFile(CONST FullPath: string);        { Save plain text to file }
     property  Verbosity: TLogVerb read FVerbosity write setVerbosity default lvVerbose;
     property  Text_: string read getText write setText;       { Extract plain text from raw data (RawLines) }
  end;


IMPLEMENTATION

Uses
   ccIO, ccCore;








{--------------------------------------------------------------------------------------------------
   RAM LOG
--------------------------------------------------------------------------------------------------}

constructor TRamLog.Create;
begin
 inherited;
 RawLines:= TStringList.Create;
 FVerbosity:= lvVerbose;   { IMPORTAN: Put all info in RamLog but let the GUI control the verbosity of the RichLog. }
end;


destructor TRamLog.Destroy;
begin
 FreeAndNil(RawLines);
 inherited Destroy;
end;


{ Note: Maybe we don't want to clear the visual log here. The visual log might have text from other RamLogs. The the GUI control the RichLog }
{ If ClearVisualLog is true, it will clear not only itself but also the associated/visual TRitchLog }
procedure TRamLog.Clear(ClearVisualLog: Boolean);
begin
 Indent:= 0;
 HasWarnings:= FALSE;      { Indicates that at least an error/warning was stored in log }
 RawLines.Clear;

 if ClearVisualLog
 AND (RichLog <> NIL)
 then RichLog.Clear;
end;


procedure TRamLog.Update;
begin
 if RichLog<> NIL
 then RichLog.Update;
end;






{----------------------------------------------
   UTILS
----------------------------------------------}

procedure TRamLog.addNewMessage(CONST Level, Mesaj: string);
VAR
   i: Integer;
   s: string;
   TSL: TStringList;
begin
 TSL:= String2TSL(Mesaj);
 TRY
   for i:= 0 to tsl.Count-1 DO
    begin
     s:= TSL[i];
     s:= StringOfChar (' ', Indent)+ s;
     RawLines.Add(Level + s);          { Here I got: "EOutOfResources-RichEdit line insertion error" from two users }
    end;
 FINALLY
   FreeAndNil(TSL);
 END;
end;


function TRamLog.Count: Integer;
begin
 Result:= RawLines.Count;
end;


procedure TRamLog.setVerbosity(const Value: TLogVerb);
begin
 FVerbosity := Value;
 //Don't set RichLog verbosity here. Put all info in RamLog but let the GUI control the verbosity of the RichLog. if RichLog <> NIL then RichLog.Verbosity:= Value;
end;


function GetLevel(CONST Line: string): TLogVerb;        { Extracts the verbosity level from the provided raw (formated) line }
begin
   Assert(Length(Line) > 1, 'Line is too short: '+ Line);
   case Line[2] of
    '1': Result:= lvVerbose;
    '2': Result:= lvHints;
    '3': Result:= lvInfos;
    '4': Result:= lvImportant;
    '5': Result:= lvWarnings;
    '6': Result:= lvErrors;
    else Result:= lvInfos;  { '7' and 'b' }
   end;
end;






{-------------------------------------------------
   APPEND
-------------------------------------------------}

{ Appends formated lines from the specified RamLog, based on AppData.LogInfos, AppData.LogHints, AppData.LogWarns, AppData.LogErrors settings }
procedure TRamLog.Append(RamLog: TRamLog);   { UNUSED }
begin
 for var s in RamLog.RawLines DO
   RawLines.Add(s);
end;


{ Import log raw lines in log. Used by Baser5.15 and below to restore Log from disk.
  The format is: #1# text_text_text

  DEPRECATED!
    Don't use anymore to import the log from disk! Use SaveToStream/LoadFromStream instead.
    It is here only for compatibility with Baser below v5.20! }
procedure TRamLog.importRawData(CONST RamLogRawLines: string);
VAR
   s: string;
   TSL: TStringList;
begin
 TSL:= String2TSL(RamLogRawLines);
 TRY
  for s in TSL DO
   if (s= '')  OR (s= ' ') //fixes some issues I introduced in 2022(?)
   then Continue
   else
     if RichLog <> NIL
     then RichLog.AddMsg(GetContent(s), getlevel(s));
  RawLines.Add(s);
 FINALLY
   FreeAndNil(TSL);
 END;
end;



{-------------------------------------------------
   EXPORT TEXT
-------------------------------------------------}
procedure TRamLog.ExportTo(aRichLog: TRichLog);
begin
 for VAR s in RawLines DO
   if Length(s) < 2      { This is for compatibility with old DNA Baser projects in which the log was saved to disk directly as TXT }
   then AddMsg(s)
   else aRichLog.AddMsg(GetContent(s), GetLevel(s));
end;


procedure TRamLog.SaveAsTextFile(CONST FullPath: string);    { Save plain text to file }
begin
 StringToFile(FullPath, Text_, woOverwrite);
end;



{-------------------------------------------------
   EXPORT STREAM
-------------------------------------------------}
CONST
   LogHeader= CRLF+'#Log';

procedure TRamLog.SaveToStream(Stream: TCubicBuffStream);
begin
  Stream.WriteStringA(LogHeader);
  Stream.WriteInteger(RawLines.Count);
  Stream.WritePadding0(16);

  for VAR s in RawLines DO
    Stream.WriteStringU(s);
end;

procedure TRamLog.SaveToStream(Stream: TCubicMemStream);  { For compatibility }
begin
  Stream.WriteStringA(LogHeader);
  Stream.WriteInteger(RawLines.Count);
  Stream.WritePadding(16);

  for VAR s in RawLines DO
    Stream.WriteStringU(s);
end;





procedure TRamLog.LoadFromStream(Stream: TCubicBuffStream);
VAR s: string;
begin
  s:= string(Stream.ReadStringA);
  if s <> LogHeader
  then RAISE Exception.Create('Invalid log header!');
  VAR iCount:= Stream.ReadInteger;
  Stream.ReadPadding0(16);

  for VAR i:= 1 to iCount DO
   begin
    s:= Stream.ReadStringU;
    RawLines.Add(s);
   end;
end;


procedure TRamLog.LoadFromStream(Stream: TCubicMemStream);   { For compatibility }
VAR s: string;
begin
  s:= string(Stream.ReadStringA);
  if s <> LogHeader
  then RAISE Exception.Create('Invalid log header!');
  VAR iCount:= Stream.ReadInteger;
  Stream.ReadPadding(16);

  for VAR i:= 1 to iCount DO
   begin
    s:= Stream.ReadStringU;
    RawLines.Add(s);
   end;
end;










{-----------------------------------------
   FULL BODY TEXT
-----------------------------------------}

procedure TRamLog.setText(Value: string);
begin
 RawLines.Clear;
 AddMsg(Value);
end;


function TRamLog.getText: string;         { Extract plain text from raw data (RawLines) }
VAR i: Integer;
begin
 Result:= '';
 for i:= 0 to RawLines.Count-1
   DO Result:= Result+ GetContent(RawLines[i])+ CRLF;

 {Cut the last enter}
 Result:= system.COPY(Result, 1, Length(Result)-2);   { Delete last enter }
end;



{-----------------------------------------
   ADD SINGLE LINE
-----------------------------------------}

procedure TRamLog.AddEmptyRow;
begin
 if RichLog<> NIL
 then RichLog.AddEmptyRow;
 RawLines.Add(' ');
end;


procedure TRamLog.AddMsgInt(const Mesaj: string; i: Integer);
begin
 AddInfo(Mesaj+ IntToStr(i));
end;


procedure TRamLog.AddMsgLvl(CONST Mesaj: string; MsgType: TLogVerb);
begin
 case MsgType of
   lvVerbose  : AddVerb(Mesaj);
   lvHints    : AddHint(Mesaj);
   lvInfos    : AddInfo(Mesaj);
   lvImportant: AddImpo(Mesaj);
   lvWarnings : AddWarn(Mesaj);
   lvErrors   : AddError(Mesaj);
 end;
end;


procedure TRamLog.AddBold(CONST Mesaj: string);
begin
 if RichLog<> NIL
 then RichLog.AddBold(Mesaj);
 addNewMessage('#b#', Mesaj);
end;









procedure TRamLog.AddVerb(CONST Mesaj: string);
begin
 if RichLog<> NIL
 then RichLog.AddVerb(Mesaj);

 if Verbosity<= lvVerbose
 then addNewMessage('#1#', Mesaj);
end;


procedure TRamLog.AddHint(CONST Mesaj: string);
begin
 if RichLog<> NIL
 then RichLog.AddHint(Mesaj);

 if Verbosity<= lvHints
 then addNewMessage('#2#', Mesaj);
end;


procedure TRamLog.AddInfo(CONST Mesaj: string);
begin
 if RichLog<> NIL
 then RichLog.AddInfo(Mesaj);

 if Verbosity<= lvinfos
 then addNewMessage('#3#', Mesaj);
end;


procedure TRamLog.AddImpo(CONST Mesaj: string);
begin
 if RichLog<> NIL
 then RichLog.AddImpo(Mesaj);

 if Verbosity<= lvImportant
 then addNewMessage('#4#', Mesaj);
end;


procedure TRamLog.AddWarn(CONST Mesaj: string);
begin
 if RichLog<> NIL
 then RichLog.AddWarn(Mesaj);
 if Verbosity<= lvWarnings
 then addNewMessage('#5#', Mesaj);

 HasWarnings:= TRUE;   { Indicates that at least an error/warning was stored in log }
end;


procedure TRamLog.AddError(CONST Mesaj: string);
begin
 if RichLog<> NIL
 then RichLog.AddError(Mesaj);
 if Verbosity<= lvErrors
 then addNewMessage('#6#', Mesaj);

 HasWarnings:= TRUE;   { Indicates that at least an error/warning was stored in log }
end;


procedure TRamLog.AddMsg(CONST Mesaj: string);
begin
 if RichLog<> NIL
 then RichLog.AddMsg(Mesaj);
 addNewMessage('#7#', Mesaj);
end;






{------------------------------------
   GET
------------------------------------}

function TRamLog.getLog(MsgType: Char): string; { UNUSED }
VAR i: Integer;
    s: string;
begin
 Result:= '';
 for i:= 0 to RawLines.Count-1 DO
  begin
   s:= RawLines[i];
   if s= ''
   then Result:= Result+ CRLF
   else
      if s[2]= MsgType
      then Result:= Result+ GetContent(RawLines[i])+ CRLF;
  end;

 {Cut the last enter}
 Result:= system.COPY(Result, 1, Length(Result)-2);
end;


function TRamLog.getContent(Line: string): string;      { Extracts the message from the provided raw (formated) line }
begin
 Result:= System.Copy(Line, 4, MaxInt);
end;


end.
