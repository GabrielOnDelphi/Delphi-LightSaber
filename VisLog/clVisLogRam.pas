UNIT clVisLogRam;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Non-visual log.
   Its data can be shown by TVisLog, but it can also work alone without being connected to a TVisLog.

   Verbosity:
     Supports several verbosity levels (verbose, info, warnings, errors, etc)

   Future plans
     TRamLog even though functional is a bit outdate. The plan is to be replaced with TVisRamLog

   Tester:
     c:\Myprojects\Packages\LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes, Vcl.Graphics, Vcl.ExtCtrls, clVisLogLines, clVisLogUtils;

TYPE
  TVisRamLog = class(TObject)
   private
    procedure UpdateVisLog;
   protected
     FVisLog: TPanel;  { This will be typecasted to TVisLog }
     function prepareString(CONST Msg: string): string;
   public
     Lines: TLogLines;
     constructor Create;
     destructor Destroy;  override;
     procedure Clear;
     function  Count: Integer;

     procedure AddBold    (const Msg: string);
     procedure AddMsg     (CONST Msg: string);                  overload;
     procedure AddMsg     (CONST Msg: string; Color: TColor);   overload;
     procedure AddMsgInt  (CONST Msg: string; i: Integer);
     procedure AddEmptyRow;

     procedure AddDebug   (CONST Msg: string);
     procedure AddVerb    (CONST Msg: string);
     procedure AddHint    (CONST Msg: string);
     procedure AddInfo    (CONST Msg: string);                             { Default level }
     procedure AddImpo    (CONST Msg: string);
     procedure AddWarn    (CONST Msg: string);
     procedure AddError   (CONST Msg: string);
     {}
     function  GetAsText: string;
     {}
     procedure SaveAsText  (CONST FullPath: string);
     procedure SaveAsRtf   (CONST FullPath: string);

     procedure SaveToFile  (CONST FullPath: string);
     function  LoadFromFile(CONST FullPath: string): Boolean;

     procedure SetVisLog(Log: TPanel);
  end;


IMPLEMENTATION
{$WARN GARBAGE OFF}   {Silence the: 'W1011 Text after final END' warning }

USES
  ccCore, ccIO, ccStreamBuff, clVisLog;







{-----------------------------------------------------------------------
   CTOR
------------------------------------------------------------------------}
constructor TVisRamLog.Create;
begin
 inherited Create;
 FVisLog := NIL;
 Lines:= TLogLines.Create;
end;


destructor TVisRamLog.Destroy;
begin
 FreeAndNil(Lines);
 inherited;
end;


procedure TVisRamLog.Clear;
begin
 Lines.Clear;   { Call Clear to empty the Items array and set the Count to 0. Clear also frees the memory used to store the Items array and sets the Capacity to 0. }
 UpdateVisLog;
end;









{--------------------------------------------------------------------------------------------------
   STUFF
--------------------------------------------------------------------------------------------------}

function TVisRamLog.Count: Integer;
begin
 Result:= Lines.Count;
end;


procedure TVisRamLog.UpdateVisLog;
begin
 if FVisLog<> NIL
 then (FVisLog as TVisLog).ContentChanged;
end;









{--------------------------------------------------------------------------------------------------
   ADD GENERIC MESSAGE
--------------------------------------------------------------------------------------------------}

procedure TVisRamLog.AddBold (CONST Msg: string);
begin
 Lines.AddNewLine( prepareString(Msg), lvInfos, TRUE );
 UpdateVisLog;
end;


procedure TVisRamLog.AddMsg(CONST Msg: string);
begin
 Lines.AddNewLine( prepareString(Msg), lvInfos, FALSE );
 UpdateVisLog;
end;


procedure TVisRamLog.AddMsg(CONST Msg: string; Color: TColor);
begin
 Lines.AddNewLine( prepareString(Msg), lvInfos, FALSE, Color);
 UpdateVisLog;
end;


procedure TVisRamLog.AddMsgInt(const Msg: string; i: Integer);   { Adds a message text followed by an integer }
begin
 Lines.AddNewLine( prepareString(Msg)+ IntToStr(i), lvInfos, FALSE );
 UpdateVisLog;
end;


procedure TVisRamLog.AddEmptyRow;
begin
 AddMsg('');
end;






{--------------------------------------------------------------------------------------------------
   ADD MESSAGE BY VERBOSITY
--------------------------------------------------------------------------------------------------}
procedure TVisRamLog.AddDebug(CONST Msg: string);                             { Relevance 0 }
begin
 Lines.AddNewLine(prepareString(Msg), lvDebug, FALSE );
 UpdateVisLog;
end;


procedure TVisRamLog.AddVerb(CONST Msg: string);                               { Relevance 1 }
begin
 Lines.AddNewLine(prepareString(Msg), lvVerbose, FALSE );
 UpdateVisLog;
end;


procedure TVisRamLog.AddHint(CONST Msg: string);                               { Relevance 2 }
begin
 Lines.AddNewLine( prepareString(Msg), lvHints, FALSE );
 UpdateVisLog;
end;


procedure TVisRamLog.AddInfo(CONST Msg: string);                               { Relevance 3 }
begin
 Lines.AddNewLine( prepareString(Msg), lvInfos, FALSE );
 UpdateVisLog;
end;


procedure TVisRamLog.AddImpo(CONST Msg: string);                               { Relevance 4 }
begin
 Lines.AddNewLine( prepareString(Msg), lvImportant, FALSE );
 UpdateVisLog;
end;


procedure TVisRamLog.AddWarn(CONST Msg: string);                               { Relevance 5 }
begin
 Lines.AddNewLine( prepareString(Msg), lvWarnings, FALSE );
 UpdateVisLog;
end;


procedure TVisRamLog.AddError(CONST Msg: string);                              { Relevance 6 }
begin
 Lines.AddNewLine( prepareString(Msg), lvErrors, FALSE );
 UpdateVisLog;
end;











{--------------------------------------------------------------------------------------------------
   TEXT
--------------------------------------------------------------------------------------------------}

function TVisRamLog.GetAsText: string;
VAR i: Integer;
begin
 Result:= '';
 for i:= 0 to Lines.Count-1
   DO Result:= Result+ Lines[i].Msg+ CRLF;

 {Cut the last enter}
 Result:= ccCore.RemoveLastEnter(Result);
end;



function TVisRamLog.prepareString(CONST Msg: string): string;
begin
 Result:= ReplaceEnters (Msg , ' ');
 //Result:= StringOfChar(' ', Indent)+ Result;
end;











{-----------------------------------------------------------------------
   I/O
------------------------------------------------------------------------}

procedure TVisRamLog.SaveAsRtf(const FullPath: string);
begin
 Assert(1=2, 'Not implemented!');
 // RichEdit:= TRichEdit.Create;
 // for all do RichEdit.addline(Lines[i].text, Color);
end;


procedure TVisRamLog.SaveAsText(CONST FullPath: string);
begin
 StringToFile(FullPath, GetAsText, woOverwrite, FALSE);
end;




CONST
   MagicNo  = 'CubiclRamLog';
   MagicVer = 2;

function TVisRamLog.LoadFromFile(const FullPath: string): Boolean;
VAR
   Stream: TCubicBuffStream;
begin
 Stream:= TCubicBuffStream.CreateRead(FullPath);
 TRY
   Result:= Stream.ReadHeaderTry(MagicNo, MagicVer);
   if NOT Result then EXIT;

   Lines.ReadFromStream(Stream);
   Stream.ReadPadding(1024);

   if FVisLog <> Nil
   then UpdateVisLog;
 FINALLY
   FreeAndNil(Stream);
 END;
end;


procedure TVisRamLog.SaveToFile(const FullPath: string);
VAR
   Stream: TCubicBuffStream;
begin
 Stream:= TCubicBuffStream.CreateWrite(FullPath);
 TRY
   Stream.WriteHeader(MagicNo, MagicVer);
   Lines.WriteToStream(Stream);
   Stream.WritePadding(1024);
 FINALLY
   FreeAndNil(Stream);
 END;
end;


{ If we call this manually, at runtime, we need to call UpdateVisLog after it, to transfer the content of the RamLog into the VisualLog }
procedure TVisRamLog.SetVisLog(Log: TPanel);
begin
 Assert(Log <> Nil, 'TVisRamLog is nil!');
 Assert(Log is TVisLog, 'The object provided is not TVisRamLog!');
 FVisLog:= Log;
end;


end.
