UNIT LightCore.IOPlatformFile;

{=============================================================================================================
   Gabriel Moraru
   2026.09.03
--------------------------------------------------------------------------------------------------------------
   Line ending conversion utilities for cross-platform text files.

   Line ending formats:
     - Windows (CRLF)  : $0D $0A (CR + LF)
     - Unix/Linux (LF) : $0A only
     - Classic Mac (CR): $0D only (pre-OS X, rarely used today)

   Note: Modern macOS uses Unix-style LF line endings.

   Also see: LightCore.TextFile.pas, System.SysUtils.AdjustLineBreaks()
--------------------------------------------------------------------------------------------------------------
   Tester app:
      c:\Projects\Project Support\Tool - Light Delphi utilities (DUT)\LDU.dpr
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes;

CONST
  CHAR_CR = $0D;  { Carriage Return }
  CHAR_LF = $0A;  { Line Feed }

TYPE
  TConvertNotifyKind = (nkMax, nkProgress);
  TConvertNotify     = reference to procedure(Kind: TConvertNotifyKind; Value: LongInt);   { 2026.09.03: was a plain procedure pointer, which cannot capture a progress bar. That is why every caller passed NIL }
  TEnterType         = (etUnknown, etWin, etNix, etMac);
  EnterType = TEnterType;  { Backward compatibility alias }

 function  IsMacFile     (InStream: TStream): Boolean;                                                { Returns true if the Enter is format from a single CR character }
 function  GetEnterType  (InStream: TStream): EnterType;
 function  GetEnterTypeS (CONST InputFile: string): string;

 procedure WinToUnix     (InStream: TStream; OutStream: TStream; Notify: TConvertNotify);   overload;
 procedure UnixToWin     (InStream: TStream; OutStream: TStream; Notify: TConvertNotify);   overload;
 procedure MacToWin      (InStream: TStream; OutStream: TStream);                           overload;
 procedure AnyToWin      (InStream: TStream; OutStream: TStream; Notify: TConvertNotify);   overload;   { Any mix of CR / LF / CRLF to CRLF }

 procedure WinToUnix     (CONST InputFile, OutputFile: String; Notify: TConvertNotify);     overload;
 procedure UnixToWin     (CONST InputFile, OutputFile: String; Notify: TConvertNotify);     overload;
 function  MacToWin      (CONST InputFile, OutputFile: string): Boolean;                    overload; { CR to CRLF }
 procedure AnyToWin      (CONST InputFile, OutputFile: String; Notify: TConvertNotify);     overload;

 function  FixEntersInPlace (CONST FileName: string): Boolean;    { Rewrites the file to CRLF only if it is not already CRLF-only. TRUE = the file was changed }


IMPLEMENTATION
USES LightCore.Types;


{ Detects line ending format in the stream.
  Returns etUnknown if no line endings found (single-line file).
  Note: Stream position is modified; caller should save/restore if needed. }
function GetEnterType(InStream: TStream): TEnterType;
VAR
   B1, B2: Byte;
   SavedPos: Int64;
begin
 if NOT Assigned(InStream)
 then raise exception.Create('Input stream not assigned!');

  Result:= etUnknown;
  SavedPos:= InStream.Position;

  while InStream.Position < InStream.Size do
  begin
    InStream.Read(B1, 1);
    case B1 of
      CHAR_CR:  { Found CR - could be Win (CRLF) or Mac (CR only) }
        begin
          if InStream.Position < InStream.Size
          then begin
            InStream.Read(B2, 1);
            if B2 = CHAR_LF
            then EXIT(etWin)   { CR+LF = Windows }
            else EXIT(etMac);  { CR alone = Classic Mac }
          end
          else EXIT(etMac);    { CR at end of file = Classic Mac }
        end;
      CHAR_LF: EXIT(etNix);    { LF alone = Unix/Linux }
    end;
  end;

  InStream.Position:= SavedPos;  { Restore position if no line ending found }
end;


function GetEnterTypeS(CONST InputFile: string): string;
VAR
   InpStream: System.Classes.TBufferedFileStream;
begin
 if NOT FileExists(InputFile)
 then raise exception.Create('Input file does not exist!');

 InpStream:= TBufferedFileStream.Create(InputFile, fmOpenRead OR fmShareDenyWrite);   { Plain fmOpenRead is fmShareCompat which acts as EXCLUSIVE on Windows }
 TRY
  case GetEnterType(InpStream) of
    etWin: result:= 'Win';
    etNix: result:= 'Nix';
    etMac: result:= 'Mac';
   else
     result:= 'unknown';
  end;
 FINALLY
  FreeAndNil(InpStream);
 END;
end;




{ Returns True if the file uses classic Mac line endings (CR only, not CRLF).
  Note: Stream position is modified; caller should save/restore if needed. }
function IsMacFile(InStream: TStream): Boolean;
VAR
   B: Byte;
begin
 if NOT Assigned(InStream) then raise exception.Create('IsMacFile - Input stream not assigned!');

  Result:= FALSE;
  while InStream.Position < InStream.Size do
  begin
    InStream.Read(B, 1);
    if B = CHAR_CR
    then begin
      { Check if CR is followed by LF (Windows) or standalone (Mac) }
      if InStream.Position < InStream.Size
      then begin
        InStream.Read(B, 1);
        if B <> CHAR_LF
        then EXIT(TRUE);  { CR not followed by LF = Mac format }
      end
      else EXIT(TRUE);    { CR at end of file = Mac format }
    end;
  end;
end;



{ Convert Unix (LF) to Windows (CRLF) line endings.
  Replaces each SOLITARY LF ($0A) with CRLF ($0D $0A). A LF that already has a CR in front of it
  is left alone, so a file holding both kinds is repaired instead of corrupted.
  A solitary CR (classic Mac) is NOT touched here - use AnyToWin for that.
  See also: System.SysUtils.AdjustLineBreaks() }
procedure UnixToWin(InStream: TStream; OutStream: TStream; Notify: TConvertNotify);
VAR
   B, PrevB: Byte;
   Pair: array[0..1] of Byte;
   BytesProcessed: Int64;
begin
 if NOT Assigned(InStream)  then raise exception.Create('UnixToWin-Input stream not assigned!');
 if NOT Assigned(OutStream) then raise exception.Create('UnixToWin-Output stream not assigned!');

  if Assigned(Notify)
  then Notify(nkMax, InStream.Size);

  Pair[0]:= CHAR_CR;
  Pair[1]:= CHAR_LF;
  PrevB:= 0;
  BytesProcessed:= 0;
  while InStream.Position < InStream.Size do
  begin
    InStream.Read(B, 1);

    { A LF with a CR before it is a finished Windows line ending. Writing a second CR
      would give CR CR LF, which is what corrupted files that held both kinds. }
    if (B = CHAR_LF) AND (PrevB <> CHAR_CR)
    then OutStream.Write(Pair, 2)   { solitary LF -> CRLF }
    else OutStream.Write(B, 1);

    PrevB:= B;

    if Assigned(Notify)
    then begin
      Inc(BytesProcessed);
      Notify(nkProgress, BytesProcessed);
    end;
  end;

  { Signal completion }
  if Assigned(Notify)
  then Notify(nkProgress, 0);
end;



{ Convert Windows (CRLF) to Unix (LF) line endings.
  Removes CR ($0D) characters, keeping only LF ($0A).
  BUG FIX: Previous version incorrectly removed LF instead of CR! }
procedure WinToUnix(InStream: TStream; OutStream: TStream; Notify: TConvertNotify);
VAR
   B: Byte;
   BytesProcessed: Int64;
begin
 if NOT Assigned(InStream)  then raise exception.Create('WinToUnix-Input stream not assigned!');
 if NOT Assigned(OutStream) then raise exception.Create('WinToUnix-Output stream not assigned!');

  if Assigned(Notify)
  then Notify(nkMax, InStream.Size);

  BytesProcessed:= 0;
  while InStream.Position < InStream.Size do
  begin
    InStream.Read(B, 1);

    { Skip CR characters - keep everything else including LF }
    if B <> CHAR_CR
    then OutStream.Write(B, 1);

    if Assigned(Notify)
    then begin
      Inc(BytesProcessed);
      Notify(nkProgress, BytesProcessed);
    end;
  end;

  { Signal completion }
  if Assigned(Notify)
  then Notify(nkProgress, 0);
end;



{ Convert classic Mac (CR) to Windows (CRLF) line endings.
  Replaces each SOLITARY CR ($0D) with CRLF ($0D $0A). A CR that is already followed by a LF is
  left alone, so a file holding both kinds is repaired instead of corrupted.
  A solitary LF (Unix) is NOT touched here - use AnyToWin for that. }
procedure MacToWin(InStream: TStream; OutStream: TStream);
VAR
   B: Byte;
   Pair: array[0..1] of Byte;
   PendingCR: Boolean;
begin
 if NOT Assigned(InStream)  then raise exception.Create('MacToWin-Input stream not assigned!');
 if NOT Assigned(OutStream) then raise exception.Create('MacToWin-Output stream not assigned!');

  Pair[0]:= CHAR_CR;
  Pair[1]:= CHAR_LF;
  PendingCR:= FALSE;

  while InStream.Position < InStream.Size do
  begin
    InStream.Read(B, 1);

    { A CR is held back for one byte, because only the byte after it says whether it is a solitary
      CR (Mac) or the first half of a CRLF pair that is already correct. Either way it comes out
      as CRLF - the difference is whether the LF that follows is written again or dropped. }
    if PendingCR then
     begin
       OutStream.Write(Pair, 2);
       PendingCR:= FALSE;
       if B = CHAR_LF
       then CONTINUE;      { that LF belonged to the CR and was just written - do not write it twice }
     end;

    if B = CHAR_CR
    then PendingCR:= TRUE
    else OutStream.Write(B, 1);
  end;

  if PendingCR             { CR as the very last byte of the file }
  then OutStream.Write(Pair, 2);
end;



{ Convert ANY mix of line endings to Windows (CRLF), in one pass.
  Solitary LF -> CRLF, solitary CR -> CRLF, an existing CRLF stays as it is.
  This is the one to call when you do not know (or do not care) what the file holds - for example
  a source tree where some files are Unix, some Windows, and some hold both kinds inside one file. }
procedure AnyToWin(InStream: TStream; OutStream: TStream; Notify: TConvertNotify);
VAR
   B: Byte;
   Pair: array[0..1] of Byte;
   PendingCR: Boolean;
   BytesProcessed: Int64;
begin
 if NOT Assigned(InStream)  then raise exception.Create('AnyToWin-Input stream not assigned!');
 if NOT Assigned(OutStream) then raise exception.Create('AnyToWin-Output stream not assigned!');

  if Assigned(Notify)
  then Notify(nkMax, InStream.Size);

  Pair[0]:= CHAR_CR;
  Pair[1]:= CHAR_LF;
  PendingCR:= FALSE;
  BytesProcessed:= 0;

  while InStream.Position < InStream.Size do
  begin
    InStream.Read(B, 1);

    if PendingCR then
     begin
       OutStream.Write(Pair, 2);      { the held-back CR, written as a full CRLF }
       PendingCR:= FALSE;
       if B = CHAR_LF
       then begin                     { that LF belonged to the CR - already written }
         if Assigned(Notify) then
          begin
            Inc(BytesProcessed);
            Notify(nkProgress, BytesProcessed);
          end;
         CONTINUE;
       end;
     end;

    if B = CHAR_CR
    then PendingCR:= TRUE             { decide on the next byte }
    else
      if B = CHAR_LF
      then OutStream.Write(Pair, 2)   { solitary LF -> CRLF }
      else OutStream.Write(B, 1);

    if Assigned(Notify)
    then begin
      Inc(BytesProcessed);
      Notify(nkProgress, BytesProcessed);
    end;
  end;

  if PendingCR                        { CR as the very last byte of the file }
  then OutStream.Write(Pair, 2);

  { Signal completion }
  if Assigned(Notify)
  then Notify(nkProgress, 0);
end;



{ File-based conversion: Windows to Unix line endings }
procedure WinToUnix(CONST InputFile, OutputFile: String; Notify: TConvertNotify);
VAR
   InpStream: TBufferedFileStream;
   OutStream: TBufferedFileStream;
begin
  if NOT FileExists(InputFile)
  then raise Exception.Create('WinToUnix: Input file does not exist: ' + InputFile);

  InpStream:= TBufferedFileStream.Create(InputFile, fmOpenRead OR fmShareDenyWrite, 1*mb);
  TRY
    OutStream:= TBufferedFileStream.Create(OutputFile, fmCreate OR fmOpenWrite, 1*mb);
    TRY
      WinToUnix(InpStream, OutStream, Notify);
    FINALLY
      FreeAndNil(OutStream);
    END;
  FINALLY
    FreeAndNil(InpStream);
  END;
end;


{ File-based conversion: Unix to Windows line endings }
procedure UnixToWin(CONST InputFile, OutputFile: String; Notify: TConvertNotify);
VAR
   InpStream: TBufferedFileStream;
   OutStream: TBufferedFileStream;
begin
  if NOT FileExists(InputFile)
  then raise Exception.Create('UnixToWin: Input file does not exist: ' + InputFile);

  InpStream:= nil;
  OutStream:= nil;  
  TRY
    InpStream:= TBufferedFileStream.Create(InputFile, fmOpenRead OR fmShareDenyWrite, 1*mb);
    OutStream:= TBufferedFileStream.Create(OutputFile, fmCreate OR fmOpenWrite, 1*mb);
    UnixToWin(InpStream, OutStream, Notify);
  FINALLY
    FreeAndNil(OutStream); 
    FreeAndNil(InpStream);
  END;
end;


{ File-based conversion: Classic Mac (CR) to Windows (CRLF).
  Returns True if the file was Mac format and was converted.
  Returns False if the file was not Mac format (no conversion performed). }
function MacToWin(CONST InputFile, OutputFile: string): Boolean;
VAR
   InpStream: TBufferedFileStream;
   OutStream: TBufferedFileStream;
begin
  if NOT FileExists(InputFile)
  then raise Exception.Create('MacToWin: Input file does not exist: ' + InputFile);

  InpStream:= TBufferedFileStream.Create(InputFile, fmOpenRead OR fmShareDenyWrite, 1*mb);
  TRY
    Result:= IsMacFile(InpStream);
    if Result then 
    begin
      InpStream.Position:= 0;  { Reset after IsMacFile scan }
      OutStream:= TBufferedFileStream.Create(OutputFile, fmCreate OR fmOpenWrite, 1*mb);
      TRY
        MacToWin(InpStream, OutStream);
      FINALLY
        FreeAndNil(OutStream);
      END;
    end;
  FINALLY
    FreeAndNil(InpStream);
  END;
end;


{ File-based conversion: any mix of CR / LF / CRLF to Windows CRLF }
procedure AnyToWin(CONST InputFile, OutputFile: String; Notify: TConvertNotify);
VAR
   InpStream: TBufferedFileStream;
   OutStream: TBufferedFileStream;
begin
  if NOT FileExists(InputFile)
  then raise Exception.Create('AnyToWin: Input file does not exist: ' + InputFile);

  InpStream:= nil;
  OutStream:= nil;
  TRY
    InpStream:= TBufferedFileStream.Create(InputFile, fmOpenRead OR fmShareDenyWrite, 1*mb);
    OutStream:= TBufferedFileStream.Create(OutputFile, fmCreate OR fmOpenWrite, 1*mb);
    AnyToWin(InpStream, OutStream, Notify);
  FINALLY
    FreeAndNil(OutStream);
    FreeAndNil(InpStream);
  END;
end;


{ Rewrites the file so that every line ending is CRLF.
  The file is written back ONLY when the conversion really changed something, so a folder of
  already-correct files keeps its time stamps and nothing is rewritten for nothing.
  Returns TRUE when the file was changed. }
function FixEntersInPlace(CONST FileName: string): Boolean;
VAR
   InpStream, OutStream: TMemoryStream;
begin
  if NOT FileExists(FileName)
  then raise Exception.Create('FixEntersInPlace: Input file does not exist: ' + FileName);

  InpStream:= TMemoryStream.Create;
  TRY
    InpStream.LoadFromFile(FileName);
    OutStream:= TMemoryStream.Create;
    TRY
      AnyToWin(InpStream, OutStream, NIL);

      Result:= (OutStream.Size <> InpStream.Size)
            OR ((InpStream.Size > 0) AND NOT CompareMem(InpStream.Memory, OutStream.Memory, InpStream.Size));

      if Result
      then OutStream.SaveToFile(FileName);
    FINALLY
      FreeAndNil(OutStream);
    END;
  FINALLY
    FreeAndNil(InpStream);
  END;
end;


end.
