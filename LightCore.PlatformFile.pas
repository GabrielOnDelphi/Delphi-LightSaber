UNIT LightCore.PlatformFile;

{=============================================================================================================
   Gabriel Moraru
   2024.05
--------------------------------------------------------------------------------------------------------------
   Converts the 'Enter' between Mac, Linux and Win.

   Also see: LightCore.TextFile.pas
--------------------------------------------------------------------------------------------------------------
   Tester app:
      c:\Projects\Project Support\Tool - Light Delphi utilities (DUT)\LDU.dpr
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes;

TYPE
  EnterType          = (etUnknown, etWin, etNix, etMac);
  TConvertNotifyKind = (nkMax, nkProgress);
  TConvertNotify     = procedure(Kind: TConvertNotifyKind; Value: LongInt);

 function  IsMacFile     (InStream: TStream): Boolean;                                                { Returns true if the Enter is format from a single CR character }
 function  GetEnterType  (InStream: TStream): EnterType;
 function  GetEnterTypeS (CONST InputFile: string): string;

 procedure WinToUnix     (InStream: TStream; OutStream: TStream; Notify: TConvertNotify);   overload;
 procedure UnixToWin     (InStream: TStream; OutStream: TStream; Notify: TConvertNotify);   overload;
 procedure MacToWin      (InStream: TStream; OutStream: TStream);                           overload;

 procedure WinToUnix     (CONST InputFile, OutputFile: String; Notify: TConvertNotify);     overload;
 procedure UnixToWin     (CONST InputFile, OutputFile: String; Notify: TConvertNotify);     overload;
 function  MacToWin      (CONST InputFile, OutputFile: string): Boolean;                    overload; { CR to CRLF }


IMPLEMENTATION
USES LightCore, LightCore.Time, LightCore.Types;


{ Detects if the specified stream contains Windows or Linux enters }
function GetEnterType(InStream: TStream): EnterType;
VAR
   B1, B2: Byte;
begin
 if NOT Assigned(InStream)
 then raise exception.Create('Input stream not assigned!');

 Result:= etUnknown;
 WHILE InStream.Position < InStream.Size-1 DO
  begin
   InStream.Read(B1, 1);
   case B1 of
    $0D:                           { Look for Mac }
     begin
      InStream.Read(B2, 1);        { Check next char }
      if B2= $0A
      then EXIT(etWin)             { Win: 0D0A }
      else EXIT(etMac);            { Mac: 0D }
     end;
    $0A: EXIT(etNix);              { Linux: 0A }
   end;
  end;
end;


function GetEnterTypeS(CONST InputFile: string): string;
VAR
   InpStream: System.Classes.TBufferedFileStream;
begin
 if NOT FileExists(InputFile)
 then raise exception.Create('Input file does not exist!');

 InpStream:= TBufferedFileStream.Create(InputFile, fmOpenRead);
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




{ Returns true if the Enter is format from a single CR character }
function IsMacFile(InStream: TStream): Boolean;
VAR
   B: Byte;
begin
 if NOT Assigned(InStream) then raise exception.Create('Input stream not assigned!');

 Result:= FALSE;
 WHILE InStream.Position < InStream.Size-1 DO
  begin
   InStream.Read(B, 1);
   if B= $0D then
     begin
      InStream.Read(B, 1);
      if (B <> $0A) then EXIT(TRUE);        { Make sure the next char is NOT a LF char }
     end;
  end;
end;



{ Convert all ENTERs in this file from Linux (LF) to Windows (CRLF) format.
  Gives feedback (so you can update a progress bar) as the file is processed.
  EXISTS: System.SysUtils.AdjustLineBreaks() }
procedure UnixToWin(InStream: TStream; OutStream: TStream; Notify: TConvertNotify);
VAR
   B, NewB: Byte; Value: LongInt;
begin
 if NOT Assigned(InStream)  then raise exception.Create('Input stream not assigned!');
 if NOT Assigned(OutStream) then raise exception.Create('Output stream not assigned!');

 if Assigned(Notify)
 then Notify(nkMax, InStream.Size);

 Value := 0;
 WHILE InStream.Position < InStream.Size DO
  begin
   InStream.Read(B, 1);
   case B of
    $0A: begin
          NewB := $0D;
          OutStream.Write(NewB, 1);
          NewB := $0A;
          OutStream.Write(NewB, 1);
         end;
   else
     OutStream.Write(B, 1);
  end;

  if Assigned(Notify) then
   begin
    Inc(Value);
    Notify(nkProgress, Value);
   end;
 end;

 if (Value = InStream.Size)
 AND Assigned(Notify)
 then Notify(nkProgress, 0);
end;



procedure WinToUnix(InStream: TStream; OutStream: TStream; Notify: TConvertNotify);
VAR
   B: Byte; Value: LongInt;
begin
 if NOT Assigned(InStream)  then raise exception.Create('Input stream not assigned!');
 if NOT Assigned(OutStream) then raise exception.Create('Output stream not assigned!');

 if Assigned(Notify)
 then Notify(nkMax, InStream.Size);

 Value := 0;
 while InStream.Position < InStream.Size DO
  begin
   InStream.Read(B, 1);
   if B <> $0A
   then OutStream.Write(B, 1);

   if Assigned(Notify) then
    begin
     Inc(Value);
     Notify(nkProgress, Value);
    end;
  end;

 OutStream.Seek(1, soFromEnd);
 OutStream.Read(B, 1);

 if B <> $0D then
  begin
   B := $0D;
   OutStream.Write(B, 1);
  end;

 if (Value = InStream.Size)
 AND Assigned(Notify)
 then Notify(nkProgress, 0);
end;



procedure MacToWin(InStream: TStream; OutStream: TStream);
VAR
   B, NewB: Byte;
begin
 if NOT Assigned(InStream)  then raise exception.Create('Input stream not assigned!');
 if NOT Assigned(OutStream) then raise exception.Create('Output stream not assigned!');

 WHILE InStream.Position < InStream.Size DO
  begin
   InStream.Read(B, 1);
   case B of
    $0D: begin
          NewB := $0D;
          OutStream.Write(NewB, 1);
          NewB := $0A;
          OutStream.Write(NewB, 1);
         end;
   else
     OutStream.Write(B, 1);
  end;
 end;
end;



procedure WinToUnix(CONST InputFile, OutputFile: String; Notify: TConvertNotify);
VAR
   InpStream: System.Classes.TBufferedFileStream;
   OutStream: System.Classes.TBufferedFileStream;
begin
 if NOT FileExists(InputFile)
 then RAISE Exception.Create('Input file does not exist!');

 InpStream:= TBufferedFileStream.Create(InputFile,  fmOpenRead, 1*mb);
 OutStream:= TBufferedFileStream.Create(OutputFile, fmOpenWrite OR fmCreate);
 TRY
   WinToUnix(InpStream, OutStream, Notify);
 FINALLY
   FreeAndNil(InpStream);
   FreeAndNil(OutStream);
 END;
end;



procedure UnixToWin(CONST InputFile, OutputFile: String; Notify: TConvertNotify);
VAR
   InpStream: System.Classes.TBufferedFileStream;
   OutStream: System.Classes.TBufferedFileStream;
begin
 if NOT FileExists(InputFile)
 then raise exception.Create('Input file does not exist!');

 InpStream:= TBufferedFileStream.Create(InputFile,  fmOpenRead);
 OutStream:= TBufferedFileStream.Create(OutputFile, fmOpenWrite OR fmCreate);
 TRY
  UnixToWin(InpStream, OutStream, Notify);
 FINALLY
  FreeAndNil(InpStream);
  FreeAndNil(OutStream);
 END;
end;



function MacToWin(CONST InputFile, OutputFile: string): Boolean;                                         { CR to CRLF. Not tested! }
VAR
   InpStream: System.Classes.TBufferedFileStream;
   OutStream: System.Classes.TBufferedFileStream;
begin
 if NOT FileExists(InputFile)
 then raise exception.Create('Input file does not exist!');

 InpStream:= TBufferedFileStream.Create(InputFile, fmOpenRead, 1*mb);
 TRY
  Result:= IsMacFile(InpStream);
  if Result then
   begin
    OutStream:= TBufferedFileStream.Create(OutputFile, fmOpenWrite OR fmCreate);
    TRY
     InpStream.Position:= 0;    { Needs reset because of IsMacFile }
     MacToWin(InpStream, OutStream);
    FINALLY
     FreeAndNil(OutStream);
    END;
   end;
 FINALLY
  FreeAndNil(InpStream);
 END;
end;


end.
