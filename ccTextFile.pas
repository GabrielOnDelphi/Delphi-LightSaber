﻿unit ccTextFile;

{=============================================================================================================
   Gabriel Moraru
   2022
--------------------------------------------------------------------------------------------------------------
   Convert between ANSI and UTF8

   Wikipedia:
   The UTF-8 representation of the BOM is the (hexadecimal) byte sequence EF BB BF .
   The Unicode Standard permits the BOM in UTF-8, but does not require or recommend its use.

   Also see: cmPlatformFile.pas

   Tester app:
      c:\My projects\Project support\DUT2\DUT2.dpr
=============================================================================================================}

interface

uses
  Winapi.Windows, System.IOUtils, System.SysUtils, System.Classes;



{--------------------------------------------------------------------------------------------------
   READ/WRITE
--------------------------------------------------------------------------------------------------}
 TYPE
   TWriteOperation= (woAppend, woOverwrite);
   TWritePreamble = (wpOff, wpOn, wpAuto);

 procedure StringToFile         (CONST FileName: string; CONST aString: String; CONST WriteOp: TWriteOperation= woOverwrite; WritePreamble: TWritePreamble= wpAuto);
 function  StringFromFile       (CONST FileName: string; Enc: TEncoding= Nil): string;

 function  StringFromFileExists (CONST FileName: string): String;                                  { Read file IF it exists. Otherwise, return '' }

 function  StringFromFileTSL    (CONST FileName: string; Enc: TEncoding= NIL): TStringList;        { Returns a TSL instead of a string. The caller has to free the result }

 // ANSI
 function  StringFromFileA      (CONST FileName: string): AnsiString;                              { Read a WHOLE file and return its content as String. Also see this: http://www.fredshack.com/docs/delphi.html }
 procedure StringToFileA        (CONST FileName: string; CONST aString: AnsiString; CONST WriteOp: TWriteOperation);  overload;
 procedure StringToFileA        (CONST FileName: string; CONST aString: String;     CONST WriteOp: TWriteOperation);  overload;

 function  CountLines           (CONST Filename: string; CONST BufferSize: Cardinal= 128000): Int64;                     { Opens a LARGE text file and counts how many lines it has. It does this by loading a small portion of the file in a RAM buffer }


{--------------------------------------------------------------------------------------------------
   UTF8 helpers
--------------------------------------------------------------------------------------------------}
 function  FileHasBOM     (CONST FileName: string): Boolean;
 function  DetectEncoding (CONST FileName: string): TEncoding;
 function  IsValidUTF8    (CONST FileName: string): Boolean;
 function  ContainsUnicodeChars(CONST S: string): Boolean;

 function  ConvertToAnsi  (CONST FileName: string): Boolean;
 procedure ConvertToUTF   (CONST FileName: string);

 function  ForceAddBOM    (CONST FileName: string): Boolean;
 function  ForceRemoveBOM (CONST FileName: string): Boolean;


IMPLEMENTATION
USES
   ccCore, ccIO;



{--------------------------------------------------------------------------------------------------
   WRITE TEXT
   Also see: ccStreamFile.pas.StringFromFileStart
--------------------------------------------------------------------------------------------------}

{ Write Unicode strings to a UTF8 file.
  FileName must be a full path. If the path does not exist it is created.
  It can also write a preamble.
  Based on: http://stackoverflow.com/questions/35710087/how-to-save-classic-delphi-string-to-disk-and-read-them-back/36106740#36106740  }
procedure StringToFile(CONST FileName: string; CONST aString: String; CONST WriteOp: TWriteOperation= woOverwrite; WritePreamble: TWritePreamble= wpAuto);
VAR
   Stream: TFileStream;
   Preamble: TBytes;
   sUTF8: RawByteString;
   aMode: Integer;
   NeedsPreamble: Boolean;
begin
 ForceDirectories(ExtractFilePath(FileName));

 if (WriteOp= woAppend)
 AND FileExists(FileName)
 then aMode := fmOpenReadWrite
 else aMode := fmCreate;

 TRY

   Stream := TFileStream.Create(filename, aMode, fmShareDenyWrite);   { Allow others to read while we write }
   TRY
    if (WriteOp= woAppend)
    then Stream.Position:= Stream.Size  // Go to the end, to append
    else
      begin
        // We don't write BOM if we append text
        if WritePreamble = wpOn
        then NeedsPreamble:= TRUE
        else NeedsPreamble:= (WritePreamble = wpAuto) AND ContainsUnicodeChars(aString);

        if NeedsPreamble then
         begin
          Preamble := TEncoding.UTF8.GetPreamble;
          Stream.WriteBuffer(Preamble[0] {PAnsiChar(Preamble)^}, Length(Preamble));
         end;
      end;

    sUTF8 := UTF8Encode(aString);               { UTF16 to UTF8 encoding conversion. It will convert UnicodeString to WideString }
    Stream.WriteBuffer(PAnsiChar(sUTF8)^, Length(sUTF8));
   FINALLY
     FreeAndNil(Stream);
   END;

 EXCEPT
   on E: Exception
    do RAISE Exception.CreateFmt('Error writing to file %s: %s', [FileName, E.Message]);
 end;

end;



procedure StringToFileA (CONST FileName: string; CONST aString: String; CONST WriteOp: TWriteOperation);
begin
 StringToFileA(FileName, AnsiString(aString), WriteOp);
end;





{--------------------------------------------------------------------------------------------------
   READ TEXT
   Also see: ccStreamFile.pas.StringFromFileStart
--------------------------------------------------------------------------------------------------}

{ Tries to autodetermine the file type (ANSI, UTF8, UTF16, etc). Works with UNC paths.
  If the file does not exist, it raises an error unless, IgnoreExists is True.

  If it cannot detect the correct encoding automatically, we can force it to what we want by setting the second paramater.
      Example: System.SysUtils.TEncoding.UTF8
      However this is buggy! It will raise an exception if the file is ANSI but it contains high characters such as ½ (#189)
      See: https://stackoverflow.com/questions/35708827/what-could-cause-no-mapping-for-the-unicode-character-exists-in-the-target-mult }
function StringFromFile(CONST FileName: string; Enc: TEncoding= NIL): String;
begin
  if Enc = NIL
  then Result:= System.IOUtils.TFile.ReadAllText(FileName)
  else Result:= System.IOUtils.TFile.ReadAllText(FileName, Enc);
end;


{ Read file IF it exists. Otherwise, return '' }
function StringFromFileExists(CONST FileName: string): String;          // Works with UNC paths
begin
 if FileExists(FileName)
 then Result:= StringFromFile(FileName)
 else Result:= '';
end;


{ Read a WHOLE file and return its content as AnsiString.
  The function will not try to autodetermine file's type.
  It will simply read the file as ANSI.

  Also see this: http://www.fredshack.com/docs/delphi.html }
function StringFromFileA(CONST FileName: string): AnsiString;
VAR Stream: TFileStream;
begin
 Result:= '';


 Stream:= TFileStream.Create(FileName, fmOpenRead OR fmShareDenyNone);
 TRY
   if Stream.Size = 0 then Exit;  // Return empty string for empty files

   if Stream.Size>= High(Longint)
   then RAISE Exception.Create('Only files below 2GB are supported.'+ CRLFw+ FileName);

   SetString(Result, NIL, Stream.Size);
   Stream.ReadBuffer(Pointer(Result)^, Stream.Size);
 FINALLY
   FreeAndNil(Stream);
 END;
end;


procedure StringToFileA (CONST FileName: string; CONST aString: AnsiString; CONST WriteOp: TWriteOperation);
VAR
   Stream: TFileStream;
   aMode: Integer;
begin
 ForceDirectories(ExtractFilePath(FileName));

 if (WriteOp= woAppend) AND FileExists(FileName)
 then aMode := fmOpenReadWrite
 else aMode := fmCreate;

 Stream := TFileStream.Create(filename, aMode, fmShareDenyWrite);   { Allow read during our writes }
 TRY
  if aMode = fmOpenReadWrite
  then Stream.Position:= Stream.Size; { Go to the end }

  Stream.WriteBuffer( PAnsiChar(aString)^, Length(aString) );
 FINALLY
   FreeAndNil(Stream);
 END;
end;


{ Example for the 'Enc' parameter: TEncoding.UTF8 }
function StringFromFileTSL(CONST FileName: string; Enc: TEncoding= NIL): TStringList;    // Works with UNC paths
begin
 Result:= TStringList.Create;
 Result.Text:= StringFromFile(FileName, Enc);
end;




{-----------------------------------------------------------------------------------------------------------------------
   LINES
-----------------------------------------------------------------------------------------------------------------------}

{ Opens a LARGE text file and counts how many lines it has.
  It does this by loading a small portion of the file in a RAM buffer.

  We only rely on LF which should be present on all OSes except old Mac:
      Windows                ->  CRLF
      Unix/Linux             ->  LF
      Mac OS (before OS X)   ->  CR
      Modern macOS           ->  LF

  Speed test
    File size: 150MB file
    Tester here: LightSaber\CubicCommonControls-Testers\CountLines tester\Tester.exe

    TFileStream (this): 15ms
    TCubicBuffStream  : 78ms
    Delphi's ReadLn   : 8518ms

  Buffer speed:
     1KB   = 31ms
     32KB  = 14.7ms
     64KB  = 14.5ms   <-- BEST
     256KB = 15ms
     512KB = 15ms
     2MB   = 16ms
   Same speed for SSD and HDD - probably because of OS buffering!

See also: http://www.delphipages.com/forum/showthread.php?t=201629 }

function CountLines(CONST Filename: string; CONST BufferSize: Cardinal= 128000): Int64;            { Source: http://www.delphipages.com/forum/showthread.php?t=201629 }
{TODO: use TBufferedFile }
VAR
   FS: TFileStream;
   bytes: array of Byte;
   i, iRead: Integer;
begin
 Result := 0;
 Assert(FileExists(FileName));
 SetLength(Bytes, BufferSize);
 FS := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
 TRY
  iRead := FS.Read(bytes[0], BufferSize);
  while iRead > 0 do
   begin
    for i := 0 to iRead - 1 do
     if (bytes[i] = 10)
     then Inc(Result);

    iRead := FS.Read(bytes[0], BufferSize);
   end;

  if FS.Size > 0 then
   begin
    { see if last line ends with a linefeed character }
    if FS.Size >= BufferSize
    then FS.Position :=  FS.Size-BufferSize         // Update to XE7. It was: FS.Seek(-BufferSize, soFromEnd)
    else FS.Position := 0;

    iRead := FS.Read(bytes[0], BufferSize);
    i := iRead - 1;

    { skip bytes < 9 or equal to Ctrl+Z (26) }
    WHILE (i > -1) AND ((bytes[i] < 9) OR (bytes[i] = 26))
     DO Dec(i);

    if (i > -1) AND (bytes[i] <> 10)
    then Inc(Result);
   end;

 FINALLY
  FreeAndNil(FS);
 END;
end;





{-------------------------------------------------------------------------------------------------------------
   TEST IsUTF8
-------------------------------------------------------------------------------------------------------------}
TYPE
  TBomBuffer= array[1..3] of Byte;


{ Returns true if this file contains UTF8 sequences.
  Useful for files that don't have BOM.

  Theory:
    Valid UTF8 has a specific binary format.
    If it's a single byte UTF8 character, then it is always of form '0xxxxxxx', where 'x' is any binary digit.
    If it's a two byte UTF8 character, then it's always of form '110xxxxx10xxxxxx'."

  How does it work
    For each chunk/buffer:
      It examines each byte to determine the length of the UTF-8 sequence (1 to 4 bytes).
      It checks if the first byte of each sequence is valid according to UTF-8 rules.
      For multi-byte sequences, it verifies that each continuation byte is valid (starts with '10' in binary).
      If any invalid byte or sequence is found, it exits with False.
    At the end it checks if the last chunk (which may be partial) ends with an incomplete multi-byte sequence.
  }
function IsValidUTF8(const FileName: string): Boolean;
const
  BufferSize = 8192; // Read in 8KB chunks
var
  Stream: TFileStream;
  Buffer: array[0..BufferSize-1] of Byte;
  I, j, BytesRead: Integer;
  SequenceLen: Integer;
begin
  Result := True; // Assume valid UTF-8 unless proven otherwise

  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    repeat
      BytesRead := Stream.Read(Buffer[0], BufferSize);
      if BytesRead = 0 then Break;

      I := 0;
      while I < BytesRead do
      begin
        if Buffer[I] <= 127
        then SequenceLen := 1               // ASCII (single byte)
        else
          if (Buffer[I] and $E0) = $C0      // 2-byte sequence: 110xxxxx 10xxxxxx
          then SequenceLen := 2
          else
            if (Buffer[I] and $F0) = $E0    // 3-byte sequence: 1110xxxx 10xxxxxx 10xxxxxx
            then SequenceLen := 3
            else
              if (Buffer[I] and $F8) = $F0  // 4-byte sequence: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
              then SequenceLen := 4
              else Exit(False);             // Invalid first byte

        // Check continuation bytes
        for j := 1 to SequenceLen - 1 do
          if (I + j >= BytesRead)
          OR ((Buffer[I + j] and $C0) <> $80)
          then Exit(False);                 // Invalid continuation byte

        Inc(I, SequenceLen);
      end;
    until BytesRead < BufferSize;

    // Check if the file ended with an incomplete sequence
    if (BytesRead > 0)
    AND ((Buffer[BytesRead -1] AND $C0) = $80)
    then Result := False;
  finally
    FreeAndNil(Stream);
  end;
end;




function StreamHasBOM(InpStream: TFileStream): Boolean;
var Buffer: TBomBuffer;
begin
  if InpStream.Size < 3 then Exit(FALSE); // File too small for BOM

  InpStream.Read(Buffer, SizeOf(Buffer));
  //InpStream.Seek(0, soFromBeginning);  // Rewind the stream

  Result:= (Buffer[1] = $EF)
       AND (Buffer[2] = $BB)
       AND (Buffer[3] = $BF);
end;


function FileHasBOM(const FileName: string): Boolean;
begin
  var InpStream:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result:= StreamHasBOM(InpStream);
  finally
    FreeAndNil(InpStream);
  end;
end;


function DetectEncoding(const FileName: string): TEncoding;
begin
  if FileHasBOM(FileName)
  then Result:= TEncoding.UTF8
  else
    if IsValidUTF8(FileName)       // If no BOM, check if the content is valid UTF-8
    then Result:= TEncoding.UTF8   // file is valid UTF-8 without BOM
    else Result := TEncoding.ANSI; // assume ANSI
end;





{-------------------------------------------------------------------------------------------------------------
   CONVERT
-------------------------------------------------------------------------------------------------------------}

procedure ConvertToUTF(CONST FileName: string);
var
  FileContent: string;
  DetectedEncoding: TEncoding;
begin
  DetectedEncoding := DetectEncoding(FileName);       // Detect the file's encoding (UTF-8 with BOM, UTF-8 without BOM, or ANSI)
  FileContent      := TFile.ReadAllText(FileName, DetectedEncoding);       // Read the file content using the detected encoding
  StringToFile(FileName, FileContent, woOverwrite, wpOn);     // Write the file back in UTF-8 encoding with BOM
end;



{ Not working }
{ Converts UTF8 to ANSI }
function ConvertToAnsi(CONST FileName: string): boolean;
var
  OutputFileName: string;
  InputFile, OutputFile: TFileStream;
  UTF8String: TStringStream;
  AnsiStr: AnsiString;
begin
  Result:= FileHasBOM(FileName);
  if result then
    begin
      InputFile := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      UTF8String := TStringStream.Create('', TEncoding.UTF8);
      try
        UTF8String.CopyFrom(InputFile, InputFile.Size);

        AnsiStr := AnsiString(UTF8String.DataString);

        OutputFileName := ChangeFileExt(FileName, '._temp_ansi_');
        OutputFile := TFileStream.Create(OutputFileName, fmCreate);
        try
          OutputFile.WriteBuffer(AnsiStr[1], Length(AnsiStr));
        finally
          OutputFile.Free;
        end;
      finally
        InputFile.Free;
        UTF8String.Free;
      end;
    end;

  // Replace the original file with the one containing BOM
  DeleteFile(FileName);
  RenameFile(OutputFileName, FileName);
end;









{-------------------------------------------------------------------------------------------------------------
   BRUTE FORCE
-------------------------------------------------------------------------------------------------------------}

{ Adds the BOM onyl if the file is not empty and does not have BOM already.
  Warning. Does this with brute force (no text conversion).
  Returns a TSearchResult that indicates if the file was found without BOM.
  Works. }
function ForceAddBOM(const FileName: string): Boolean;
var
  OutStream, InpStream: TFileStream;
  OutFileName: string;
  Buffer: TBomBuffer;
begin
  InpStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    if StreamHasBOM(InpStream) then Exit(False);

    OutFileName := ChangeFileExt(FileName, '._WithBOM_');
    OutStream := TFileStream.Create(OutFileName, fmCreate);
    try
      Buffer[1]:= $EF;
      Buffer[2]:= $BB;
      Buffer[3]:= $BF;
      OutStream.WriteBuffer(Buffer, SizeOf(Buffer));
      OutStream.CopyFrom(InpStream, InpStream.Size);
    finally
      FreeAndNil(OutStream);
    end;

    Result:= True;  // We indicate that we added BOM
  finally
    FreeAndNil(InpStream);
  end;

  // Replace the original file with the one containing BOM
  DeleteFile(FileName);
  RenameFile(OutFileName, FileName);
end;


{ Remove BOM with brute force (no text conversion) }
function ForceRemoveBOM(CONST FileName: string): Boolean;
var
  OutStream, InpStream: TFileStream;
begin
  var OutFileName := ChangeFileExt(FileName, '._WithoutBOM_');

  InpStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    if not StreamHasBOM(InpStream) then Exit(False);

    // Move cursor past BOM
    InpStream.Seek(3, soFromBeginning);

    OutStream:= TFileStream.Create(OutFileName, fmCreate);
    try
      OutStream.CopyFrom(InpStream, InpStream.Size - 3);
    finally
      FreeAndNil(OutStream);
    end;

    Result:= True;  // We indicate that we removed BOM
  finally
    FreeAndNil(InpStream);
  end;

  // Replace the original file with the one containing BOM
  DeleteFile(FileName);
  RenameFile(OutFileName, FileName);
end;


function ContainsUnicodeChars(const S: string): Boolean;
begin
  for VAR I := 1 to Length(S) do
    if Ord(S[I]) > 127 then Exit(True);
  Result := False;
end;



end.