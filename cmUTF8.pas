unit cmUTF8;

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


function  FileHasBOM    (CONST FileName: string): Boolean;

function  ConvertToAnsi (CONST FileName: string): boolean;
procedure ConvertToUTF  (CONST FileName: string);

function  ForceAddBOM   (CONST FileName: string): Boolean;
function  ForceRemoveBOM(CONST FileName: string): Boolean;


IMPLEMENTATION
USES
   ccIO;

Type
  TBomBuffer= array[1..3] of Byte;



{-------------------------------------------------------------------------------------------------------------
   TEST IsUTF8
-------------------------------------------------------------------------------------------------------------}

function StreamHasBOM(InpStream: TFileStream): Boolean;
var Buffer: TBomBuffer;
begin
  if InpStream.Size < 3 then Exit(false); // File is empty

  InpStream.Read(Buffer, SizeOf(Buffer));
  InpStream.Seek(0, soFromBeginning);  // Rewind the stream

  Result:= (Buffer[1] = $EF)
       and (Buffer[2] = $BB)
       and (Buffer[3] = $BF);
end;


{ Removed the BOM from a file.
  Returns a TSearchResult that indicates if the file was found without BOM.
  Works. }
function FileHasBOM(const FileName: string): Boolean;
begin
  var InpStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result:= StreamHasBOM(InpStream);
  finally
    FreeAndNil(InpStream);
  end;
end;


function IsValidUTF8(const FileName: string): Boolean;
var
  InpStream: TFileStream;
  ByteBuffer: TBytes;
  I, BytesRead: Integer;
begin
  Result := True;
  InpStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(ByteBuffer, InpStream.Size);
    BytesRead := InpStream.Read(ByteBuffer[0], InpStream.Size);

    I := 0;
    while I < BytesRead do
    begin
      // Check the first byte of UTF-8 sequences
      if ByteBuffer[I] <= $7F then
        Inc(I) // ASCII (single byte)
      else if (ByteBuffer[I] and $E0) = $C0 then
      begin
        // 2-byte sequence: 110xxxxx 10xxxxxx
        if (I + 1 >= BytesRead) or ((ByteBuffer[I + 1] and $C0) <> $80) then
          Exit(False);
        Inc(I, 2);
      end
      else if (ByteBuffer[I] and $F0) = $E0 then
      begin
        // 3-byte sequence: 1110xxxx 10xxxxxx 10xxxxxx
        if (I + 2 >= BytesRead) or ((ByteBuffer[I + 1] and $C0) <> $80) or ((ByteBuffer[I + 2] and $C0) <> $80) then
          Exit(False);
        Inc(I, 3);
      end
      else if (ByteBuffer[I] and $F8) = $F0 then
      begin
        // 4-byte sequence: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
        if (I + 3 >= BytesRead) or ((ByteBuffer[I + 1] and $C0) <> $80) or ((ByteBuffer[I + 2] and $C0) <> $80) or ((ByteBuffer[I + 3] and $C0) <> $80) then
          Exit(False);
        Inc(I, 4);
      end
      else
        Exit(False); // Invalid byte sequence
    end;
  finally
    FreeAndNil(InpStream);
  end;
end;


function DetectFileEncoding(const FileName: string): TEncoding;
var
  InpStream: TFileStream;
  Buffer: TBomBuffer;
begin
  InpStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    // Check for BOM
    if InpStream.Size >= 3 then
    begin
      InpStream.Read(Buffer, SizeOf(Buffer));
      if (Buffer[1] = $EF) and (Buffer[2] = $BB) and (Buffer[3] = $BF) then
        Exit(TEncoding.UTF8); // File has UTF-8 BOM
    end;
  finally
    FreeAndNil(InpStream);
  end;

  // If no BOM, check if the content is valid UTF-8
  if IsValidUTF8(FileName) then
    Exit(TEncoding.UTF8); // File is valid UTF-8 without BOM

  // If not UTF-8, assume ANSI
  Result := TEncoding.ANSI;
end;





{-------------------------------------------------------------------------------------------------------------
   CONVERT
-------------------------------------------------------------------------------------------------------------}

procedure ConvertToUTF(CONST FileName: string);
var
  FileContent: string;
  DetectedEncoding: TEncoding;
begin
  // Detect the file's encoding (UTF-8 with BOM, UTF-8 without BOM, or ANSI)
  DetectedEncoding := DetectFileEncoding(FileName);

  // Read the file content using the detected encoding
  FileContent := TFile.ReadAllText(FileName, DetectedEncoding);

  // Write the file back in UTF-8 encoding with BOM
  //TFile.WriteAllText(FileName, FileContent, TEncoding.UTF8);

  StringToFile(FileName, FileContent, woOverwrite, TRUE);
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


end.
