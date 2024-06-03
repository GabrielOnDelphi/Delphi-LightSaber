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
  System.SysUtils, System.Classes;


function  IsUTF8        (CONST FileName: string): Boolean;

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
function IsUTF8(const FileName: string): Boolean;
begin
  var InpStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result:= StreamHasBOM(InpStream);
  finally
    FreeAndNil(InpStream);
  end;
end;






{-------------------------------------------------------------------------------------------------------------
   CONVERT
-------------------------------------------------------------------------------------------------------------}

{ Works }
procedure ConvertToUTF(CONST FileName: string);
begin
 VAR s:= StringFromFile(FileName, nil);
 StringToFile(FileName, s, woOverwrite, TRUE);
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
  Result:= IsUTF8(FileName);
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
