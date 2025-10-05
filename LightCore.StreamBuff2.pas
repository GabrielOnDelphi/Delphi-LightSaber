UNIT LightCore.StreamBuff2;

{-------------------------------------------------------------------------------------------------------------
   Extension for TCubicBuffStream
   https://gabrielmoraru.com/saving-an-object-to-disk-file/
--------------------------------------------------------------------------------------------------------------

   NEW HEADER FORMAT

     4 bytes (Card): LiSa (always the same)
     4 bytes (Card): Length of the magic signature
       bytes (Ansi): Magic signature
     2 bytes (Word): File version number.

     This new file header is more reliable because we check
       the magic number  - this is fixed for all TCubicBuffStream2 files
       the signature
       the file version
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, math, System.Classes, LightCore.StreamBuff;

TYPE
  TCubicBuffStream2 = class(TCubicBuffStream)
  private
  public
    { Padding }
    procedure ReadPaddingE (CONST Bytes: Integer);          //Raises an exception if the buffer does not contain the signature
    procedure WritePadding (CONST Bytes: Integer);
    procedure ReadPaddingDef;
    procedure WritePaddingDef;
    {}
    procedure WriteHeader         (CONST Signature: AnsiString; Version: Word);
    procedure ReadHeader          (CONST Signature: AnsiString; Version: Word);
    function  TryReadHeader       (CONST Signature: AnsiString; Version: Word): Boolean;
    function  TryReadHeaderVersion(CONST Signature: AnsiString; OUT Version: Word): Boolean;
  end;

IMPLEMENTATION

CONST
  ctMagicNumber: Cardinal= $4C695361; // The LiSa string for "Light Saber'


procedure TCubicBuffStream2.WriteHeader(CONST Signature: AnsiString; Version: Word);
begin
  WriteCardinal(ctMagicNumber);  // Write fixed magic no
  WriteStringA(Signature);       // Write signature
  WriteWord(Version);            // Write the file version number
end;


{ Returns True if it can read all 3 fields.
  Returns the version number.
  Does not check the version number. }
function TCubicBuffStream2.TryReadHeaderVersion(CONST Signature: AnsiString; OUT Version: Word): Boolean;
VAR
  MagicNumber: Cardinal;
  FileSignature: AnsiString;
begin
  // Read magic no
  MagicNumber := ReadCardinal;
  Result:= MagicNumber = ctMagicNumber;

  if Result then
    begin
      // Read signature
      FileSignature:= ReadStringA;
      Result:= FileSignature = Signature;

      // Read the version number
      if Result
      then Version:= ReadWord;
    end;
end;


{ Returns True if signature & version number matches.
  No exception will be raised except if the file smaller than what we want to read. }
function TCubicBuffStream2.TryReadHeader(CONST Signature: AnsiString; Version: Word): Boolean;
VAR lVersion: Word;
begin
 Assert(Signature > '', 'Signature is empty!');
 Assert(Version   > 0 , 'Version must be > 0');

 Result:= TryReadHeaderVersion(Signature, lVersion);
 Result:= Result AND (Version = lVersion);
end;


// This will raise an exception if anything goes wrong
procedure TCubicBuffStream2.ReadHeader(CONST Signature: AnsiString; Version: Word);
VAR
  MagicNumber: Cardinal;
  FileVersion: Word;
  FileSignature: AnsiString;
begin
  // Read magic no
  MagicNumber := ReadCardinal;
  if MagicNumber <> ctMagicNumber
  then Exception.Create('Magic number mismatch!');

  // Read signature
  FileSignature:= ReadStringA;
  if FileSignature <> Signature
  then RAISE Exception.Create('Header signature expected: '+ string(Signature)+ '. Found: '+ string(FileSignature));

  // Read the version number
  FileVersion:= ReadWord;
  if FileVersion <> Version
  then RAISE Exception.Create('Header version expected: '+ IntToStr(Version)+ '. Found: '+ IntToStr(FileVersion));
end;





{-------------------------------------------------------------------------------------------------------------
   PADDING
   It is important to leave some space at the end of your file (aka padding bytes).
   If you later (as your program evolves) need to save extra data into your file,
     you use the padding bytes. This way you don't need to change your file format.
-------------------------------------------------------------------------------------------------------------}

CONST
  PaddingStr: AnsiString= '<LightSaber - Buffer of 100 bytes. Pattern check -> Raises exception if the pattern not found!! ###>'; //This string is exactly 100 chars long

// Write a string as padding bytes.
procedure TCubicBuffStream2.WritePadding(CONST Bytes: Integer);
VAR
  b: TBytes;
  CheckPointSize: Integer;
  i: Integer;
begin
  SetLength(b, Bytes);
  CheckPointSize:= Length(PaddingStr);

  // Copy the string to the byte array (up to the available bytes or string length)
  for i := 0 to Min(Bytes, CheckPointSize) - 1
    do b[i] := Byte(PaddingStr[i + 1]);

  // Fill the rest of the buffer with zeros
  if Bytes > CheckPointSize
  then FillChar(b[CheckPointSize], Bytes - CheckPointSize, #0);

  // Write the buffer to the stream
  WriteBuffer(b[0], Bytes);
end;


// Raises an exception if the buffer does not contain the signature string (CheckpointStr)
procedure TCubicBuffStream2.ReadPaddingE(CONST Bytes: Integer);
VAR
  b: TBytes;
  CheckPointSize: Integer;
  i: Integer;
begin
  if Bytes > 0 then
  begin
    Assert(Bytes + Position <= Size, 'Read beyond stream!');

    SetLength(b, Bytes);
    ReadBuffer(b[0], Bytes);
    CheckPointSize := Length(PaddingStr);

    // Check if the beginning of the buffer matches the string
    VAR MinSize:= Min(CheckPointSize, Bytes);
    for i := 0 to MinSize- 1 do
        if b[i] <> Byte(PaddingStr[i + 1])
        then RAISE Exception.Create('Invalid checkpoint!!');
  end;
end;


CONST
   PaddingSize = 100; // 100 bytes. Enough for 18 Int64 variables. NEVER-EVER MODIFY THIS CONSTANT! All files saved with this constant will not work anymore.


procedure TCubicBuffStream2.ReadPaddingDef;   // Def = default (default number of padding bytes is 100
begin
  ReadPaddingE(PaddingSize);
end;

procedure TCubicBuffStream2.WritePaddingDef;
begin
  WritePadding(PaddingSize);
end;



end.
