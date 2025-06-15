UNIT LightCore.StreamMem2;

{-------------------------------------------------------------------------------------------------------------
   Extension for TCubicMemStream
   https://gabrielmoraru.com/saving-an-object-to-disk-file/
--------------------------------------------------------------------------------------------------------------

   NEW HEADER FORMAT

     4 bytes (Card): LiSa (always the same)
     4 bytes (Card): Length of the magic signature
       bytes (Ansi): Magic signature
     2 bytes (Word): File version number.

     This new file header is more reliable because we check
       the magic number  - this is fixed for all TCubicFileStream2 files
       the signature
       the file version
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Math, System.Classes,
  LightCore.StreamMem;

TYPE
  TMemStreamEx = class(TCubicMemStream)
  public
    { Padding }
    procedure ReadPaddingE (CONST Bytes: Integer);          //Raises an exception if the buffer does not contain the signature
    procedure WritePadding (CONST Bytes: Integer);
    procedure ReadPaddingDef;
    procedure WritePaddingDef;
    {}
    procedure WriteHeader  (CONST Signature: AnsiString; Version: Word);
    function  ReadHeader   (CONST Signature: AnsiString; Version: Word): Boolean;

    function  ReadHeaderVersion(CONST Signature: AnsiString; OUT Version: Word): Boolean;
  end;

IMPLEMENTATION

CONST
  ctMagicNumber: Cardinal= $4C695361; // The LiSa string for "Light Saber'


procedure TMemStreamEx.WriteHeader(CONST Signature: AnsiString; Version: Word);
begin
  WriteCardinal(ctMagicNumber);  // Write fixed magic no
  WriteStringA(Signature);       // Write signature
  WriteWord(Version);            // Write the file version number
end;


{ Returns True if it can read all 3 fields.
  Returns the version number.
  Does not check the version number. }
function TMemStreamEx.ReadHeaderVersion(CONST Signature: AnsiString; OUT Version: Word): Boolean;
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
  An exception may be raised if the file smaller than what we want to read }
function TMemStreamEx.ReadHeader(CONST Signature: AnsiString; Version: Word): Boolean;
VAR lVersion: Word;
begin
 Assert(Signature > '', 'Signature is empty!');
 Assert(Version   > 0 , 'Version must be > 0');

 Result:= ReadHeaderVersion(Signature, lVersion);
 Result:= Result AND (Version = lVersion);
end;







{--------------------------------------------------------------------------------------------------
   PADDING
   It is important to read/write some padding bytes.
   If you later (as your program evolves) need to save extra data into your file, you use the padding bytes. This way you don't need to change your file format.
--------------------------------------------------------------------------------------------------}

CONST
   CheckpointStr: AnsiString= '<LightSaber - Buffer of 100 bytes. Pattern check -> Raises exception if the pattern not found!! ###>'; //This string is 100 chars long

// Write a string as padding bytes.
procedure TMemStreamEx.WritePadding(CONST Bytes: Integer);
VAR
  b: TBytes;
  CheckPointSize: Integer;
  i: Integer;
begin
  SetLength(b, Bytes);
  CheckPointSize:= Length(CheckpointStr);
  // Copy the string to the byte array (up to the available bytes or string length)
  for i := 0 to Min(Bytes, CheckPointSize) - 1
    do b[i] := Byte(CheckpointStr[i + 1]);

  // Fill the rest of the buffer with zeros
  if Bytes > CheckPointSize
  then FillChar(b[CheckPointSize], Bytes - CheckPointSize, #0);

  WriteBuffer(b[0], Bytes);
end;


 //Raises an exception if the buffer does not contain the signature string (CheckpointStr)
procedure TMemStreamEx.ReadPaddingE(CONST Bytes: Integer);
VAR
  b: TBytes;
  CheckPointSize: Integer;
  i: Integer;
begin
  if Bytes > 0 then
  begin
    SetLength(b, Bytes);
    ReadBuffer(b[0], Bytes);
    CheckPointSize := Length(CheckpointStr);
    // Check if the beginning of the buffer matches the string
    if CheckPointSize <= Bytes then
      for i := 0 to CheckPointSize - 1 do
        if b[i] <> Byte(CheckpointStr[i + 1]) then
          RAISE Exception.Create('Invalid checkpoint!!');
  end;
end;


CONST
   PaddingSize = 100; // 100 bytes. Enough for 18 Int64 variables. NEVER-EVER MODIFY THIS CONSTANT! All files saved with this constant will not work anymore.


procedure TMemStreamEx.ReadPaddingDef;
begin
  ReadPaddingE(PaddingSize);
end;

procedure TMemStreamEx.WritePaddingDef;
begin
  WritePadding(PaddingSize);
end;




end.
