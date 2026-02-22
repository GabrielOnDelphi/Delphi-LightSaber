UNIT GifProperties;

{-------------------------------------------------------------------------------------------------------------
  Detects animated GIFs by counting frames without decoding any pixel data.
  Only parses GIF headers and skips compressed image data — extremely fast.

  Slim version of the original FMX GIF reader/player.
  See: GifProperties (orig).pas for the full original version.

  Source:
    Original code: http://www.raysoftware.cn/?p=559
    (from: https://stackoverflow.com/questions/45285599/how-to-use-animated-gif-in-firemonkey)

  Updated by Gabriel Moraru 2026.02

  Tester:
    Project Testers\gr GIF frame counter\
-------------------------------------------------------------------------------------------------------------}

INTERFACE
USES
  System.Classes, System.SysUtils;

  function IsAnimatedGif(CONST FileName: string): Boolean;


IMPLEMENTATION

TYPE
  TGIFHeaderX = packed record
    Signature: array [0..2] of Byte;    // Header Signature (always "GIF")
    Version  : array [0..2] of Byte;    // GIF format version ("87a" or "89a")
    // Logical Screen Descriptor
    ScreenWidth : Word;                 // Width of Display Screen in Pixels
    ScreenHeight: Word;                 // Height of Display Screen in Pixels
    Packedbit: Byte;                    // Screen and Color Map Information
    BackgroundColor: Byte;              // Background Color Index
    AspectRatio: Byte;                  // Pixel Aspect Ratio
  end;

  TGifImageDescriptor = packed record
    Left: Word;                         // X position of image on the display
    Top: Word;                          // Y position of image on the display
    Width: Word;                        // Width of the image in pixels
    Height: Word;                       // Height of the image in pixels
    Packedbit: Byte;                    // Image and Color Table Data Information
  end;

CONST
  GifSignature   : array [0..2] of Byte = ($47, $49, $46); // GIF
  VerSignature87a: array [0..2] of Byte = ($38, $37, $61); // 87a
  VerSignature89a: array [0..2] of Byte = ($38, $39, $61); // 89a


{ Returns True if the GIF file contains more than one frame (animated).
  Parses only headers and skips compressed data — does not decode any pixels.
  Tested with 100MB GIF: ~4.1s }
function IsAnimatedGif(CONST FileName: string): Boolean;
VAR
  fs: TFileStream;
  Header: TGIFHeaderX;
  Descriptor: TGifImageDescriptor;
  Introducer: Byte;
  ColorTableSize: Integer;
  FrameIndex: Integer;

  // Skip a color table (each entry is 3 bytes: RGB)
  procedure SkipPalette(EntryCount: Integer);
  begin
    fs.Position:= fs.Position + (EntryCount * 3);
  end;

  // Read and skip extension blocks. Returns the block introducer byte ($2C=image, $3B=trailer, $21=extension).
  function ReadAndSkipBlock: Byte;
  VAR
    Labels, SkipByte: Byte;
  begin
    fs.Read(Result, 1);
    if Result = $21 then
    begin
      fs.Read(Labels, 1);
      // All extension types use sub-blocks terminated by a zero-length block.
      // $F9 (Graphics Control) is a single 4-byte sub-block + terminator.
      // $FE (Comment), $FF (Application), $01 (Plain Text) use variable-length sub-blocks.
      // The same skip loop handles all of them correctly.
      REPEAT
        fs.Read(SkipByte, 1);
        if SkipByte > 0
        then fs.Position:= fs.Position + SkipByte;
      UNTIL SkipByte = 0;
    end;
  end;

  // Skip LZW-compressed image data sub-blocks without decompressing
  procedure SkipImageData;
  VAR B: Byte;
  begin
    fs.Read(B, 1);  // LZW minimum code size byte
    REPEAT
      fs.Read(B, 1);
      if B > 0
      then fs.Position:= fs.Position + B;
    UNTIL B = 0;
  end;

begin
  Result:= False;
  fs:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  TRY
    TRY
      // Read and validate GIF header
      if fs.Size < SizeOf(Header) then EXIT;
      fs.Read(Header, SizeOf(Header));

      if NOT (CompareMem(@Header.Signature, @GifSignature, 3)
          AND (CompareMem(@Header.Version, @VerSignature87a, 3)
            OR CompareMem(@Header.Version, @VerSignature89a, 3)))
      then EXIT;

      // Skip global color table
      if (Header.Packedbit and $80) = $80 then
      begin
        ColorTableSize:= 1 shl ((Header.Packedbit and 7) + 1);
        SkipPalette(ColorTableSize);
      end;

      // Count frames
      FrameIndex:= 0;
      WHILE True DO
      begin
        REPEAT
          Introducer:= ReadAndSkipBlock;
        UNTIL (Introducer in [$2C, $3B]);

        if Introducer = $3B then Break;  // GIF trailer - end of file

        // Read image descriptor
        fs.Read(Descriptor, SizeOf(Descriptor));

        // Skip local color table
        if (Descriptor.Packedbit and $80) <> 0 then
        begin
          ColorTableSize:= 1 shl ((Descriptor.Packedbit and 7) + 1);
          SkipPalette(ColorTableSize);
        end;

        SkipImageData;

        /// Found 2+ frames — it's animated. No need to read the entire GIF.
        Inc(FrameIndex);
        if FrameIndex >= 2 then EXIT(True);
      end;
    EXCEPT
      // Corrupt or truncated GIF — treat as not animated (Result stays False).
      // File-not-found exceptions propagate from TFileStream.Create above.
    END;
  FINALLY
    FreeAndNil(fs);
  END;
end;


end.
