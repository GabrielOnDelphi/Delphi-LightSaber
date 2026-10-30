UNIT GifProperties;

{-------------------------------------------------------------------------------------------------------------
  Detects animated GIFs by counting frames without decoding any pixel data.
  Only parses GIF headers and skips compressed image data — extremely fast.

  Slim version of the original FMX GIF reader/player.
  See: GifProperties (orig).pas for the full original version.

  Source:
    Original code: http://www.raysoftware.cn/?p=559
    (from: https://stackoverflow.com/questions/45285599/how-to-use-animated-gif-in-firemonkey)

  Updated by Gabriel Moraru 2026.10.30

  Tester:
    Project Testers\gr GIF frame counter\
  Unit tests:
    UnitTesting\Test.GifProperties.pas
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
  A file that ends too early (an interrupted download or copy) ends the scan: the frames found until then decide.
  Tested with 100MB GIF: ~4.1s }
function IsAnimatedGif(CONST FileName: string): Boolean;
VAR
  fs: TFileStream;
  Header: TGIFHeaderX;
  Descriptor: TGifImageDescriptor;
  Introducer: Byte;
  Skipped: Byte;
  ColorTableSize: Integer;
  FrameIndex: Integer;

  // Reads one byte into B. FALSE at the end of the file; B is then not set.
  function ReadByte(OUT B: Byte): Boolean;
  begin
    Result:= fs.Read(B, 1) = 1;
  end;

  // Skip a color table (each entry is 3 bytes: RGB)
  procedure SkipPalette(EntryCount: Integer);
  begin
    fs.Position:= fs.Position + (EntryCount * 3);
  end;

  // Skip a chain of sub-blocks: a length byte, that many data bytes, again, until a length of 0.
  // Extensions and the LZW-compressed image data are both stored this way.
  // FALSE when the file ends before the chain does.
  function SkipSubBlocks: Boolean;
  VAR BlockSize: Byte;
  begin
    REPEAT
      if NOT ReadByte(BlockSize) then EXIT(FALSE);
      fs.Position:= fs.Position + BlockSize;
    UNTIL BlockSize = 0;
    Result:= TRUE;
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

      // Count frames. The scan ends at the trailer, or at the end of a file that is cut off.
      // $3B = trailer, $21 = extension, $2C = image. Any other byte is skipped, one at a time.
      FrameIndex:= 0;
      WHILE ReadByte(Introducer) DO
        case Introducer of
          $3B: Break;  // GIF trailer - end of file

          $21: // Extension: a label byte, then sub-blocks
            begin
              // All extension types use sub-blocks terminated by a zero-length block.
              // $F9 (Graphics Control) is a single 4-byte sub-block + terminator.
              // $FE (Comment), $FF (Application), $01 (Plain Text) use variable-length sub-blocks.
              // The same skip loop handles all of them correctly.
              if NOT ReadByte(Skipped) OR NOT SkipSubBlocks then Break;
            end;

          $2C: // Image: descriptor, optional local color table, LZW minimum code size byte, sub-blocks
            begin
              // A frame counts as soon as its separator byte is read, so a GIF whose second frame is cut off still counts as animated.
              /// Found 2+ frames — it's animated. No need to read the entire GIF.
              Inc(FrameIndex);
              if FrameIndex >= 2 then EXIT(True);

              if fs.Read(Descriptor, SizeOf(Descriptor)) <> SizeOf(Descriptor) then Break;

              // Skip local color table
              if (Descriptor.Packedbit and $80) <> 0 then
              begin
                ColorTableSize:= 1 shl ((Descriptor.Packedbit and 7) + 1);
                SkipPalette(ColorTableSize);
              end;

              // Skip the LZW-compressed image data without decompressing
              if NOT ReadByte(Skipped) OR NOT SkipSubBlocks then Break;
            end;
        end;
    EXCEPT
      // Corrupt GIF - treat as not animated (Result stays False). A cut-off GIF raises nothing here: every read above is checked.
      // File-not-found exceptions propagate from TFileStream.Create above.
    END;
  FINALLY
    FreeAndNil(fs);
  END;
end;


end.
