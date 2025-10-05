UNIT GifProperties;

{-------------------------------------------------------------------------------------------------------------
  Returns the properties of a Gif without decoding the actual frames! Very fast!
  This was converted from FMX.

  Source:
       Original code: http://www.raysoftware.cn/?p=559
       Pointing from: https://stackoverflow.com/questions/45285599/how-to-use-animated-gif-in-firemonkey

    Updated by Gabriel Moraru 2023


  TESTER:
     Project Testers\gr GIF frame counter\
-------------------------------------------------------------------------------------------------------------}

INTERFACE  
USES 
  System.Classes, System.SysUtils, System.Types, System.UITypes,
  Vcl.Graphics, Vcl.Imaging.GIFImg;

TYPE
  TGifVer = (verUnknow, ver87a, ver89a);

  TInternalColor = packed record
    case Integer of
      0: (B, G, R, A: Byte;);
      1: (Color: TAlphaColor;);
  end;

{$POINTERMATH ON}
  PInternalColor = ^TInternalColor;
{$POINTERMATH OFF}

  TGifRGB = packed record
    R: Byte;
    G: Byte;
    B: Byte;
  end;

  TGIFHeaderX = packed record
    Signature: array [0 .. 2] of Byte;    // * Header Signature (always "GIF")
    Version  : array [0 .. 2] of Byte;    // * GIF format version("87a" or "89a")

    // Logical Screen Descriptor
    ScreenWidth : word;                   // * Width of Display Screen in Pixels
    ScreenHeight: word;                   // * Height of Display Screen in Pixels
    Packedbit: Byte;                      // * Screen and Color Map Information
    BackgroundColor: Byte;                // * Background Color Index
    AspectRatio: Byte;                    // * Pixel Aspect Ratio
  end;

  TGifImageDescriptor = packed record
    Left: word;                           // * X position of image on the display
    Top: word;                            // * Y position of image on the display
    Width: word;                          // * Width of the image in pixels
    Height: word;                         // * Height of the image in pixels
    Packedbit: Byte;                      // * Image and Color Table Data Information
  end;

  TGifGraphicsControlExtension = packed record
    BlockSize: Byte;                      // * Size of remaining fields (always 04h)
    Packedbit: Byte;                      // * Method of graphics disposal to use
    DelayTime: word;                      // * Hundredths of seconds to wait
    ColorIndex: Byte;                     // * Transparent Color Index
    Terminator: Byte;                     // * Block Terminator (always 0)
  end;

  // Palette
  TPalette = TArray<TInternalColor>;

  TGifProperties = class(TObject)
  protected
    FHeader: TGIFHeaderX;
    FPalette: TPalette;
    FScreenWidth: Integer;
    FScreenHeight: Integer;
    FBitsPerPixel: Byte;
    FBackgroundColorIndex: Byte;
    FResolution: Byte;
    FGifVer: TGifVer;
    function Read(Stream: TStream): Boolean; overload; virtual;
  public
    Interlace: Boolean;
    FrameIndex: Integer;
    function Read (FileName: string): Boolean; overload; virtual;
    function Check(FileName: string): Boolean; overload; virtual;
    function Check(Stream: TStream): Boolean;  overload; virtual;
  public
    property Header      : TGIFHeaderX read FHeader;
    property ScreenWidth : Integer read FScreenWidth;
    property ScreenHeight: Integer read FScreenHeight;
    property BitsPerPixel: Byte read FBitsPerPixel;
    property Resolution  : Byte read FResolution;
    property GifVer      : TGifVer read FGifVer;
  end;

  function IsAnimatedGif    (CONST FileName: string): Boolean;     // Fast
  function IsAnimatedGif_VCL(CONST FileName: string): Boolean;     // Slow


IMPLEMENTATION


{ Tested with 100mb GIF: ~24 sec }
function IsAnimatedGif_VCL(CONST FileName: string): Boolean;
VAR GIF: TGIFImage;
begin
  GIF:= TGIFImage.Create;
  TRY
    GIF.LoadFromFile(FileName);
    Result:= Gif.Images.Count > 1;
  FINALLY
    FreeAndNil(GIF);
  END;
end;


{ Returns the FrameGount of an animated gif without decoding the actual frames!
  I won't be able to get resolution for images that have wrong extension (image is GIF but the extension is JPG)

  Tested with 100mb GIF: 4.1s }
function IsAnimatedGif(CONST FileName: string): Boolean;
VAR GIF: TGifProperties;
begin
  GIF:= TGifProperties.Create;
  TRY
    GIF.Read(FileName);
    Result:= GIF.FrameIndex > 1; //GifFrameList.Count;
  FINALLY
    FreeAndNil(GIF);
  END;
end;











{ TGifReader }

CONST
  alphaTransparent = $00;
  GifSignature   : array [0 .. 2] of Byte = ($47, $49, $46); // GIF
  VerSignature87a: array [0 .. 2] of Byte = ($38, $37, $61); // 87a
  VerSignature89a: array [0 .. 2] of Byte = ($38, $39, $61); // 89a


function TGifProperties.Read(FileName: string): Boolean;
VAR
  fs: TFileStream;
begin
  Result := False;
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  TRY
    Result := Read(fs);
  EXCEPT
  END;
  fs.Free;
end;


function TGifProperties.Read(Stream: TStream): Boolean;
var
  LDescriptor: TGifImageDescriptor;
  LGraphicsCtrlExt: TGifGraphicsControlExtension;
  LIsTransparent: Boolean;
  LGraphCtrlExt: Boolean;
  LFrameWidth: Integer;
  LFrameHeight: Integer;
  LLocalPalette: TPalette;
  LScanLineBuf: TBytes;
  // Read palette
  procedure ReadPalette(Stream: TStream; Size: Integer; var APalette: TPalette);
  VAR
    RGBEntry: TGifRGB;
    I: Integer;
  begin
    SetLength(APalette, Size);
    For I := 0 To Size - 1 DO
      Stream.Read(RGBEntry, SizeOf(RGBEntry));
  end;

// Processing file headers, parsing file headers to object properties
  function ProcHeader: Boolean;
  begin
    With FHeader do
    begin
      if(CompareMem(@Signature, @GifSignature, 3)) AND
        (CompareMem(@Version, @VerSignature87a, 3)) OR
        (CompareMem(@Version, @VerSignature89a, 3)) then
      begin
        FScreenWidth  := FHeader.ScreenWidth;
        FScreenHeight := FHeader.ScreenHeight;

        FResolution := Packedbit and $70 shr 5 + 1;
        FBitsPerPixel := Packedbit and 7 + 1;
        FBackgroundColorIndex := BackgroundColor;
        if CompareMem(@Version, @VerSignature87a, 3)
        then FGifVer := ver87a
        else
          if CompareMem(@Version, @VerSignature89a, 3)
          then FGifVer := ver89a;
        Result := True;
      end
      else
        RAISE Exception.Create('Unknown GIF image format');
    end;
  end;

  // Processing a frame
  function ProcFrame: Boolean;
  VAR
    LineSize: Integer;
    LBackColorIndex: Integer;
  begin
    LBackColorIndex:= 0;
    With LDescriptor do
     begin
      LFrameWidth := Width;
      LFrameHeight:= Height;
      Interlace   := ((Packedbit and $40) = $40);  // interwoven logo
     end;

    if LGraphCtrlExt then
     begin
      LIsTransparent := (LGraphicsCtrlExt.Packedbit and $01) <> 0;
      If LIsTransparent
      then LBackColorIndex := LGraphicsCtrlExt.ColorIndex;
     end
    else
     begin
      LIsTransparent := FBackgroundColorIndex <> 0;
      LBackColorIndex := FBackgroundColorIndex;
     end;
    LineSize := LFrameWidth * (LFrameHeight + 1);
    SetLength(LScanLineBuf, LineSize);

    //  If you have transparency, change the Alpha value of the color in the palette of the transparent color to transparent
    if LIsTransparent then
     if LBackColorIndex < Length(LLocalPalette)   { !!!!!!!!!!!! This is added by me to "fix" a bug. I don't know if the bug is in the gif or in the code }
     then LLocalPalette[LBackColorIndex].A := alphaTransparent;
    Result := True;
  end;


  function ReadAndProcBlock(Stream: TStream): Byte;
  var
    Introducer, Labels, SkipByte: Byte;
  begin
    Stream.Read(Introducer, 1);
    if Introducer = $21 then
    begin
      Stream.Read(Labels, 1);
      Case Labels of
        $FE, $FF:
          // Comment Extension block or Application Extension block
          while True do
           begin
            Stream.Read(SkipByte, 1);
            if SkipByte = 0
            then Break;
            Stream.Position:= Stream.Position+ SkipByte; //was Stream.Seek(Int64( SkipByte), soFromCurrent);
           end;
        $F9: // Graphics Control Extension block
          begin
            Stream.Read(LGraphicsCtrlExt, SizeOf(LGraphicsCtrlExt));
            LGraphCtrlExt := True;
          end;
        $01: // Plain Text Extension block
          begin
            Stream.Read(SkipByte, 1);
            Stream.Position:= Stream.Position+ SkipByte;
            while True do
             begin
              Stream.Read(SkipByte, 1);
              if SkipByte = 0
              then Break;
              //was Stream.Seek(Int64( SkipByte), soFromCurrent);
              Stream.Position:= Stream.Position+ SkipByte;
             end;
          end;
      end;
    end;
    Result := Introducer;
  end;

  // Parsing a frame to ScanLine
  function ReadScanLine(Stream: TStream; AScanLine: PByte): Boolean;
  var
    OldPos, PackedSize: longint;
    I: Integer;
    SourcePtr: PByte;
    Prefix: array [0 .. 4095] of Cardinal;
    Suffix: array [0 .. 4095] of Byte;
    DataComp: TBytes;
    B, FInitialCodeSize: Byte;
    ClearCode: word;
  begin
    DataComp := NIL;
    TRY
      TRY
        // Read directory size
        Stream.Read(FInitialCodeSize, 1);

        // Find the end of the compressed table.
        OldPos := Stream.Position;
        PackedSize := 0;
        REPEAT
          Stream.Read(B, 1);
          if B > 0 then
           begin
            Inc(PackedSize, B);
            //was Stream.Seek(Int64(B), soFromCurrent);
            Stream.Position:= Stream.Position+ B;
           end;
        UNTIL B = 0;

        // Read the compressed table.
        SetLength(DataComp, 2 * PackedSize);
        // Read a compressed table
        SourcePtr := @DataComp[0];
        Stream.Position := OldPos;
        REPEAT
          Stream.Read(B, 1);
          if B > 0 then
           begin
            Stream.ReadBuffer(SourcePtr^, B);
            Inc(SourcePtr, B);
           end;
        UNTIL B = 0;

        ClearCode := 1 shl FInitialCodeSize;
        for I := 0 to ClearCode - 1 do
         begin
          Prefix[I] := 4096;
          Suffix[I] := I;
         end;
      FINALLY
        DataComp := nil;
      END;
    EXCEPT
    END;
    Result := True;
  end;

VAR
  Introducer: Byte;
  ColorTableSize: Integer;
  rendered : array of TBitmap;
begin
  Result := False;
  FrameIndex:= 0;
  if NOT Check(Stream) then EXIT;
  FGifVer  := verUnknow;
  FPalette := nil;
  LScanLineBuf := NIL;
  TRY
    Stream.Position := 0;
    // read a file header
    Stream.Read(FHeader, SizeOf(FHeader));

    // If there is a global color palette
    if (FHeader.Packedbit and $80) = $80 then
     begin
      ColorTableSize := FHeader.Packedbit and 7 + 1;
      ReadPalette(Stream, 1 shl ColorTableSize, FPalette);
     end;

    // processing head
    if NOT ProcHeader then EXIT;

    FrameIndex := 0;
    WHILE True DO
      begin
       LLocalPalette := NIL;
       REPEAT
         Introducer := ReadAndProcBlock(Stream);
       UNTIL (Introducer in [$2C, $3B]);

       if Introducer = $3B then Break;

      // descriptors
       Stream.Read(LDescriptor, SizeOf(LDescriptor));

      // If you have a local palette, you use the local palette, otherwise copy the global palette
       if (LDescriptor.Packedbit and $80) <> 0
       then
        begin
         ColorTableSize := LDescriptor.Packedbit and 7 + 1;
         ReadPalette(Stream, 1 shl ColorTableSize, LLocalPalette);
        end
       else
         LLocalPalette := Copy(FPalette, 0, Length(FPalette));

       if not ProcFrame then EXIT;
       // retrieve ScanLine
       if not ReadScanLine(Stream, @LScanLineBuf[0]) then EXIT;

       /// I got my answer. Now I can exit without reading the entire GIF
       Inc(FrameIndex);
       if FrameIndex >= 2 then EXIT;
      end;

    Result := True;
  FINALLY
    LLocalPalette := nil;
    LScanLineBuf  := nil;
    rendered      := nil;
  end;
end;


function TGifProperties.Check(Stream: TStream): Boolean;
var OldPos: Int64;
begin
  TRY
    OldPos := Stream.Position;
    Stream.Read(FHeader, SizeOf(FHeader));
    Result := (CompareMem(@FHeader.Signature, @GifSignature, 3)) AND (CompareMem(@FHeader.Version, @VerSignature87a, 3)) OR (CompareMem(@FHeader.Version, @VerSignature89a, 3));
    Stream.Position := OldPos;
  EXCEPT
    Result := False;
  end;
end;


function TGifProperties.Check(FileName: string): Boolean;
var fs: TFileStream;
begin
  Result:= False;
  fs:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  TRY 
    Result:= Check(fs);
  EXCEPT 
  end;
  fs.Free;
end;


end.
