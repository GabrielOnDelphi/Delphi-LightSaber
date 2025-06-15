UNIT LightVcl.Graph.Loader.WBC;

{=============================================================================================================
   Gabriel Moraru
   2023.08.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   Decoder for WBC file format

   How to use it 1:
     LoadFromFile('test.wbc')
     ExtractJpegs(OutputFolder);

   How to use it 2:
    for i:= 0 to Count-1 DO
      JpgStream:= GetJpgStream(i);
      JpgStream.SaveToFile(OutputFolder + HeaderVec[i].ImageTitle + '.jpg');

--------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Classes, System.AnsiStrings, vcl.imaging.Jpeg;

CONST
  WBC    = '*.WBC';
  WB1Ftl = 'WebShots|'+ WBC;

TYPE
  String256= array[1..256] of Byte;

TYPE
  RHeader= packed record                                                                           { ms-help://embarcadero.rs_xe/rad/Simple_Types.htm }
   Tag        : Longint;
   HeaderSize : Longint;                                                                           { 8606 HeaderSize (the first unit offset must be at least 8606) }
   Unknown    : Longint;
   FileTitle  : String256;
  end;

  RUnitIndex= packed record
   Offset    : Longint;
   Size      : Longint;
   Unknown1  : Longint;
   TimeStamp : Longint;
   Unknown2  : array[1..24] of Byte;
  end;

  RUnitHeader= packed record
   UnitTag           : Longint;
   HeaderSize        : Longint;                                                                    { 2088 }
   Size              : Longint;
   OrigFilename      : String256;
   ImageTitle        : array[1..128] of Byte;
   ImageDesc         : String256;
   ImageCredits      : String256;
   FileExt           : array[1..8] of Byte;
   PictureFileSize   : Longint;
   ThumbFileSize     : Longint;
   unknown           : array[1..140] of Byte;
   DailyDate         : Longint;                                                                    {(Date when the picture was published as the Daily picture  on www.webshots.com) in the form: yyymmdd }
   Timestamp         : Longint;                                                                    {Picture Addition Time }
   Fit2Screen        : Longint;                                                                    // 0=default, 1=never, 2=always
   PictureID         : array[1..128] of Byte;                                                      // the first letter isthe ‘source’ of the picture, see the note)
   AlbumTitle        : array[1..128] of Byte;
   CategTitle        : array[1..788] of Byte;
  end;

  UUnitHeader= record
   UnitTag           : Longint;
   HeaderSize        : Longint;                                                                    { 2088 }
   Size              : Longint;
   OrigFilename      : String;
   ImageTitle        : String;
   ImageDesc         : String;
   ImageCredits      : String;
   FileExt           : String;
   PictureFileSize   : Longint;
   ThumbFileSize     : Longint;
   DailyDate         : Longint;                                                                    {(Date when the picture was published as the Daily picture  on www.webshots.com) in the form: yyymmdd }
   Timestamp         : Longint;                                                                    {Picture Addition Time }
   Fit2Screen        : Longint;                                                                    // 0=default, 1=never, 2=always
   PictureID         : String;                                                                     // the first letter isthe ‘source’ of the picture, see the note)
   AlbumTitle        : String;
   CategTitle        : String;
   Offset            : Longint;                                                                    //the offset in the file where this Header starts
  end;


TYPE
 TWbCObj = class(TObject)
  private
    FFileName: string;
    Header: RHeader;
    WBCStream: TFileStream;
    HeaderVec : array of UUnitHeader;
    function GetJpgStream(CONST Index: Integer): TMemoryStream;
  protected
  public
    Count: Integer;                                                                                { Number of NON-EMPTY units (jpegs) in the file }
    destructor  Destroy; override;
    procedure Clear;
    function  LoadFromFile(CONST FullFileName: string): Boolean;
    procedure ExtractJpegs(CONST OutputFolder: string);
    function  GetJpeg(const Index: Integer): TJpegImage;                                           { Get access to the specified JPEG }
    property  FileName: string read FFileName;
 end;


IMPLEMENTATION




{--------------------------------------------------------------------------------------------------
   CREATE/DESTROY
--------------------------------------------------------------------------------------------------}

procedure TWbcObj.Clear;
begin
 Count:= 0;
 SetLength(HeaderVec, Count);
 FFileName:= '';
 if WBCStream <> NIL
 then FreeAndNil(WBCStream);
end;


destructor TWbcObj.Destroy;
begin
 if WBCStream <> NIL
 then FreeAndNil(WBCStream);
 inherited;
end;




{--------------------------------------------------------------------------------------------------
   LOAD
--------------------------------------------------------------------------------------------------}

function TWbcObj.LoadFromFile(CONST FullFileName: string): Boolean;
CONST
   MaxCount                 = 50000;                                                              { Supposition: a WBC file will never contain such a high number of JPEGs }
   HeaderSectionStartOffset = 2196;
   StandardHeaderSize       = 2088;
   FileHeaderMaxSize        = 8606;
   ctTag                    = -1778772309;                                                        { AB 16 FA 95 }
   ctUnitTag                = -260977182;                                                         { E2 CD 71 F0 }
VAR
   I, initialCount: Integer;
   Index: RUnitIndex;
   UnitHeader: RUnitHeader;
   UH: UUnitHeader;
   ReturnPos: Int64;
   Res: Boolean;
begin
 Clear;
 Assert(FileExists(FullFileName), 'File not found: '+ FullFileName);
 FFileName:= FullFileName;

 TRY
  WBCStream:= TFileStream.Create(FullFileName, fmShareDenyWrite);
 except
  //todo 1: trap only specific exceptions
  EXIT(FALSE);
 END;

 { Read magic number }
 WBCStream.Position := 0;
 WBCStream.ReadBuffer(Header, SizeOf(Header));

 { Magic number is ok? }
 Result:= (Header.Tag= ctTag) AND (Header.HeaderSize>= FileHeaderMaxSize);
 if NOT Result then EXIT(FALSE);

 WBCStream.Position := HeaderSectionStartOffset;
 WBCStream.ReadBuffer(initialCount, 4);

 Res:= (initialCount> 0) AND (initialCount< MaxCount);
 if NOT Res then EXIT(FALSE);

 SetLength(HeaderVec, initialCount);
 ReturnPos := HeaderSectionStartOffset + 4;
 for i:= 1 to initialCount DO
  begin
   WBCStream.Position := ReturnPos;
   WBCStream.ReadBuffer(Index, SizeOf(Index));
   ReturnPos:= WBCStream.Position;

   WBCStream.Position:= Index.Offset;
   WBCStream.ReadBuffer(UnitHeader, SizeOf(UnitHeader));

   Res:= (UnitHeader.UnitTag = ctUnitTag) AND
         (UnitHeader.HeaderSize = StandardHeaderSize);
   if NOT Res then Continue;

   UH.UnitTag           := UnitHeader.UnitTag;
   UH.HeaderSize        := UnitHeader.HeaderSize;
   UH.Size              := UnitHeader.Size;
   UH.OrigFilename      := String (PAnsiChar(@UnitHeader.OrigFilename[1]));
   UH.ImageTitle        := String (PAnsiChar(@UnitHeader.ImageTitle[1]));
   UH.ImageDesc         := String (PAnsiChar(@UnitHeader.ImageDesc[1]));
   UH.ImageCredits      := String (PAnsiChar(@UnitHeader.ImageCredits[1]));
   UH.FileExt           := String (PAnsiChar(@UnitHeader.FileExt[1]));
   UH.PictureID         := String (PAnsiChar(@UnitHeader.PictureID[1]));
   UH.AlbumTitle        := String (PAnsiChar(@UnitHeader.AlbumTitle[1]));
   UH.CategTitle        := String (PAnsiChar(@UnitHeader.CategTitle[1]));
   UH.PictureFileSize   := UnitHeader.PictureFileSize;
   UH.ThumbFileSize     := UnitHeader.ThumbFileSize;
   UH.DailyDate         := UnitHeader.DailyDate;
   UH.Timestamp         := UnitHeader.Timestamp;
   UH.Fit2Screen        := UnitHeader.Fit2Screen;
   UH.Offset            := Index.Offset;

   HeaderVec[Count] := UH;
   inc(Count);
  end;

 Result:= Count> 0;
end;






{--------------------------------------------------------------------------------------------------
   ACCESS
--------------------------------------------------------------------------------------------------}

function TWbcObj.GetJpeg(CONST Index: Integer): TJpegImage;
VAR Stream: TMemoryStream;
begin
 Result:= TJpegImage.Create;
 Stream:= GetJpgStream(Index);
 TRY
  Stream.Position:= 0;
  Result.LoadFromStream(stream);
 FINALLY
  FreeAndNil(Stream);
 END;
end;


function TWbcObj.GetJpgStream(CONST Index: Integer): TMemoryStream;
CONST
   WB0Hdr: AnsiString = 'WWBB0000';
   WB1Hdr: AnsiString = 'WWBB1111';
VAR
   A100, B100, D100: array[1..100] of Byte;
   key, i: byte;
   tempBuff : array[0..7] of ansiChar;
   WBBHeader: AnsiString;
begin
 Assert(WBCStream<> NIL);
 Assert(Index >= 0);
 Assert(Index < Count);

 Result := TMemoryStream.Create;
 WBCStream.Position := HeaderVec[index].Offset + HeaderVec[index].HeaderSize;

 key := 0;                                                                                         { Zero means no encryption }
 WBCStream.ReadBuffer(tempBuff, 8);                                                                { Read the WWBB... header }
 SetString(WBBHeader, tempBuff, 8);

 if AnsiSameStr(WBBHeader, WB0Hdr)
 then key := $A4
 else
    if AnsiSameStr(WBBHeader, WB1Hdr)
    then key := $F2;

 { Decode encrypted JPEG }
 if key <> 0 then
  begin
   { Decode 100 bytes }
   WBCStream.Read(A100, 100);
   WBCStream.Read(B100, 100);
   for I:= 1 to Length(A100)                                                                       { Formula: B(n) = (B(n) XOR (NOT A(n)) XOR key  -> key = ‘F2’ (hex) }
    DO D100[I]:= (B100[I] XOR (NOT A100[I])) XOR Key;

   { Build the final JPEG image }
   Result.WriteBuffer(D100, Sizeof(D100));
   Result.WriteBuffer(B100, Sizeof(B100));
   Result.CopyFrom(WBCStream, HeaderVec[index].PictureFileSize - 200);
  end
 else
   Result.CopyFrom(WBCStream, HeaderVec[index].PictureFileSize);
end;


procedure TWbcObj.ExtractJpegs(CONST OutputFolder: string);
VAR
   i: integer;
   JpgStream: TMemoryStream;
begin
 if NOT DirectoryExists(OutputFolder, TRUE) then EXIT;

 for i:= 0 to Count-1 DO
  TRY
    JpgStream:= GetJpgStream(i);
    JpgStream.SaveToFile(OutputFolder + HeaderVec[i].OrigFilename + '.jpg');
  FINALLY
    FreeAndNil(JpgStream);
  END;
end;


end.
