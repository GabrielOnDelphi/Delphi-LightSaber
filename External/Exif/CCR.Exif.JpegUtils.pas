{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing Exif metadata in JPEG files  }
{ Version 1.5.0 beta                                                                   }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is CCR.Exif.JPEGUtils.pas.                                         }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2011 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}
{$I CCR.Exif.inc}
unit CCR.Exif.JPEGUtils
{$IFDEF DepCom}
deprecated 'Use CCR.Exif.BaseUtils instead'
{$ELSE}
{$MESSAGE WARN 'CCR.Exif.JPEGUtils is deprecated: use CCR.Exif.BaseUtils instead'}
{$ENDIF};

interface

uses
  Types, SysUtils, Classes, Vcl.Graphics, Vcl.Imaging.jpeg, CCR.Exif.StreamHelper;

type
  EInvalidJPEGHeader = class(EInvalidGraphic);

  PJPEGSegmentHeader = ^TJPEGSegmentHeader;
  TJPEGSegmentHeader = packed record
  strict private
    function GetDataSize: Word;
    procedure SetDataSize(const Value: Word);
  public
    property DataSize: Word read GetDataSize write SetDataSize;
  public
    NewMarkerByte: Byte; //$FF
    MarkerNum: Byte;
    case Integer of
      0: (DataSizeHi, DataSizeLo: Byte;
          Data: record end);
      1: (DataSizeBigEndian: Word);
  end;

  TJFIFDensityUnits = (duNone, duPixelsPerInch, duPixelsPerCentimetre);

  TJFIFData = packed record
  strict private
    function GetHorzDensity: Word;
    procedure SetHorzDensity(const Value: Word);
    function GetVertDensity: Word;
    procedure SetVertDensity(const Value: Word);
  public
    property HorzDensity: Word read GetHorzDensity write SetHorzDensity;
    property VertDensity: Word read GetVertDensity write SetVertDensity;
  public
    Ident: array[0..4] of AnsiChar; //should be 'JFIF', including null terminator
    MajorVersion, MinorVersion: Byte;
    DensityUnits: TJFIFDensityUnits;
    case Integer of
      0: (HorzDensityHi, HorzDensityLo: Byte;
          VertDensityHi, VertDensityLo: Byte;
          ThumbnailWidth, ThumbnailHeight: Byte;
          ThumbnailPixels: record end);
      1: (HorzDensityBigEndian: Word;
          VertDensityBigEndian: Word);
  end;

  PJPEGStartOfFrameData = ^TJPEGStartOfFrameData;
  TJPEGStartOfFrameData = packed record
  strict private
    function GetImageWidth: Word;
    function GetImageHeight: Word;
    procedure SetImageHeight(const Value: Word);
    procedure SetImageWidth(const Value: Word);
  public
    property ImageWidth: Word read GetImageWidth write SetImageWidth;
    property ImageHeight: Word read GetImageHeight write SetImageHeight;
  public
    SamplePrecision: Byte; //8
    ImageHeightHi, ImageHeightLo: Byte;
    ImageWidthHi, ImageWidthLo: Byte;
    ComponentCount: Byte; //1 = gray scaled, 3 = color YCbCr or YIQ, 4 = color CMYK
    ComponentData: record end;
  end;

  TJPEGMarker = Byte;
  TJPEGMarkers = set of TJPEGMarker;

  IAdobeBlock = interface(IStreamPersist)
  ['{6E17F2E8-1486-4D6C-8824-CE7F06AB9319}']
    function GetSignature: AnsiString;
    procedure SetSignature(const Value: AnsiString);
    function GetTypeID: Word;
    procedure SetTypeID(Value: Word);
    function GetName: AnsiString;
    procedure SetName(const Value: AnsiString);
    function GetData: TCustomMemoryStream;

    function HasIPTCData: Boolean;
    property Signature: AnsiString read GetSignature write SetSignature;
    property TypeID: Word read GetTypeID write SetTypeID;
    property Name: AnsiString read GetName write SetName;
    property Data: TCustomMemoryStream read GetData;
  end;

  TAdobeApp13Enumerator = class
  strict private
    FCurrent: IAdobeBlock;
    FNextPos: Int64;
    FStream: TStream;
  public
    constructor Create(AStream: TStream; const AFirstBlockPos: Int64);
    function MoveNext: Boolean;
    property Current: IAdobeBlock read FCurrent;
  end;

  IJPEGSegment = interface
  ['{CF516230-D958-4C50-8C05-5AD985C6CBDD}']
    function GetData: TCustomMemoryStream;
    function GetEnumerator: TAdobeApp13Enumerator;
    function GetMarkerNum: TJPEGMarker;

    function IsAdobeApp13: Boolean;
    property Data: TCustomMemoryStream read GetData;
    property MarkerNum: TJPEGMarker read GetMarkerNum;
  end;

  TUserJPEGSegment = class(TInterfacedObject, IJPEGSegment)
  strict private
    FAdobeHeaderSize: Integer;
    FData: TMemoryStream;
    FMarkerNum: TJPEGMarker;
  protected
    { IJPEGSegment }
    function GetData: TCustomMemoryStream;
    function GetEnumerator: TAdobeApp13Enumerator;
    function GetMarkerNum: TJPEGMarker;
    function IsAdobeApp13: Boolean;
  public const
    AdobeHeader: array[0..13] of AnsiChar = 'Photoshop 3.0'#0;
    OldAdobeHeader: array[0..18] of AnsiChar = 'Adobe_Photoshop2.5:';
  public
    constructor Create(AMarkerNum: TJPEGMarker = 0; const ASource: IStreamPersist = nil); overload;
    destructor Destroy; override;
    property Data: TMemoryStream read FData;
    property MarkerNum: TJPEGMarker read FMarkerNum write FMarkerNum;
  end;

  IFoundJPEGSegment = interface(IJPEGSegment)
  ['{138192CD-85DD-4CEB-B1A7-4678C7D67C88}']
    function GetOffset: Int64;
    function GetOffsetOfData: Int64;
    function GetTotalSize: Word;

    property Offset: Int64 read GetOffset;
    property OffsetOfData: Int64 read GetOffsetOfData;
    property TotalSize: Word read GetTotalSize;
  end;

  IJPEGHeaderParser = interface
    function GetCurrent: IFoundJPEGSegment;
    function GetEnumerator: IJPEGHeaderParser;
    function MoveNext: Boolean;
    property Current: IFoundJPEGSegment read GetCurrent;
  end;

const
  JPEGSegmentHeaderSize = 4 deprecated {$IFDEF DepCom}'Use TJPEGSegment.HeaderSize in CCR.Exif.BaseUtils instead'{$ENDIF};

  AllJPEGMarkers = [Low(TJPEGMarker)..High(TJPEGMarker)];
  AnyJPEGMarker = AllJPEGMarkers;

  jmNewOrPadding       = TJPEGMarker($FF);
  jmStartOfImage       = TJPEGMarker($D8);
  jmEndOfImage         = TJPEGMarker($D9);
  jmQuantizationTable  = TJPEGMarker($DB);
  jmStartOfFrame0      = TJPEGMarker($C0);
  jmStartOfFrame1      = TJPEGMarker($C1);
  jmStartOfFrame2      = TJPEGMarker($C2);
  jmStartOfFrame3      = TJPEGMarker($C3);
  jmStartOfFrame5      = TJPEGMarker($C5);
  jmStartOfFrame6      = TJPEGMarker($C6);
  jmStartOfFrame7      = TJPEGMarker($C7);
  jmJPEGExtension      = TJPEGMarker($C8);
  jmHuffmanTable       = TJPEGMarker($C4);
  jmRestartInternal    = TJPEGMarker($DD);
  jmComment            = TJPEGMarker($FE);
  jmAppSpecificFirst   = TJPEGMarker($E0);
  jmAppSpecificLast    = TJPEGMarker($EF);
  jmJFIF               = TJPEGMarker($E0);
  jmApp1               = TJPEGMarker($E1);
  jmApp13              = TJPEGMarker($ED);
  jmStartOfScan        = TJPEGMarker($DA);

  JPEGFileHeader: Word = jmNewOrPadding or jmStartOfImage shl 8;
  StartOfFrameMarkers  = [jmStartOfFrame0..jmStartOfFrame3,
    jmStartOfFrame5..jmStartOfFrame7]; //there's no jmStartOfFrame4
  MarkersWithNoData = [$00, $01, $D0..$D9];
{
var
  Segment: IFoundJPEGSegment;
begin
  for Segment in JPEGHeader(JPEGStream) do
    ...
  for Segment in JPEGHeader('myimage.jpg') do
    ...
  for Segment in JPEGHeader(JPEGImage) do
    ...
}
function JPEGHeader(JPEGStream: TStream; const MarkersToLookFor: TJPEGMarkers;
  StreamOwnership: TStreamOwnership = soReference): IJPEGHeaderParser; overload;
function JPEGHeader(const JPEGFile: string;
  const MarkersToLookFor: TJPEGMarkers = AnyJPEGMarker): IJPEGHeaderParser; overload; inline;
function JPEGHeader(Image: TJPEGImage;
  const MarkersToLookFor: TJPEGMarkers = AnyJPEGMarker): IJPEGHeaderParser; overload;

function HasJPEGHeader(Stream: TStream): Boolean; overload;
function HasJPEGHeader(const FileName: string): Boolean; overload;

function GetJPEGDataSize(Data: TStream): Int64; overload;
function GetJPEGDataSize(JPEG: TJPEGImage): Int64; overload;

procedure WriteJPEGFileHeaderToStream(Stream: TStream); inline; deprecated {$IFDEF DepCom}'Use WriteJPEGFileHeader in CCR.Exif.BaseUtils instead'{$ENDIF};
procedure WriteJPEGSegmentToStream(Stream: TStream; MarkerNum: TJPEGMarker;
  const Data; DataSize: Word); overload; deprecated {$IFDEF DepCom}'Use WriteJPEGSegment in CCR.Exif.BaseUtils instead'{$ENDIF};
procedure WriteJPEGSegmentToStream(Stream: TStream; MarkerNum: TJPEGMarker;
  Data: TStream; DataSize: Word = 0); overload; deprecated {$IFDEF DepCom}'Use WriteJPEGSegment in CCR.Exif.BaseUtils instead'{$ENDIF};
procedure WriteJPEGSegmentToStream(Stream: TStream; Segment: IJPEGSegment); overload; deprecated {$IFDEF DepCom}'Use WriteJPEGSegment in CCR.Exif.BaseUtils instead'{$ENDIF};

const
  NewIPTCTagMarker: Byte = 28;

function CreateAdobeBlock: IAdobeBlock; overload;
function CreateAdobeBlock(ATypeID: Word; const AName: AnsiString;
  const ADataSource: IStreamPersist = nil): IAdobeBlock; overload;
function CreateAdobeBlock(const ASignature: AnsiString; ATypeID: Word;
  const AName: AnsiString; const ADataSource: IStreamPersist = nil): IAdobeBlock; overload;
function CreateAdobeApp13Segment(const Blocks: array of IAdobeBlock): IJPEGSegment; overload;
function CreateAdobeApp13Segment(const Blocks: IInterfaceList = nil): IJPEGSegment; overload;

function RemoveJPEGSegments(const JPEGFile: string; Markers: TJPEGMarkers): TJPEGMarkers; overload;
function RemoveJPEGSegments(Image: TJPEGImage; Markers: TJPEGMarkers): TJPEGMarkers; overload;

implementation

uses RTLConsts, CCR.Exif.Consts;

type
  TAdobeBlock = class(TInterfacedObject, IStreamPersist, IAdobeBlock)
  strict private
    FSignature: AnsiString;
    FTypeID: Word;
    FName: AnsiString;
    FData: TMemoryStream;
  protected
    { IStreamPersist}
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    { IAdobeBlock}
    function GetSignature: AnsiString;
    procedure SetSignature(const Value: AnsiString);
    function GetTypeID: Word;
    procedure SetTypeID(Value: Word);
    function GetName: AnsiString;
    procedure SetName(const Value: AnsiString);
    function GetData: TCustomMemoryStream;
    function HasIPTCData: Boolean;
  public
    constructor Create(AStream: TStream = nil); overload;
    constructor Create(const ASignature: AnsiString; ATypeID: Word;
      const AName: AnsiString; const ADataSource: IStreamPersist); overload;
    destructor Destroy; override;
  end;

  TFoundJPEGSegment = class(TUserJPEGSegment, IFoundJPEGSegment)
  strict private
    FOffset: Int64;
  protected
    { IFoundJPEGSegment }
    function GetOffset: Int64;
    function GetOffsetOfData: Int64;
    function GetTotalSize: Word;
  public
    constructor Create(AMakerNum: TJPEGMarker; ASource: TStream; ADataSize: Integer); overload;
  end;

  TJPEGHeaderParser = class(TInterfacedObject, IJPEGHeaderParser)
  strict private
    FCurrent: IFoundJPEGSegment;
    FLastMarker: TJPEGMarker;
    FMarkersToLookFor: TJPEGMarkers;
    FSavedPos, FStartPos: Int64;
    FStream: TStream;
    FStreamOwnership: TStreamOwnership;
  protected
    function GetEnumerator: IJPEGHeaderParser;
    function GetCurrent: IFoundJPEGSegment;
    function MoveNext: Boolean;
  public
    constructor Create(JPEGStream: TStream; const MarkersToLookFor: TJPEGMarkers;
      StreamOwnership: TStreamOwnership);
    destructor Destroy; override;
  end;

{ TAdobeBlock }

function GetValidSignature(const S: AnsiString): AnsiString;
begin
  Result := StringOfChar(AnsiChar(' '), 4);
  StrPLCopy(PAnsiChar(Result), S, 4);
end;

constructor TAdobeBlock.Create(AStream: TStream);
begin
  FData := TMemoryStream.Create;
  if AStream <> nil then LoadFromStream(AStream);
end;

constructor TAdobeBlock.Create(const ASignature: AnsiString; ATypeID: Word;
  const AName: AnsiString; const ADataSource: IStreamPersist);
begin
  Create;
  FSignature := GetValidSignature(ASignature);
  FTypeID := ATypeID;
  FName := AName;
  if ADataSource <> nil then ADataSource.SaveToStream(FData);
end;

destructor TAdobeBlock.Destroy;
begin
   FreeAndNil(FData);
  inherited;
end;

function TAdobeBlock.GetData: TCustomMemoryStream;
begin
  Result := FData;
end;

function TAdobeBlock.GetName: AnsiString;
begin
  Result := FName;
end;

function TAdobeBlock.GetSignature: AnsiString;
begin
  Result := FSignature;
end;

function TAdobeBlock.GetTypeID: Word;
begin
  Result := FTypeID;
end;

function TAdobeBlock.HasIPTCData: Boolean;
begin
  Result := (FTypeID = $0404) and (FData.Size > 4) and (PByte(FData.Memory)^ = NewIPTCTagMarker);
end;

procedure TAdobeBlock.LoadFromStream(Stream: TStream);
var
  Len: Integer;
begin
  SetString(FSignature, nil, 4);
  Stream.ReadBuffer(Pointer(FSignature)^, 4);
  FTypeID := Stream.ReadWord(BigEndian);
  Len := Stream.ReadByte;
  SetString(FName, nil, Len);
  if Len <> 0 then Stream.ReadBuffer(Pointer(FName)^, Len);
  if not Odd(Len) then Stream.ReadByte;
  Len := Stream.ReadLongInt(BigEndian);
  if Len < 0 then Len := 0; //!!!
  FData.Size := Len;
  if Len <> 0 then
  begin
    Stream.ReadBuffer(FData.Memory^, Len);
    if Odd(Len) then Stream.ReadByte;
  end;
end;

procedure TAdobeBlock.SaveToStream(Stream: TStream);
var
  Buffer: array[0..4] of AnsiChar;
  Len: Integer;
begin
  Stream.WriteBuffer(StrPLCopy(Buffer, FSignature, 4)^, 4);
  Stream.WriteWord(FTypeID, BigEndian);
  Len := Length(FName);
  if Len > High(Byte) then Len := High(Byte);
  Stream.WriteByte(Len);
  if Len <> 0 then Stream.WriteBuffer(Pointer(FName)^, Len);
  if not Odd(Len) then Stream.WriteByte(0);
  Len := FData.Size;
  Stream.WriteLongInt(Len, BigEndian);
  if Len <> 0 then Stream.WriteBuffer(FData.Memory^, Len);
  if Odd(Len) then Stream.WriteByte(0);
end;

procedure TAdobeBlock.SetName(const Value: AnsiString);
begin
  FName := Value;
end;

procedure TAdobeBlock.SetSignature(const Value: AnsiString);
begin
  FSignature := GetValidSignature(Value);
end;

procedure TAdobeBlock.SetTypeID(Value: Word);
begin
  FTypeID := Value;
end;

{ TAdobeApp13Enumerator }

constructor TAdobeApp13Enumerator.Create(AStream: TStream; const AFirstBlockPos: Int64);
begin
  FStream := AStream;
  FNextPos := AFirstBlockPos;
end;

function TAdobeApp13Enumerator.MoveNext: Boolean;
begin
  FCurrent := nil;
  Result := FNextPos < FStream.Size - 12;
  if not Result then Exit;
  FStream.Position := FNextPos;
  FCurrent := TAdobeBlock.Create(FStream);
  FNextPos := FStream.Position;
end;

{ TJPEGSegmentHeader }

function TJPEGSegmentHeader.GetDataSize: Word;
begin
  Result := DataSizeLo or (DataSizeHi shl 8);
end;

procedure TJPEGSegmentHeader.SetDataSize(const Value: Word);
begin
  with WordRec(Value) do
  begin
    DataSizeHi := Hi;
    DataSizeLo := Lo;
  end;
end;

{ TJFIFData }

function TJFIFData.GetHorzDensity: Word;
begin
  Result := HorzDensityLo or (HorzDensityHi shl 8);
end;

function TJFIFData.GetVertDensity: Word;
begin
  Result := VertDensityLo or (VertDensityHi shl 8);
end;

procedure TJFIFData.SetHorzDensity(const Value: Word);
begin
  with WordRec(Value) do
  begin
    HorzDensityHi := Hi;
    HorzDensityLo := Lo;
  end;
end;

procedure TJFIFData.SetVertDensity(const Value: Word);
begin
  with WordRec(Value) do
  begin
    VertDensityHi := Hi;
    VertDensityLo := Lo;
  end;
end;

{ TJPEGStartOfFrameData }

function TJPEGStartOfFrameData.GetImageWidth: Word;
begin
  Result := ImageWidthLo or (ImageWidthHi shl 8);
end;

function TJPEGStartOfFrameData.GetImageHeight: Word;
begin
  Result := ImageHeightLo or (ImageHeightHi shl 8);
end;

procedure TJPEGStartOfFrameData.SetImageHeight(const Value: Word);
begin
  with WordRec(Value) do
  begin
    ImageHeightHi := Hi;
    ImageHeightLo := Lo;
  end;
end;

procedure TJPEGStartOfFrameData.SetImageWidth(const Value: Word);
begin
  with WordRec(Value) do
  begin
    ImageWidthHi := Hi;
    ImageWidthLo := Lo;
  end;
end;

{ TUserJPEGSegment }

constructor TUserJPEGSegment.Create(AMarkerNum: TJPEGMarker = 0;
  const ASource: IStreamPersist = nil);
begin
  FData := TMemoryStream.Create;
  FMarkerNum := AMarkerNum;
  if ASource <> nil then
  begin
    ASource.SaveToStream(FData);
    FData.Position := 0;
  end;
end;

destructor TUserJPEGSegment.Destroy;
begin
   FreeAndNil(FData);
  inherited;
end;

function TUserJPEGSegment.GetData: TCustomMemoryStream;
begin
  Result := FData;
end;

function TUserJPEGSegment.GetEnumerator: TAdobeApp13Enumerator;
begin
  if IsAdobeApp13 then
    Result := TAdobeApp13Enumerator.Create(Data, FAdobeHeaderSize)
  else
    Result := TAdobeApp13Enumerator.Create(Data, MaxLongint);
end;

function TUserJPEGSegment.GetMarkerNum: TJPEGMarker;
begin
  Result := FMarkerNum;
end;

function TUserJPEGSegment.IsAdobeApp13: Boolean;
begin
  Result := False;
  FAdobeHeaderSize := 0;
  if (MarkerNum <> jmApp13) or (Data.Size < 20) then Exit;
  if CompareMem(@AdobeHeader, Data.Memory, SizeOf(AdobeHeader)) then
    FAdobeHeaderSize := SizeOf(AdobeHeader)
  else if CompareMem(@OldAdobeHeader, Data.Memory, SizeOf(OldAdobeHeader)) then
    FAdobeHeaderSize := SizeOf(OldAdobeHeader)
  else
    Exit;
  Result := True;
end;

{ TFoundJPEGSegment }

constructor TFoundJPEGSegment.Create(AMakerNum: TJPEGMarker; ASource: TStream;
  ADataSize: Integer);
begin
  inherited Create(AMakerNum);
  FOffset := ASource.Position - SizeOf(TJPEGSegmentHeader);
  if AMakerNum in MarkersWithNoData then
    Inc(FOffset, 2)
  else if ADataSize > 0 then
  begin
    Data.Size := ADataSize;
    ASource.ReadBuffer(Data.Memory^, ADataSize);
  end;
end;

{ TFoundJPEGSegment.IFoundJPEGSegment }

function TFoundJPEGSegment.GetOffset: Int64;
begin
  Result := FOffset;
end;

function TFoundJPEGSegment.GetOffsetOfData: Int64;
begin
  Result := FOffset + SizeOf(TJPEGSegmentHeader);
end;

function TFoundJPEGSegment.GetTotalSize: Word;
begin
  Result := Word(Data.Size + SizeOf(TJPEGSegmentHeader));
  if MarkerNum in MarkersWithNoData then Dec(Result, 2);
end;

{ TJPEGHeaderParser }

constructor TJPEGHeaderParser.Create(JPEGStream: TStream;
  const MarkersToLookFor: TJPEGMarkers; StreamOwnership: TStreamOwnership);
begin
  inherited Create;
  FMarkersToLookFor := MarkersToLookFor;
  FStartPos := JPEGStream.Position;
  FStream := JPEGStream;
  FStreamOwnership := StreamOwnership;
  if JPEGStream.ReadWord(SmallEndian) <> JPEGFileHeader then
    raise EInvalidJPEGHeader.Create(SInvalidJPEGHeader);
  FSavedPos := FStartPos + 2;
end;

destructor TJPEGHeaderParser.Destroy;
begin
  if FStreamOwnership = soOwned then //If loop didn't complete, MoveNext
    FreeAndNil(FStream);             //wouldn't have had the chance to
  inherited;                         //free the stream.
end;

function TJPEGHeaderParser.GetCurrent: IFoundJPEGSegment;
begin
  Result := FCurrent;
end;

function TJPEGHeaderParser.GetEnumerator: IJPEGHeaderParser;
begin
  Result := Self;
end;

function TJPEGHeaderParser.MoveNext: Boolean;
var
  AllocatedBuffer: Boolean;
  Buffer: PAnsiChar;
  BufferSize: Integer;
  DataSize: Word;
  MaxPos, SeekPtr: PAnsiChar;
begin
  FCurrent := nil;
  Result := False;
  if (FStream = nil) or (FLastMarker = jmEndOfImage) then Exit;
  FStream.Position := FSavedPos;
  while FStream.ReadByte = jmNewOrPadding do
  begin
    repeat
      FLastMarker := FStream.ReadByte;
    until (FLastMarker <> jmNewOrPadding); //extra $FF bytes are legal as padding
    if FLastMarker in MarkersWithNoData then
      DataSize := 0
    else
      DataSize := FStream.ReadWord(BigEndian) - 2;
    if not (FLastMarker in FMarkersToLookFor) then
      if FLastMarker = jmEndOfImage then
        Break
      else
        FStream.Seek(DataSize, soCurrent)
    else
    begin
      FCurrent := TFoundJPEGSegment.Create(FLastMarker, FStream, DataSize);
      FSavedPos := FStream.Position;
      Result := True;
      Exit;
    end;
  end;
  if (FLastMarker = jmStartOfScan) and (jmEndOfImage in FMarkersToLookFor) then
  begin
    FSavedPos := FStream.Position;
    BufferSize := FStream.Size - FSavedPos;
    AllocatedBuffer := not (FStream is TCustomMemoryStream);
    if AllocatedBuffer then
      GetMem(Buffer, BufferSize)
    else
    begin
      Buffer := TCustomMemoryStream(FStream).Memory;
      Inc(Buffer, FSavedPos);
    end;
    try
      if AllocatedBuffer then FStream.ReadBuffer(Buffer^, BufferSize);
      MaxPos := @Buffer[BufferSize - 1];
      SeekPtr := Buffer;
      while SeekPtr < MaxPos do
      begin
        Inc(SeekPtr);
        if Byte(SeekPtr^) <> jmNewOrPadding then Continue;
        Inc(SeekPtr);
        if Byte(SeekPtr^) <> jmEndOfImage then Continue;
        Inc(SeekPtr);
        FStream.Position := FSavedPos + (SeekPtr - Buffer);
        FCurrent := TFoundJPEGSegment.Create(jmEndOfImage, FStream, 0);
        FLastMarker := jmEndOfImage;
        Result := True;
        Exit;
      end;
    finally
      if AllocatedBuffer then FreeMem(Buffer);
    end;
    FStream.Seek(0, soEnd);
  end;
  if FStreamOwnership = soOwned then
    FreeAndNil(FStream);
end;

function JPEGHeader(JPEGStream: TStream; const MarkersToLookFor: TJPEGMarkers;
  StreamOwnership: TStreamOwnership = soReference): IJPEGHeaderParser; overload;
begin
  Result := TJPEGHeaderParser.Create(JPEGStream, MarkersToLookFor, StreamOwnership);
end;

function JPEGHeader(const JPEGFile: string;
  const MarkersToLookFor: TJPEGMarkers): IJPEGHeaderParser;
begin
  Result := JPEGHeader(TFileStream.Create(JPEGFile, fmOpenRead), MarkersToLookFor,
    soOwned);
end;

function JPEGHeader(Image: TJPEGImage;
  const MarkersToLookFor: TJPEGMarkers): IJPEGHeaderParser;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Image.SaveToStream(Stream);
  Stream.Position := 0;
  Result := JPEGHeader(Stream, MarkersToLookFor, soOwned);
end;

function HasJPEGHeader(Stream: TStream): Boolean; overload;
begin
  Result := Stream.TryReadHeader(JPEGFileHeader, SizeOf(JPEGFileHeader));
  if Result then Stream.Seek(-SizeOf(JPEGFileHeader), soCurrent);
end;

function HasJPEGHeader(const FileName: string): Boolean; overload;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := HasJPEGHeader(Stream);
  finally
     FreeAndNil(Stream);
  end;
end;

function GetJPEGDataSize(Data: TStream): Int64; overload;
var
  OrigPos: Int64;
  Segment: IFoundJPEGSegment;
begin
  OrigPos := Data.Position;
  Result := Data.Size - OrigPos;
  for Segment in JPEGHeader(Data, [jmEndOfImage]) do
    Result := Data.Position - OrigPos;
  Data.Position := OrigPos;
end;

function GetJPEGDataSize(JPEG: TJPEGImage): Int64; overload;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    JPEG.SaveToStream(Stream); //TJPEGImage.LoadFromStream keeps everything from the starting pos
    Stream.Position := 0;
    Result := GetJPEGDataSize(Stream);
  finally
     FreeAndNil(Stream);
  end;
end;

procedure WriteJPEGFileHeaderToStream(Stream: TStream); inline;
begin
  Stream.WriteBuffer(JPEGFileHeader, SizeOf(JPEGFileHeader));
end;

procedure WriteJPEGSegmentToStream(Stream: TStream; MarkerNum: TJPEGMarker;
  const Data; DataSize: Word); overload;
var
  Header: TJPEGSegmentHeader;
begin
  Header.NewMarkerByte := jmNewOrPadding;
  Header.MarkerNum := MarkerNum;
  Inc(DataSize, 2);
  Header.DataSizeHi := Hi(DataSize);
  Header.DataSizeLo := Lo(DataSize);
  Dec(DataSize, 2);
  Stream.WriteBuffer(Header, SizeOf(Header));
  if (DataSize > 0) and (@Data <> nil) then Stream.WriteBuffer(Data, DataSize);
end;

procedure WriteJPEGSegmentToStream(Stream: TStream; MarkerNum: TJPEGMarker;
  Data: TStream; DataSize: Word); overload;
var
  Buffer: Pointer;
  BufferAllocated: Boolean;
begin
  if DataSize = 0 then
  begin
    Data.Position := 0;
    DataSize := Word(Data.Size);
    if DataSize = 0 then
    begin
      WriteJPEGSegmentToStream(Stream, MarkerNum, MarkerNum, 0);
      Exit;
    end;
  end;
  BufferAllocated := not (Data is TCustomMemoryStream);
  if BufferAllocated then
    GetMem(Buffer, DataSize)
  else
  begin
    Buffer := @PByteArray(TCustomMemoryStream(Data).Memory)[Data.Position];
    Data.Seek(DataSize, soCurrent);
  end;
  try
    if BufferAllocated then Data.ReadBuffer(Buffer^, DataSize);
    WriteJPEGSegmentToStream(Stream, MarkerNum, Buffer^, DataSize);
  finally
    if BufferAllocated then FreeMem(Buffer);
  end;
end;

procedure WriteJPEGSegmentToStream(Stream: TStream; Segment: IJPEGSegment); overload;
begin
  WriteJPEGSegmentToStream(Stream, Segment.MarkerNum, Segment.Data);
end;

function CreateAdobeBlock: IAdobeBlock; overload;
begin
  Result := TAdobeBlock.Create;
end;

function CreateAdobeBlock(ATypeID: Word; const AName: AnsiString;
  const ADataSource: IStreamPersist): IAdobeBlock; overload;
begin
  Result := TAdobeBlock.Create('8BIM', ATypeID, AName, ADataSource);
end;

function CreateAdobeBlock(const ASignature: AnsiString; ATypeID: Word;
  const AName: AnsiString; const ADataSource: IStreamPersist): IAdobeBlock; overload;
begin
  Result := TAdobeBlock.Create(ASignature, ATypeID, AName, ADataSource);
end;

function CreateAdobeApp13Segment(const Blocks: array of IAdobeBlock): IJPEGSegment;
var
  Item: IAdobeBlock;
  Stream: TStream;
begin
  Result := TUserJPEGSegment.Create(jmApp13);
  Stream := Result.Data;
  Stream.WriteBuffer(TUserJPEGSegment.AdobeHeader, SizeOf(TUserJPEGSegment.AdobeHeader));
  for Item in Blocks do
    Item.SaveToStream(Stream);
end;

function CreateAdobeApp13Segment(const Blocks: IInterfaceList): IJPEGSegment;
var
  I: Integer;
  Stream: TStream;
begin
  Result := TUserJPEGSegment.Create(jmApp13);
  Stream := Result.Data;
  Stream.WriteBuffer(TUserJPEGSegment.AdobeHeader, SizeOf(TUserJPEGSegment.AdobeHeader));
  if Blocks <> nil then
    for I := 0 to Blocks.Count - 1 do
      (Blocks[I] as IAdobeBlock).SaveToStream(Stream);
end;

function DoRemoveJPEGSegments(InStream, OutStream: TStream; Markers: TJPEGMarkers): TJPEGMarkers;
var
  Segment: IFoundJPEGSegment;
  StartPos: Int64;
begin
  Result := [];
  StartPos := InStream.Position;
  for Segment in JPEGHeader(InStream, Markers) do
  begin
    Include(Result, Segment.MarkerNum);
    if Segment.Offset <> StartPos then
    begin
      InStream.Position := StartPos;
      OutStream.CopyFrom(InStream, Segment.Offset - StartPos);
    end;
    StartPos := Segment.Offset + Segment.TotalSize;
  end;
  InStream.Position := StartPos;
  OutStream.CopyFrom(InStream, InStream.Size - StartPos);
end;

function RemoveJPEGSegments(const JPEGFile: string; Markers: TJPEGMarkers): TJPEGMarkers;
var
  InStream: TMemoryStream;
  OutStream: TFileStream;
begin
  if Markers = [] then Exit;
  OutStream := nil;
  InStream := TMemoryStream.Create;
  try
    InStream.LoadFromFile(JPEGFile);
    OutStream := TFileStream.Create(JPEGFile, fmCreate);
    Result := DoRemoveJPEGSegments(InStream, OutStream, Markers);
  finally
     FreeAndNil(OutStream);
     FreeAndNil(InStream);
  end;
end;

function RemoveJPEGSegments(Image: TJPEGImage; Markers: TJPEGMarkers): TJPEGMarkers;
var
  InStream, OutStream: TMemoryStream;
begin
  if Markers = [] then Exit;
  OutStream := nil;
  InStream := TMemoryStream.Create;
  try
    Image.SaveToStream(InStream);
    InStream.Position := 0;
    OutStream := TMemoryStream.Create;
    Result := DoRemoveJPEGSegments(InStream, OutStream, Markers);
    if Result <> [] then
    begin
      OutStream.Position := 0;
      Image.LoadFromStream(OutStream);
    end;
  finally
     FreeAndNil(OutStream);
     FreeAndNil(InStream);
  end;
end;

end.
