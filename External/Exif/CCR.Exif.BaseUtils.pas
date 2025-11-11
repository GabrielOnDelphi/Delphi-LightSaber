{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing image metadata               }
{ Version 1.5.3                                                                        }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is CCR.Exif.BaseUtils.pas.                                         }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2014 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

{$I CCR.Exif.inc}
unit CCR.Exif.BaseUtils;

interface

uses
  Types, SysUtils, Classes, {$IFNDEF UNICODE}WideStrings,{$ENDIF}
  {$IFDEF NeedHausladenByteStringsFix}System.ByteStrings,{$ENDIF}
  {$IFDEF HasGenerics}Generics.Collections{$ELSE}Contnrs{$ENDIF},
  CCR.Exif.StreamHelper;

{ Mappings for compatibility with compilers for mobile }
type
{$IFDEF NeedHausladenByteStringsFix} //see http://andy.jgknet.de/blog/2013/10/the-return-of-the-byte-strings/
  AnsiChar = System.ByteStrings.AnsiChar;
  AnsiString = System.ByteStrings.AnsiString;
  PAnsiChar = System.ByteStrings.PAnsiChar;
  RawByteString = System.ByteStrings.RawByteString;
  UTF8String = System.ByteStrings.UTF8String;
{$ENDIF}
{$IFDEF HasGenerics}
  TClassList = TList<TClass>;
  TObjectList = TObjectList<TObject>;
{$ELSE}
  TClassList = Contnrs.TClassList;
  TObjectList = Contnrs.TObjectList;
{$ENDIF}
{$IFDEF NEXTGEN}
  {$WARNINGS OFF}
  TSysCharSet = set of Char;
  {$WARNINGS ON}
{$ENDIF}

{ Mappings for compatibility with older compilers }
type
{$IF not Declared(EProgrammerNotFound)}
  EProgrammerNotFound = class(Exception);
{$IFEND}
{$IFDEF UNICODE}
  TUnicodeStrings = TStrings;
  TUnicodeStringList = TStringList;
{$ELSE}
  TUnicodeStrings = TWideStrings;
  TUnicodeStringList = TWideStringList;
  RawByteString = AnsiString;       //added in D2009
  UnicodeString = WideString;       //added in D2009
  {$IF not Declared(TBytes)}
  TBytes = array of Byte;           //added in D2007
  {$IFEND}
function UpCase(const Ch: AnsiChar): AnsiChar; overload;
function UpCase(const Ch: WideChar): WideChar; overload;
function UTF8ToString(const UTF8: RawByteString): string; inline; overload;
{$ENDIF}
function IsCharIn(Ch: AnsiChar; const CharSet: TSysCharSet): Boolean; inline; overload;
function IsCharIn(Ch: WideChar; const CharSet: TSysCharSet): Boolean; inline; overload; //not CharInSet to avoid ambiguous overload errors
function UTF8ToString(UTF8Buffer: Pointer; ByteLen: Integer): string; overload;
function BinToHexStr(Data: Pointer; Size: Integer): string; overload;
function BinToHexStr(const Buffer; Size: Integer): string; overload; inline;
function HexStrToBin(const Text: string): TBytes;

function GetExecutableName: string; inline;

{ non-specific }

type
  ECCRExifException = class(Exception);
  EUnsupportedGraphicFormat = class(ECCRExifException);

  TMetadataLoadError = (leBadOffset, leBadTagCount, leBadTagHeader);
  TMetadataLoadErrors = set of TMetadataLoadError;

  TNoRefCountInterfacedObject = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  IStreamPersistEx = interface(IStreamPersist)
  ['{2E9F3433-E5A9-4FBA-9C89-68B97DADA33C}']
    function GetEmpty: Boolean;
    property Empty: Boolean read GetEmpty;
  end;

  IMetadataBlock = interface
  ['{F77F5203-C526-4457-81A7-13941FE461CB}']
    function GetData: TCustomMemoryStream;

    function IsExifBlock(CheckID: Boolean = True): Boolean;
    function IsIPTCBlock(CheckID: Boolean = True): Boolean;
    function IsXMPBlock(CheckID: Boolean = True): Boolean;
    property Data: TCustomMemoryStream read GetData; //this is not guaranteed to be writeable
  end;

  TMetadataBlockDynArray = array of IMetadataBlock;

  TMetadataBlock = class(TInterfacedObject, IMetadataBlock)
  protected type
    TDataStream = class(TMemoryStream)
    strict private
      FDisableChanges: Boolean;
    protected
      {$IF CompilerVersion < 35}
      function Realloc(var NewCapacity: Longint): Pointer; override;
      {$ELSE}
      function Realloc(var NewCapacity: NativeInt): Pointer; override;
      {$IFEND}
    {$IF CompilerVersion < 33} protected {$ELSE} public {$IFEND}
      procedure SetSize(const NewSize: Int64); override;
    public
      destructor Destroy; override;
      procedure SetSize(NewSize: Longint); override;
      function Write(const Buffer; Count: Longint): Longint; override;
      property DisableChanges: Boolean read FDisableChanges write FDisableChanges;
    end;
  strict private
    FData: TDataStream;
  protected
    function GetData: TCustomMemoryStream;
    function HasExifBlockID: Boolean; virtual;
    function HasIPTCBlockID: Boolean; virtual;
    function HasXMPBlockID: Boolean; virtual;
  public
    constructor Create(const DataSource: IStreamPersist = nil); overload;
    constructor Create(DataSource: TStream; BytesToRead: Integer); overload;
    destructor Destroy; override;
    function IsExifBlock(CheckID: Boolean = True): Boolean;
    function IsIPTCBlock(CheckID: Boolean = True): Boolean;
    function IsXMPBlock(CheckID: Boolean = True): Boolean;
    property Data: TDataStream read FData;
  end;

  TDateTimeTagValue = record
  strict private const
    FirstValidDateTime: TDateTime = -693593; //1st Jan, 1 AD
    MissingOrInvalidValue: TDateTime = -700000;
  strict private
    FValue: TDateTime;
  public
    constructor CreateFromString(const AString: string);
    class function CreateMissingOrInvalid: TDateTimeTagValue; static;
    class operator Equal(const A, B: TDateTimeTagValue): Boolean;
    class operator NotEqual(const A, B: TDateTimeTagValue): Boolean;
    class operator Implicit(const Source: TDateTimeTagValue): TDateTime;
    class operator Implicit(const Source: TDateTime): TDateTimeTagValue;
    class operator LessThan(const A, B: TDateTimeTagValue): Boolean;
    class operator LessThanOrEqual(const A, B: TDateTimeTagValue): Boolean;
    class operator GreaterThan(const A, B: TDateTimeTagValue): Boolean;
    class operator GreaterThanOrEqual(const A, B: TDateTimeTagValue): Boolean;
    function ToString: string; overload;
    function ToString(const Format: string): string; overload;
    {$IFDEF HasFormatSettings}
    function ToString(const Format: string; const Settings: TFormatSettings): string; overload;
    {$ENDIF}
    property AsString: string read ToString;
    function MissingOrInvalid: Boolean; inline;
    property Value: TDateTime read FValue;
  end;

  TLongIntTagValue = record
  strict private
    FValue: LongInt;
    FMissingOrInvalid: Boolean;
  public
    constructor CreateFromString(const AString: string);
    class function CreateMissingOrInvalid: TLongIntTagValue; static;
    class operator Equal(const A, B: TLongIntTagValue): Boolean;
    class operator NotEqual(const A, B: TLongIntTagValue): Boolean;
    class operator Implicit(const Source: TLongIntTagValue): Int64;
    class operator Implicit(const Source: TLongIntTagValue): LongInt;
    class operator Implicit(const Source: LongInt): TLongIntTagValue;
    class operator LessThan(const A, B: TLongIntTagValue): Boolean;
    class operator LessThanOrEqual(const A, B: TLongIntTagValue): Boolean;
    class operator GreaterThan(const A, B: TLongIntTagValue): Boolean;
    class operator GreaterThanOrEqual(const A, B: TLongIntTagValue): Boolean;
    class operator Negative(const Source: TLongIntTagValue): TLongIntTagValue;
    function ToString: string;
    property AsString: string read ToString;
    property MissingOrInvalid: Boolean read FMissingOrInvalid;
    property Value: LongInt read FValue;
  end;

  TLongWordTagValue = record
  strict private
    FValue: LongWord;
    FMissingOrInvalid: Boolean;
  public
    constructor CreateFromString(const AString: string);
    class function CreateMissingOrInvalid: TLongWordTagValue; static;
    class operator Equal(const A, B: TLongWordTagValue): Boolean;
    class operator NotEqual(const A, B: TLongWordTagValue): Boolean;
    class operator Implicit(const Source: TLongWordTagValue): Int64;
    class operator Implicit(const Source: TLongWordTagValue): UInt64;
    class operator Implicit(const Source: TLongWordTagValue): LongWord;
    class operator Implicit(const Source: LongWord): TLongWordTagValue;
    class operator LessThan(const A, B: TLongWordTagValue): Boolean;
    class operator LessThanOrEqual(const A, B: TLongWordTagValue): Boolean;
    class operator GreaterThan(const A, B: TLongWordTagValue): Boolean;
    class operator GreaterThanOrEqual(const A, B: TLongWordTagValue): Boolean;
    function ToString: string;
    property AsString: string read ToString;
    property MissingOrInvalid: Boolean read FMissingOrInvalid;
    property Value: LongWord read FValue;
  end;

  TWordTagValue = record
  strict private
    FValue: Word;
    FMissingOrInvalid: Boolean;
  public
    constructor CreateFromString(const AString: string);
    class function CreateMissingOrInvalid: TWordTagValue; static;
    class operator Equal(const A, B: TWordTagValue): Boolean;
    class operator NotEqual(const A, B: TWordTagValue): Boolean;
    class operator Implicit(const Source: TWordTagValue): Int64;
    class operator Implicit(const Source: TWordTagValue): UInt64;
    class operator Implicit(const Source: TWordTagValue): Integer;
    class operator Implicit(const Source: TWordTagValue): Word;
    class operator Implicit(const Source: TWordTagValue): TLongIntTagValue;
    class operator Implicit(const Source: TWordTagValue): TLongWordTagValue;
    class operator Implicit(const Source: Word): TWordTagValue;
    class operator LessThan(const A, B: TWordTagValue): Boolean;
    class operator LessThanOrEqual(const A, B: TWordTagValue): Boolean;
    class operator GreaterThan(const A, B: TWordTagValue): Boolean;
    class operator GreaterThanOrEqual(const A, B: TWordTagValue): Boolean;
    function ToString: string;
    property AsString: string read ToString;
    property MissingOrInvalid: Boolean read FMissingOrInvalid;
    property Value: Word read FValue;
  end;

  TSmallPointHelper = record helper for TSmallPoint
    class function CreateMissingOrInvalid: TSmallPoint; static;
    function MissingOrInvalid: Boolean; inline;
  end;

  TUserMemoryStream = class(TCustomMemoryStream) //read-only stream access on an existing buffer;
  protected
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(Memory: Pointer; Size: Integer);
    procedure ChangeMemory(NewMemory: Pointer; NewSize: Integer);
    function Write(const Buffer; Count: Integer): Integer; override;
  end;

  TGraphicSaveMethod = procedure (InStream, OutStream: TStream) of object;
  TGetGraphicSaveMethod = procedure (InStream: TStream; var AMethod: TGraphicSaveMethod) of object;

procedure DoSaveToGraphic(GraphicStream: TStream;
  const DetectTypeFunc: TGetGraphicSaveMethod); overload;
procedure DoSaveToGraphic(const FileName: string;
  const DetectTypeFunc: TGetGraphicSaveMethod); overload;
procedure DoSaveToGraphic(const Graphic: IStreamPersist; //!!!changed from TGraphic
  const DetectTypeFunc: TGetGraphicSaveMethod); overload;

{ PSD file parsing and writing; note that Adobe resource blocks may be found in other types of file too }

type
  EInvalidPSDHeader = class(ECCRExifException);

  TIPTCSectionID = 1..9;
  TIPTCTagID = type Byte;

  TIPTCTagInfo = record
    SectionID: TIPTCSectionID;
    TagID: TIPTCTagID;
    DataSize: Integer;
  end;

  TAdobeResBlock = record public const
    ExifTypeID = $0422;
    IPTCTypeID = $0404;
    XMPTypeID = $0424;
    NewIPTCTagMarker: Byte = 28;
    class function TryReadIPTCHeader(Stream: TStream; var TagInfo: TIPTCTagInfo;
      AlwaysResetStreamPos: Boolean = False): Boolean; static;
  end;

  IAdobeResBlock = interface(IMetadataBlock)//also IStreamPersist...
  ['{D7F88330-C1DF-4261-A3B2-1701AFAB8873}']
    function GetSignature: AnsiString;
    procedure SetSignature(const Value: AnsiString);
    function GetTypeID: Word;
    procedure SetTypeID(Value: Word);
    function GetName: AnsiString;
    procedure SetName(const Value: AnsiString);
    function GetTotalSize: Integer;
    function GetLoadedCleanly: Boolean;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    property LoadedCleanly: Boolean read GetLoadedCleanly;
    property Signature: AnsiString read GetSignature write SetSignature;
    property TypeID: Word read GetTypeID write SetTypeID;
    property Name: AnsiString read GetName write SetName;
    property TotalSize: Integer read GetTotalSize;
  end;

  IAdobeResBlockEnumerator = interface
    function GetCurrent: IAdobeResBlock;
    function MoveNext: Boolean;
    property Current: IAdobeResBlock read GetCurrent;
  end;

  {$Z2}
  TAdobeColorMode = (cmMonochrome, cmGrayscale, cmIndexed, cmRGB, cmCMYK,
    cmMultichannel, cmHalftone, cmLAB);
  {$Z1}

  TPSDHeaderInfo = record
    Width, Height: Integer;
    Channels, Depth: Word;
    ColorMode: TAdobeColorMode;
    PaletteData: TBytes;
  end;

  TPSDInfo = record
    Header: TPSDHeaderInfo;
    ResourceSectionSize, LayersSectionSize: Integer; //includes size of section length field
    function ResourceSectionOffset: Integer;
    function LayersSectionOffset: Integer;
  end;

  IAdobeResBlockParser = interface
    function GetEnumerator: IAdobeResBlockEnumerator;
  end;

function CreateAdobeBlock: IAdobeResBlock; overload;
function CreateAdobeBlock(ATypeID: Word; const ADataSource: IStreamPersist = nil): IAdobeResBlock; overload;
function CreateAdobeBlock(ATypeID: Word; ADataSource: TStream; ADataSize: Integer = 0): IAdobeResBlock; overload;
function CreateAdobeBlock(ATypeID: Word; const AName: AnsiString;
  const ADataSource: IStreamPersist = nil): IAdobeResBlock; overload;
function CreateAdobeBlock(const ASignature: AnsiString; ATypeID: Word;
  const AName: AnsiString; const ADataSource: IStreamPersist = nil): IAdobeResBlock; overload;

function HasPSDHeader(Stream: TStream): Boolean; overload;
function HasPSDHeader(const FileName: string): Boolean; overload;

function ParsePSDHeader(Stream: TStream; var Info: TPSDInfo;
  StreamOwnership: TStreamOwnership = soReference): IAdobeResBlockParser; overload;
function ParsePSDHeader(const FileName: string; var Info: TPSDInfo): IAdobeResBlockParser; overload; inline;

procedure WritePSDHeader(Stream: TStream; const Info: TPSDHeaderInfo);
procedure WritePSDResourceSection(Stream: TStream; const Blocks: array of IAdobeResBlock); overload;
procedure WritePSDResourceSection(Stream: TStream; const Blocks: IInterfaceList); overload;

{ JPEG file parsing and writing }

type
  EInvalidJPEGHeader = class(ECCRExifException);

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

  IJPEGSegment = interface(IMetadataBlock) //also IStreamPersistEx...
  ['{5F41F0BA-C43F-4671-8B66-4B6D5B2BF90F}']
    function GetEnumerator: IAdobeResBlockEnumerator;
    function GetMarkerNum: TJPEGMarker;

    function IsAdobeApp13: Boolean;
    property MarkerNum: TJPEGMarker read GetMarkerNum;
  end;

  TUserJPEGSegment = class(TMetadataBlock, IMetadataBlock, IJPEGSegment,
    IStreamPersist, IStreamPersistEx)
  strict private
    FAdobeHeaderSize: Integer;
    FMarkerNum: TJPEGMarker;
  protected
    function HasExifBlockID: Boolean; override;
    function HasIPTCBlockID: Boolean; override;
    function HasXMPBlockID: Boolean; override;
    { IStreamPersist }
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    { IStreamPersistEx}
    function GetEmpty: Boolean;
    { IJPEGSegment }
    function GetMarkerNum: TJPEGMarker;
    function IsAdobeApp13: Boolean;
  public const
    AdobeHeader: array[0..13] of AnsiChar = 'Photoshop 3.0'#0;
    OldAdobeHeader: array[0..18] of AnsiChar = 'Adobe_Photoshop2.5:';
  public
    constructor Create(AMarkerNum: TJPEGMarker = 0; const ASource: IStreamPersist = nil); overload;
    function GetEnumerator: IAdobeResBlockEnumerator;
    function IsIPTCBlock(CheckID: Boolean = True): Boolean; //override inherited implementation
    property MarkerNum: TJPEGMarker read FMarkerNum write FMarkerNum;
  end;

  IFoundJPEGSegment = interface(IJPEGSegment)
  ['{C236AF21-5577-439A-BD44-8C0673347F44}']
    function GetOffset: Int64;
    function GetOffsetOfData: Int64;
    function GetTotalSize: Integer;

    property Offset: Int64 read GetOffset;
    property OffsetOfData: Int64 read GetOffsetOfData;
    property TotalSize: Integer read GetTotalSize;
  end;

  IJPEGHeaderParser = interface
    function GetCurrent: IFoundJPEGSegment;
    function GetEnumerator: IJPEGHeaderParser;
    function MoveNext: Boolean;
    property Current: IFoundJPEGSegment read GetCurrent;
  end;

const
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

type
  TJPEGSegment = record const
    HeaderSize = 4;
    AllMarkers = [Low(TJPEGMarker)..High(TJPEGMarker)];
    AnyMarker = AllMarkers;
    MarkersWithNoData = [$00, $01, $D0..$D9];
    StartOfFrameMarkers  = [jmStartOfFrame0..jmStartOfFrame3,
      jmStartOfFrame5..jmStartOfFrame7]; //there's no jmStartOfFrame4
    ExifHeader: array[0..5] of AnsiChar = 'Exif'#0#0;
    XMPHeader: array[0..28] of AnsiChar = 'http://ns.adobe.com/xap/1.0/'#0;
  end;

const
  AnyJPEGMarker = TJPEGSegment.AnyMarker;
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
function JPEGHeader(JPEGStream: TStream; const MarkersToLookFor: TJPEGMarkers = TJPEGSegment.AnyMarker;
  StreamOwnership: TStreamOwnership = soReference): IJPEGHeaderParser; overload;
function JPEGHeader(const JPEGFile: string;
  const MarkersToLookFor: TJPEGMarkers = TJPEGSegment.AnyMarker): IJPEGHeaderParser; overload; inline;
function JPEGHeader(const JPEGImage: IStreamPersist; //!!!changed from TGraphic
  const MarkersToLookFor: TJPEGMarkers = TJPEGSegment.AnyMarker): IJPEGHeaderParser; overload;

function HasJPEGHeader(Stream: TStream): Boolean; overload;
function HasJPEGHeader(const FileName: string): Boolean; overload;

function GetJPEGDataSize(Data: TStream; ReturnZeroIfFindNoEOI: Boolean = False): Int64; overload;
function GetJPEGDataSize(const JPEGImage: IStreamPersist): Int64; overload;

function RemoveJPEGSegments(const JPEGFile: string; Markers: TJPEGMarkers): TJPEGMarkers; overload;
function RemoveJPEGSegments(const JPEGImage: IStreamPersist; Markers: TJPEGMarkers): TJPEGMarkers; overload;

function CreateAdobeApp13Segment(const Blocks: array of IAdobeResBlock): IJPEGSegment; overload;
function CreateAdobeApp13Segment(const Blocks: IInterfaceList = nil): IJPEGSegment; overload;

procedure UpdateApp1JPEGSegments(InStream, OutStream: TStream;
  NewExifSource, NewXMPSource: IStreamPersistEx); //used by the SaveToStream methods of TExifData and TXMPPacket; if NewXXXSource is nil, the data in InStream is copied over

procedure WriteJPEGFileHeader(Stream: TStream); inline;
procedure WriteJPEGSegment(Stream: TStream; MarkerNum: TJPEGMarker;
  const Data; DataSize: Word); overload;
procedure WriteJPEGSegment(Stream: TStream; MarkerNum: TJPEGMarker;
  Data: TStream; DataSize: Word = 0); overload;
procedure WriteJPEGSegment(Stream: TStream; Segment: IJPEGSegment); overload;

{ TIFF bits - the rest is in CCR.Exif.TiffUtils.pas }

const
  TiffSmallEndianCode = Word($4949);
  TiffBigEndianCode = Word($4D4D);
  TiffMagicNum = Word($002A);
  TiffMagicNumBigEndian = Word($2A00);

implementation

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}{$IFDEF POSIX}Posix.Unistd,{$ENDIF}
  {$IFDEF HasIOUtils}IOUtils,{$ENDIF}Math, RTLConsts,
  CCR.Exif.Consts, CCR.Exif.TagIDs;

type
  TAdobeBlock = class(TMetadataBlock, IStreamPersist, IAdobeResBlock)
  strict private
    FLoadedCleanly: Boolean;
    FSignature: AnsiString;
    FTypeID: Word;
    FName: AnsiString;
    class function GetValidSignature(const S: AnsiString): AnsiString; static;
  protected
    procedure Clear;
    function HasExifBlockID: Boolean; override;
    function HasIPTCBlockID: Boolean; override;
    function HasXMPBlockID: Boolean; override;
    { IStreamPersist}
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    { IAdobeResBlock}
    function GetSignature: AnsiString;
    procedure SetSignature(const Value: AnsiString);
    function GetTypeID: Word;
    procedure SetTypeID(Value: Word);
    function GetName: AnsiString;
    procedure SetName(const Value: AnsiString);
    function GetTotalSize: Integer;
    function GetLoadedCleanly: Boolean;
    function HasIPTCData: Boolean;
  public
    constructor Create; overload;
    constructor Create(AStream: TStream); overload;
    constructor Create(const ASignature: AnsiString; ATypeID: Word;
      const AName: AnsiString; const ADataSource: IStreamPersist); overload;
  end;

  TAdobeResBlockEnumerator = class(TInterfacedObject, IAdobeResBlockEnumerator)
  strict private
    FCurrent: IAdobeResBlock;
    FNextPos, FPosTooFar: Int64;
    FStream: TStream;
    FStreamOwnership: TStreamOwnership;
  protected
    function GetCurrent: IAdobeResBlock;
    property NextPos: Int64 read FNextPos write FNextPos;
    property PosTooFar: Int64 read FPosTooFar write FPosTooFar;
  public
    constructor Create(AStream: TStream; AStreamOwnership: TStreamOwnership = soReference);
    destructor Destroy; override;
    function MoveNext: Boolean;
    property Current: IAdobeResBlock read FCurrent;
    property Stream: TStream read FStream;
  end;

  TFoundJPEGSegment = class(TUserJPEGSegment, IFoundJPEGSegment)
  strict private
    FOffset: Int64;
  protected
    { IFoundJPEGSegment }
    function GetOffset: Int64;
    function GetOffsetOfData: Int64;
    function GetTotalSize: Integer;
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

function IsCharIn(Ch: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := Ch in CharSet;
end;

function IsCharIn(Ch: WideChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := (Ch <= High(AnsiChar)) and (AnsiChar(Ch) in CharSet);
end;

{$IFNDEF UNICODE}
function UpCase(const Ch: AnsiChar): AnsiChar;
begin
  Result := Ch;
  case Result of
    'a'..'z':  Dec(Result, Ord('a') - Ord('A'));
  end;
end;

function UpCase(const Ch: WideChar): WideChar;
begin
  Result := Ch;
  case Result of
    'a'..'z':  Dec(Result, Ord('a') - Ord('A'));
  end;
end;

function UTF8ToString(const UTF8: RawByteString): string;
begin
  Result := Utf8ToAnsi(UTF8);
end;
{$ENDIF}

function UTF8ToString(UTF8Buffer: Pointer; ByteLen: Integer): string;
var
  S: UTF8String;
begin
  SetString(S, PAnsiChar(UTF8Buffer), ByteLen);
  Result := UTF8ToString(S);
end;

function BinToHexStr(Data: Pointer; Size: Integer): string;
const
  Map: array[0..15] of Char = (#$30, #$31, #$32, #$33, #$34, #$35,
    #$36, #$37, #$38, #$39, #$41, #$42, #$43, #$44, #$45, #$46);
var
  Buffer: PByteArray absolute Data;
  I: Integer;
begin
  SetString(Result, nil, 0);
  if Size = 0 then Exit;
  SetString(Result, nil, Size * 2);
  for I := 0 to Size - 1 do
  begin
    Result[I * 2 + 1] := Map[Buffer[I] shr 4];
    Result[I * 2 + 2] := Map[Buffer[I] and $0F];
  end;
end;

function BinToHexStr(const Buffer; Size: Integer): string;
begin
  Result := BinToHexStr(@Buffer, Size);
end;

function IsValidHexChar(Ch: Char): Boolean;
begin
  case Ch of
    '0'..'9','A'..'F','a'..'f': Result := True;
  else Result := False;
  end;
end;

function HexStrToBin(const Text: string): TBytes;
const
  Map: array['0'..'f'] of SmallInt =
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15);
var
  Ch1, Ch2: Char;
  I: Integer;
begin
  SetLength(Result, Length(Text) div 2);
  for I := 0 to High(Result) do
  begin
    Ch1 := Text[I * 2 + 1];
    Ch2 := Text[I * 2 + 2];
    if not IsValidHexChar(Ch1) or not IsValidHexChar(Ch2) then
      raise EInvalidOperation.CreateFmt(SInvalidHexString, [Text]);
    Result[I] := (Map[Ch1] shl 4) + Map[Ch2];
  end;
end;

{$IF Declared(TPath)}
function GetTempFileName: string; inline;
begin
  Result := TPath.GetTempFileName;
end;
{$ELSE}
function GetTempFileName: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if (Windows.GetTempPath(MAX_PATH, Buffer) = 0) or (Windows.GetTempFileName(Buffer,
    'tmp', 0, Buffer) = 0) then
    RaiseLastOSError
  else
    Result := Buffer;
end;
{$IFEND}

procedure DoSaveToGraphic(GraphicStream: TStream;
  const DetectTypeFunc: TGetGraphicSaveMethod); overload;
var
  SaveMethod: TGraphicSaveMethod;
  TempFileName: string;
  TempStream: TStream;
begin
  GraphicStream.Seek(0, soBeginning);
  TempFileName := '';
  TempStream := nil;
  try
    SaveMethod := nil;
    DetectTypeFunc(GraphicStream, SaveMethod);
    if not Assigned(SaveMethod) then
      raise EUnsupportedGraphicFormat.CreateRes(@SUnsupportedGraphicFormat);
    if GraphicStream.Size < $FFFFFF then //If the source data is under 16MB, buffer to
      TempStream := TMemoryStream.Create //memory, else buffer to a temporary file.
    else
    begin
      TempFileName := GetTempFileName;
      TempStream := TFileStream.Create(TempFileName, fmCreate);
    end;
    SaveMethod(GraphicStream, TempStream);
    GraphicStream.Seek(0, soBeginning);
    if TempStream is TMemoryStream then
      GraphicStream.WriteBuffer(TMemoryStream(TempStream).Memory^, TempStream.Size)
    else
      GraphicStream.CopyFrom(TempStream, 0);
    GraphicStream.Size := TempStream.Size;
  finally
     FreeAndNil(TempStream);
    if TempFileName <> '' then SysUtils.DeleteFile(TempFileName);
  end;
end;

procedure DoSaveToGraphic(const FileName: string;
  const DetectTypeFunc: TGetGraphicSaveMethod);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenReadWrite);
  try
    DoSaveToGraphic(Stream, DetectTypeFunc);
  finally
     FreeAndNil(Stream);
  end;
end;

procedure DoSaveToGraphic(const Graphic: IStreamPersist;
  const DetectTypeFunc: TGetGraphicSaveMethod);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Graphic.SaveToStream(Stream);
    DoSaveToGraphic(Stream, DetectTypeFunc);
    Stream.Seek(0, soBeginning);
    Graphic.LoadFromStream(Stream);
  finally
     FreeAndNil(Stream);
  end;
end;

{ TNoRefCountInterfacedObject }

function TNoRefCountInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TNoRefCountInterfacedObject._AddRef: Integer; stdcall;
begin
  Result := -1;
end;

function TNoRefCountInterfacedObject._Release: Integer; stdcall;
begin
  Result := -1;
end;

{ TMetadataBlock.TDataStream }

destructor TMetadataBlock.TDataStream.Destroy;
begin
  FDisableChanges := False;
  inherited;
end;

procedure TMetadataBlock.TDataStream.SetSize(NewSize: Longint);
begin
  if DisableChanges then
    raise EStreamError.CreateRes(@SStreamIsReadOnly);
  inherited SetSize(NewSize);
end;

{$IF CompilerVersion < 35}
function TMetadataBlock.TDataStream.Realloc(var NewCapacity: Longint): Pointer;
{$ELSE}
function TMetadataBlock.TDataStream.Realloc(var NewCapacity: NativeInt): Pointer;
{$IFEND}
begin
  if DisableChanges then
    raise EStreamError.CreateRes(@SStreamIsReadOnly)
  else
    Result := inherited Realloc(NewCapacity);
end;

procedure TMetadataBlock.TDataStream.SetSize(const NewSize: Int64);
begin
  if DisableChanges then
    raise EStreamError.CreateRes(@SStreamIsReadOnly);
  {
  if NewSize > 16000000
  then NewSize:= 16000000;
  //This will catch "out of memory" for images with broken exif
  //c:\projects\BIONIX\Wallpapers\Testing set\corrupted\Broken exif (out of mem).jpg }

  inherited SetSize(NewSize);
end;

function TMetadataBlock.TDataStream.Write(const Buffer; Count: Longint): Longint;
begin
  if DisableChanges then
    raise EStreamError.CreateRes(@SStreamIsReadOnly);
  Result := inherited Write(Buffer, Count);
end;

{ TMetadataBlock }

constructor TMetadataBlock.Create(const DataSource: IStreamPersist = nil);
begin
  inherited Create;
  FData := TDataStream.Create;
  if DataSource <> nil then DataSource.SaveToStream(FData);
end;

constructor TMetadataBlock.Create(DataSource: TStream; BytesToRead: Integer);
begin
  Create;
  if BytesToRead > 0 then FData.CopyFrom(DataSource, BytesToRead);
end;

destructor TMetadataBlock.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

function TMetadataBlock.GetData: TCustomMemoryStream;
begin
  Result := FData;
end;

function TMetadataBlock.HasExifBlockID: Boolean;
begin
  Result := False;
end;

function TMetadataBlock.HasIPTCBlockID: Boolean;
begin
  Result := False;
end;

function TMetadataBlock.HasXMPBlockID: Boolean;
begin
  Result := False;
end;

function TMetadataBlock.IsExifBlock(CheckID: Boolean = True): Boolean;
var
  Words: PWordArray;
begin
  Result := False;
  if (CheckID and not HasExifBlockID) or (FData.Size < 8) then Exit;
  Words := FData.Memory;
  if CompareMem(Words, @TJPEGSegment.ExifHeader, SizeOf(TJPEGSegment.ExifHeader)) then //TExifData will just skip any Exif segment header
    Inc(PByte(Words), SizeOf(TJPEGSegment.ExifHeader));
  if (Words[0] = TiffSmallEndianCode) or (Words[0] = TiffBigEndianCode) then
    case Words[1] of
      TiffMagicNum, TiffMagicNumBigEndian: Result := True; //ParseTiff doesn't mind if these are confused
    end;
end;

function TMetadataBlock.IsIPTCBlock(CheckID: Boolean = True): Boolean;
var
  Header: TIPTCTagInfo;
begin
  FData.Seek(0, soFromBeginning);
  Result := (not CheckID or HasIPTCBlockID) and
    TAdobeResBlock.TryReadIPTCHeader(FData, Header, True);
end;

function TMetadataBlock.IsXMPBlock(CheckID: Boolean = True): Boolean;
const
  XMPStart: PAnsiChar = '<?xpacket ';
  XMPStartLen = 10;
var
  SeekPtr: PAnsiChar;
begin
  Result := False;
  if (CheckID and not HasXMPBlockID) or (FData.Size < 20) then Exit;
  SeekPtr := FData.Memory;
  if CompareMem(SeekPtr, @TJPEGSegment.XMPHeader, SizeOf(TJPEGSegment.XMPHeader)) then
    Inc(SeekPtr, SizeOf(TJPEGSegment.XMPHeader));
  Result := CompareMem(SeekPtr, XMPStart, XMPStartLen);
end;

{ TDateTimeTagValue }

constructor TDateTimeTagValue.CreateFromString(const AString: string);
begin
  if not TryStrToDateTime(AString, FValue) then
    FValue := MissingOrInvalidValue;
end;

class function TDateTimeTagValue.CreateMissingOrInvalid: TDateTimeTagValue;
begin
  Result.FValue := MissingOrInvalidValue;
end;

function TDateTimeTagValue.MissingOrInvalid: Boolean;
begin
  Result := (Value < FirstValidDateTime);
end;

function TDateTimeTagValue.ToString: string;
begin
  if MissingOrInvalid then
    Result := ''
  else
    Result := DateTimeToStr(Value);
end;

function TDateTimeTagValue.ToString(const Format: string): string;
begin
  if MissingOrInvalid then
    Result := ''
  else
    DateTimeToString(Result, Format, Value);
end;

{$IFDEF HasFormatSettings}
function TDateTimeTagValue.ToString(const Format: string; const Settings: TFormatSettings): string;
begin
  if MissingOrInvalid then
    Result := ''
  else
    DateTimeToString(Result, Format, Value, Settings);
end;
{$ENDIF}

class operator TDateTimeTagValue.Equal(const A, B: TDateTimeTagValue): Boolean;
begin
  if A.MissingOrInvalid then
    Result := B.MissingOrInvalid
  else
    Result := (A.Value = B.Value);
end;

class operator TDateTimeTagValue.NotEqual(const A, B: TDateTimeTagValue): Boolean;
begin
  Result := not (A = B);
end;

class operator TDateTimeTagValue.Implicit(const Source: TDateTimeTagValue): TDateTime;
begin
  Result := Source.Value;
end;

class operator TDateTimeTagValue.Implicit(const Source: TDateTime): TDateTimeTagValue;
begin
  Result.FValue := Source;
end;

class operator TDateTimeTagValue.LessThan(const A, B: TDateTimeTagValue): Boolean;
begin
  Result := (A.Value < B.Value) and not A.MissingOrInvalid and not B.MissingOrInvalid;
end;

class operator TDateTimeTagValue.LessThanOrEqual(const A, B: TDateTimeTagValue): Boolean;
begin
  Result := (A.MissingOrInvalid and B.MissingOrInvalid) or
    ((A.Value <= B.Value) and not A.MissingOrInvalid and not B.MissingOrInvalid);
end;

class operator TDateTimeTagValue.GreaterThan(const A, B: TDateTimeTagValue): Boolean;
begin
  Result := (A.Value > B.Value) and not A.MissingOrInvalid and not B.MissingOrInvalid;
end;

class operator TDateTimeTagValue.GreaterThanOrEqual(const A, B: TDateTimeTagValue): Boolean;
begin
  Result := (A.MissingOrInvalid and B.MissingOrInvalid) or
    ((A.Value >= B.Value) and not A.MissingOrInvalid and not B.MissingOrInvalid);
end;

{ TLongIntTagValue }

constructor TLongIntTagValue.CreateFromString(const AString: string);
begin
  if TryStrToInt(AString, FValue) then
    FMissingOrInvalid := False
  else
    Self := CreateMissingOrInvalid;
end;

class function TLongIntTagValue.CreateMissingOrInvalid;
begin
  Result.FMissingOrInvalid := True;
  Result.FValue := 0;
end;

function TLongIntTagValue.ToString: string;
begin
  if FMissingOrInvalid then
    Result := ''
  else
    Result := IntToStr(FValue);
end;

class operator TLongIntTagValue.Equal(const A, B: TLongIntTagValue): Boolean;
begin
  Result := (A.Value = B.Value) and (A.MissingOrInvalid = B.MissingOrInvalid);
end;

class operator TLongIntTagValue.Implicit(const Source: TLongIntTagValue): Int64;
begin
  Result := Source.Value;
end;

class operator TLongIntTagValue.Implicit(const Source: TLongIntTagValue): LongInt;
begin
  Result := Source.Value;
end;

class operator TLongIntTagValue.Implicit(const Source: LongInt): TLongIntTagValue;
begin
  Result.FValue := Source;
  Result.FMissingOrInvalid := False;
end;

class operator TLongIntTagValue.LessThan(const A, B: TLongIntTagValue): Boolean;
begin
  Result := (A.Value < B.Value) and not A.MissingOrInvalid and not B.MissingOrInvalid;
end;

class operator TLongIntTagValue.LessThanOrEqual(const A, B: TLongIntTagValue): Boolean;
begin
  Result := (A.MissingOrInvalid and B.MissingOrInvalid) or
    ((A.Value <= B.Value) and not A.MissingOrInvalid and not B.MissingOrInvalid);
end;

class operator TLongIntTagValue.NotEqual(const A, B: TLongIntTagValue): Boolean;
begin
  Result := not (A = B);
end;

class operator TLongIntTagValue.GreaterThan(const A, B: TLongIntTagValue): Boolean;
begin
  Result := (A.Value > B.Value) and not A.MissingOrInvalid and not B.MissingOrInvalid;
end;

class operator TLongIntTagValue.GreaterThanOrEqual(const A, B: TLongIntTagValue): Boolean;
begin
  Result := (A.MissingOrInvalid and B.MissingOrInvalid) or
    ((A.Value >= B.Value) and not A.MissingOrInvalid and not B.MissingOrInvalid);
end;

class operator TLongIntTagValue.Negative(const Source: TLongIntTagValue): TLongIntTagValue;
begin
  if Source.MissingOrInvalid then
    Result := Source
  else
    Result := -Source.Value;
end;

{ TLongWordTagValue }

constructor TLongWordTagValue.CreateFromString(const AString: string);
var
  Int64Val: Int64;
begin
  if TryStrToInt64(AString, Int64Val) and (Int64Val >= 0) and (Int64Val <= High(LongWord)) then
  begin
    FValue := Int64Val;
    FMissingOrInvalid := False;
  end
  else
    Self := CreateMissingOrInvalid;
end;

class function TLongWordTagValue.CreateMissingOrInvalid;
begin
  Result.FMissingOrInvalid := True;
  Result.FValue := 0;
end;

class operator TLongWordTagValue.Equal(const A, B: TLongWordTagValue): Boolean;
begin
  Result := (A.Value = B.Value) and (A.MissingOrInvalid = B.MissingOrInvalid);
end;

function TLongWordTagValue.ToString: string;
begin
  if FMissingOrInvalid then
    Result := ''
  else
    Result := IntToStr(FValue);
end;

class operator TLongWordTagValue.GreaterThan(const A, B: TLongWordTagValue): Boolean;
begin
  Result := (A.Value > B.Value) and not B.MissingOrInvalid; //don't need check for A.MissingOrInvalid
end;

class operator TLongWordTagValue.GreaterThanOrEqual(const A, B: TLongWordTagValue): Boolean;
begin
  Result := (A.MissingOrInvalid and B.MissingOrInvalid) or
    ((A.Value >= B.Value) and not A.MissingOrInvalid and not B.MissingOrInvalid);
end;

class operator TLongWordTagValue.Implicit(const Source: TLongWordTagValue): Int64;
begin
  Result := Source.Value;
end;

class operator TLongWordTagValue.Implicit(const Source: TLongWordTagValue): UInt64;
begin
  Result := Source.Value;
end;

class operator TLongWordTagValue.Implicit(const Source: TLongWordTagValue): LongWord;
begin
  Result := Source.Value;
end;

class operator TLongWordTagValue.Implicit(const Source: LongWord): TLongWordTagValue;
begin
  Result.FValue := Source;
  Result.FMissingOrInvalid := False;
end;

class operator TLongWordTagValue.LessThan(const A, B: TLongWordTagValue): Boolean;
begin
  Result := (A.Value < B.Value) and not A.MissingOrInvalid;
end;

class operator TLongWordTagValue.LessThanOrEqual(const A, B: TLongWordTagValue): Boolean;
begin
  Result := (A.MissingOrInvalid and B.MissingOrInvalid) or
    ((A.Value <= B.Value) and not A.MissingOrInvalid and not B.MissingOrInvalid);
end;

class operator TLongWordTagValue.NotEqual(const A, B: TLongWordTagValue): Boolean;
begin
  Result := not (A = B);
end;

{ TWordTagValue }

constructor TWordTagValue.CreateFromString(const AString: string);
var
  IntVal: Integer;
begin
  if TryStrToInt(AString, IntVal) and (IntVal >= 0) and (IntVal <= High(Word)) then
  begin
    FValue := IntVal;
    FMissingOrInvalid := False;
  end
  else
    Self := CreateMissingOrInvalid;
end;

class function TWordTagValue.CreateMissingOrInvalid;
begin
  Result.FMissingOrInvalid := True;
  Result.FValue := 0;
end;

class operator TWordTagValue.Equal(const A, B: TWordTagValue): Boolean;
begin
  Result := (A.Value = B.Value) and (A.MissingOrInvalid = B.MissingOrInvalid);
end;

function TWordTagValue.ToString: string;
begin
  if FMissingOrInvalid then
    Result := ''
  else
    Result := IntToStr(FValue);
end;

class operator TWordTagValue.GreaterThan(const A, B: TWordTagValue): Boolean;
begin
  Result := (A.Value > B.Value) and not B.MissingOrInvalid; //don't need check for A.MissingOrInvalid
end;

class operator TWordTagValue.GreaterThanOrEqual(const A, B: TWordTagValue): Boolean;
begin
  Result := (A.MissingOrInvalid and B.MissingOrInvalid) or
    ((A.Value >= B.Value) and not A.MissingOrInvalid and not B.MissingOrInvalid);
end;

class operator TWordTagValue.Implicit(const Source: TWordTagValue): TLongIntTagValue;
begin
  if Source.MissingOrInvalid then
    Result := TLongIntTagValue.CreateMissingOrInvalid
  else
    Result := Source.Value;
end;

class operator TWordTagValue.Implicit(const Source: TWordTagValue): TLongWordTagValue;
begin
  if Source.MissingOrInvalid then
    Result := TLongWordTagValue.CreateMissingOrInvalid
  else
    Result := Source.Value;
end;

class operator TWordTagValue.Implicit(const Source: TWordTagValue): Int64;
begin
  Result := Source.Value;
end;

class operator TWordTagValue.Implicit(const Source: TWordTagValue): UInt64;
begin
  Result := Source.Value;
end;

class operator TWordTagValue.Implicit(const Source: TWordTagValue): Integer;
begin
  Result := Source.Value;
end;

class operator TWordTagValue.Implicit(const Source: TWordTagValue): Word;
begin
  Result := Source.Value;
end;

class operator TWordTagValue.Implicit(const Source: Word): TWordTagValue;
begin
  Result.FValue := Source;
  Result.FMissingOrInvalid := False;
end;

class operator TWordTagValue.LessThan(const A, B: TWordTagValue): Boolean;
begin
  Result := (A.Value < B.Value) and not A.MissingOrInvalid;
end;

class operator TWordTagValue.LessThanOrEqual(const A, B: TWordTagValue): Boolean;
begin
  Result := (A.MissingOrInvalid and B.MissingOrInvalid) or
    ((A.Value <= B.Value) and not A.MissingOrInvalid and not B.MissingOrInvalid);
end;

class operator TWordTagValue.NotEqual(const A, B: TWordTagValue): Boolean;
begin
  Result := not (A = B);
end;

{ TSmallPointHelper }

class function TSmallPointHelper.CreateMissingOrInvalid: TSmallPoint;
begin
  Result.x := -1;
  Result.y := -1;
end;

function TSmallPointHelper.MissingOrInvalid: Boolean;
begin
  Result := InvalidPoint(Self);
end;

{ TUserMemoryStream }

constructor TUserMemoryStream.Create(Memory: Pointer; Size: Integer);
begin
  inherited Create;
  SetPointer(Memory, Size);
end;

procedure TUserMemoryStream.ChangeMemory(NewMemory: Pointer; NewSize: Integer);
begin
  SetPointer(NewMemory, NewSize);
  Position := 0;
end;

procedure TUserMemoryStream.SetSize(NewSize: Longint);
begin
  raise EStreamError.CreateRes(@SStreamIsReadOnly);
end;

procedure TUserMemoryStream.SetSize(const NewSize: Int64);
begin
  raise EStreamError.CreateRes(@SStreamIsReadOnly);
end;

function TUserMemoryStream.Write(const Buffer; Count: Integer): Integer;
begin
  raise EStreamError.CreateRes(@SStreamIsReadOnly);
end;

{ TAdobeResBlock }

class function TAdobeResBlock.TryReadIPTCHeader(Stream: TStream;
  var TagInfo: TIPTCTagInfo; AlwaysResetStreamPos: Boolean = False): Boolean;
var           //IPTC data is made up of a continuous stream of tags that just begin.
  Header: packed record
    StartMarker: Byte;
    RecID: Byte;
    TagID: Byte;
    DataSize: SmallInt;
  end;
  ByteVal: Byte;
  WordVal: Word;
  LongIntVal: LongInt;
begin
  Result := False;
  if not Stream.TryReadBuffer(Header, SizeOf(Header)) then Exit;
  if (Header.StartMarker = NewIPTCTagMarker) and (Header.RecID in [Low(TIPTCSectionID)..High(TIPTCSectionID)]) then
  begin
    Result := True;
    Header.DataSize := Swap(Header.DataSize); //big endian
    if Header.DataSize >= 0 then
      TagInfo.DataSize := Header.DataSize
    else
      case Header.DataSize of
        -1: if Stream.ReadByte(ByteVal) then
              TagInfo.DataSize := ByteVal
            else
              Result := False;
        -2: if Stream.ReadWord(BigEndian, WordVal) then
              TagInfo.DataSize := WordVal
            else
              Result := False;
        -4: if Stream.ReadLongInt(BigEndian, LongIntVal) then
              TagInfo.DataSize := LongIntVal
            else
              Result := False;
      else Result := False;
      end;
  end;
  if Result then
  begin
    TagInfo.SectionID := Header.RecID;
    TagInfo.TagID := Header.TagID;
  end;
  if AlwaysResetStreamPos or not Result then
    Stream.Seek(-SizeOf(Header), soCurrent);
end;

{ TAdobeBlock }

class function TAdobeBlock.GetValidSignature(const S: AnsiString): AnsiString;
begin
  case Length(S) of
    0..3: Result := S + StringOfChar(AnsiChar(' '), 4 - Length(S));
    4: Result := S;
  else Result := Copy(S, 1, 4);
  end;
end;

constructor TAdobeBlock.Create;
begin
  inherited Create;
  Clear;
end;

constructor TAdobeBlock.Create(AStream: TStream);
begin
  inherited Create;
  LoadFromStream(AStream)
end;

constructor TAdobeBlock.Create(const ASignature: AnsiString; ATypeID: Word;
  const AName: AnsiString; const ADataSource: IStreamPersist);
begin
  inherited Create;
  FLoadedCleanly := True;
  FSignature := GetValidSignature(ASignature);
  FTypeID := ATypeID;
  FName := AName;
  if ADataSource <> nil then ADataSource.SaveToStream(Data);
end;

procedure TAdobeBlock.Clear;
begin
  Data.Clear;
  FSignature := GetValidSignature('');
  FTypeID := 0;
  FName := '';
  FLoadedCleanly := True;
end;

function TAdobeBlock.GetLoadedCleanly: Boolean;
begin
  Result := FLoadedCleanly;
end;

function TAdobeBlock.GetName: AnsiString;
begin
  Result := FName;
end;

function TAdobeBlock.GetSignature: AnsiString;
begin
  Result := FSignature;
end;

function TAdobeBlock.GetTotalSize: Integer; //!!!test
begin
  Result := 11 + Length(FName) + Data.Size;
  if not Odd(Length(FName)) then Inc(Result);
  if Odd(Result) then Inc(Result);
end;

function TAdobeBlock.GetTypeID: Word;
begin
  Result := FTypeID;
end;

function TAdobeBlock.HasExifBlockID: Boolean;
begin
  Result := (FTypeID = TAdobeResBlock.ExifTypeID);
end;

function TAdobeBlock.HasIPTCBlockID: Boolean;
begin
  Result := (FTypeID = TAdobeResBlock.IPTCTypeID);
end;

function TAdobeBlock.HasIPTCData: Boolean;
begin
  Result := IsIPTCBlock;
end;

function TAdobeBlock.HasXMPBlockID: Boolean;
begin
  Result := (FTypeID = TAdobeResBlock.XMPTypeID);
end;

procedure TAdobeBlock.LoadFromStream(Stream: TStream);
var                 //Method amended in trunk r.14 to avoid exceptions on some malformed
  Len: Integer;     //data, implementing LoadedCleanly property at the same time.
  Header: packed record
    Signature: array[0..3] of AnsiChar;
    BigEndianTypeID: Word;
    NameLen: Byte;
  end;
begin
  Clear;
  FLoadedCleanly := False;
  if not Stream.TryReadBuffer(Header, SizeOf(Header)) then Exit;
  SetString(FSignature, Header.Signature, 4);
  FTypeID := Swap(Header.BigEndianTypeID);
  SetString(FName, nil, Header.NameLen);
  if Header.NameLen <> 0 then
    if not Stream.TryReadBuffer(Pointer(FName)^, Header.NameLen) then Exit;
  if not Odd(Header.NameLen) then
    if Stream.Read(Len, 1) <> 1 then Exit;
  if not Stream.ReadLongInt(BigEndian, Len) or (Len < 0) then Exit;
  if Len <> 0 then
  begin
    Data.SetSize(Len);
    if not Stream.TryReadBuffer(Data.Memory^, Len) then Exit;
    if Odd(Len) then
      if Stream.Read(Len, 1) <> 1 then Exit;
  end;
  FLoadedCleanly := True;
end;

procedure TAdobeBlock.SaveToStream(Stream: TStream);
var
  Len: Integer;
begin
  Assert(Length(FSignature) = 4);
  Stream.WriteBuffer(Pointer(FSignature)^, 4);
  Stream.WriteWord(FTypeID, BigEndian);
  Len := Length(FName);
  if Len > High(Byte) then Len := High(Byte);
  Stream.WriteByte(Len);
  if Len <> 0 then Stream.WriteBuffer(Pointer(FName)^, Len);
  if not Odd(Len) then Stream.WriteByte(0);
  Len := Data.Size;
  Stream.WriteLongInt(Len, BigEndian);
  if Len <> 0 then Stream.WriteBuffer(Data.Memory^, Len);
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

{ TAdobeResBlockEnumerator }

constructor TAdobeResBlockEnumerator.Create(AStream: TStream;
  AStreamOwnership: TStreamOwnership);
begin
  inherited Create;
  FStream := AStream;
  FStreamOwnership := AStreamOwnership;
  if AStream = nil then Exit;
  FNextPos := AStream.Position;
  FPosTooFar := AStream.Size;
end;

destructor TAdobeResBlockEnumerator.Destroy;
begin
  if FStreamOwnership = soOwned then //If loop didn't complete, MoveNext
    FreeAndNil(FStream);             //wouldn't have had the chance to
  inherited;                         //free the stream.
end;

function TAdobeResBlockEnumerator.GetCurrent: IAdobeResBlock;
begin
  Result := FCurrent;
end;

function TAdobeResBlockEnumerator.MoveNext: Boolean;
begin
  FCurrent := nil;
  Result := (FStream <> nil) and (FNextPos < FPosTooFar - 12);
  if not Result then
  begin
    if FStreamOwnership = soOwned then
      FreeAndNil(FStream);
    Exit;
  end;
  FStream.Position := FNextPos;
  FCurrent := TAdobeBlock.Create(FStream);
  if FCurrent.LoadedCleanly then
    FNextPos := FStream.Position
  else
    FNextPos := FPosTooFar;
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
  inherited Create;
  FMarkerNum := AMarkerNum;
  if ASource <> nil then
  begin
    ASource.SaveToStream(Data);
    Data.Position := 0;
  end;
end;

function TUserJPEGSegment.GetEmpty: Boolean;
begin
  Result := (Data.Size = 0);
end;

function TUserJPEGSegment.GetEnumerator: IAdobeResBlockEnumerator;
begin
  if not IsAdobeApp13 then
    Result := TAdobeResBlockEnumerator.Create(nil)
  else
  begin
    Data.Position := FAdobeHeaderSize;
    Result := TAdobeResBlockEnumerator.Create(Data)
  end;
end;

function TUserJPEGSegment.GetMarkerNum: TJPEGMarker;
begin
  Result := FMarkerNum;
end;

function TUserJPEGSegment.HasExifBlockID: Boolean;
begin
  Result := (FMarkerNum = jmApp1);
end;

function TUserJPEGSegment.HasIPTCBlockID: Boolean;
begin
  Result := (FMarkerNum = jmApp13);
end;

function TUserJPEGSegment.HasXMPBlockID: Boolean;
begin
  Result := (FMarkerNum = jmApp1);
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

function TUserJPEGSegment.IsIPTCBlock(CheckID: Boolean = True): Boolean;
var
  ResBlock: IAdobeResBlock;
begin
  Result := True;
  for ResBlock in Self do
    if ResBlock.IsIPTCBlock(CheckID) then Exit;
  Result := False;
end;

procedure TUserJPEGSegment.LoadFromStream(Stream: TStream);
begin
  Data.Clear;
  repeat
    FMarkerNum := Stream.ReadByte;
  until (FMarkerNum <> jmNewOrPadding); //extra $FF bytes are legal as padding
  if not (FMarkerNum in TJPEGSegment.MarkersWithNoData) then
  begin
    Data.SetSize(Stream.ReadWord(BigEndian) - 2);
    if Stream.Read(Data.Memory^, Data.Size) <> Data.Size then
    begin
      Data.Clear;
      raise EReadError.CreateRes(@SReadError);
    end;
  end;
end;

procedure TUserJPEGSegment.SaveToStream(Stream: TStream);
begin
  WriteJPEGSegment(Stream, MarkerNum, Data);
end;

{ TFoundJPEGSegment }

constructor TFoundJPEGSegment.Create(AMakerNum: TJPEGMarker; ASource: TStream;
  ADataSize: Integer);
begin
  inherited Create(AMakerNum);
  FOffset := ASource.Position - SizeOf(TJPEGSegmentHeader);
  if AMakerNum in TJPEGSegment.MarkersWithNoData then
    Inc(FOffset, 2)
  else if ADataSize > 0 then
  begin
    Data.SetSize(ADataSize);
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

function TFoundJPEGSegment.GetTotalSize: Integer;
begin
  Result := Data.Size + SizeOf(TJPEGSegmentHeader);
  if MarkerNum in TJPEGSegment.MarkersWithNoData then Dec(Result, 2);
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
    raise EInvalidJPEGHeader.CreateRes(@SInvalidJPEGHeader);
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
    if FLastMarker in TJPEGSegment.MarkersWithNoData then
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
  StreamOwnership: TStreamOwnership): IJPEGHeaderParser; overload;
begin
  Result := TJPEGHeaderParser.Create(JPEGStream, MarkersToLookFor, StreamOwnership);
end;

function JPEGHeader(const JPEGFile: string;
  const MarkersToLookFor: TJPEGMarkers): IJPEGHeaderParser;
begin
  Result := JPEGHeader(TFileStream.Create(JPEGFile, fmOpenRead or fmShareDenyWrite), MarkersToLookFor,
    soOwned);
end;

function JPEGHeader(const JPEGImage: IStreamPersist;
  const MarkersToLookFor: TJPEGMarkers): IJPEGHeaderParser;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  JPEGImage.SaveToStream(Stream);
  Stream.Position := 0;
  Result := JPEGHeader(Stream, MarkersToLookFor, soOwned);
end;

function HasJPEGHeader(Stream: TStream): Boolean; overload;
begin
  Result := Stream.TryReadHeader(JPEGFileHeader, SizeOf(JPEGFileHeader), True);
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

function GetJPEGDataSize(Data: TStream; ReturnZeroIfFindNoEOI: Boolean = False): Int64; overload;
var
  OrigPos: Int64;
  Segment: IFoundJPEGSegment;
begin
  OrigPos := Data.Position;
  if ReturnZeroIfFindNoEOI then
    Result := 0
  else
    Result := Data.Size - OrigPos;
  for Segment in JPEGHeader(Data, [jmEndOfImage]) do
    Result := Data.Position - OrigPos;
  Data.Position := OrigPos;
end;

function GetJPEGDataSize(const JPEGImage: IStreamPersist): Int64; overload;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    JPEGImage.SaveToStream(Stream); //TJPEGImage.LoadFromStream keeps everything from the starting pos
    Stream.Position := 0;
    Result := GetJPEGDataSize(Stream);
  finally
    FreeAndNil(Stream);
  end;
end;

{ UpdateApp1JPEGSegments notes:
  - Used internally by TExifData and TXMPPacket.
  - Enforces the proper segment order of JFIF -> Exif -> XMP -> everything else.
  - TExifData will pass in itself as NewExifSource and its child TXMPPacket instance as
    NewXMPSource.
  - A standalone TXMPPacket will pass nil as NewExifSource and itself as NewXMPSource. }
procedure UpdateApp1JPEGSegments(InStream, OutStream: TStream;
  NewExifSource, NewXMPSource: IStreamPersistEx);
var
  BytesToSegment, InStreamStartPos: Int64;
  FoundExif, FoundXMP: Boolean;
  I: Integer;
  Segment: IFoundJPEGSegment;
  SegmentsToSkip: IInterfaceList;
begin
  if not HasJPEGHeader(InStream) then
    raise EInvalidJPEGHeader.CreateRes(@SInvalidJPEGHeader);
  FoundExif := False;
  FoundXMP := False;
  InStreamStartPos := InStream.Position;
  SegmentsToSkip := TInterfaceList.Create;
  WriteJPEGFileHeader(OutStream);
  for Segment in JPEGHeader(InStream, [jmJFIF, jmApp1]) do
  begin
    if Segment.MarkerNum = jmJFIF then
      WriteJPEGSegment(OutStream, Segment)
    else
    begin
      if not FoundExif and Segment.IsExifBlock then
      begin
        FoundExif := True;
        if NewExifSource = nil then NewExifSource := IStreamPersistEx(Segment);
      end
      else if not FoundXMP and Segment.IsXMPBlock then
      begin
        FoundXMP := True;
        if NewXMPSource = nil then NewXMPSource := IStreamPersistEx(Segment);
      end
      else
        Continue;
    end;
    SegmentsToSkip.Add(Segment);
  end;
  Segment := nil;
  if (NewExifSource <> nil) and not NewExifSource.Empty then
    if Supports(NewExifSource, IJPEGSegment, Segment) then
      WriteJPEGSegment(OutStream, Segment)
    else
    begin
      Segment := TFoundJPEGSegment.Create(jmApp1);
      Segment.Data.WriteBuffer(TJPEGSegment.ExifHeader, SizeOf(TJPEGSegment.ExifHeader));
      NewExifSource.SaveToStream(Segment.Data);
      WriteJPEGSegment(OutStream, Segment);
    end;
  Segment := nil;
  if (NewXMPSource <> nil) and not NewXMPSource.Empty then
    if Supports(NewXMPSource, IJPEGSegment, Segment) then
      WriteJPEGSegment(OutStream, Segment)
    else
    begin
      Segment := TFoundJPEGSegment.Create(jmApp1);
      Segment.Data.WriteBuffer(TJPEGSegment.XMPHeader, SizeOf(TJPEGSegment.XMPHeader));
      NewXMPSource.SaveToStream(Segment.Data);
      WriteJPEGSegment(OutStream, Segment);
    end;
  InStream.Position := InStreamStartPos + SizeOf(JPEGFileHeader);
  for I := 0 to SegmentsToSkip.Count - 1 do
  begin
    Segment := IFoundJPEGSegment(SegmentsToSkip[I]);
    BytesToSegment := Segment.Offset - InStream.Position;
    if BytesToSegment > 0 then
      OutStream.CopyFrom(InStream, BytesToSegment);
    InStream.Seek(Segment.TotalSize, soCurrent)
  end;
  OutStream.CopyFrom(InStream, InStream.Size - InStream.Position);
  OutStream.Size := OutStream.Position;
end;

procedure WriteJPEGFileHeader(Stream: TStream); inline;
begin
  Stream.WriteBuffer(JPEGFileHeader, SizeOf(JPEGFileHeader));
end;

procedure WriteJPEGSegment(Stream: TStream; MarkerNum: TJPEGMarker;
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

procedure WriteJPEGSegment(Stream: TStream; MarkerNum: TJPEGMarker;
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
      WriteJPEGSegment(Stream, MarkerNum, MarkerNum, 0);
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
    WriteJPEGSegment(Stream, MarkerNum, Buffer^, DataSize);
  finally
    if BufferAllocated then FreeMem(Buffer);
  end;
end;

procedure WriteJPEGSegment(Stream: TStream; Segment: IJPEGSegment); overload;
begin
  WriteJPEGSegment(Stream, Segment.MarkerNum, Segment.Data);
end;

function CreateAdobeBlock: IAdobeResBlock; overload;
begin
  Result := TAdobeBlock.Create;
end;

function CreateAdobeBlock(ATypeID: Word; const ADataSource: IStreamPersist = nil): IAdobeResBlock; overload;
begin
  Result := TAdobeBlock.Create('8BIM', ATypeID, '', ADataSource);
end;

function CreateAdobeBlock(ATypeID: Word; ADataSource: TStream; ADataSize: Integer): IAdobeResBlock;
begin
  Result := TAdobeBlock.Create('8BIM', ATypeID, '', nil);
  if ADataSize < 0 then Exit;
  if ADataSize = 0 then
  begin
    ADataSource.Position := 0;
    ADataSize := Max(ADataSource.Size, MaxLongint);
  end;
  Result.Data.Size := ADataSize;
  ADataSource.WriteBuffer(Result.Data.Memory^, ADataSize);
end;

function CreateAdobeBlock(ATypeID: Word; const AName: AnsiString;
  const ADataSource: IStreamPersist): IAdobeResBlock; overload;
begin
  Result := TAdobeBlock.Create('8BIM', ATypeID, AName, ADataSource);
end;

function CreateAdobeBlock(const ASignature: AnsiString; ATypeID: Word;
  const AName: AnsiString; const ADataSource: IStreamPersist): IAdobeResBlock; overload;
begin
  Result := TAdobeBlock.Create(ASignature, ATypeID, AName, ADataSource);
end;

function CreateAdobeApp13Segment(const Blocks: array of IAdobeResBlock): IJPEGSegment;
var
  Item: IAdobeResBlock;
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
      (Blocks[I] as IAdobeResBlock).SaveToStream(Stream);
end;

function DoRemoveJPEGSegments(InStream, OutStream: TStream; Markers: TJPEGMarkers): TJPEGMarkers;
var
  Segment: IFoundJPEGSegment;
  StartPos: Int64;
begin
  Result := [];
  StartPos := InStream.Position;
  for Segment in JPEGHeader(InStream, Markers - [jmEndOfImage]) do
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

function RemoveJPEGSegments(const JPEGImage: IStreamPersist; Markers: TJPEGMarkers): TJPEGMarkers;
var
  InStream, OutStream: TMemoryStream;
begin
  if Markers = [] then Exit;
  OutStream := nil;
  InStream := TMemoryStream.Create;
  try
    JPEGImage.SaveToStream(InStream);
    InStream.Position := 0;
    OutStream := TMemoryStream.Create;
    Result := DoRemoveJPEGSegments(InStream, OutStream, Markers);
    if Result <> [] then
    begin
      OutStream.Position := 0;
      JPEGImage.LoadFromStream(OutStream);
    end;
  finally
     FreeAndNil(OutStream);
     FreeAndNil(InStream);
  end;
end;

{ TPSDInfo }

const
  FixedPSDHeaderSize = 12;
  FixedPSDHeader: array[1..FixedPSDHeaderSize] of AnsiChar = '8BPS'#0#1#0#0#0#0#0#0;

function TPSDInfo.ResourceSectionOffset: Integer;
begin
  Result := FixedPSDHeaderSize + 18 + Length(Header.PaletteData);
end;

function TPSDInfo.LayersSectionOffset: Integer;
begin
  Result := ResourceSectionOffset + ResourceSectionSize;
end;

{ PSD file parsing }

type
  TAdobeResBlockParser = class(TAdobeResBlockEnumerator, IAdobeResBlockParser)
  protected
    function GetEnumerator: IAdobeResBlockEnumerator;
  public
    constructor Create(AStream: TStream; AStreamOwnership: TStreamOwnership; var Info: TPSDInfo);
  end;

constructor TAdobeResBlockParser.Create(AStream: TStream;
  AStreamOwnership: TStreamOwnership; var Info: TPSDInfo);

  function ReadLen: LongInt;
  begin
    if not Stream.ReadLongInt(BigEndian, Result) or (Result < 0) then
      raise EInvalidPSDHeader.CreateRes(@SInvalidPSDHeader);
  end;
begin
  inherited Create(AStream, AStreamOwnership);
  if not Stream.TryReadHeader(FixedPSDHeader, SizeOf(FixedPSDHeader)) then
    raise EInvalidPSDHeader.CreateRes(@SInvalidPSDHeader);
  Info.Header.Channels := Stream.ReadWord(BigEndian);
  Info.Header.Height := Stream.ReadLongInt(BigEndian);
  Info.Header.Width := Stream.ReadLongInt(BigEndian);
  Info.Header.Depth := Stream.ReadWord(BigEndian);
  Word(Info.Header.ColorMode) := Stream.ReadWord(BigEndian);
  SetLength(Info.Header.PaletteData, ReadLen);
  if Info.Header.PaletteData <> nil then
    Stream.ReadBuffer(Info.Header.PaletteData[0], Length(Info.Header.PaletteData));
  Info.ResourceSectionSize := ReadLen + 4;
  NextPos := AStream.Position;
  PosTooFar := Min(PosTooFar, NextPos + Info.ResourceSectionSize - 4);
  Stream.Seek(Info.ResourceSectionSize - 4, soCurrent);
  Info.LayersSectionSize := ReadLen + 4;
end;

function TAdobeResBlockParser.GetEnumerator: IAdobeResBlockEnumerator;
begin
  Result := Self;
end;

function HasPSDHeader(Stream: TStream): Boolean; overload;
var
  Buffer: array[1..FixedPSDHeaderSize] of AnsiChar;
  BytesRead: Integer;
begin
  BytesRead := Stream.Read(Buffer, FixedPSDHeaderSize);
  Result := (BytesRead = FixedPSDHeaderSize) and
    CompareMem(@Buffer, @FixedPSDHeader, FixedPSDHeaderSize);
  Stream.Seek(-BytesRead, soCurrent);
end;

function HasPSDHeader(const FileName: string): Boolean; overload;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := HasPSDHeader(Stream);
  finally
     FreeAndNil(Stream);
  end;
end;

function ParsePSDHeader(Stream: TStream; var Info: TPSDInfo;
  StreamOwnership: TStreamOwnership): IAdobeResBlockParser;
begin
  Result := TAdobeResBlockParser.Create(Stream, StreamOwnership, Info);
end;

function ParsePSDHeader(const FileName: string; var Info: TPSDInfo): IAdobeResBlockParser; overload; inline;
begin
  Result := ParsePSDHeader(TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite), Info, soOwned);
end;

procedure WritePSDHeader(Stream: TStream; const Info: TPSDHeaderInfo);
begin
  Stream.WriteBuffer(FixedPSDHeader, FixedPSDHeaderSize);
  Stream.WriteWord(Info.Channels, BigEndian);
  Stream.WriteLongInt(Info.Height, BigEndian);
  Stream.WriteLongInt(Info.Width, BigEndian);
  Stream.WriteWord(Info.Depth, BigEndian);
  Stream.WriteWord(Word(Info.ColorMode), BigEndian);
  Stream.WriteLongInt(Length(Info.PaletteData), BigEndian);
  if Info.PaletteData <> nil then
    Stream.WriteBuffer(Info.PaletteData[0], Length(Info.PaletteData));
end;

procedure WritePSDResourceSection(Stream: TStream; const Blocks: array of IAdobeResBlock); overload;
var
  Block: IAdobeResBlock;
  TotalSize: Integer;
begin
  TotalSize := 0;
  for Block in Blocks do
    if Block <> nil then Inc(TotalSize, Block.TotalSize);
  Stream.WriteLongInt(TotalSize, BigEndian);
  if TotalSize <> 0 then
    for Block in Blocks do
      if Block <> nil then Block.SaveToStream(Stream);
end;

procedure WritePSDResourceSection(Stream: TStream; const Blocks: IInterfaceList);
var
  DynArray: array of IAdobeResBlock;
  I: Integer;
begin
  if Blocks <> nil then
  begin
    SetLength(DynArray, Blocks.Count);
    for I := 0 to Blocks.Count - 1 do
      Supports(Blocks[I], IAdobeResBlock, DynArray[I]);
  end;
  WritePSDResourceSection(Stream, DynArray);
end;

function GetExecutableName: string;
VAR
   MatchedCase: TFilenameCaseMatch;
begin                             
  { $Warnings OFF}
  Result:= ExpandFileNameCase(GetModuleName(0), MatchedCase);
  { $Warnings On}
end;

end.

