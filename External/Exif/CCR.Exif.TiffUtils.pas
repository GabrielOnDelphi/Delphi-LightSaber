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
{ The Original Code is CCR.Exif.TiffUtils.pas.                                         }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2014 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

{$I CCR.Exif.inc}
unit CCR.Exif.TiffUtils;

interface

uses
  Types, SysUtils, Classes, CCR.Exif.BaseUtils, CCR.Exif.StreamHelper;

type
  EInvalidTiffData = class(ECCRExifException);
  ETagAlreadyExists = class(EInvalidTiffData);

  TTiffTagID = type Word;
{$Z2}
  TTiffDataType = (tdByte = 1, tdAscii, tdWord, tdLongWord, tdLongWordFraction,
    tdShortInt, tdUndefined, tdSmallInt, tdLongInt, tdLongIntFraction, tdSingle,
    tdDouble, tdSubDirectory);
{$Z1}
  TTiffDataTypes = set of TTiffDataType;

  ITiffTag = interface;

  TTiffTag = record const
    HeaderSize = 12;
    OffsetDataTypes = [tdWord, tdLongWord, {tdLongInt, }tdSubDirectory];
    class function IsWellFormed(DataType: TTiffDataType; ElementCount: LongInt): Boolean; overload; static; {$IFDEF CanInline}inline;{$ENDIF}
    class function IsWellFormed(const Tag: ITiffTag): Boolean; overload; static; {$IFDEF CanInline}inline;{$ENDIF}
    class function MatchOffsetToByteCountID(OffsetTagID: TTiffTagID;
      var ByteCountID: TTiffTagID): Boolean; static;
  end;

  PTiffTagInfo = ^TTiffTagInfo;
  TTiffTagInfo = record
  public
    HeaderOffset, DataOffset: LongWord;
    ID: TTiffTagID;
    DataType: TTiffDataType;
    ElementCount: LongInt;
    function DataSize: Integer;
    function IsWellFormed: Boolean; overload;{$IFDEF CanInline}inline;{$ENDIF}
  end;

  TTiffTagInfoDynArray = array of TTiffTagInfo;

  TiffString = type AnsiString;

  TTiffLongWordFraction = packed record
    constructor Create(ANumerator: LongWord; ADenominator: LongWord = 1); overload;
    constructor Create(const AQuotient: Currency); overload;
    constructor CreateFromString(const AString: string);
    class function CreateMissingOrInvalid: TTiffLongWordFraction; static;
    function AsString: string; deprecated {$IFDEF DepCom}'Use ToString instead'{$ENDIF};
    function MissingOrInvalid: Boolean;
    function Quotient: Extended;
    function ToString: string;
    case Integer of
      0: (Numerator, Denominator: LongWord);
      1: (PackedValue: Int64);
  end;

  TTiffLongIntFraction = packed record
    constructor Create(ANumerator: LongInt; ADenominator: LongInt = 1); overload;
    constructor Create(const AQuotient: Currency); overload;
    constructor CreateFromString(const AString: string);
    class function CreateMissingOrInvalid: TTiffLongIntFraction; static;
    function AsString: string; deprecated {$IFDEF DepCom}'Use ToString instead'{$ENDIF};
    function MissingOrInvalid: Boolean;
    function Quotient: Extended;
    function ToString: string;
    case Integer of
      0: (Numerator, Denominator: LongInt);
      1: (PackedValue: Int64);
  end;

  ITiffParser = interface;
  ITiffDirectory = interface;

  ITiffTag = interface(IMetadataBlock)
  ['{DC9C12D5-EFEF-4B59-9B02-89A757896CCA}']
    function GetDataType: TTiffDataType;
    function GetParent: ITiffDirectory;
    function GetElementCount: LongInt;
    function GetID: TTiffTagID;
    function GetOriginalDataOffset: LongWord;

    property DataType: TTiffDataType read GetDataType;
    property ElementCount: LongInt read GetElementCount;
    property ID: TTiffTagID read GetID;
    property OldDataOffset: LongWord read GetOriginalDataOffset;
    property Parent: ITiffDirectory read GetParent;
  end;

  IFoundTiffTag = interface(ITiffTag)
  ['{8258F43E-38E8-4FD9-A1D7-D3C42B38F043}']
    function GetParser: ITiffParser;

    property Parser: ITiffParser read GetParser;
  end;

  ITiffDirectoryEnumerator = interface
  ['{7E882310-28E7-4AF3-9978-27D1E5F36D09}']
    function GetCurrent: ITiffTag;

    function MoveNext: Boolean;
    property Current: ITiffTag read GetCurrent;
  end;

  ITiffDirectory = interface //implemented by both the internal TFoundTiffDirectory below and TExifSection in CCR.Exif.pas
  ['{3DA5B982-CB8C-412A-8E5F-AD3A98CF345E}']
    function GetEnumerator: ITiffDirectoryEnumerator;
    function GetIndex: Integer;
    function GetParent: ITiffDirectory;
    function GetTagCount: Integer;

    function FindTag(TagID: TTiffTagID; out ParsedTag: ITiffTag): Boolean;
    function TagExists(TagID: TTiffTagID; ValidDataTypes: TTiffDataTypes =
      [Low(TTiffDataType)..High(TTiffDataType)]; MinElementCount: LongInt = 1;
      MaxElementCount: LongInt = MaxLongInt): Boolean;
    property Index: Integer read GetIndex;
    property Parent: ITiffDirectory read GetParent;
    property TagCount: Integer read GetTagCount;
  end;

  IFoundTiffDirectory = interface(ITiffDirectory)
  ['{A8B5DB5F-2084-437C-872D-0139DE7C3E54}']
    function GetIndex: Integer;
    function GetLoadErrors: TMetadataLoadErrors;
    function GetParser: ITiffParser;
    function GetTagInfo: TTiffTagInfoDynArray;

    function IsLastDirectoryInFile: Boolean;
    function IsExifThumbailDirectory: Boolean;
    function TryLoadExifThumbnail(const Dest: IStreamPersist): Boolean;
    property LoadErrors: TMetadataLoadErrors read GetLoadErrors;
    property Parser: ITiffParser read GetParser;
    property TagInfo: TTiffTagInfoDynArray read GetTagInfo;
  end;

  TTiffParserEnumerator = {$IFDEF NoRecEnumBug}record{$ELSE}class sealed{$ENDIF}
  strict private
    FCurrent: IFoundTiffDirectory;
    FMaxPossibleOffset: Int64;
    FNextIndex: Integer;
    FNextOffset: LongInt;
    FSource: ITiffParser;
  public
    constructor Create(const ASource: ITiffParser; const AMaxPossibleOffset: Int64;
      const ANextOffset: LongInt);
    function MoveNext: Boolean;
    property Current: IFoundTiffDirectory read FCurrent;
  end;

  ITiffParser = interface
    function GetBasePosition: Int64;
    function GetEndianness: TEndianness;
    function GetEnumerator: TTiffParserEnumerator;
    function GetStream: TStream;

    procedure LoadTagData(const TagInfo: TTiffTagInfo; var Buffer); overload;
    function ParseSubDirectory(const OffsetTag: ITiffTag;
      out Directory: IFoundTiffDirectory): Boolean; overload;
    function ParseSubDirectory(const Parent: ITiffDirectory;
      const OffsetTag: TTiffTagInfo; out Directory: IFoundTiffDirectory): Boolean; overload;
    property BasePosition: Int64 read GetBasePosition;
    property Endianness: TEndianness read GetEndianness;
    property Stream: TStream read GetStream;
  end;

  ITiffRewriteCallback = interface;

  TTiffDirectoryRewriter = class sealed
  protected type
    TTagToWrite = class abstract
    strict private
      FNewDataOffset: LongWord;
      FInfo: TTiffTagInfo;
      FParent: TTiffDirectoryRewriter;
    protected
      property Info: TTiffTagInfo read FInfo;
    public
      constructor Create(AParent: TTiffDirectoryRewriter; const AInfo: TTiffTagInfo); overload;
      constructor Create(AParent: TTiffDirectoryRewriter; AID: TTiffTagID;
        ADataType: TTiffDataType; AElementCount: LongInt; AOriginalDataOffset: LongWord = 0); overload;
      function DataSize: Integer; inline;
      procedure WriteData(Stream: TStream; Endianness: TEndianness); virtual; abstract;
      property DataType: TTiffDataType read FInfo.DataType;
      property ElementCount: LongInt read FInfo.ElementCount;
      property ID: TTiffTagID read FInfo.ID;
      property NewDataOffset: LongWord read FNewDataOffset write FNewDataOffset;
      property OldDataOffset: LongWord read FInfo.DataOffset;
      property Parent: TTiffDirectoryRewriter read FParent;
    end;

    TImageItem = record
      ByteCount: 0..High(LongInt);
      OldOffset, NewOffset: LongWord;
    end;
    TImageInfo = array of TImageItem;
  strict private
    FDoneInitialization: Boolean;
    FImageInfo: TImageInfo;
    FNewIFDOffset: LongWord;
    FRewriteSource: Boolean;
    FSource: ITiffDirectory;
    FSourceInfo: TTiffTagInfoDynArray;
    FSourceParser: ITiffParser;
    FTagsToWrite: TObjectList;
    procedure AddTag(Tag: TTagToWrite); overload;
    function FindTagIndex(AID: TTiffTagID; out Index: Integer): Boolean;
    function GetTag(Index: Integer): TTagToWrite;
    function GetTagCount: Integer;
  protected
    constructor Create(const Source: ITiffDirectory; const Callback: ITiffRewriteCallback);
    function FindTagToWrite(ID: TTiffTagID; var Tag: TTagToWrite): Boolean;
    function HasImageData: Boolean; inline;
    function ProtectMakerNote(var MakerNoteTag: TTagToWrite): Boolean; //looks for the ttExifOffset sub-dir, then ttMakerNote under that, checking the data type and size
    procedure InitializeImageInfo(const Offsets, ByteCounts: array of LongWord);
    procedure InitializeNewOffsets(var NextOffset: LongWord; ProtectedTag: TTagToWrite);
    procedure Write(AStream: TStream; const ABasePosition: Int64;
      AEndianness: TEndianness; ANextIFDOffset: LongWord);
    property ImageInfo: TImageInfo read FImageInfo;
    property NewIFDOffset: LongWord read FNewIFDOffset write FNewIFDOffset;
    property RewriteSource: Boolean read FRewriteSource;
    property TagCount: Integer read GetTagCount;
    property Tags[Index: Integer]: TTagToWrite read GetTag;
  public
    destructor Destroy; override;
    procedure AddTag(ID: TTiffTagID; const DataSource: IStreamPersist;
      DataType: TTiffDataType = tdUndefined); overload;
    procedure AddTag(const Source: ITiffTag); overload;
    procedure AddSubDirectory(OffsetID: TTiffTagID; const SubDirectory: ITiffDirectory;
      const Callback: ITiffRewriteCallback = nil);
    procedure IgnoreDirectory;
    property Source: ITiffDirectory read FSource;
  end;

  ITiffRewriteCallback = interface
  ['{55D2E445-1E18-4887-A004-947F16B696D1}']
    procedure AddNewTags(Rewriter: TTiffDirectoryRewriter);
    procedure RewritingOldTag(const Source: ITiffDirectory; TagID: TTiffTagID;
      DataType: TTiffDataType; var Rewrite: Boolean);
  end;

  TSimpleTiffRewriteCallbackImpl = class(TAggregatedObject, ITiffRewriteCallback)
  strict private                //Internal helper class for TIPTCData and TXMPPacket that
    FMetadataTagID: TTiffTagID; //standardises the implementation of ITiffRewriteCallback.
    function GetOwner: IStreamPersistEx; inline;
  protected
    { ITiffRewriteCallback }
    procedure AddNewTags(Rewriter: TTiffDirectoryRewriter);
    procedure RewritingOldTag(const Source: ITiffDirectory; TagID: TTiffTagID;
      DataType: TTiffDataType; var Rewrite: Boolean);
  public
    constructor Create(const AOwner: IStreamPersistEx; AMetadataTagID: TTiffTagID);
    property Owner: IStreamPersistEx read GetOwner;
  end;

const
  TiffElementSizes: array[TTiffDataType] of Integer = (
    1, 1, 2, 4, 8, 1, 1, 2, 4, 8, 4, 8, 4);

function HasTiffHeader(Stream: TStream; var Endianness: TEndianness;
  MoveOnSuccess: Boolean = False): Boolean; overload;
function HasTiffHeader(Stream: TStream; MoveOnSuccess: Boolean = False): Boolean; overload; inline;
function ParseTiff(Stream: TStream; StreamOwnership: TStreamOwnership = soReference): ITiffParser; overload;
function ParseTiff(const FileName: string): ITiffParser; overload; inline;
function ParseTiffDirectory(Stream: TStream; Endianness: TEndianness;
  const BasePosition, Offset, InternalOffset: Int64): IFoundTiffDirectory;

procedure RewriteTiff(InStream, OutStream: TStream; const Callback: ITiffRewriteCallback);

procedure WriteTiffHeader(Stream: TStream; Endianness: TEndianness;
  FirstDirectoryOffset: LongWord = 8);

implementation

uses Math, RTLConsts, CCR.Exif.Consts, CCR.Exif.TagIDs;

function TryLoadJpegImageFromStream(const AJpegImage: IStreamPersist; AStream: TStream): Boolean;
var
  JpegSize: Int64;
  Temp: TMemoryStream;
begin
  try
    JpegSize := GetJPEGDataSize(AStream);
    if JpegSize = (AStream.Size - AStream.Position) then
      AJpegImage.LoadFromStream(AStream)
    else
    begin
      Temp := TMemoryStream.Create;
      try
        Temp.Size := JpegSize;
        AStream.ReadBuffer(Temp.Memory^, JpegSize);
        AJpegImage.LoadFromStream(Temp);
      finally
         FreeAndNil(Temp);
      end;
    end;
    Result := True;
  except
    on Exception do Result := False;
  end;
end;

type
  TFoundTiffDirectory = class;

  TFoundTiffTag = class(TMetadataBlock, ITiffTag)
  strict private
    FParent: TFoundTiffDirectory;
    FParentIntf: ITiffDirectory; //need a 'hard' intf ref to ensure the parent is kept alive
    FSourcePtr: ^TTiffTagInfo;
  protected
    function GetDataType: TTiffDataType;
    function GetElementCount: LongInt;
    function GetOriginalDataOffset: LongWord;
    function GetID: TTiffTagID;
    function GetParent: ITiffDirectory;
    function HasIPTCBlockID: Boolean; override;
    function HasXMPBlockID: Boolean; override;
  public
    constructor Create(AParent: TFoundTiffDirectory; AIndex: Integer); overload;
    constructor Create(AID: TTiffTagID; ADataType: TTiffDataType;
      const ADataSource: IStreamPersist); overload;
    destructor Destroy; override;
    property DataType: TTiffDataType read GetDataType;
    property ElementCount: LongInt read GetElementCount;
    property ID: TTiffTagID read GetID;
    property Parent: TFoundTiffDirectory read FParent;
  end;

  TFoundTiffDirectoryEnumerator = class(TInterfacedObject, ITiffDirectoryEnumerator)
  strict private
    FCurrentIndex: Integer;
    FSource: TFoundTiffDirectory;
  protected
    function GetCurrent: ITiffTag;
    function MoveNext: Boolean;
  public
    constructor Create(Source: TFoundTiffDirectory);
  end;

  TFoundTiffDirectory = class(TInterfacedObject, ITiffDirectory, IFoundTiffDirectory)
  strict private
    FIndex: Integer;
    FLoadErrors: TMetadataLoadErrors;
    FParent: ITiffDirectory;
    FParser: ITiffParser;
    FTags: TTiffTagInfoDynArray;
  protected
    FMoreToFollow: Boolean;
    constructor Create(const AParser: ITiffParser; const AParent: ITiffDirectory; AIndex: Integer;
      const Tags: TTiffTagInfoDynArray; const LoadErrors: TMetadataLoadErrors); overload;
    function GetEnumerator: ITiffDirectoryEnumerator;
    function GetIndex: Integer;
    function GetLoadErrors: TMetadataLoadErrors;
    function GetParent: ITiffDirectory;
    function GetParser: ITiffParser;
    function LoadSubDirectory(OffsetTagID: TTiffTagID): ITiffDirectory;
    function GetTagCount: Integer;
    function GetTagInfo: TTiffTagInfoDynArray;
    function FindTag(TagID: TTiffTagID; out Index: Integer): Boolean; overload; inline;
    function FindTag(TagID: TTiffTagID; out ParsedTag: ITiffTag): Boolean; overload;
    function IndexOfTag(TagID: TTiffTagID): Integer;
    function IsExifThumbailDirectory: Boolean;
    function IsLastDirectoryInFile: Boolean; inline;
    function TagExists(TagID: TTiffTagID; ValidDataTypes: TTiffDataTypes =
      [Low(TTiffDataType)..High(TTiffDataType)]; MinElementCount: LongInt = 1;
      MaxElementCount: LongInt = MaxLongInt): Boolean;
    function TryLoadExifThumbnail(Dest: TStream): Boolean; overload;
    function TryLoadExifThumbnail(const Dest: IStreamPersist): Boolean; overload;
  public
    constructor Create(const AParser: ITiffParser; const AParent: ITiffDirectory;
      AIndex: Integer; const AOffset: Int64; const AInternalOffset: Int64 = 0); overload;
    class function TryCreate(const AParser: ITiffParser; const AParent: ITiffDirectory;
      AIndex: Integer; const AOffset: Int64; out Instance: IFoundTiffDirectory): Boolean; static;
    property Parser: ITiffParser read FParser;
    property TagInfo: TTiffTagInfoDynArray read FTags;
  end;

  TTiffParser = class(TInterfacedObject, ITiffParser)
  strict private
    FBasePosition, FMaxOffsetValue: Int64;
    FEndianness: TEndianness;
    FFirstDirOffset: LongInt;
    FStream: TStream;
    FStreamOwnership: TStreamOwnership;
  protected
    function GetBasePosition: Int64;
    function GetEndianness: TEndianness;
    function GetEnumerator: TTiffParserEnumerator;
    function GetStream: TStream;
    procedure LoadTagData(const TagInfo: TTiffTagInfo; var Buffer); overload;
    function ParseSubDirectory(const OffsetTag: ITiffTag;
      out Directory: IFoundTiffDirectory): Boolean; overload;
    function ParseSubDirectory(const Parent: ITiffDirectory; const OffsetTag: TTiffTagInfo;
      out Directory: IFoundTiffDirectory): Boolean; overload;
  public
    constructor Create(AStream: TStream; AStreamOwnership: TStreamOwnership); overload;
    constructor Create(AStream: TStream; const ABasePosition: Int64;
      AEndianness: TEndianness); overload;
    destructor Destroy; override;
    property BasePosition: Int64 read FBasePosition;
    property Endianness: TEndianness read FEndianness;
    property Stream: TStream read FStream;
  end;

  TInternaTiffTagToWrite = class(TTiffDirectoryRewriter.TTagToWrite)
  strict private
    FParser: ITiffParser;
  strict protected
    Kind: (itNormal, itImageOffsets, itImageByteCounts);
  public
    constructor Create(AParent: TTiffDirectoryRewriter; const AParser: ITiffParser;
      const ATagInfo: TTiffTagInfo);
    procedure WriteData(Stream: TStream; Endianness: TEndianness); override;
    property Info;
    property Parser: ITiffParser read FParser;
  end;

  TExternalTiffTagToWrite = class(TTiffDirectoryRewriter.TTagToWrite)
  strict protected
    FDataSize: Integer;
    FTag: ITiffTag;
  public
    constructor Create(AOwner: TTiffDirectoryRewriter; const ATag: ITiffTag); overload;
    constructor Create(AOwner: TTiffDirectoryRewriter; AID: TTiffTagID;
      ADataType: TTiffDataType; const ADataSource: IStreamPersist); overload;
    procedure WriteData(Stream: TStream; Endianness: TEndianness); override;
    property Source: ITiffTag read FTag;
  end;

  TTiffSubDirToWrite = class(TTiffDirectoryRewriter.TTagToWrite)
  strict private
    FDirectory: TTiffDirectoryRewriter;
  public
    constructor Create(AOwner: TTiffDirectoryRewriter; AOffsetID: TTiffTagID;
      ADirectory: TTiffDirectoryRewriter);
    destructor Destroy; override;
    procedure WriteData(Stream: TStream; Endianness: TEndianness); override;
    property Directory: TTiffDirectoryRewriter read FDirectory;
  end;

{ TTiffTag }

{$Q-}
class function TTiffTag.IsWellFormed(DataType: TTiffDataType; ElementCount: LongInt): Boolean;
begin
  case Ord(DataType) of
    Ord(Low(DataType))..Ord(High(DataType)):
      Result := (LongInt(ElementCount * TiffElementSizes[DataType]) >= 0);
  else Result := False;
  end;
end;
{$IFDEF OverflowCheckingOn}{$Q+}{$ENDIF}

class function TTiffTag.IsWellFormed(const Tag: ITiffTag): Boolean;
begin
  Result := IsWellFormed(Tag.DataType, Tag.ElementCount);
end;

class function TTiffTag.MatchOffsetToByteCountID(OffsetTagID: TTiffTagID;
  var ByteCountID: TTiffTagID): Boolean; 
begin
  Result := True;
  case OffsetTagID of
    ttStripOffsets: ByteCountID := ttStripByteCounts;
    ttTileOffsets: ByteCountID := ttTileOffsets;
    ttThumbnailOffset: ByteCountID := ttThumbnailSize;
  else Result := False;
  end;
end;

{ TTiffTagInfo }

function TTiffTagInfo.DataSize: Integer;
begin
  if IsWellFormed then
    Result := ElementCount * TiffElementSizes[DataType]
  else
    Result := 0;
end;

function TTiffTagInfo.IsWellFormed: Boolean;
begin
  Result := TTiffTag.IsWellFormed(DataType, ElementCount);
end;

{ TTiffLongXXXFraction }

function GCD(A, B: Int64): Int64;
var
  Temp: Int64;
begin
  while B <> 0 do
  begin
    Temp := B;
    B := A mod B;
    A := Temp;
  end;
  Result := A;
end;

procedure CurrencyToFraction(const Source: Currency; var N, D: Int64);
var
  Factor: Int64;
begin
  N := Trunc(Source * 10000);
  D := 10000;
  Factor := GCD(N, D);
  N := N div Factor;
  D := D div Factor;
end;

constructor TTiffLongIntFraction.Create(ANumerator, ADenominator: LongInt);
begin
  Numerator := ANumerator;
  Denominator := ADenominator;
end;

constructor TTiffLongIntFraction.Create(const AQuotient: Currency);
var
  N, D: Int64;
begin
  CurrencyToFraction(AQuotient, N, D);
  {$RANGECHECKS ON}
  Numerator := N;
  Denominator := D;
  {$IFDEF RangeCheckingOff}{$RANGECHECKS OFF}{$ENDIF}
end;

constructor TTiffLongIntFraction.CreateFromString(const AString: string);
var
  DivSignPos: Integer;
  Quotient: Currency;
  Valid: Boolean;
begin
  DivSignPos := Pos('/', AString);
  if DivSignPos <> 0 then
    Valid := TryStrToInt(Copy(AString, 1, DivSignPos - 1), Numerator) and
      TryStrToInt(Copy(AString, DivSignPos + 1, MaxInt), Denominator)
  else
  begin
    Valid := TryStrToCurr(AString, Quotient);
    if Valid then
      try
        Create(Quotient);
      except
        on ERangeError do Valid := False;
      end;
  end;
  if not Valid then
    PackedValue := 0;
end;

class function TTiffLongIntFraction.CreateMissingOrInvalid: TTiffLongIntFraction;
begin
  Result.Numerator := 0;
  Result.Denominator := 0;
end;

function TTiffLongIntFraction.AsString: string;
begin
  Result := ToString;
end;

function TTiffLongIntFraction.MissingOrInvalid: Boolean;
begin
  Result := (Denominator = 0);
end;

function TTiffLongIntFraction.Quotient: Extended;
begin
  if MissingOrInvalid then
    Result := 0
  else
    Result := Numerator / Denominator
end;

function TTiffLongIntFraction.ToString: string;
begin
  if MissingOrInvalid then
    Result := ''
  else if Denominator = 1 then
    Result := IntToStr(Numerator)
  else
    FmtStr(Result, '%d/%d', [Numerator, Denominator]);
end;

function TryStrToLongWord(const S: string; var Value: LongWord): Boolean;
var
  Int64Value: Int64;
begin
  Result := TryStrToInt64(S, Int64Value) and (Int64Value >= 0) and
    (Int64Value <= High(Value));
  if Result then Value := LongWord(Int64Value);
end;

constructor TTiffLongWordFraction.Create(ANumerator: LongWord; ADenominator: LongWord);
begin
  Numerator := ANumerator;
  Denominator := ADenominator;
end;

constructor TTiffLongWordFraction.Create(const AQuotient: Currency);
var
  N, D: Int64;
begin
  CurrencyToFraction(AQuotient, N, D);
  {$RANGECHECKS ON}
  Numerator := N;
  Denominator := D;
  {$IFDEF RangeCheckingOff}{$RANGECHECKS OFF}{$ENDIF}
end;

constructor TTiffLongWordFraction.CreateFromString(const AString: string);
var
  DivSignPos: Integer;
  Quotient: Currency;
  Valid: Boolean;
begin
  DivSignPos := Pos('/', AString);
  if DivSignPos <> 0 then
    Valid := TryStrToLongWord(Copy(AString, 1, DivSignPos - 1), Numerator) and
      TryStrToLongWord(Copy(AString, DivSignPos + 1, MaxInt), Denominator)
  else
  begin
    Valid := TryStrToCurr(AString, Quotient);
    if Valid then
      try
        Create(Quotient);
      except
        on ERangeError do Valid := False;
      end;
  end;
  if not Valid then
    PackedValue := 0;
end;

class function TTiffLongWordFraction.CreateMissingOrInvalid: TTiffLongWordFraction;
begin
  Result.Numerator := 0;
  Result.Denominator := 0;
end;

function TTiffLongWordFraction.AsString: string;
begin
  Result := ToString;
end;

function TTiffLongWordFraction.MissingOrInvalid: Boolean;
begin
  Result := (Denominator = 0);
end;

function TTiffLongWordFraction.Quotient: Extended;
begin
  if MissingOrInvalid then
    Result := 0
  else
    Result := Numerator / Denominator
end;

function TTiffLongWordFraction.ToString: string;
begin
  if MissingOrInvalid then
    Result := ''
  else if Denominator = 1 then
    Result := IntToStr(Numerator)
  else
    FmtStr(Result, '%d/%d', [Numerator, Denominator]);
end;

{ TIFF parsing routines }

function HasTiffHeader(Stream: TStream; var Endianness: TEndianness;
  MoveOnSuccess: Boolean = False): Boolean; overload;
var
  Buffer: array[0..1] of Word;
  BytesRead: Integer;
begin
  Result := False;
  BytesRead := Stream.Read(Buffer, SizeOf(Buffer));
  if BytesRead = SizeOf(Buffer) then
    case Buffer[0] of
      TiffSmallEndianCode, TiffBigEndianCode: //accept small endian marker when meant big endian and vice versa
        case Buffer[1] of
          TiffMagicNum:
          begin
            Endianness := SmallEndian;
            Result := True;
          end;
          TiffMagicNumBigEndian:
          begin
            Endianness := BigEndian;
            Result := True;
          end;
        end;
    end;
  if not Result or not MoveOnSuccess then
    Stream.Seek(-BytesRead, soCurrent);
end;

function HasTiffHeader(Stream: TStream; MoveOnSuccess: Boolean = False): Boolean; overload;
var
  Endianness: TEndianness;
begin
  Result := HasTiffHeader(Stream, Endianness, MoveOnSuccess)
end;
{
  LoadTiffDirectory - sanity checking:
  - We check the reported tag count is theoretically possible up front.
  - Data offsets are validated.
  - Two bad tag headers in a row, and any further parsing is aborted.
  - General aim is to avoid an exception being raised. This is because it becomes
    annoying in general use otherwise, Exif data in the wild frequently being badly
    formed in some fashion...
}
function LoadTiffDirectory(Stream: TStream; Endianness: TEndianness;
  const BasePosition, Offset, InternalOffset: Int64;
  out LoadErrors: TMetadataLoadErrors): TTiffTagInfoDynArray; overload;
var
  I, TagCount: Integer;
  MaxTagCount, StartPos, StreamSize, Offset64: Int64;
begin
  LoadErrors := [];
  Result := nil;                    
  StartPos := BasePosition + Offset;
  StreamSize := Stream.Seek(0, soEnd);
  if (StartPos < 0) or (StartPos + 2 > StreamSize) then
  begin
    LoadErrors := [leBadOffset];
    Exit;
  end;
  MaxTagCount := (StreamSize - StartPos - 2) div TTiffTag.HeaderSize;
  Stream.Position := StartPos;
  TagCount := Stream.ReadWord(Endianness);
  if TagCount > MaxTagCount then
  begin
    TagCount := MaxTagCount;
    Include(LoadErrors, leBadTagCount);
  end;
  SetLength(Result, TagCount);
  for I := 0 to TagCount - 1 do
  begin
    Result[I].HeaderOffset := Stream.Position - BasePosition;
    Result[I].ID := Stream.ReadWord(Endianness);
    Word(Result[I].DataType) := Stream.ReadWord(Endianness);
    Result[I].ElementCount := Stream.ReadLongInt(Endianness);
    if Result[I].DataSize > 4 then //DataSize will validate DataType for us
    begin
      Offset64 := Stream.ReadLongWord(Endianness) + InternalOffset;
      if (Offset64 < 8) or (Offset64 + BasePosition < 0) or
         (Offset64 + Result[I].DataSize + BasePosition > StreamSize) then
        Result[I].ElementCount := -1
      else
        Result[I].DataOffset := Offset64;
    end
    else
    begin
      Result[I].DataOffset := Result[I].HeaderOffset + 8;
      Stream.Seek(4, soCurrent);
    end;
    if not Result[I].IsWellFormed then
      if (I = 0) or Result[I - 1].IsWellFormed then
      begin
        Include(LoadErrors, leBadTagHeader);
        if (I = 0) and (leBadTagCount in LoadErrors) then
        begin
          Result := nil;
          Exit;
        end;
      end
      else
      begin
        Include(LoadErrors, leBadTagCount);
        SetLength(Result, I);
        Exit;
      end;
  end;
end;

function LoadTiffDirectory(const Parser: ITiffParser; const Offset: Int64;
  out LoadErrors: TMetadataLoadErrors): TTiffTagInfoDynArray; overload; inline;
begin
  Result := LoadTiffDirectory(Parser.Stream, Parser.Endianness, Parser.BasePosition,
    Offset, 0, LoadErrors);
end;

function InitLoadOffsetArray(DataType: TTiffDataType; ElementCount: LongInt;
  out ArrayResult: TLongWordDynArray): Boolean; inline;
begin
  Assert((ElementCount >= 0) and (DataType in TTiffTag.OffsetDataTypes));
  SetLength(ArrayResult, ElementCount);
  Result := (ArrayResult <> nil);
end;

function LoadOffsetArray(const Source: ITiffParser;
  const TagInfo: TTiffTagInfo): TLongWordDynArray; overload;
var
  WordArray: TWordDynArray;
  I: Integer;
begin
  if not InitLoadOffsetArray(TagInfo.DataType, TagInfo.ElementCount, Result) then Exit;
  if TagInfo.DataType = tdWord then
  begin
    SetLength(WordArray, TagInfo.ElementCount);
    Source.LoadTagData(TagInfo, WordArray[0]);
    for I := 0 to TagInfo.ElementCount - 1 do
      Result[I] := WordArray[I];
  end
  else
    Source.LoadTagData(TagInfo, Result[0]);
end;

function LoadOffsetArray(const Tag: ITiffTag): TLongWordDynArray; overload;
var
  DataPtr: PWordArray;
  I: LongInt;
begin
  if not InitLoadOffsetArray(Tag.DataType, Tag.ElementCount, Result) then Exit;
  DataPtr := Tag.Data.Memory;
  if Tag.DataType = tdWord then
    for I := 0 to High(Result) do
      Result[I] := DataPtr[I]
  else
    Move(DataPtr[0], Result[0], Length(Result) * 4);
end;

function SeekToExifThumbnail(const Parser: ITiffParser;
  const OffsetTag: TTiffTagInfo): Boolean;
begin
  Result := False;
  if not (OffsetTag.DataType in TTiffTag.OffsetDataTypes) or
    (OffsetTag.ElementCount <> 1) then Exit;
  Parser.Stream.Position := Parser.BasePosition + LoadOffsetArray(Parser, OffsetTag)[0];
  if HasJPEGHeader(Parser.Stream) then Result := True;
end;

procedure WriteTiffTagData(DataType: TTiffDataType; ElementCount: Longint;
  DataPtr: Pointer; Dest: TStream; Endianness: TEndianness);
var
  I, DataSize: Integer;
begin
  DataSize := TiffElementSizes[DataType] * ElementCount;
  if DataType = tdDouble then
    for I := 0 to ElementCount - 1 do
      Dest.WriteDouble(PDoubleArray(DataPtr)[I], Endianness)
  else
    case TiffElementSizes[DataType] of
      1: Dest.WriteBuffer(DataPtr^, ElementCount);
      2: for I := 0 to ElementCount - 1 do
           Dest.WriteWord(PWordArray(DataPtr)[I], Endianness);
    else
      for I := 0 to DataSize div 4 - 1 do
        Dest.WriteLongWord(PLongWordArray(DataPtr)[I], Endianness)
    end;
  for I := 3 downto DataSize do
    Dest.WriteByte(0);
end;

{ TFoundTiffTag }

constructor TFoundTiffTag.Create(AParent: TFoundTiffDirectory; AIndex: Integer);
begin
  Assert((AParent <> nil) and (AIndex >= 0));
  inherited Create;
  FParent := AParent;
  FParentIntf := AParent;
  FSourcePtr := @AParent.TagInfo[AIndex];
  Data.SetSize(FSourcePtr.DataSize);
  AParent.Parser.LoadTagData(FSourcePtr^, Data.Memory^);
  Data.DisableChanges := True;
end;

constructor TFoundTiffTag.Create(AID: TTiffTagID; ADataType: TTiffDataType;
  const ADataSource: IStreamPersist);
begin
  inherited Create;
  New(FSourcePtr);
  FSourcePtr.ID := AID;
  FSourcePtr.DataType := ADataType;
  if ADataSource <> nil then
  begin
    ADataSource.SaveToStream(Data);
    FSourcePtr.ElementCount := LongInt(Data.Size div TiffElementSizes[ADataType]);
  end
  else
  begin
    FSourcePtr.ElementCount := 1;
    Data.Size := TiffElementSizes[ADataType];
    FillChar(Data.Memory^, TiffElementSizes[ADataType], 0);
  end;
end;

destructor TFoundTiffTag.Destroy;
begin
  if (FParent = nil) and (FSourcePtr <> nil) then Dispose(FSourcePtr);
  inherited;
end;

function TFoundTiffTag.GetDataType: TTiffDataType;
begin
  Result := FSourcePtr.DataType;
end;

function TFoundTiffTag.GetElementCount: LongInt;
begin
  Result := FSourcePtr.ElementCount;
end;

function TFoundTiffTag.GetID: TTiffTagID;
begin
  Result := FSourcePtr.ID;
end;

function TFoundTiffTag.GetOriginalDataOffset: LongWord;
begin
  Result := FSourcePtr.DataOffset;
end;

function TFoundTiffTag.HasIPTCBlockID: Boolean;
begin
  Result := (FSourcePtr.ID = ttIPTC);
end;

function TFoundTiffTag.HasXMPBlockID: Boolean;
begin
  Result := (FSourcePtr.ID = ttXMP);
end;

function TFoundTiffTag.GetParent: ITiffDirectory;
begin
  Result := FParentIntf;
end;

{ TFoundTiffDirectoryEnumerator }

constructor TFoundTiffDirectoryEnumerator.Create(Source: TFoundTiffDirectory);
begin
  FCurrentIndex := -1;
  FSource := Source;
end;

function TFoundTiffDirectoryEnumerator.GetCurrent: ITiffTag;
begin
  Result := TFoundTiffTag.Create(FSource, FCurrentIndex);
end;

function TFoundTiffDirectoryEnumerator.MoveNext: Boolean;
begin
  Result := (FCurrentIndex < High(FSource.TagInfo));
  if Result then Inc(FCurrentIndex);
end;

{ TFoundTiffDirectory }

constructor TFoundTiffDirectory.Create(const AParser: ITiffParser; const AParent: ITiffDirectory;
  AIndex: Integer; const AOffset: Int64; const AInternalOffset: Int64 = 0);
begin
  inherited Create;
  FTags := LoadTiffDirectory(AParser.Stream, AParser.Endianness,
    AParser.BasePosition, AOffset, AInternalOffset, FLoadErrors);
  FIndex := AIndex;
  FParser := AParser;
  FParent := AParent;
end;

constructor TFoundTiffDirectory.Create(const AParser: ITiffParser; const AParent: ITiffDirectory;
  AIndex: Integer; const Tags: TTiffTagInfoDynArray; const LoadErrors: TMetadataLoadErrors);
begin
  inherited Create;
  FLoadErrors := LoadErrors;
  FTags := Tags;
  FIndex := AIndex;
  FParser := AParser;
  FParent := AParent;
end;

class function TFoundTiffDirectory.TryCreate(const AParser: ITiffParser;
  const AParent: ITiffDirectory; AIndex: Integer; const AOffset: Int64; out Instance: IFoundTiffDirectory): Boolean;
var
  Tags: TTiffTagInfoDynArray;
  LoadErrors: TMetadataLoadErrors;
begin
  Tags := LoadTiffDirectory(AParser.Stream, AParser.Endianness,
    AParser.BasePosition, AOffset, 0, LoadErrors);
  Result := (LoadErrors = []) or (Length(Tags) > 2);
  if Result then Instance := TFoundTiffDirectory.Create(AParser, AParent, AIndex, Tags, LoadErrors);
end;

function TFoundTiffDirectory.GetEnumerator: ITiffDirectoryEnumerator;
begin
  Result := TFoundTiffDirectoryEnumerator.Create(Self);
end;

function TFoundTiffDirectory.GetIndex: Integer;
begin
  Result := FIndex;
end;

function TFoundTiffDirectory.GetLoadErrors: TMetadataLoadErrors;
begin
  Result := FLoadErrors;
end;

function TFoundTiffDirectory.GetParent: ITiffDirectory;
begin
  Result := FParent;
end;

function TFoundTiffDirectory.GetParser: ITiffParser;
begin
  Result := FParser;
end;

function TFoundTiffDirectory.LoadSubDirectory(OffsetTagID: TTiffTagID): ITiffDirectory;
var
  ParsedDir: IFoundTiffDirectory;
  TagIndex: Integer;
begin
  if FindTag(OffsetTagID, TagIndex) and FParser.ParseSubDirectory(Self, FTags[TagIndex], ParsedDir) then
    Result := ParsedDir
  else
    raise EInvalidTiffData.CreateRes(@SInvalidOffsetTag);
end;

function TFoundTiffDirectory.GetTagCount: Integer;
begin
  Result := Length(FTags);
end;

function TFoundTiffDirectory.GetTagInfo: TTiffTagInfoDynArray;
begin
  Result := FTags;
end;

function TFoundTiffDirectory.IndexOfTag(TagID: TTiffTagID): Integer;
begin
  for Result := 0 to High(FTags) do
    if FTags[Result].ID > TagID then
      Break
    else if FTags[Result].ID = TagID then
      Exit;
  Result := -1;
end;

function TFoundTiffDirectory.FindTag(TagID: TTiffTagID; out Index: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FTags) do
    if FTags[I].ID >= TagID then
    begin
      Index := I;
      Result := (FTags[I].ID = TagID);
      Exit;
    end;
  Index := Length(FTags);
  Result := False;
end;

function TFoundTiffDirectory.FindTag(TagID: TTiffTagID; out ParsedTag: ITiffTag): Boolean;
var
  Index: Integer;
begin
  Result := FindTag(TagID, Index);
  if Result then ParsedTag := TFoundTiffTag.Create(Self, Index);
end;

function TFoundTiffDirectory.IsExifThumbailDirectory: Boolean;
begin
  Result := (FIndex = 1) and IsLastDirectoryInFile and
    TagExists(ttThumbnailOffset, TTiffTag.OffsetDataTypes, 1, 1) and
    not TagExists(ttStripOffsets) and not TagExists(ttTileOffsets);
end;

function TFoundTiffDirectory.IsLastDirectoryInFile: Boolean;
begin
  Result := not FMoreToFollow;
end;

function TFoundTiffDirectory.TagExists(TagID: TTiffTagID; ValidDataTypes: TTiffDataTypes;
  MinElementCount, MaxElementCount: LongInt): Boolean;
var
  Index: Integer;
begin
  Result := FindTag(TagID, Index) and (FTags[Index].DataType in ValidDataTypes) and
    (FTags[Index].ElementCount >= MinElementCount) and
    (FTags[Index].ElementCount <= MaxElementCount);
end;

function TFoundTiffDirectory.TryLoadExifThumbnail(Dest: TStream): Boolean;
var
  Index: Integer;
begin
  Result := FindTag(ttThumbnailOffset, Index) and
    SeekToExifThumbnail(FParser, FTags[Index]);
  if Result then
    Dest.CopyFrom(FParser.Stream, GetJPEGDataSize(FParser.Stream));
end;

function TFoundTiffDirectory.TryLoadExifThumbnail(const Dest: IStreamPersist): Boolean;
var
  Index: Integer;
begin
  Result := FindTag(ttThumbnailOffset, Index) and
    SeekToExifThumbnail(FParser, FTags[Index]);
  if Result then Result := TryLoadJpegImageFromStream(Dest, FParser.Stream);
end;

{ TTiffParserEnumerator }

constructor TTiffParserEnumerator.Create(const ASource: ITiffParser;
  const AMaxPossibleOffset: Int64; const ANextOffset: LongInt);
begin
  FSource := ASource;
  FMaxPossibleOffset := AMaxPossibleOffset;
  FNextIndex := 0;
  FNextOffset := ANextOffset;
end;

function TTiffParserEnumerator.MoveNext: Boolean;
var
  NewDir: TFoundTiffDirectory;
begin
  Result := (FNextOffset <> 0);
  if not Result then Exit;
  NewDir := TFoundTiffDirectory.Create(FSource, nil, FNextIndex, FNextOffset);
  NewDir.FMoreToFollow := FSource.Stream.ReadLongInt(FSource.Endianness,
    FNextOffset) and (FNextOffset <> 0) and (FNextOffset <= FMaxPossibleOffset);
  if NewDir.FMoreToFollow then
    Inc(FNextIndex)
  else
    FNextOffset := 0;
  FCurrent := NewDir;
end;

{ TTiffParser }

constructor TTiffParser.Create(AStream: TStream; AStreamOwnership: TStreamOwnership);
begin
  inherited Create;
  FBasePosition := AStream.Position;
  FStream := AStream;
  FStreamOwnership := AStreamOwnership;
  if not HasTiffHeader(AStream, FEndianness, True) or not AStream.ReadLongInt(FEndianness, FFirstDirOffset) then
    raise EInvalidTiffData.CreateRes(@SInvalidTiffData);
  FMaxOffsetValue := AStream.Size - FBasePosition - 4; //4 is the minimum size of an IFD (word-sized tag count + word-sized next IFD offset)
end;

constructor TTiffParser.Create(AStream: TStream; const ABasePosition: Int64;
  AEndianness: TEndianness);
begin
  inherited Create;
  FBasePosition := ABasePosition;
  FEndianness := AEndianness;
  FFirstDirOffset := 0;
  FStream := AStream;
  FStreamOwnership := soReference;
  FMaxOffsetValue := AStream.Size - FBasePosition - 4; //4 is the minimum size of an IFD (word-sized tag count + word-sized next IFD offset)
end;

destructor TTiffParser.Destroy;
begin
  if FStreamOwnership = soOwned then FreeAndNil(FStream);
  inherited;
end;

function TTiffParser.GetBasePosition: Int64;
begin
  Result := FBasePosition;
end;

function TTiffParser.GetEndianness: TEndianness;
begin
  Result := FEndianness;
end;

function TTiffParser.GetEnumerator: TTiffParserEnumerator;
begin
  Result := TTiffParserEnumerator.Create(Self, FMaxOffsetValue, FFirstDirOffset);
end;

function TTiffParser.GetStream: TStream;
begin
  Result := FStream;
end;

procedure TTiffParser.LoadTagData(const TagInfo: TTiffTagInfo; var Buffer);
var
  Size: Integer;
  I: Integer;
begin
  Size := TagInfo.DataSize;
  if Size = 0 then Exit;
  Stream.Position := BasePosition + TagInfo.DataOffset;
  case TiffElementSizes[TagInfo.DataType] of
    1: Stream.ReadBuffer(Buffer, Size);
    2:
      for I := 0 to TagInfo.ElementCount - 1 do
        PWordArray(@Buffer)[I] := Stream.ReadWord(Endianness);
  else
    if TagInfo.DataType = tdDouble then
      for I := 0 to TagInfo.ElementCount - 1 do
        PDoubleArray(@Buffer)[I] := Stream.ReadDouble(Endianness)
    else
      for I := 0 to (Size div 4) - 1 do
        PLongWordArray(@Buffer)[I] := Stream.ReadLongWord(Endianness);
  end;
end;

function PossibleSubIFDOffset(ID: TTiffTagID; DataType: TTiffDataType;
  ElementCount: LongInt): Boolean;
begin
  Result := False;
  if ElementCount = 1 then
    case DataType of
      tdSubDirectory: Result := True;
      tdLongWord:
        case ID of
          ttImageWidth, ttImageHeight, ttStripOffsets, ttStripByteCounts,
          ttTileOffsets, ttTileByteCounts, ttThumbnailOffset, ttThumbnailSize,
          ttExifImageWidth, ttExifImageHeight: {nope};
        else Result := True;
        end;
    end;
end;

function TTiffParser.ParseSubDirectory(const OffsetTag: ITiffTag;
  out Directory: IFoundTiffDirectory): Boolean;
begin
  Result := PossibleSubIFDOffset(OffsetTag.ID, OffsetTag.DataType, OffsetTag.ElementCount) and
    TFoundTiffDirectory.TryCreate(Self, OffsetTag.Parent, OffsetTag.ID,
      LoadOffsetArray(OffsetTag)[0], Directory);
end;

function TTiffParser.ParseSubDirectory(const Parent: ITiffDirectory;
  const OffsetTag: TTiffTagInfo; out Directory: IFoundTiffDirectory): Boolean;
begin
  Result := PossibleSubIFDOffset(OffsetTag.ID, OffsetTag.DataType, OffsetTag.ElementCount) and
    TFoundTiffDirectory.TryCreate(Self, Parent, OffsetTag.ID,
      LoadOffsetArray(Self, OffsetTag)[0], Directory);
end;

{ TTiffDirectoryRewriter.TTagToWrite }

constructor TTiffDirectoryRewriter.TTagToWrite.Create(AParent: TTiffDirectoryRewriter;
  const AInfo: TTiffTagInfo);
begin
  inherited Create;
  FInfo := AInfo;
  FParent := AParent;
end;

constructor TTiffDirectoryRewriter.TTagToWrite.Create(AParent: TTiffDirectoryRewriter;
  AID: TTiffTagID; ADataType: TTiffDataType; AElementCount: LongInt;
  AOriginalDataOffset: LongWord = 0);
begin
  inherited Create;
  FInfo.DataOffset := AOriginalDataOffset;
  FInfo.ID := AID;
  FInfo.DataType := ADataType;
  FInfo.ElementCount := AElementCount;
  FParent := AParent;
end;

function TTiffDirectoryRewriter.TTagToWrite.DataSize: Integer;
begin
  Result := FInfo.DataSize;
end;

{ TInternaTiffTagToWrite }

constructor TInternaTiffTagToWrite.Create(AParent: TTiffDirectoryRewriter;
  const AParser: ITiffParser; const ATagInfo: TTiffTagInfo);

  procedure LoadImageInfo(OffsetID: TTiffTagID);
  var
    BadOffset, Item: LongWord;
    Offsets: TLongWordDynArray;
    OffsetTag: TTiffDirectoryRewriter.TTagToWrite;
  begin //the byte counts tag should always come after the offsets one
    if AParent.HasImageData then Exit;
    if not AParent.FindTagToWrite(OffsetID, OffsetTag) or
       not (OffsetTag is TInternaTiffTagToWrite) then Exit;
    if (OffsetTag.ElementCount <> ATagInfo.ElementCount) or
       not (OffsetTag.DataType in TTiffTag.OffsetDataTypes) or
       not (ATagInfo.DataType in [tdWord, tdLongWord, tdLongInt]) then Exit;
    BadOffset := LongWord(AParser.Stream.Size - AParser.BasePosition);
    Offsets := LoadOffsetArray(AParser, OffsetTag.Info);
    for Item in Offsets do
      if (Item < 8) or (Item >= BadOffset) then Exit;
    AParent.InitializeImageInfo(Offsets, LoadOffsetArray(AParser, ATagInfo));
    TInternaTiffTagToWrite(OffsetTag).Kind := itImageOffsets;
    Self.Kind := itImageByteCounts;
  end;
begin
  inherited Create(AParent, ATagInfo);
  FParser := AParser;
  case ID of
    ttStripByteCounts: LoadImageInfo(ttStripOffsets);
    ttTileByteCounts: LoadImageInfo(ttTileOffsets);
    ttThumbnailSize: LoadImageInfo(ttThumbnailOffset);
  end;
end;

procedure TInternaTiffTagToWrite.WriteData(Stream: TStream; Endianness: TEndianness);
var
  Data: TLongWordDynArray;
  I: Integer;
begin
  inherited;
  case Kind of
    itImageOffsets:
      for I := 0 to High(Parent.ImageInfo) do
        Stream.WriteLongWord(Parent.ImageInfo[I].NewOffset, Endianness);
    itImageByteCounts:
      for I := 0 to High(Parent.ImageInfo) do
        Stream.WriteLongWord(Parent.ImageInfo[I].ByteCount, Endianness);
  else
    SetLength(Data, (Info.DataSize + 3) div 4);
    if Data = nil then Exit;
    FParser.LoadTagData(Info, Data[0]);
    WriteTiffTagData(DataType, ElementCount, Data, Stream, Endianness);
  end;
end;

{ TExternalTiffTagToWrite }

constructor TExternalTiffTagToWrite.Create(AOwner: TTiffDirectoryRewriter;
  const ATag: ITiffTag);
begin
  inherited Create(AOwner, ATag.ID, ATag.DataType, ATag.ElementCount, ATag.OldDataOffset);
  FTag := ATag;
end;

constructor TExternalTiffTagToWrite.Create(AOwner: TTiffDirectoryRewriter;
  AID: TTiffTagID; ADataType: TTiffDataType; const ADataSource: IStreamPersist);
begin
  Create(AOwner, TFoundTiffTag.Create(AID, ADataType, ADataSource));
end;

procedure TExternalTiffTagToWrite.WriteData(Stream: TStream; Endianness: TEndianness);
var
  ElemCount: Integer;
begin
  inherited;
  ElemCount := Min(ElementCount, FTag.Data.Size div TiffElementSizes[DataType]);
  WriteTiffTagData(DataType, ElemCount, FTag.Data.Memory, Stream, Endianness);
end;

{ TTiffSubDirToWrite }

constructor TTiffSubDirToWrite.Create(AOwner: TTiffDirectoryRewriter;
  AOffsetID: TTiffTagID; ADirectory: TTiffDirectoryRewriter);
begin
  inherited Create(AOwner, AOffsetID, tdLongWord, 1);
  FDirectory := ADirectory;
end;

destructor TTiffSubDirToWrite.Destroy;
begin
   FreeAndNil(FDirectory);
  inherited;
end;

procedure TTiffSubDirToWrite.WriteData(Stream: TStream; Endianness: TEndianness);
begin
  Stream.WriteLongWord(Directory.NewIFDOffset, Endianness);
end;

{ TTiffDirectoryRewriter }

constructor TTiffDirectoryRewriter.Create(const Source: ITiffDirectory;
  const Callback: ITiffRewriteCallback);

  function RewriteOldTag(TagID: TTiffTagID; DataType: TTiffDataType;
    ElementCount: LongInt; out Index: Integer): Boolean;
  begin
    Result := TTiffTag.IsWellFormed(DataType, ElementCount) and
      not FindTagIndex(TagID, Index);
    if Result and (Callback <> nil) then
      Callback.RewritingOldTag(Source, TagID, DataType, Result);
  end;
var
  ExtTag: ITiffTag;
  I, TagIndex: Integer;
  IntDirectory: IFoundTiffDirectory;
  SubDir: IFoundTiffDirectory;
  SubRewriter: TTiffDirectoryRewriter;
  TagPtr: PTiffTagInfo;
begin
  inherited Create;
  FSource := Source;
  FTagsToWrite := TObjectList.Create(True);
  FRewriteSource := True;
  if Callback <> nil then Callback.AddNewTags(Self);
  FDoneInitialization := True;
  if not FRewriteSource or (Source = nil) then Exit;
  if Supports(Source, IFoundTiffDirectory, IntDirectory) then
  begin
    FSourceInfo := IntDirectory.TagInfo;
    FSourceParser := IntDirectory.Parser;
    TagPtr := PTiffTagInfo(FSourceInfo);
    for I := 0 to High(FSourceInfo) do
    begin
      if RewriteOldTag(TagPtr.ID, TagPtr.DataType, TagPtr.ElementCount, TagIndex) then
        if not FSourceParser.ParseSubDirectory(Source, TagPtr^, SubDir) then
          FTagsToWrite.Insert(TagIndex, TInternaTiffTagToWrite.Create(Self, FSourceParser, TagPtr^))
        else
        begin
          SubRewriter := TTiffDirectoryRewriter.Create(SubDir, Callback);
          if SubRewriter.RewriteSource then
            FTagsToWrite.Insert(TagIndex, TTiffSubDirToWrite.Create(Self, TagPtr.ID, SubRewriter))
          else
             FreeAndNil(SubRewriter);
        end;
      Inc(TagPtr);
    end;
  end
  else
    for ExtTag in Source do
      if RewriteOldTag(ExtTag.ID, ExtTag.DataType, ExtTag.ElementCount, TagIndex) then
        FTagsToWrite.Insert(TagIndex, TExternalTiffTagToWrite.Create(Self, ExtTag));
end;

destructor TTiffDirectoryRewriter.Destroy;
begin
   FreeAndNil(FTagsToWrite);
  inherited;
end;

procedure TTiffDirectoryRewriter.AddTag(Tag: TTagToWrite);
var
  Index: Integer;
begin
  if FDoneInitialization then
    raise EInvalidOperation.Create('Tags can only be added during AddTagsToWrite');
  if not FindTagIndex(Tag.ID, Index) then
    FTagsToWrite.Insert(Index, Tag)
  else
  begin
    Index := Tag.ID;
     FreeAndNil(Tag);
    raise ETagAlreadyExists.CreateResFmt(@STagAlreadyExists, [Index]);
  end;
end;

procedure TTiffDirectoryRewriter.AddTag(const Source: ITiffTag);
begin
  AddTag(TExternalTiffTagToWrite.Create(Self, Source));
end;

procedure TTiffDirectoryRewriter.AddTag(ID: TTiffTagID; const DataSource: IStreamPersist;
  DataType: TTiffDataType = tdUndefined);
begin
  AddTag(TExternalTiffTagToWrite.Create(Self, ID, DataType, DataSource));
end;

procedure TTiffDirectoryRewriter.AddSubDirectory(OffsetID: TTiffTagID;
  const SubDirectory: ITiffDirectory; const Callback: ITiffRewriteCallback = nil);
var
  Rewriter: TTiffDirectoryRewriter;
begin
  Rewriter := TTiffDirectoryRewriter.Create(SubDirectory, Callback);
  if Rewriter.RewriteSource then
    AddTag(TTiffSubDirToWrite.Create(Self, OffsetID, Rewriter))
  else
     FreeAndNil(Rewriter);
end;

function TTiffDirectoryRewriter.FindTagIndex(AID: TTiffTagID; out Index: Integer): Boolean;
var
  ExistingID: TTiffTagID;
  I: Integer;
begin
  for I := FTagsToWrite.Count - 1 downto 0 do
  begin
    ExistingID := TTagToWrite(FTagsToWrite[I]).ID;
    if AID > ExistingID then
    begin
      Index := I + 1;
      Result := False;
      Exit;
    end;
    if AID = ExistingID then
    begin
      Index := I;
      Result := True;
      Exit;
    end;
  end;
  Index := 0;
  Result := False;
end;

function TTiffDirectoryRewriter.FindTagToWrite(ID: TTiffTagID; var Tag: TTagToWrite): Boolean;
var
  Index: Integer;
begin
  Result := FindTagIndex(ID, Index);
  if Result then Tag := TTagToWrite(FTagsToWrite[Index]);
end;

function TTiffDirectoryRewriter.GetTag(Index: Integer): TTagToWrite;
begin
  Result := TTagToWrite(FTagsToWrite[Index]);
end;

function TTiffDirectoryRewriter.GetTagCount: Integer;
begin
  Result := FTagsToWrite.Count;
end;

procedure TTiffDirectoryRewriter.IgnoreDirectory;
begin
  FRewriteSource := False;
end;

function TTiffDirectoryRewriter.HasImageData: Boolean;
begin
  Result := FImageInfo <> nil;
end;

function TTiffDirectoryRewriter.ProtectMakerNote(var MakerNoteTag: TTagToWrite): Boolean;
var
  SubDir: TTiffDirectoryRewriter;
  Index: Integer;
  OffsetValue: LongInt;
  RevisedOffset: Int64;
  Tag: TTagToWrite;
begin
  Result := False;
  if not FindTagToWrite(ttExifOffset, Tag) or not (Tag is TTiffSubDirToWrite) then Exit;
  SubDir := TTiffSubDirToWrite(Tag).Directory;
  if not SubDir.FindTagToWrite(ttMakerNote, Tag) or (Tag.DataType <> tdUndefined) or
    (Tag.ElementCount <= 4) or (Tag.OldDataOffset < 8) then Exit;
  Tag.NewDataOffset := Tag.OldDataOffset;
  MakerNoteTag := Tag;
  Result := True;
  if SubDir.FindTagIndex(ttOffsetSchema, Index) then //Aim to move the maker note back
  begin                                              //if an MS application such as the
    Tag := TTagToWrite(SubDir.FTagsToWrite[Index]);  //Windows shell has moved it.
    if (Tag.DataType = tdLongInt) and (Tag.ElementCount = 1) then
    begin
      if Tag is TExternalTiffTagToWrite then
        OffsetValue := PLongInt(TExternalTiffTagToWrite(Tag).Source.Data.Memory)^
      else if Tag is TInternaTiffTagToWrite then
        TInternaTiffTagToWrite(Tag).Parser.LoadTagData(Tag.Info, OffsetValue)
      else
        Exit;
      RevisedOffset := Int64(MakerNoteTag.OldDataOffset) + OffsetValue;
      if not InRange(RevisedOffset, 8, $FFFFFFF0) then Exit;
      MakerNoteTag.NewDataOffset := LongWord(RevisedOffset);
      SubDir.FTagsToWrite.Delete(Index);
    end;
  end;
end;

procedure TTiffDirectoryRewriter.InitializeImageInfo(const Offsets, ByteCounts: array of LongWord);
var
  I: Integer;
begin
  Assert((FSourceParser <> nil) and (Length(Offsets) = Length(ByteCounts)));
  SetLength(FImageInfo, Length(Offsets));
  for I := High(FImageInfo) downto 0 do
    with FImageInfo[I] do
    begin
      ByteCount := ByteCounts[I];
      OldOffset := Offsets[I];
    end;
end;

procedure TTiffDirectoryRewriter.InitializeNewOffsets(var NextOffset: LongWord;
  ProtectedTag: TTagToWrite);
var
  ProtectedTagToCome: Boolean;

  function FixupNextOffset(const DataSize: LongWord): LongWord;
  begin
    if ProtectedTagToCome and (NextOffset + DataSize >= ProtectedTag.NewDataOffset) then
    begin
      NextOffset := ProtectedTag.NewDataOffset + LongWord(ProtectedTag.ElementCount);
      ProtectedTagToCome := False;
    end;
    Result := NextOffset;
    Inc(NextOffset, DataSize);
  end;
var
  I: Integer;
  Tag: TTagToWrite;
begin
  ProtectedTagToCome := (ProtectedTag <> nil) and
    (NextOffset < ProtectedTag.NewDataOffset + LongWord(ProtectedTag.ElementCount));
  FNewIFDOffset := FixupNextOffset(2 + TTiffTag.HeaderSize * FTagsToWrite.Count + 4);
  for I := 0 to FTagsToWrite.Count - 1 do
  begin
    Tag := TTagToWrite(FTagsToWrite[I]);
    if Tag = ProtectedTag then Continue; //assume its NewDataOffset property has already been set
    if Tag.DataSize > 4 then
      Tag.NewDataOffset := FixupNextOffset(Tag.DataSize)
    else
    begin
      Tag.NewDataOffset := FNewIFDOffset + LongWord(2 + I * TTiffTag.HeaderSize + 8);
      if Tag is TTiffSubDirToWrite then
        TTiffSubDirToWrite(Tag).Directory.InitializeNewOffsets(NextOffset, ProtectedTag);
    end;
  end;
  for I := 0 to High(FImageInfo) do
    FImageInfo[I].NewOffset := FixupNextOffset(FImageInfo[I].ByteCount);
end;

procedure TTiffDirectoryRewriter.Write(AStream: TStream; const ABasePosition: Int64;
  AEndianness: TEndianness; ANextIFDOffset: LongWord);
var
  Buffer: TBytes;
  I: Integer;
  Tag: TTagToWrite;
begin
  //write the IFD
  AStream.Seek(ABasePosition + NewIFDOffset, soBeginning);
  AStream.WriteWord(FTagsToWrite.Count, AEndianness);
  for I := 0 to FTagsToWrite.Count - 1 do
  begin
    Tag := TTagToWrite(FTagsToWrite[I]);
    AStream.WriteWord(Tag.ID, AEndianness);
    AStream.WriteWord(Ord(Tag.DataType), AEndianness);
    AStream.WriteLongInt(Tag.ElementCount, AEndianness);
    if Tag.DataSize <= 4 then
      Tag.WriteData(AStream, AEndianness)
    else
      AStream.WriteLongWord(Tag.NewDataOffset, AEndianness);
  end;
  AStream.WriteLongWord(ANextIFDOffset, AEndianness);
  //write any offsetted tag data and sub-directories
  for I := 0 to FTagsToWrite.Count - 1 do
  begin
    Tag := TTagToWrite(FTagsToWrite[I]);
    if Tag.DataSize > 4 then
    begin
      AStream.Seek(ABasePosition + Tag.NewDataOffset, soBeginning);
      Tag.WriteData(AStream, AEndianness);
    end
    else if Tag is TTiffSubDirToWrite then
      TTiffSubDirToWrite(Tag).Directory.Write(AStream, ABasePosition, AEndianness, 0);
  end;
  //write image data
  for I := 0 to High(FImageInfo) do
  begin
    if FImageInfo[I].ByteCount <= 0 then Continue;
    if FImageInfo[I].ByteCount > Length(Buffer) then
      SetLength(Buffer, FImageInfo[I].ByteCount);
    with FSourceParser do
    begin
      Stream.Position := BasePosition + FImageInfo[I].OldOffset;
      Stream.ReadBuffer(Buffer[0], FImageInfo[I].ByteCount);
    end;
    AStream.Seek(ABasePosition + FImageInfo[I].NewOffset, soBeginning);
    AStream.WriteBuffer(Buffer[0], FImageInfo[I].ByteCount);
  end;
end;

{ TSimpleTiffRewriteCallbackImpl }

constructor TSimpleTiffRewriteCallbackImpl.Create(const AOwner: IStreamPersistEx;
  AMetadataTagID: TTiffTagID);
begin
  inherited Create(AOwner);
  FMetadataTagID := AMetadataTagID;
end;

function TSimpleTiffRewriteCallbackImpl.GetOwner: IStreamPersistEx;
begin
  Result := IStreamPersistEx(Controller);
end;

procedure TSimpleTiffRewriteCallbackImpl.AddNewTags(Rewriter: TTiffDirectoryRewriter);
begin
  if (Rewriter.Source.Parent = nil) and (Rewriter.Source.Index = 0) and not Owner.Empty then
    Rewriter.AddTag(FMetadataTagID, Owner, tdUndefined);
end;

procedure TSimpleTiffRewriteCallbackImpl.RewritingOldTag(const Source: ITiffDirectory;
  TagID: TTiffTagID; DataType: TTiffDataType; var Rewrite: Boolean);
begin
  if (TagID = FMetadataTagID) and (Source.Parent = nil) or (Source.Index <> 0) then
    Rewrite := False; //for the case of when Owner.Empty returned True and the TIFF file already contained metadata of the kind Owner implements
end;

{ ParseTiff }

function ParseTiff(Stream: TStream; StreamOwnership: TStreamOwnership = soReference): ITiffParser;
begin
  Result := TTiffParser.Create(Stream, StreamOwnership);
end;

function ParseTiff(const FileName: string): ITiffParser;
begin
  Result := ParseTiff(TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite), soOwned);
end;

function ParseTiffDirectory(Stream: TStream; Endianness: TEndianness;
  const BasePosition, Offset, InternalOffset: Int64): IFoundTiffDirectory;
var
  Parser: ITiffParser;
begin
  Parser := TTiffParser.Create(Stream, BasePosition, Endianness);
  Result := TFoundTiffDirectory.Create(Parser, nil, -1, Offset, InternalOffset);
end;

procedure RewriteTiff(InStream, OutStream: TStream; const Callback: ITiffRewriteCallback);
var
  ProtectedTag: TTiffDirectoryRewriter.TTagToWrite;
  NewBasePosition: Int64;
  NewEndianness: TEndianness;
  NextOffset: LongWord;
var
  Directory: IFoundTiffDirectory;
  I: Integer;
  Parser: ITiffParser;
  Rewriter: TTiffDirectoryRewriter;
  Rewriters: TObjectList;

  function GetRewriter(Index: Integer): TTiffDirectoryRewriter;
  begin
    Result := TTiffDirectoryRewriter(Rewriters[Index]);
  end;
begin
  NewBasePosition := OutStream.Position;
  Parser := TTiffParser.Create(InStream, soReference);
  NewEndianness := Parser.Endianness;
  Rewriters := TObjectList.Create(True);
  try
    for Directory in Parser do
    begin
      Rewriter := TTiffDirectoryRewriter.Create(Directory, Callback);
      if Rewriter.RewriteSource then
        Rewriters.Add(Rewriter)
      else
         FreeAndNil(Rewriter);
    end;
    if Rewriters.Count = 0 then
      WriteTiffHeader(OutStream, NewEndianness, 0)
    else
    begin
      if not GetRewriter(0).ProtectMakerNote(ProtectedTag) then
        ProtectedTag := nil;
      NextOffset := 8; //2 byte endianness code + 2 byte magic num + 4 byte next IFD pointer
      for I := 0 to Rewriters.Count - 1 do
        GetRewriter(I).InitializeNewOffsets(NextOffset, ProtectedTag);
      WriteTiffHeader(OutStream, NewEndianness, GetRewriter(0).NewIFDOffset);
      if NewBasePosition + NextOffset > OutStream.Size then
        OutStream.Size := NewBasePosition + NextOffset;
      for I := 0 to Rewriters.Count - 2 do
        GetRewriter(I).Write(OutStream, NewBasePosition, NewEndianness,
          GetRewriter(I + 1).NewIFDOffset);
      GetRewriter(Rewriters.Count - 1).Write(OutStream, NewBasePosition, NewEndianness, 0);
      OutStream.Seek(NewBasePosition + NextOffset, soBeginning);
    end;
  finally
     FreeAndNil(Rewriters);
  end;
end;

procedure WriteTiffHeader(Stream: TStream; Endianness: TEndianness;
  FirstDirectoryOffset: LongWord = 8);
begin
  if Endianness = SmallEndian then
  begin
    Stream.WriteWord(TiffSmallEndianCode, SmallEndian);
    Stream.WriteWord(TiffMagicNum, SmallEndian);
  end
  else
  begin
    Stream.WriteWord(TiffBigEndianCode, SmallEndian);
    Stream.WriteWord(TiffMagicNum, BigEndian);
  end;
  Stream.WriteLongWord(FirstDirectoryOffset, Endianness);
end;

end.
