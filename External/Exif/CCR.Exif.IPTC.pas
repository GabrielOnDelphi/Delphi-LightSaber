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
{ The Original Code is CCR.Exif.IPTC.pas.                                              }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2014 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

{$I CCR.Exif.inc}
unit CCR.Exif.IPTC;
{
  As saved, IPTC data is a flat list of tags ('datasets'), no more no less, which is
  reflected in the implementation of TIPTCData.LoadFromStream. However, as found in JPEG
  files, they are put in an Adobe data structure, itself put inside an APP13 segment.

  Note that by default, string tags are 'only' interpreted as having UTF-8 data if the
  encoding tag is set, with the UTF-8 marker as its data. If you don't load any tags
  before adding others, however, the default is to persist to UTF-8, writing said marker
  tag of course. To force interpreting loaded tags as UTF-8, set the
  AlwaysAssumeUTF8Encoding property of TIPTCData to True *before* calling
  LoadFromGraphic or LoadFromStream.
}
interface

uses
  Types, SysUtils, Classes, {$IFDEF HasGenerics}Generics.Collections, Generics.Defaults,{$ENDIF}
  {$IFDEF VCL}Vcl.Imaging.Jpeg,{$ENDIF} CCR.Exif.BaseUtils, CCR.Exif.TagIDs, CCR.Exif.TiffUtils;

type
  EInvalidIPTCData = class(ECCRExifException);

{$IFDEF HasGenerics}
  TIPTCRepeatablePair = TPair<string, string>;

  TIPTCRepeatablePairs = TArray<TIPTCRepeatablePair>;
{$ELSE}
  TIPTCRepeatablePair = record
    Key, Value: string;
  end;

  TIPTCRepeatablePairs = array of TIPTCRepeatablePair;
{$ENDIF}

  TIPTCStringArray = TStringDynArray;

  TIPTCData = class;
  TIPTCSection = class;

  TIPTCTagID = CCR.Exif.BaseUtils.TIPTCTagID;
  TIPTCTagIDs = set of TIPTCTagID;

  TIPTCTag = class //an instance represents a 'dataset' in IPTC jargon; instances need not be written in numerical order
  public type
    TChangeType = (ctID, ctData);
  private
    FData: Pointer;
    FDataSize: Integer;
    FID: TIPTCTagID;
    FSection: TIPTCSection;
    procedure SetDataSize(Value: Integer);
    procedure SetID(const Value: TIPTCTagID);
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetIndex: Integer;
    procedure SetIndex(NewIndex: Integer);
  public
    destructor Destroy; override;
    procedure Assign(Source: TIPTCTag);
    procedure Changed(AType: TChangeType = ctData); overload; //call this if Data is modified directly
    procedure Delete;
    procedure UpdateData(const Buffer); overload; inline;
    procedure UpdateData(NewDataSize: Integer; const Buffer); overload;
    procedure UpdateData(NewDataSize: Integer; Source: TStream); overload;
    { ReadString treats the data as string data, whatever the spec says. It respects
      TIPTCData.UTF8Encoded however. }
    function ReadString: string;
    { ReadUTF8String just assumes the tag data is UTF-8 text. }
    function ReadUTF8String: UTF8String; inline;
    procedure WriteString(const NewValue: RawByteString); overload;
    procedure WriteString(const NewValue: UnicodeString); overload;
    { AsString assumes the underlying data type is as per the spec (unlike the case of
      Exif tag headers, IPTC ones do not specify their data type). }
    property AsString: string read GetAsString write SetAsString;
    property Data: Pointer read FData;
    property DataSize: Integer read FDataSize write SetDataSize;
    property ID: TIPTCTagID read FID write SetID; //tag IDs need only be unique within sections 1, 7, 8 and 9
    property Index: Integer read GetIndex write SetIndex;
    property Section: TIPTCSection read FSection;
  end;

  TIPTCSectionID = CCR.Exif.BaseUtils.TIPTCSectionID;

{$Z1} //only TIPTCImageOrientation directly maps to the stored value
  TIPTCActionAdvised = (iaTagMissing, iaObjectKill, iaObjectReplace, iaObjectAppend, iaObjectReference);
  TIPTCImageOrientation = (ioTagMissing, ioLandscape = Ord('L'), ioPortrait = Ord('P'),
    ioSquare = Ord('S'));
  TIPTCPriority = (ipTagMissing, ipLowest, ipVeryLow, ipLow, ipNormal, ipNormalHigh,
    ipHigh, ipVeryHigh, ipHighest, ipUserDefined, ipReserved);

  TIPTCSection = class //an instance represents a 'record' in IPTC jargon
  public type
    TEnumerator = record
    strict private
      FIndex: Integer;
      FSource: TIPTCSection;
      function GetCurrent: TIPTCTag;
    public
      constructor Create(ASection: TIPTCSection);
      function MoveNext: Boolean;
      property Current: TIPTCTag read GetCurrent;
    end;
  private type
    {$IFDEF HasGenerics}
    TTagList = class(TObjectList<TIPTCTag>)
      constructor Create;
    end;
    {$ELSE}
    TTagList = class(TList)
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    public
      procedure Sort;
    end;
    {$ENDIF}
  private
    FDefinitelySorted: Boolean;
    FID: TIPTCSectionID;
    FModified: Boolean;
    FOwner: TIPTCData;
    FTags: TTagList;
    function GetTag(Index: Integer): TIPTCTag;
    function GetTagCount: Integer;
  public
    constructor Create(AOwner: TIPTCData; AID: TIPTCSectionID);
    destructor Destroy; override;
    function GetEnumerator: TEnumerator;
    function Add(ID: TIPTCTagID): TIPTCTag; //will try to insert in an appropriate place
    function Append(ID: TIPTCTagID): TIPTCTag; //will literally just append
    procedure Clear;
    procedure Delete(Index: Integer);
    function Find(ID: TIPTCTagID; out Tag: TIPTCTag): Boolean;
    function Insert(Index: Integer; ID: TIPTCTagID = 0): TIPTCTag;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(TagID: TIPTCTagID): Integer; overload; inline;
    function Remove(TagIDs: TIPTCTagIDs): Integer; overload;
    procedure Sort;
    function TagExists(ID: TIPTCTagID; MinSize: Integer = 1): Boolean;
    function AddOrUpdate(TagID: TIPTCTagID; NewDataSize: LongInt; const Buffer): TIPTCTag;
    function GetDateValue(TagID: TIPTCTagID): TDateTimeTagValue;
    procedure SetDateValue(TagID: TIPTCTagID; const Value: TDateTimeTagValue);
    function GetDateTimeValue(DateTagID, TimeTagID: TIPTCTagID; AsUtc: Boolean = False): TDateTimeTagValue;
    procedure SetDateTimeValue(DateTagID, TimeTagID: TIPTCTagID;
      const Value: TDateTimeTagValue; AheadOfUtc: Boolean; UtcOffsetHrs, UtcOffsetMins: Byte); overload;
    {$IFDEF HasTTimeZone}
    procedure SetDateTimeValue(DateTagID, TimeTagID: TIPTCTagID; const Value: TDateTimeTagValue); overload;
    {$ENDIF}
    function GetPriorityValue(TagID: TIPTCTagID): TIPTCPriority;
    procedure SetPriorityValue(TagID: TIPTCTagID; Value: TIPTCPriority);
    function GetRepeatableValue(TagID: TIPTCTagID): TIPTCStringArray; overload;
    procedure GetRepeatableValue(TagID: TIPTCTagID; Dest: TStrings; ClearDestFirst: Boolean = True); overload;
    procedure SetRepeatableValue(TagID: TIPTCTagID; const Value: array of string); overload;
    procedure SetRepeatableValue(TagID: TIPTCTagID; Value: TStrings); overload;
    procedure GetRepeatablePairs(KeyID, ValueID: TIPTCTagID; Dest: TStrings; ClearDestFirst: Boolean = True); overload;
    procedure SetRepeatablePairs(KeyID, ValueID: TIPTCTagID; Pairs: TStrings); overload;
    function GetRepeatablePairs(KeyID, ValueID: TIPTCTagID): TIPTCRepeatablePairs; overload;
    procedure SetRepeatablePairs(KeyID, ValueID: TIPTCTagID; const Pairs: TIPTCRepeatablePairs); overload;
    function GetStringValue(TagID: TIPTCTagID): string;
    procedure SetStringValue(TagID: TIPTCTagID; const Value: string);
    function GetWordValue(TagID: TIPTCTagID): TWordTagValue;
    procedure SetWordValue(TagID: TIPTCTagID; const Value: TWordTagValue);
    property Count: Integer read GetTagCount;
    property ID: TIPTCSectionID read FID;
    property Modified: Boolean read FModified write FModified;
    property Owner: TIPTCData read FOwner;
    property Tags[Index: Integer]: TIPTCTag read GetTag; default;
  end;

  TIPTCData = class(TComponent, IStreamPersist, IStreamPersistEx, ITiffRewriteCallback)
  public type
    TEnumerator = record
    private
      FCurrentID: TIPTCSectionID;
      FDoneFirst: Boolean;
      FSource: TIPTCData;
      function GetCurrent: TIPTCSection;
    public
      constructor Create(ASource: TIPTCData);
      function MoveNext: Boolean;
      property Current: TIPTCSection read GetCurrent;
    end;
  strict private
    FAlwaysAssumeUTF8Encoding: Boolean;
    FDataToLazyLoad: IMetadataBlock;
    FLoadErrors: TMetadataLoadErrors;
    FSections: array[TIPTCSectionID] of TIPTCSection;
    FTiffRewriteCallback: TSimpleTiffRewriteCallbackImpl;
    FUTF8Encoded: Boolean;
    procedure GetGraphicSaveMethod(Stream: TStream; var Method: TGraphicSaveMethod);
    function GetModified: Boolean;
    function GetSection(ID: Integer): TIPTCSection;
    function GetSectionByID(ID: TIPTCSectionID): TIPTCSection;
    function GetUTF8Encoded: Boolean;
    procedure SetDataToLazyLoad(const Value: IMetadataBlock);
    procedure SetModified(Value: Boolean);
    procedure SetUTF8Encoded(Value: Boolean);
    function GetEnvelopeString(TagID: Integer): string;
    procedure SetEnvelopeString(TagID: Integer; const Value: string);
    function GetEnvelopeWord(TagID: Integer): TWordTagValue;
    procedure SetEnvelopeWord(TagID: Integer; const Value: TWordTagValue);
    function GetApplicationWord(TagID: Integer): TWordTagValue;
    procedure SetApplicationWord(TagID: Integer; const Value: TWordTagValue);
    function GetApplicationString(TagID: Integer): string;
    procedure SetApplicationString(TagID: Integer; const Value: string);
    function GetApplicationStrings(TagID: Integer): TIPTCStringArray;
    procedure SetApplicationStrings(TagID: Integer; const Value: TIPTCStringArray);
    function GetApplicationPairs(PackedTagIDs: Integer): TIPTCRepeatablePairs;
    procedure SetApplicationPairs(PackedTagIDs: Integer; const NewPairs: TIPTCRepeatablePairs);
    function GetUrgency: TIPTCPriority;
    procedure SetUrgency(Value: TIPTCPriority);
    function GetEnvelopePriority: TIPTCPriority;
    procedure SetEnvelopePriority(const Value: TIPTCPriority);
    function GetDate(PackedIndex: Integer): TDateTimeTagValue;
    procedure SetDate(PackedIndex: Integer; const Value: TDateTimeTagValue);
    function GetActionAdvised: TIPTCActionAdvised;
    procedure SetActionAdvised(Value: TIPTCActionAdvised);
    function GetDateTimeCreated: TDateTimeTagValue;
    function GetDateTimeSent: TDateTimeTagValue;
    {$IFDEF HasTTimeZone}
    procedure SetDateTimeCreatedProp(const Value: TDateTimeTagValue);
    procedure SetDateTimeSentProp(const Value: TDateTimeTagValue);
    {$ENDIF}
    function GetImageOrientation: TIPTCImageOrientation;
    procedure SetImageOrientation(Value: TIPTCImageOrientation);
  protected
    function CreateAdobeBlock: IAdobeResBlock; inline;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoSaveToJPEG(InStream, OutStream: TStream);
    procedure DoSaveToPSD(InStream, OutStream: TStream);
    procedure DoSaveToTIFF(InStream, OutStream: TStream);
    function GetEmpty: Boolean;
    procedure NeedLazyLoadedData;
    property TiffRewriteCallback: TSimpleTiffRewriteCallbackImpl read FTiffRewriteCallback implements ITiffRewriteCallback;
  public const
    UTF8Marker: array[1..3] of Byte = ($1B, $25, $47);
  public
    constructor Create(AOwner: TComponent = nil); override;
    class function CreateAsSubComponent(AOwner: TComponent): TIPTCData; //use a class function rather than a constructor to avoid compiler warning re C++ accessibility
    destructor Destroy; override;
    function GetEnumerator: TEnumerator;
    procedure AddFromStream(Stream: TStream);
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure SetDateTimeCreated(const Value: TDateTimeTagValue; AheadOfUtc: Boolean;
      UtcOffsetHrs, UtcOffsetMins: Byte); overload;
    procedure SetDateTimeSent(const Value: TDateTimeTagValue; AheadOfUtc: Boolean;
      UtcOffsetHrs, UtcOffsetMins: Byte); overload;
    { Whether or not metadata was found, LoadFromGraphic returns True if the graphic format
      was recognised as one that *could* contain relevant metadata and False otherwise. }
    function LoadFromGraphic(Stream: TStream): Boolean; overload;
    function LoadFromGraphic(const Graphic: IStreamPersist): Boolean; overload;
    function LoadFromGraphic(const FileName: string): Boolean; overload;
    procedure LoadFromStream(Stream: TStream);
    { SaveToGraphic raises an exception if the target graphic either doesn't exist, or
      is neither a JPEG nor a PSD image. }
    procedure SaveToGraphic(const FileName: string); overload;
    procedure SaveToGraphic(const Graphic: IStreamPersist); overload;
    procedure SaveToStream(Stream: TStream);
    procedure SortTags;
    property LoadErrors: TMetadataLoadErrors read FLoadErrors write FLoadErrors; //!!!v. rarely set at present
    property Modified: Boolean read GetModified write SetModified;
    property EnvelopeSection: TIPTCSection index 1 read GetSection;
    property ApplicationSection: TIPTCSection index 2 read GetSection;
    property DataToLazyLoad: IMetadataBlock read FDataToLazyLoad write SetDataToLazyLoad;
    property FirstDescriptorSection: TIPTCSection index 7 read GetSection;
    property ObjectDataSection: TIPTCSection index 8 read GetSection;
    property SecondDescriptorSection: TIPTCSection index 9 read GetSection;
    property Sections[ID: TIPTCSectionID]: TIPTCSection read GetSectionByID; default;
  published
    property AlwaysAssumeUTF8Encoding: Boolean read FAlwaysAssumeUTF8Encoding write FAlwaysAssumeUTF8Encoding default False;
    property Empty: Boolean read GetEmpty;
    property UTF8Encoded: Boolean read GetUTF8Encoded write SetUTF8Encoded default True;
    { record 1 }
    property ModelVersion: TWordTagValue index itModelVersion read GetEnvelopeWord write SetEnvelopeWord stored False;
    property Destination: string index itDestination read GetEnvelopeString write SetEnvelopeString stored False;
    property FileFormat: TWordTagValue index itFileFormat read GetEnvelopeWord write SetEnvelopeWord stored False; //!!!make an enum
    property FileFormatVersion: TWordTagValue index itFileFormatVersion read GetEnvelopeWord write SetEnvelopeWord stored False; //!!!make an enum
    property ServiceIdentifier: string index itServiceIdentifier read GetEnvelopeString write SetEnvelopeString stored False;
    property EnvelopeNumberString: string index itEnvelopeNumber read GetEnvelopeString write SetEnvelopeString stored False;
    property ProductID: string index itProductID read GetEnvelopeString write SetEnvelopeString stored False;
    property EnvelopePriority: TIPTCPriority read GetEnvelopePriority write SetEnvelopePriority stored False;
    property DateSent: TDateTimeTagValue index isEnvelope or itDateSent shl 8 read GetDate write SetDate;
    property TimeSent: string index itTimeSent read GetEnvelopeString write SetEnvelopeString stored False;
    property DateTimeSent: TDateTimeTagValue read GetDateTimeSent {$IFDEF HasTTimeZone}write SetDateTimeSentProp{$ENDIF} stored False;
    property UNOCode: string index itUNO read GetEnvelopeString write SetEnvelopeString stored False; //should have a specific format
    property ARMIdentifier: TWordTagValue index itARMIdentifier read GetEnvelopeWord write SetEnvelopeWord stored False; //!!!make an enum
    property ARMVersion: TWordTagValue index itARMVersion read GetEnvelopeWord write SetEnvelopeWord stored False; //!!!make an enum
    { record 2 }
    property RecordVersion: TWordTagValue index itRecordVersion read GetApplicationWord write SetApplicationWord stored False;
    property ObjectTypeRef: string index itObjectTypeRef read GetApplicationString write SetApplicationString stored False;
    property ObjectAttributeRef: string index itObjectAttributeRef read GetApplicationString write SetApplicationString stored False;
    property ObjectName: string index itObjectName read GetApplicationString write SetApplicationString stored False;
    property EditStatus: string index itEditStatus read GetApplicationString write SetApplicationString stored False;
    property Urgency: TIPTCPriority read GetUrgency write SetUrgency stored False;
    property SubjectRefs: TIPTCStringArray index itSubjectRef read GetApplicationStrings write SetApplicationStrings stored False;
    property CategoryCode: string index itCategory read GetApplicationString write SetApplicationString stored False; //should be a 3 character code
    property SupplementaryCategories: TIPTCStringArray index itSupplementaryCategory read GetApplicationStrings write SetApplicationStrings stored False;
    property FixtureIdentifier: string index itFixtureIdentifier read GetApplicationString write SetApplicationString stored False;
    property Keywords: TIPTCStringArray index itKeyword read GetApplicationStrings write SetApplicationStrings stored False;
    procedure GetKeywords(Dest: TStrings); overload;
    procedure SetKeywords(NewWords: TStrings); overload;
    property ContentLocationCodes: TIPTCStringArray index itContentLocationCode read GetApplicationStrings write SetApplicationStrings stored False;
    property ContentLocationNames: TIPTCStringArray index itContentLocationName read GetApplicationStrings write SetApplicationStrings stored False;
    procedure GetContentLocationValues(Strings: TStrings; ClearDestFirst: Boolean = True); //Code=Name
    procedure SetContentLocationValues(Strings: TStrings);
    property ContentLocations: TIPTCRepeatablePairs index itContentLocationCode or itContentLocationName shl 8 read GetApplicationPairs write SetApplicationPairs;
    property ReleaseDate: TDateTimeTagValue index isApplication or itReleaseDate shl 8 read GetDate write SetDate stored False;
    property ExpirationDate: TDateTimeTagValue index isApplication or itExpirationDate shl 8 read GetDate write SetDate stored False;
    property SpecialInstructions: string index itSpecialInstructions read GetApplicationString write SetApplicationString stored False;
    property ActionAdvised: TIPTCActionAdvised read GetActionAdvised write SetActionAdvised stored False;
    property DateCreated: TDateTimeTagValue index isApplication or itDateCreated shl 8 read GetDate write SetDate stored False;
    property DateTimeCreated: TDateTimeTagValue read GetDateTimeCreated {$IFDEF HasTTimeZone}write SetDateTimeCreatedProp{$ENDIF} stored False;
    property DigitalCreationDate: TDateTimeTagValue index isApplication or itDigitalCreationDate shl 8 read GetDate write SetDate stored False;
    property OriginatingProgram: string index itOriginatingProgram read GetApplicationString write SetApplicationString stored False;
    property ProgramVersion: string index itProgramVersion read GetApplicationString write SetApplicationString stored False;
    property ObjectCycleCode: string index itObjectCycle read GetApplicationString write SetApplicationString stored False; //!!!enum
    property Bylines: TIPTCStringArray index itByline read GetApplicationStrings write SetApplicationStrings stored False;
    property BylineTitles: TIPTCStringArray index itBylineTitle read GetApplicationStrings write SetApplicationStrings stored False;
    procedure GetBylineValues(Strings: TStrings); inline; deprecated {$IFDEF DepCom}'Use GetBylineDetails instead'{$ENDIF};
    procedure SetBylineValues(Strings: TStrings); inline; deprecated {$IFDEF DepCom}'Use SetBylineDetails instead'{$ENDIF};
    procedure GetBylineDetails(Strings: TStrings; ClearDestFirst: Boolean = True); //Name=Title
    procedure SetBylineDetails(Strings: TStrings);
    property BylineDetails: TIPTCRepeatablePairs index itByline or itBylineTitle shl 8 read GetApplicationPairs write SetApplicationPairs;
    property City: string index itCity read GetApplicationString write SetApplicationString stored False; //!!!enum
    property SubLocation: string index itSubLocation read GetApplicationString write SetApplicationString stored False; //!!!enum
    property ProvinceOrState: string index itProvinceOrState read GetApplicationString write SetApplicationString stored False; //!!!enum
    property CountryCode: string index itCountryCode read GetApplicationString write SetApplicationString stored False; //!!!enum
    property CountryName: string index itCountryName read GetApplicationString write SetApplicationString stored False; //!!!enum
    property OriginalTransmissionRef: string index itOriginalTransmissionRef read GetApplicationString write SetApplicationString stored False; //!!!enum
    property Headline: string index itHeadline read GetApplicationString write SetApplicationString stored False;
    property Credit: string index itCredit read GetApplicationString write SetApplicationString stored False;
    property Source: string index itSource read GetApplicationString write SetApplicationString stored False;
    property CopyrightNotice: string index itCopyrightNotice read GetApplicationString write SetApplicationString stored False;
    property Contacts: TIPTCStringArray index itContact read GetApplicationStrings write SetApplicationStrings stored False;
    property CaptionOrAbstract: string index itCaptionOrAbstract read GetApplicationString write SetApplicationString stored False;
    property WritersOrEditors: TIPTCStringArray index itWriterOrEditor read GetApplicationStrings write SetApplicationStrings stored False;
    property ImageTypeCode: string index itImageType read GetApplicationString write SetApplicationString stored False;
    property ImageOrientation: TIPTCImageOrientation read GetImageOrientation write SetImageOrientation stored False;
    property LanguageIdentifier: string index itLanguageIdentifier read GetApplicationString write SetApplicationString stored False;
    property AudioTypeCode: string index itAudioType read GetApplicationString write SetApplicationString stored False;
    { Photoshop aliases }
    procedure GetPSCreatorValues(Strings: TStrings); inline;//Name=Title
    procedure SetPSCreatorValues(Strings: TStrings); inline;
    property PSDocumentTitle: string index itObjectName read GetApplicationString write SetApplicationString stored False;
    property PSCopyrightNotice: string index itCopyrightNotice read GetApplicationString write SetApplicationString stored False;
    property PSDescription: string index itCaptionOrAbstract read GetApplicationString write SetApplicationString stored False;
    property PSCreator: string index itByline read GetApplicationString write SetApplicationString stored False;
    property PSCreatorJobTitle: string index itBylineTitle read GetApplicationString write SetApplicationString stored False;
  end;

implementation

uses
  DateUtils, Math, {$IFDEF HasTTimeZone}TimeSpan,{$ENDIF}
  CCR.Exif.Consts, CCR.Exif.StreamHelper;

const
  PriorityChars: array[TIPTCPriority] of AnsiChar = (#0, '8', '7', '6', '5', '4',
    '3', '2', '1', '9', '0');

type
  TIPTCTagDataType = (idString, idWord, idBinary);

function FindProperDataType(Tag: TIPTCTag): TIPTCTagDataType;
begin
  Result := idString;
  if Tag.Section <> nil then
    case Tag.Section.ID of
      isEnvelope:
        case Tag.ID of
          itModelVersion, itFileFormat, itFileFormatVersion, itARMIdentifier,
          itARMVersion: Result := idWord;
        end;
      isApplication:
        case Tag.ID of
          itRecordVersion: Result := idWord;
        end;
    end;
end;

{ TIPTCStringArrayHelper }

{$IF DECLARED(TIPTCStringArrayHelper)}
class function TIPTCStringArrayHelper.CreateFromStrings(const Strings: TStrings): TIPTCStringArray;
var
  I: Integer;
begin
  SetLength(Result, Strings.Count);
  for I := Strings.Count - 1 downto 0 do
    Result[I] := Strings[I];
end;

function TIPTCStringArrayHelper.Join(const Separator: string): string;
var
  TotalLen: Integer;
  S: string;
  SeekPtr: PChar;
begin
  TotalLen := High(Self) * Length(Separator);
  for S in Self do
    Inc(TotalLen, Length(S));
  SetLength(Result, TotalLen);
  SeekPtr := Pointer(Result);
  for S in Self do
  begin
    MoveChars(Pointer(S)^, SeekPtr^, Length(S));
    Inc(SeekPtr, Length(S) + Length(Separator));
  end;
end;
{$IFEND}



{ TIPTCTag }
destructor TIPTCTag.Destroy;
begin
  if (FSection <> NIL) 
  AND (FSection.FTags <> NIL)  { I had a crash because the second check was missing. Reason: FTags are already freed by TIPTCSection.Destroy }
  then FSection.FTags.Extract(Self);
  ReallocMem(FData, 0);
  inherited;
end;

procedure TIPTCTag.Assign(Source: TIPTCTag);
begin
  if Source = nil then
    SetDataSize(0)
  else
  begin
    FID := Source.ID;
    UpdateData(Source.DataSize, Source.Data^);
  end;
end;

procedure TIPTCTag.Changed(AType: TChangeType);
begin
  if FSection = nil then Exit;
  FSection.Modified := True;
  if AType = ctID then FSection.FDefinitelySorted := False;
end;

procedure TIPTCTag.Delete;
begin
  {$IFDEF NEXTGEN}
  DisposeOf;
  {$ELSE}
  Free;
  {$ENDIF}
end;

function TIPTCTag.GetAsString: string;
begin
  case FindProperDataType(Self) of
    idString: Result := ReadString;
    idBinary: Result := BinToHexStr(FData, FDataSize);
  else
    case DataSize of
      1: Result := IntToStr(PByte(Data)^);
      2: Result := IntToStr(Swap(PWord(Data)^));
      4: Result := IntToStr(SwapLongInt(PLongInt(Data)^));
    else Result := ReadString;
    end;
  end;
end;

function TIPTCTag.GetIndex: Integer;
begin
  if FSection = nil then
    Result := -1
  else
    Result := FSection.FTags.IndexOf(Self);
end;

function TIPTCTag.ReadString: string;
var
  Ansi: AnsiString;
begin
  if (Section <> nil) and (Section.Owner <> nil) and Section.Owner.UTF8Encoded then
  begin
    Result := UTF8ToString(FData, FDataSize);
    Exit;
  end;
  SetString(Ansi, PAnsiChar(FData), FDataSize);
  Result := string(Ansi);
end;

function TIPTCTag.ReadUTF8String: UTF8String;
begin
  SetString(Result, PAnsiChar(FData), FDataSize);
end;

procedure TIPTCTag.SetAsString(const Value: string);
var
  Bytes: TBytes;
  WordVal: Integer;
begin
  case FindProperDataType(Self) of
    idString: WriteString(Value);
    idBinary:
    begin
      Bytes := HexStrToBin(Value);
      SetDataSize(Length(Bytes));
      if FDataSize > 0 then
        Move(Bytes[0], FData^, FDataSize);
    end
  else
    {$RANGECHECKS ON}
    WordVal := StrToInt(Value);
    {$IFDEF RangeCheckingOff}{$RANGECHECKS OFF}{$ENDIF}
    WordVal := Swap(WordVal);
    UpdateData(2, WordVal);
  end;
end;

procedure TIPTCTag.SetDataSize(Value: Integer);
begin
  if Value = FDataSize then Exit;
  ReallocMem(FData, Value);
  FDataSize := Value;
  Changed;
end;

procedure TIPTCTag.SetID(const Value: TIPTCTagID);
begin
  if Value = FID then Exit;
  FID := Value;
  Changed(ctID);
end;

procedure TIPTCTag.SetIndex(NewIndex: Integer);
begin
  if FSection <> nil then FSection.Move(Index, NewIndex);
end;

procedure TIPTCTag.UpdateData(const Buffer);
begin
  UpdateData(DataSize, Buffer);
end;

procedure TIPTCTag.UpdateData(NewDataSize: Integer; const Buffer);
begin
  ReallocMem(FData, NewDataSize);
  FDataSize := NewDataSize;
  Move(Buffer, FData^, NewDataSize);
  Changed;
end;

procedure TIPTCTag.UpdateData(NewDataSize: Integer; Source: TStream);
begin
  ReallocMem(FData, NewDataSize);
  FDataSize := NewDataSize;
  Source.Read(FData^, NewDataSize);
  Changed;
end;

procedure TIPTCTag.WriteString(const NewValue: RawByteString);
begin
  ReallocMem(FData, 0);
  FDataSize := Length(NewValue);
  if FDataSize <> 0 then
  begin
    ReallocMem(FData, FDataSize);
    Move(Pointer(NewValue)^, FData^, FDataSize);
  end;
  Changed;
end;

procedure TIPTCTag.WriteString(const NewValue: UnicodeString);
begin
  if (Section <> nil) and (Section.Owner <> nil) and Section.Owner.UTF8Encoded then
    WriteString(UTF8Encode(NewValue))
  else
    WriteString(AnsiString(NewValue));
end;

{ TIPTCSection.TEnumerator }

constructor TIPTCSection.TEnumerator.Create(ASection: TIPTCSection);
begin
  FIndex := -1;
  FSource := ASection;
end;

function TIPTCSection.TEnumerator.GetCurrent: TIPTCTag;
begin
  Result := FSource[FIndex];
end;

function TIPTCSection.TEnumerator.MoveNext: Boolean;
begin
  Result := (Succ(FIndex) < FSource.Count);
  if Result then Inc(FIndex);
end;

{ TIPTCSection.TTagList }

{$IFDEF HasGenerics}
constructor TIPTCSection.TTagList.Create;
begin
  inherited Create(TComparer<TIPTCTag>.Construct(
    function(const Left, Right: TIPTCTag): Integer
    begin
      Result := Right.ID - Left.ID;
    end));
end;
{$ELSE}
procedure TIPTCSection.TTagList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    TObject(Ptr).Free;
end;

function CompareIDs(Item1, Item2: TIPTCTag): Integer;
begin
  Result := Item2.ID - Item1.ID;
end;

procedure TIPTCSection.TTagList.Sort;
begin
  inherited Sort(@CompareIDs);
end;
{$ENDIF}

{ TIPTCSection }

constructor TIPTCSection.Create(AOwner: TIPTCData; AID: TIPTCSectionID);
begin
  inherited Create;
  FOwner := AOwner;
  FID := AID;
  FTags := TTagList.Create;
end;

destructor TIPTCSection.Destroy;
begin
  FreeAndNil(FTags);
  inherited;
end;

function TIPTCSection.Add(ID: TIPTCTagID): TIPTCTag;
var
  I: Integer;
begin
  if ID = 0 then
    Result := Insert(Count)
  else
  begin
    for I := Count - 1 downto 0 do
      if ID > GetTag(I).ID then
      begin
        Result := Insert(I + 1, ID);
        Exit;
      end;
    Result := Insert(0, ID);
  end;
end;

function TIPTCSection.AddOrUpdate(TagID: TIPTCTagID; NewDataSize: Integer;
  const Buffer): TIPTCTag;
begin
  if not Find(TagID, Result) then
    Result := Add(TagID);
  Result.UpdateData(NewDataSize, Buffer);
end;

function TIPTCSection.Append(ID: TIPTCTagID): TIPTCTag;
begin
  Result := Insert(Count, ID);
end;

function TIPTCSection.Find(ID: TIPTCTagID; out Tag: TIPTCTag): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to FTags.Count - 1 do
  begin
    Tag := FTags[I];
    if Tag.ID = ID then Exit;
    if FDefinitelySorted and (Tag.ID > ID) then Break;
  end;
  Tag := nil;
  Result := False;
end;

function TIPTCSection.GetDateValue(TagID: TIPTCTagID): TDateTimeTagValue;
var
  Date: TDateTime;
  S: string;
  Year, Month, Day: Integer;
begin
  S := GetStringValue(TagID);
  if TryStrToInt(Copy(S, 1, 4), Year) and TryStrToInt(Copy(S, 5, 2), Month) and
     TryStrToInt(Copy(S, 7, 2), Day) and TryEncodeDate(Year, Month, Day, Date) then
    Result := Date
  else
    Result := TDateTimeTagValue.CreateMissingOrInvalid;
end;

procedure TIPTCSection.SetDateValue(TagID: TIPTCTagID; const Value: TDateTimeTagValue);
var
  Year, Month, Day: Word;
begin
  if Value.MissingOrInvalid then
  begin
    Remove(TagID);
    Exit;
  end;
  DecodeDate(Value, Year, Month, Day);
  SetStringValue(TagID, Format('%.4d%.2d%.2d', [Year, Month, Day]));
end;

function TIPTCSection.GetDateTimeValue(DateTagID, TimeTagID: TIPTCTagID; AsUtc: Boolean): TDateTimeTagValue;
var
  OffsetHrs, OffsetMins: Integer;
  I: Integer;
  S: string;
  TimePart: TDateTimeTagValue;
  Temp: TDateTime;
begin
  TimePart := TDateTimeTagValue.CreateMissingOrInvalid;
  S := GetStringValue(TimeTagID);
  if (Length(S) = 11) and IsCharIn(S[7], ['-', '+']) and TryStrToInt(Copy(S, 8, 2),
    OffsetHrs) and TryStrToInt(Copy(S, 10, 2), OffsetMins) then
  begin
    for I in [1, 2, 3, 4, 5, 6, 8, 9, 10, 11] do
      if not IsCharIn(S[I], ['0'..'9']) then
      begin
        S := '';
        Break;
      end;
    if (S <> '') then
    begin
      if not AsUtc then
      begin
        OffsetHrs := 0;
        OffsetMins := 0;
      end
      else if S[7] = '+' then //i.e., if the local time is ahead of GMT
      begin
        OffsetHrs := -OffsetHrs;
        OffsetMins := -OffsetMins;
      end;
      if TryEncodeTime(StrToInt(Copy(S, 1, 2)) + OffsetHrs, StrToInt(Copy(S, 3, 2)) +
        OffsetMins, StrToInt(Copy(S, 5, 2)), 0, Temp) then
        TimePart := Temp;
    end;
  end;
  Result := GetDateValue(DateTagID);
  if TimePart.MissingOrInvalid then Exit;
  if Result.MissingOrInvalid then
    Result := TimePart
  else
    Result := Result.Value + TimePart.Value;
end;

procedure TIPTCSection.SetDateTimeValue(DateTagID, TimeTagID: TIPTCTagID;
  const Value: TDateTimeTagValue; AheadOfUtc: Boolean; UtcOffsetHrs, UtcOffsetMins: Byte);
const
  PlusOrMinus: array[Boolean] of string = ('-', '+');
var
  Year, Month, Day, Hour, Minute, Second, MilliSecond: Word;
begin
  if Value.MissingOrInvalid then
  begin
    Remove([DateTagID, TimeTagID]);
    Exit;
  end;
  DecodeDateTime(Value, Year, Month, Day, Hour, Minute, Second, MilliSecond);
  SetStringValue(DateTagID, Format('%.4d%.2d%.2d', [Year, Month, Day]));
  SetStringValue(TimeTagID, Format('%.2d%.2d%.2d%s%.2d%.2d', [Hour, Minute, Second,
    PlusOrMinus[AheadOfUtc], UtcOffsetHrs, UtcOffsetMins]));
end;

{$IFDEF HasTTimeZone}
procedure TIPTCSection.SetDateTimeValue(DateTagID, TimeTagID: TIPTCTagID; const Value: TDateTimeTagValue);
var
  Offset: TTimeSpan;
begin
  Offset := TTimeZone.Local.UtcOffset;
  SetDateTimeValue(DateTagID, TimeTagID, Value, Offset.Ticks >= 0, Abs(Offset.Hours),
    Abs(Offset.Minutes));
end;
{$ENDIF}

function TIPTCSection.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TIPTCSection.GetStringValue(TagID: TIPTCTagID): string;
var
  Tag: TIPTCTag;
begin
  if Find(TagID, Tag) then
    Result := Tag.ReadString
  else
    Result := '';
end;

function TIPTCSection.Remove(TagID: TIPTCTagID): Integer;
begin
  Result := Remove([TagID]);
end;

function TIPTCSection.Remove(TagIDs: TIPTCTagIDs): Integer;
var
  I: Integer;
  Tag: TIPTCTag;
begin
  Result := -1;
  for I := Count - 1 downto 0 do
  begin
    Tag := Tags[I];
    if Tag.ID in TagIDs then
    begin
      Delete(I);
      Result := I;
    end;
  end;
end;

procedure TIPTCSection.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Delete(I);
end;

procedure TIPTCSection.Delete(Index: Integer);
var
  Tag: TIPTCTag;
begin
  Tag := FTags[Index];
  Tag.FSection := nil;
  FTags.Delete(Index);
  FModified := True;
  if FTags.Count <= 1 then FDefinitelySorted := True;
end;

function TIPTCSection.GetPriorityValue(TagID: TIPTCTagID): TIPTCPriority;
var
  Tag: TIPTCTag;
begin
  if Find(TagID, Tag) and (Tag.DataSize = 1) then
    for Result := Low(TIPTCPriority) to High(TIPTCPriority) do
      if PriorityChars[Result] = PAnsiChar(Tag.Data)^ then Exit;
  Result := ipTagMissing;
end;

function TIPTCSection.GetRepeatableValue(TagID: TIPTCTagID): TIPTCStringArray;
var
  Count: Integer;
  Tag: TIPTCTag;
begin
  Count := 0;
  Result := nil;
  for Tag in Self do
    if Tag.ID = TagID then
    begin
      if Count = Length(Result) then SetLength(Result, Count + 8);
      Result[Count] := Tag.AsString;
      Inc(Count);
    end;
  if Count <> 0 then SetLength(Result, Count);
end;

procedure TIPTCSection.GetRepeatableValue(TagID: TIPTCTagID; Dest: TStrings;
  ClearDestFirst: Boolean);
var
  Tag: TIPTCTag;
begin
  Dest.BeginUpdate;
  try
    if ClearDestFirst then Dest.Clear;
    for Tag in Self do
      if Tag.ID = TagID then Dest.Add(Tag.AsString);
  finally
    Dest.EndUpdate;
  end;
end;

procedure TIPTCSection.GetRepeatablePairs(KeyID, ValueID: TIPTCTagID; Dest: TStrings;
  ClearDestFirst: Boolean = True);
var
  Keys, Values: TIPTCStringArray;
  I: Integer;
begin
  Dest.BeginUpdate;
  try
    if ClearDestFirst then Dest.Clear;
    Keys := GetRepeatableValue(KeyID);
    Values := GetRepeatableValue(ValueID);
    if Length(Values) > Length(Keys) then
      SetLength(Keys, Length(Values))
    else
      SetLength(Values, Length(Keys));
    for I := 0 to High(Keys) do
      Dest.Add(Keys[I] + Dest.NameValueSeparator + Values[I]);
  finally
    Dest.EndUpdate;
  end;
end;

function TIPTCSection.GetRepeatablePairs(KeyID, ValueID: TIPTCTagID): TIPTCRepeatablePairs;
var
  Keys, Values: TIPTCStringArray;
  I: Integer;
begin
  Keys := GetRepeatableValue(KeyID);
  Values := GetRepeatableValue(ValueID);
  if Length(Values) > Length(Keys) then
    SetLength(Keys, Length(Values))
  else
    SetLength(Values, Length(Keys));
  SetLength(Result, Length(Keys));
  for I := 0 to High(Result) do
  begin
    Result[I].Key := Keys[I];
    Result[I].Value := Values[I];
  end;
end;

procedure TIPTCSection.SetPriorityValue(TagID: TIPTCTagID; Value: TIPTCPriority);
begin
  if Value = ipTagMissing then
    Remove(TagID)
  else
    AddOrUpdate(TagID, 1, PriorityChars[Value]);
end;

procedure TIPTCSection.SetRepeatableValue(TagID: TIPTCTagID; const Value: array of string);
var
  I, DestIndex: Integer;
begin
  DestIndex := Remove(TagID);
  if DestIndex < 0 then
  begin
    DestIndex := 0;
    for I := Count - 1 downto 0 do
      if TagID > GetTag(I).ID then
      begin
        DestIndex := I + 1;
        Break;
      end;
  end;
  for I := 0 to High(Value) do
  begin
    Insert(DestIndex, TagID).AsString := Value[I];
    Inc(DestIndex);
  end;
end;

procedure TIPTCSection.SetRepeatableValue(TagID: TIPTCTagID; Value: TStrings);
var
  DynArray: TIPTCStringArray;
  I: Integer;
begin
  SetLength(DynArray, Value.Count);
  for I := High(DynArray) downto 0 do
    DynArray[I] := Value[I];
  SetRepeatableValue(TagID, DynArray);
end;

procedure TIPTCSection.SetRepeatablePairs(KeyID, ValueID: TIPTCTagID; Pairs: TStrings);
var
  I, DestIndex: Integer;
  Key, Value: string;
begin
  DestIndex := Remove([KeyID, ValueID]);
  if DestIndex < 0 then
  begin
    DestIndex := 0;
    for I := Count - 1 downto 0 do
      if KeyID > GetTag(I).ID then
      begin
        DestIndex := I + 1;
        Break;
      end;
  end;
  for I := 0 to Pairs.Count - 1 do
  begin
    Key := Pairs.Names[I];
    Value := Pairs.ValueFromIndex[I];
    if (Key <> '') or (Value <> '') then
    begin
      Insert(DestIndex, KeyID).AsString := Key;
      Inc(DestIndex);
      Insert(DestIndex, ValueID).AsString := Value;
      Inc(DestIndex);
    end;
  end;
end;

procedure TIPTCSection.SetRepeatablePairs(KeyID, ValueID: TIPTCTagID; const Pairs: TIPTCRepeatablePairs);
var
  I, DestIndex: Integer;
  Pair: TIPTCRepeatablePair;
begin
  DestIndex := Remove([KeyID, ValueID]);
  if DestIndex < 0 then
  begin
    DestIndex := 0;
    for I := Count - 1 downto 0 do
      if KeyID > GetTag(I).ID then
      begin
        DestIndex := I + 1;
        Break;
      end;
  end;
  for Pair in Pairs do
    if (Pair.Key <> '') or (Pair.Value <> '') then
    begin
      Insert(DestIndex, KeyID).AsString := Pair.Key;
      Inc(DestIndex);
      Insert(DestIndex, ValueID).AsString := Pair.Value;
      Inc(DestIndex);
    end;
end;

function TIPTCSection.GetTag(Index: Integer): TIPTCTag;
begin
  Result := FTags[Index];
end;

function TIPTCSection.GetTagCount: Integer;
begin
  Result := FTags.Count;
end;

function TIPTCSection.GetWordValue(TagID: TIPTCTagID): TWordTagValue;
var
  Tag: TIPTCTag;
begin
  if Find(TagID, Tag) and (Tag.DataSize = 2) then
    Result := Swap(PWord(Tag.Data)^)
  else
    Result := TWordTagValue.CreateMissingOrInvalid;
end;

function TIPTCSection.Insert(Index: Integer; ID: TIPTCTagID = 0): TIPTCTag;
begin
  if FDefinitelySorted and (Index < Count) and (ID > GetTag(Index).ID) then
    FDefinitelySorted := False;
  Result := TIPTCTag.Create;
  try
    Result.FID := ID;
    Result.FSection := Self;
    FTags.Insert(Index, Result);
  except
     FreeAndNil(Result);
    raise;
  end;
  FModified := True;
end;

procedure TIPTCSection.Move(CurIndex, NewIndex: Integer);
begin
  if CurIndex = NewIndex then Exit;
  FTags.Move(CurIndex, NewIndex);
  FModified := True;
  FDefinitelySorted := False;
end;

procedure TIPTCSection.SetStringValue(TagID: TIPTCTagID; const Value: string);
var
  Tag: TIPTCTag;
begin
  if Value = '' then
  begin
    Remove(TagID);
    Exit;
  end;
  if not Find(TagID, Tag) then Tag := Add(TagID);
  Tag.WriteString(Value);
end;

procedure TIPTCSection.SetWordValue(TagID: TIPTCTagID; const Value: TWordTagValue);
var
  Data: Word;
begin
  if Value.MissingOrInvalid then
    Remove(TagID)
  else
  begin
    Data := Swap(Value.Value);
    AddOrUpdate(TagID, 2, Data);
  end;
end;

procedure TIPTCSection.Sort;
begin
  if FDefinitelySorted then Exit;
  FTags.Sort;
  FModified := True;
  FDefinitelySorted := True;
end;

function TIPTCSection.TagExists(ID: TIPTCTagID; MinSize: Integer = 1): Boolean;
var
  Tag: TIPTCTag;
begin
  Result := Find(ID, Tag) and (Tag.DataSize >= MinSize);
end;

{ TIPTCData.TEnumerator }

constructor TIPTCData.TEnumerator.Create(ASource: TIPTCData);
begin
  FCurrentID := Low(FCurrentID);
  FDoneFirst := False;
  FSource := ASource;
end;

function TIPTCData.TEnumerator.GetCurrent: TIPTCSection;
begin
  Result := FSource.Sections[FCurrentID];
end;

function TIPTCData.TEnumerator.MoveNext: Boolean;
begin
  Result := (FCurrentID < High(FCurrentID));
  if Result and FDoneFirst then
    Inc(FCurrentID)
  else
    FDoneFirst := True;
end;

{ TIPTCData }

constructor TIPTCData.Create(AOwner: TComponent);
var
  ID: TIPTCSectionID;
begin
  inherited Create(AOwner);
  for ID := Low(ID) to High(ID) do
    FSections[ID] := TIPTCSection.Create(Self, ID);
  FTiffRewriteCallback := TSimpleTiffRewriteCallbackImpl.Create(Self, ttIPTC);
  FUTF8Encoded := True;
end;

class function TIPTCData.CreateAsSubComponent(AOwner: TComponent): TIPTCData;
begin
  Result := Create(AOwner);
  Result.Name := 'IPTCData';
  Result.SetSubComponent(True);
end;

destructor TIPTCData.Destroy;
var
  ID: TIPTCSectionID;
begin
  for ID := Low(ID) to High(ID) do
     FreeAndNil(FSections[ID]);
  FreeAndNil(FTiffRewriteCallback);
  inherited;
end;

procedure TIPTCData.AddFromStream(Stream: TStream);
var
  Info: TIPTCTagInfo;
  NewTag: TIPTCTag;
begin
  NeedLazyLoadedData;
  while TAdobeResBlock.TryReadIPTCHeader(Stream, Info) do
  begin
    NewTag := FSections[Info.SectionID].Append(Info.TagID);
    try
      NewTag.UpdateData(Info.DataSize, Stream);
    except
       FreeAndNil(NewTag);
      raise;
    end;
    if (Info.SectionID = 1) and (Info.TagID = 90) and (Info.DataSize = SizeOf(UTF8Marker)) and
      CompareMem(NewTag.Data, @UTF8Marker, Info.DataSize) then
      FUTF8Encoded := True;
  end;
end;

procedure TIPTCData.Assign(Source: TPersistent);
var
  SectID: TIPTCSectionID;
  Tag: TIPTCTag;
begin
  if not (Source is TIPTCData) then
  begin
    if Source = nil then
      Clear
    else
      inherited;
    Exit;
  end;
  if TIPTCData(Source).DataToLazyLoad <> nil then
    DataToLazyLoad := TIPTCData(Source).DataToLazyLoad
  else
    for SectID := Low(SectID) to High(SectID) do
    begin
      FSections[SectID].Clear;
      for Tag in TIPTCData(Source).Sections[SectID] do
        FSections[SectID].Add(Tag.ID).UpdateData(Tag.DataSize, Tag.Data^);
    end;
end;

procedure TIPTCData.Clear;
var
  ID: TIPTCSectionID;
begin
  FLoadErrors := [];
  FDataToLazyLoad := nil;
  for ID := Low(ID) to High(ID) do
    FSections[ID].Clear;
end;

procedure TIPTCData.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('Data', LoadFromStream, SaveToStream, not Empty);
end;

function TIPTCData.CreateAdobeBlock: IAdobeResBlock;
begin
  Result := CCR.Exif.BaseUtils.CreateAdobeBlock(TAdobeResBlock.IPTCTypeID, Self);
end;

procedure TIPTCData.DoSaveToJPEG(InStream, OutStream: TStream);
var
  Block: IAdobeResBlock;
  RewrittenSegment: IJPEGSegment;
  SavedIPTC: Boolean;
  Segment: IFoundJPEGSegment;
  StartPos: Int64;
begin
  SavedIPTC := Empty;
  StartPos := InStream.Position;
  for Segment in JPEGHeader(InStream, [jmApp13]) do
    if Segment.IsAdobeApp13 then
    begin //IrfanView just wipes the original segment and replaces it with the new IPTC data, even if it contained non-IPTC blocks too. Eek!
      if StartPos <> Segment.Offset then
      begin
        InStream.Position := StartPos;
        OutStream.CopyFrom(InStream, Segment.Offset - StartPos);
      end;
      if SavedIPTC then
        RewrittenSegment := CreateAdobeApp13Segment
      else
      begin
        RewrittenSegment := CreateAdobeApp13Segment([CreateAdobeBlock]);
        SavedIPTC := True;
      end;
      for Block in Segment do
        if not Block.IsIPTCBlock then Block.SaveToStream(RewrittenSegment.Data);
      WriteJPEGSegment(OutStream, RewrittenSegment);
      StartPos := Segment.Offset + Segment.TotalSize;
    end;
  if not SavedIPTC then
  begin
    InStream.Position := StartPos;
    for Segment in JPEGHeader(InStream, AnyJPEGMarker - [jmJFIF, jmApp1]) do
    begin //insert immediately after any Exif or XMP segments if they exist, at the top of the file if they do not
      InStream.Position := StartPos;
      OutStream.CopyFrom(InStream, Segment.Offset - StartPos);
      WriteJPEGSegment(OutStream, CreateAdobeApp13Segment([CreateAdobeBlock]));
      StartPos := Segment.Offset;
      Break;
    end;
  end;
  InStream.Position := StartPos;
  OutStream.CopyFrom(InStream, InStream.Size - StartPos);
end;

procedure TIPTCData.DoSaveToPSD(InStream, OutStream: TStream);
var
  Block: IAdobeResBlock;
  Info: TPSDInfo;
  NewBlocks: IInterfaceList;
  StartPos: Int64;
begin
  StartPos := InStream.Position;
  NewBlocks := TInterfaceList.Create;
  if not Empty then NewBlocks.Add(CreateAdobeBlock);
  for Block in ParsePSDHeader(InStream, Info) do
    if not Block.IsIPTCBlock then NewBlocks.Add(Block);
  WritePSDHeader(OutStream, Info.Header);
  WritePSDResourceSection(OutStream, NewBlocks);
  InStream.Position := StartPos + Info.LayersSectionOffset;
  OutStream.CopyFrom(InStream, InStream.Size - InStream.Position);
end;

procedure TIPTCData.DoSaveToTIFF(InStream, OutStream: TStream);
begin
  RewriteTiff(InStream, OutStream, Self);
end;

function TIPTCData.GetActionAdvised: TIPTCActionAdvised;
var
  IntValue: Integer;
begin
  if TryStrToInt(ApplicationSection.GetStringValue(itActionAdvised), IntValue) and
    (IntValue > 0) and (IntValue <= 99) then
    Result := TIPTCActionAdvised(IntValue)
  else
    Result := iaTagMissing;
end;

procedure TIPTCData.SetActionAdvised(Value: TIPTCActionAdvised);
begin
  if Value = iaTagMissing then
    ApplicationSection.Remove(itActionAdvised)
  else
    ApplicationSection.SetStringValue(itActionAdvised, Format('%.2d', [Ord(Value)]));
end;

procedure TIPTCData.GetBylineDetails(Strings: TStrings; ClearDestFirst: Boolean = True);
begin
  ApplicationSection.GetRepeatablePairs(itByline, itBylineTitle, Strings, ClearDestFirst);
end;

procedure TIPTCData.GetBylineValues(Strings: TStrings);
begin
  GetBylineDetails(Strings);
end;

procedure TIPTCData.GetPSCreatorValues(Strings: TStrings);
begin
  GetBylineDetails(Strings);
end;

procedure TIPTCData.GetContentLocationValues(Strings: TStrings; ClearDestFirst: Boolean = True);
begin
  ApplicationSection.GetRepeatablePairs(itContentLocationCode, itContentLocationName, Strings, ClearDestFirst);
end;

function TIPTCData.GetDate(PackedIndex: Integer): TDateTimeTagValue;
begin
  Result := Sections[Lo(PackedIndex)].GetDateValue(Hi(PackedIndex));
end;

function TIPTCData.GetDateTimeCreated: TDateTimeTagValue;
begin
  Result := Sections[isApplication].GetDateTimeValue(itDateCreated, itTimeCreated)
end;

function TIPTCData.GetDateTimeSent: TDateTimeTagValue;
begin
  Result := Sections[isEnvelope].GetDateTimeValue(itDateSent, itTimeSent)
end;

procedure TIPTCData.SetDateTimeCreated(const Value: TDateTimeTagValue; AheadOfUtc: Boolean;
  UtcOffsetHrs, UtcOffsetMins: Byte);
begin
  Sections[isApplication].SetDateTimeValue(itDateCreated, itTimeCreated, Value,
    AheadOfUtc, UtcOffsetHrs, UtcOffsetMins);
end;

procedure TIPTCData.SetDateTimeSent(const Value: TDateTimeTagValue; AheadOfUtc: Boolean;
  UtcOffsetHrs, UtcOffsetMins: Byte);
begin
  Sections[isEnvelope].SetDateTimeValue(itDateSent, itTimeSent, Value,
    AheadOfUtc, UtcOffsetHrs, UtcOffsetMins);
end;

{$IFDEF HasTTimeZone}
procedure TIPTCData.SetDateTimeCreatedProp(const Value: TDateTimeTagValue);
begin
  Sections[isApplication].SetDateTimeValue(itDateCreated, itTimeCreated, Value);
end;

procedure TIPTCData.SetDateTimeSentProp(const Value: TDateTimeTagValue);
begin
  Sections[isEnvelope].SetDateTimeValue(itDateSent, itTimeSent, Value);
end;
{$ENDIF}

function TIPTCData.GetImageOrientation: TIPTCImageOrientation;
var
  Tag: TIPTCTag;
begin
  if not ApplicationSection.Find(itImageOrientation, Tag) or (Tag.DataSize <> 1) then
    Result := ioTagMissing
  else
    Result := TIPTCImageOrientation(UpCase(PAnsiChar(Tag.Data)^));
end;

procedure TIPTCData.SetImageOrientation(Value: TIPTCImageOrientation);
begin
  if Value = ioTagMissing then
    ApplicationSection.Remove(itImageOrientation)
  else
    ApplicationSection.AddOrUpdate(itImageOrientation, 1, Value);
end;

function TIPTCData.GetApplicationPairs(PackedTagIDs: Integer): TIPTCRepeatablePairs;
begin
  Result := ApplicationSection.GetRepeatablePairs(Lo(PackedTagIDs), Hi(PackedTagIDs));
end;

function TIPTCData.GetApplicationString(TagID: Integer): string;
begin
  Result := ApplicationSection.GetStringValue(TagID)
end;

function TIPTCData.GetApplicationStrings(TagID: Integer): TIPTCStringArray;
begin
  Result := ApplicationSection.GetRepeatableValue(TagID);
end;

function TIPTCData.GetEnvelopePriority: TIPTCPriority;
begin
  Result := EnvelopeSection.GetPriorityValue(itEnvelopePriority);
end;

function TIPTCData.GetEnvelopeString(TagID: Integer): string;
begin
  Result := EnvelopeSection.GetStringValue(TagID)
end;

function TIPTCData.GetApplicationWord(TagID: Integer): TWordTagValue;
begin
  Result := ApplicationSection.GetWordValue(TagID);
end;

function TIPTCData.GetEnvelopeWord(TagID: Integer): TWordTagValue;
begin
  Result := EnvelopeSection.GetWordValue(TagID);
end;

function TIPTCData.GetUrgency: TIPTCPriority;
begin
  Result := ApplicationSection.GetPriorityValue(itUrgency);
end;

procedure TIPTCData.SetUrgency(Value: TIPTCPriority);
begin
  ApplicationSection.SetPriorityValue(itUrgency, Value);
end;

function TIPTCData.GetEmpty: Boolean;
var
  Section: TIPTCSection;
begin
  Result := False;
  if DataToLazyLoad <> nil then Exit;
  for Section in FSections do
    if Section.Count <> 0 then Exit;
  Result := True;
end;

function TIPTCData.GetEnumerator: TEnumerator;
begin
  NeedLazyLoadedData;
  Result := TEnumerator.Create(Self);
end;

procedure TIPTCData.GetKeyWords(Dest: TStrings);
begin
  ApplicationSection.GetRepeatableValue(itKeyword, Dest);
end;

function TIPTCData.GetModified: Boolean;
var
  ID: TIPTCSectionID;
begin
  Result := True;
  if DataToLazyLoad = nil then
    for ID := Low(ID) to High(ID) do
      if FSections[ID].Modified then Exit;
  Result := False;
end;

function TIPTCData.GetSection(ID: Integer): TIPTCSection;
begin
  NeedLazyLoadedData;
  Result := FSections[ID];
end;

function TIPTCData.GetSectionByID(ID: TIPTCSectionID): TIPTCSection;
begin
  NeedLazyLoadedData;
  Result := FSections[ID];
end;

function TIPTCData.GetUTF8Encoded: Boolean;
begin
  NeedLazyLoadedData;
  Result := FUTF8Encoded;
end;

function TIPTCData.LoadFromGraphic(Stream: TStream): Boolean;
var
  AdobeBlock: IAdobeResBlock;
  Directory: IFoundTiffDirectory;
  Info: TPSDInfo;
  Segment: IFoundJPEGSegment;
  Tag: ITiffTag;
begin
  Result := True;
  if HasJPEGHeader(Stream) then
  begin
    for Segment in JPEGHeader(Stream, [jmApp13]) do
      for AdobeBlock in Segment do
        if AdobeBlock.IsIPTCBlock then
        begin
          LoadFromStream(AdobeBlock.Data);
          Exit;
        end
  end
  else if HasPSDHeader(Stream) then
  begin
    for AdobeBlock in ParsePSDHeader(Stream, Info) do
      if AdobeBlock.IsIPTCBlock then
      begin
        LoadFromStream(AdobeBlock.Data);
        Exit;
      end;
  end
  else if HasTiffHeader(Stream) then
  begin
    for Directory in ParseTiff(Stream) do
    begin
      if Directory.FindTag(ttIPTC, Tag) then
      begin
        LoadFromStream(Tag.Data);
        Exit;
      end;
      Break; //only interested in the main IFD
    end;
  end
  else
    Result := False;
  Clear;
end;

function TIPTCData.LoadFromGraphic(const Graphic: IStreamPersist): Boolean;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Graphic.SaveToStream(Stream);
    Stream.Position := 0;
    Result := LoadFromGraphic(Stream);
  finally
     FreeAndNil(Stream);
  end;
end;

function TIPTCData.LoadFromGraphic(const FileName: string): Boolean;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromGraphic(Stream)
  finally
     FreeAndNil(Stream);
  end;
end;

procedure TIPTCData.LoadFromStream(Stream: TStream);
var
  I: Integer;
  Section: TIPTCSection;
begin
  Clear;
  FUTF8Encoded := AlwaysAssumeUTF8Encoding;
  AddFromStream(Stream);
  for Section in FSections do
  begin
    Section.Modified := False;
    Section.FDefinitelySorted := True;
    for I := 1 to Section.Count - 1 do
      if Section[I].ID < Section[I - 1].ID then
      begin
        Section.FDefinitelySorted := False;
        Break;
      end;
  end;
end;

procedure TIPTCData.NeedLazyLoadedData;
var
  Item: IMetadataBlock;
begin
  if FDataToLazyLoad = nil then Exit;
  Item := FDataToLazyLoad;
  FDataToLazyLoad := nil; //in case of an exception, nil this beforehand
  Item.Data.Seek(0, soFromBeginning);
  LoadFromStream(Item.Data);
end;

procedure TIPTCData.GetGraphicSaveMethod(Stream: TStream; var Method: TGraphicSaveMethod);
begin
  if HasJPEGHeader(Stream) then
    Method := DoSaveToJPEG
  else if HasPSDHeader(Stream) then
    Method := DoSaveToPSD
  else if HasTiffHeader(Stream) then
    Method := DoSaveToTIFF
end;

procedure TIPTCData.SaveToGraphic(const FileName: string);
begin
  DoSaveToGraphic(FileName, GetGraphicSaveMethod);
end;

procedure TIPTCData.SaveToGraphic(const Graphic: IStreamPersist);
begin
  DoSaveToGraphic(Graphic, GetGraphicSaveMethod);
end;

procedure TIPTCData.SaveToStream(Stream: TStream);
const
  BigDataSizeMarker: SmallInt = -4;
var
  Section: TIPTCSection;
  Tag: TIPTCTag;
begin
  if DataToLazyLoad <> nil then
  begin
    DataToLazyLoad.Data.SaveToStream(Stream);
    Exit;
  end;
  if UTF8Encoded then
    Sections[1].AddOrUpdate(90, SizeOf(UTF8Marker), UTF8Marker)
  else
    if Sections[1].Find(90, Tag) and (Tag.DataSize = SizeOf(UTF8Marker))
    and CompareMem(Tag.Data, @UTF8Marker, SizeOf(UTF8Marker))
    then FreeAndNil(Tag);
  for Section in FSections do
    for Tag in Section do
    begin
      Stream.WriteBuffer(TAdobeResBlock.NewIPTCTagMarker, SizeOf(TAdobeResBlock.NewIPTCTagMarker));
      Stream.WriteBuffer(Tag.Section.ID, SizeOf(TIPTCSectionID));
      Stream.WriteBuffer(Tag.ID, SizeOf(TIPTCTagID));
      if Tag.DataSize <= High(SmallInt) then
        Stream.WriteSmallInt(Tag.DataSize, BigEndian)
      else
      begin
        Stream.WriteSmallInt(BigDataSizeMarker, BigEndian);
        Stream.WriteLongInt(Tag.DataSize, BigEndian);
      end;
      if Tag.DataSize <> 0 then Stream.WriteBuffer(Tag.Data^, Tag.DataSize);
    end;
end;

procedure TIPTCData.SetDataToLazyLoad(const Value: IMetadataBlock);
begin
  if Value = FDataToLazyLoad then Exit;
  Clear;
  FDataToLazyLoad := Value;
end;

procedure TIPTCData.SortTags;
var
  SectID: TIPTCSectionID;
begin
  NeedLazyLoadedData;
  for SectID := Low(SectID) to High(SectID) do
    FSections[SectID].Sort;
end;

procedure TIPTCData.SetBylineDetails(Strings: TStrings);
begin
  ApplicationSection.SetRepeatablePairs(itByline, itBylineTitle, Strings);
end;

procedure TIPTCData.SetBylineValues(Strings: TStrings);
begin
  SetBylineDetails(Strings);
end;

procedure TIPTCData.SetPSCreatorValues(Strings: TStrings);
begin
  SetBylineDetails(Strings);
end;

procedure TIPTCData.SetContentLocationValues(Strings: TStrings);
begin
  ApplicationSection.SetRepeatablePairs(itContentLocationCode, itContentLocationName, Strings);
end;

procedure TIPTCData.SetDate(PackedIndex: Integer; const Value: TDateTimeTagValue);
begin
  FSections[Lo(PackedIndex)].SetDateValue(Hi(PackedIndex), Value)
end;

procedure TIPTCData.SetApplicationPairs(PackedTagIDs: Integer;
  const NewPairs: TIPTCRepeatablePairs);
begin
  ApplicationSection.SetRepeatablePairs(Lo(PackedTagIDs), Hi(PackedTagIDs), NewPairs);
end;

procedure TIPTCData.SetApplicationString(TagID: Integer; const Value: string);
begin
  ApplicationSection.SetStringValue(TagID, Value);
end;

procedure TIPTCData.SetApplicationStrings(TagID: Integer; const Value: TIPTCStringArray);
begin
  ApplicationSection.SetRepeatableValue(TagID, Value);
end;

procedure TIPTCData.SetApplicationWord(TagID: Integer; const Value: TWordTagValue);
begin
  ApplicationSection.SetWordValue(TagID, Value);
end;

procedure TIPTCData.SetEnvelopePriority(const Value: TIPTCPriority);
begin
  EnvelopeSection.SetPriorityValue(itEnvelopePriority, Value);
end;

procedure TIPTCData.SetEnvelopeString(TagID: Integer; const Value: string);
begin
  EnvelopeSection.SetStringValue(TagID, Value);
end;

procedure TIPTCData.SetEnvelopeWord(TagID: Integer; const Value: TWordTagValue);
begin
  EnvelopeSection.SetWordValue(TagID, Value);
end;

procedure TIPTCData.SetKeywords(NewWords: TStrings);
begin
  ApplicationSection.SetRepeatableValue(itKeyword, NewWords);
end;

procedure TIPTCData.SetModified(Value: Boolean);
var
  ID: TIPTCSectionID;
begin
  NeedLazyLoadedData;
  for ID := Low(ID) to High(ID) do
    FSections[ID].Modified := Value;
end;

procedure TIPTCData.SetUTF8Encoded(Value: Boolean);
var
  I: Integer;
  SectID: TIPTCSectionID;
  Tag: TIPTCTag;
  Strings: array[TIPTCSectionID] of TIPTCStringArray;
begin
  if Value = UTF8Encoded then Exit;
  for SectID := Low(SectID) to High(SectID) do
  begin
    SetLength(Strings[SectID], FSections[SectID].Count);
    for I := 0 to High(Strings[SectID]) do
    begin
      Tag := FSections[SectID].Tags[I];
      if FindProperDataType(Tag) = idString then
        Strings[SectID][I] := Tag.ReadString;
    end;
  end;
  FUTF8Encoded := Value;
  for SectID := Low(SectID) to High(SectID) do
    for I := 0 to High(Strings[SectID]) do
    begin
      Tag := FSections[SectID].Tags[I];
      if FindProperDataType(Tag) = idString then
        Tag.WriteString(Strings[SectID][I]);
    end;
end;

end.
