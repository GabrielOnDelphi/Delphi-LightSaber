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
{ The Original Code is CCR.Exif.XMPUtils.pas.                                          }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2014 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

{$I CCR.Exif.inc}
unit CCR.Exif.XMPUtils;
{
  This unit implements a IDOMDocument/IDOMNode-based XMP packet parser and editor. Since
  v1.1.0, edited data is written out manually (i.e., the IDOMXXX interfaces are only
  used to read). 

  Note that the UpdateXXX methods of TXMPPacket are called automatically when you set a
  tag property of TCustomExifData. Their behaviour depends upon the TXMPPacket's
  UpdatePolicy property, which has three possible values:
  - xwAlwaysUpdate: if the new value is an empty string, then the XMP property is
    deleted, else it is changed (or added). This setting is the default for standalone
    TXMPPacket instances.
  - xwUpdateIfExists: any existing property is updated, but if it doesn't already exist,
    no property is added. This setting is the default for an TXMPPacket instance that is
    attached to a TCustomExifData object; change this to xwAlwaysUpdate to mimic Windows
    Vista's behaviour.
  - xwRemove: always removes the property when UpdateProperty is called.
}
interface

uses
  Types, SysUtils, Classes, xmldom, CCR.Exif.BaseUtils, CCR.Exif.TiffUtils;

type
  EInvalidXMPPacket = class(Exception);
  EInvalidXMPOperation = class(EInvalidOperation);

  TXMPProperty = class;
  TXMPSchema = class;
  TXMPPacket = class;

  TXMPNamespace = (xsUnknown, xsRDF,
    xsCameraRaw, xsColorant, xsDimensions, xsDublinCore, xsFont, xsExif,
    xsExifAux, xsIPTC, xsJob, xsMicrosoftPhoto, xsPDF, xsPhotoshop, xsResourceEvent{*},
    xsResourceRef{*}, xsThumbnail, xsTIFF, xsVersion, xsXMPBasic, xsXMPBasicJobTicket,
    xsXMPDynamicMedia, xsXMPMediaManagement, xsXMPPagedText, xsXMPRights);
  TXMPKnownNamespace = xsRDF..High(TXMPNamespace);

  TKnownXMPNamespaces = record //personally I wouldn't have the 'T', but I'll keep with the D2009+ style...
  private class var
    class function GetPreferredPrefix(Namespace: TXMPKnownNamespace): UnicodeString; static;
    class function GetURI(Namespace: TXMPKnownNamespace): UnicodeString; static;
  public
    class function Find(const URI: UnicodeString; out Namespace: TXMPNamespace): Boolean; static;
    class property PreferredPrefix[Namespace: TXMPKnownNamespace]: UnicodeString read GetPreferredPrefix;
    class property URI[Namespace: TXMPKnownNamespace]: UnicodeString read GetURI;
  end;

  TXMPNamespaceInfo = class(TPersistent)
  strict private
    FCustomPrefix: UnicodeString;
    FCustomURI: UnicodeString;
    FKind: TXMPNamespace;
    FOnChange: TNotifyEvent;
    procedure Changed;
    function GetPrefix: UnicodeString;
    function GetURI: UnicodeString;
    procedure SetPrefix(const Value: UnicodeString);
  protected
    constructor Create(AKind: TXMPNamespace;
      const APreferredPrefix, AURI: UnicodeString); overload;
    procedure DoAssign(Source: TXMPNamespaceInfo); overload;
    function DoAssign(const APrefix, AURI: UnicodeString): Boolean; overload;
  public
    constructor Create(AKind: TXMPKnownNamespace); overload;
    constructor Create(const APreferredPrefix, AURI: UnicodeString); overload;
    constructor Create(ASource: TXMPNamespaceInfo); overload;
    procedure Assign(Source: TPersistent); overload; override;
    procedure Assign(AKind: TXMPKnownNamespace); reintroduce; overload;
    procedure Assign(const APrefix, AURI: UnicodeString); reintroduce; overload; 
    property Kind: TXMPNamespace read FKind;
    property Prefix: UnicodeString read GetPrefix write SetPrefix;
    property URI: UnicodeString read GetURI;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  IXMPPropertyEnumerator = interface
  ['{32054DDD-5415-4F5D-8A38-C79BBCFD1A50}']
    function GetCurrent: TXMPProperty;
    function MoveNext: Boolean;
    property Current: TXMPProperty read GetCurrent;
  end;

  IXMPPropertyCollection = interface
  ['{A72E8B43-34AE-49E8-A889-36B51B6A6E48}']
    function GetNamespaceInfo: TXMPNamespaceInfo;
    function GetProperty(Index: Integer): TXMPProperty;
    function GetPropertyCount: Integer;
    function GetEnumerator: IXMPPropertyEnumerator;
    property Count: Integer read GetPropertyCount;
    property Items[Index: Integer]: TXMPProperty read GetProperty; default;
    property NamespaceInfo: TXMPNamespaceInfo read GetNamespaceInfo;
  end;

  TXMPPropertyKind = (xpSimple, xpStructure, xpAltArray, xpBagArray, xpSeqArray);

  TXMPProperty = class(TInterfacedPersistent, IXMPPropertyCollection)
  strict private
    FKind: TXMPPropertyKind;
    FName: UnicodeString;
    FNamespaceInfo: TXMPNamespaceInfo;
    FParentNamespace: Boolean;
    FParentProperty: TXMPProperty;
    FSchema: TXMPSchema;
    FSubProperties: TObjectList;
    FValue: UnicodeString;
    procedure NamespaceInfoChanged(Sender: TObject);
    procedure SetKind(const Value: TXMPPropertyKind);
    procedure SetName(const Value: UnicodeString);
    procedure SetParentNamespace(Value: Boolean);
    procedure SetSubPropertyCount(NewCount: Integer);
  protected
    procedure Changed;
    function GetNamespaceInfo: TXMPNamespaceInfo;
    function GetSubProperty(Index: Integer): TXMPProperty;
    function GetSubPropertyByName(const AName: UnicodeString): TXMPProperty;
    function GetSubPropertyCount: Integer;
    function IXMPPropertyCollection.GetProperty = GetSubProperty;
    function IXMPPropertyCollection.GetPropertyCount = GetSubPropertyCount;
  public
    class function HasNamedSubProperties(Kind: TXMPPropertyKind): Boolean; overload; static; inline;
    class function SupportsSubProperties(Kind: TXMPPropertyKind): Boolean; overload; static; inline;
  public
    constructor Create(ASchema: TXMPSchema; AParentProperty: TXMPProperty;
      const ASourceNode: IDOMNode = nil);
    destructor Destroy; override;
    function GetEnumerator: IXMPPropertyEnumerator;
    function AddSubProperty(const AName: UnicodeString): TXMPProperty; //AName is ignored if self is an array property
    function FindSubProperty(const AName: UnicodeString; out Prop: TXMPProperty): Boolean;
    function ReadValue(const Default: Boolean): Boolean; overload;
    function ReadValue(const Default: Integer): Integer; overload;
    function ReadValue(const Default: TDateTime): TDateTime; overload;
    function ReadValue(const Default: UnicodeString = ''): UnicodeString; overload;
    function RemoveSubProperty(const AName: UnicodeString): Boolean;
    function HasNamedSubProperties: Boolean; overload;
    function SupportsSubProperties: Boolean; overload;
    procedure UpdateSubProperty(const SubPropName: UnicodeString;
      SubPropKind: TXMPPropertyKind; const NewValue: UnicodeString); overload;
    procedure UpdateSubProperty(const SubPropName, NewValue: UnicodeString); overload; inline;
    procedure UpdateSubProperty(const SubPropName: UnicodeString; NewValue: Integer); overload;
    procedure UpdateSubProperty(const SubPropName: UnicodeString; NewValue: Boolean); overload;
    procedure WriteValue(const NewValue: UnicodeString); overload;
    procedure WriteValue(const NewValue: TDateTime); overload;
    procedure WriteValue(const NewValue: Integer); overload;
    procedure WriteValue(const NewValue: Boolean); overload;
    property Kind: TXMPPropertyKind read FKind write SetKind;
    property Name: UnicodeString read FName write SetName;
    property NamespaceInfo: TXMPNamespaceInfo read FNamespaceInfo;
    property ParentNamespace: Boolean read FParentNameSpace write SetParentNamespace default True;
    property ParentProperty: TXMPProperty read FParentProperty;
    property Schema: TXMPSchema read FSchema;
    property SubProperties[const Name: UnicodeString]: TXMPProperty read GetSubPropertyByName; default; //adds if necessary
    property SubProperties[Index: Integer]: TXMPProperty read GetSubProperty; default;
    property SubPropertyCount: Integer read GetSubPropertyCount write SetSubPropertyCount;
  end;

  TXMPSchema = class(TInterfacedPersistent, IXMPPropertyCollection)
  strict private
    FLoadingProperty: Boolean;
    FNamespaceInfo: TXMPNamespaceInfo;
    FOwner: TXMPPacket;
    FProperties: TObjectList;
    procedure NamespaceInfoChanged(Sender: TObject);
  protected
    function LoadProperty(const ASourceNode: IDOMNode): TXMPProperty;
    procedure Changed;
    function FindOrAddProperty(const AName: UnicodeString): TXMPProperty;
    function GetNamespaceInfo: TXMPNamespaceInfo;
    function GetOwner: TPersistent; override;
    function GetProperty(Index: Integer): TXMPProperty;
    function GetPropertyCount: Integer;
  public
    constructor Create(AOwner: TXMPPacket; const AURI: UnicodeString);
    destructor Destroy; override;
    function GetEnumerator: IXMPPropertyEnumerator;
    function AddProperty(const AName: UnicodeString): TXMPProperty;
    function FindProperty(const AName: UnicodeString; var AProperty: TXMPProperty): Boolean;
    function RemoveProperty(const AName: UnicodeString): Boolean;
    function RemoveProperties(const ANames: array of UnicodeString): Boolean;
    property NamespaceInfo: TXMPNamespaceInfo read FNamespaceInfo;
    property Owner: TXMPPacket read FOwner;
    property Properties[const Name: UnicodeString]: TXMPProperty read FindOrAddProperty; default;
    property Properties[Index: Integer]: TXMPProperty read GetProperty; default;
    property PropertyCount: Integer read GetPropertyCount;
  end;

  TXMPWritePolicy = (xwAlwaysUpdate, xwUpdateIfExists, xwRemove);

  TXMPPacket = class(TComponent, IStreamPersist, IStreamPersistEx, ITiffRewriteCallback)
  public type
    TEnumerator = record
    private
      FIndex: Integer;
      FPacket: TXMPPacket;
      function GetCurrent: TXMPSchema;
    public
      constructor Create(Packet: TXMPPacket);
      function MoveNext: Boolean;
      property Current: TXMPSchema read GetCurrent;
    end;
    TLoadErrorEvent = procedure (Sender: TXMPPacket; Source: TStream) of object;
  strict private
    FAboutAttributeValue: UnicodeString;
    FDataToLazyLoad: IMetadataBlock;
    FRawXMLCache: UTF8String;
    FSchemas: TUnicodeStringList;
    FTiffRewriteCallback: TSimpleTiffRewriteCallbackImpl;
    FUpdatePolicy: TXMPWritePolicy;
    FOnChange: TNotifyEvent;
    FOnLoadError: TLoadErrorEvent;
    procedure Clear(StillUpdating: Boolean); overload;
    procedure GetGraphicSaveMethod(Stream: TStream; var Method: TGraphicSaveMethod);
    function GetRawXML: UTF8String;
    function GetSchema(Index: Integer): TXMPSchema;
    function GetSchemaCount: Integer;
    procedure SetAboutAttributeValue(const Value: UnicodeString);
    procedure SetDataToLazyLoad(const Value: IMetadataBlock);
    procedure SetRawXML(const XML: UTF8String);
    procedure DoSaveToJPEG(InStream, OutStream: TStream); inline;
    procedure DoSaveToPSD(InStream, OutStream: TStream);
    procedure DoSaveToTIFF(InStream, OutStream: TStream);
    procedure DoUpdateProperty(Policy: TXMPWritePolicy; SchemaKind: TXMPKnownNamespace;
      const PropName: UnicodeString; PropKind: TXMPPropertyKind; const NewValue: UnicodeString);
    procedure DoUpdateArrayProperty(SchemaKind: TXMPKnownNamespace;
      const PropName: UnicodeString; ArrayPropKind: TXMPPropertyKind;
      const NewValues: array of UnicodeString); overload;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Changed(AResetRawXMLCache: Boolean = True); virtual;
    function FindOrAddSchema(const URI: UnicodeString): TXMPSchema; overload;
    function FindOrAddSchema(Kind: TXMPKnownNamespace): TXMPSchema; overload; inline;
    function GetEmpty: Boolean; inline;
    procedure LoadError(Source: TStream); virtual;
    procedure NeedLazyLoadedData;
    property UpdatePolicy: TXMPWritePolicy read FUpdatePolicy write FUpdatePolicy;
    property TiffRewriteCallback: TSimpleTiffRewriteCallbackImpl read FTiffRewriteCallback implements ITiffRewriteCallback;
  public
    constructor Create(AOwner: TComponent = nil); override;
    class function CreateAsSubComponent(AOwner: TComponent): TXMPPacket; //use a class function rather than a constructor to avoid compiler warning re C++ accessibility
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; overload; inline;
    function FindSchema(const URI: UnicodeString; var Schema: TXMPSchema): Boolean; overload;
    function FindSchema(Kind: TXMPKnownNamespace; var Schema: TXMPSchema): Boolean; overload; inline;
    function GetEnumerator: TEnumerator;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    { Whether or not metadata was found, LoadFromGraphic returns True if the graphic
      format was one recognised as one we could extract XMP data from and False otherwise. }
    function LoadFromGraphic(Stream: TStream): Boolean; overload;
    function LoadFromGraphic(const Graphic: IStreamPersist): Boolean; overload;
    function LoadFromGraphic(const FileName: string): Boolean; overload;
    procedure SaveToFile(const FileName: string);
    procedure SaveToGraphic(const FileName: string); overload;
    procedure SaveToGraphic(const Graphic: IStreamPersist); overload;
    procedure SaveToStream(Stream: TStream);
    function TryLoadFromStream(Stream: TStream): Boolean;
    procedure ResetRawXMLCache;
    procedure RemoveProperty(SchemaKind: TXMPKnownNamespace;
      const PropName: UnicodeString); overload;
    procedure RemoveProperties(SchemaKind: TXMPKnownNamespace;
      const PropNames: array of UnicodeString); overload;
    procedure UpdateProperty(SchemaKind: TXMPKnownNamespace; const PropName: UnicodeString;
      PropKind: TXMPPropertyKind; const NewValue: UnicodeString); overload;
    procedure UpdateBagProperty(SchemaKind: TXMPKnownNamespace; const PropName: UnicodeString;
      const NewValues: array of UnicodeString); overload;
    procedure UpdateBagProperty(SchemaKind: TXMPKnownNamespace; const PropName: UnicodeString;
      const NewValueList: UnicodeString); overload; inline; //commas and/or semi-colons act as delimiters
    procedure UpdateSeqProperty(SchemaKind: TXMPKnownNamespace; const PropName: UnicodeString;
      const NewValues: array of UnicodeString); overload;
    procedure UpdateSeqProperty(SchemaKind: TXMPKnownNamespace; const PropName: UnicodeString;
      const NewValueList: UnicodeString); overload; inline; //commas and/or semi-colons act as delimiters
    procedure UpdateProperty(SchemaKind: TXMPKnownNamespace;
      const PropName, NewValue: UnicodeString); overload; inline;
    procedure UpdateProperty(SchemaKind: TXMPKnownNamespace;
      const PropName: UnicodeString; const NewValue: Integer); overload;
    procedure UpdateDateTimeProperty(SchemaKind: TXMPKnownNamespace;
      const PropName: UnicodeString; const NewValue: TDateTimeTagValue; ApplyLocalBias: Boolean = True); overload;
    property AboutAttributeValue: UnicodeString read FAboutAttributeValue write SetAboutAttributeValue;
    property DataToLazyLoad: IMetadataBlock read FDataToLazyLoad write SetDataToLazyLoad;
    property Schemas[Kind: TXMPKnownNamespace]: TXMPSchema read FindOrAddSchema; default;
    property Schemas[Index: Integer]: TXMPSchema read GetSchema; default;
    property Schemas[const URI: UnicodeString]: TXMPSchema read FindOrAddSchema; default;
    { TCustomIniFile-like value getters and setters for convenience }
    function ReadBool(SchemaKind: TXMPKnownNamespace; const PropertyName: string; DefValue: Boolean): Boolean; overload;
    function ReadBool(const SchemaURI: string; const PropertyName: string; DefValue: Boolean): Boolean; overload;
    function ReadDateTime(SchemaKind: TXMPKnownNamespace; const PropertyName: string; const DefValue: TDateTime): TDateTime; overload;
    function ReadDateTime(const SchemaURI: string; const PropertyName: string; const DefValue: TDateTime): TDateTime; overload;
    function ReadInteger(SchemaKind: TXMPKnownNamespace; const PropertyName: string; DefValue: Integer): Integer; overload;
    function ReadInteger(const SchemaURI: string; const PropertyName: string; DefValue: Integer): Integer; overload;
    function ReadString(SchemaKind: TXMPKnownNamespace; const PropertyName: string; const DefValue: string): string; overload;
    function ReadString(const SchemaURI: string; const PropertyName: string; const DefValue: string): string; overload;
    procedure WriteBool(SchemaKind: TXMPKnownNamespace; const PropertyName: string; Value: Boolean);  overload; inline;
    procedure WriteBool(const SchemaURI: string; const PropertyName: string; Value: Boolean);  overload; inline;
    procedure WriteDateTime(SchemaKind: TXMPKnownNamespace; const PropertyName: string; const Value: TDateTime); overload; inline;
    procedure WriteDateTime(const SchemaURI: string; const PropertyName: string; const Value: TDateTime); overload; inline;
    procedure WriteInteger(SchemaKind: TXMPKnownNamespace; const PropertyName: string; Value: Integer); overload; inline;
    procedure WriteInteger(const SchemaURI: string; const PropertyName: string; Value: Integer); overload; inline;
    procedure WriteString(SchemaKind: TXMPKnownNamespace; const PropertyName: string; const Value: string); overload; inline;
    procedure WriteString(const SchemaURI: string; const PropertyName: string; const Value: string); overload; inline;
  published
    property Empty: Boolean read GetEmpty;
    property RawXML: UTF8String read GetRawXML write SetRawXML;
    property SchemaCount: Integer read GetSchemaCount;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnLoadError: TLoadErrorEvent read FOnLoadError write FOnLoadError;
  end;

const
  XMPBoolStrs: array[Boolean] of string = ('False', 'True'); //case as per the XMP spec

function DateTimeToXMPString(const Value: TDateTime; ApplyLocalBias: Boolean = True): UnicodeString;
function XMPStringToDateTime(const Value: string; AsUTCTime: Boolean = False): TDateTime;
function EscapeXML(const Source: UnicodeString): UnicodeString;

implementation

uses
  {$IFDEF HasTTimeZone}TimeSpan{$ELSE}Windows{$ENDIF}, Math, RTLConsts, SysConst, DateUtils, StrUtils,
  CCR.Exif.Consts, CCR.Exif.TagIDs, CCR.Exif.StreamHelper;

const
  XMLLangAttrName = 'xml:lang';
  DefaultLangIdent = 'x-default';

type
  RDF = record const
    URI = UnicodeString('http://www.w3.org/1999/02/22-rdf-syntax-ns#');
    PreferredPrefix = 'rdf';
    AboutAttrLocalName = 'about';
    AboutAttrName = PreferredPrefix + ':' + AboutAttrLocalName;
    AltNodeName = PreferredPrefix + ':' + 'Alt';
    BagNodeName = PreferredPrefix + ':' + 'Bag';
    DescriptionNodeName = PreferredPrefix + ':' + 'Description';
    ListNodeLocalName = 'li';
    ListNodeName = PreferredPrefix + ':' + ListNodeLocalName;
    SeqNodeName = PreferredPrefix + ':' + 'Seq';
  end;

  TStringListThatOwnsItsObjects = class(TUnicodeStringList)
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
  end;

  TXMPPropertyEnumerator = class(TInterfacedObject, IXMPPropertyEnumerator)
  strict private
    FIndex: Integer;
    FSource: TObjectList;
  protected
    function GetCurrent: TXMPProperty;
    function MoveNext: Boolean;
  public
    constructor Create(Source: TObjectList);
  end;

const
  KnownURIs: array[TXMPKnownNamespace] of string = (
    RDF.URI,
    'http://ns.adobe.com/camera-raw-settings/1.0/',
    'http://ns.adobe.com/xap/1.0/g',
    'http://ns.adobe.com/xap/1.0/sType/Dimensions#',
    'http://purl.org/dc/elements/1.1/',
    'http://ns.adobe.com/xap/1.0/sType/Font#',
    'http://ns.adobe.com/exif/1.0/',
    'http://ns.adobe.com/exif/1.0/aux/',
    'http://iptc.org/std/Iptc4xmpCore/1.0/xmlns/',
    'http://ns.adobe.com/xap/1.0/sType/Job#',
    'http://ns.microsoft.com/photo/1.0',
    'http://ns.adobe.com/pdf/1.3/',
    'http://ns.adobe.com/photoshop/1.0/',
    'http://ns.adobe.com/xap/1.0/sType/ResourceEvent#',
    'http://ns.adobe.com/xap/1.0/sType/ResourceRef#',
    'http://ns.adobe.com/xap/1.0/g/img/',
    'http://ns.adobe.com/tiff/1.0/',
    'http://ns.adobe.com/xap/1.0/sType/Version#',
    'http://ns.adobe.com/xap/1.0/',
    'http://ns.adobe.com/xap/1.0/bj/',
    'http://ns.adobe.com/xmp/1.0/DynamicMedia/',
    'http://ns.adobe.com/xap/1.0/mm/',
    'http://ns.adobe.com/xap/1.0/t/pg/',
    'http://ns.adobe.com/xap/1.0/rights/');

  PreferredPrefixes: array[TXMPKnownNamespace] of string = (
    {xsRDF} RDF.PreferredPrefix,
    {xsCameraRaw} 'crs',
    {xsColorant} 'xmpG',
    {xsDimensions} 'stDim',
    {xsDublinCore} 'dc',
    {xsFont} 'stFnt',
    {xsExif} 'exif',
    {xsExifAux} 'aux',
    {xsIPTC} 'Iptc4xmpCore',
    {xsJob} 'stJob',
    {xsMicrosoftPhoto} 'MicrosoftPhoto',
    {xsPDF} 'pdf',
    {xsPhotoshop} 'photoshop',
    {xsResourceEvent} 'stEvt',
    {xsResourceRef} 'stRef',
    {xsThumbnail} 'xmpGImg',
    {xsTIFF} 'tiff',
    {xsVersion} 'stVer',
    {xsXMPBasic} 'xmp',
    {xsXMPBasicJobTicket} 'xmpBJ',
    {xsXMPDynamicMedia} 'xmpDM',
    {xsXMPMediaManagement} 'xmpMM',
    {xsXMPPagedText} 'xmpTPg',
    {xsXMPRights} 'xmpRights');

function GetUTCOffset(const Value: TDateTime; out AHours, AMins: Integer): Int64;
{$IFDEF HasTTimeZone}
begin
  with TTimeZone.Local.GetUTCOffset(Value) do
  begin
    Result := Ticks;
    AHours := Hours;
    AMins := Minutes;
  end;
{$ELSE} {$J+}
const
  UseNewAPI: (Maybe, No, Yes) = Maybe;
  GetTimeZoneInformationForYear: function(wYear: Word; DynInfo: Pointer;
    var TZInfo: TTimeZoneInformation): BOOL; stdcall = nil;
  TzSpecificLocalTimeToSystemTime: function(var TZInfo: TTimeZoneInformation;
    const LocalTime: TSystemTime; out UTCTime: TSystemTime): BOOL; stdcall = nil;
var
  DoFallback: Boolean;
  LocalTime, UTCTime: TSystemTime;
  TimeZoneInfo: TTimeZoneInformation;
begin
  Result := 0;
  DoFallback := True;
  if UseNewAPI = Maybe then
  begin
    GetTimeZoneInformationForYear := GetProcAddress(GetModuleHandle(kernel32), 'GetTimeZoneInformationForYear');
    TzSpecificLocalTimeToSystemTime := GetProcAddress(GetModuleHandle(kernel32), 'TzSpecificLocalTimeToSystemTime');
    if Assigned(GetTimeZoneInformationForYear) and Assigned(TzSpecificLocalTimeToSystemTime) then
      UseNewAPI := Yes
    else
      UseNewAPI := No;
  end;
  if UseNewAPI = Yes then
  begin
    DateTimeToSystemTime(Value, LocalTime);
    if GetTimeZoneInformationForYear(LocalTime.wYear, nil, TimeZoneInfo) then
      if TzSpecificLocalTimeToSystemTime(TimeZoneInfo, LocalTime, UTCTime) then
      begin
        Result := Round(MinsPerDay * (Value - SystemTimeToDateTime(UTCTime)));
        DoFallback := False;
      end;
  end;
  if DoFallback then
    case GetTimeZoneInformation(TimeZoneInfo) of
      TIME_ZONE_ID_UNKNOWN: Result := TimeZoneInfo.Bias;
      TIME_ZONE_ID_STANDARD: Result := TimeZoneInfo.Bias + TimeZoneInfo.StandardBias;
      TIME_ZONE_ID_DAYLIGHT: Result := TimeZoneInfo.Bias + TimeZoneInfo.DaylightBias;
    end;
  AHours := Abs(Result) div MinsPerHour;
  AMins := Abs(Result) mod MinsPerHour;
{$ENDIF}
end;

function DateTimeToXMPString(const Value: TDateTime; ApplyLocalBias: Boolean): UnicodeString;
const
  PlusNegSyms: array[Boolean] of string = ('-', '+');
var
  Hours, Mins: Integer;
begin
  Result := FormatDateTime('yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz', Value);
  if ApplyLocalBias then
    Result := Format('%s%s%.2d:%.2d', [Result,
      PlusNegSyms[GetUTCOffset(Value, Hours, Mins) >= 0], Hours, Mins]);
end;

function XMPStringToDateTime(const Value: string; AsUTCTime: Boolean = False): TDateTime;
var
  SeekPtr: PChar;

  function ReadDigit: Byte;
  begin
    case SeekPtr^ of
      '0'..'9': Result := Ord(SeekPtr^) - Ord('0');
    else raise EConvertError.CreateResFmt(@SInvalidInteger, [string(SeekPtr^)]);
    end;
    Inc(SeekPtr);
  end;

  function Read2Digits: Word;
  begin
    Result := ReadDigit * 10 + ReadDigit;
  end;

  function Read4Digits: Word;
  begin
    Result := ReadDigit * 1000 + ReadDigit * 100 + ReadDigit * 10 + ReadDigit;
  end;

  function Read2DigitsAfter(CharsToSkip: Integer): Word;
  begin
    Inc(SeekPtr, CharsToSkip);
    Result := ReadDigit * 10 + ReadDigit;
  end;
var
  Minus: Boolean;
  Year, Month, Day, Hour, Min, Sec, MSecs: Integer;
  Offset: TDateTime;
begin
  SeekPtr := PChar(Value);
  Year := Read4Digits;
  Month := Read2DigitsAfter(1);
  Day := Read2DigitsAfter(1);
  Result := EncodeDate(Year, Month, Day);
  if SeekPtr^ = #0 then Exit;
  Hour := Read2DigitsAfter(1);
  Min := Read2DigitsAfter(1);
  Sec := Read2DigitsAfter(1);
  MSecs := 0;
  if SeekPtr^ = '.' then
  begin
    Inc(SeekPtr);
    repeat
      MSecs := MSecs * 10 + ReadDigit;
    until CharInSet(SeekPtr^, [#0, 'Z', 'z', '-', '+']);
  end;
  Result := Result + EncodeTime(Hour, Min, Sec, MSecs);
  if AsUTCTime then Exit;
  case SeekPtr^ of
    #0, 'Z', 'z': Exit;
    '-': Minus := True;
    '+': Minus := False;
  else raise EConvertError.CreateResFmt(@SInvalidDateTime, [Value]);
  end;
  Hour := Read2DigitsAfter(1);
  Min := Read2DigitsAfter(1);
  Offset := EncodeTime(Hour, Min, 0, 0);
  if Minus then
    Result := Result - Offset
  else
    Result := Result + Offset;
end;

function DefinesNS(const Attr: IDOMNode): Boolean;
begin
  Result := (Attr.prefix = 'xmlns') or (Attr.namespaceURI = 'http://www.w3.org/2000/xmlns/');
end;

function EscapeXML(const Source: UnicodeString): UnicodeString;
var
  Ch: WideChar;
begin
  with TMemoryStream.Create do
  try
    for Ch in Source do
      case Ch of
        '<': WriteWideChars('&lt;', SmallEndian);
        '>': WriteWideChars('&gt;', SmallEndian);
        '&': WriteWideChars('&amp;', SmallEndian);
        '''': WriteWideChars('&apos;', SmallEndian);
        '"': WriteWideChars('&quot;', SmallEndian);
      else WriteBuffer(Ch, 2);
      end;
    SetString(Result, PWideChar(Memory), Size div 2);
  finally
    Free;
  end;
end;

function FindRootRDFNode(const Document: IDOMDocument; out Node: IDOMNode): Boolean;
begin
  Result := True;
  Node := Document.firstChild;
  while Node <> nil do
  begin
    if Node.nodeType = ELEMENT_NODE then
      case IndexStr(Node.localName, ['RDF', 'xmpmeta', 'xapmeta']) of
        0: Exit; //support ExifTool's XML dumps, which don't parent the RDF node
        1..2: Break;
      end;
    Node := Node.nextSibling;
  end;
  if Node <> nil then
  begin
    Node := Node.firstChild;
    while Node <> nil do
    begin
      if (Node.nodeName = 'rdf:RDF') and (Node.nodeType = ELEMENT_NODE) then Break;
      Node := Node.nextSibling;
    end;
  end;
  Result := (Node <> nil);
end;

function HasXMPSegmentHeader(Stream: TStream): Boolean;
begin
  Result := Stream.TryReadHeader(TJPEGSegment.XMPHeader, SizeOf(TJPEGSegment.XMPHeader), True);
end;

procedure UpdateChildNamespaceInfos(const Parent: IXMPPropertyCollection);
var
  Prop: TXMPProperty;
begin
  for Prop in Parent do
    if Prop.ParentNamespace then
    begin
      Prop.NamespaceInfo.DoAssign(Parent.NamespaceInfo);
      UpdateChildNamespaceInfos(Prop);
    end;
end;

{ TKnownXMPNamespaces }

class function TKnownXMPNamespaces.GetPreferredPrefix(Namespace: TXMPKnownNamespace): UnicodeString;
begin
  Result := PreferredPrefixes[Namespace];
end;

class function TKnownXMPNamespaces.GetURI(Namespace: TXMPKnownNamespace): UnicodeString;
begin
  Result := KnownURIs[Namespace];
end;

class function TKnownXMPNamespaces.Find(const URI: UnicodeString; out Namespace: TXMPNamespace): Boolean;
var
  I: TXMPKnownNamespace;
begin
  for I := Low(KnownURIs) to High(KnownURIs) do
    if URI = KnownURIs[I] then
    begin
      Namespace := I;
      Result := True;
      Exit;
    end;
  Namespace := xsUnknown;
  Result := False;
end;

{ TStringListThatOwnsItsObjects }

destructor TStringListThatOwnsItsObjects.Destroy;
begin
  Clear;
  inherited;
end;

procedure TStringListThatOwnsItsObjects.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    {$IFDEF NEXTGEN}
    Objects[I].DisposeOf;
    {$ELSE}
    Objects[I].Free;
    {$ENDIF}
  inherited;
end;

procedure TStringListThatOwnsItsObjects.Delete(Index: Integer);
begin
  {$IFDEF NEXTGEN}
  Objects[Index].DisposeOf;
  {$ELSE}
  Objects[Index].Free;
  {$ENDIF}
  inherited;
end;

{ TXMPPropertyEnumerator }

constructor TXMPPropertyEnumerator.Create(Source: TObjectList);
begin
  FIndex := -1;
  FSource := Source;
end;

function TXMPPropertyEnumerator.GetCurrent: TXMPProperty;
begin
  Result := TXMPProperty(FSource[FIndex]);
end;

function TXMPPropertyEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := (FIndex < FSource.Count);
end;

{ TXMPNamespaceInfo }

constructor TXMPNamespaceInfo.Create(AKind: TXMPNamespace;
  const APreferredPrefix, AURI: UnicodeString);
begin
  inherited Create;
  FKind := AKind;
  FCustomPrefix := APreferredPrefix;
  FCustomURI := AURI;
end;

constructor TXMPNamespaceInfo.Create(AKind: TXMPKnownNamespace);
begin
  Create(AKind, '', '');
end;

constructor TXMPNamespaceInfo.Create(const APreferredPrefix, AURI: UnicodeString);
begin
  Create(xsUnknown, APreferredPrefix, AURI);
end;

constructor TXMPNamespaceInfo.Create(ASource: TXMPNamespaceInfo);
begin
  inherited Create;
  Assign(ASource);
end;

procedure TXMPNamespaceInfo.Assign(Source: TPersistent);
begin
  if Source is TXMPNamespaceInfo then
  begin
    DoAssign(TXMPNamespaceInfo(Source));
    Changed;
    Exit;
  end;
  inherited;
end;

procedure TXMPNamespaceInfo.Assign(AKind: TXMPKnownNamespace);
begin
  if FKind = AKind then Exit;
  FCustomPrefix := '';
  FCustomURI := '';
  FKind := AKind;
  Changed;
end;

procedure TXMPNamespaceInfo.Assign(const APrefix, AURI: UnicodeString);
begin
  if DoAssign(APrefix, AURI) then Changed;
end;

procedure TXMPNamespaceInfo.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TXMPNamespaceInfo.DoAssign(Source: TXMPNamespaceInfo);
begin
  FCustomPrefix := Source.FCustomPrefix;
  FCustomURI := Source.FCustomURI;
  FKind := Source.FKind;
end;

function TXMPNamespaceInfo.DoAssign(const APrefix, AURI: UnicodeString): Boolean;
begin
  Result := (AURI <> URI) or (APrefix <> Prefix);
  if not Result then Exit;
  if TKnownXMPNamespaces.Find(AURI, FKind) and (AURI = TKnownXMPNamespaces.PreferredPrefix[FKind]) then
  begin
    FCustomPrefix := '';
    FCustomURI := '';
  end
  else
  begin
    FCustomPrefix := APrefix;
    FCustomURI := AURI;
  end;
end;

function TXMPNamespaceInfo.GetPrefix: UnicodeString;
begin
  if (FCustomPrefix <> '') or (FKind = xsUnknown) then
    Result := FCustomPrefix
  else
    Result := TKnownXMPNamespaces.PreferredPrefix[FKind];
end;

function TXMPNamespaceInfo.GetURI: UnicodeString;
begin
  if FKind = xsUnknown then
    Result := FCustomURI
  else
    Result := TKnownXMPNamespaces.URI[FKind];
end;

procedure TXMPNamespaceInfo.SetPrefix(const Value: UnicodeString);
begin
  if Value = Prefix then Exit;
  FCustomPrefix := Value;
  Changed;
end;

{ TXMPProperty }

class function TXMPProperty.HasNamedSubProperties(Kind: TXMPPropertyKind): Boolean;
begin
  Result := (Kind in [xpStructure, xpAltArray]);
end;

class function TXMPProperty.SupportsSubProperties(Kind: TXMPPropertyKind): Boolean;
begin
  Result := (Kind in [xpAltArray, xpBagArray, xpSeqArray, xpStructure]);
end;

constructor TXMPProperty.Create(ASchema: TXMPSchema; AParentProperty: TXMPProperty;
  const ASourceNode: IDOMNode);

  procedure UseSourceNodeNameAndNamespace;
  begin
    FName := ASourceNode.localName;
    if FNamespaceInfo.DoAssign(ASourceNode.prefix, ASourceNode.namespaceURI) then
      FParentNamespace := False;
  end;
var
  Attrs: IDOMNamedNodeMap;
  ChildNode, DataNode, TextNode: IDOMNode;
  DoAdd: Boolean;
  I: Integer;
  PropAndNodeStructure: Boolean;
  S: string;
  SourceAsElem: IDOMElement;
begin
  inherited Create;
  FParentNamespace := True;
  FNamespaceInfo := TXMPNamespaceInfo.Create(ASchema.NamespaceInfo);
  if AParentProperty <> nil then
    FNamespaceInfo.DoAssign(AParentProperty.NamespaceInfo)
  else if ASchema <> nil then
    FNamespaceInfo.DoAssign(ASchema.NamespaceInfo)
  else
    FParentNamespace := False;
  FParentProperty := AParentProperty;
  FSchema := ASchema;
  FSubProperties := TObjectList.Create;
  if ASourceNode = nil then Exit;
  //figure out our kind
  PropAndNodeStructure := Supports(ASourceNode, IDOMElement, SourceAsElem) and
    (SourceAsElem.getAttributeNS(RDF.URI, 'parseType') = 'Resource'); //http://www.w3.org/TR/REC-rdf-syntax/#section-Syntax-parsetype-resource
  if PropAndNodeStructure then
  begin
    FKind := xpStructure;
    DataNode := ASourceNode;
  end
  else
  begin
    DataNode := ASourceNode.firstChild;
    if DataNode <> nil then
    begin
      repeat
        { Originally, this loop was exited as soon as a text node was found. This
          however doesn't work with the ADOM backend (as used by default on OS X)
          because ADOM can intepret whitespace prior to a child element as a text
          node of the parent. }
        case DataNode.nodeType of
          TEXT_NODE: TextNode := DataNode;
          ELEMENT_NODE:
          begin
            S := DataNode.nodeName;
            if S = RDF.AltNodeName then
              FKind := xpAltArray
            else if S = RDF.BagNodeName then
              FKind := xpBagArray
            else if S = RDF.SeqNodeName then
              FKind := xpSeqArray
            else if S = RDF.DescriptionNodeName then
              FKind := xpStructure;
            Break;
          end;
        end;
        DataNode := DataNode.nextSibling;
      until (DataNode = nil);
      if DataNode = nil then DataNode := TextNode;
    end;
  end;
  { Get the value, if appropriate, and check for super-inlined structures. For life of
    me I can't see where in the XMP spec the latter are allowed - cf. p.19 of the spec
    pt.1 PDF), but Adobe's XMP Toolkit can output them, at least in v4.x. }
  if FKind = xpSimple then
    if DataNode <> nil then
      FValue := DataNode.nodeValue
    else if ASourceNode.attributes <> nil then
    begin
      Attrs := ASourceNode.attributes;
      for I := 0 to Attrs.length - 1 do
        if not DefinesNS(Attrs[I]) then
        begin
          DataNode := ASourceNode;
          FKind := xpStructure;
          Break;
        end;
    end;
  //get the name
  if ParentProperty = nil then
    UseSourceNodeNameAndNamespace
  else
    case ParentProperty.Kind of
      xpBagArray, xpSeqArray: FName := '';
      xpAltArray:
        if SourceAsElem <> nil then
        begin
          FName := SourceAsElem.getAttribute(XMLLangAttrName);
          if FName = '' then //fix for ADOM
            for I := SourceAsElem.attributes.length - 1 downto 0 do
              if SourceAsElem.attributes[I].nodeName = XMLLangAttrName then
              begin
                FName := SourceAsElem.attributes[I].nodeValue;
                Break;
              end;
        end
        else
          FName := '';
    else UseSourceNodeNameAndNamespace;
    end;
  //load any sub-props
  if not SupportsSubProperties then Exit;
  Assert(DataNode <> nil);
  Attrs := DataNode.attributes;
  if (FKind = xpStructure) and not PropAndNodeStructure and (Attrs <> nil) then
  begin
    FSubProperties.Capacity := Attrs.length;
    for I := 0 to FSubProperties.Capacity - 1 do
    begin
      ChildNode := Attrs[I];
      if not DefinesNS(ChildNode) then
        FSubProperties.Add(TXMPProperty.Create(Schema, Self, ChildNode));
    end;
  end;
  FSubProperties.Capacity := FSubProperties.Count + DataNode.childNodes.length;
  ChildNode := DataNode.firstChild;
  while ChildNode <> nil do
  begin
    DoAdd := False;
    if ChildNode.nodeType = ELEMENT_NODE then
    begin
      if FKind = xpStructure then
        DoAdd := True
      else if ChildNode.nodeName = RDF.ListNodeName then
        if (FKind <> xpAltArray) or (ChildNode.attributes.getNamedItem(XMLLangAttrName) <> nil) then
          DoAdd := True
        else //fix for ADOM added v1.5.2
          for I := ChildNode.attributes.length - 1 downto 0 do
            if ChildNode.attributes[I].nodeName = XMLLangAttrName then
            begin
              DoAdd := True;
              Break;
            end;
    end;
    if DoAdd then
      FSubProperties.Add(TXMPProperty.Create(Schema, Self, ChildNode));
    ChildNode := ChildNode.nextSibling;
  end;
  FNamespaceInfo.OnChange := NamespaceInfoChanged;
end;

destructor TXMPProperty.Destroy;
begin
   FreeAndNil(FNamespaceInfo);
   FreeAndNil(FSubProperties);
  inherited;
end;

function TXMPProperty.AddSubProperty(const AName: UnicodeString): TXMPProperty;
begin
  if not SupportsSubProperties then
    raise EInvalidXMPOperation.CreateRes(@SSubPropertiesNotSupported);
  if HasNamedSubProperties and (AName = '') then
    raise EInvalidXMPOperation.CreateRes(@SSubPropertiesMustBeNamed);
  Result := TXMPProperty.Create(Schema, Self);
  FSubProperties.Add(Result);
  if HasNamedSubProperties then Result.FName := AName;
  Changed;
end;

procedure TXMPProperty.Changed;
begin
  if FSchema <> nil then FSchema.Changed;
end;

function TXMPProperty.FindSubProperty(const AName: UnicodeString; out Prop: TXMPProperty): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := SubPropertyCount - 1 downto 0 do
  begin
    Prop := SubProperties[I];
    if Prop.Name = AName then Exit;
  end;
  Prop := nil;
  Result := False;
end;

function TXMPProperty.GetEnumerator: IXMPPropertyEnumerator;
begin
  Result := TXMPPropertyEnumerator.Create(FSubProperties);
end;

function TXMPProperty.GetNamespaceInfo: TXMPNamespaceInfo;
begin
  Result := FNamespaceInfo;
end;

function TXMPProperty.GetSubProperty(Index: Integer): TXMPProperty;
begin
  Result := TXMPProperty(FSubProperties[Index]);
end;

function TXMPProperty.GetSubPropertyByName(const AName: UnicodeString): TXMPProperty;
begin
  if not FindSubProperty(AName, Result) then
    Result := AddSubProperty(AName);
end;

function TXMPProperty.GetSubPropertyCount: Integer;
begin
  Result := FSubProperties.Count;
end;

procedure TXMPProperty.NamespaceInfoChanged(Sender: TObject);
begin
  FParentNamespace := False;
  UpdateChildNamespaceInfos(Self);
  Changed;
end;

function TXMPProperty.ReadValue(const Default: Boolean): Boolean;
var
  S: string;
begin
  S := ReadValue;
  if SameText(S, XMPBoolStrs[True]) then
    Result := True
  else if SameText(S, XMPBoolStrs[False]) then
    Result := False
  else if S = '1' then
    Result := True
  else if S = '0' then
    Result := False
  else
    Result := Default;
end;

function TXMPProperty.ReadValue(const Default: Integer): Integer;
begin
  Result := StrToIntDef(ReadValue, Default);
end;

function TXMPProperty.ReadValue(const Default: TDateTime): TDateTime;
begin
  try
    Result := XMPStringToDateTime(ReadValue);
  except
    on EConvertError do
      Result := Default;
  end;
end;

function TXMPProperty.ReadValue(const Default: UnicodeString): UnicodeString;
var
  I: Integer;
begin
  case Kind of
    xpSimple: if Pointer(FValue) <> nil then Result := FValue else Result := Default;
    xpAltArray: Result := SubProperties[DefaultLangIdent].ReadValue(Default);
    xpBagArray, xpSeqArray:
      case SubPropertyCount of
        0: Result := Default;
        1: Result := SubProperties[0].ReadValue(Default);
      else
        Result := SubProperties[0].ReadValue;
        for I := 1 to SubPropertyCount - 1 do
          Result := Result + ',' + SubProperties[I].ReadValue;
      end;
  else Result := Default;
  end;
end;

function TXMPProperty.RemoveSubProperty(const AName: UnicodeString): Boolean;
var
  I: Integer;
  Prop: TXMPProperty;
begin
  Result := False;
  for I := 0 to SubPropertyCount - 1 do
  begin
    Prop := TXMPProperty(FSubProperties[I]);
    if AName = Prop.Name then
    begin
      FSubProperties.Delete(I);
      Result := True;
      Changed;
      Exit;
    end;
  end;
end;

procedure TXMPProperty.SetKind(const Value: TXMPPropertyKind);
var
  I: Integer;
  SubProp: TXMPProperty;
begin
  if Value = Kind then Exit;
  if not SupportsSubProperties(Value) then
    SubPropertyCount := 0
  else
    for I := SubPropertyCount - 1 downto 0 do
    begin
      SubProp := SubProperties[I];
      if not HasNamedSubProperties(Value) then
        SubProp.FName := ''
      else if SubProp.FName = '' then
        SubProp.FName := Format('SubProp%d', [I]);
    end;
  FKind := Value;
  Changed;
end;

procedure TXMPProperty.SetName(const Value: UnicodeString);
begin
  if Value = FName then Exit;
  Assert(Value <> '');
  FName := Value;
  Changed;
end;

procedure TXMPProperty.SetParentNamespace(Value: Boolean);
begin
  if Value = FParentNamespace then Exit;
  FParentNamespace := Value;
  if Value then
    if ParentProperty <> nil then
      FNamespaceInfo.DoAssign(ParentProperty.NamespaceInfo)
    else
      FNamespaceInfo.DoAssign(Schema.NamespaceInfo);
  Changed;
end;

procedure TXMPProperty.SetSubPropertyCount(NewCount: Integer);
var
  I: Integer;
  NewSubProp: TXMPProperty;
begin
  if NewCount < 0 then NewCount := 0;
  if NewCount = FSubProperties.Count then Exit;
  if not SupportsSubProperties then
    raise EInvalidXMPOperation.CreateRes(@SSubPropertiesNotSupported);
  if NewCount < FSubProperties.Count then
    FSubProperties.Count := NewCount
  else
    for I := FSubProperties.Count to NewCount - 1 do
    begin
      NewSubProp := TXMPProperty.Create(Schema, Self);
      if HasNamedSubProperties then NewSubProp.FName := Format('SubProp%d', [I]);
      FSubProperties.Add(NewSubProp);
    end;
  Changed;
end;

function TXMPProperty.HasNamedSubProperties: Boolean;
begin
  Result := HasNamedSubProperties(Kind);
end;

function TXMPProperty.SupportsSubProperties: Boolean;
begin
  Result := SupportsSubProperties(Kind);
end;

procedure TXMPProperty.UpdateSubProperty(const SubPropName: UnicodeString;
  SubPropKind: TXMPPropertyKind; const NewValue: UnicodeString);
var
  SubProp: TXMPProperty;
begin
  if (FSchema.Owner.UpdatePolicy = xwRemove) or (NewValue = '') then
  begin
    RemoveSubProperty(SubPropName);
    Exit;
  end;
  if not FindSubProperty(SubPropName, SubProp) then
  begin
    if FSchema.Owner.UpdatePolicy = xwUpdateIfExists then Exit;
    if not (Kind in [xpStructure, xpAltArray]) then Kind := xpStructure;
    SubProp := AddSubProperty(SubPropName)
  end;
  SubProp.Kind := SubPropKind;
  SubProp.WriteValue(NewValue);
end;

procedure TXMPProperty.UpdateSubProperty(const SubPropName, NewValue: UnicodeString);
begin
  UpdateSubProperty(SubPropName, xpSimple, NewValue);
end;

procedure TXMPProperty.UpdateSubProperty(const SubPropName: UnicodeString; NewValue: Integer);
begin
  UpdateSubProperty(SubPropName, xpSimple, IntToStr(NewValue));
end;

procedure TXMPProperty.UpdateSubProperty(const SubPropName: UnicodeString; NewValue: Boolean);
begin
  UpdateSubProperty(SubPropName, xpSimple, XMPBoolStrs[NewValue]);
end;

procedure TXMPProperty.WriteValue(const NewValue: UnicodeString);
var
  I, BeginPos, TotalLen: Integer;
  Strings: TUnicodeStringList;
begin
  case Kind of
    xpSimple:
      if NewValue <> FValue then
      begin
        FValue := NewValue;
        Changed;
      end;
    xpStructure: raise EInvalidXMPOperation.CreateRes(@SCannotWriteSingleValueToStructureProperty);
    xpAltArray: SubProperties[DefaultLangIdent].WriteValue(NewValue);
  else
    Strings := TUnicodeStringList.Create;
    try
      BeginPos := 1;
      TotalLen := Length(NewValue);
      for I := 1 to TotalLen do
        if IsCharIn(NewValue[I], [',', ';']) then
        begin
          Strings.Add(Copy(NewValue, BeginPos, I - BeginPos));
          BeginPos := I + 1;
        end;
      if BeginPos <= TotalLen then Strings.Add(Copy(NewValue, BeginPos, TotalLen));
      SubPropertyCount := Strings.Count;
      for I := 0 to Strings.Count - 1 do
        SubProperties[I].WriteValue(Strings[I]);
    finally
       FreeAndNil(Strings);
    end;
  end;
end;

procedure TXMPProperty.WriteValue(const NewValue: TDateTime);
begin
  WriteValue(DateTimeToXMPString(NewValue));
end;

procedure TXMPProperty.WriteValue(const NewValue: Integer);
begin
  WriteValue(IntToStr(NewValue));
end;

procedure TXMPProperty.WriteValue(const NewValue: Boolean);
begin
  WriteValue(XMPBoolStrs[NewValue]);
end;

{ TXMPSchema }

constructor TXMPSchema.Create(AOwner: TXMPPacket; const AURI: UnicodeString);
var
  Kind: TXMPNamespace;
begin
  if TKnownXMPNamespaces.Find(AURI, Kind) then
    FNamespaceInfo := TXMPNamespaceInfo.Create(Kind)
  else
    FNamespaceInfo := TXMPNamespaceInfo.Create('', AURI);
  FNamespaceInfo.OnChange := NamespaceInfoChanged;
  FOwner := AOwner;
  FProperties := TObjectList.Create;
end;

destructor TXMPSchema.Destroy;
begin
   FreeAndNil(FNamespaceInfo);
   FreeAndNil(FProperties);
  inherited;
end;

function TXMPSchema.AddProperty(const AName: UnicodeString): TXMPProperty;
begin
  if NamespaceInfo.Prefix = '' then
    raise EInvalidXMPOperation.CreateRes(@SPreferredPrefixMustBeSet);
  Result := TXMPProperty.Create(Self, nil);
  FProperties.Add(Result);
  if AName <> '' then
    Result.Name := AName
  else
    Changed;
end;

procedure TXMPSchema.Changed;
begin
  if FOwner <> nil then FOwner.Changed;
end;

function TXMPSchema.FindProperty(const AName: UnicodeString; var AProperty: TXMPProperty): Boolean;
var
  I: Integer;
begin
  for I := 0 to PropertyCount - 1 do
  begin
    AProperty := Properties[I];
    if AProperty.Name = AName then
    begin
      Result := True;
      Exit;
    end;
  end;
  AProperty := nil;
  Result := False;
end;

function TXMPSchema.FindOrAddProperty(const AName: UnicodeString): TXMPProperty;
begin
  if not FindProperty(AName, Result) then
    Result := AddProperty(AName);
end;

function TXMPSchema.GetEnumerator: IXMPPropertyEnumerator;
begin
  Result := TXMPPropertyEnumerator.Create(FProperties);
end;

function TXMPSchema.GetNamespaceInfo: TXMPNamespaceInfo;
begin
  Result := FNamespaceInfo;
end;

function TXMPSchema.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TXMPSchema.GetProperty(Index: Integer): TXMPProperty;
begin
  Result := TXMPProperty(FProperties[Index]);
end;

function TXMPSchema.GetPropertyCount: Integer;
begin
  Result := FProperties.Count;
end;

function TXMPSchema.LoadProperty(const ASourceNode: IDOMNode): TXMPProperty;
begin                          
  FLoadingProperty := True;
  try
    if FProperties.Count = 0 then
      NamespaceInfo.Prefix := ASourceNode.prefix;
    Result := TXMPProperty.Create(Self, nil, ASourceNode);
    FProperties.Add(Result);
  finally
    FLoadingProperty := False;
  end;
end;

procedure TXMPSchema.NamespaceInfoChanged(Sender: TObject);
begin
  UpdateChildNamespaceInfos(Self);
  if not FLoadingProperty then Changed;
end;

function TXMPSchema.RemoveProperty(const AName: UnicodeString): Boolean;
begin
  Result := RemoveProperties([AName]);
end;

function TXMPSchema.RemoveProperties(const ANames: array of UnicodeString): Boolean;
var
  I, J: Integer;
  Prop: TXMPProperty;
begin
  Result := False;
  for I := FProperties.Count - 1 downto 0 do
  begin
    Prop := TXMPProperty(FProperties[I]);
    for J := High(ANames) downto Low(ANames) do
      if Prop.Name = ANames[J] then
      begin
        FProperties.Delete(I);
        Result := True;
        Break;
      end;
  end;
end;

{ TXMPPacket }

constructor TXMPPacket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSchemas := TStringListThatOwnsItsObjects.Create;
  FSchemas.CaseSensitive := False;
  FSchemas.Sorted := True;
  FTiffRewriteCallback := TSimpleTiffRewriteCallbackImpl.Create(Self, ttXMP);
end;

class function TXMPPacket.CreateAsSubComponent(AOwner: TComponent): TXMPPacket;
begin
  Result := Create(AOwner);
  Result.Name := 'XMPPacket';
  Result.SetSubComponent(True);
end;

destructor TXMPPacket.Destroy;
begin
   FreeAndNil(FSchemas);
   FreeAndNil(FTiffRewriteCallback);
  inherited;
end;

procedure TXMPPacket.Assign(Source: TPersistent);
begin
  if Source = nil then
    Clear
  else if Source is TXMPPacket then
  begin
    if TXMPPacket(Source).DataToLazyLoad <> nil then
      DataToLazyLoad := TXMPPacket(Source).DataToLazyLoad
    else
      RawXML := TXMPPacket(Source).RawXML;
  end
  else if Source is TStrings then
    RawXML := UTF8Encode(TStrings(Source).Text)
  else if Source is TUnicodeStrings then
    RawXML := UTF8Encode(TUnicodeStrings(Source).Text)
  else
    inherited;
end;

procedure TXMPPacket.AssignTo(Dest: TPersistent);
begin
  if Dest is TStrings then
    TStrings(Dest).Text := UTF8ToString(RawXML)
  else if Dest is TUnicodeStrings then
    TUnicodeStrings(Dest).Text := UTF8ToString(RawXML)
  else
    inherited;
end;

procedure TXMPPacket.Changed(AResetRawXMLCache: Boolean = True);
begin
  if AResetRawXMLCache then FRawXMLCache := '';
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TXMPPacket.ResetRawXMLCache;
begin
  FRawXMLCache := '';
end;

procedure TXMPPacket.Clear(StillUpdating: Boolean);
begin
  FDataToLazyLoad := nil;
  if (FSchemas.Count = 0) and (FAboutAttributeValue = '') then Exit;
  FAboutAttributeValue := '';
  FSchemas.Clear;
  if not StillUpdating then Changed;
end;

procedure TXMPPacket.Clear;
begin
  Clear(False);
end;

procedure TXMPPacket.DoSaveToJPEG(InStream, OutStream: TStream);
begin
  UpdateApp1JPEGSegments(InStream, OutStream, nil, Self);
end;

procedure TXMPPacket.DoSaveToPSD(InStream, OutStream: TStream);
var
  Block: IAdobeResBlock;
  Info: TPSDInfo;
  NewBlocks: IInterfaceList;
  StartPos: Int64;
begin
  StartPos := InStream.Position;
  NewBlocks := TInterfaceList.Create;
  if not Empty then NewBlocks.Add(CreateAdobeBlock(TAdobeResBlock.XMPTypeID, Self));
  for Block in ParsePSDHeader(InStream, Info) do
    if not Block.IsXMPBlock then NewBlocks.Add(Block);
  WritePSDHeader(OutStream, Info.Header);
  WritePSDResourceSection(OutStream, NewBlocks);
  InStream.Position := StartPos + Info.LayersSectionOffset;
  OutStream.CopyFrom(InStream, InStream.Size - InStream.Position);
end;

procedure TXMPPacket.DoSaveToTIFF(InStream, OutStream: TStream);
begin
  RewriteTiff(InStream, OutStream, Self);
end;

function TXMPPacket.FindSchema(const URI: UnicodeString; var Schema: TXMPSchema): Boolean;
var
  Index: Integer;
begin
  NeedLazyLoadedData;
  Result := FSchemas.Find(URI, Index);
  if Result then Schema := FSchemas.Objects[Index] as TXMPSchema;
end;

function TXMPPacket.FindSchema(Kind: TXMPKnownNamespace; var Schema: TXMPSchema): Boolean;
begin
  Result := FindSchema(TKnownXMPNamespaces.URI[Kind], Schema);
end;

function TXMPPacket.FindOrAddSchema(const URI: UnicodeString): TXMPSchema;
var
  Index: Integer;
begin
  NeedLazyLoadedData;
  if FSchemas.Find(URI, Index) then
    Result := FSchemas.Objects[Index] as TXMPSchema
  else
  begin
    Result := TXMPSchema.Create(Self, URI);
    FSchemas.AddObject(URI, Result);
  end;
end;

function TXMPPacket.FindOrAddSchema(Kind: TXMPKnownNamespace): TXMPSchema;
begin
  Result := FindOrAddSchema(TKnownXMPNamespaces.URI[Kind]);
end;

procedure TXMPPacket.SetAboutAttributeValue(const Value: UnicodeString);
begin
  NeedLazyLoadedData;
  if Value = FAboutAttributeValue then Exit;
  FAboutAttributeValue := Value;
  Changed;
end;

function TXMPPacket.GetEmpty: Boolean;
begin
  Result := (DataToLazyLoad = nil) and (SchemaCount = 0);
end;

function TXMPPacket.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TXMPPacket.GetGraphicSaveMethod(Stream: TStream; var Method: TGraphicSaveMethod);
begin
  if HasJPEGHeader(Stream) then
    Method := DoSaveToJPEG
  else if HasPSDHeader(Stream) then
    Method := DoSaveToPSD
  else if HasTiffHeader(Stream) then
    Method := DoSaveToTIFF
end;

function TXMPPacket.GetRawXML: UTF8String;
var
  Stream: TMemoryStream;
begin
  if FRawXMLCache = '' then
  begin
    Stream := TMemoryStream.Create;
    try
      SaveToStream(Stream);
      SetString(FRawXMLCache, PAnsiChar(Stream.Memory), Stream.Size);
    finally
       FreeAndNil(Stream);
    end;
  end;
  Result := FRawXMLCache;
end;

function TXMPPacket.GetSchema(Index: Integer): TXMPSchema;
begin
  NeedLazyLoadedData;
  Result := FSchemas.Objects[Index] as TXMPSchema;
end;

function TXMPPacket.GetSchemaCount: Integer;
begin
  NeedLazyLoadedData;
  Result := FSchemas.Count;
end;

procedure TXMPPacket.LoadError(Source: TStream);
begin
  if Assigned(FOnLoadError) then
    FOnLoadError(Self, Source)
  else
    raise EInvalidXMPPacket.CreateRes(@SInvalidXMPPacket);
end;

procedure TXMPPacket.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
     FreeAndNil(Stream);
  end;
end;

function TXMPPacket.LoadFromGraphic(Stream: TStream): Boolean;
var
  Block: IAdobeResBlock;
  Directory: IFoundTiffDirectory;
  PSDInfo: TPSDInfo;
  Segment: IFoundJPEGSegment;
  Tag: ITiffTag;
begin
  Result := True;
  if HasJPEGHeader(Stream) then
  begin
    for Segment in JPEGHeader(Stream, [jmApp1]) do
      if Segment.IsXMPBlock then
      begin
        LoadFromStream(Segment.Data);
        Exit;
      end;
  end
  else if HasPSDHeader(Stream) then
  begin
    for Block in ParsePSDHeader(Stream, PSDInfo) do
      if Block.IsXMPBlock then
      begin
        LoadFromStream(Block.Data);
        Exit;
      end;
  end
  else if HasTiffHeader(Stream) then
    for Directory in ParseTiff(Stream) do
    begin
      if Directory.FindTag(ttXMP, Tag) and Tag.IsXMPBlock then
      begin
        LoadFromStream(Tag.Data);
        Exit;
      end;
      Break; //only interested in the main IFD
    end
  else
    Result := False;
  Clear;
end;

function TXMPPacket.LoadFromGraphic(const Graphic: IStreamPersist): Boolean;
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

function TXMPPacket.LoadFromGraphic(const FileName: string): Boolean;
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

procedure TXMPPacket.LoadFromStream(Stream: TStream);
begin
  if not TryLoadFromStream(Stream) then LoadError(Stream);
end;

procedure TXMPPacket.NeedLazyLoadedData;
var
  Item: IMetadataBlock;
begin
  if FDataToLazyLoad = nil then Exit;
  Item := FDataToLazyLoad;
  FDataToLazyLoad := nil; //in case of an exception, nil this beforehand
  Item.Data.Seek(0, soFromBeginning);
  LoadFromStream(Item.Data);
end;

procedure TXMPPacket.SaveToFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
     FreeAndNil(Stream);
  end;
end;

procedure TXMPPacket.SaveToGraphic(const FileName: string);
begin
  DoSaveToGraphic(FileName, GetGraphicSaveMethod);
end;

procedure TXMPPacket.SaveToGraphic(const Graphic: IStreamPersist);
begin
  DoSaveToGraphic(Graphic, GetGraphicSaveMethod);
end;

procedure TXMPPacket.SaveToStream(Stream: TStream);

  procedure WriteProps(Schema: TXMPSchema; Level: Integer;
    const Props: IXMPPropertyCollection);
  const
    PropNodeStart: UnicodeString = #9#9#9'%s<%s:%s>'#10;
    PropNodeEnd: UnicodeString   = #9#9#9'%s</%s:%s>'#10;
    SimpleFmt: UnicodeString = #9#9#9'%s<%s>%s</%1s>'#10;
  var
    Indent, QualName, NSDecl, RDFName, Value: UnicodeString;
    Prop: TXMPProperty;
    IsArrayElem: Boolean;
  begin
    Indent := StringOfChar(WideChar(#9), Level);
    for Prop in Props do
    begin
      IsArrayElem := (Prop.ParentProperty <> nil) and (Prop.ParentProperty.Kind <> xpStructure);
      if IsArrayElem or Prop.ParentNamespace then
        NSDecl := '' //inherit from parent
      else
        NSDecl := UnicodeFormat(' xmlns:%s="%s"', [Prop.NamespaceInfo.Prefix,
          Prop.NamespaceInfo.URI]);
      QualName := Prop.NamespaceInfo.Prefix + ':' + Prop.Name;
      if IsArrayElem then
      begin
        if Prop.ParentProperty.Kind = xpAltArray then
          Value := UnicodeFormat(' xml:lang="%s"', [Prop.Name])
        else
          Value := '';
        Stream.WriteUTF8Chars('%s<%s%s>', [Indent, RDF.ListNodeName, Value]);
      end;
      if Prop.Kind = xpSimple then
      begin
        Value := EscapeXML(Prop.ReadValue);
        if IsArrayElem then
          Stream.WriteUTF8Chars('%s</%s>'#10, [Value, RDF.ListNodeName])
        else
          Stream.WriteUTF8Chars('%s<%s%s>%s</%1:s>'#10, [Indent, QualName, NSDecl, Value]);
        Continue;
      end;
      if IsArrayElem then
        Stream.WriteByte(10)
      else
        Stream.WriteUTF8Chars('%s<%s%s>'#10, [Indent, QualName, NSDecl]);
      case Prop.Kind of
        xpStructure: RDFName := RDF.DescriptionNodeName;
        xpAltArray: RDFName := RDF.AltNodeName;
        xpBagArray: RDFName := RDF.BagNodeName;
        xpSeqArray: RDFName := RDF.SeqNodeName;
      else Assert(False);
      end;
      Stream.WriteUTF8Chars('%s'#9'<%s>'#10, [Indent, RDFName]);
      WriteProps(Schema, Level + 2, Prop);
      Stream.WriteUTF8Chars('%s'#9'</%s>'#10, [Indent, RDFName]);
      if IsArrayElem then
        Stream.WriteUTF8Chars('%s</%s>'#10, [Indent, RDF.ListNodeName])
      else
        Stream.WriteUTF8Chars('%s</%s>'#10, [Indent, QualName]);
    end;
  end;
const
  PacketStart: UTF8String =
    '<?xpacket begin="" id="W5M0MpCehiHzreSzNTczkc9d"?>'#10 +
    '<x:xmpmeta xmlns:x="adobe:ns:meta/" CCRExifVersion="' + CCRExifVersion + '">'#10 +
    #9'<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">'#10;
  PacketEnd: UTF8String =
    #9'</rdf:RDF>'#10 +
    '</x:xmpmeta>'#10 +
    '<?xpacket end="w"?> ';      //The packet wrapper (i.e., the <?xpacket lines) are optional according to the XMP spec, 
  PaddingByte: AnsiChar = ' ';   //but required by Windows Explorer in Vista. Vista also needs a trailing space character,
  DescNodeStart: UnicodeString = //else it doesn't read and raises a spurious error when the user tries to write.
    #9#9'<rdf:Description rdf:about="%s" xmlns:%s="%s">'#10;
  DescNodeEnd: UTF8String =
    #9#9'</rdf:Description>'#10;
var
  DataPtr: PAnsiChar;
  DataSize: Integer;
  Schema: TXMPSchema;
begin
  if FRawXMLCache <> '' then
  begin
    Stream.WriteBuffer(FRawXMLCache[1], Length(FRawXMLCache));
    if FRawXMLCache[Length(FRawXMLCache)] <> ' ' then
      Stream.WriteBuffer(PaddingByte, 1); //see note above
    Exit;
  end;
  if FDataToLazyLoad <> nil then
  begin
    DataPtr := FDataToLazyLoad.Data.Memory;
    DataSize := FDataToLazyLoad.Data.Size;
    if (DataSize >= SizeOf(TJPEGSegment.XMPHeader)) and
       CompareMem(DataPtr, @TJPEGSegment.XMPHeader, SizeOf(TJPEGSegment.XMPHeader)) then
    begin
      Inc(DataPtr, SizeOf(TJPEGSegment.XMPHeader));
      Dec(DataSize, SizeOf(TJPEGSegment.XMPHeader));
    end;
    Stream.WriteBuffer(DataPtr^, DataSize);
    Exit;
  end;
  Stream.WriteBuffer(PacketStart[1], Length(PacketStart));
  for Schema in Self do
  begin
    Stream.WriteUTF8Chars(DescNodeStart, [AboutAttributeValue,
      Schema.NamespaceInfo.Prefix, Schema.NamespaceInfo.URI]);
    WriteProps(Schema, 3, Schema);
    Stream.WriteBuffer(DescNodeEnd[1], Length(DescNodeEnd));
  end;
  Stream.WriteBuffer(PacketEnd[1], Length(PacketEnd));
end;

procedure TXMPPacket.SetDataToLazyLoad(const Value: IMetadataBlock);
begin
  if Value = FDataToLazyLoad then Exit;
  Clear;
  FDataToLazyLoad := Value;
end;

procedure TXMPPacket.SetRawXML(const XML: UTF8String);
var
  Stream: TUserMemoryStream;
begin
  if XML = '' then
    Clear
  else
  begin
    Stream := TUserMemoryStream.Create(Pointer(XML), Length(XML));
    try
      LoadFromStream(Stream);
    finally
       FreeAndNil(Stream);
    end;
  end;
end;

function TXMPPacket.TryLoadFromStream(Stream: TStream): Boolean;
const
  XPacketStart: array[0..9] of AnsiChar = '<?xpacket ';
var
  I: Integer;
  CharsPtr: PAnsiChar;
  Document: IDOMDocument;
  NewStream: TMemoryStream;
  PropNode, RootRDFNode, SchemaNode: IDOMNode;
  URI: UnicodeString;
begin
  Result := False;
  Stream.TryReadHeader(TJPEGSegment.XMPHeader, SizeOf(TJPEGSegment.XMPHeader)); //doesn't matter whether it was there or not
  Document := GetDOM.createDocument('', '', nil);
  NewStream := TMemoryStream.Create;
  try
    NewStream.SetSize(Stream.Size - Stream.Position);
    Stream.ReadBuffer(NewStream.Memory^, NewStream.Size);
    CharsPtr := NewStream.Memory;
    for I := NewStream.Size - 1 downto 0 do //MSXML chokes on embedded nulls
      if CharsPtr[I] = #0 then CharsPtr[I] := ' ';
    if not (Document as IDOMPersist).loadFromStream(NewStream) then Exit;
    if not FindRootRDFNode(Document, RootRDFNode) then Exit;
    Clear(True);
    if (NewStream.Size > SizeOf(XPacketStart)) and CompareMem(CharsPtr, @XPacketStart, SizeOf(XPacketStart)) then
      SetString(FRawXMLCache, CharsPtr, NewStream.Size)
    else
      FRawXMLCache := UTF8Encode((Document as IDOMPersist).xml)
  finally
     FreeAndNil(NewStream);
  end;
  SchemaNode := RootRDFNode.firstChild;
  while SchemaNode <> nil do
  begin
    if (SchemaNode.nodeType = ELEMENT_NODE) and (SchemaNode.namespaceURI = RDF.URI) and
      UnicodeSameText(SchemaNode.nodeName, RDF.DescriptionNodeName) then
    begin
      if FAboutAttributeValue = '' then
        with SchemaNode as IDOMElement do
        begin
          FAboutAttributeValue := getAttributeNS(RDF.URI, RDF.AboutAttrLocalName);
          if FAboutAttributeValue = '' then
            FAboutAttributeValue := getAttribute(RDF.AboutAttrLocalName);
        end;
      //look for tags stored as attributes
      for I := 0 to SchemaNode.attributes.length - 1 do
      begin
        PropNode := SchemaNode.attributes.item[I];
        if DefinesNS(PropNode) then Continue;
        URI := PropNode.namespaceURI;
        if (URI <> '') and (URI <> RDF.URI) then
          Schemas[URI].LoadProperty(PropNode);
      end;
      //look for tags stored as element nodes
      PropNode := SchemaNode.firstChild;
      while PropNode <> nil do
      begin
        URI := PropNode.namespaceURI;
        if (URI <> '') and (PropNode.nodeType = ELEMENT_NODE) then
          Schemas[URI].LoadProperty(PropNode);
        PropNode := PropNode.nextSibling;
      end;
    end;
    SchemaNode := SchemaNode.nextSibling;
  end;
  Changed(False);
  Result := True;
end;

procedure TXMPPacket.RemoveProperty(SchemaKind: TXMPKnownNamespace;
  const PropName: UnicodeString);
var
  Schema: TXMPSchema;
begin
  if FindSchema(SchemaKind, Schema) then Schema.RemoveProperty(PropName);
end;

procedure TXMPPacket.RemoveProperties(SchemaKind: TXMPKnownNamespace;
  const PropNames: array of UnicodeString);
var
  Schema: TXMPSchema;
begin
  if FindSchema(SchemaKind, Schema) then Schema.RemoveProperties(PropNames);
end;

procedure TXMPPacket.DoUpdateProperty(Policy: TXMPWritePolicy; SchemaKind: TXMPKnownNamespace;
  const PropName: UnicodeString; PropKind: TXMPPropertyKind; const NewValue: UnicodeString);
var
  Prop: TXMPProperty;
  Schema: TXMPSchema;
begin
  if (Policy = xwRemove) or (NewValue = '') then
  begin
    RemoveProperty(SchemaKind, PropName);
    Exit;
  end;
  if Policy = xwAlwaysUpdate then
    Schema := FindOrAddSchema(SchemaKind)
  else
    if not FindSchema(SchemaKind, Schema) then Exit;
  if not Schema.FindProperty(PropName, Prop) then
    if Policy = xwAlwaysUpdate then
      Prop := Schema.AddProperty(PropName)
    else
      Exit;
  Prop.Kind := PropKind;
  Prop.WriteValue(TrimRight(NewValue));
end;

procedure TXMPPacket.DoUpdateArrayProperty(SchemaKind: TXMPKnownNamespace;
  const PropName: UnicodeString; ArrayPropKind: TXMPPropertyKind;
  const NewValues: array of UnicodeString);
var
  I, NewSubPropCount: Integer;
  Prop: TXMPProperty;
  Schema: TXMPSchema;
begin
  NewSubPropCount := Length(NewValues);
  if (UpdatePolicy = xwRemove) or (NewSubPropCount = 0) then
  begin
    RemoveProperty(SchemaKind, PropName);
    Exit;
  end;                                   
  if UpdatePolicy = xwAlwaysUpdate then
    Schema := FindOrAddSchema(SchemaKind)
  else
    if not FindSchema(SchemaKind, Schema) then Exit;
  if not Schema.FindProperty(PropName, Prop) then
    if UpdatePolicy = xwAlwaysUpdate then
      Prop := Schema.AddProperty(PropName)
    else
      Exit;
  if not (Prop.Kind in [xpSimple, xpBagArray, xpSeqArray]) then
  begin
    RemoveProperty(SchemaKind, PropName);
    Prop := Schema.AddProperty(PropName);
  end;
  Prop.Kind := ArrayPropKind;
  Prop.SubPropertyCount := NewSubPropCount;
  for I := 0 to NewSubPropCount - 1 do
    Prop[I].WriteValue(NewValues[I]);
end;

procedure TXMPPacket.UpdateProperty(SchemaKind: TXMPKnownNamespace;
  const PropName: UnicodeString; PropKind: TXMPPropertyKind; const NewValue: UnicodeString);
var
  SubPolicy: TXMPWritePolicy;
begin
  DoUpdateProperty(UpdatePolicy, SchemaKind, PropName, PropKind, NewValue);
  if (SchemaKind <> xsTIFF) or (PropKind <> xpSimple) or
     (PropName[1] <> UpCase(PropName[1])) then Exit;
  { Special handling for the TIFF schema - basically, the spec has its properties all
    with an initial capital, but Vista (perhaps for bkwards compat reasons) can write
    to both this *and* an all lowercase version. }
  SubPolicy := UpdatePolicy;
  if SubPolicy = xwAlwaysUpdate then SubPolicy := xwUpdateIfExists;
  DoUpdateProperty(SubPolicy, SchemaKind, LowerCase(PropName), PropKind, NewValue);
end;

procedure TXMPPacket.UpdateBagProperty(SchemaKind: TXMPKnownNamespace;
  const PropName: UnicodeString; const NewValues: array of UnicodeString);
begin
  DoUpdateArrayProperty(SchemaKind, PropName, xpBagArray, NewValues);
end;

procedure TXMPPacket.UpdateBagProperty(SchemaKind: TXMPKnownNamespace;
  const PropName: UnicodeString; const NewValueList: UnicodeString);
begin
  UpdateProperty(SchemaKind, PropName, xpBagArray, NewValueList);
end;

procedure TXMPPacket.UpdateSeqProperty(SchemaKind: TXMPKnownNamespace;
  const PropName: UnicodeString; const NewValues: array of UnicodeString);
begin
  DoUpdateArrayProperty(SchemaKind, PropName, xpSeqArray, NewValues);
end;

procedure TXMPPacket.UpdateSeqProperty(SchemaKind: TXMPKnownNamespace;
  const PropName: UnicodeString; const NewValueList: UnicodeString);
begin
  UpdateProperty(SchemaKind, PropName, xpSeqArray, NewValueList);
end;

procedure TXMPPacket.UpdateProperty(SchemaKind: TXMPKnownNamespace;
  const PropName, NewValue: UnicodeString);
begin
  UpdateProperty(SchemaKind, PropName, xpSimple, NewValue);
end;

procedure TXMPPacket.UpdateProperty(SchemaKind: TXMPKnownNamespace;
  const PropName: UnicodeString; const NewValue: Integer);
begin
  UpdateProperty(SchemaKind, PropName, xpSimple, IntToStr(NewValue));
end;

procedure TXMPPacket.UpdateDateTimeProperty(SchemaKind: TXMPKnownNamespace;
  const PropName: UnicodeString; const NewValue: TDateTimeTagValue;
  ApplyLocalBias: Boolean);
var
  S: UnicodeString;
begin
  if NewValue.MissingOrInvalid then
    RemoveProperty(SchemaKind, PropName)
  else
  begin
    S := DateTimeToXMPString(NewValue, ApplyLocalBias);
    UpdateProperty(SchemaKind, PropName, xpSimple, S);
  end;
end;

function TXMPPacket.ReadBool(SchemaKind: TXMPKnownNamespace; const PropertyName: string; DefValue: Boolean): Boolean;
var
  Prop: TXMPProperty;
  Schema: TXMPSchema;
begin
  if FindSchema(SchemaKind, Schema) and Schema.FindProperty(PropertyName, Prop) then
    Result := Prop.ReadValue(DefValue)
  else
    Result := DefValue;
end;

function TXMPPacket.ReadBool(const SchemaURI: string; const PropertyName: string; DefValue: Boolean): Boolean;
var
  Prop: TXMPProperty;
  Schema: TXMPSchema;
begin
  if FindSchema(SchemaURI, Schema) and Schema.FindProperty(PropertyName, Prop) then
    Result := Prop.ReadValue(DefValue)
  else
    Result := DefValue;
end;

function TXMPPacket.ReadDateTime(SchemaKind: TXMPKnownNamespace; const PropertyName: string; const DefValue: TDateTime): TDateTime;
var
  Prop: TXMPProperty;
  Schema: TXMPSchema;
begin
  if FindSchema(SchemaKind, Schema) and Schema.FindProperty(PropertyName, Prop) then
    Result := Prop.ReadValue(DefValue)
  else
    Result := DefValue;
end;

function TXMPPacket.ReadDateTime(const SchemaURI: string; const PropertyName: string; const DefValue: TDateTime): TDateTime;
var
  Prop: TXMPProperty;
  Schema: TXMPSchema;
begin
  if FindSchema(SchemaURI, Schema) and Schema.FindProperty(PropertyName, Prop) then
    Result := Prop.ReadValue(DefValue)
  else
    Result := DefValue;
end;

function TXMPPacket.ReadInteger(SchemaKind: TXMPKnownNamespace; const PropertyName: string; DefValue: Integer): Integer;
var
  Prop: TXMPProperty;
  Schema: TXMPSchema;
begin
  if FindSchema(SchemaKind, Schema) and Schema.FindProperty(PropertyName, Prop) then
    Result := Prop.ReadValue(DefValue)
  else
    Result := DefValue;
end;

function TXMPPacket.ReadInteger(const SchemaURI: string; const PropertyName: string; DefValue: Integer): Integer;
var
  Prop: TXMPProperty;
  Schema: TXMPSchema;
begin
  if FindSchema(SchemaURI, Schema) and Schema.FindProperty(PropertyName, Prop) then
    Result := Prop.ReadValue(DefValue)
  else
    Result := DefValue;
end;

function TXMPPacket.ReadString(SchemaKind: TXMPKnownNamespace; const PropertyName: string; const DefValue: string): string;
var
  Prop: TXMPProperty;
  Schema: TXMPSchema;
begin
  if FindSchema(SchemaKind, Schema) and Schema.FindProperty(PropertyName, Prop) then
    Result := Prop.ReadValue(DefValue)
  else
    Result := DefValue;
end;

function TXMPPacket.ReadString(const SchemaURI: string; const PropertyName: string; const DefValue: string): string;
var
  Prop: TXMPProperty;
  Schema: TXMPSchema;
begin
  if FindSchema(SchemaURI, Schema) and Schema.FindProperty(PropertyName, Prop) then
    Result := Prop.ReadValue(DefValue)
  else
    Result := DefValue;
end;

procedure TXMPPacket.WriteBool(SchemaKind: TXMPKnownNamespace; const PropertyName: string; Value: Boolean);
begin
  Schemas[SchemaKind].Properties[PropertyName].WriteValue(Value);
end;

procedure TXMPPacket.WriteBool(const SchemaURI: string; const PropertyName: string; Value: Boolean);
begin
  Schemas[SchemaURI].Properties[PropertyName].WriteValue(Value);
end;

procedure TXMPPacket.WriteDateTime(SchemaKind: TXMPKnownNamespace; const PropertyName: string; const Value: TDateTime);
begin
  Schemas[SchemaKind].Properties[PropertyName].WriteValue(Value);
end;

procedure TXMPPacket.WriteDateTime(const SchemaURI: string; const PropertyName: string; const Value: TDateTime);
begin
  Schemas[SchemaURI].Properties[PropertyName].WriteValue(Value);
end;

procedure TXMPPacket.WriteInteger(SchemaKind: TXMPKnownNamespace; const PropertyName: string; Value: Integer);
begin
  Schemas[SchemaKind].Properties[PropertyName].WriteValue(Value);
end;

procedure TXMPPacket.WriteInteger(const SchemaURI: string; const PropertyName: string; Value: Integer);
begin
  Schemas[SchemaURI].Properties[PropertyName].WriteValue(Value);
end;

procedure TXMPPacket.WriteString(SchemaKind: TXMPKnownNamespace; const PropertyName: string; const Value: string);
begin
  Schemas[SchemaKind].Properties[PropertyName].WriteValue(Value);
end;

procedure TXMPPacket.WriteString(const SchemaURI: string; const PropertyName: string; const Value: string);
begin
  Schemas[SchemaURI].Properties[PropertyName].WriteValue(Value);
end;

{ TXMPPacket.TEnumerator }

constructor TXMPPacket.TEnumerator.Create(Packet: TXMPPacket);
begin
  FPacket := Packet;
  FIndex := -1;
end;

function TXMPPacket.TEnumerator.GetCurrent: TXMPSchema;
begin
  Result := FPacket[FIndex];
end;

function TXMPPacket.TEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := (FIndex < FPacket.SchemaCount);
end;

initialization
{$IFDEF MSWINDOWS}
  if IsConsole and Assigned(InitProc) then
  begin                   //TXMPPacket implicitly uses MSXML, which requires CoInitialize
    TProcedure(InitProc); //or CoInitializeEx to be called. This will be done
    InitProc := nil;      //automatically with a VCL app (the RTL's MSXML wrapper uses
  end;                    //ComObj.pas, which assigns InitProc, which is called by
{$ENDIF}                  //Application.Initialize), but not in a console one.
end.
