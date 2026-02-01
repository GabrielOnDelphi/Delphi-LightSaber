unit HtmlParserEx;
{$DEFINE UseXPath }
{$IF RTLVersion < 24.0}
{$MESSAGE ERROR 'Only XE3 and later versions are supported'}
{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections
  {$IFDEF UseXPath}
  ,System.RegularExpressionsCore
  {$ENDIF};

{$IF (defined(IOS) and defined(CPUARM)) or defined(ANDROID)}
{$DEFINE MOBILE_DEV}
{$ENDIF}
const
  // By design each IHtmlElement has a TagName property, even it's a 'Text' element.
  // so cSpecialTagName_Text represents the fake tag name of a Text element
  cSpecialTagName_Text = '#TEXT';
  LowStrIndex = low(string); // Mobile platform=0, PC platform=1

type
  IHtmlElement = interface;
  IHtmlElementList = interface;
  TElementEachEvent = reference to procedure(AIndex:Integer; AEl:IHtmlElement);


  IHtmlElement = interface
    ['{8C75239C-8CFA-499F-B115-7CEBEDFB421B}']
    function GetParent:IHtmlElement; stdcall;
    function GetTagName:String; stdcall;
    procedure SetTagName(Value:String); stdcall;
    function GetContent:String; stdcall;
    function GetOrignal:String; stdcall;
    function GetChildrenCount:Integer; stdcall;
    function GetChildren(Index:Integer):IHtmlElement; stdcall;
    function GetCloseTag:IHtmlElement; stdcall;
    function GetInnerHtml():String; stdcall;
    function GetOuterHtml():String; stdcall;
    function GetInnerText():String; stdcall;
    procedure SetInnerText(Value:String); stdcall;
    function GetAttributes(Key:String):String; stdcall;
    procedure SetAttributes(Key:String; Value:String); stdcall;
    procedure RemoveAttr(AAttrName:string); stdcall;
    function GetSourceLineNum():Integer; stdcall;
    function GetSourceColNum():Integer; stdcall;
    // Add and remove nodes
    function RemoveChild(ANode:IHtmlElement):Integer; stdcall;
    procedure Remove; stdcall;
    function AppedChild(const ATag:string):IHtmlElement; stdcall;
    // Does the property exist
    function HasAttribute(AttributeName:String):Boolean; stdcall;
    { Find Element using CSS selector syntax, "pseudo-class" is not supported
      http://www.w3.org/TR/CSS2/selector.html
    }
    function SimpleCSSSelector(const selector:String):IHtmlElementList; stdcall;
    function Find(const selector:String):IHtmlElementList; stdcall;
    {$IFDEF UseXPath}
    function FindX(const AXPath:String):IHtmlElementList; stdcall;
    {$ENDIF}
    // enum property
    function EnumAttributeNames(Index:Integer):String; stdcall;
    property TagName:String read GetTagName write SetTagName;
    property ChildrenCount:Integer read GetChildrenCount;
    property Children[index:Integer]:IHtmlElement read GetChildren; default;
    property CloseTag:IHtmlElement read GetCloseTag;
    property Content:String read GetContent;
    property Orignal:String read GetOrignal;
    property Parent:IHtmlElement read GetParent;
    // Get the position of an element in the source code
    property SourceLineNum:Integer read GetSourceLineNum;
    property SourceColNum:Integer read GetSourceColNum;
    //
    property InnerHtml:String read GetInnerHtml;
    property OuterHtml:String read GetOuterHtml;
    property InnerText:String read GetInnerText write SetInnerText;
    property Text:String read GetInnerText write SetInnerText;
    property Attributes[Key:String]:String read GetAttributes write SetAttributes;
    // ying32 does not change the original, just simplifies the use
    property Attrs[Key:String]:String read GetAttributes write SetAttributes;
  end;


  THtmlListEnumerator = class
  private
    FIndex:Integer;
    FList:IHtmlElementList;
  public
    constructor Create(AList:IHtmlElementList);
    function GetCurrent:IHtmlElement; inline;
    function MoveNext:Boolean;
    property Current:IHtmlElement read GetCurrent;
  end;


  IHtmlElementList = interface
    ['{8E1380C6-4263-4BF6-8D10-091A86D8E7D9}']
    function GetCount:Integer; stdcall;
    function GetItems(Index:Integer):IHtmlElement; stdcall;
    procedure RemoveAll; stdcall;
    procedure Remove(ANode:IHtmlElement); stdcall;
    procedure Each(f:TElementEachEvent); stdcall;
    function GetText:String; stdcall;
    function GetEnumerator:THtmlListEnumerator;
    property Text:String read GetText;
    property Count:Integer read GetCount;
    property Items[index:Integer]:IHtmlElement read GetItems; default;
  end;


function ParserHTML(const Source:String):IHtmlElement; stdcall;
function DecodeHtmlEntities(S:string):string; forward;

implementation

uses
  System.StrUtils;

type
  TStringDictionary = TDictionary<string, string>;
  TPropDictionary = TDictionary<string, WORD>;
  TStringDynArray = TArray<string>;
const
  WhiteSpace = [' ', #13, #10, #9];
  // CSS Attribute Compare Operator
  OperatorChar = ['=', '!', '*', '~', '|', '^', '$'];
  MaxListSize = Maxint div 16;
  // TagProperty
  tpBlock = $01;
  tpInline = $02;
  tpEmpty = $04;
  tpFormatAsInline = $08;
  tpPreserveWhitespace = $10;
  tpInlineOrEmpty = tpInline or tpEmpty;

type
  TAttrOperator = (aoExist, aoEqual, aoNotEqual, aoIncludeWord, aoBeginWord, aoBegin, aoEnd, aoContain);
  PAttrSelectorItem = ^TAttrSelectorItem;


  TAttrSelectorItem = record
    Key:string;
    AttrOperator:TAttrOperator;
    Value:string;
  end;


  TSelectorItemRelation = (sirNONE, sirDescendant, sirChildren, sirYoungerBrother, sirAllYoungerBrother);
  PCSSSelectorItem = ^TCSSSelectorItem;


  TCSSSelectorItem = record
    Relation:TSelectorItemRelation;
    szTag:string;
    Attributes: array of TAttrSelectorItem;
  end;


  TCSSSelectorItems = array of TCSSSelectorItem;
  PCSSSelectorItems = ^TCSSSelectorItems;
  TCSSSelectorItemGroup = array of TCSSSelectorItems;


  //
  TSourceContext = record
  private
    function GetCharOfCurrent(Index:Integer):Char; inline;
    function PassedEndOfSourceCode:Boolean; inline;
  public
    SourceCode:string;
    CharIndex:Integer;
    LineNum:Integer;
    ColNum:Integer;
    CurrentChar:Char;
    {$IFDEF DEBUG}
    currentCode:PChar;
    {$ENDIF}
    // Jump to the next char and return true if not exceeding the end of SourceCode
    function JumpToNextChar:Boolean; overload; inline;
    // Jump multiple characters, and return the jumped steps
    function JumpToNextChar(Step:Integer):Integer; overload; inline;
    procedure setCode(const ACode:string); inline;
    function ReadStr(UntilChars:TSysCharSet):string; inline;
    function PeekStr(Index:Integer):string; overload; inline;
    function PeekStr():string; overload; inline;
    function subStr(Index, Count:Integer):string; overload; inline;
    function subStr(Count:Integer):string; overload; inline;
    procedure SkipBlank(); inline;
    property charOfCurrent[index:Integer]:Char read GetCharOfCurrent;
  end;


  TAttributeItem = record
    Key, Value:string;
  end;


  TAttributeDynArray = TArray<TAttributeItem>;
  TIHtmlElementList = class;
  THtmlElement = class;
  THtmlElementList = TList<THtmlElement>;


  THtmlElement = class(TInterfacedObject, IHtmlElement)
  private
    function GetChildrens:IHtmlElementList;
  protected
    // ying32
    function GetParent:IHtmlElement; stdcall;
    function GetTagName:String; stdcall;
    procedure SetTagName(Value:String); stdcall;
    function GetContent:String; stdcall;
    function GetOrignal:String; stdcall;
    function GetChildrenCount:Integer; stdcall;
    function GetChildren(Index:Integer):IHtmlElement; stdcall;
    function GetCloseTag:IHtmlElement; stdcall;
    function GetInnerHtml():String; stdcall;
    function GetOuterHtml():String; stdcall;
    function GetInnerText():String; stdcall;
    procedure SetInnerText(Value:String); stdcall;
    function GetAttributes(Key:String):String; stdcall;
    procedure SetAttributes(Key:String; Value:String); stdcall;
    procedure RemoveAttr(AAttrName:string); stdcall;
    function GetSourceLineNum():Integer; stdcall;
    function GetSourceColNum():Integer; stdcall;
    // ying32Added
    function RemoveChild(ANode:IHtmlElement):Integer; stdcall;
    procedure Remove; stdcall;
    function AppedChild(const ATag:string):IHtmlElement; stdcall;

    // Does the property exist
    function HasAttribute(AttributeName:String):Boolean; stdcall;
    { Find Element with CSS selector syntax, does not support "pseudo-class"
      http://www.w3.org/TR/CSS2/selector.html
    }
    function SimpleCSSSelector(const selector:String):IHtmlElementList; stdcall;
    function Find(const selector:String):IHtmlElementList; stdcall;
    {$IFDEF UseXPath}
    function FindX(const AXPath:String):IHtmlElementList; stdcall;
    {$ENDIF}
    // enum property
    function EnumAttributeNames(Index:Integer):String; stdcall;
    property TagName:String read GetTagName write SetTagName;
    property ChildrenCount:Integer read GetChildrenCount;
    property Children[index:Integer]:IHtmlElement read GetChildren; default;
    property CloseTag:IHtmlElement read GetCloseTag;
    property Content:String read GetContent;
    property Orignal:String read GetOrignal;
    property Parent:IHtmlElement read GetParent;
    // Get the position of an element in the source code
    property SourceLineNum:Integer read GetSourceLineNum;
    property SourceColNum:Integer read GetSourceColNum;
    //
    property InnerHtml:String read GetInnerHtml;
    property OuterHtml:String read GetOuterHtml;
    property InnerText:String read GetInnerText;
    property Attributes[Key:String]:String read GetAttributes write SetAttributes;
    property Childrens:IHtmlElementList read GetChildrens;
  private
    FClosed:Boolean;
    //
    FOwner:THtmlElement;
    FCloseTag:IHtmlElement;
    FTagName:string;
    FIsCloseTag:Boolean;
    FContent:string;
    FOrignal:string;
    FSourceLine:Integer;
    FSourceCol:Integer;
    //
    FAttributes:TStringDictionary;
    FChildren:TIHtmlElementList;
    procedure _GetHtml(IncludeSelf:Boolean; Sb:TStringBuilder);
    procedure _GetText(IncludeSelf:Boolean; Sb:TStringBuilder);
    procedure _SimpleCSSSelector(const ItemGroup:TCSSSelectorItemGroup; r:TIHtmlElementList);
    procedure _Select(Item:PCSSSelectorItem; Count:Integer; r:TIHtmlElementList; OnlyTopLevel:Boolean = false);
  public
    constructor Create(AOwner:THtmlElement; AText:string; ALine, ACol:Integer);
    destructor Destroy; override;
  end;


  TIHtmlElementList = class(TInterfacedObject, IHtmlElementList)
  private
    // IHtmlElementList
    function GetItems(Index:Integer):IHtmlElement; stdcall;
    function GetCount:Integer; stdcall;
  protected
    FList:TList<IHtmlElement>;
    procedure SetItems(Index:Integer; const Value:IHtmlElement); inline;
    function Add(Value:IHtmlElement):Integer; inline;
    procedure Delete(Index:Integer); inline;
    procedure Clear; inline;
    // ying32Added
    procedure RemoveAll; stdcall;
    procedure Remove(ANode:IHtmlElement); stdcall;
    procedure Each(f:TElementEachEvent); stdcall;
    function GetText:String; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
    function GetEnumerator:THtmlListEnumerator;
    function IndexOf(Item:IHtmlElement):Integer;
    // IHtmlElementList
    property Items[index:Integer]:IHtmlElement read GetItems write SetItems; default;
    property Count:Integer read GetCount;
  end;


function SplitStr(ACharSet:TSysCharSet; AStr:string):TStringDynArray;
var
  L, I:Integer;
  S:string;
  StrChar:Char;
begin
  Result := nil;
  if Length(AStr) <= 0 then
    Exit;
  I := low(AStr);
  L := low(AStr);
  StrChar := #0;
  while I <= high(AStr) do
  begin
    if CharInSet(AStr[I], ['''', '"']) then
      if StrChar = #0 then
        StrChar := AStr[I]
      else if StrChar = AStr[I] then
        StrChar := #0;
    // Not in the string, the delimiter takes effect
    if StrChar = #0 then
      if CharInSet(AStr[I], ACharSet) then
      begin
        if I > L then
        begin
          S := Copy(AStr, L{$IF (LowStrIndex = 0)} + 1{$ENDIF}, I - L);
          SetLength(Result, Length(Result) + 1);
          Result[Length(Result) - 1] := S;
        end;
        L := I + 1;
      end;
    Inc(I);
  end;
  if (I > L) then
  begin
    S := Copy(AStr, L{$IF (LowStrIndex = 0)} + 1{$ENDIF}, I - L);
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := S;
  end;
end;


function StrRight(const Value:string; Count:Integer):string;
var
  start:Integer;
begin
  start := Length(Value) - Count + 1;
  if start <= 0 then
    Result := Value
  else
    Result := Copy(Value, start, Count);
end;


function StrLeft(const Value:string; Count:Integer):string;
begin
  Result := Copy(Value, LowStrIndex, Count);
end;


// ComapreAttr
function _aoExist(const Item:TAttrSelectorItem; E:THtmlElement):Boolean;
begin
  Result := E.FAttributes.ContainsKey(Item.Key);
end;


function _aoEqual(const Item:TAttrSelectorItem; E:THtmlElement):Boolean;
begin
  Result := E.FAttributes.ContainsKey(Item.Key) and (E.FAttributes[Item.Key] = Item.Value);
end;


function _aoNotEqual(const Item:TAttrSelectorItem; E:THtmlElement):Boolean;
begin
  Result := E.FAttributes.ContainsKey(Item.Key) and (E.FAttributes[Item.Key] <> Item.Value);
end;


function _aoIncludeWord(const Item:TAttrSelectorItem; E:THtmlElement):Boolean;
var
  S:TStringDynArray;
  I:Integer;
begin
  Result := false;
  if not E.FAttributes.ContainsKey(Item.Key) then
    Exit;
  Result := True;
  S := SplitStr(WhiteSpace, E.FAttributes[Item.Key]);
  for I := low(S) to high(S) do
    if S[I] = Item.Value then
      Exit;
  Result := false;
end;


function _aoBeginWord(const Item:TAttrSelectorItem; E:THtmlElement):Boolean;
var
  S:TStringDynArray;
begin
  Result := false;
  if not E.FAttributes.ContainsKey(Item.Key) then
    Exit;
  S := SplitStr((WhiteSpace + ['_', '-']), E.FAttributes[Item.Key]);
  Result := (Length(S) > 0) and (S[0] = Item.Value);
end;


function _aoBegin(const Item:TAttrSelectorItem; E:THtmlElement):Boolean;
var
  attr, Value:string;
begin
  Result := false;
  if not E.FAttributes.ContainsKey(Item.Key) then
    Exit;
  attr := E.FAttributes[Item.Key];
  Value := Item.Value;
  Result := (Length(attr) > Length(Value)) and (StrLeft(attr, Length(Value)) = Value);
end;


function _aoEnd(const Item:TAttrSelectorItem; E:THtmlElement):Boolean;
var
  attr, Value:string;
begin
  Result := false;
  if not E.FAttributes.ContainsKey(Item.Key) then
    Exit;
  attr := E.FAttributes[Item.Key];
  Value := Item.Value;
  Result := (Length(attr) > Length(Value)) and (StrRight(attr, Length(Value)) = Value);
end;


function _aoContain(const Item:TAttrSelectorItem; E:THtmlElement):Boolean;
begin
  Result := false;
  if not E.FAttributes.ContainsKey(Item.Key) then
    Exit;
  Result := Pos(Item.Value, E.FAttributes[Item.Key]) > 0;
end;

type
  TFNCompareAttr = function(const Item:TAttrSelectorItem; E:THtmlElement):Boolean;
const
  AttrCompareFuns: array [TAttrOperator] of TFNCompareAttr = (_aoExist, _aoEqual, _aoNotEqual, _aoIncludeWord, _aoBeginWord, _aoBegin, _aoEnd, _aoContain);
function GetTagProperty(const TagName:string):WORD; forward;


procedure DoError(const Msg:string);
begin
  raise Exception.Create(Msg);
end;


procedure _ParserAttrs(var sc:TSourceContext; var Attrs:TAttributeDynArray);
var
  Item:TAttributeItem;
begin
  SetLength(Attrs, 0);
  while True do
  begin
    sc.SkipBlank();
    if sc.CurrentChar = #0 then
      Break;
    Item.Key := sc.ReadStr((WhiteSpace + [#0, '=']));
    Item.Value := '';
    sc.SkipBlank;
    if sc.CurrentChar = '=' then
    begin
      sc.JumpToNextChar;
      sc.SkipBlank;
      Item.Value := sc.ReadStr((WhiteSpace + [#0]));
    end;
    SetLength(Attrs, Length(Attrs) + 1);
    Attrs[Length(Attrs) - 1] := Item;
  end;
end;


procedure _ParserNodeItem(S:string; var ATagName:string; var Attrs:TAttributeDynArray);
var
  sc:TSourceContext;
begin
  sc.setCode(S);
  sc.SkipBlank;
  ATagName := UpperCase(sc.ReadStr((WhiteSpace + [#0, '/', '>'])));
  _ParserAttrs(sc, Attrs);
end;


function CreateTextElement(AOwner:THtmlElement; AText:string; ALine, ACol:Integer):THtmlElement;
begin
  Result := THtmlElement.Create(AOwner, AText, ALine, ACol);
  with Result do
  begin
    // Edwin: 2019-07-08: html entities in the text element shouldn't be decoded?
    // FContent := DecodeHtmlEntities(AText);
    FContent := AText;
    FTagName := cSpecialTagName_Text;
    FClosed := True;
  end;
end;


function CreateScriptElement(AOwner:THtmlElement; AText:string; ALine, ACol:Integer):THtmlElement;
begin
  Result := THtmlElement.Create(AOwner, AText, ALine, ACol);
  with Result do
  begin
    // Edwin: 2019-07-09: html entities in scripts shouldn't be decoded?
    // FContent := DecodeHtmlEntities(AText);
    FContent := AText;
    FTagName := '#SCRIPT';
    FClosed := True;
  end;
end;


function CreateStyleElement(AOwner:THtmlElement; AText:string; ALine, ACol:Integer):THtmlElement;
begin
  Result := THtmlElement.Create(AOwner, AText, ALine, ACol);
  with Result do
  begin
    // Edwin: 2019-07-09: html entities in <style> tags shouldn't be decoded?
    // FContent := DecodeHtmlEntities(AText);
    FContent := AText;
    FTagName := '#STYLE';
    FClosed := True;
  end;
end;


function CreateCommentElement(AOwner:THtmlElement; AText:string; ALine, ACol:Integer):THtmlElement;
begin
  Result := THtmlElement.Create(AOwner, AText, ALine, ACol);
  with Result do
  begin
    // Edwin: 2019-07-09: html entities in comments shouldn't be decoded?
    // FContent := DecodeHtmlEntities(AText);
    FContent := AText;
    FTagName := '#COMMENT';
    FClosed := True;
  end;
end;


function CreateTagElement(AOwner:THtmlElement; AText:string; ALine, ACol:Integer):THtmlElement;
var
  I:Integer;
  Attrs:TAttributeDynArray;
begin
  Result := THtmlElement.Create(AOwner, AText, ALine, ACol);
  with Result do
  begin
    // TODO parsing TagName and attributes
    if AText = '' then
      Exit;
    // remove the <
    if AText[low(AText)] = '<' then
      AText := StrRight(AText, Length(AText) - 1);
    if AText = '' then
      Exit;
    if AText[high(AText)] = '>' then
      AText := StrLeft(AText, Length(AText) - 1);
    // Check if a node is down, or a single down node
    if AText = '' then
      Exit;
    FClosed := AText[high(AText)] = '/';
    FIsCloseTag := AText[LowStrIndex] = '/';
    if FIsCloseTag then
      AText := StrRight(AText, Length(AText) - 1);
    if FClosed then
      AText := StrLeft(AText, Length(AText) - 1);
    //
    _ParserNodeItem(AText, FTagName, Attrs);
    for I := low(Attrs) to high(Attrs) do
      FAttributes.AddOrSetValue(LowerCase(Attrs[I].Key), DecodeHtmlEntities(Attrs[I].Value));
  end;
end;


function CreateDocTypeElement(AOwner:THtmlElement; AText:string; ALine, ACol:Integer):THtmlElement;
begin
  Result := THtmlElement.Create(AOwner, AText, ALine, ACol);
  with Result do
  begin
    FContent := DecodeHtmlEntities(AText);
    FTagName := '#DOCTYPE';
    FClosed := True;
    if FContent = '' then
      Exit;
    if FContent[1] = '<' then
      Delete(FContent, 1, 1);
    if FContent = '' then
      Exit;
    if FContent[Length(FContent)] = '>' then
      Delete(FContent, Length(FContent), 1);
    FContent := Trim(Copy(Trim(FContent), 9, Length(FContent)));
  end;
end;


procedure _ParserHTML(const Source:string; AElementList:THtmlElementList);
var
  BeginLineNum, BeginColNum:Integer;
  sc:TSourceContext;
  function IsEndOfTag(TagName:string):Boolean;
  begin
    Result := false;
    if sc.charOfCurrent[1] = '/' then
    begin
      Result := UpperCase(sc.subStr(sc.CharIndex + 2, Length(TagName))) = UpperCase(TagName);
    end;
  end;
  function PosCharInTag(AChar:Char):Boolean;
  var
    StrChar:Char;
  begin
    Result := false;
    StrChar := #0;
    while True do
    begin
      if sc.CurrentChar = #0 then
        Break;
      if sc.CurrentChar = '"' then
      begin
        if StrChar = #0 then
          StrChar := sc.CurrentChar
        else
          StrChar := #0;
      end;
      if (sc.CurrentChar = AChar) and (StrChar = #0) then
      begin
        Result := True;
        Break;
      end;
      sc.JumpToNextChar;
    end;
  end;
  function ParserStyleData():string;
  var
    oldIndex:Integer;
  begin
    oldIndex := sc.CharIndex;
    if sc.subStr(4) = '<!--' then
    begin
      sc.JumpToNextChar(5);
      while True do
      begin
        if sc.CurrentChar = #0 then
          DoError(Format('Unfinished Style row: %d; Column: %d;', [sc.LineNum, sc.ColNum]))
        else if sc.CurrentChar = '>' then
        begin
          if (sc.charOfCurrent[ -1] = '-') and (sc.charOfCurrent[ -2] = '-') then
          begin
            sc.JumpToNextChar;
            sc.SkipBlank();
            Break;
          end;
        end;
        sc.JumpToNextChar;
      end;
    end
    else
      while True do
      begin
        case sc.CurrentChar of
          #0:
            begin
              Break;
            end;
          '<':
            begin
              if IsEndOfTag('style') then
              begin
                Break;
              end;
            end;
        end;
        sc.JumpToNextChar;
      end;
    Result := sc.subStr(oldIndex, sc.CharIndex - oldIndex);
  end;
  function ParserScriptData():string;
  var
    oldIndex:Integer;
    stringChar:Char;
    PreIsblique:Boolean;
  begin
    oldIndex := sc.CharIndex;
    sc.SkipBlank();
    if sc.subStr(4) = '<!--' then
    begin
      sc.JumpToNextChar(5);
      while True do
      begin
        if sc.CurrentChar = #0 then
          DoError(Format('Unfinished script line: %d; column: %d;', [sc.LineNum, sc.ColNum]))
        else if sc.CurrentChar = '>' then
        begin
          if (sc.charOfCurrent[ -1] = '-') and (sc.charOfCurrent[ -2] = '-') then
          begin
            sc.JumpToNextChar;
            sc.SkipBlank();
            Break;
          end;
        end;
        sc.JumpToNextChar;
      end;
    end
    else
    begin
      while True do
      begin
        case sc.CurrentChar of
          #0:
            Break;
          '"', '''': // string
            begin
              stringChar := sc.CurrentChar;
              PreIsblique := false;
              sc.JumpToNextChar();
              while True do
              begin
                if sc.CurrentChar = #0 then
                  Break;
                if (sc.CurrentChar = stringChar) and (not PreIsblique) then
                  Break;
                if sc.CurrentChar = '\' then
                  PreIsblique := not PreIsblique
                else
                  PreIsblique := false;
                sc.JumpToNextChar;
              end;
            end;
          '/': // Comments
            begin
              sc.JumpToNextChar();
              case sc.CurrentChar of
                '/': // line comment
                  begin
                    while True do
                    begin
                      if CharInSet(sc.CurrentChar, [#0, #$0A]) then
                      begin
                        Break;
                      end;
                      sc.JumpToNextChar();
                    end;
                  end;
                '*': // block comment
                  begin
                    sc.JumpToNextChar();
                    sc.JumpToNextChar();
                    while True do
                    begin
                      if sc.CurrentChar = #0 then
                        Break;
                      if (sc.CurrentChar = '/') and (sc.charOfCurrent[ -1] = '*') then
                      begin
                        Break;
                      end;
                      sc.JumpToNextChar();
                    end;
                  end;
              end;
            end;
          '<':
            begin
              if IsEndOfTag('script') then
              begin
                Break;
              end;
            end;
        end;
        sc.JumpToNextChar();
      end;
    end;
    Result := sc.subStr(oldIndex, sc.CharIndex - oldIndex)
  end;


var
  ElementType:(EtUnknow, EtTag, EtDocType, EtText, EtComment);
  OldCodeIndex:Integer;
  tmp:string;
  Tag:THtmlElement;
begin
  sc.setCode(Source);
  while sc.CharIndex <= high(sc.SourceCode) do
  begin
    OldCodeIndex := sc.CharIndex;
    BeginLineNum := sc.LineNum;
    BeginColNum := sc.ColNum;
    if sc.CurrentChar = #0 then
      Break;
    // "<" It starts with something like Tag
    if sc.CurrentChar = '<' then
    begin
      sc.JumpToNextChar;
      if sc.CurrentChar = '!' then // Comments
      begin
        ElementType := EtComment;
        sc.JumpToNextChar;
        case sc.CurrentChar of
          '-': // <!--  -->
            begin
              sc.JumpToNextChar; // -
              while True do
              begin
                if not PosCharInTag('>') then
                  DoError('LineNum:' + IntToStr(BeginLineNum) + 'Unable to find tag end point:' + sc.subStr(100))
                else if (sc.charOfCurrent[ -1] = '-') and (sc.charOfCurrent[ -2] = '-') then
                begin
                  sc.JumpToNextChar;
                  Break;
                end;
                sc.JumpToNextChar;
              end;
            end;
          '[': // <![CDATA[.....]]>
            begin
              sc.JumpToNextChar; //
              while True do
              begin
                if not PosCharInTag('>') then
                  DoError('LineNum:' + IntToStr(BeginLineNum) + 'Unable to find tag end point:' + sc.subStr(100))
                else if (sc.charOfCurrent[ -1] = ']') then
                begin
                  sc.JumpToNextChar;
                  Break;
                end;
                sc.JumpToNextChar;
              end;
            end;
        else // <!.....>
          begin
            if UpperCase(sc.PeekStr()) = 'DOCTYPE' then
            begin
              ElementType := EtDocType;
              sc.JumpToNextChar; //
              if PosCharInTag('>') then
                sc.JumpToNextChar
              else
                DoError('LineNum:' + IntToStr(BeginLineNum) + 'Unable to find tag end point:' + sc.subStr(100));
            end
            else
            begin
              sc.JumpToNextChar; //
              if PosCharInTag('>') then
                sc.JumpToNextChar
              else
                DoError('LineNum:' + IntToStr(BeginLineNum) + 'Unable to find tag end point:' + sc.subStr(100));
            end;
          end;
        end;
      end
      else if sc.CurrentChar = '?' then // <?...?>  XML
      begin
        ElementType := EtComment;
        sc.JumpToNextChar; //
        while True do
        begin
          if not PosCharInTag('>') then
            DoError('LineNum:' + IntToStr(BeginLineNum) + 'Unable to find tag end point:' + sc.subStr(100))
          else if (sc.charOfCurrent[ -1] = '?') then
          begin
            sc.JumpToNextChar;
            Break;
          end;
          sc.JumpToNextChar;
        end;
      end
      else // normal node
      begin
        ElementType := EtTag;
        sc.JumpToNextChar;
        if PosCharInTag('>') then
          sc.JumpToNextChar
        else
          DoError('LineNum:' + IntToStr(BeginLineNum) + 'Unable to find tag end point:' + sc.subStr(100));
      end;
      tmp := sc.subStr(OldCodeIndex, sc.CharIndex - OldCodeIndex);
    end
    else // If it doesn't start with "<", it is a plain text node
    begin
      ElementType := EtText;
      while True do
      begin
        if CharInSet(sc.CurrentChar, [#0, '<']) then
          Break;
        sc.JumpToNextChar;
      end;
      tmp := sc.subStr(OldCodeIndex, sc.CharIndex - OldCodeIndex);
    end;
    //
    // ShowMessage(sc.subStr(30));
    case ElementType of
      EtUnknow:
        begin
          DoError('LineNum:' + IntToStr(BeginLineNum) + 'Unparseable content:' + sc.subStr(100));
        end;
      EtDocType:
        begin
          Tag := CreateDocTypeElement(nil, tmp, BeginLineNum, BeginColNum);
          AElementList.Add(Tag);
        end;
      EtTag:
        begin
          Tag := CreateTagElement(nil, tmp, BeginLineNum, BeginColNum);
          AElementList.Add(Tag);
          //
          if (UpperCase(Tag.FTagName) = 'SCRIPT') and (not Tag.FIsCloseTag) and (not Tag.FClosed) then
          begin
            // read script
            BeginLineNum := sc.LineNum;
            BeginColNum := sc.ColNum;
            tmp := ParserScriptData();
            Tag := CreateScriptElement(nil, tmp, BeginLineNum, BeginColNum);
            AElementList.Add(Tag);
          end
          else if (UpperCase(Tag.FTagName) = 'STYLE') and (not Tag.FIsCloseTag) and (not Tag.FClosed) then
          begin
            // read Style
            BeginLineNum := sc.LineNum;
            BeginColNum := sc.ColNum;
            tmp := ParserStyleData();
            Tag := CreateStyleElement(nil, tmp, BeginLineNum, BeginColNum);
            AElementList.Add(Tag);
          end;
        end;
      EtText:
        begin
          Tag := CreateTextElement(nil, tmp, BeginLineNum, BeginColNum);
          Tag.FSourceLine := BeginLineNum;
          Tag.FSourceCol := BeginColNum;
          AElementList.Add(Tag);
        end;
      EtComment:
        begin
          Tag := CreateCommentElement(nil, tmp, BeginLineNum, BeginColNum);
          Tag.FSourceLine := BeginLineNum;
          Tag.FSourceCol := BeginColNum;
          AElementList.Add(Tag);
        end;
    end;
  end;
  //
  //
end;


function BuildTree(ElementList:THtmlElementList):THtmlElement;
var
  I, J:Integer;
  E:THtmlElement;
  T:THtmlElement;
  FoundIndex:Integer;
begin
  Result := THtmlElement.Create(nil, '', 0, 0);
  Result.FTagName := '#DOCUMENT';
  Result.FClosed := false;
  ElementList.Insert(0, Result);
  I := 1;
  while I < ElementList.Count do
  begin
    E := ElementList[I] as THtmlElement;
    //TagProperty := GetTagProperty(E.FTagName);
    // Empty node, look down, if the next node with Tag is not its closed node, then automatically close
    FoundIndex := -1;
    if E.FIsCloseTag then
    begin
      for J := (I - 1) downto 0 do
      begin
        T := ElementList[J] as THtmlElement;
        if (not T.FClosed) and (T.FTagName = E.FTagName) and (not T.FIsCloseTag) then
        begin
          FoundIndex := J;
          Break;
        end;
      end;
      // If you look up and can't find it, this closing Tag is definitely meaningless.
      if FoundIndex > 0 then
      begin
        for J := (I - 1) downto FoundIndex do
        begin
          T := ElementList[J] as THtmlElement;
          T.FClosed := True;
        end;
        (ElementList[FoundIndex] as THtmlElement).FCloseTag := E;
      end
      else
      begin
        E.Free;
      end;
      ElementList.Delete(I);
      Continue;
    end
    else
    begin
      for J := (I - 1) downto 0 do
      begin
        T := ElementList[J] as THtmlElement;
        if not T.FClosed then
        begin
          if ((GetTagProperty(T.FTagName) and tpEmpty) <> 0) then
            T.FClosed := True
          else
          begin
            T.FChildren.Add(E);
            E.FOwner := T;
            Break;
          end;
        end;
      end;
    end;
    Inc(I);
  end;
  Result.FClosed := True;
end;


function ParserHTML(const Source:String):IHtmlElement; stdcall;
var
  ElementList:THtmlElementList;
begin
  ElementList := THtmlElementList.Create;
  _ParserHTML(Source, ElementList);
  Result := BuildTree(ElementList);
  ElementList.Free;
end;
{$REGION 'entity conversions'}
var
  gEntities:TStringDictionary;

type
  TEntityItem = record
    Key:string;
    Value:WideChar;
  end;
const
  EntityTable: array [0 .. 252 - 1] of TEntityItem = ((Key:'&nbsp;'; Value:#32), (Key:'&iexcl;'; Value:WideChar(161)), (Key:'&cent;'; Value:WideChar(162)), (Key:'&pound;'; Value:WideChar(163)), (Key:'&curren;'; Value:WideChar(164)),
    (Key:'&yen;'; Value:WideChar(165)), (Key:'&brvbar;'; Value:WideChar(166)), (Key:'&sect;'; Value:WideChar(167)), (Key:'&uml;'; Value:WideChar(168)), (Key:'&copy;'; Value:WideChar(169)), (Key:'&ordf;'; Value:WideChar(170)),
    (Key:'&laquo;'; Value:WideChar(171)), (Key:'&not;'; Value:WideChar(172)), (Key:'&shy;'; Value:WideChar(173)), (Key:'&reg;'; Value:WideChar(174)), (Key:'&macr;'; Value:WideChar(175)), (Key:'&deg;'; Value:WideChar(176)), (Key:'&plusmn;';
    Value:WideChar(177)), (Key:'&sup2;'; Value:WideChar(178)), (Key:'&sup3;'; Value:WideChar(179)), (Key:'&acute;'; Value:WideChar(180)), (Key:'&micro;'; Value:WideChar(181)), (Key:'&para;'; Value:WideChar(182)), (Key:'&middot;';
    Value:WideChar(183)), (Key:'&cedil;'; Value:WideChar(184)), (Key:'&sup1;'; Value:WideChar(185)), (Key:'&ordm;'; Value:WideChar(186)), (Key:'&raquo;'; Value:WideChar(187)), (Key:'&frac14;'; Value:WideChar(188)), (Key:'&frac12;';
    Value:WideChar(189)), (Key:'&frac34;'; Value:WideChar(190)), (Key:'&iquest;'; Value:WideChar(191)), (Key:'&Agrave;'; Value:WideChar(192)), (Key:'&Aacute;'; Value:WideChar(193)), (Key:'&Acirc;'; Value:WideChar(194)), (Key:'&Atilde;';
    Value:WideChar(195)), (Key:'&Auml;'; Value:WideChar(196)), (Key:'&Aring;'; Value:WideChar(197)), (Key:'&AElig;'; Value:WideChar(198)), (Key:'&Ccedil;'; Value:WideChar(199)), (Key:'&Egrave;'; Value:WideChar(200)), (Key:'&Eacute;';
    Value:WideChar(201)), (Key:'&Ecirc;'; Value:WideChar(202)), (Key:'&Euml;'; Value:WideChar(203)), (Key:'&Igrave;'; Value:WideChar(204)), (Key:'&Iacute;'; Value:WideChar(205)), (Key:'&Icirc;'; Value:WideChar(206)), (Key:'&Iuml;';
    Value:WideChar(207)), (Key:'&ETH;'; Value:WideChar(208)), (Key:'&Ntilde;'; Value:WideChar(209)), (Key:'&Ograve;'; Value:WideChar(210)), (Key:'&Oacute;'; Value:WideChar(211)), (Key:'&Ocirc;'; Value:WideChar(212)), (Key:'&Otilde;';
    Value:WideChar(213)), (Key:'&Ouml;'; Value:WideChar(214)), (Key:'&times;'; Value:WideChar(215)), (Key:'&Oslash;'; Value:WideChar(216)), (Key:'&Ugrave;'; Value:WideChar(217)), (Key:'&Uacute;'; Value:WideChar(218)), (Key:'&Ucirc;';
    Value:WideChar(219)), (Key:'&Uuml;'; Value:WideChar(220)), (Key:'&Yacute;'; Value:WideChar(221)), (Key:'&THORN;'; Value:WideChar(222)), (Key:'&szlig;'; Value:WideChar(223)), (Key:'&agrave;'; Value:WideChar(224)), (Key:'&aacute;';
    Value:WideChar(225)), (Key:'&acirc;'; Value:WideChar(226)), (Key:'&atilde;'; Value:WideChar(227)), (Key:'&auml;'; Value:WideChar(228)), (Key:'&aring;'; Value:WideChar(229)), (Key:'&aelig;'; Value:WideChar(230)), (Key:'&ccedil;';
    Value:WideChar(231)), (Key:'&egrave;'; Value:WideChar(232)), (Key:'&eacute;'; Value:WideChar(233)), (Key:'&ecirc;'; Value:WideChar(234)), (Key:'&euml;'; Value:WideChar(235)), (Key:'&igrave;'; Value:WideChar(236)), (Key:'&iacute;';
    Value:WideChar(237)), (Key:'&icirc;'; Value:WideChar(238)), (Key:'&iuml;'; Value:WideChar(239)), (Key:'&eth;'; Value:WideChar(240)), (Key:'&ntilde;'; Value:WideChar(241)), (Key:'&ograve;'; Value:WideChar(242)), (Key:'&oacute;';
    Value:WideChar(243)), (Key:'&ocirc;'; Value:WideChar(244)), (Key:'&otilde;'; Value:WideChar(245)), (Key:'&ouml;'; Value:WideChar(246)), (Key:'&divide;'; Value:WideChar(247)), (Key:'&oslash;'; Value:WideChar(248)), (Key:'&ugrave;';
    Value:WideChar(249)), (Key:'&uacute;'; Value:WideChar(250)), (Key:'&ucirc;'; Value:WideChar(251)), (Key:'&uuml;'; Value:WideChar(252)), (Key:'&yacute;'; Value:WideChar(253)), (Key:'&thorn;'; Value:WideChar(254)), (Key:'&yuml;';
    Value:WideChar(255)), (Key:'&fnof;'; Value:WideChar(402)), (Key:'&Alpha;'; Value:WideChar(913)), (Key:'&Beta;'; Value:WideChar(914)), (Key:'&Gamma;'; Value:WideChar(915)), (Key:'&Delta;'; Value:WideChar(916)), (Key:'&Epsilon;';
    Value:WideChar(917)), (Key:'&Zeta;'; Value:WideChar(918)), (Key:'&Eta;'; Value:WideChar(919)), (Key:'&Theta;'; Value:WideChar(920)), (Key:'&Iota;'; Value:WideChar(921)), (Key:'&Kappa;'; Value:WideChar(922)), (Key:'&Lambda;';
    Value:WideChar(923)), (Key:'&Mu;'; Value:WideChar(924)), (Key:'&Nu;'; Value:WideChar(925)), (Key:'&Xi;'; Value:WideChar(926)), (Key:'&Omicron;'; Value:WideChar(927)), (Key:'&Pi;'; Value:WideChar(928)), (Key:'&Rho;';
    Value:WideChar(929)), (Key:'&Sigma;'; Value:WideChar(931)), (Key:'&Tau;'; Value:WideChar(932)), (Key:'&Upsilon;'; Value:WideChar(933)), (Key:'&Phi;'; Value:WideChar(934)), (Key:'&Chi;'; Value:WideChar(935)), (Key:'&Psi;';
    Value:WideChar(936)), (Key:'&Omega;'; Value:WideChar(937)), (Key:'&alpha;'; Value:WideChar(945)), (Key:'&beta;'; Value:WideChar(946)), (Key:'&gamma;'; Value:WideChar(947)), (Key:'&delta;'; Value:WideChar(948)), (Key:'&epsilon;';
    Value:WideChar(949)), (Key:'&zeta;'; Value:WideChar(950)), (Key:'&eta;'; Value:WideChar(951)), (Key:'&theta;'; Value:WideChar(952)), (Key:'&iota;'; Value:WideChar(953)), (Key:'&kappa;'; Value:WideChar(954)), (Key:'&lambda;';
    Value:WideChar(955)), (Key:'&mu;'; Value:WideChar(956)), (Key:'&nu;'; Value:WideChar(957)), (Key:'&xi;'; Value:WideChar(958)), (Key:'&omicron;'; Value:WideChar(959)), (Key:'&pi;'; Value:WideChar(960)), (Key:'&rho;';
    Value:WideChar(961)), (Key:'&sigmaf;'; Value:WideChar(962)), (Key:'&sigma;'; Value:WideChar(963)), (Key:'&tau;'; Value:WideChar(964)), (Key:'&upsilon;'; Value:WideChar(965)), (Key:'&phi;'; Value:WideChar(966)), (Key:'&chi;';
    Value:WideChar(967)), (Key:'&psi;'; Value:WideChar(968)), (Key:'&omega;'; Value:WideChar(969)), (Key:'&thetasym;'; Value:WideChar(977)), (Key:'&upsih;'; Value:WideChar(978)), (Key:'&piv;'; Value:WideChar(982)), (Key:'&bull;';
    Value:WideChar(8226)), (Key:'&hellip;'; Value:WideChar(8230)), (Key:'&prime;'; Value:WideChar(8242)), (Key:'&Prime;'; Value:WideChar(8243)), (Key:'&oline;'; Value:WideChar(8254)), (Key:'&frasl;'; Value:WideChar(8260)), (Key:'&weierp;';
    Value:WideChar(8472)), (Key:'&image;'; Value:WideChar(8465)), (Key:'&real;'; Value:WideChar(8476)), (Key:'&trade;'; Value:WideChar(8482)), (Key:'&alefsym;'; Value:WideChar(8501)), (Key:'&larr;'; Value:WideChar(8592)), (Key:'&uarr;';
    Value:WideChar(8593)), (Key:'&rarr;'; Value:WideChar(8594)), (Key:'&darr;'; Value:WideChar(8595)), (Key:'&harr;'; Value:WideChar(8596)), (Key:'&crarr;'; Value:WideChar(8629)), (Key:'&lArr;'; Value:WideChar(8656)), (Key:'&uArr;';
    Value:WideChar(8657)), (Key:'&rArr;'; Value:WideChar(8658)), (Key:'&dArr;'; Value:WideChar(8659)), (Key:'&hArr;'; Value:WideChar(8660)), (Key:'&forall;'; Value:WideChar(8704)), (Key:'&part;'; Value:WideChar(8706)), (Key:'&exist;';
    Value:WideChar(8707)), (Key:'&empty;'; Value:WideChar(8709)), (Key:'&nabla;'; Value:WideChar(8711)), (Key:'&isin;'; Value:WideChar(8712)), (Key:'&notin;'; Value:WideChar(8713)), (Key:'&ni;'; Value:WideChar(8715)), (Key:'&prod;';
    Value:WideChar(8719)), (Key:'&sum;'; Value:WideChar(8721)), (Key:'&minus;'; Value:WideChar(8722)), (Key:'&lowast;'; Value:WideChar(8727)), (Key:'&radic;'; Value:WideChar(8730)), (Key:'&prop;'; Value:WideChar(8733)), (Key:'&infin;';
    Value:WideChar(8734)), (Key:'&ang;'; Value:WideChar(8736)), (Key:'&and;'; Value:WideChar(8743)), (Key:'&or;'; Value:WideChar(8744)), (Key:'&cap;'; Value:WideChar(8745)), (Key:'&cup;'; Value:WideChar(8746)), (Key:'&int;';
    Value:WideChar(8747)), (Key:'&there4;'; Value:WideChar(8756)), (Key:'&sim;'; Value:WideChar(8764)), (Key:'&cong;'; Value:WideChar(8773)), (Key:'&asymp;'; Value:WideChar(8776)), (Key:'&ne;'; Value:WideChar(8800)), (Key:'&equiv;';
    Value:WideChar(8801)), (Key:'&le;'; Value:WideChar(8804)), (Key:'&ge;'; Value:WideChar(8805)), (Key:'&sub;'; Value:WideChar(8834)), (Key:'&sup;'; Value:WideChar(8835)), (Key:'&nsub;'; Value:WideChar(8836)), (Key:'&sube;';
    Value:WideChar(8838)), (Key:'&supe;'; Value:WideChar(8839)), (Key:'&oplus;'; Value:WideChar(8853)), (Key:'&otimes;'; Value:WideChar(8855)), (Key:'&perp;'; Value:WideChar(8869)), (Key:'&sdot;'; Value:WideChar(8901)), (Key:'&lceil;';
    Value:WideChar(8968)), (Key:'&rceil;'; Value:WideChar(8969)), (Key:'&lfloor;'; Value:WideChar(8970)), (Key:'&rfloor;'; Value:WideChar(8971)), (Key:'&lang;'; Value:WideChar(9001)), (Key:'&rang;'; Value:WideChar(9002)), (Key:'&loz;';
    Value:WideChar(9674)), (Key:'&spades;'; Value:WideChar(9824)), (Key:'&clubs;'; Value:WideChar(9827)), (Key:'&hearts;'; Value:WideChar(9829)), (Key:'&diams;'; Value:WideChar(9830)), (Key:'&quot;'; Value:WideChar(34)), (Key:'&amp;';
    Value:WideChar(38)), (Key:'&lt;'; Value:WideChar(60)), (Key:'&gt;'; Value:WideChar(62)), (Key:'&OElig;'; Value:WideChar(338)), (Key:'&oelig;'; Value:WideChar(339)), (Key:'&Scaron;'; Value:WideChar(352)), (Key:'&scaron;';
    Value:WideChar(353)), (Key:'&Yuml;'; Value:WideChar(376)), (Key:'&circ;'; Value:WideChar(710)), (Key:'&tilde;'; Value:WideChar(732)), (Key:'&ensp;'; Value:WideChar(8194)), (Key:'&emsp;'; Value:WideChar(8195)), (Key:'&thinsp;';
    Value:WideChar(8201)), (Key:'&zwnj;'; Value:WideChar(8204)), (Key:'&zwj;'; Value:WideChar(8205)), (Key:'&lrm;'; Value:WideChar(8206)), (Key:'&rlm;'; Value:WideChar(8207)), (Key:'&ndash;'; Value:WideChar(8211)), (Key:'&mdash;';
    Value:WideChar(8212)), (Key:'&lsquo;'; Value:WideChar(8216)), (Key:'&rsquo;'; Value:WideChar(8217)), (Key:'&sbquo;'; Value:WideChar(8218)), (Key:'&ldquo;'; Value:WideChar(8220)), (Key:'&rdquo;'; Value:WideChar(8221)), (Key:'&bdquo;';
    Value:WideChar(8222)), (Key:'&dagger;'; Value:WideChar(8224)), (Key:'&Dagger;'; Value:WideChar(8225)), (Key:'&permil;'; Value:WideChar(8240)), (Key:'&lsaquo;'; Value:WideChar(8249)), (Key:'&rsaquo;'; Value:WideChar(8250)),
    (Key:'&euro;'; Value:WideChar(8364)));


function HexToChar(Value:string):Char;
var
  I:Integer;
  W:WORD;
begin
  W := 0;
  for I := low(Value) to high(Value) do
  begin
    case Value[I] of
      '0' .. '9':
        W := (W shl 4) or (ord(Value[I]) - ord('0'));
      'a' .. 'f':
        W := (W shl 4) or (ord(Value[I]) - ord('a') + 10);
      'A' .. 'F':
        W := (W shl 4) or (ord(Value[I]) - ord('A') + 10);
    else
      W := 0;
    end;
  end;
  Result := Char(W);
end;


function DecToChar(Value:string):Char;
var
  I:Integer;
  W:WORD;
begin
  W := 0;
  for I := low(Value) to high(Value) do
  begin
    case Value[I] of
      '0' .. '9':
        W := 10 * W + (ord(Value[I]) - ord('0'));
    else
      W := 0;
    end;
  end;
  Result := Char(W);
end;


function DecodeHtmlEntities(S:string):string;
var
  tmp:string;
  I, p:Integer;
  Sb:TStringBuilder;
begin
  if Length(S) <= 3 then
    Exit(S);
  if Pos('&#', S) > 0 then
  begin
    S[low(S)] := S[low(S)];
  end;
  Sb := TStringBuilder.Create;
  I := 0;
  while I < Length(S) do
  begin
    if S.Chars[I] = '&' then
    begin
      p := S.IndexOf(';', I);
      if p >= 0 then
      begin
        tmp := LowerCase(S.Substring(I, p - I + 1));
        if (Length(tmp) > 2) and (tmp.Chars[1] = '#') then
        begin
          if (Length(tmp) > 3) and (tmp.Chars[2] = '$') then
            Sb.Append(HexToChar(tmp.Substring(3, Length(tmp) - 4)))
          else
            Sb.Append(DecToChar(tmp.Substring(2, Length(tmp) - 3)));
        end
        else if gEntities.ContainsKey(tmp) then
          Sb.Append(gEntities[tmp])
        else
          Sb.Append(tmp);
        Inc(I, Length(tmp));
      end
      else
      begin
        Sb.Append(S.Chars[I]);
        Inc(I);
      end;
    end
    else
    begin
      Sb.Append(S.Chars[I]);
      Inc(I);
    end;
  end;
  Result := Sb.ToString;
  FreeAndNil(Sb);
end;


function ConvertWhiteSpace(S:string):string;
var
  Sb:TStringBuilder;
  I:Integer;
  PreIssWhite, ThisIsWhite:Boolean;
begin
  Sb := TStringBuilder.Create;
  PreIssWhite := false;
  for I := low(S) to high(S) do
  begin
    ThisIsWhite := CharInSet(S[I], WhiteSpace);
    if ThisIsWhite then
    begin
      if not PreIssWhite then
        Sb.Append(S[I]);
      PreIssWhite := True;
    end
    else
    begin
      Sb.Append(S[I]);
      PreIssWhite := false;
    end;
  end;
  Result := Sb.ToString;
  Sb.Free;
end;
const
  BlockTags: array [0 .. 59 - 1] of string = ('HTML', 'HEAD', 'BODY', 'FRAMESET', 'SCRIPT', 'NOSCRIPT', 'STYLE', 'META', 'LINK', 'TITLE', 'FRAME', 'NOFRAMES', 'SECTION', 'NAV', 'ASIDE', 'HGROUP', 'HEADER', 'FOOTER', 'P', 'H1', 'H2', 'H3',
    'H4', 'H5', 'H6', 'UL', 'OL', 'PRE', 'DIV', 'BLOCKQUOTE', 'HR', 'ADDRESS', 'FIGURE', 'FIGCAPTION', 'FORM', 'FIELDSET', 'INS', 'DEL', 'S', 'DL', 'DT', 'DD', 'LI', 'TABLE', 'CAPTION', 'THEAD', 'TFOOT', 'TBODY', 'COLGROUP', 'COL', 'TR',
    'TH', 'TD', 'VIDEO', 'AUDIO', 'CANVAS', 'DETAILS', 'MENU', 'PLAINTEXT');
  InlineTags: array [0 .. 56 - 1] of string = ('OBJECT', 'BASE', 'FONT', 'TT', 'I', 'B', 'U', 'BIG', 'SMALL', 'EM', 'STRONG', 'DFN', 'CODE', 'SAMP', 'KBD', 'VAR', 'CITE', 'ABBR', 'TIME', 'ACRONYM', 'MARK', 'RUBY', 'RT', 'RP', 'A', 'IMG',
    'BR', 'WBR', 'MAP', 'Q', 'SUB', 'SUP', 'BDO', 'IFRAME', 'EMBED', 'SPAN', 'INPUT', 'SELECT', 'TEXTAREA', 'LABEL', 'BUTTON', 'OPTGROUP', 'OPTION', 'LEGEND', 'DATALIST', 'KEYGEN', 'OUTPUT', 'PROGRESS', 'METER', 'AREA', 'PARAM', 'SOURCE',
    'TRACK', 'SUMMARY', 'COMMAND', 'DEVICE');
  EmptyTags: array [0 .. 14 - 1] of string = ('META', 'LINK', 'BASE', 'FRAME', 'IMG', 'BR', 'WBR', 'EMBED', 'HR', 'INPUT', 'KEYGEN', 'COL', 'COMMAND', 'DEVICE');
  FormatAsInlineTags: array [0 .. 19 - 1] of string = ('TITLE', 'A', 'P', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6', 'PRE', 'ADDRESS', 'LI', 'TH', 'TD', 'SCRIPT', 'STYLE', 'INS', 'DEL', 'S');
  PreserveWhitespaceTags: array [0 .. 4 - 1] of string = ('PRE', 'PLAINTEXT', 'TITLE', 'TEXTAREA');
var
  gTagProperty:TPropDictionary;


function GetTagProperty(const TagName:string):WORD;
var
  Key:string;
begin
  Result := 0;
  Key := UpperCase(TagName);
  if gTagProperty.ContainsKey(Key) then
    Result := gTagProperty[UpperCase(TagName)]
  else
    Exit;
end;


function ParserCSSSelector(const Value:string):TCSSSelectorItemGroup;
var
  sc:TSourceContext;
  function AddAttr(var Item:TCSSSelectorItem):PAttrSelectorItem;
  begin
    SetLength(Item.Attributes, Length(Item.Attributes) + 1);
    Result := @Item.Attributes[Length(Item.Attributes) - 1];
  end;
  function ParserAttr():TAttrSelectorItem;
  var
    oldIndex:Integer;
    tmp:string;
    stringChar:Char;
  begin
    sc.JumpToNextChar(); // [
    Result.Key := '';
    Result.AttrOperator := aoEqual;
    Result.Value := '';
    // Key
    sc.SkipBlank();
    oldIndex := sc.CharIndex;
    while not CharInSet(sc.CurrentChar, (WhiteSpace + OperatorChar + [']', #0])) do
      sc.JumpToNextChar();
    Result.Key := sc.subStr(oldIndex, sc.CharIndex - oldIndex);
    Result.Key := LowerCase(Result.Key);
    // Operator
    sc.SkipBlank();
    oldIndex := sc.CharIndex;
    case sc.CurrentChar of
      '=', '!', '*', '~', '|', '^', '$':
        begin
          sc.JumpToNextChar;
          if sc.CurrentChar = '=' then
            sc.JumpToNextChar;
        end;
      ']':
        begin
          Result.AttrOperator := aoExist;
          sc.JumpToNextChar;
          Exit;
        end;
    else
      begin
        DoError(Format('Could not resolve CSS Attribute operator [%d,%d]', [sc.LineNum, sc.ColNum]));
      end;
    end;
    tmp := sc.subStr(oldIndex, sc.CharIndex - oldIndex);
    if Length(tmp) >= 1 then
    begin
      case tmp[LowStrIndex] of
        '=':
          Result.AttrOperator := aoEqual;
        '!':
          Result.AttrOperator := aoNotEqual;
        '*':
          Result.AttrOperator := aoContain;
        '~':
          Result.AttrOperator := aoIncludeWord;
        '|':
          Result.AttrOperator := aoBeginWord;
        '^':
          Result.AttrOperator := aoBegin;
        '$':
          Result.AttrOperator := aoEnd;
      end;
    end;
    // Value
    sc.SkipBlank();
    oldIndex := sc.CharIndex;
    if CharInSet(sc.CurrentChar, ['"', '''']) then
      stringChar := sc.CurrentChar
    else
      stringChar := #0;
    sc.JumpToNextChar();
    while True do
    begin
      if stringChar = #0 then
      begin
        if CharInSet(sc.CurrentChar, (WhiteSpace + [#0, ']'])) then
          Break;
      end
      else if (sc.CurrentChar = stringChar) then
      begin
        sc.JumpToNextChar();
        Break;
      end;
      sc.JumpToNextChar();
    end;
    Result.Value := sc.subStr(oldIndex, sc.CharIndex - oldIndex);
    // SetString(Result.Value, oldP, P - oldP);
    if (stringChar <> #0) and (Length(Result.Value) >= 2) then
      Result.Value := Copy(Result.Value, 2, Length(Result.Value) - 2);
    Result.Value := DecodeHtmlEntities(Result.Value);
    //
    sc.SkipBlank();
    if sc.CurrentChar = ']' then
      sc.JumpToNextChar
    else
      DoError(Format('Unable to parse attribute value [%d,%d]', [sc.LineNum, sc.ColNum]));
  end;
  procedure ParserItem(var Item:TCSSSelectorItem);
  var
    pAttr:PAttrSelectorItem;
  begin
    sc.SkipBlank();
    while True do
    begin
      case sc.CurrentChar of
        #0, ',', ' ':
          Break;
        '.': // class
          begin
            sc.JumpToNextChar();
            pAttr := AddAttr(Item);
            pAttr^.Key := 'class';
            pAttr^.AttrOperator := aoIncludeWord;
            pAttr^.Value := sc.ReadStr((WhiteSpace + OperatorChar + ['[', ']', '"', '''', ',', '.', '#', #0]));
          end;
        '#': // id
          begin
            sc.JumpToNextChar();
            pAttr := AddAttr(Item);
            pAttr^.Key := 'id';
            pAttr^.AttrOperator := aoEqual;
            pAttr^.Value := sc.ReadStr((WhiteSpace + OperatorChar + ['[', ']', '"', '''', ',', '.', '#', #0]));
          end;
        '[': // attribute
          begin
            pAttr := AddAttr(Item);
            pAttr^ := ParserAttr();
          end;
        '/':
          begin
            sc.JumpToNextChar();
            if sc.CurrentChar = '*' then // /**/
            begin
              sc.JumpToNextChar();
              sc.JumpToNextChar();
              while True do
              begin
                if (sc.CurrentChar = '/') and (sc.charOfCurrent[ -1] = '*') then
                begin
                  sc.JumpToNextChar;
                  Break;
                end;
                sc.JumpToNextChar;
              end;
            end;
          end;
      else
        begin
          Item.szTag := UpperCase(sc.ReadStr((WhiteSpace + ['[', ']', '"', '''', ',', '.', '#', #0])));
        end;
      end;
    end;
  end;
  function AddItems(var Group:TCSSSelectorItemGroup):PCSSSelectorItems;
  begin
    SetLength(Group, Length(Group) + 1);
    Result := @Group[Length(Group) - 1];
  end;
  function AddItem(var Items:TCSSSelectorItems):PCSSSelectorItem;
  begin
    SetLength(Items, Length(Items) + 1);
    Result := @Items[Length(Items) - 1];
    Result^.Relation := sirNONE;
  end;


var
  pitems:PCSSSelectorItems;
  pItem:PCSSSelectorItem;
begin
  sc.setCode(Value);
  //
  pitems := AddItems(Result);
  pItem := AddItem(pitems^);
  while True do
  begin
    sc.SkipBlank;
    ParserItem(pItem^);
    sc.SkipBlank;
    case sc.CurrentChar of
      ',':
        begin
          sc.JumpToNextChar();
          pitems := AddItems(Result);
          pItem := AddItem(pitems^);
        end;
      '>':
        begin
          sc.JumpToNextChar();
          pItem := AddItem(pitems^);
          pItem^.Relation := sirChildren;
        end;
      '+':
        begin
          sc.JumpToNextChar();
          pItem := AddItem(pitems^);
          pItem^.Relation := sirYoungerBrother;
        end;
      '~':
        begin
          sc.JumpToNextChar();
          pItem := AddItem(pitems^);
          pItem^.Relation := sirAllYoungerBrother;
        end;
      #0:
        Break;
    else
      begin
        pItem := AddItem(pitems^);
        pItem^.Relation := sirDescendant;
      end;
    end;
  end;
end;


procedure Init();
var
  I:Integer;
  Key:string;
  S:WORD;
begin
  gEntities := TStringDictionary.Create();
  gTagProperty := TPropDictionary.Create;
  for I := low(EntityTable) to high(EntityTable) do
  begin
    gEntities.Add(EntityTable[I].Key, EntityTable[I].Value);
  end;
  //
  for I := low(BlockTags) to high(BlockTags) do
    gTagProperty.AddOrSetValue(BlockTags[I], tpBlock);
  for I := low(InlineTags) to high(InlineTags) do
    gTagProperty.AddOrSetValue(InlineTags[I], tpInline);
  for I := low(EmptyTags) to high(EmptyTags) do
  begin
    Key := EmptyTags[I];
    if gTagProperty.ContainsKey(Key) then
      S := gTagProperty[Key]
    else
      S := 0;
    S := S or tpEmpty;
    gTagProperty.AddOrSetValue(Key, S);
  end;
  for I := low(FormatAsInlineTags) to high(FormatAsInlineTags) do
  begin
    Key := FormatAsInlineTags[I];
    if gTagProperty.ContainsKey(Key) then
      S := gTagProperty[Key]
    else
      S := 0;
    S := S or tpFormatAsInline;
    gTagProperty.AddOrSetValue(FormatAsInlineTags[I], S);
  end;
  for I := low(PreserveWhitespaceTags) to high(PreserveWhitespaceTags) do
  begin
    Key := PreserveWhitespaceTags[I];
    if gTagProperty.ContainsKey(Key) then
      S := gTagProperty[Key]
    else
      S := 0;
    S := S or tpPreserveWhitespace;
    gTagProperty.AddOrSetValue(PreserveWhitespaceTags[I], S);
  end;
end;


procedure UnInit();
begin
  gTagProperty.Free;
  gEntities.Free;
end;
{$ENDREGION 'conversion table etc.'}


{ TIHtmlElementList }
function TIHtmlElementList.Add(Value:IHtmlElement):Integer;
begin
  Result := FList.Add(Value);
end;


procedure TIHtmlElementList.Clear;
begin
  FList.Clear;
end;


constructor TIHtmlElementList.Create;
begin
  inherited Create;
  FList := TList<IHtmlElement>.Create;
end;


procedure TIHtmlElementList.Delete(Index:Integer);
begin
  FList.Delete(index);
end;


destructor TIHtmlElementList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;


procedure TIHtmlElementList.Each(f:TElementEachEvent);
var
  I:Integer;
begin
  if not Assigned(f) then
    Exit;
  for I := 0 to FList.Count - 1 do
  begin
    f(I, FList[I]);
  end;
end;


function TIHtmlElementList.GetCount:Integer;
begin
  Result := FList.Count;
end;


function TIHtmlElementList.GetEnumerator:THtmlListEnumerator;
begin
  Result := THtmlListEnumerator.Create(Self);
end;


function TIHtmlElementList.GetItems(Index:Integer):IHtmlElement;
begin
  Result := FList[index];
end;


function TIHtmlElementList.GetText:String;
var
  LEL:IHtmlElement;
begin
  Result := '';
  for LEL in FList do
    Result := Result + LEL.InnerText;
end;


function TIHtmlElementList.IndexOf(Item:IHtmlElement):Integer;
begin
  Result := FList.IndexOf(Item)
end;


procedure TIHtmlElementList.Remove(ANode:IHtmlElement);
begin
  FList.Remove(ANode);
end;


procedure TIHtmlElementList.RemoveAll;
var
  I:Integer;
  LParent:IHtmlElement;
begin
  for I := 0 to FList.Count - 1 do
  begin
    LParent := FList[I].Parent;
    if LParent <> nil then
      LParent.RemoveChild(FList[I]);
  end;
end;


procedure TIHtmlElementList.SetItems(Index:Integer; const Value:IHtmlElement);
begin
  FList[index] := Value;
end;


{ THtmlElement }
constructor THtmlElement.Create(AOwner:THtmlElement; AText:string; ALine, ACol:Integer);
begin
  inherited Create;
  FAttributes := TStringDictionary.Create();
  FChildren := TIHtmlElementList.Create;
  FOwner := AOwner;
  FOrignal := AText;
  FSourceLine := ALine;
  FSourceCol := ACol;
end;


destructor THtmlElement.Destroy;
begin
  FChildren.Free;
  FAttributes.Free;
  inherited Destroy;
end;


function THtmlElement.AppedChild(const ATag:string):IHtmlElement;
begin
  Result := nil;
  if ATag = '' then
    Exit;
  if Assigned(FChildren) then
  begin
    Result := THtmlElement.Create(Self, '', 0, 0);
    Result.TagName := ATag;
    THtmlElement(Result).FClosed := True;
    THtmlElement(Result).FCloseTag := THtmlElement.Create(Self, '', 0, 0);
    Result.CloseTag.TagName := ATag;
    THtmlElement(Result.CloseTag).FClosed := false;
    FChildren.Add(Result);
  end;
end;


function THtmlElement.EnumAttributeNames(Index:Integer):String;
var
  Attrs:TStringDynArray;
begin
  Result := '';
  Attrs := FAttributes.Keys.ToArray;
  if index < Length(Attrs) then
    Result := Attrs[index];
end;


function THtmlElement.GetAttributes(Key:String):String;
begin
  Result := '';
  Key := LowerCase(Key);
  if FAttributes.ContainsKey(Key) then
    Result := FAttributes[Key];
end;


function THtmlElement.GetChildren(Index:Integer):IHtmlElement;
begin
  Result := FChildren[index];
end;


function THtmlElement.GetChildrenCount:Integer;
begin
  Result := FChildren.Count;
end;


function THtmlElement.GetChildrens:IHtmlElementList;
begin
  Result := FChildren;
end;


function THtmlElement.GetCloseTag:IHtmlElement;
begin
  Result := FCloseTag;
end;


function THtmlElement.GetContent:String;
begin
  Result := FContent;
end;


procedure THtmlElement._GetHtml(IncludeSelf:Boolean; Sb:TStringBuilder);
  function GetSelfHtml:string;
  var
    LAttrs:string;
    LV:TPair<string, string>;
  begin
    if FTagName.Equals(cSpecialTagName_Text) or FTagName.Equals('#SCRIPT') or FTagName.Equals('#STYLE') or FTagName.Equals('#COMMENT') or FTagName.Equals('#DOCTYPE') or FTagName.Equals('#DOCUMENT') then
    begin
      Result := FContent;
    end
    else
    begin
      if not FClosed then
      begin
        Result := Format('</%s>', [FTagName.ToLower]);
      end
      else
      begin
        LAttrs := '';
        for LV in FAttributes do
          if ContainsStr(LV.Value, '"') then
            LAttrs := LAttrs + Format('%s=''%s'' ', [LV.Key, LV.Value])
          else
            LAttrs := LAttrs + Format('%s="%s" ', [LV.Key, LV.Value]);
        if LAttrs.Length > 2 then
          LAttrs := ' ' + LAttrs.Trim;
        Result := Format('<%s%s>%s', [FTagName.ToLower, LAttrs, FContent]);
      end;
    end;
  end;


var
  I:Integer;
  E:THtmlElement;
begin
  if IncludeSelf then
  begin
    FOrignal := GetSelfHtml;
    Sb.Append(FOrignal); // FOrignal
  end;
  for I := 0 to FChildren.Count - 1 do
  begin
    E := FChildren[I] as THtmlElement;
    E._GetHtml(True, Sb);
  end;
  if IncludeSelf and (FCloseTag <> nil) then
    (FCloseTag as THtmlElement)._GetHtml(True, Sb);
end;


procedure THtmlElement._GetText(IncludeSelf:Boolean; Sb:TStringBuilder);
var
  I:Integer;
  E:THtmlElement;
begin
  if IncludeSelf and (FTagName = cSpecialTagName_Text) then
  begin
    Sb.Append(FContent);
  end;
  for I := 0 to FChildren.Count - 1 do
  begin
    E := FChildren[I] as THtmlElement;
    E._GetText(True, Sb);
  end;
end;


procedure THtmlElement._Select(Item:PCSSSelectorItem; Count:Integer; r:TIHtmlElementList; OnlyTopLevel:Boolean);
  function _Filtered():Boolean;
  var
    I:Integer;
  begin
    Result := false;
    if (Item^.szTag = '') or (Item^.szTag = '*') or (Item^.szTag = FTagName) then
    begin
      for I := low(Item^.Attributes) to high(Item^.Attributes) do
        if not AttrCompareFuns[Item^.Attributes[I].AttrOperator](Item^.Attributes[I], Self) then
          Exit;
      Result := True;
    end;
  end;


var
  f:Boolean;
  I, SelfIndex:Integer;
  PE, E:THtmlElement;
  Next:PCSSSelectorItem;
begin
  // ShowMessage(item^.szTag);
  // ShowMessage(item^.Attributes[0].Key + ' ' + item^.Attributes[0].Value);
  f := _Filtered();
  if f then
  begin
    if (Count = 1) then
    begin
      if (r.IndexOf(Self as IHtmlElement) < 0) then
        r.Add(Self as IHtmlElement);
    end
    else if Count > 1 then
    begin
      Next := Item;
      Inc(Next);
      PE := Self.FOwner;
      if PE = nil then
        SelfIndex := -1
      else
        SelfIndex := PE.FChildren.IndexOf(Self as IHtmlElement);
      case Next^.Relation of
        sirDescendant, sirChildren:
          begin
            for I := 0 to FChildren.Count - 1 do
            begin
              E := FChildren[I] as THtmlElement;
              E._Select(Next, Count - 1, r, Next^.Relation = sirChildren);
            end;
          end;
        sirAllYoungerBrother, sirYoungerBrother:
          begin
            if (PE <> nil) and (SelfIndex >= 0) then
              for I := (SelfIndex + 1) to PE.FChildren.Count - 1 do
              begin
                E := PE.FChildren[I] as THtmlElement;
                if (Length(E.FTagName) = 0) or (E.FTagName[LowStrIndex] <> '#') then
                begin
                  E._Select(Next, Count - 1, r, True);
                  if (Next^.Relation = sirYoungerBrother) then
                    Break;
                end;
              end;
          end;
      end;
    end;
  end;
  if not OnlyTopLevel then
    for I := 0 to FChildren.Count - 1 do
    begin
      E := FChildren[I] as THtmlElement;
      E._Select(Item, Count, r);
    end;
end;


procedure THtmlElement._SimpleCSSSelector(const ItemGroup:TCSSSelectorItemGroup; r:TIHtmlElementList);
var
  I:Integer;
begin
  for I := low(ItemGroup) to high(ItemGroup) do
  begin
    _Select(@ItemGroup[I][0], Length(ItemGroup[I]), r);
  end;
end;


function THtmlElement.GetInnerHtml:String;
var
  Sb:TStringBuilder;
begin
  Sb := TStringBuilder.Create;
  _GetHtml(false, Sb);
  Result := Sb.ToString;
  Sb.Free;
end;


function THtmlElement.GetInnerText:String;
var
  Sb:TStringBuilder;
begin
  Sb := TStringBuilder.Create;
  _GetText(True, Sb);
  Result := Sb.ToString;
  Sb.Free;
end;


function THtmlElement.GetOrignal:String;
begin
  Result := FOrignal;
end;


function THtmlElement.GetOuterHtml:String;
var
  Sb:TStringBuilder;
begin
  Sb := TStringBuilder.Create;
  _GetHtml(True, Sb);
  Result := Sb.ToString;
  Sb.Free;
end;


function THtmlElement.GetParent:IHtmlElement;
begin
  Result := Self.FOwner;
end;


function THtmlElement.GetSourceColNum:Integer;
begin
  Result := FSourceCol;
end;


function THtmlElement.GetSourceLineNum:Integer;
begin
  Result := FSourceLine;
end;


function THtmlElement.GetTagName:String;
begin
  Result := FTagName;
end;


function THtmlElement.HasAttribute(AttributeName:String):Boolean;
begin
  Result := FAttributes.ContainsKey(LowerCase(AttributeName));
end;


function THtmlElement.RemoveChild(ANode:IHtmlElement):Integer;
begin
  Result := -1;
  if ANode.Parent = IHtmlElement(Self) then
  begin
    if Self.FChildren <> nil then
      FChildren.Remove(ANode);
  end;
end;


procedure THtmlElement.Remove;
begin
  if FOwner <> nil then
    FOwner.RemoveChild(Self);
end;


procedure THtmlElement.RemoveAttr(AAttrName:string);
begin
  FAttributes.Remove(LowerCase(AAttrName));
  if Assigned(FCloseTag) then
    FCloseTag.RemoveAttr(AAttrName);
end;


procedure THtmlElement.SetAttributes(Key:String; Value:String);
begin
  FAttributes.AddOrSetValue(LowerCase(Key), Value);
end;


procedure THtmlElement.SetInnerText(Value:String);
begin
  FContent := Value;
end;


procedure THtmlElement.SetTagName(Value:String);
begin
  FTagName := UpperCase(Value);
  if FCloseTag <> nil then
    FCloseTag.TagName := Self.FTagName;
end;


function THtmlElement.SimpleCSSSelector(const selector:String):IHtmlElementList;
var
  r:TIHtmlElementList;
begin
  r := TIHtmlElementList.Create;
  _SimpleCSSSelector(ParserCSSSelector(selector), r);
  Result := r as IHtmlElementList;
end;


function THtmlElement.Find(const selector:String):IHtmlElementList;
begin
  Result := SimpleCSSSelector(selector);
end;
{$IFDEF UseXPath}
// .....
function XPathToCSSSelector(const AXPath:string):string; forward;


function THtmlElement.FindX(const AXPath:String):IHtmlElementList;
begin
  Result := SimpleCSSSelector(XPathToCSSSelector(AXPath));
end;
{$ENDIF}


{ TSourceContext }
function TSourceContext.subStr(Index, Count:Integer):string;
begin
  Result := System.Copy(SourceCode, index{$IF (LowStrIndex = 0)} + 1{$ENDIF}, Count);
end;


function TSourceContext.subStr(Count:Integer):string;
begin
  Result := subStr(CharIndex, Count);
end;


function TSourceContext.ReadStr(UntilChars:TSysCharSet):string;
var
  oldIndex:Integer;
  stringChar:Char;
begin
  SkipBlank;
  oldIndex := CharIndex;
  if CharInSet(CurrentChar, ['"', '''']) then
    stringChar := CurrentChar
  else
    stringChar := #0;
  JumpToNextChar;
  while True do
  begin
    if stringChar = #0 then
    begin
      if CharInSet(CurrentChar, UntilChars) then
        Break;
    end
    else if (CurrentChar = stringChar) then
    begin
      JumpToNextChar;
      Break;
    end;
    JumpToNextChar;
  end;
  Result := subStr(oldIndex, CharIndex - oldIndex);
  if (stringChar <> #0) and (Length(Result) >= 2) then
    Result := System.Copy(Result, 2, Length(Result) - 2);
end;


function TSourceContext.GetCharOfCurrent(Index:Integer):Char;
begin
  Result := SourceCode[CharIndex + index];
end;


function TSourceContext.JumpToNextChar:Boolean;
begin
  // Edwin: 2020-05-06
  if PassedEndOfSourceCode then
    Exit(false);
  // End Edwin: 2020-05-06
  if CurrentChar = #10 then
  begin
    Inc(LineNum);
    ColNum := 1;
  end
  else
    Inc(ColNum);
  Inc(CharIndex);
  Result := True;
  if PassedEndOfSourceCode then
    CurrentChar := #0
  else
  begin
    CurrentChar := SourceCode[CharIndex];
    {$IFDEF DEBUG}
    currentCode := PChar(@SourceCode[CharIndex]);
    {$ENDIF}
  end;
end;


function TSourceContext.JumpToNextChar(Step:Integer):Integer;
var
  I:Integer;
begin
  Result := 0;
  for I := 0 to Step - 1 do
    if JumpToNextChar() then
      Inc(Result)
    else
      Break;
end;


function TSourceContext.PeekStr:string;
begin
  Result := PeekStr(CharIndex);
end;


procedure TSourceContext.setCode(const ACode:string);
begin
  CurrentChar := #0;
  SourceCode := ACode;
  LineNum := 1;
  ColNum := 1;
  CharIndex := low(SourceCode);
  if Length(ACode) > 0 then
  begin
    CurrentChar := SourceCode[CharIndex];
    {$IFDEF DEBUG}
    currentCode := PChar(@SourceCode[CharIndex]);
    {$ENDIF}
  end;
end;


function TSourceContext.PeekStr(Index:Integer):string;
var
  oldIndex:Integer;
begin
  Result := '';
  oldIndex := index;
  while not CharInSet(SourceCode[index], (WhiteSpace + ['/', '>'])) do
    Inc(index);
  Result := subStr(oldIndex, index - oldIndex);
end;


procedure TSourceContext.SkipBlank();
begin
  while CharInSet(CurrentChar, WhiteSpace) do
    JumpToNextChar();
end;


function TSourceContext.PassedEndOfSourceCode:Boolean;
begin
  Result := CharIndex > high(SourceCode);
end;

{$IFDEF UseXPath}
/// <summary>
/// The main reason to put it here is to distinguish the original code,
/// The conversion code is from python: https://github.com/santiycr/cssify/blob/master/cssify.py
/// </summary>
var
  Validation_re:TPerlRegEx;


function XPathToCSSSelector(const AXPath:string):string;
  function GetValue(AName:string):string;
  begin
    Result := Validation_re.Groups[Validation_re.NamedGroup(AName)];
  end;


var
  LPosition, LXPathLen:Integer;
  LNav, LTag, LAttr, LNth, LMattr, LMvalue, LNode_css:string;
begin
  Result := '';
  LPosition := 1;
  LXPathLen := Length(AXPath);
  while LPosition < LXPathLen do
  begin
    Validation_re.Subject := Copy(AXPath, LPosition, LXPathLen - LPosition + 1);
    if not Validation_re.Match then
      Exit;
    LNav := '';
    if LPosition <> 1 then
    begin
      LNav := ' ';
      if GetValue('nav') <> '//' then
        LNav := ' > ';
    end;
    LTag := '';
    if GetValue('tag') <> '*' then
      LTag := GetValue('tag');
    LAttr := '';
    if GetValue('idvalue') <> '' then
    begin
      LAttr := '#' + GetValue('idvalue').Replace(' ', '#');
    end
    else if GetValue('matched') <> '' then
    begin
      LMattr := GetValue('mattr');
      LMvalue := GetValue('mvalue');
      if LMattr = '@id' then
        LAttr := '#' + LMvalue.Replace(' ', '#')
      else if LMattr = '@class' then
        LAttr := '.' + LMvalue.Replace(' ', '.')
      else if (LMattr = 'text()') or (LMattr = '.') then
        LAttr := ':contains(^' + LMvalue + '$)'
      else
      begin
        if Pos(' ', LMvalue) <> 0 then
          LMvalue := '"' + LMvalue + '"';
        LAttr := Format('[%s=%s]"', [LMattr.Replace('@', ''), LMvalue]);
      end;
    end
    else if GetValue('contained') <> '' then
    begin
      LMattr := GetValue('cattr');
      LMvalue := GetValue('cvalue');
      if Pos('@', LMattr) > 0 then
        LAttr := Format('[%s*=%s]', [LMattr.Replace('@', ''), GetValue('cvalue')])
      else if GetValue('cattr') = 'text()' then
        LAttr := ':contains(' + GetValue('cvalue') + ')';
    end;
    LNth := GetValue('nth');
    if LNth <> '' then
      LNth := ':nth-of-type(' + LNth + ')';
    LNode_css := LNav + LTag + LAttr + LNth;
    Result := Result + LNode_css;
    Inc(LPosition, Validation_re.MatchedOffset + Validation_re.MatchedLength - 1);
  end;
end;
const
  uRegExPattern = '(?P<node>(^id\(["'']?(?P<idvalue>\s*[\w/:][-/\w\s,:;.]*)["'']?\)|' + '(?P<nav>//?)(?P<tag>([a-zA-Z][a-zA-Z0-9]{0,10}|\*))(\[((?P<matched>' + '(?P<mattr>@?[.a-zA-Z_:][-\w:.]*(\(\))?)=["''](?P<mvalue>\s*[\w/:]' +
    '[-/\w\s,:;.]*))["'']|(?P<contained>contains\((?P<cattr>@?[.a-zA-Z_:]' + '[-\w:.]*(\(\))?),\s*["''](?P<cvalue>\s*[\w/:][-/\w\s,:;.]*)' + '["'']\)))\])?(\[(?P<nth>\d)\])?))';


procedure InitRegExs;
begin
  Validation_re := TPerlRegEx.Create;
  Validation_re.Options := [preCaseLess, preMultiLine];
  Validation_re.RegEx := uRegExPattern;
  Validation_re.Compile;
end;
{$ENDIF UseXPath}


{ THtmlListEnumerator }
constructor THtmlListEnumerator.Create(AList:IHtmlElementList);
begin
  inherited Create;
  FIndex := -1;
  FList := AList;
end;


function THtmlListEnumerator.GetCurrent:IHtmlElement;
begin
  Result := FList[FIndex];
end;


function THtmlListEnumerator.MoveNext:Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;

initialization

{$IFDEF UseXPath}
  InitRegExs;
{$ENDIF}
Init;

finalization

UnInit;
{$IFDEF UseXPath}
if Validation_re <> nil then
  Validation_re.Free;
{$ENDIF}

end.
