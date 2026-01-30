unit Test.LightCore.HTML;

{=============================================================================================================
   2026.01.30
   Unit tests for LightCore.HTML
   Tests HTML parsing, tag extraction, URL manipulation

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes;

type
  [TestFixture]
  TTestLightCoreHTML = class
  public
    { Body Extraction Tests }
    [Test]
    procedure TestGetBodyFromHtml;

    [Test]
    procedure TestGetBodyFromHtml_Empty;

    { Tag Stripping Tests }
    [Test]
    procedure TestStripAllTags;

    [Test]
    procedure TestStripAllTags_NoTags;

    [Test]
    procedure TestStripAllTags_NestedTags;

    { Line Extraction Tests }
    [Test]
    procedure TestExtractLine;

    { Quote Position Tests }
    [Test]
    procedure TestFindQuoteStart_DoubleQuotes;

    [Test]
    procedure TestFindQuoteStart_SingleQuotes;

    [Test]
    procedure TestFindQuoteEnd;

    { Attribute Extraction Tests }
    [Test]
    procedure TestExtractAttribValue_Href;

    [Test]
    procedure TestExtractAttribValue_Src;

    [Test]
    procedure TestExtractAttribValue_SingleQuotes;

    [Test]
    procedure TestExtractAttribValue_NotFound;

    [Test]
    procedure TestExtractAttribValueIE;

    { Attribute Replacement Tests }
    [Test]
    procedure TestReplaceAttribValue;

    { Tag Data Extraction Tests }
    [Test]
    procedure TestExtractTagsData;

    [Test]
    procedure TestExtractTagsData_Multiple;

    { URL Extraction Tests }
    [Test]
    procedure TestExtractAhrefTags;

    [Test]
    procedure TestExtractAhrefTags_Multiple;

    [Test]
    procedure TestExtractURLs;

    [Test]
    procedure TestExtractURLsText;

    { Link Analysis Tests }
    [Test]
    procedure TestLinkHasNoFollow_True;

    [Test]
    procedure TestLinkHasNoFollow_False;

    [Test]
    procedure TestLinkOpensInNewWind_True;

    [Test]
    procedure TestLinkOpensInNewWind_False;

    { URL Manipulation Tests }
    [Test]
    procedure TestMakeLinkRelativeToRoot;

    [Test]
    procedure TestColapseUrlDots;

    { Meta/Title Tests }
    [Test]
    procedure TestLineIsMeta_True;

    [Test]
    procedure TestLineIsMeta_False;

    [Test]
    procedure TestLineIsTitle_True;

    [Test]
    procedure TestLineIsTitle_False;

    { Sanitization Tests }
    [Test]
    procedure TestSanitizeText;

    [Test]
    procedure TestIsSafeHtmlChar;

    { HTML Generation Tests }
    [Test]
    procedure TestGenerateHTMLHeader;
  end;

implementation

uses
  LightCore.HTML;

{ Body Extraction Tests }

procedure TTestLightCoreHTML.TestGetBodyFromHtml;
var
  HTML, Body: string;
begin
  HTML:= '<html><head><title>Test</title></head><body>Content here</body></html>';
  Body:= GetBodyFromHtml(HTML);
  Assert.AreEqual('Content here', Body);
end;

procedure TTestLightCoreHTML.TestGetBodyFromHtml_Empty;
var
  HTML, Body: string;
begin
  HTML:= '<html><head></head><body></body></html>';
  Body:= GetBodyFromHtml(HTML);
  Assert.AreEqual('', Body);
end;

{ Tag Stripping Tests }

procedure TTestLightCoreHTML.TestStripAllTags;
var
  Result: string;
begin
  Result:= StripAllTags('<p>Hello <b>World</b></p>');
  Assert.AreEqual('Hello World', Result);
end;

procedure TTestLightCoreHTML.TestStripAllTags_NoTags;
var
  Result: string;
begin
  Result:= StripAllTags('Plain text');
  Assert.AreEqual('Plain text', Result);
end;

procedure TTestLightCoreHTML.TestStripAllTags_NestedTags;
var
  Result: string;
begin
  Result:= StripAllTags('<div><p><span>Nested</span></p></div>');
  Assert.AreEqual('Nested', Result);
end;

{ Line Extraction Tests }

procedure TTestLightCoreHTML.TestExtractLine;
var
  HTML, Line: string;
begin
  HTML:= '<a href="demo"><meta data="img" content="http://example.com/img.jpg"><img src="stuff">';
  Line:= ExtractLine(HTML, '<meta');
  Assert.AreEqual('<meta data="img" content="http://example.com/img.jpg">', Line);
end;

{ Quote Position Tests }

procedure TTestLightCoreHTML.TestFindQuoteStart_DoubleQuotes;
var
  Pos: Integer;
begin
  Pos:= FindQuoteStart('<a href="www.example.com">', 1);
  Assert.AreEqual(9, Pos);
end;

procedure TTestLightCoreHTML.TestFindQuoteStart_SingleQuotes;
var
  Pos: Integer;
begin
  Pos:= FindQuoteStart('<a href=''www.example.com''>', 1);
  Assert.AreEqual(9, Pos);
end;

procedure TTestLightCoreHTML.TestFindQuoteEnd;
var
  Pos: Integer;
begin
  Pos:= FindQuoteEnd('<a href="www.example.com">', 10);  { Start after opening quote }
  Assert.AreEqual(25, Pos);
end;

{ Attribute Extraction Tests }

procedure TTestLightCoreHTML.TestExtractAttribValue_Href;
var
  Value: string;
begin
  Value:= ExtractAttribValue('<a href="http://example.com">', 'href');
  Assert.AreEqual('http://example.com', Value);
end;

procedure TTestLightCoreHTML.TestExtractAttribValue_Src;
var
  Value: string;
begin
  Value:= ExtractAttribValue('<img src="image.jpg" alt="test">', 'src');
  Assert.AreEqual('image.jpg', Value);
end;

procedure TTestLightCoreHTML.TestExtractAttribValue_SingleQuotes;
var
  Value: string;
begin
  Value:= ExtractAttribValue('<a href=''http://example.com''>', 'href');
  Assert.AreEqual('http://example.com', Value);
end;

procedure TTestLightCoreHTML.TestExtractAttribValue_NotFound;
var
  Value: string;
begin
  Value:= ExtractAttribValue('<a href="test">', 'class');
  Assert.AreEqual('', Value);
end;

procedure TTestLightCoreHTML.TestExtractAttribValueIE;
var
  Value: string;
begin
  { IE doesn't always put quotes around attribute values }
  Value:= ExtractAttribValueIE('<a target=_blank>', 'target');
  Assert.AreEqual('_blank', Value);
end;

{ Attribute Replacement Tests }

procedure TTestLightCoreHTML.TestReplaceAttribValue;
var
  Tag, Result: string;
begin
  Tag:= '<a href="old-url.com">';
  Result:= ReplaceAttribValue(Tag, 'href', 'new-url.com');
  Assert.AreEqual('<a href="new-url.com">', Result);
end;

{ Tag Data Extraction Tests }

procedure TTestLightCoreHTML.TestExtractTagsData;
var
  TSL: TStringList;
begin
  TSL:= ExtractTagsData('<div>content</div>', 'div');
  try
    Assert.AreEqual(1, TSL.Count);
    Assert.AreEqual('content', TSL[0]);
  finally
    FreeAndNil(TSL);
  end;
end;

procedure TTestLightCoreHTML.TestExtractTagsData_Multiple;
var
  TSL: TStringList;
begin
  TSL:= ExtractTagsData('<div>first</div><div>second</div>', 'div');
  try
    Assert.AreEqual(2, TSL.Count);
    Assert.AreEqual('first', TSL[0]);
    Assert.AreEqual('second', TSL[1]);
  finally
    FreeAndNil(TSL);
  end;
end;

{ URL Extraction Tests }

procedure TTestLightCoreHTML.TestExtractAhrefTags;
var
  TSL: TStringList;
begin
  TSL:= ExtractAhrefTags('<a href="http://example.com">Link</a>');
  try
    Assert.AreEqual(1, TSL.Count);
    Assert.IsTrue(Pos('href', TSL[0]) > 0);
  finally
    FreeAndNil(TSL);
  end;
end;

procedure TTestLightCoreHTML.TestExtractAhrefTags_Multiple;
var
  TSL: TStringList;
begin
  TSL:= ExtractAhrefTags('<a href="url1">L1</a><a href="url2">L2</a>');
  try
    Assert.AreEqual(2, TSL.Count);
  finally
    FreeAndNil(TSL);
  end;
end;

procedure TTestLightCoreHTML.TestExtractURLs;
var
  TSL: TStringList;
begin
  TSL:= ExtractURLs('<a href="http://example.com">Link</a>');
  try
    Assert.AreEqual(1, TSL.Count);
    Assert.AreEqual('http://example.com', TSL[0]);
  finally
    FreeAndNil(TSL);
  end;
end;

procedure TTestLightCoreHTML.TestExtractURLsText;
var
  TSL: TStringList;
  Count: Integer;
begin
  TSL:= TStringList.Create;
  try
    Count:= ExtractURLsText('Visit www.example.com or http://test.com for more.', TSL);
    Assert.AreEqual(2, Count);
    Assert.AreEqual(2, TSL.Count);
  finally
    FreeAndNil(TSL);
  end;
end;

{ Link Analysis Tests }

procedure TTestLightCoreHTML.TestLinkHasNoFollow_True;
begin
  Assert.IsTrue(LinkHasNoFollow('<a href="url" rel="nofollow">'));
end;

procedure TTestLightCoreHTML.TestLinkHasNoFollow_False;
begin
  Assert.IsFalse(LinkHasNoFollow('<a href="url">'));
  Assert.IsFalse(LinkHasNoFollow('<a href="url" rel="follow">'));
end;

procedure TTestLightCoreHTML.TestLinkOpensInNewWind_True;
begin
  Assert.IsTrue(LinkOpensInNewWind('<a href="url" target="_blank">'));
end;

procedure TTestLightCoreHTML.TestLinkOpensInNewWind_False;
begin
  Assert.IsFalse(LinkOpensInNewWind('<a href="url">'));
  Assert.IsFalse(LinkOpensInNewWind('<a href="url" target="_self">'));
end;

{ URL Manipulation Tests }

procedure TTestLightCoreHTML.TestMakeLinkRelativeToRoot;
var
  Result: string;
begin
  { Absolute URL should stay unchanged }
  Result:= MakeLinkRelativeToRoot('/x/master/index.html', 'http://example.com');
  Assert.AreEqual('http://example.com', Result);

  { Empty link should stay empty }
  Result:= MakeLinkRelativeToRoot('/x/master/index.html', '');
  Assert.AreEqual('', Result);
end;

procedure TTestLightCoreHTML.TestColapseUrlDots;
begin
  { ColapseUrlDots does naive string replacement of /../ and ../ with /
    It does NOT perform proper URL path resolution }
  Assert.AreEqual('http://example.com/page', ColapseUrlDots('http://example.com/../page'));
  { Note: /a/b/../b becomes /a/b/b (naive replacement), not /a/b (proper resolution) }
  Assert.AreEqual('http://example.com/a/b/b', ColapseUrlDots('http://example.com/a/b/../b'));
end;

{ Meta/Title Tests }

procedure TTestLightCoreHTML.TestLineIsMeta_True;
begin
  Assert.IsTrue(LineIsMeta('<meta name="Keywords" content="test">', 'Keywords'));
  Assert.IsTrue(LineIsMeta('<meta name="description" content="test desc">', 'description'));
end;

procedure TTestLightCoreHTML.TestLineIsMeta_False;
begin
  Assert.IsFalse(LineIsMeta('<p>Not a meta tag</p>', 'Keywords'));
  Assert.IsFalse(LineIsMeta('<meta name="other" content="test">', 'Keywords'));
end;

procedure TTestLightCoreHTML.TestLineIsTitle_True;
begin
  Assert.IsTrue(LineIsTitle('<title>My Page Title</title>'));
end;

procedure TTestLightCoreHTML.TestLineIsTitle_False;
begin
  Assert.IsFalse(LineIsTitle('<p>Not a title</p>'));
  Assert.IsFalse(LineIsTitle('<title>Incomplete'));
end;

{ Sanitization Tests }

procedure TTestLightCoreHTML.TestSanitizeText;
var
  Result: UTF8String;
begin
  { Spaces should become + }
  Result:= SanitizeText('hello world');
  Assert.IsTrue(Pos('+', string(Result)) > 0);

  { Safe characters should stay unchanged }
  Result:= SanitizeText('abc123');
  Assert.AreEqual(UTF8String('abc123'), Result);
end;

procedure TTestLightCoreHTML.TestIsSafeHtmlChar;
begin
  { Alphanumeric are safe }
  Assert.IsTrue(IsSafeHtmlChar(Ord('a')));
  Assert.IsTrue(IsSafeHtmlChar(Ord('Z')));
  Assert.IsTrue(IsSafeHtmlChar(Ord('5')));

  { Special safe chars }
  Assert.IsTrue(IsSafeHtmlChar(Ord('!')));
  Assert.IsTrue(IsSafeHtmlChar(Ord('_')));
  Assert.IsTrue(IsSafeHtmlChar(Ord('-')));
  Assert.IsTrue(IsSafeHtmlChar(Ord('.')));

  { Unsafe chars }
  Assert.IsFalse(IsSafeHtmlChar(Ord(' ')));
  Assert.IsFalse(IsSafeHtmlChar(Ord('<')));
  Assert.IsFalse(IsSafeHtmlChar(Ord('>')));
end;

{ HTML Generation Tests }

procedure TTestLightCoreHTML.TestGenerateHTMLHeader;
var
  Header: string;
begin
  Header:= GenerateHTMLHeader('Test Title', 'Description', 'key1,key2', 'style.css');
  Assert.IsTrue(Pos('<title>Test Title</title>', Header) > 0);
  Assert.IsTrue(Pos('Description', Header) > 0);
  Assert.IsTrue(Pos('key1,key2', Header) > 0);
  Assert.IsTrue(Pos('style.css', Header) > 0);
  Assert.IsTrue(Pos('<body>', Header) > 0);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestLightCoreHTML);

end.
