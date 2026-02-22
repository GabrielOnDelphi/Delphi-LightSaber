unit HTMLParserEx.Tests.Main;

interface

uses
  TestFramework,
  HtmlParserEx;


type

  TestHTMLParserEx = class(TTestCase)
  published
    procedure TestRoot_EmptyString;
    procedure TestRoot_BasicHTML;
  end;

implementation


procedure TestHTMLParserEx.TestRoot_EmptyString;
var
  iRoot:IHtmlElement;
begin
  iRoot := ParserHTML('');
  CheckEquals('#DOCUMENT', iRoot.TagName, 'TagName should be #DOCUMENT');
  CheckEquals(0, iRoot.ChildrenCount, 'ChildrenCount should be 0');
  CheckEquals(1, iRoot.Find('*').Count, 'Find result should be 1');
{$IFDEF UseXPath}
  CheckEquals(1, iRoot.FindX('//*').Count, 'FindX result should be 1');
{$ENDIF}
  CheckFalse(Assigned(iRoot.Parent), 'Parent should be nil');
  CheckFalse(Assigned(iRoot.CloseTag), 'CloseTag should be nil');

  CheckEquals('', iRoot.InnerHtml, 'InnerHtml should be empty string');
  CheckEquals('', iRoot.InnerText, 'InnerText should be empty string');
  CheckEquals('', iRoot.OuterHtml, 'OuterHtml should be empty string');
  CheckEquals('', iRoot.Text, 'Text should be empty string');
  CheckEquals('', iRoot.Content, 'Content should be empty string');
  CheckEquals('', iRoot.Orignal, 'Orignal should be empty string');
  CheckEquals(0, iRoot.SourceLineNum, 'SourceLineNum should be 0');
  CheckEquals(0, iRoot.SourceColNum, 'SourceColNum should be 0');
end;


procedure TestHTMLParserEx.TestRoot_BasicHTML;
const
  _DOC = '<html><head><title>Hello </title></head><body><div id="div1"> World</div><div id="div2">!</div></body></html>';
var
  iRoot:IHtmlElement;
begin
  iRoot := ParserHTML(_DOC);

  CheckEquals('#DOCUMENT', iRoot.TagName, 'TagName should be #DOCUMENT');
  CheckEquals(1, iRoot.ChildrenCount, 'ChildrenCount should be 1');
  CheckEquals('HTML', iRoot.Children[0].TagName, 'Children[0] Tag should be HTML');

  CheckEquals(2, iRoot.Children[0].Childrencount, 'Child[0].ChildrenCount should be 2');
  CheckEquals('HEAD', iRoot.Children[0].Children[0].TagName, 'Child/Child[0] Tag should be HEAD');
  CheckEquals('BODY', iRoot.Children[0].Children[1].TagName, 'Child/Child[1] Tag should be BODY');
  CheckEquals(2, iRoot.Children[0].Children[1].ChildrenCount, 'Child/Child[1] count should be 2');
  CheckEquals('DIV', iRoot.Children[0].Children[1].Children[0].TagName, 'First tag should be DIV');
  CheckTrue(iRoot.Children[0].Children[1].Children[0].HasAttribute('id'), 'first div has id=div1');
  CheckEquals('div1', iRoot.Children[0].Children[1].Children[0].Attributes['id'], 'first div has id=div1');
  CheckEquals('DIV', iRoot.Children[0].Children[1].Children[1].TagName, 'Second tag should be DIV');
  CheckTrue(iRoot.Children[0].Children[1].Children[1].HasAttribute('id'), 'second div has id=div1');
  CheckEquals('div2', iRoot.Children[0].Children[1].Children[1].Attributes['id'], 'second div has id=div1');

  CheckEquals(1, iRoot.Find('title').Count, 'Find title should be 1');
  CheckEquals('Hello ', iRoot.Find('title').Text, 'Title text mismatch');
  CheckEquals(1, iRoot.Find('body').Count, 'Find body should be 1');
  CheckEquals(' World!', iRoot.Find('body').Text, 'Body text mismatch');
{$IFDEF UseXPath}
  CheckEquals(1, iRoot.FindX('//title').Count, 'FindX title result should be 1');
  CheckEquals(1, iRoot.FindX('//body').Count, 'FindX body result should be 1');
  CheckEquals(2, iRoot.FindX('/html/body/div').Count, 'FindX html/body/div result should be 2');
  CheckEquals(2, iRoot.FindX('//div').Count, 'FindX //div result should be 2');
{$ENDIF}
  CheckFalse(Assigned(iRoot.Parent), 'Parent should be nil');
  CheckFalse(Assigned(iRoot.CloseTag), 'CloseTag should be nil');

  CheckEquals(_DOC, iRoot.InnerHtml, 'InnerHtml should match input for root element');
  CheckEquals('Hello  World!', iRoot.InnerText, 'InnerText should be title text and text within div');
  CheckEquals(_DOC, iRoot.OuterHtml, 'OuterHtml should match input for root element');
  CheckEquals('Hello  World!', iRoot.Text, 'Text should be title text and text within div');
  CheckEquals('', iRoot.Content, 'Content should be empty string');
  CheckEquals('', iRoot.Orignal, 'Orignal should be empty string');
  CheckEquals(0, iRoot.SourceLineNum, 'SourceLineNum should be 0');
  CheckEquals(0, iRoot.SourceColNum, 'SourceColNum should be 0');
end;


initialization
RegisterTest(TestHTMLParserEx.Suite);

end.
