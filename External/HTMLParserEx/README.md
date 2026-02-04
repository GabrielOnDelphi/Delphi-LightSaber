# HTMLParserEx

Delphi HTML Parser

April 28, 2022 Forked from https://github.com/edwinyzh/htmlparser
Translated Chinese strings/comments to English via Google Translate

---
The code is modified from [HtmlParser](http://www.raysoftware.cn/?p=370) of the original wr960204.
Because of my own needs, I need to modify the html, but only support the read operation, so on this basis Modified and named HtmlParserEx.pas to distinguish it.


#### use

```delphi
// Load example from file
procedure Test;
var
   LHtml: IHtmlElement;
   LList: IHtmlElementList;
   LStrStream: TStringStream;
begin
   LStrStream := TStringStream.Create('', TEncoding.UTF8);
   try
     LStrStream.LoadFromFile('view-source_https___github.com_ying32_htmlparser.html');
     LHtml := ParserHTML(LStrStream.DataString);
     if LHtml <> nil then
     begin
       LList := LHtml.SimpleCSSSelector('a');
       for LHtml in LList do
         Writeln('url:', lhtml.Attributes['href']);
     end;
   finally
     LStrStream.Free;
   end;
end;
````

#### Modify records
ying32 modification record:
Email:1444386932@qq.com

May 4, 2017

 1. Remove the reference to the RegularExpressions unit, no longer use TRegEx and use TPerlRegEx in the RegularExpressionsCore unit

April 19, 2017

1. Add the compilation instruction "UseXPath" that uses the XPath function. By default, XPath is not used. Personally, it is useless.
 
November 23, 2016

1. Simple support for XPath, simple, use xpath to css selector, hey
     The code of xpath conversion is changed from [python version](https://github.com/santiycr/cssify/blob/master/cssify.py)

> IHtmlElement

```delphi  

  LHtml.FindX('/html/head/title').Each(
    procedure(AIndex: Integer; AEl: IHtmlElement) 
    begin
      Writeln('xpath index=', AIndex, ',  a=', AEl.Text);  
    end
  );

```  
   
November 15, 2016


>
Changes to IHtmlElement and THtmlElement:
   1. Add the Set method to the Attributes attribute
   2. Add Set method to TagName property
   3. Add Parent property
   4. Add RemoveAttr method
   5. Add Remove method
   6. Add RemoveChild method
   7. Add the Find method, which is another name for SimpleCSSSelector
   8. _GetHtml no longer directly attaches the value of the FORignal attribute, but uses GetSelfHtml to re-assign the modified element and update the value of FORignal
   9. Add the Text property
   10. Modify InnerText and Text properties to add write function
   11. Add the AppliedChild method
>
Changes to IHtmlElementList and THtmlElementList:
   1. Add RemoveAll method
   2. Add the Remove method
   3. Add the Each method
   4. Add the Text property

#### Some usages of the modified new functions

> IHtmlElement  

```delphi  
// modify properties
      EL.Attributes['class'] := 'xxxx';
      // modify the tag
      EL.TagName = 'a';
      // remove myself
      EL.Remove;
      // remove child nodes
      EL.RemoveChild(El2);
      // css selector search, simplified use
      El.Find('a');
      // append a new element
      el2 := El.AppendChild('a');
     
     
```  

> IHtmlElementList  

```delphi  

// remove the selected element
   LHtml.Find('a').RemoveAll;

   // find and iterate
   LHtml.Find('a').Each(
     procedure(AIndex: Integer; AEl: IHtmlElement)
     begin
       Writeln('Index=', AIndex, ', href=', AEl.Attributes['href']);
     end);

   // Direct output, only the first element selected
   Writeln(LHtml.Find('title').Text);
```  
