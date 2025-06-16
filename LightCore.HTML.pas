UNIT LightCore.HTML;

//This hould be moved to core

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2023.06
   www.GabrielMoraru.com
   See Copyright file

   HTML Parsing

   Also see: c:\MyProjects\Packages\Third party packages\uHTMLBuilder.pas
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
   System.SysUtils, // msHtml,
   System.AnsiStrings, System.StrUtils, System.Classes, System.Math,
   LightCore.StringList, LightCore;

{--------------------------------------------------------------------------------------------------
   HTML PARSING
--------------------------------------------------------------------------------------------------}
 function  GetBodyFromHtml      (CONST AHTML: string): string;                                      { Get the HTML code contained between the <Body> tags }
 function  GenerateHTMLHeader   (CONST Title, MetaDescription, Keywords, CssFile: string): string;  { Old name: GenerateStandardHTMLHeader }

 { Tags }
 function  FindQuoteStart       (CONST HtmlTag: string; StartAt: Integer): Integer;
 function  FindQuoteEnd         (CONST HtmlTag: string; StartAt: Integer): Integer;

 { Tags - Extract }
 function  ExtractLine          (CONST HtmlBody, LineStart: string): string;
 function  StripAllTags         (CONST S: string): string;                                          { Remove all HTML / XML type tags from a string of text. }
 function  ExtractTagsData      (CONST HtmlBody: string; TagName: string): TStringList;             {   Example: for '<div>xxx</div>' it returns xxx }
 {}
 function  ReplaceAttribValue   (HtmlTag, Attrib, NewValue: string): string;
 function  ExtractAttribValue   (CONST HtmlTag, Attrib: string): string;                            //old name: ExtractTagValue?     { Extract the value of a user defined Attrib from a single HTML line (if you provide an entire HTML body then the function will only return the first Attrib found). }
 function  ExtractAttribValueIE (CONST HtmlTag, Attrib: string): string;                            { This fixes: IE nu pune quotes arround 'blank'. Example:  target=_blank }

 { URLs }
 function  ExtractAhrefTags     (CONST HtmlBody: string): TStringList;                              { Parse HTML file and extract <A HRef> tag. Return a list of TAGS }
 function  ExtractURLs          (CONST HtmlBody: string): TStringList;                              { Returns a list of links (that start with http://) }
 function  ExtractURLsText      (Text    : string; AdreseExtrase: TStringlist): integer;            { Extracts URLs from a TEXT. It searches the 'www' and 'http' strings }

 {}
 function  LinkHasNoFollow      (CONST Link: string): Boolean;
 function  LinkOpensInNewWind   (CONST Link: string): Boolean;

 { URL manipulation }
 function  MakeLinkRelativeToRoot  (CONST MasterPageUrl, Link: string): string;                     { Rewrites an URLs located in the MasterPage (/x/master/index.html) that points to 'slave' to look like this... }
 function  MakeLinksRelativeToRoot (CONST HTMLBody,   HtmlUrl: string): string;
 function  ColapseUrlDots          (sURL: string): string;

 { Also see LightCore.ExtractTex tBetween }
 function  LineIsMeta      (CONST HtmlLine, MetaName: string): Boolean;                             { Returns true if this line is the specified 'meta'. For example: <meta name="Keywords" content="">  }
 function  LineIsTitle     (CONST HtmlLine: string): Boolean;

 {}
 function  SanitizeText    (CONST URL: string): UTF8String;                                         { Encode a string into a URL query string parameter (as per web forms).  One would expect that Indy contains well-tested functions to handle this. Well, Indy contains some functions to help with this, but they may not work quite as you expect.  In fact, they may not be much use at all. }
 function  IsSafeHtmlChar  (CONST Ch: Integer): Boolean;

 { Formattings }
 function  FixHtmlFormatings(Body: TStringList): string; overload;
 procedure FixHtmlFormatings(CONST FileName: string);  overload;




IMPLEMENTATION

USES //LightVcl.Internet, LightCore.Internet,
     LightCore.IO, LightCore.Internet, LightCore.TextFile;




{-------------------------------------------------------------------------------------------------------------
 Fixes HTML lines that are "broken" in two parts back to single-line
  Example:
    <p>&nbsp;
    </p>
  is put back to:
     <p>&nbsp;</p>
-------------------------------------------------------------------------------------------------------------}
function FixHtmlFormatings(Body: TStringList): string;
VAR
   i: Integer;
   s, s2: string;
begin
 Result:= '';
 for i:= 0 to Body.Count- 1 DO
  begin
   s:= Body[i];

   if s.Length < 3 then
    begin
     Result:= Result+ s+ CRLF;
     Continue;
    end;

   s2:= Copy(s, 1, 4);
   if (s2= '</a>')
   OR (s2= '</p>')
   OR (s2= '</li')
   OR (s2= '</sp')
   OR (s2= '</td')
   OR (s2= '</h1')
   OR (s2= '</h2')
   OR (s2= '</h3')
   OR (s2= '</h4')
   OR (s2= '</h5')
   OR (s2= '</ti')
   then Result:= RemoveLastEnter(Result)+ s+ CRLF
   else Result:= Result+ s+ CRLF;
  end;

 Result:= RemoveLastEnter(Result);
end;


procedure FixHtmlFormatings(CONST FileName: string);
VAR
   Text: TStringList;
   s: string;
begin
 Text:= StringFromFileTSL(FileName);

 TRY
  s:= FixHtmlFormatings(Text);
 FINALLY
  FreeAndNil(Text);
 END;

 StringToFile(BackupFileIncrement(FileName), s, woOverwrite);
end;





function StripAllTags(const S: string): string; // A Delphi function to remove all HTML / XML type tags from a string of text.
var
  Len: Integer;

  function ReadUntil(const ReadFrom: Integer; const C: Char): Integer;
  var
    j: Integer;
  begin
    for j := ReadFrom to Len do
      if (s[j] = C) then
      begin
        Result := j;
        Exit;
      end;
    Result := Len+1;
  end;

var
  i, APos: Integer;
begin
  Len := Length(S);
  i := 0;
  Result := '';
  while (i <= Len) do
  begin
    Inc(i);
    APos := ReadUntil(i, '<');
    Result := Result + Copy(S, i, APos-i);
    i := ReadUntil(APos+1, '>');
  end;
end;


{ From a HTML body that is obfuscated/wrapped, extract the HTML tag that starts with LineStart
  Example:
     Extract only the middle tag ( the one that starts with '<meta').
     EctractLine(HTMLBody, '<meta')
        returns: <meta data="img" content="http://images.unsplash.com/img.jpg">

     <a href= "demo"><meta data="img" content="http://images.unsplash.com/img.jpg"><img href= "stuff">}
function ExtractLine(CONST HtmlBody, LineStart: string): string;
begin
 Result:= CopyFromTo(HtmlBody, LineStart, '>', TRUE);
end;



{--------------------------------------------------------------------------------------------------
                                HTML MANIPULATION
--------------------------------------------------------------------------------------------------}
function GetBodyFromHtml(CONST AHTML: string): string;                                                   { Get the HTML code contained between the <Body> tags }
CONST
  BODY_OPEN_TAG  = '<body>';
  BODY_CLOSE_TAG = '</body>';
begin
 Result:= system.COPY(AHTML, Pos(BODY_OPEN_TAG, LowerCase(AHTML)) + Length(BODY_OPEN_TAG), Length(AHTML));
 Delete (Result, Pos(BODY_CLOSE_TAG, result), Length(Result));
end;










function GenerateHTMLHeader(CONST Title, MetaDescription, Keywords, CssFile: string): string;
begin
 Result:=
  '<HTML><HEAD>'+ CRLF
 +'<title>'+ Title+'</title>'+ CRLF+CRLF
 +'<meta name="description" content="'+ MetaDescription+'" />'+ CRLF
 +'<meta name="Keywords"    content="'+ Keywords+'" />'+ CRLF

 +' <meta name="ROBOTS"               content="NOODP, ALL" />'+ CRLF
 +' <meta http-equiv="Content-Type"   content="text/html; charset=iso-8859-1" />'+ CRLF
 +' <meta http-equiv="Page-Enter"     content="blendTrans(Duration=0.4)" />'+ CRLF
 +' <meta http-equiv="Page-Exit"      content="blendTrans(Duration=0.4)" />'+ CRLF;

 if CssFile<> ''
 then Result:= Result+ ' <link href="'+ CssFile+ '" rel="stylesheet" type="text/css" />'+ CRLF;

 Result:= Result+ ' <link rel="shortcut icon"         HREF ="favicon.ico" />'+ CRLF
                + '</HEAD>'+ CRLF
                + '<body>' + CRLF;
end;











{--------------------------------------------------------------------------------------------------
   URL TEXT MANIPULATION
--------------------------------------------------------------------------------------------------}


{ Rewrites an URLs located in the MasterPage (/x/master/index.html) that points to 'slave' to look like this:
    orig page           /x/master/index.html
    link before:        /x/slave/index2.html
    link after :   ../../x/slave/index2.html

    TODO:
    Ideal it should like this:
                   ../slave/index2.html

 Tech details about absolute/relative links:
     https://stackoverflow.com/questions/5559578/having-links-relative-to-root }
function MakeLinkRelativeToRoot(CONST MasterPageUrl, Link: string): string;
VAR
   i, Depth: Integer;
begin
  if (Link = '')
  OR CheckHttpStart(Link)                              { URL is complete. Leave it like this }
  OR NOT FirstCharIs(Link, '/')                        { URL is relative to document. Leave it like this }
  then Result:= Link
  else
   begin
    Result:= '';
    Depth:= CountAppearance('/', UrlRemoveHttp(MasterPageUrl))-1;

    for i:= 1 to Depth DO
      Result:= Result+ '../';

    Result:= RemoveLastChar(Result);
    Result:= Result+ Link;
   end;
end;


{ Same as MakeLinkRelativeToRoot but applies to a whole HTML document

  HTML Body is the content of the page that needs processed
  HTML URL  is the location of server of that page }
function MakeLinksRelativeToRoot(CONST HTMLBody, HtmlUrl: string): string;
VAR
   Tag, url: string;
   Offset, iEnd, iStart1, iStart2: Integer;
begin
 Result:= HTMLBody;

 { Process <a> tags }
 Offset:= 1;
 REPEAT
   iStart1:= PosEx('<a ', Result, Offset);

   if iStart1> 0 then
    begin
     iStart2:= PosEx(' href', Result, iStart1+2);
     if iStart2> 0
     then
      begin
        iEnd:= PosEx('>', Result, iStart1+7);

        if (iEnd> 0) AND (iStart2< iend)
        then
         begin
          Tag:= LightCore.CopyTo(Result, iStart1, iEnd);

          URL:= ExtractAttribValue(Tag, 'href');
          URL:= MakeLinkRelativeToRoot(HtmlUrl, URL);

          { Put the new URL back into the tag }
          Tag:= ReplaceAttribValue(Tag, 'href', URL);
          offset:= iEnd+1;  {todo-optimization: here instead of 1 I should use the number of extra characters added (for example '../../../' means that I inserted 9 extra chars }

          Result:= System.Copy(Result, 1, iStart1-1) + Tag + Copy(Result, iEnd+1, MaxInt);
         end
        else
         Offset:= iStart1+ 3;
      end
     else Offset:= iStart1+ 3;
    end
   else Offset:= Length(Result);
 UNTIL Offset>= Length(Result);


 { Process <img> tags }
 Offset:= 1;
 REPEAT
   iStart1:= PosEx('<img ', Result, Offset);

   if iStart1> 0 then
    begin
     iStart2:= PosEx(' src', Result, iStart1+2);
     if iStart2> 0
     then
      begin
        iEnd:= PosEx('>', Result, iStart1+6);

        if (iEnd> 0) AND (iStart2< iend)
        then
         begin
          Tag:= LightCore.CopyTo(Result, iStart1, iEnd);

          URL:= ExtractAttribValue(Tag, 'src');
          URL:= MakeLinkRelativeToRoot(HtmlUrl, URL);

          { Put the new URL back into the tag }
          Tag:= ReplaceAttribValue(Tag, 'src', URL);
          offset:= iEnd+1;  {todo-optimization: here instead of 1 I should use the number of extra characters added (for example '../../../' means that I inserted 9 extra chars }

          Result:= System.Copy(Result, 1, iStart1-1) + Tag + Copy(Result, iEnd+1, MaxInt);
         end
        else
         Offset:= iStart1+ 3;
      end
     else Offset:= iStart1+ 3;
    end
   else Offset:= Length(Result);
 UNTIL Offset>= Length(Result);
end;

















{--------------------------------------------------------------------------------------------------
   FIND QUOTE SIGN POSITION

   Returns the opening quote and closing quote position.
   The search starts from StartAt

   Example:
         FindQuoteStart('<a href ="www.x.com/img.jpg"', 1) returns 10.
--------------------------------------------------------------------------------------------------}
function FindQuoteStart(CONST HtmlTag: string; StartAt: Integer): Integer;
VAR iStart1, iStart2: Integer;
begin
 { We don't know if ' or " is used so we look for both }
 iStart1:= PosEx('"' , HtmlTag, StartAt);
 iStart2:= PosEx('''', HtmlTag, StartAt);

 if iStart1 = 0
 then Result:= iStart2
 else if iStart2= 0
      then Result:= iStart1
      else Result:= min(iStart1, iStart2);    { Get the left-most one }
end;


function FindQuoteEnd(CONST HtmlTag: string; StartAt: Integer): Integer;
VAR iEnd1, iEnd2: Integer;
begin
 { We don't know if ' or " is used so we look for both }
 iEnd1:= PosEx('"' , HtmlTag, StartAt);
 iEnd2:= PosEx('''', HtmlTag, StartAt);

 if iEnd1 = 0
 then Result:= iEnd2
 else if iEnd2= 0
      then Result:= iEnd1
      else Result:= min(iEnd1, iEnd2);   { Get the left-most one }
end;




{--------------------------------------------------------------------------------------------------
   EXTRACT Attrib

   Extract the value of a user defined Attrib from a single HTML line (if you provide an entire HTML body then the function will only return the first Attrib found).
   Works with multiple spaces between = and the "value"
   Works with single and double quotes.

   Usage:
         ExtractAttribValue('<a href="www.x.com/img.jpg"', 'href')
--------------------------------------------------------------------------------------------------}
function ExtractAttribValue(CONST HtmlTag, Attrib: string): string;
VAR
   iTag, Equal, QStart, QEnd: Integer;
begin
 Result:= '';

  { find 'tag' }
 if (Attrib= 'src') AND (Pos('<img', HtmlTag)> 0)
 then                              {  We treat the case the case where the <img> tag, might have a "scrset" attribute. Example:   <img srcset="https://images.pexels.com/photos/217130/pexels-photo-217130.jpeg?h=350&amp;auto=compress&amp;cs=tinysrgb 1x, https://images.pexels.com/photos/217130/pexels-photo-217130.jpeg?h=350&amp;dpr=2&amp;auto=compress&amp;cs=tinysrgb 2x" width="525" height="350" style="background:rgb(87,87,87)" class="photo-item__img" alt="Kostenloses Stock Foto zu fahrzeug, schwarz, motorrad, nahansicht" data-pin-media="https://images.pexels.com/photos/217130/pexels-photo-217130.jpeg?w=800&amp;h=1200&amp;fit=crop&amp;auto=compress&amp;cs=tinysrgb" src="https://images.pexels.com/photos/217130/pexels-photo-217130.jpeg?h=350&amp;auto=compress&amp;cs=tinysrgb" />     }
  begin
    iTag:= PosInsensitive(Attrib+'=', HtmlTag);      { Make sure that we have "src=" or "scr =" and not a "srcset" }
    if iTag= 0
    then iTag:= PosInsensitive(Attrib+' =', HtmlTag);
  end
 else iTag:= PosInsensitive(Attrib, HtmlTag);

 { find its corresponding = sign }
 if iTag= 0 then EXIT;

 Equal:= PosEx('=', HtmlTag, iTag+ Length(Attrib));

 if Equal > 0 then
  begin
   { We don't know if ' or " is used so we look for both }
   QStart := FindQuoteStart(HtmlTag, iTag+1);

   {This fixes: IE nu pune quotes arround 'blank'. Example:  target=_blank }
   if QStart < 1 then QStart:= Equal+1;

   if QStart> 0 then
    begin
     QEnd := FindQuoteEnd(HtmlTag, QStart+1);
     if QEnd> 0 then
      begin
        Result:= LightCore.CopyTo(HtmlTag, QStart+1, QEnd-1);               { +1 -1 to remove the quotes }
        {
        if Pos('?', Result) > 0
        then Result:= '';                                         { treat this case: src="http://sstatic1.histats.com/0.gif?2376752&amp;101" }
      end;
    end;
  end;
end;


{This fixes: IE does not put quotes arround 'blank'. Example:  target=_blank }
function ExtractAttribValueIE(CONST HtmlTag, Attrib: string): string;

  function FindQuoteEndIE(CONST HtmlTag: string; StartAt: Integer): Integer;
  begin
   { We don't know if ' or " is used so we look for both }
   Result:= PosEx('"' , HtmlTag, StartAt);
   if Result > 0 then EXIT;

   Result:= PosEx('''', HtmlTag, StartAt);
   if Result > 0 then EXIT;

   Result:= PosEx('>', HtmlTag, StartAt);
   if Result > 0 then EXIT;

   Result:= PosEx(' ', HtmlTag, StartAt);
   if Result > 0 then EXIT;
  end;

VAR iTag, QStart, QEnd: Integer;
begin
 Result:= '';

 iTag:= PosInsensitive(Attrib, HtmlTag);               { find 'tag' }
 if iTag= 0 then EXIT;

 iTag:= PosEx('=', HtmlTag, iTag+ Length(Attrib));     { find its corresponding = sign }

 if iTag> 0 then
  begin
   { We don't know if ' or " is used so we look for both }
   QStart := FindQuoteStart(HtmlTag, iTag+1);

   if QStart < 1 then QStart:= iTag;

   if QStart> 0 then
    begin
     QEnd := FindQuoteEndIE(HtmlTag, QStart+1);
     if QEnd> 0
     then Result:= LightCore.CopyTo(HtmlTag, QStart+1, QEnd-1);               { +1 -1 to remove the quotes }
    end;
  end;
end;


function ReplaceAttribValue(HtmlTag, Attrib, NewValue: string): string;
VAR iTag, QStart, QEnd: Integer;
begin
 Result:= '';

 iTag:= PosInsensitive(Attrib, HtmlTag);               { find 'tag' }
 if iTag= 0 then EXIT;

 iTag:= PosEx('=', HtmlTag, iTag+ Length(Attrib));     { find its corresponding = sign }

 if iTag> 0 then
  begin
   { We don't know if ' or " is used so we look for both }
   QStart := FindQuoteStart(HtmlTag, iTag+1);
   if QStart> 0 then
    begin
     QEnd := FindQuoteEnd(HtmlTag, QStart+1);
     if QEnd> 0 then
      begin
        Delete(HtmlTag, QStart+1, QEnd- QStart- 1);  { -1 to keep the quotes }
        insert(NewValue, HtmlTag, QStart+1);               { +1 -1 to remove the quotes }
        Result:= HtmlTag;
      end;
    end;
  end;
end;




{--------------------------------------------------------------------------------------------------
   EXTRACT TAG DATA

   Example: for '<div>xxx</div>' it returns xxx.
   It works for multiple tags.


 ALTERNATIVE: http://www.delphipages.com/forum/showthread.php?t=90262
--------------------------------------------------------------------------------------------------}
function ExtractTagsData(CONST HtmlBody: string; TagName: string): TStringList;
VAR
   Tag, LowBody, OpenTag, CloseTag: string;
   Offset, TextLen, Tag2Start, Tag1Start, Tag1End: Integer;
begin
 Offset   := 1;
 Result   := TStringList.Create;  { The caller must free the result }
 TextLen  := Length(HtmlBody);
 LowBody  := LowerCase(HtmlBody);
 TagName  := LowerCase(TagName);
 OpenTag  := '<'+ TagName+ '>';
 CloseTag := '</'+ TagName+ '>';

 REPEAT
   Tag1Start:= PosEx(OpenTag, LowBody, Offset);
   if Tag1Start> 0
   then
    begin
      Tag1End:= Tag1Start+ Length(OpenTag);    { This is the first char AFTER the tag }
      Tag2Start:= PosEx(CloseTag, LowBody, Tag1End);
      if (Tag2Start> 0)
      then
       begin
        Tag:= LightCore.CopyTo(HtmlBody, Tag1End, Tag2Start-1);
        Result.Add(Tag);
        Offset:= Tag1End;
       end
      else Break;
    end
   else
     Break;
 UNTIL (Offset>= TextLen);
end;







{--------------------------------------------------------------------------------------------------
   EXTRACT AHREF TAGS
---------------------------------------------------------------------------------------------------
  Parse HTML file and extract <A HRef> tags.
  Returns: A list of AHREF tags.

  Alternatives:
      Use ExtractURLs if you want a list of pure URLs.
      Use ExtractURLsText if you want a list of pure URLs extracted from TEXT (non-html)

  Example:
     GetOnlyLinksToWebPages accept only URLS that are HTML, PHP, ASP
     Example of URL ignored: <a href="background_wallpaper_1/setup.bin" alt="Download">

  ALTERNATIVE:
     http://www.delphipages.com/forum/showthread.php?t=90262
--------------------------------------------------------------------------------------------------}
function ExtractAhrefTags(CONST HtmlBody: string): TStringList;
VAR
   Tag, LowBody: string;
   Offset, TextLen, iEnd, iStart1, iStart2: Integer;
begin
 Result:= TStringList.Create;
 TextLen:= Length(HtmlBody);
 LowBody:= lowercase(HtmlBody);
 Offset:= 1;

 REPEAT
   iStart1:= PosEx('<a ', LowBody, Offset);

   if iStart1> 0 then
    begin
     iStart2:= PosEx(' href', LowBody, iStart1+2);
     if iStart2> 0
     then
      begin
        iEnd:= PosEx('>', HtmlBody, iStart1+7);
        if (iEnd> 0) AND (iStart2< iend)
        then
         begin
          Tag:= LightCore.CopyTo(HtmlBody, iStart1, iEnd);
          if Tag = ''
          then EmptyDummy; // raise exception.Create('Empty tag!');
          Result.Add(Tag);
          offset:= iEnd+1;
         end
        else
         Offset:= iStart1+ 3;
      end
     else Offset:= iStart1+ 3;
    end
   else Offset:= textlen;
  UNTIL Offset>= TextLen;
end;



function ExtractURLs(CONST HtmlBody: string): TStringList;    { Returns a list of links (that start with http://) }
VAR
   i: Integer;
begin
 Result:= ExtractAhrefTags(HtmlBody);
 for i:= 0 to Result.Count-1
   DO Result[i]:= ExtractAttribValue(Result[i], 'href');
 Result.RemoveEmptyLines;       { Empty lines are added when the 'href' cannot parsed correctly because of malformed tags. Example of thing that I encountered: <a href=\"https://www.instagram.com/minimalsetups/\">Instagram<\/a>  }
end;




{--------------------------------------------------------------------------------------------------
  Extracts URLs from a TEXT (non-html).

  For this, it searches the 'www' and 'http' strings.
  If the TEXT is a HTML page, it will NOT parse the '<>' tags. If this is what you need, use ExtractAhrefTags.

  Returns a list of URLs and the number of extracted addresses


 ALTERNATIVE:
    http://www.delphipages.com/forum/showthread.php?t=90262
    https://www.nsonic.de/blog/2007/05/check-for-valid-url/comment-page-1/#comment-412606
---------------------------------------------------------------------------------------------------}
function ExtractURLsText(Text: string; AdreseExtrase: TStringlist): Integer;       //old name ExtractWebLinks
CONST
   PadHttp= 7;
   PadWww = 4;
VAR
   cnt, Pad: integer;
   StartPos, PosHttp, PosWww, EndPos: integer;
begin
 Text:= LowerCase(Text);
 Result:= 0;
 StartPos:= 1;

 while (StartPos> 0) AND (StartPos< length(Text)) DO
  begin
   { Search the 'http' and 'www' strings }
   Text    := system.COPY (Text, StartPos, high(Integer));
   PosHttp := PosInsensitive ('http://', Text);                                    { asta poate sa returneze si 0, asa ca trebuie sa o verific mai jos sa nu fie 0 }
   PosWww  := PosInsensitive ('www.', Text);

   { Finds out which appears first }
   if  (PosHttp < PosWww)
   AND (PosHttp > 0)                                                               { ASTA e neaparat necesara aici }
   then
    begin
     StartPos := PosHttp;
     Pad:= PadHttp;
    end
   else
    begin
     StartPos := PosWww;
     Pad:= PadWww;
    end;

   if StartPos> 0 then                                                             { daca e o adrese web valida, atunci}
    for cnt:= (StartPos+ Pad) to length(Text) DO
      if CharInSet(Text[cnt], SeparatorsHTTP)                                      { Look for separators }
      OR (cnt= length(Text))                                                       { OR end of file }
      then
       begin
         { I have found also its end }
         EndPos:= cnt-1;

         { Add it to list }
         Inc(Result);
         AdreseExtrase.Add(LightCore.CopyTo(Text, StartPos, EndPos) );

         { Prepare for the next loop }
         StartPos:= EndPos+1;
         Break;
       end;

   end;
end;



function ColapseUrlDots(sURL: string): string;     { Usd by AID }
begin
 { This happens for this web site: http://wallpapercave.net  Their URL is wrong! }
 Result:= sURL;

 { First we treat the /../ case }
 while Pos('/../', Result)  > 0
  DO Result:= System.SysUtils.StringReplace(Result, '/../', '/', [rfReplaceAll]);   { StringReplace should replaces ALL occurences but it doesn't  }

 { Then we deal with what is left or with ../ cases }
 while Pos('../', Result)  > 0
  do Result:= System.SysUtils.StringReplace(Result, '../', '/', [rfReplaceAll]);

 Assert( Pos('../', Result)  = 0, 'ColapseUrlDots failed');   //raise - make sure this never happens
end;




















function LinkHasNoFollow(CONST Link: string): Boolean;
VAR s: string;
begin
 if Link= '' then EXIT(FALSE);
 s:= AnsiLowerCase(ExtractAttribValue(Link, 'rel'));
 Result:= s= 'nofollow';
end;


function LinkOpensInNewWind(CONST Link: string): Boolean;
VAR s: string;
begin
 if Link= '' then EXIT(FALSE);
 s:= AnsiLowerCase(ExtractAttribValueIE(Link, 'target'));
 Result:= s= '_blank';
end;





{ META }
function LineIsMeta(CONST HtmlLine, MetaName: string): Boolean;  { Returns true if this line is the specified 'meta'. For example: <meta name="Keywords" content="">  }
VAR s: string;
begin
 s:= ExtractTextBetween(HtmlLine, '<', '>');        { Example of line:  <meta name="Keywords" content=""> }
 Result:= (PosInsensitive(MetaName, s) > 11)
      AND (PosInsensitive('meta', s) < 3)
      AND (PosInsensitive('name', s) > 5)
      AND (PosInsensitive('content', s) > 14);
end;




function LineIsTitle(CONST HtmlLine: string): Boolean;    {Example of line:   <title>Small Business-Bootstrap Template</title> }
begin
 Result:= (PosInsensitive(HtmlLine, '<title>') > 0) AND (PosInsensitive(HtmlLine, '</title>') > 7);
end;
















{==================================================================================================
   URL
==================================================================================================}

function IsSafeHtmlChar(CONST Ch: Integer): Boolean;
begin
  case ch of
    48..57 : Result:= TRUE;    // 0-9
    65..90 : Result:= TRUE;    // A-Z
    97..122: Result:= TRUE;    // a-z
    33, 95 : Result:= TRUE;    // ! _
    39..42 : Result:= TRUE;    // '()*
    45..46 : Result:= TRUE;    // -.
    126    : Result:= TRUE;    // ~
   else      Result:= FALSE;
  end;
end;


function SanitizeText(CONST URL: string): UTF8String;   { Converts all Internet unsafe chars (like : space tab etc) to encoded chars like %20 }    { SOURCE: http://marc.durdin.net/2012/07/indy-tiduripathencode-urlencode-and.html }
CONST
  HexMap: UTF8String = '0123456789ABCDEF';
VAR
   I, J: Integer;
   ASrcUTF8: UTF8String;
begin
  Result := '';                                                                                    {Do not Localize}

  ASrcUTF8 := UTF8Encode(URL);     // UTF8Encode call not strictly necessary but prevents implicit conversion warning

  I := 1; J := 1;
  SetLength(Result, Length(ASrcUTF8) * 3); // space to %xx encode every byte
  while I <= Length(ASrcUTF8) DO
   begin
    if IsSafeHtmlChar(Ord(ASrcUTF8[I]))
    then
     begin
      Result[J] := ASrcUTF8[I];
      Inc(J);
     end
    else
     if ASrcUTF8[I] = ' '
     then
      begin
        Result[J] := '+';
        Inc(J);
      end
     else
      begin
        Result[J] := '%';
        Result[J+1] := HexMap[(Ord(ASrcUTF8[I]) shr 4) + 1];
        Result[J+2] := HexMap[(Ord(ASrcUTF8[I]) and 15) + 1];
        Inc(J,3);
      end;
    Inc(I);
   end;

 SetLength(Result, J-1);
end;





 end.
