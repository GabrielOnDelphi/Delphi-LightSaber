UNIT LightCore.HTML;

{-------------------------------------------------------------------------------------------------------------
   2026.11.13
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt

   HTML Parsing

   Also see: c:\Projects-3rd_Packages\Third party packages\_Out\_TEMPORARY EXCLUDED (Don't delete them yet!)\uHTMLBuilder.pas
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
   System.SysUtils, System.AnsiStrings, System.StrUtils, System.Classes, System.Math,
   LightCore.StringList, LightCore;

{--------------------------------------------------------------------------------------------------
   HTML PARSING
--------------------------------------------------------------------------------------------------}
 function  GetBodyFromHtml      (CONST AHTML: string): string;                                      { Get the HTML code contained between the <Body> tags }
 function  GenerateHTMLHeader   (CONST Title, MetaDescription, Keywords, CssFile: string): string;  { Old name: GenerateStandardHTMLHeader }
 function  HtmlEscape           (CONST S: string): string;                                          { Escape &<>" so plain text is safe inside HTML markup and double-quoted attribute values }
 function  HtmlUnescape         (CONST S: string): string;                                          { Reverse of HtmlEscape: decode ONLY the four entities it emits (&amp; &lt; &gt; &quot;); &amp; last }

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

 { Also see LightCore.ExtractTextBetween }
 function  LineIsMeta      (CONST HtmlLine, MetaName: string): Boolean;                             { Returns true if this line is the specified 'meta'. For example: <meta name="Keywords" content="">  }
 function  LineIsTitle     (CONST HtmlLine: string): Boolean;

 {}
 function  SanitizeText    (CONST URL: string): UTF8String;                                         { Encode a string into a URL query string parameter (as per web forms).  One would expect that Indy contains well-tested functions to handle this. Well, Indy contains some functions to help with this, but they may not work quite as you expect.  In fact, they may not be much use at all. }
 function  IsSafeHtmlChar  (CONST Ch: Integer): Boolean;

 { Formattings }
 function  FixHtmlFormatings(Body: TStringList): string; overload;
 procedure FixHtmlFormatings(CONST FileName: string);  overload;




IMPLEMENTATION

USES
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

  { Removes exactly one CRLF from the end. RemoveLastEnter removes up to 2 CR/LF characters, which on POSIX (CRLF = LF) also eats the empty line before. }
  function removeOneLineBreak(CONST Text: string): string;
  begin
   if (Length(Text) >= Length(CRLF))
   AND (Copy(Text, Length(Text)- Length(CRLF)+ 1, Length(CRLF)) = CRLF)
   then Result:= Copy(Text, 1, Length(Text)- Length(CRLF))
   else Result:= Text;
  end;

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
   OR (s2= '</h6')
   OR (s2= '</ti')
   then Result:= removeOneLineBreak(Result)+ s+ CRLF
   else Result:= Result+ s+ CRLF;
  end;

 Result:= removeOneLineBreak(Result);
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

 { BackupFileIncrement COPIES the file to an incremented '.bak' name and returns THAT name.
   So: back up the original first, then write the fixed text back into the original file.
   (The old code wrote the fixed text into the .bak file, leaving the original untouched.) }
 if BackupFileIncrement(FileName) = ''
 then RAISE Exception.Create('FixHtmlFormatings: Cannot back up '+ FileName);

 StringToFile(FileName, s, woOverwrite);
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
function GetBodyFromHtml(CONST AHTML: string): string;                                                   { Get the HTML code contained between the <Body> tags. Tolerates attributes on <body ...> and any casing on both open and close tags. Returns '' if either tag is missing. }
VAR
   LowHtml: string;
   OpenStart, OpenEnd, CloseStart: Integer;
begin
 Result:= '';
 LowHtml:= LowerCase(AHTML);

 OpenStart:= Pos('<body', LowHtml);                                                                       { match <body and <body class="..." alike }
 if OpenStart= 0 then EXIT;

 OpenEnd:= PosEx('>', LowHtml, OpenStart);                                                                { end of the opening tag }
 if OpenEnd= 0 then EXIT;

 CloseStart:= PosEx('</body>', LowHtml, OpenEnd+1);
 if CloseStart= 0 then EXIT;

 Result:= System.Copy(AHTML, OpenEnd+1, CloseStart- OpenEnd- 1);
end;










{ The 4 inputs are plain text: they are HTML-escaped here. }
function GenerateHTMLHeader(CONST Title, MetaDescription, Keywords, CssFile: string): string;
begin
 Result:=
  '<HTML><HEAD>'+ CRLF
 +'<title>'+ HtmlEscape(Title)+'</title>'+ CRLF+CRLF
 +'<meta name="description" content="'+ HtmlEscape(MetaDescription)+'" />'+ CRLF
 +'<meta name="Keywords"    content="'+ HtmlEscape(Keywords)+'" />'+ CRLF

 +' <meta name="ROBOTS"               content="NOODP, ALL" />'+ CRLF
 +' <meta http-equiv="Content-Type"   content="text/html; charset=iso-8859-1" />'+ CRLF
 +' <meta http-equiv="Page-Enter"     content="blendTrans(Duration=0.4)" />'+ CRLF
 +' <meta http-equiv="Page-Exit"      content="blendTrans(Duration=0.4)" />'+ CRLF;

 if CssFile<> ''
 then Result:= Result+ ' <link href="'+ HtmlEscape(CssFile)+ '" rel="stylesheet" type="text/css" />'+ CRLF;

 Result:= Result+ ' <link rel="shortcut icon"         HREF ="favicon.ico" />'+ CRLF
                + '</HEAD>'+ CRLF
                + '<body>' + CRLF;
end;


{ Escapes the characters that are unsafe in HTML text or in a double-quoted attribute value.
  '&' is replaced first, so the entities introduced afterwards are not escaped a second time.
  The single quote (') is intentionally left alone: this output only lands in text nodes and in
  double-quoted attributes (e.g. src="..."), never in single-quoted attributes. }
function HtmlEscape(CONST S: string): string;
begin
  Result := StringReplace(S,      '&', '&amp;',  [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;',   [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;',   [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
end;


{ The exact inverse of HtmlEscape. Decodes ONLY the four entities HtmlEscape emits - it is NOT a general
  HTML entity decoder (it must not turn an author's literal &nbsp; into a space). '&amp;' is decoded LAST so
  an escaped "&lt;" (held on disk as "&amp;lt;") does not collapse one step too far. }
function HtmlUnescape(CONST S: string): string;
begin
  Result := StringReplace(S,      '&lt;',   '<', [rfReplaceAll]);
  Result := StringReplace(Result, '&gt;',   '>', [rfReplaceAll]);
  Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll]);
  Result := StringReplace(Result, '&amp;',  '&', [rfReplaceAll]);
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
  OR (Copy(Link, 1, 2) = '//')                         { Protocol-relative URL (//cdn.example.com/x.js) points to another host. Leave it like this }
  then Result:= Link
  else
   begin
    Result:= '';
    Depth:= CountAppearance('/', UrlRemoveHttp(MasterPageUrl))-1;

    if Depth < 1
    then
     begin
      { The master page sits in the root: the link only loses its leading '/' }
      Result:= Copy(Link, 2, MaxInt);
      if Result = ''
      then Result:= './';
     end
    else
     begin
      for i:= 1 to Depth DO
        Result:= Result+ '../';

      Result:= RemoveLastChar(Result);
      Result:= Result+ Link;
     end;
   end;
end;


{ Same as MakeLinkRelativeToRoot but applies to a whole HTML document

  HTML Body is the content of the page that needs processed
  HTML URL  is the location of server of that page }
function MakeLinksRelativeToRoot(CONST HTMLBody, HtmlUrl: string): string;
VAR
   Tag, url, Lower: string;
   Offset, iEnd, iStart1, iStart2: Integer;
begin
 Result:= HTMLBody;
 Lower:= LowerCase(Result);   { Tag names are case-insensitive: search in a lowercase copy. LowerCase maps only A..Z, so the positions in Lower are the positions in Result. }

 { Process <a> tags }
 Offset:= 1;
 REPEAT
   iStart1:= PosEx('<a ', Lower, Offset);

   if iStart1> 0 then
    begin
     iStart2:= PosEx(' href', Lower, iStart1+2);
     if iStart2> 0
     then
      begin
        iEnd:= PosEx('>', Lower, iStart1+7);

        if (iEnd> 0) AND (iStart2< iend)
        then
         begin
          Tag:= LightCore.CopyTo(Result, iStart1, iEnd);

          URL:= ExtractAttribValue(Tag, 'href');
          if URL = ''
          then Offset:= iEnd+ 1                    { No (quoted) href value -> leave the tag untouched. Without this guard ReplaceAttribValue returns '' and the whole tag would be silently DELETED from the page (same guard as LightCore.Internet.HTMLImg) }
          else
           begin
            URL:= MakeLinkRelativeToRoot(HtmlUrl, URL);

            { Put the new URL back into the tag }
            Tag:= ReplaceAttribValue(Tag, 'href', URL);
            offset:= iEnd+1;  {todo-optimization: here instead of 1 I should use the number of extra characters added (for example '../../../' means that I inserted 9 extra chars }

            Result:= System.Copy(Result, 1, iStart1-1) + Tag + Copy(Result, iEnd+1, MaxInt);
            Lower:= LowerCase(Result);
           end;
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
   iStart1:= PosEx('<img ', Lower, Offset);

   if iStart1> 0 then
    begin
     iStart2:= PosEx(' src', Lower, iStart1+2);
     if iStart2> 0
     then
      begin
        iEnd:= PosEx('>', Lower, iStart1+6);

        if (iEnd> 0) AND (iStart2< iend)
        then
         begin
          Tag:= LightCore.CopyTo(Result, iStart1, iEnd);

          URL:= ExtractAttribValue(Tag, 'src');
          if URL = ''
          then Offset:= iEnd+ 1                    { No (quoted) src value -> leave the tag untouched. Without this guard ReplaceAttribValue returns '' and the whole tag would be silently DELETED from the page (same guard as LightCore.Internet.HTMLImg) }
          else
           begin
            URL:= MakeLinkRelativeToRoot(HtmlUrl, URL);

            { Put the new URL back into the tag }
            Tag:= ReplaceAttribValue(Tag, 'src', URL);
            offset:= iEnd+1;  {todo-optimization: here instead of 1 I should use the number of extra characters added (for example '../../../' means that I inserted 9 extra chars }

            Result:= System.Copy(Result, 1, iStart1-1) + Tag + Copy(Result, iEnd+1, MaxInt);
            Lower:= LowerCase(Result);
           end;
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




{ Finds the value of the attribute Attrib in HtmlTag.
  Returns FALSE when the attribute is missing or its value is malformed.
  On TRUE: ValStart is the first character of the value, ValEnd the last one (ValEnd = ValStart-1 for an empty value),
  Quote is the quote character around the value, or #0 for an unquoted value (target=_blank).

  The name matches case-insensitively, and only when:
    (a) the character before it is a boundary: start of the string, whitespace, '<', '/' or the closing quote of the previous value
    (b) it is followed by '=' (whitespace is allowed around '=')
  Without (a) we'd match 'href' inside 'hreflang', or 'target' inside 'data-target'.
  The value after every '=' is skipped, so a name inside another attribute's value (title="see href=x") never matches.
  A quoted value ends at the next quote of the same kind, so "Gabriel's book" keeps its apostrophe.
  An unquoted value ends at whitespace or '>'. An unquoted value that holds a quote is malformed. }
function locateAttribValue(CONST HtmlTag, Attrib: string; OUT ValStart, ValEnd: Integer; OUT Quote: Char): Boolean;
CONST
   WhiteSpace = [' ', #9, #13, #10];
VAR
   LowTag, LowAttrib: string;
   i, k, TagLen, AttrLen: Integer;
   PrevCh: Char;
   IsName: Boolean;

  { Reads the value that starts at k, the first character after '='. }
  function readValue(k: Integer): Boolean;
  VAR q: Integer;
  begin
   while (k <= TagLen) AND CharInSet(HtmlTag[k], WhiteSpace) DO
     Inc(k);
   if k > TagLen then EXIT(FALSE);

   if CharInSet(HtmlTag[k], ['"', ''''])
   then
    begin
     Quote:= HtmlTag[k];
     ValStart:= k+ 1;
     q:= PosEx(Quote, HtmlTag, ValStart);
     if q = 0 then EXIT(FALSE);                       { Unterminated quote }
     ValEnd:= q- 1;
    end
   else
    begin
     Quote:= #0;
     ValStart:= k;
     q:= k;
     while (q <= TagLen) AND NOT CharInSet(HtmlTag[q], WhiteSpace+ ['>']) DO
      begin
       if CharInSet(HtmlTag[q], ['"', '''']) then EXIT(FALSE);
       Inc(q);
      end;
     ValEnd:= q- 1;
     if ValEnd < ValStart then EXIT(FALSE);           { '=' followed by '>': no value }
    end;
   Result:= TRUE;
  end;

begin
 Result:= FALSE;
 ValStart:= 0;
 ValEnd  := -1;
 Quote   := #0;

 LowTag   := LowerCase(HtmlTag);
 LowAttrib:= LowerCase(Trim(Attrib));     { Trim: callers pass ' src' too (AID cImgList.pas) }
 TagLen   := Length(LowTag);
 AttrLen  := Length(LowAttrib);
 if (AttrLen= 0) OR (TagLen< AttrLen) then EXIT;

 i:= 1;
 while i <= TagLen DO
  begin
   if HtmlTag[i] = '=' then
    begin
     { Skip the value of an attribute that is not ours }
     if readValue(i+ 1)
     then
      begin
       if Quote = #0
       then i:= ValEnd+ 1
       else i:= ValEnd+ 2;                            { Past the closing quote }
      end
     else Inc(i);
     Continue;
    end;

   { (a) word-boundary check on the char BEFORE the match }
   if i= 1
   then PrevCh:= ' '
   else PrevCh:= HtmlTag[i- 1];

   IsName:= CharInSet(PrevCh, WhiteSpace+ ['<', '/', '"', ''''])
        AND (Copy(LowTag, i, AttrLen) = LowAttrib);

   if IsName then
    begin
     { (b) the next non-space char after the name must be '=' }
     k:= i+ AttrLen;
     while (k <= TagLen) AND CharInSet(HtmlTag[k], WhiteSpace) DO
       Inc(k);
     if (k <= TagLen) AND (HtmlTag[k]= '=')
     then EXIT(readValue(k+ 1));
    end;

   Inc(i);
  end;

 ValStart:= 0;
 ValEnd  := -1;
 Quote   := #0;
end;


{--------------------------------------------------------------------------------------------------
   EXTRACT Attrib

   Extract the value of a user defined Attrib from a single HTML line (if you provide an entire HTML body then the function will only return the first Attrib found).
   Works with whitespace around the = sign.
   Works with single quotes, double quotes and unquoted values (target=_blank).
   Returns '' when the attribute is missing.

   Usage:
         ExtractAttribValue('<a href="www.x.com/img.jpg"', 'href')
--------------------------------------------------------------------------------------------------}
function ExtractAttribValue(CONST HtmlTag, Attrib: string): string;
VAR
   ValStart, ValEnd: Integer;
   Quote: Char;
begin
 if locateAttribValue(HtmlTag, Attrib, ValStart, ValEnd, Quote)
 then Result:= Copy(HtmlTag, ValStart, ValEnd- ValStart+ 1)
 else Result:= '';
end;


{ TO BE DELETED - ExtractAttribValue also reads unquoted values (target=_blank), so this routine is a plain call to it.
  Call ExtractAttribValue directly. }
function ExtractAttribValueIE(CONST HtmlTag, Attrib: string): string;
begin
 Result:= ExtractAttribValue(HtmlTag, Attrib);
end;


{ Replaces the value of Attrib with NewValue. An unquoted value is written back inside double quotes.
  Returns '' when the attribute is missing. }
function ReplaceAttribValue(HtmlTag, Attrib, NewValue: string): string;
VAR
   ValStart, ValEnd: Integer;
   Quote: Char;
begin
 if NOT locateAttribValue(HtmlTag, Attrib, ValStart, ValEnd, Quote)
 then EXIT('');

 Delete(HtmlTag, ValStart, ValEnd- ValStart+ 1);
 if Quote = #0
 then Insert('"'+ NewValue+ '"', HtmlTag, ValStart)
 else Insert(NewValue, HtmlTag, ValStart);
 Result:= HtmlTag;
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
 TRY
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
 EXCEPT
   FreeAndNil(Result);   { Don't leak the list if anything raises before we return it }
   RAISE;
 END;
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
 TRY
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
            if Tag <> ''
            then Result.Add(Tag);
            offset:= iEnd+1;
           end
          else
           Offset:= iStart1+ 3;
        end
       else Offset:= iStart1+ 3;
      end
     else Offset:= textlen;
    UNTIL Offset>= TextLen;
 EXCEPT
   FreeAndNil(Result);   { Don't leak the list if anything raises before we return it }
   RAISE;
 END;
end;



function ExtractURLs(CONST HtmlBody: string): TStringList;    { Returns a list of links (that start with http://) }
VAR
   i: Integer;
begin
 Result:= ExtractAhrefTags(HtmlBody);
 TRY
   for i:= 0 to Result.Count-1
     DO Result[i]:= ExtractAttribValue(Result[i], 'href');
   Result.RemoveEmptyLines;       { Empty lines are added when the 'href' cannot parsed correctly because of malformed tags. Example of thing that I encountered: <a href=\"https://www.instagram.com/minimalsetups/\">Instagram<\/a>  }
 EXCEPT
   FreeAndNil(Result);   { Don't leak the list if anything raises before we return it }
   RAISE;
 END;
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
   PadHttp = 7;
   PadHttps= 8;
   PadWww  = 4;
   { The characters that end an URL: SeparatorsHTTP without '&', ':' and '~', which are legal inside an URL (query a=1&b=2, port :8080, user folder /~name) }
   UrlEnd = [' ', '`', '!', '$', '^', '*', '(', ')', '[', ']', '{', '}', ';', '''', '"', '<', '>', ',', '\', '|', #10, #13, #9];
VAR
   cnt, Pad: integer;
   StartPos, PosHttp, PosHttps, PosWww, EndPos: integer;
begin
 { Do NOT lowercase the text: PosInsensitive already searches case-insensitively, and lowercasing
   would return mangled URLs (paths are case-sensitive on most web servers). }
 Result:= 0;
 StartPos:= 1;

 while (StartPos> 0) AND (StartPos< length(Text)) DO
  begin
   { Search the 'http', 'https' and 'www' strings }
   Text    := system.COPY (Text, StartPos, high(Integer));
   PosHttp := PosInsensitive ('http://', Text);                                    { asta poate sa returneze si 0, asa ca trebuie sa o verific mai jos sa nu fie 0 }
   PosHttps:= PosInsensitive ('https://', Text);
   PosWww  := PosInsensitive ('www.', Text);

   { Finds out which appears first. StartPos stays 0 when none is found, which ends the loop. }
   StartPos:= 0;
   Pad:= 0;
   if PosHttp > 0 then
    begin
     StartPos:= PosHttp;
     Pad:= PadHttp;
    end;
   if (PosHttps > 0) AND ((StartPos = 0) OR (PosHttps < StartPos)) then
    begin
     StartPos:= PosHttps;
     Pad:= PadHttps;
    end;
   if (PosWww > 0) AND ((StartPos = 0) OR (PosWww < StartPos)) then
    begin
     StartPos:= PosWww;
     Pad:= PadWww;
    end;

   if StartPos> 0 then                                                             { daca e o adrese web valida, atunci}
    if StartPos+ Pad > Length(Text)
    then StartPos:= 0                                                              { Bare 'www.'/'http://' prefix at the very END of the text (no URL body follows). Stop here: the FOR below would not execute at all and the WHILE would re-scan the same slice forever (infinite loop) }
    else
     for cnt:= (StartPos+ Pad) to length(Text) DO
       if CharInSet(Text[cnt], UrlEnd)                                             { Look for separators }
       OR (cnt= length(Text))                                                      { OR end of file }
       then
        begin
          { I have found also its end }
          if CharInSet(Text[cnt], UrlEnd)
          then EndPos:= cnt-1                                                      { Stop before the separator }
          else EndPos:= cnt;                                                       { URL runs to the very end of the text - keep its last char }

          { ':' is not in UrlEnd (it is legal in 'host:8080'), but an URL never ends in ':' - in 'see www.x.com: now' the colon belongs to the sentence }
          while (EndPos > StartPos) AND (Text[EndPos] = ':') DO
            Dec(EndPos);

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

 if Pos('../', Result) > 0
 then raise Exception.Create('ColapseUrlDots failed - URL still contains "../"');
end;















{ 'rel' holds a list of words separated by whitespace (rel="nofollow noopener"). TRUE when one of them is 'nofollow'. }
function LinkHasNoFollow(CONST Link: string): Boolean;
VAR s: string;
begin
 if Link= '' then EXIT(FALSE);
 s:= AnsiLowerCase(ExtractAttribValue(Link, 'rel'));
 s:= StringReplace(s, #9,  ' ', [rfReplaceAll]);
 s:= StringReplace(s, #13, ' ', [rfReplaceAll]);
 s:= StringReplace(s, #10, ' ', [rfReplaceAll]);
 Result:= Pos(' nofollow ', ' '+ s+ ' ') > 0;
end;


function LinkOpensInNewWind(CONST Link: string): Boolean;
VAR s: string;
begin
 if Link= '' then EXIT(FALSE);
 s:= AnsiLowerCase(ExtractAttribValue(Link, 'target'));
 Result:= s= '_blank';
end;





{ META }
function LineIsMeta(CONST HtmlLine, MetaName: string): Boolean;  { Returns true if this line is the specified 'meta'. For example: <meta name="Keywords" content="">  }
VAR
   s: string;
   ValStart, ValEnd: Integer;
   Quote: Char;
begin
 s:= TrimLeft(ExtractTextBetween(HtmlLine, '<', '>'));        { Example of line:  <meta name="Keywords" content=""> }

 { The tag must be 'meta', its 'name' must be MetaName, and it must have a 'content' attribute (the attributes can come in any order) }
 Result:= (MetaName <> '')
      AND (Length(s) > 4)
      AND SameText(Copy(s, 1, 4), 'meta')
      AND CharInSet(s[5], [' ', #9, #13, #10])
      AND SameText(ExtractAttribValue(s, 'name'), MetaName)
      AND locateAttribValue(s, 'content', ValStart, ValEnd, Quote);
end;




function LineIsTitle(CONST HtmlLine: string): Boolean;    {Example of line:   <title>Small Business-Bootstrap Template</title> }
begin
 Result:= (PosInsensitive('<title>', HtmlLine) > 0) AND (PosInsensitive('</title>', HtmlLine) > 7);
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
