UNIT LightVcl.Internet.HTMLImg;

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt

   Simple HTML Parsing. Specialized in extracting images from a webpage.

   Process 'relative path' for IMG tags
   Extract IMG tags
   Extract URLs from IMG/A HREF tags

   FOR STORMY & AID

   Also see: c:\MyProjects\Packages\Third party packages\uHTMLBuilder.pas
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
   System.SysUtils, System.NetEncoding, System.IOUtils, System.StrUtils, System.Classes;


{ <IMG> TAGS }
function ExtractIMGTags        (CONST HTMLBody: string): TStringList;            { Extract <IMG> tags images from HTML body }      { Old name: ExtractImage_Img }

{ URLs }
function ExtractImagesFromIMG  (CONST HTMLBody: string): TStringList;            { Extract images from <IMG src> tags }            { Oldname: ExtractImage_Img }
function ExtractImagesFromAHREF(CONST HtmlBody: string): TStringList;            { Extract images from '<a href>' tags }           { Oldname: ExtractImage_Href }
function ExtractImages         (CONST HtmlBody: string): TStringList;            { Extract images from '<a href>' and <img>'. }    { Oldname: ExtractImages }

{ RELATIVE PATH }
function MakeImgRelativePaths(CONST HtmlBody, RelativeTo: string): string; { Locates all IMG tags in a HTML document and converts their SRC (paths) from full path to relative path }
function MakeImgRelativePath (CONST HtmlLine, RelativeTo: string): string;
function MakeImgFullPath     (CONST HtmlLine, Base: string): string;
function ExpandRelativePaths (CONST HtmlBody, Base: string): string;



IMPLEMENTATION                                                                                                                                   {$WARN GARBAGE OFF}   {Silence the: 'W1011 Text after final END' warning }

USES
   LightCore.IO, LightVcl.Internet.Common, LightCore.Internet, LightCore, LightCore.Time, LightCore.Types,LightVcl.Internet.HTML, LightCore.HTML;



{ Extracts all <IMG> tags from a HTML
  Works if the tag is split on two lines. }
function ExtractIMGTags(CONST HTMLBody: string): TStringList;
VAR
   LowBody: string;
   Offset, TextLen, iEnd, iStart1: Integer;
   CurTag: string;
begin
 Result:= TStringList.Create;

 { Extract images from '<img>' }
 TextLen:= Length(HtmlBody);
 LowBody:= LowerCase(HtmlBody);
 Offset:= 1;
 REPEAT
   iStart1:= PosEx('<img ', LowBody, Offset);

   if iStart1> 0 then
    begin
      iEnd:= PosEx('>', HtmlBody, iStart1+7);
      if (iEnd> 0)
      then
       begin
        CurTag:= CopyTo(HtmlBody, iStart1, iEnd);
        if CurTag > ''
        then Result.Add(CurTag);

        Offset:= iEnd+1;
       end
      else
       Offset:= iStart1+ 3;
    end
   else Offset:= textlen;
  UNTIL Offset>= TextLen;
end;




{ Extracts URL of images from <IMG scr="xxx"> tags }
function ExtractImagesFromIMG(CONST HTMLBody: string): TStringList;   { Used by: StormyWebSite Builder 2017.08 / AID 2017.10 }
VAR
   i: Integer;
   CurTag, ImgURL: string;
   Tags: TStringList;
begin
 Tags:= ExtractIMGTags(HTMLBody);
 Result:= TStringList.Create;
 for i:= 0 to Tags.Count-1 DO
  begin
   CurTag:= Tags[i];
   ImgURL:= ExtractAttribValue(CurTag, 'src');
   ImgURL:= CleanServerCommands(ImgURL);
   Result.Add(ImgURL);
  end;
 FreeAndNil(Tags);
end;





{ Extract URLs of images from all <a href> tags in a HTML file.
  Return a list URLs.
  Example:  <a href="www.tst.com/1.jpg"> returns 'www.tst.com/1.jpg' }
function ExtractImagesFromAHREF(CONST HtmlBody: string): TStringList;
VAR
   CurTag, ImgURL: string;
   AHRefTags: TStringList;
begin
 Result:= TStringList.Create;

 { Extract images from '<a href>' }
 AHRefTags:= ExtractAhrefTags(HtmlBody);
 TRY
  for CurTag in AHRefTags DO
   begin
    ImgURL:= ExtractAttribValue(CurTag, 'href');
    if LightCore.IO.IsImage(ImgURL)
    then Result.Add(ImgURL);   { This is an <a href> tag so it contains no info about the size of the image }
   end;
 FINALLY
  FreeAndNil(AHRefTags);
 END;
end;






{ Extract images from '<a href>' and <img src="">'.
  Returns the URL of the image(s).
  ParentPageURL is the URL of the page from where the link was extracted. }
function ExtractImages(CONST HtmlBody: string): TStringList;   { use a separate procedure to add ParentPageURL to all urls WHEN necessary }
VAR
   Temp: TStringList;
begin
 Result:= TStringList.Create;

 { Extract images from '<href>' }
 Temp:= ExtractImagesFromAHREF(HtmlBody);
 Result.Add(Temp.Text+ CRLF);
 FreeAndNil(Temp);

 { Extract URLs to images from <img src="URL"> }
 Temp:= ExtractImagesFromIMG(HtmlBody);
 Result.Add(Temp.text);
 FreeAndNil(Temp);
end;


















{-------------------------------------------------------------------------------------------------------------
   RELATIVE PATHS
-------------------------------------------------------------------------------------------------------------}

{ Locates all IMG tags in a HTML document and converts their SRC (paths) from relative path to full path }
function ExpandRelativePaths(CONST HtmlBody, Base: string): string;
VAR
   i: Integer;
   TSL: TStringList;
begin
 TSL:= TStringList.Create;
 TRY
   TSL.Text:= HtmlBody;
   if TSL.Count = 0 then EXIT;

   for i:= 0 to TSL.Count-1
    DO TSL[i]:= MakeImgFullPath(TSL[i], Base);

   Result:= TSL.Text;
 FINALLY
   FreeAndNil (TSL);
 END;
end;






{ Locates all IMG tags in a HTML document and converts their SRC (paths) from full path to relative path }
function MakeImgRelativePaths(CONST HtmlBody, RelativeTo: string): string;
VAR
   i: Integer;
   TSL: TStringList;
begin
 TSL:= TStringList.Create;
 TRY
   TSL.Text:= HtmlBody;
   if TSL.Count = 0 then EXIT;

   for i:= 0 to TSL.Count-1
    DO TSL[i]:= MakeImgRelativePath(TSL[i], RelativeTo);

   Result:= TSL.Text;
 FINALLY
   FreeAndNil (TSL);
 END;
end;




{ Locates ONLY THE FIRST <img> tag in a HTML line and converts its SRC (path) from full path to relative path
  DOES NOT WORK if the line has more than one IMG tag }
function MakeImgRelativePath(CONST HtmlLine, RelativeTo: string): string;
VAR
   NewTag, Src, Tag, BeforeTag, AfterTag: string;
   iEnd, iStart: Integer;
   URLDecoder: TURLEncoding;
begin
 { Extracts <IMG> tag }
 iStart:= PosInsensitive('<img', HtmlLine);
 if iStart< 1 then EXIT(HtmlLine);

 iEnd:= System.StrUtils.PosEx('>', HtmlLine, iStart+1);
 if iEnd < 1 then EXIT(HtmlLine);

 Tag:= CopyTo(HtmlLine, iStart, iEnd);

 { Extract path }
 src:= ExtractAttribValue(Tag, 'src');     {example: <IMG src="file:///c:/!Joongle%20CMS/Joongle%20SC/My%20web%20site%20(template)/resources/R_screenshot__8556B707__B73E9467.PNG">&nbsp;</P> }

 { Remove 'file:///' }
 Src:= ReplaceString(Src, 'file:///', '');

 { Convert to DOS paths }
 Src:= ReplaceCharF(Src, '/', '\');

 { Convert encoded URL to human URL - convert %20 to space, etc }
 URLDecoder:= TURLEncoding.Create;
 src:= URLDecoder.Decode(src);
 FreeAndNil(URLDecoder);

 { Make path relative }
 src:= System.SysUtils.ExtractRelativePath(RelativeTo, src);

 { convert back to Linux paths }
 Src:= Convert2LinuxPath(Src);

 { Encode }
 src:= UrlEncode(src);

 { put path back in tag }
 NewTag:= ReplaceAttribValue(Tag, 'src', src);

 { Put tag back in HtmlLine }
 if iStart > 1
 then BeforeTag:= system.copy(HtmlLine, 1, iStart-1)
 else BeforeTag:= '';

 if iEnd < Length(HtmlLine)
 then AfterTag:= system.copy(HtmlLine, iEnd+1, MaxInt)
 else AfterTag:= '';

 Result:= BeforeTag+ NewTag + AfterTag;
end;






{ Locates ONLY THE FIRST <img> tag in a HTML line and converts its SRC (path) from relative path to full path
  DOES NOT WORK if the line has more than one IMG tag }
function MakeImgFullPath(CONST HtmlLine, Base: string): string;
VAR
   NewTag, Src, Tag, BeforeTag, AfterTag: string;
   iEnd, iStart: Integer;
   URLDecoder: TURLEncoding;
begin
 { Extracts <IMG> tag }
 iStart:= PosInsensitive('<img', HtmlLine);
 if iStart< 1 then EXIT(HtmlLine);

 iEnd:= System.StrUtils.PosEx('>', HtmlLine, iStart+1);
 if iEnd < 1 then EXIT(HtmlLine);

 Tag:= CopyTo(HtmlLine, iStart, iEnd);

 { Extract path }
 src:= ExtractAttribValue(Tag, 'src');     {example: <IMG src="file:///c:/!Joongle%20CMS/Joongle%20SC/My%20web%20site%20(template)/resources/R_screenshot__8556B707__B73E9467.PNG">&nbsp;</P> }

 { Remove 'file:///' }
 Src:= ReplaceString(Src, 'file:///', '');

 { Convert to DOS paths }
 Src:= Convert2DosPath(Src);

 { Convert encoded URL to human URL - convert %20 to space, etc }
 URLDecoder:= TURLEncoding.Create;
 src:= URLDecoder.Decode(src);
 FreeAndNil(URLDecoder);

 { Make path relative }
 src:= System.IOUtils.TPath.Combine(base, src);

 { convert back to Linux paths }
 Src:= Convert2LinuxPath(Src);

 { Encode }
 src:= UrlEncode(src);
 src:= 'file:///'+ src;

 { put path back in tag }
 NewTag:= ReplaceAttribValue(Tag, 'src', src);

 { Put tag back in HtmlLine }
 if iStart > 1
 then BeforeTag:= system.copy(HtmlLine, 1, iStart-1)
 else BeforeTag:= '';

 if iEnd < Length(HtmlLine)
 then AfterTag:= system.copy(HtmlLine, iEnd+1, MaxInt)
 else AfterTag:= '';

 Result:= BeforeTag+ NewTag + AfterTag;
end;










end.(*=================================================================================================================




{ Extracts all <IMG> tags from a HTML
  ISSUE: Does not work if the tag is split on two lines! }
{
function ExtractIMGTags(HTMLBody: TStringList): TStringList;
VAR
   Tag, s, CurLine: string;
   iEnd, iStart: Integer;
begin
 Result:= TStringList.Create;
 for CurLine in HTMLBody DO
  begin
   iStart:= PosInsensitive('<img', CurLine);
   if iStart> 0 then
    begin
     s:= Copy(CurLine, iStart, High(integer));
     iEnd:= Pos('>', s);
     if iEnd> 0 then
      begin
       Tag:= CopyTo(s, 1, iEnd);
       Result.Add(Tag);
      end;
    end;
  end;
end;   }






TESTER:

procedure TForm5.Button1Click(Sender: TObject);
CONST HtmlFile= 'c:\MyProjects\BX\SourceCode\Tester StripWebPage\testerLightVcl.Internet.HTMLImg\2.html';
VAR
   Tags: TStringList;
   HtmlBody: TStringList;
   Tag: string;
   ImgTag: TImgTag;
begin
 HtmlBody:= ReadFileTSL(HtmlFile);
 Tags:= ExtractImgTags(HtmlBody);

 for Tag in Tags DO
  begin
   ImgTag:= ExtractImgTagData(Tag);
   Log.Lines.Add(ImgTag.URL+ '  Width='+ IntToStr(ImgTag.Width)+ '  Height='+ IntToStr(ImgTag.Height));
  end;

 FreeAndNil(Tags);
 FreeAndNil(HtmlBody);
end;














{ UNUSED }
{ Extracts <IMG> tags from a HTML file }  (*
function extractImgTags(HtmlBody: string): string;
VAR
   Tag, LowBody: string;
   Offset, TextLen, iEnd, iStart1: Integer;
begin
 Result:= '';
 TextLen:= Length(HtmlBody);
 LowBody:= lowercase(HtmlBody);
 Offset:= 1;

 REPEAT
   iStart1:= PosEx('<img ', LowBody, Offset);

   if iStart1> 0 then
    begin
      iEnd:= PosEx('>', HtmlBody, iStart1+7);
      if (iEnd> 0)
      then
       begin
        Tag:= CopyTo(HtmlBody, iStart1, iEnd);
        if Tag = ''
        then EmptyDummy; // raise exception.Create('Empty tag!');
        Result:= Result+ Tag+ crlf;
        offset:= iEnd+1;
       end
      else
       Offset:= iStart1+ 3;
    end
   else Offset:= textlen;
  UNTIL Offset>= TextLen;

  RemoveLastEnter(Result);
end;


function ExtractImgTagsTSL(HtmlBody: string): TStringList;  { UNUSED }
begin
 Result:= TStringList.Create;
 Result.Text:= ExtractImgTags(HtmlBody);
end;   *)








{--------------------------------------------------------------------------------------------------
  Extract links to images from a list of <a href> tags.
    Input: a list of <a href> tags.
    Output: a list URLs pointing to images.
  Example:  <a href="www.tst.com/1.jpg"> returns 'www.tst.com/1.jpg'
--------------------------------------------------------------------------------------------------}
(*UNUSED

instead of this, use:LightVcl.Internet.ExtractAttribValue


function extractLinkedImges(TagList: TStringList): TStringList;
VAR
   URL, CurLine: string;
   QStart, QEnd: Integer;
begin
 Result:= TStringList.Create;

 for CurLine in TagList DO
  begin
   QStart:= PosInsensitive('href', CurLine);
   if QStart> 0 then
    begin
     QStart:= FindQuoteStart(CurLine, QStart+1);                                                       { find first " sign }
     if QStart> 0 then
      begin
       QEnd:= FindQuoteEnd(CurLine, QStart+1);
       if QEnd> 0 then
        begin
         URL:= CopyTo(CurLine, QStart+1, QEnd-1);                                                  { +1 -1 to remove the " signs }
         if LightCore.IO.IsImage(URL)
         then Result.Add(URL);
        end;
      end;
    end;
  end;
end;



{--------------------------------------------------------------------------------------------------
  Extract links to images from all <a href> tags in a HTML file.
  Output: a list URLs

  Example:  <a href="www.tst.com/1.jpg"> returns 'www.tst.com/1.jpg'

  UNUSED!!!!!!!!!!!!!!!
--------------------------------------------------------------------------------------------------}
function extractLinkedImges(HtmlBody: string): TStringList;
VAR AHRefTags: TStringList;
begin
 raise exception.Create('extractLinkedImges function is obsolete. UseLightVcl.Internet.HTMLImg.ExtractImagesFromAHREF instead.');

 AHRefTags:= ExtractAhrefTags(HtmlBody);
 TRY
   //Result:= extractLinkedImges(AHRefTags);
 FINALLY
   FreeAndNil(AHRefTags);
 END;
end;

*)






{ Extracts <IMG> tags from a HTML file
  UNUSED
  Problem: does not work when there are multiple <img tags on the same line!!!
  }
function ExtractImgTags_old(HTMLBody: string): TStringList;
VAR
   Tag, s, CurLine: string;
   iEnd, iStart: Integer;
begin
 Result:= TStringList.Create;
 for CurLine in HTMLBody DO
  begin
   iStart:= PosInsensitive('<img', CurLine);
   if iStart> 0 then
    begin
     s:= system.copy(CurLine, iStart, High(integer));
     iEnd:= Pos('>', s);
     if iEnd> 0 then
      begin
       Tag:= CopyTo(s, 1, iEnd);
       Result.Add(Tag);
      end;
    end;
  end;
end;


