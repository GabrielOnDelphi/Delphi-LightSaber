UNIT LightVcl.Internet.HTMLWriter;

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file

   Basic objects that allow you to enter miscellaneous HTML parts such as
   keywords, title, and body text and write this data to disk as an HTML file.

   Also see: c:\MyProjects\Packages\Third party packages\uHTMLBuilder.pas
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES System.SysUtils, System.Classes;

{$R *.Res}

TYPE
 THtmlWriter = class(TComponent)
  private
   FSiteRoot     : string;                                                       { site root }
   FTitle        : string;
   FKeywords     : string;
   FDescription  : string;
   FStyles       : string;
   FRobots       : string;
   FHavIcon      : string;
   FBody         : string;
   FContent      : string;
   FExtraHeaders : string;
  protected
   function  GetContent: string;
   procedure AddContent(CONST s: string);                                        { append 's' to the current content without inserting a ENTER }
   procedure AddContentNewLine(CONST s: string);
  public
   constructor Create(AOwner: TComponent); override;
   procedure SaveToFile(CONST FileName: string);
   procedure GenerateContent;
   procedure Reset;
   procedure AddBodyLine(CONST s: string);                                       { add 's' in a new line to the current Body }
   procedure AddBodyLineB(CONST s: string);                                      { add 's' in a new line to the current Body. Also add a <BR> tag }
  published
   property SiteRoot:       string    read FSiteRoot        Write FSiteRoot;
   property hTitle:         string    read FTitle           Write FTitle;
   property hKeywords:      string    read FKeywords        Write FKeywords;
   property hDescription:   string    read FDescription     Write FDescription;
   property hRobots:        string    read FRobots          Write FRobots;
   property hStylesLink:    string    read FStyles          Write FStyles;
   property hHavIconLink:   string    read FHavIcon         Write FHavIcon;
   property hExtraHeaders:  string    read FExtraHeaders    Write FExtraHeaders;
   property hBody:          string    read FBody            write FBody;         { this is the content only between the <BODY> tag }
   property Content:        string    read GetContent;                           { this is the content of the WHOLE html page - obligatoriu trebuie sa apelez 'GenerateContent' inainte de a apela 'Content' }
 end;

procedure Register;

IMPLEMENTATION

USES
   ccCore, ccIO, ccTextFile;


constructor THtmlWriter.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);                                                                         { It has no parent }
 Reset;
end;

procedure THtmlWriter.Reset;
begin
 FContent := '';
 FRobots  := '';
 FBody    := '';
 FStyles  := 'style.css';
 FHavIcon := 'favicon.ico';
 FSiteRoot:= 'http://www.dnabaser.com/';
 hExtraHeaders:= ''; 
end;




{--------------------------------------------------------------------------------------------------
                               WHOLE HTML CONTENT
--------------------------------------------------------------------------------------------------}

procedure THtmlWriter.AddContent(CONST s: string);                                                       { append 's' to the current content without inserting a ENTER }
begin
 FContent:= FContent+ ' '+ s;
end;
procedure THtmlWriter.AddContentNewLine(CONST s: string);
begin
 FContent:= FContent+ CRLF+ s;
end;


procedure THtmlWriter.AddBodyLine(CONST s: string);                                                      { add 's' in a new line to the current Body }
begin
 FBody:= FBody+ CRLF+ s;
end;
procedure THtmlWriter.AddBodyLineB(CONST s: string);                                                     { add 's' in a new line to the current Body. Also add a <BR> tag }
begin
 FBody:= FBody+ CRLF+ s+ '<BR>';
end;


procedure THtmlWriter.GenerateContent;
begin
 FContent:= '';
 AddContentNewLine('<html xmlns="http://www.w3.org/1999/xhtml">');                                 { HEAD }
 AddContentNewLine('<head>');
 AddContentNewLine(' <TITLE>'+ hTitle +'</TITLE>');
 AddContentNewLine(' <META NAME="DESCRIPTION"         content="'+ hDescription+ '" />');
 AddContentNewLine(' <META NAME="KEYWORDS"            content="'+ hKeywords+ '" />');
 if hRobots<> '' then
 AddContentNewLine(' <META NAME="ROBOTS"              content="'+ hRobots+ '" />');
 AddContentNewLine(' <META http-equiv="Content-Type"  content="text/html; charset=iso-8859-1" />');
 AddContentNewLine(' <META http-equiv="Page-Enter"    content="blendTrans(Duration=0.3)" />');
 AddContentNewLine(' <META http-equiv="Page-Exit"     content="blendTrans(Duration=0.3)" />');
 AddContentNewLine(' <LINK href="'+ SiteRoot+hStylesLink+ '"  REL  ="stylesheet"  type="text/css" />');
 AddContentNewLine(' <LINK REL="shortcut icon"            HREF ="'+ SiteRoot+FHavIcon+ '" />');
 if hExtraHeaders<> '' then 
 AddContentNewLine(hExtraHeaders);
 AddContentNewLine('</HEAD>'+LBRK);

 AddContentNewLine('<BODY>');                                                                      { BODY }
 AddContentNewLine(hBody);
 AddContentNewLine('</BODY></HTML>');
end;


function THtmlWriter.GetContent: string;
begin
 Result:= FContent;                                                                                { daca schimb din FContent in Content, se fute si imi prabuseste IDE }
end;


procedure THtmlWriter.SaveToFile(CONST FileName: string);
VAR Folder: string;
begin
 Folder:= ExtractFilePath(FileName);
 if (Folder<>'') AND NOT DirectoryExists(Folder)
 then ForceDirectories(Folder);
 GenerateContent;
 StringToFile(FileName, Content, woOverwrite, wpOff);
end;



{--------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------}
procedure Register;
begin
  RegisterComponents('LightSaber', [THtmlWriter]);
end;


end.
