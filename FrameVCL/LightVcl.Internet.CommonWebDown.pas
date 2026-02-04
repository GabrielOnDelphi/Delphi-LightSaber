UNIT LightVcl.Internet.CommonWebDown;

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt

  Common WebSite Downloader
  This tool will locate the high resolution images on webpages of popular websites like: Unsplash.com, Pexels, pixabay.com, wallpaperscraft.com (work in progress)
  Unsplash.com
    The user provides the URL of a web page that holds an image.
    The program will try to automatically locate the high res image from that page.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, LightCore;

function GetUnsplashImage(CONST URL, LocalFile: string): Boolean;

IMPLEMENTATION

USES LightCore.Download, LightCore.HTML;






{--------------------------------------------------------------------------------------------------
   UNSPLASH
--------------------------------------------------------------------------------------------------}
{ Receives the URL of an Unsplash.com webpage and extracts the main high-resolution image from it.
  The function parses the HTML to find the og:image meta tag which contains the image URL.
  Parameters:
    URL       - The Unsplash page URL (e.g., https://unsplash.com/photos/xxxxx)
    LocalFile - Full path where the downloaded image will be saved
  Returns:
    True if the image was successfully downloaded, False otherwise }
function GetUnsplashImage(CONST URL, LocalFile: string): Boolean;
VAR
   HighResImg, Tag, HTML, ErrorMsg: string;
begin
 Assert(URL <> '', 'GetUnsplashImage: URL is empty');
 Assert(LocalFile <> '', 'GetUnsplashImage: LocalFile is empty');

 HTML:= DownloadAsString(URL);
 if HTML = ''
 then EXIT(FALSE);

 // Find the og:image meta tag which contains the high-resolution image URL
 Tag:= LightCore.HTML.ExtractLine(HTML, '<meta data-react-helmet="true" property="og:image"');
 if Tag = ''
 then EXIT(FALSE);

 // Extract the image URL from the content attribute
 HighResImg:= ExtractAttribValue(Tag, 'content');
 if HighResImg = ''
 then EXIT(FALSE);

 // Remove URL parameters (everything after '?') to get the clean image URL
 HighResImg:= CopyTo(HighResImg, 1, '?', FALSE);

 DownloadToFile(HighResImg, LocalFile, ErrorMsg);
 Result:= ErrorMsg = '';
end;





{Tester:

// original page: https://unsplash.com/photos/0QkyEoTQ6Pc
// final image:   https://images.unsplash.com/photo-1542291771-64f962701184?ixlib=rb-1.2.1&

procedure TfrmTester.btnStartClick(Sender: TObject);
VAR FileName, URL: string;
begin
 LightVcl.Common.System.CursorBusy;
 TRY
  URL:= GetImageFromUnsplashPage(Edit1.Text);
  lblInfo.Caption:= 'High res image: '+ URL;

  FileName:= Appdata.AppFolder+ GenerateUniqueString(5)+ '.jpg';
  [DownloadFile->new name] DownloadToFile(URL, URL, FileName);
  Image.Picture.LoadFromFile(FileName);
 FINALLY
  CursorNotBusy;
 END;
end;  }



end.
