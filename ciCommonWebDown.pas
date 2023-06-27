UNIT ciCommonWebDown;

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2023.06
   See Copyright.txt

  Common WebSite Downloader
  This tool will locate the high resolution images on webpages of popular websites like: Unsplash.com, Pexels, pixabay.com, wallpaperscraft.com (work in progress)

  Unsplash.com
    The user provides the URL of a web page that holds an image.
    The program will try to automatically locate the high res image from that page.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, ccCore;

function GetUnsplashImage(URL, LocalFile: string): Boolean;

IMPLEMENTATION

USES ciDownload, ciHTML;






{--------------------------------------------------------------------------------------------------
   UNSPLASH
--------------------------------------------------------------------------------------------------}
{ Receive the URL of a Unsplash.com webpage and extracts the main image from it.
  Returns the file name of the file downloaded to disk }
function GetUnsplashImage(URL, LocalFile: string): Boolean;
VAR
   HighResImg, Tag, HTML: string;
begin
 HTML:= GetTextFile(URL);
 if HTML = '' then EXIT(FALSE);

 Tag:= ciHTML.EctractLine(HTML, '<meta data-react-helmet="true" property="og:image"');    //find: <meta data-react-helmet="true" property="og:image"
 HighResImg:= ExtractAttribValue(Tag, 'content');
 HighResImg:= CopyTo(HighResImg, 1, '?', FALSE);

 //FullPath:= LocalFolder+ extractfileext(HighResImg);
 Result:= DownloadFile(HighResImg, URL, LocalFile);
end;





{Tester:

// original page: https://unsplash.com/photos/0QkyEoTQ6Pc
// final image:   https://images.unsplash.com/photo-1542291771-64f962701184?ixlib=rb-1.2.1&

procedure TfrmTester.btnStartClick(Sender: TObject);
VAR FileName, URL: string;
begin
 CursorBusy;
 TRY
  URL:= GetImageFromUnsplashPage(Edit1.Text);
  lblInfo.Caption:= 'High res image: '+ URL;

  FileName:= AppData.CurFolder+ GenerateUniqueString(5)+ '.jpg';
  DownloadFile(URL, URL, FileName);
  Image.Picture.LoadFromFile(FileName);
 FINALLY
  CursorNotBusy;
 END;
end;  }



end.
