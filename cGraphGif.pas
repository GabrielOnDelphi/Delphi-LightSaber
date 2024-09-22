UNIT cGraphGif;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

  Basic GIF functions:
     ExtractFrame
     FrameCount
     FrameDelay

  Uses TGIFRenderer as gif decoder.

  External dependencies:
     * GifParser.pas

  Also see:
     BioniX VCL\cFrameServerGIF.pas

  Also see:
     Play GIF image inside TImage: https://www.youtube.com/watch?v=eHw7SvimazM
     Painting gif frames: http://www.delphigroups.info/2/9/322088.html
     https://stackoverflow.com/questions/44332339/retrieve-the-first-frame-from-a-large-gif-in-optimal-time/44342164#44342164

  Tester:
    See cVideoAnimator.pas
    100mb Animated Elementalist Lux Desktop Background.gif = 4.1s
-----------------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, Vcl.Graphics, Vcl.Imaging.GIFImg;

TYPE
  TGifLoader = class(TObject)
  private
    FileName: string;
    GIFImg  : TGIFImage;
    Renderer: TGIFRenderer;
  public
    { Output }
    FrameDelay: Integer;
    FrameCount: Cardinal;

    constructor Create;
    destructor Destroy; override; 

    { Utils }
    function  Open(aFileName: string): Boolean;
    function  SaveFrames(OutputFolder: string): Boolean;
    procedure SaveFrame (Frame: TBitmap; FrameNo: Integer; OutputFolder: string);
    function  ExtractFrame(FrameNo: Cardinal): TBitmap;
 end;


function IsAnimatedGif     (CONST FileName: string): Integer;
function IsAnimated        (CONST AGraphFile: string): Boolean;

function ExtractMiddleFrame(CONST FileName: string; OUT FrameCount: Cardinal): TBitmap;


IMPLEMENTATION

USES
  GifParser, ccCore, cbDialogs, cbAppData, {cbINIFile,} ccIO, ccTextFile;


constructor TGifLoader.Create;
begin
 inherited Create;
 GIFImg:= TGIFImage.Create;
end;


destructor TGifLoader.Destroy;
begin
 FreeAndNil(GIFImg);
 FreeAndNil(Renderer);
 inherited Destroy;    { First stop the timer }
end;


function TGifLoader.Open(aFileName: string): Boolean;  { GIF frames are stored in RAM or to disk }
begin
 FrameCount:= 0;
 Result    := TRUE;
 FileName  := aFileName;

 { Load GIF }
 TRY
   GIFImg.Animate := FALSE;                            { This is set to False by default anyway! }
   GIFImg.LoadFromFile(FileName);
 EXCEPT
   on E: Exception do
    begin
     AppData.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
     EXIT(FALSE);    { Do not crash on failure }
    end;
 END;

 { STATIC GIF? }
 FrameCount:= GIFImg.Images.Count;
 if FrameCount <= 1 then
  begin
   AppData.LogError('This is not an animated GIF: '+ FileName);
   EXIT(FALSE);
  end;

 { GIF render }
 FreeAndNil(Renderer);                    { Destroy it so we can created it with a new parameter }
 Renderer:= TGIFRenderer.Create(GIFImg);
 Renderer.Animate:= TRUE;  { THIS IS FUCKING IMPORTANT!  Animate must be true!  Details: http://stackoverflow.com/questions/36444024/how-to-extract-frames-from-this-gif-image-access-violation-in-tgifrenderer-dra/36468732#36468732 }

 { Obtain frame delay }
 VAR TempBMP:= TBitmap.Create;
 TRY
  TempBMP.SetSize(GIFImg.Width, GIFImg.Height);
  Renderer.Draw(TempBMP.Canvas, TempBMP.Canvas.ClipRect); { THIS IS FUCKING IMPORTANT! WE NEED TO PAINT FIRST FRAME OTHERWIESE WE DON'T FET A VALID d FrameDelay }
  Renderer.FrameIndex:= 0;                                { We return to the first frame }
  FrameDelay:= Renderer.FrameDelay;
 FINALLY
  FreeAndNil(TempBMP);
 END;
end;






{todo 5: save as PNG }
procedure TGifLoader.SaveFrame(Frame: TBitmap; FrameNo: Integer; OutputFolder: string);   { Save frame to disk }
VAR
   CurFile, sCounter: string;
begin
 sCounter:= ccCore.LeadingZeros(IntToStr(FrameNo), 6);
 CurFile:= OutputFolder+ ExtractOnlyName(FileName)+ sCounter+ '.bmp';
 Frame.SaveToFile(CurFile);
end;



function TGifLoader.SaveFrames(OutputFolder: string): Boolean;  { Stand alone function }
VAR
   i: Integer;
   BMP: TBitmap;
begin
 Result:= FrameCount > 1;  //del Result:= VideoOpened;
 if Result then
  begin
   BMP:= TBitmap.Create;
   TRY
     BMP.SetSize(GIFImg.Width, GIFImg.Height);

     { Save each frame to disk }
     for i:= 0 to FrameCount-1 DO
      begin
       if Renderer.Frame.Empty then Continue;
       Renderer.Draw(BMP.Canvas, BMP.Canvas.ClipRect);                      { Todo: do zoom here! }
       Renderer.NextFrame;
       SaveFrame(BMP, i, OutputFolder);    { Zoom }
      end;

      { Check }
      if FrameCount < 1
      then AppData.LogWarn('AVI frame count mismatch!');
   FINALLY
    FreeAndNil(BMP);
   END;
  end
end;








function TGifLoader.ExtractFrame(FrameNo: Cardinal): TBitmap;
begin
 if FrameNo >= FrameCount then
  begin
   MesajWarning('Invalid frame number. Total frames in this GIF: '+ IntToStr(FrameCount));
   EXIT(NIL);
  end;

 Assert(Renderer.Frame <> NIL, 'No frame in renderer!');
 if Renderer.Frame.Empty
 then EXIT(NIL);

 Result:= TBitmap.Create;
 Result.SetSize(GIFImg.Width, GIFImg.Height);

 Renderer.FrameIndex:= FrameNo;  { Remember to go back to frame zero if neccessary }
 Renderer.Draw(Result.Canvas, Result.Canvas.ClipRect);
end;



{-------------------------------------------------------------------------------------------------------------
  Returns true in the input file is a movie or an animated gif (returns false for static GIFs).
  https://stackoverflow.com/questions/59010649/how-to-detect-animated-gif
  100mb Animated Elementalist Lux Desktop Background.gif = 4.1s

  Tester:
     c:\Myprojects\Project Testers\gr GIF frame counter\Tester.dpr
-------------------------------------------------------------------------------------------------------------}
function IsAnimated(CONST AGraphFile: string): Boolean;
VAR IsAnimGif: Boolean;
begin
 if IsGIF(AGraphFile)
 then IsAnimGif := IsAnimatedGif(AGraphFile) > 1
 else IsAnimGif := FALSE;

 Result:= IsAnimGif OR ccIO.IsVideo(AGraphFile);
end;


function IsAnimatedGif(CONST FileName: string): integer;
VAR
   GIFImg: TGifReader;
begin
 GIFImg := TGifReader.Create;
 TRY
   GIFImg.Read(FileName);
   Result:= GIFImg.FrameIndex; //GifFrameList.Count;
 FINALLY
   FreeAndNil(GIFImg);
 END;
end;




function ExtractMiddleFrame(CONST FileName: string; OUT FrameCount: Cardinal): TBitmap;
VAR
   MidFrame: Integer;
   Gif: TGifLoader;
begin
 Result:= NIL;
 Gif:= TGifLoader.Create;
 TRY
  if Gif.Open(FileName) then
   begin
    FrameCount:= Gif.FrameCount;
    MidFrame:= FrameCount DIV 2;
    Result:= Gif.ExtractFrame(MidFrame);
   end;
 FINALLY
  FreeAndNil(Gif);
 END;
end;



end.

