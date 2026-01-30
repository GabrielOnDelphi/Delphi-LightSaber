UNIT LightVcl.Graph.Gif;

{=============================================================================================================
   Gabriel Moraru
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
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
    FFrameDelay: Integer;
    FFrameCount: Cardinal;
    FileName: string;
    GIFImg  : TGIFImage;
    Renderer: TGIFRenderer;
    procedure saveFrame (Frame: TBitmap; FrameNo: Integer; OutputFolder: string);
  public


    constructor Create;
    destructor Destroy; override; 

    { Utils }
    function  Open(aFileName: string): Boolean;
    function  SaveFrames(OutputFolder: string): Boolean;
    function  ExtractFrame(FrameNo: Cardinal): TBitmap;

    // Output
    property FrameDelay: Integer  read FFrameDelay;
    property FrameCount: Cardinal read FFrameCount;
 end;


 function IsAnimated        (CONST AGraphFile: string): Boolean;
 function ExtractMiddleFrame(CONST FileName: string; OUT FrameCount: Cardinal): TBitmap;


IMPLEMENTATION

USES
  GifProperties {External lib},
  LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.Dialogs, LightCore.AppData, LightCore.IO;


constructor TGifLoader.Create;
begin
 inherited Create;
 GIFImg:= TGIFImage.Create;
end;


destructor TGifLoader.Destroy;
begin
 FreeAndNil(GIFImg);
 FreeAndNil(Renderer);
 inherited Destroy;
end;


function TGifLoader.Open(aFileName: string): Boolean;  { GIF frames are stored in RAM or to disk }
begin
 FFrameCount:= 0;
 Result    := TRUE;
 FileName  := aFileName;

 { Load GIF }
 TRY
   GIFImg.Animate := FALSE;                            { This is set to False by default anyway! }
   GIFImg.LoadFromFile(FileName);
 EXCEPT
   on E: Exception do
    begin
     AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
     EXIT(FALSE);    { Do not crash on failure }
    end;
 END;

 { STATIC GIF? }
 FFrameCount:= GIFImg.Images.Count;
 if FrameCount <= 1 then
  begin
   AppDataCore.LogError('This is not an animated GIF: '+ FileName);
   EXIT(FALSE);
  end;

 { GIF render }
 FreeAndNil(Renderer);                    { Destroy it so we can created it with a new parameter }
 Renderer:= TGIFRenderer.Create(GIFImg);
 Renderer.Animate:= TRUE;  { CRITICAL: Animate must be TRUE or TGIFRenderer.Draw will cause access violations. See: stackoverflow.com/questions/36444024 }

 { Obtain frame delay }
 VAR TempBMP:= TBitmap.Create;
 TRY
  TempBMP.SetSize(GIFImg.Width, GIFImg.Height);
  Renderer.Draw(TempBMP.Canvas, TempBMP.Canvas.ClipRect); { CRITICAL: Must paint first frame to get valid FrameDelay value }
  Renderer.FrameIndex:= 0;                                { We return to the first frame }
  FFrameDelay:= Renderer.FrameDelay;
 FINALLY
  FreeAndNil(TempBMP);
 END;
end;






{todo 5: save as PNG }
procedure TGifLoader.saveFrame(Frame: TBitmap; FrameNo: Integer; OutputFolder: string);   { Save frame to disk }
VAR
   CurFile, sCounter: string;
begin
 sCounter:= LightCore.LeadingZeros(IntToStr(FrameNo), 6);
 CurFile:= OutputFolder+ ExtractOnlyName(FileName)+ sCounter+ '.bmp';
 Frame.SaveToFile(CurFile);
end;



{ Saves all frames of the loaded GIF to individual BMP files in OutputFolder.
  Returns True if successful, False if no frames to save or Renderer not initialized. }
function TGifLoader.SaveFrames(OutputFolder: string): Boolean;
VAR
   i: Integer;
   BMP: TBitmap;
begin
 Result:= (FrameCount > 1) AND (Renderer <> NIL);
 if Result then
  begin
   BMP:= TBitmap.Create;
   TRY
     BMP.SetSize(GIFImg.Width, GIFImg.Height);

     { Save each frame to disk }
     for i:= 0 to FrameCount-1 DO
      begin
       if Renderer.Frame.Empty then Continue;
       Renderer.Draw(BMP.Canvas, BMP.Canvas.ClipRect);
       Renderer.NextFrame;
       SaveFrame(BMP, i, OutputFolder);
      end;

      { Sanity check }
      if FrameCount < 1
      then AppDataCore.LogWarn('GIF frame count mismatch!');
   FINALLY
    FreeAndNil(BMP);
   END;
  end;
end;








{ Extracts a specific frame from the loaded GIF.
  Returns the frame as TBitmap or NIL if frame is invalid or unavailable.
  Caller is responsible for freeing the returned bitmap. }
function TGifLoader.ExtractFrame(FrameNo: Cardinal): TBitmap;
begin
 if Renderer = NIL then
  begin
   AppDataCore.LogError('ExtractFrame: Renderer not initialized. Call Open() first.');
   EXIT(NIL);
  end;

 if FrameNo >= FrameCount then
  begin
   MessageWarning('Invalid frame number. Total frames in this GIF: '+ IntToStr(FrameCount));
   EXIT(NIL);
  end;

 Assert(Renderer.Frame <> NIL, 'No frame in renderer!');
 if Renderer.Frame.Empty
 then EXIT(NIL);

 Result:= TBitmap.Create;
 Result.SetSize(GIFImg.Width, GIFImg.Height);

 Renderer.FrameIndex:= FrameNo;  { Note: Remember to reset to frame zero if necessary }
 Renderer.Draw(Result.Canvas, Result.Canvas.ClipRect);
end;



{-------------------------------------------------------------------------------------------------------------
  Returns True if the input file is a video or an animated GIF (returns False for static GIFs).
  Uses IsAnimatedGif from GifProperties for GIF detection.

  Performance: 100MB animated GIF takes ~4.1s to analyze.

  See: stackoverflow.com/questions/59010649/how-to-detect-animated-gif
  Tester: c:\Myprojects\Project Testers\gr GIF frame counter\Tester.dpr
-------------------------------------------------------------------------------------------------------------}
function IsAnimated(CONST AGraphFile: string): Boolean;
begin
 if IsGIF(AGraphFile)
 then Result:= IsAnimatedGif(AGraphFile)
 else Result:= LightCore.IO.IsVideo(AGraphFile);
end;


{ Extracts the middle frame from an animated GIF file.
  Returns the frame as TBitmap or NIL if extraction fails.
  FrameCount is set to the total number of frames, or 0 on failure.
  Caller is responsible for freeing the returned bitmap. }
function ExtractMiddleFrame(CONST FileName: string; OUT FrameCount: Cardinal): TBitmap;
VAR
   MidFrame: Integer;
   Gif: TGifLoader;
begin
 Result:= NIL;
 FrameCount:= 0;  { Initialize OUT parameter for failure case }

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

