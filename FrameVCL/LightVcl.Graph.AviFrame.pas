UNIT LightVcl.Graph.AviFrame;

{=============================================================================================================
   Gabriel Moraru
   2023.08.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   Obtains a frame from the middle of a video file.
   Requires the FFVCL 3rd party library.

   As decoder it uses FFVCL
        www.delphiffmpeg.com  ($199)
        Support@DelphiFFmpeg.com
        c:\Myprojects\Projects GRAPH-Video\lib-FFVCL\2020\FFVCL_Trial\
   Replaces the old cFrameServerVFW

   Advantage:
      Real-time, onthe fly video decoding.
      No video frames must be written to disk first.
      Fast
   Disadvantage:
      The library (DLLs) is 46MB!

   Documentation:
      http://www.delphiffmpeg.com/interface-ffplayer.html
      in c:\Users\trei\Documents\FFVCL_Demos\Player\PlayerFrm.pas
      see:
         SpeedChange
         cboAspectRatioChange
         FFPlayer.mute
         FFPlayer.Seek
         if FFPlayer.CurrentFrame(BMP, PTS)
         FFPlayer.TryOpen(URL, LScreenHandle, chkOpenPaused.Checked);
         change brightness range (-10 - 10) FBrightness := trbBrightness.Position / 10; FFPlayer.SendVideoFilterCommand('hue', 'b', FloatToStr(FBrightness), 0);

         if FFPlayer.Paused then FFPlayer.Resume else FFPlayer.Pause;
         FFPlayer.OnOpenFailed := FFPlayerOpenFailed;
         FFPlayer.OnPosition   := FFPlayerPosition;

     See:
        LightCore.IO.IsVideo() and LightCore.IO.IsVideoGeneric()

   Tester:
      See cVideoAnimator.pas

  FFVCL Installation:
    The FFMpeg package needs to be installed: c:\Users\Public\Documents\Embarcadero\Studio\22.0\Bpl\FFmpeg_DXT4.bpl
    Seach path must include: c:\Myprojects\Packages\FFVCL\FFVCL Lite\DCU_DXT4\Win32\

-------------------------------------------------------------------------------------------------------------}
INTERFACE

USES
  System.SysUtils, Vcl.Graphics;

CONST
  VideoFiles     = '*.AVI;*.MKV;*.MPEG;*.MP4;*.MP;*.MPG;*.WMV;*.VOB;*.ASF;*.OGM;*.AVS;*.MOV;*.3GP;*.RM;*.RMVB;*.NSV;*.TP;*.TS;*.FLV;*.DAT;*.AVM';
  VideoFilesFtl  = 'Video Files|' + VideoFiles;


function GetVideoPlayerLogo: TBitmap;


IMPLEMENTATION
USES
   LightVcl.Graph.Bitmap, LightVcl.Graph.Loader, LightCore.AppData, LightVcl.Common.AppData
;



{ The free FFVCL library does not support frame capture so we fake it.
  Instead of a real video frame we show in icon/logo representing a video camera. }
function GetVideoPlayerLogo: TBitmap;   // Old name: ExtractMiddleFrame
begin
 Result:= LightVcl.Graph.Bitmap.CreateBlankBitmap(192, 128, clBlack);   // 234x174 is the size of the Preview window in BX
 VAR AviLogo:= LightVcl.Graph.Loader.LoadGraph(AppData.SysDir+ 'video_player_icon.png', FALSE, TRUE);
 TRY
   //Result.Canvas.Draw(10, 10, AviLogo);
   LightVcl.Graph.Bitmap.CenterBitmap(AviLogo, Result);
 FINALLY
   FreeAndNil(AviLogo);
 END;


 CONST Text: string= 'Video file';

 Result.Canvas.Brush.Color:= clBlack;
 Result.Canvas.Font.Name  := 'Verdana';
 Result.Canvas.Font.Size  := 9;
 Result.Canvas.Font.Color:= clLime;
 Result.Canvas.TextOut((Result.Width- Result.Canvas.TextWidth(Text)) DIV 2, 4, Text);

 //FrameCount:= 2; { Fake it }
end;


{ All the things below are useless because the lite version will not support frame capture.
  If you want to see how to open videos see c:\Myprojects\BIONIX\SourceCode\BioniX VCL\cFrameServerAVI.pas }

(*
TYPE
  TAspectRatio = (arOriginal=-1, arFitScreen);

  TFrameServerFF = class(TObject)
  private
    AspectRatio: TAspectRatio;
    FileName: string;
    FFPlayer: TFFPlayer;
    procedure FFPlayerFileOpen(Sender: TObject; const ADuration: Int64; AFrameWidth, AFrameHeight: Integer; var AScreenWidth, AScreenHeight: Integer);
  public
    FrameHeight : Integer;
    FrameWidth  : Integer;
    FrameCount  : Integer;
    LibraryPath : string;  { Path to the FFMpeg decoder DLLs }
    constructor Create;
    destructor Destroy; override;

    function  Open(aFileName: string): Boolean;
    procedure CaptureFrame;             // Doesn't work in the Lite version.
 end;


function GetVideoPlayerLogo(FileName: string; OUT FrameCount: Cardinal): TBitmap;


IMPLEMENTATION
USES
   LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.SystemTime, LightVcl.Common.Clipboard, LightVcl.Common.Dialogs, LightCore.INIFile, LightVcl.Common.AppDataForm, LightVcl.Graph.Bitmap, LightVcl.Graph.Loader, LightCore.IO, LightCore.TextFile, LightVcl.Common.IO;





constructor TFrameServerFF.Create;
begin
 inherited Create;

 AspectRatio:= arOriginal;
 FFPlayer:= TFFPlayer.Create(NIL);
 FFPlayer.RepeatType:= rtLoop;
 FFPlayer.OnFileOpen:= FFPlayerFileOpen;
 FFPlayer.DisableFPUExceptions;   // Disable all fpu exceptions(floating point exceptions): invalid operation, denormalized, divide by zero, overflow, underflow, inexact/precision
 //FFPlayer.ScreenWidth := 400;   //????????
end;


destructor TFrameServerFF.Destroy;
begin
 WITH FFPlayer DO
  begin
    OnPosition  := nil;  // Clear the event handlers
    OnState     := nil;
    OnVideoHook := nil;
    OnAudioHook := nil;
  end;

 FreeAndNil(FFPlayer);
 inherited Destroy;
end;





function TFrameServerFF.Open(aFileName: string): Boolean;                                { GIF frames are stored in RAM or to disk }
begin
 Result:= TRUE;
 FrameCount:= 2;
 FileName  := aFileName;
 Assert(LibraryPath <> '');

 { Load dynamic link libraries }
 if not FFPlayer.AVLibLoaded then
   if not FFPlayer.LoadAVLib(LibraryPath) then
   begin
     MessageError(FFPlayer.LastErrMsg);
     EXIT(FALSE);
   end;

 // FFPlayer.TryOpen(FileName, DrawForm);
end;


procedure TFrameServerFF.FFPlayerFileOpen(Sender: TObject; const ADuration: Int64; AFrameWidth, AFrameHeight: Integer; var AScreenWidth, AScreenHeight: Integer);
begin
 FrameHeight := AFrameWidth;
 FrameWidth  := AFrameHeight;
 FrameCount  := ADuration DIV 24;  //24 fps

 // Change aspect ratio
 case AspectRatio of
   arFitScreen: FFPlayer.AspectRatio := -1;  //   < 0 -> scaling to fit screen
   arOriginal : FFPlayer.AspectRatio := 0;   //   = 0 -> keeping original
 end;

 { Mesaj(Format('duration: %s,   frame size: %dx%d,   screensize: %dx%d ',  //screensize:= 0x0 [ IntToStr(ADuration), AFrameWidth, AFrameHeight, AScreenWidth, AScreenHeight]));  }
end;

(*
function TFrameServerFF.Start(BlankDesktop: Boolean): Boolean;
begin
 inherited Start(BlankDesktop);
 // try to open and play media file, render on the custom window specified by handle
 Result:= FFPlayer.Open(FileName, FDrawingForm.Handle);
 if NOT Result
 then AppData.LogError(FFPlayer.LastErrMsg);
end;


procedure TFrameServerFF.Stop;
VAR Playling: Boolean;
begin
 Playling:= FFPlayer.PlayState = psPlay;
 FFPlayer.Stop(TRUE);

 { Super dirty trick to fix crash because of FFVCL.
   https://stackoverflow.com/questions/64550585/edbkerror-unable-to-access-debug-process-memory-only-part-of-a-readprocessmemo?noredirect=1#comment114144119_64550585}
 if Playling
 then Sleep(1500);
end;


FFPlayer.StepToNextFrame;
* )


{ Screenshot. Does not work in FFVCL trial version }
procedure TFrameServerFF.CaptureFrame;
var
  BMP: TBitmap;
  PTS: Int64;
begin
  if FFPlayer.CurrentFrame(BMP, PTS)
  then BMP.SaveToFile(AppData.ExeFolder+ '\capture' + IntToStr(PTS) + '.bmp');
end;





(*
Here I was trying to copy the canvas of the video form, but it doesn't work. It just returns a white image
VAR Timer: Vcl.ExtCtrls.TTimer;

procedure TFrameServerFF.TimerTimer(Sender: TObject);
VAR BMP: TBitmap;
begin
   BMP:= TBitmap.Create;
   TRY
    BMP.SetSize(DrawingForm.Width, DrawingForm.Height);
    DrawingForm.Canvas.CopyRect(BMP.Canvas.ClipRect, BMP.Canvas, BMP.Canvas.ClipRect);
    BMP.SaveToFile(AppData.ExeFolder+ TimeToStr_IO(now));
   FINALLY
    FreeAndNil(BMP);
   END;
end;

function TFrameServerFF.Start: Boolean;
begin
 ...
 timer:= TTimer.Create(NIL);
 Timer.Enabled:= FALSE;
 Timer.Interval:= 1000;
 Timer.OnTimer:= TimerTimer;

 if VideoOpened
 then Timer.Enabled:= TRUE
 else AppData.LogError(FFPlayer.LastErrMsg);
end;

*)

end.








