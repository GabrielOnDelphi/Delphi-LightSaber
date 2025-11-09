UNIT LightVcl.Graph.Loader.Thread;

{=============================================================================================================
   Gabriel Moraru
   2023.08.05
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
  Loads multiple images in separate threads.
  Images are automatically resized to Width, Height.
  When the thumbnail of an image is ready, it is put in a Queue by PushPicture which
    also informs the caller that the thumbnail is ready (via a WM_THUMBNAIL_NOTIFY signal)
  The caller gets the picture from queue using PopThumb (the image is deleted from queue).
  The caller is responsible to free the thumbnail.

  HOW TO USE IT:

     BkgThread            := TBkgImgLoader.Create(Handle);
     BkgThread.Priority   := tpLower;
     BkgThread.FileList   := GetFolderContent("c:\images\");
     BkgThread.Height     := 800;
     BkgThread.Width      := 600;
     BkgThread.ResamplerQual:= ResamplerQual;
     BkgThread.Start;

  The result is sent back to the application via a WM_THUMBNAIL_NOTIFY message/notification:

     procedure TForm1.WMThumbnailReady(VAR AMessage: TMessage);                                   Receive notifications from worker thread each time a thumbnail is ready
     begin
       BMP:= BkgThread.PopPicture;
       Show(BMP);
       FreeAndNil(BMP);
     end;

  We can put a single file in FileList and process onyl that file.

  Used by: LightVcl.Visual.ThumbViewerM
--------------------------------------------------------------------------------------------------}

INTERFACE

USES
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, Generics.Collections, System.SyncObjs,
  Vcl.Forms, Vcl.Graphics;

CONST
   WM_THUMBNAIL_NOTIFY = WM_APP + 1;

TYPE
  TBkgImgLoader = class(TThread)
  private
    FWndHandle : HWND;
    ReadyThumbs: TQueue<TBitmap>;
    FQueueLock : TCriticalSection;
    Fexception : exception;
    procedure DoHandleexception;    
    procedure PushPicture(APicture: TBitmap);
    procedure ProcessFile(const AFileName: string);
  protected
    procedure Execute; override;
    procedure Handleexception; virtual;
  public
    SilentErrors: Boolean;                        { Silent: don't show errors in case the image cannot beloaded }
    Width: Integer;
    Height: Integer;
    FileList: TStringList;
    constructor Create(CONST AWndHandle: HWND);   { Handler to the caller window. The thread will inform the caller that a new thumbnail is ready via this handle }
    destructor Destroy; override;

    function  PopPicture: TBitmap;
  end;



IMPLEMENTATION
USES LightVcl.Graph.Resize, LightVcl.Graph.ResizeVCL;




{--------------------------------------------------------------------------------------------------
  THREAD CREATE/DESTROY
--------------------------------------------------------------------------------------------------}

constructor TBkgImgLoader.Create(CONST AWndHandle: HWND);                                          { Handler to the caller window. The thread will inform the caller that a new thumbnail is ready via this handle }
begin
 inherited Create(TRUE);                                                                           { Create suspended }
 FWndHandle      := AWndHandle;
 ReadyThumbs     := TQueue<TBitmap>.Create;
 FQueueLock      := TCriticalSection.Create;
 SilentErrors    := False;    { Silent: don't show errors in case the image cannot beloaded }
 Width           := 256;
 Height          := 128;
 //ResamplerQual   := 1;  // corect ?????????
 FileList        := nil;
 FreeOnTerminate := FALSE;                                                                         { You do not need to clean up after termination. }
 Priority        := tpLower;
end;


destructor TBkgImgLoader.Destroy;
begin
 FreeAndNil(FQueueLock);
 FreeAndNil(ReadyThumbs);
 inherited Destroy;
end;





{--------------------------------------------------------------------------------------------------
   THREAD EXECUTE
--------------------------------------------------------------------------------------------------}

procedure TBkgImgLoader.Execute;
VAR CurFile: string;
begin
 Fexception := NIL;
 Assert(FileList<> nil, 'TBkgImgLoader - FileList is nil!');

 TRY
   for CurFile in FileList DO
    TRY
      if Terminated then EXIT;
      ProcessFile(CurFile);
    except
      //todo 1: trap only specific exceptions
      Handleexception;                                                  { Borland documentation: The Execute method must catch all exceptions that occur in the thread. If you fail to catch an exception in your thread function, your application can cause access violations. This may not be obvious when you are developing your application, because the IDE catches the exception, but when you run your application outside of the debugger, the exception will cause a runtime error and the application will stop running.  }
    END;
 FINALLY
   FreeAndNil(FileList);
 END;
end;


procedure TBkgImgLoader.ProcessFile(const aFileName: string);
VAR BMP: TBitmap;
begin
 if Terminated then EXIT;                                              { are asta vreun sens aici }
 if NOT FileExists(AFileName) then EXIT;                               { I think I need to push a NIL bmp so the caller will know that is something wrong with this file }

 TRY
  BMP:= LoadAndStretch(AFileName, Width, Height);

  { Push thumbnail in queue }
  if (BMP <> NIL) then
    if Terminated
    then FreeAndNil(BMP)                                               { The caller won't receive this bitmap so it cannot free it. We have to free it. }
    else PushPicture(BMP);

 EXCEPT
   //todo 1: trap only specific exceptions
   FreeAndNil(BMP);                                                     { Release the image ONLY in case of an error }
   if NOT SilentErrors then RAISE;
 END;
end;





{--------------------------------------------------------------------------------------------------
   exception HANDLING
--------------------------------------------------------------------------------------------------}

procedure TBkgImgLoader.DoHandleexception;
begin
 if GetCapture <> 0
 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);                    // Cancel the mouse capture

 { Now actually show the exception }
 if Fexception is exception
 then Application.Showexception(Fexception)
 else System.SysUtils.Showexception(Fexception, nil);
end;


procedure TBkgImgLoader.Handleexception;                               // This function is virtual so you can override it and add your own functionality.
begin
 Fexception := exception(exceptObject);
 TRY
   if NOT (Fexception is EAbort)                                       // Don't show EAbort messages
   then Synchronize(DoHandleexception);
 FINALLY
   Fexception := NIL;
 end;
end;





{--------------------------------------------------------------------------------------------------
   PUSH RESULTS TO QUEUE
--------------------------------------------------------------------------------------------------}

procedure TBkgImgLoader.PushPicture(APicture: TBitmap);     { Push thumbnail in queue }
begin
 FQueueLock.Enter;
 TRY
   ReadyThumbs.Enqueue(APicture);
 FINALLY
   FQueueLock.Leave;
 END;
 
 { Sent a message to the main program to let it know a new thumb is ready/available }
 PostMessage(FWndHandle, WM_THUMBNAIL_NOTIFY, 0, 0);
end;


function TBkgImgLoader.PopPicture: TBitmap;
begin
 FQueueLock.Enter;
 TRY 
   Result := ReadyThumbs.Dequeue;
 FINALLY
   FQueueLock.Leave;
 end;
end;





end.
