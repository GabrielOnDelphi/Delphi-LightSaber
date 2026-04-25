UNIT LightFmx.Common.CamUtils;

{=============================================================================================================
   2026.04.25
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Android Camera and Gallery Utilities

   Usage Instructions:
   1. Add necessary permissions to Android manifest:
        <uses-permission android:name="android.permission.READ_MEDIA_IMAGES" /> (for Android 13+)
        or <uses-permission android:name="android.permission.READ_EXTERNAL_STORAGE" /> for older.
   2. Before calling PickImageFromGallery, request permission:
        RequestStorageReadPermission(procedure begin PickImageFromGallery; end);
   3. In your form's OnCreate:
        FPickerSubId:= SetupImagePickerCallback(procedure(const Path: string) begin if not Path.IsEmpty then ProcessImage(Path); end);
        // In FormDestroy: TMessageManager.DefaultManager.Unsubscribe(FPickerSubId);
   4. To save: AddToPhotosAlbum(MyBitmap);

   Generic File Picker (ACTION_OPEN_DOCUMENT, Storage Access Framework):
   - PickAnyFileFromStorage opens the system Documents UI for any file type.
   - Goes through SAF, so READ_MEDIA_IMAGES / READ_EXTERNAL_STORAGE are NOT required.
   - Pair with SetupAnyFilePickerCallback (separate request code from image picker).
==============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.IOUtils, System.Messaging, System.Types, System.Permissions,
  FMX.Graphics, FMX.MediaLibrary, FMX.Platform, FMX.DialogService;


TYPE
  TImageSelectedEvent = procedure(const Path: string) of object;
  TFileSelectedEvent  = procedure(const Path: string) of object;

{$IFDEF IOS}
TYPE
  // iOS picker results — posted by the bridge after a pick/cancel completes.
  // SetupImagePickerCallback/SetupAnyFilePickerCallback subscribe to these on iOS.
  TMessageImagePickerResult = class(TMessage<string>);
  TMessageFilePickerResult  = class(TMessage<string>);
{$ENDIF}

procedure RequestCameraPermission(const AOnGranted: TProc);
procedure RequestStorageReadPermission(const AOnGranted: TProc);       // For image picking on Android 13+

procedure AddToPhotosAlbum(const ABitmap: TBitmap);                    // Saves to gallery, handles indexing

procedure ScanMediaFile(const AFileName: string);                      // If manually saving files

procedure PickImageFromGallery;                                        // Opens the Photos/Gallery picker (Android, iOS)

procedure PickAnyFileFromStorage(const MimeType: string = '*/*');      // Opens system file UI: SAF on Android, UIDocumentPickerViewController on iOS

// Paths
function  GetPublicPicturesFolder: string;

// Picker result subscription — cross-platform (Android + iOS).
// Returns the subscription ID. Caller MUST unsubscribe (TMessageManager.DefaultManager.Unsubscribe)
// when the subscribing object is destroyed, to prevent callbacks firing on freed memory.
// On Windows/macOS desktop / Linux: returns 0 and never fires (no async picker on those platforms).
function SetupImagePickerCallback  (const AOnImageSelected: TImageSelectedEvent): TMessageSubscriptionId;
function SetupAnyFilePickerCallback(const AOnFileSelected : TFileSelectedEvent ): TMessageSubscriptionId;


{$IFDEF ANDROID}
{ Incoming ACTION_VIEW intent (file shared from another app — WhatsApp, Email, Drive, etc).
  Subscribes to TMessageReceivedNotification, fires AOnFileReceived(LocalPath) when an
  ACTION_VIEW intent arrives with a content:// or file:// URI.

  - On WARM start (app already running, user taps file in another app, our manifest
    intent-filter routes here): TFMXNativeActivityListener.onReceiveNotification fires,
    which posts TMessageReceivedNotification.
  - On COLD start (app launched fresh by the intent): the launch intent must be read
    manually via ProcessLaunchIntent (see below), because the message is not auto-posted
    on the very first activity creation.

  Caller MUST unsubscribe in destructor.

  Note: subscription is shared across all senders, but we filter by Action = ACTION_VIEW
  so unrelated TMessageReceivedNotification fires (e.g. push notifications) are ignored. }
function SubscribeToIncomingFileIntents(const AOnFileReceived: TFileSelectedEvent): TMessageSubscriptionId;

{ Reads the activity's launch intent (the one passed at app start) and, if it's an
  ACTION_VIEW with a usable URI, copies the bytes to cache and calls AOnFileReceived.
  Call once from FormCreate AFTER the form is fully built (so the wizard can be shown).

  Idempotent: clears the launch intent's data after processing so repeated calls are safe. }
procedure ProcessLaunchIntent(const AOnFileReceived: TFileSelectedEvent);
{$ENDIF}


IMPLEMENTATION

uses
  LightCore.IO, LightCore.AppData
  {$IFDEF ANDROID}
  , FMX.Platform.Android, Androidapi.Helpers, Androidapi.JNI.Os, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Net, Androidapi.JNI.App, Androidapi.JNI.Media, Androidapi.JNI.Provider, Androidapi.JNIBridge, FMX.Helpers.Android
  {$ENDIF}
  {$IFDEF IOS}
  , FMX.Helpers.iOS, Macapi.Helpers, Macapi.ObjectiveC, iOSapi.UIKit, iOSapi.Foundation
  {$ENDIF};

CONST
  REQUEST_PICK_IMAGE = 1;
  REQUEST_PICK_FILE  = 2;



procedure RequestCameraPermission(const AOnGranted: TProc);
begin
{$IFDEF ANDROID}
  var CameraPermission := JStringToString(TJManifest_permission.JavaClass.CAMERA);
  PermissionsService.RequestPermissions([CameraPermission],
      procedure(const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray)
      begin
        if (Length(AGrantResults) = 1)
        AND (AGrantResults[0] = TPermissionStatus.Granted)
        then
          if Assigned(AOnGranted)
          then AOnGranted
          else
        else
          TDialogService.ShowMessage('Cannot access the camera because the required permission has not been granted');
      end);
{$ELSE}
  // iOS: NSCameraUsageDescription must be in Info.plist. Runtime authorization prompt
  // is triggered automatically by FMX capture APIs (IFMXCameraService) on first use,
  // so no preemptive request is required here.
  // macOS Desktop / Windows: no runtime permission system for these APIs.
  if Assigned(AOnGranted) then AOnGranted;
{$ENDIF}
end;


procedure RequestStorageReadPermission(const AOnGranted: TProc);
begin
{$IFDEF ANDROID}
  // Android 13+ (API 33) uses READ_MEDIA_IMAGES, older uses READ_EXTERNAL_STORAGE
  var ReadPermission: string;
  if TOSVersion.Check(13) then
    ReadPermission := JStringToString(TJManifest_permission.JavaClass.READ_MEDIA_IMAGES)
  else
    ReadPermission := JStringToString(TJManifest_permission.JavaClass.READ_EXTERNAL_STORAGE);

  PermissionsService.RequestPermissions([ReadPermission],
      procedure(const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray)
      begin
        if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
        begin
          if Assigned(AOnGranted)
          then AOnGranted;
        end
        else
          TDialogService.ShowMessage('Cannot access storage because the required permission has not been granted');
      end);
{$ELSE}
  // iOS: NSPhotoLibraryUsageDescription must be in Info.plist. Runtime authorization
  // prompt is triggered automatically by IFMXPhotoLibrary on first use, so no
  // preemptive request is required here.
  // macOS Desktop / Windows: no runtime permission system for these APIs.
  if Assigned(AOnGranted) then AOnGranted;
{$ENDIF}
end;


procedure AddToPhotosAlbum(const ABitmap: TBitmap);
VAR PhotoLibrary: IFMXPhotoLibrary;
begin
  Assert(ABitmap <> nil, 'AddToPhotosAlbum: ABitmap cannot be nil');
  // No need for ScanMediaFile here, as AddImageToSavedPhotosAlbum handles indexing internally on Android
  if TPlatformServices.Current.SupportsPlatformService(IFMXPhotoLibrary, PhotoLibrary)
  then PhotoLibrary.AddImageToSavedPhotosAlbum(ABitmap);
end;


procedure ScanMediaFile(const AFileName: string);
begin
{$IFDEF ANDROID}
  // ACTION_MEDIA_SCANNER_SCAN_FILE was deprecated in API 29 (Android 10) and is silently
  // ignored on newer devices. Use MediaScannerConnection.scanFile instead on API 29+.
  if TOSVersion.Check(10) then
  begin
    var Paths: TJavaObjectArray<JString>;
    Paths:= TJavaObjectArray<JString>.Create(1);
    Paths.Items[0]:= StringToJString(AFileName);
    TJMediaScannerConnection.JavaClass.scanFile(TAndroidHelper.Context, Paths, nil, nil);
  end
  else
  begin
    var Intent: JIntent;
    Intent:= TJIntent.Create;
    Intent.setAction(TJIntent.JavaClass.ACTION_MEDIA_SCANNER_SCAN_FILE);
    Intent.setData(TJnet_Uri.JavaClass.fromFile(TJFile.JavaClass.&init(StringToJString(AFileName))));
    TAndroidHelper.Activity.sendBroadcast(Intent);
  end;
{$ENDIF}
end;


{-------------------------------------------------------------------------------------------------------------
   iOS PICKER BRIDGES

   FMX iOS picker fires a method-callback (not anonymous) carrying a TBitmap that is freed
   immediately after the callback returns. We must save the bitmap during the callback,
   then post a TMessage so cross-platform subscribers (SetupImagePickerCallback) get a path.

   File picker uses native UIDocumentPickerViewController via a TOCLocal delegate. Same idea:
   convert NSURL → file path, post TMessageFilePickerResult.

   Both bridges are unit-private singletons (created lazily, freed in finalization).
   FINALIZATION is normally avoided per project rule, but here it is the only safe lifetime
   for these singletons (they outlive any single picker invocation, must be freed at app exit
   to release the registered Objective-C class for the document delegate).
-------------------------------------------------------------------------------------------------------------}
{$IFDEF IOS}
type
  // Image picker bridge — receives TBitmap from FMX iOS service, saves to temp, posts result.
  TIosImagePickerBridge = class
    procedure OnDidFinishTaking(Image: TBitmap);
    procedure OnDidCancelTaking;
  end;

  // File picker delegate — Objective-C delegate for UIDocumentPickerViewController.
  TIosDocumentPickerDelegate = class(TOCLocal, UIDocumentPickerDelegate)
  public
    function GetObjectiveCClass: PTypeInfo; override;
    procedure documentPicker(controller: UIDocumentPickerViewController; didPickDocumentsAtURLs: NSArray); overload; cdecl;
    procedure documentPicker(controller: UIDocumentPickerViewController; didPickDocumentAtURL: NSURL); overload; cdecl;
    procedure documentPickerWasCancelled(controller: UIDocumentPickerViewController); cdecl;
  end;

var
  GIosImagePickerBridge   : TIosImagePickerBridge   = NIL;
  GIosDocumentPickerBridge: TIosDocumentPickerDelegate = NIL;
  GIosDocumentPicker      : UIDocumentPickerViewController = NIL;  // retained while modal is up

function GenTempPath(const ASuffix: string): string;
var Guid: string;
begin
  // TGUID.ToString format: '{NNNNNNNN-NNNN-NNNN-NNNN-NNNNNNNNNNNN}' (38 chars).
  // Skip leading '{', take 36 chars (dashes are valid in filenames).
  Guid:= Copy(TGUID.NewGuid.ToString, 2, 36);
  Result:= TPath.Combine(TPath.GetTempPath, 'picked_' + Guid + ASuffix);
end;

{ TIosImagePickerBridge }

procedure TIosImagePickerBridge.OnDidFinishTaking(Image: TBitmap);
var TempPath: string;
begin
  TempPath:= GenTempPath('.jpg');
  TRY
    Image.SaveToFile(TempPath);
    TMessageManager.DefaultManager.SendMessage(NIL, TMessageImagePickerResult.Create(TempPath));
  EXCEPT
    on E: Exception do
      begin
        if Assigned(AppDataCore) AND Assigned(AppDataCore.RamLog)
        then AppDataCore.RamLog.AddError('iOS image picker save failed: ' + E.ClassName + ' - ' + E.Message);
        TMessageManager.DefaultManager.SendMessage(NIL, TMessageImagePickerResult.Create(''));
      end;
  END;
end;

procedure TIosImagePickerBridge.OnDidCancelTaking;
begin
  TMessageManager.DefaultManager.SendMessage(NIL, TMessageImagePickerResult.Create(''));
end;

{ TIosDocumentPickerDelegate }

function TIosDocumentPickerDelegate.GetObjectiveCClass: PTypeInfo;
begin
  Result:= TypeInfo(UIDocumentPickerDelegate);
end;

procedure CopyPickedNSURLAndPost(const AUrl: NSURL);
var
  PathStr  : string;
  TempPath : string;
  ScopedOK : Boolean;
begin
  if AUrl = NIL then
    begin
      TMessageManager.DefaultManager.SendMessage(NIL, TMessageFilePickerResult.Create(''));
      EXIT;
    end;

  // Security-scoped resource access required for files outside our sandbox (iCloud, other apps).
  ScopedOK:= AUrl.startAccessingSecurityScopedResource;
  TRY
    if AUrl.path = NIL
    then PathStr:= ''
    else PathStr:= NSStrToStr(AUrl.path);

    if PathStr = '' then
      begin
        TMessageManager.DefaultManager.SendMessage(NIL, TMessageFilePickerResult.Create(''));
        EXIT;
      end;

    // Copy to our cache so the path is stable beyond the security scope.
    TempPath:= GenTempPath(ExtractFileExt(PathStr));
    if LightCore.IO.CopyFile(PathStr, TempPath)
    then TMessageManager.DefaultManager.SendMessage(NIL, TMessageFilePickerResult.Create(TempPath))
    else
      begin
        if Assigned(AppDataCore) AND Assigned(AppDataCore.RamLog)
        then AppDataCore.RamLog.AddError('iOS file picker copy failed: ' + PathStr + ' -> ' + TempPath);
        TMessageManager.DefaultManager.SendMessage(NIL, TMessageFilePickerResult.Create(''));
      end;
  FINALLY
    if ScopedOK then AUrl.stopAccessingSecurityScopedResource;
  END;
end;

procedure TIosDocumentPickerDelegate.documentPicker(controller: UIDocumentPickerViewController; didPickDocumentsAtURLs: NSArray); cdecl;
begin
  // Multi-URL variant (iOS 11+) — we take the first URL.
  if (didPickDocumentsAtURLs <> NIL) AND (didPickDocumentsAtURLs.count > 0)
  then CopyPickedNSURLAndPost(TNSURL.Wrap(didPickDocumentsAtURLs.objectAtIndex(0)))
  else TMessageManager.DefaultManager.SendMessage(NIL, TMessageFilePickerResult.Create(''));

  controller.dismissModalViewControllerAnimated(True);
  GIosDocumentPicker:= NIL;  // released by ARC after dismiss
end;

procedure TIosDocumentPickerDelegate.documentPicker(controller: UIDocumentPickerViewController; didPickDocumentAtURL: NSURL); cdecl;
begin
  // Single-URL variant (iOS &lt; 11). Keeping for compat.
  CopyPickedNSURLAndPost(didPickDocumentAtURL);
  controller.dismissModalViewControllerAnimated(True);
  GIosDocumentPicker:= NIL;
end;

procedure TIosDocumentPickerDelegate.documentPickerWasCancelled(controller: UIDocumentPickerViewController); cdecl;
begin
  TMessageManager.DefaultManager.SendMessage(NIL, TMessageFilePickerResult.Create(''));
  controller.dismissModalViewControllerAnimated(True);
  GIosDocumentPicker:= NIL;
end;

procedure iOSPickImage;
var
  Service: IFMXTakenImageService;
  Params : TParamsPhotoQuery;
begin
  if NOT TPlatformServices.Current.SupportsPlatformService(IFMXTakenImageService, Service)
  then raise ENotSupportedException.Create('PickImageFromGallery: IFMXTakenImageService unavailable on this iOS build');

  if GIosImagePickerBridge = NIL
  then GIosImagePickerBridge:= TIosImagePickerBridge.Create;

  FillChar(Params, SizeOf(Params), 0);
  Params.Editable          := False;
  Params.NeedSaveToAlbum   := False;
  Params.RequiredResolution:= TSize.Create(0, 0);  // 0 = native resolution
  Params.OnDidFinishTaking := GIosImagePickerBridge.OnDidFinishTaking;
  Params.OnDidCancelTaking := GIosImagePickerBridge.OnDidCancelTaking;
  // AControl is unused by FMX iOS impl (only TakeImage's IsPad branch references it, and ignores it).
  Service.TakeImageFromLibrary(NIL, Params);
end;

procedure iOSPickFile;
var
  Window     : UIWindow;
  AllowedUTIs: NSMutableArray;
begin
  if GIosDocumentPickerBridge = NIL
  then GIosDocumentPickerBridge:= TIosDocumentPickerDelegate.Create;

  AllowedUTIs:= TNSMutableArray.Create;
  AllowedUTIs.addObject(NSObjectToID(StrToNSStr('public.item')));  // any file

  // initWithDocumentTypes is deprecated in iOS 14 (use initForOpeningContentTypes with UTType),
  // but still functional. UTType bindings not yet stable in Delphi RTL.
  GIosDocumentPicker:= TUIDocumentPickerViewController.Wrap(
    TUIDocumentPickerViewController.Alloc.initWithDocumentTypes(AllowedUTIs, UIDocumentPickerModeImport));
  GIosDocumentPicker.setDelegate(GIosDocumentPickerBridge.GetObjectID);

  Window:= SharedApplication.keyWindow;
  if (Window <> NIL) AND (Window.rootViewController <> NIL)
  then Window.rootViewController.presentModalViewController(GIosDocumentPicker, True)
  else
    begin
      if Assigned(AppDataCore) AND Assigned(AppDataCore.RamLog)
      then AppDataCore.RamLog.AddError('iOSPickFile: keyWindow.rootViewController unavailable');
      GIosDocumentPicker:= NIL;
      TMessageManager.DefaultManager.SendMessage(NIL, TMessageFilePickerResult.Create(''));
    end;
end;
{$ENDIF}


// Before calling PickImageFromGallery, request permission: RequestStorageReadPermission(procedure begin PickImageFromGallery; end);
procedure PickImageFromGallery;
begin
{$IFDEF ANDROID}
  VAR Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_PICK);
  Intent.setType(StringToJString('image/*'));
  MainActivity.startActivityForResult(Intent, REQUEST_PICK_IMAGE);
{$ELSEIF DEFINED(IOS)}
  iOSPickImage;
{$ENDIF}
end;


// Opens the Storage Access Framework document UI. No permission needed — OS grants temporary URI access.
// MimeType: '*/*' for any file, or a specific MIME like 'application/pdf'. For multi-MIME, use EXTRA_MIME_TYPES.
procedure PickAnyFileFromStorage(const MimeType: string = '*/*');
begin
{$IFDEF ANDROID}
  VAR Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_OPEN_DOCUMENT);
  Intent.addCategory(TJIntent.JavaClass.CATEGORY_OPENABLE);
  Intent.setType(StringToJString(MimeType));
  MainActivity.startActivityForResult(Intent, REQUEST_PICK_FILE);
{$ELSEIF DEFINED(IOS)}
  // MimeType is ignored on iOS — UIDocumentPickerViewController takes UTI strings, not MIME.
  // We use 'public.item' (any file) which mirrors '*/*'. Specific UTI filtering is a future enhancement.
  iOSPickFile;
{$ENDIF}
end;


{$IFDEF ANDROID}
{ Copies a file from a content:// URI to the app's cache directory.
  Returns the local file path on success, or empty string on failure.
  ASuffix is just the temp filename suffix (e.g. '.jpg', '.dat'); the bytes copied are whatever's in the URI.
  The caller is responsible for deleting the cached file when no longer needed. }
function CopyUriToCache(const AUri: Jnet_Uri; const ASuffix: string = '.jpg'): string;
var
  InputStream: JInputStream;
  OutputStream: JFileOutputStream;
  CacheFile: JFile;
  BytesRead: Integer;
  JBuffer: TJavaArray<Byte>;
begin
  Result:= '';
  OutputStream:= NIL;
  JBuffer:= NIL;
  CacheFile:= nil;

  try
    // 1. Create a safe temp file in the cache directory
    CacheFile:= TJFile.JavaClass.createTempFile(StringToJString('picked_'), StringToJString(ASuffix), TAndroidHelper.Context.getCacheDir);
    if CacheFile = nil then Exit; // Failed to create file

    // 2. Try to open the source URI
    InputStream:= TAndroidHelper.ContentResolver.openInputStream(AUri);
    if InputStream = nil
    then EXIT;

    try
      OutputStream:= TJFileOutputStream.JavaClass.&init(CacheFile);
      try
        // Allocate Java Byte Array ONCE
        JBuffer:= TJavaArray<Byte>.Create(4096);

        // 3. Loop and copy. Java InputStream.read returns -1 at EOF; 0 means
        // "no bytes currently available" on PipeInputStream-backed providers
        // (some FileProvider impls). Don't treat 0 as EOF — only -1.
        BytesRead:= InputStream.read(JBuffer);
        while BytesRead <> -1 do
        begin
          if BytesRead > 0
          then OutputStream.write(JBuffer, 0, BytesRead);
          BytesRead:= InputStream.read(JBuffer);
        end;

        // 4. ONLY set result if we actually wrote something or finished without error
        Result:= JStringToString(CacheFile.getAbsolutePath);
      finally
        OutputStream.close;
        FreeAndNil(JBuffer);
      end;
    finally
      InputStream.close;
    end;

    // 5. Final check: verify file is not 0 bytes
    if (Result <> '') AND (TJFile.JavaClass.&init(StringToJString(Result)).length = 0)
    then Result:= '';
  finally
    // Clean up the temp file if operation failed
    if (Result = '') AND (CacheFile <> nil)
    then CacheFile.delete;
  end;
end;
{$ENDIF}



{ Call this once (e.g., in FormCreate) to handle the picker result asynchronously.
  Callback receives the full file path or empty string if canceled.
  CALLER MUST store the returned TMessageSubscriptionId and call
  TMessageManager.DefaultManager.Unsubscribe(Id) in FormDestroy (or equivalent),
  to prevent the callback firing on freed memory.
  Do NOT call multiple times without unsubscribing — duplicate subscriptions accumulate.

  Cross-platform behavior:
    Android: subscribes to TMessageResultNotification (intent result).
    iOS    : subscribes to TMessageImagePickerResult (posted by TIosImagePickerBridge).
    Other  : returns 0; callback never fires (no async picker on Win/macOS desktop). }
function SetupImagePickerCallback(const AOnImageSelected: TImageSelectedEvent): TMessageSubscriptionId;
begin
  Result:= 0;
{$IFDEF ANDROID}
  Result:= TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification,
    procedure(const Sender: TObject; const M: TMessage)
    var
      Msg: TMessageResultNotification;
      Path: string;
    begin
      // Hard try/except: this anonymous proc is dispatched by FMX.Platform.Android
      // outside any application-level handler. An exception that escapes here
      // kills the process silently on Android (no madExcept, no dialog).
      TRY
        Msg:= TMessageResultNotification(M);
        if Msg.RequestCode = REQUEST_PICK_IMAGE then
        begin
          if Msg.ResultCode = TJActivity.JavaClass.RESULT_OK then
          begin
            var Uri := Msg.Value.getData;
            // We MUST copy the file to a local path (Cache) because
            // TBitmap.LoadFromFile cannot read 'content://' URIs directly.
            Path := CopyUriToCache(Uri, '.jpg');

            if Assigned(AOnImageSelected)
            then AOnImageSelected(Path);
          end
          else
            if Assigned(AOnImageSelected)
            then AOnImageSelected('');
        end;
      EXCEPT
        on E: Exception do
          if Assigned(AppDataCore) AND Assigned(AppDataCore.RamLog)
          then AppDataCore.RamLog.AddError('SetupImagePickerCallback: '+ E.ClassName +' - '+ E.Message);
      END;
    end);
{$ENDIF}
{$IFDEF IOS}
  Result:= TMessageManager.DefaultManager.SubscribeToMessage(TMessageImagePickerResult,
    procedure(const Sender: TObject; const M: TMessage)
    begin
      TRY
        if Assigned(AOnImageSelected)
        then AOnImageSelected(TMessageImagePickerResult(M).Value);
      EXCEPT
        on E: Exception do
          if Assigned(AppDataCore) AND Assigned(AppDataCore.RamLog)
          then AppDataCore.RamLog.AddError('SetupImagePickerCallback (iOS): '+ E.ClassName +' - '+ E.Message);
      END;
    end);
{$ENDIF}
end;


{ Generic file picker callback (paired with PickAnyFileFromStorage / REQUEST_PICK_FILE).
  Same lifetime rules as SetupImagePickerCallback: caller MUST unsubscribe.
  Bytes are copied to cache as 'picked_xxx.dat'. ImportSharedFile (and similar) read
  by content header, not by filename, so the suffix is cosmetic.

  Cross-platform behavior:
    Android: subscribes to TMessageResultNotification (intent result).
    iOS    : subscribes to TMessageFilePickerResult (posted by TIosDocumentPickerDelegate).
    Other  : returns 0; callback never fires. }
function SetupAnyFilePickerCallback(const AOnFileSelected: TFileSelectedEvent): TMessageSubscriptionId;
begin
  Result:= 0;
{$IFDEF ANDROID}
  Result:= TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification,
    procedure(const Sender: TObject; const M: TMessage)
    var
      Msg : TMessageResultNotification;
      Path: string;
    begin
      // Hard try/except: see comment in SetupImagePickerCallback.
      TRY
        Msg:= TMessageResultNotification(M);
        if Msg.RequestCode = REQUEST_PICK_FILE then
        begin
          if Msg.ResultCode = TJActivity.JavaClass.RESULT_OK then
          begin
            var Uri := Msg.Value.getData;
            Path := CopyUriToCache(Uri, '.dat');

            if Assigned(AOnFileSelected)
            then AOnFileSelected(Path);
          end
          else
            if Assigned(AOnFileSelected)
            then AOnFileSelected('');
        end;
      EXCEPT
        on E: Exception do
          if Assigned(AppDataCore) AND Assigned(AppDataCore.RamLog)
          then AppDataCore.RamLog.AddError('SetupAnyFilePickerCallback: '+ E.ClassName +' - '+ E.Message);
      END;
    end);
{$ENDIF}
{$IFDEF IOS}
  Result:= TMessageManager.DefaultManager.SubscribeToMessage(TMessageFilePickerResult,
    procedure(const Sender: TObject; const M: TMessage)
    begin
      TRY
        if Assigned(AOnFileSelected)
        then AOnFileSelected(TMessageFilePickerResult(M).Value);
      EXCEPT
        on E: Exception do
          if Assigned(AppDataCore) AND Assigned(AppDataCore.RamLog)
          then AppDataCore.RamLog.AddError('SetupAnyFilePickerCallback (iOS): '+ E.ClassName +' - '+ E.Message);
      END;
    end);
{$ENDIF}
end;


{$IFDEF ANDROID}
{-------------------------------------------------------------------------------------------------------------
   INCOMING ACTION_VIEW INTENT (file shared from another app)

   Both helpers below feed into the same callback: the local cache path of the file
   delivered by the OS. Caller (typically FormMain) decides what to do with it
   (e.g. show the import wizard, then call TLesson.ImportSharedFile).

   TEST-ON-DEVICE — items NOT verified on Windows compile, need real Android testing:

     1. pathPattern matching for content:// URIs from FileProviders.
        AndroidManifest.template.xml has <data pathPattern=".*\\.LSN" /> as a fallback for
        chat apps that drop MIME (WhatsApp, Telegram). pathPattern matches against the URI
        PATH component. Some FileProviders expose the original filename in the path
        (in which case our pattern matches → user sees LearnAssist in the share sheet);
        other providers obscure it with opaque IDs (in which case the pattern misses).
        WhatsApp's FileProvider behavior should be tested specifically.

     2. MainActivity.getIntent on cold start.
        Assumed to return the launch intent reliably (by analogy with native Android).
        FMX may wrap it differently. If cold start fails to surface the file, ProcessLaunchIntent
        below will silently no-op — the warm-start path via TMessageReceivedNotification still
        works, so the bug would manifest as "must launch app first, then re-share" UX.

     3. TMessageReceivedNotification firing for ACTION_VIEW.
        FMX.Platform.Android.TFMXNativeActivityListener.onReceiveNotification is documented
        in source but not necessarily exercised for user-share intents on all OEM Android
        builds (Samsung One UI, MIUI, etc. sometimes route differently). If warm start
        misses the intent, SubscribeToIncomingFileIntents simply never fires.

   How to verify (3 quick tests on an Android device after deploying):
     a) Cold start:  app NOT running. From Files app, tap a .LSN. Expect import wizard.
     b) Warm start:  app running, in TreeView. From WhatsApp, tap a .LSN, pick LearnAssist.
                     Expect import wizard.
     c) Cancel path: in wizard, press Cancel. Expect orphan content file at
                     <AppDataFolder>/Lessons/<GUID>.content.LSN to be DELETED, and the
                     /cache/picked_*.lsn copy also deleted.
-------------------------------------------------------------------------------------------------------------}

{ Reads the URI from a JIntent (ACTION_VIEW), copies the bytes to /cache/ as a .dat file,
  returns the local path. Returns '' on failure (no URI, copy error, etc).
  Internal helper — used by both warm-intent (TMessageReceivedNotification subscribe) and
  cold-intent (ProcessLaunchIntent) flows. }
function ExtractFileFromIntent(const AIntent: JIntent): string;
var
  Action: string;
  Uri   : Jnet_Uri;
begin
  Result:= '';
  if AIntent = nil then EXIT;

  // Filter by action: only ACTION_VIEW carries a file URI we should import.
  // Push notifications, MAIN launcher events, etc. would also raise TMessageReceivedNotification
  // and must be ignored here.
  if AIntent.getAction = nil then EXIT;
  Action:= JStringToString(AIntent.getAction);
  if Action <> JStringToString(TJIntent.JavaClass.ACTION_VIEW) then EXIT;

  Uri:= AIntent.getData;
  if Uri = nil then EXIT;

  // Copy bytes to cache. Suffix '.lsn' is cosmetic — ImportSharedFile reads by header.
  Result:= CopyUriToCache(Uri, '.lsn');
end;


{ Subscribes to TMessageReceivedNotification — fired when the activity is woken by
  an intent (onNewIntent). Used for WARM start (app already running, user shares file). }
function SubscribeToIncomingFileIntents(const AOnFileReceived: TFileSelectedEvent): TMessageSubscriptionId;
begin
  Result:= TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification,
    procedure(const Sender: TObject; const M: TMessage)
    var
      Msg : TMessageReceivedNotification;
      Path: string;
    begin
      // Hard try/except: this fires on FMX dispatch path; an unhandled exception
      // would kill the process on Android. Log and swallow.
      TRY
        Msg := TMessageReceivedNotification(M);
        Path:= ExtractFileFromIntent(Msg.Value);
        if (Path <> '') AND Assigned(AOnFileReceived)
        then AOnFileReceived(Path);
      EXCEPT
        on E: Exception do
          if Assigned(AppDataCore) AND Assigned(AppDataCore.RamLog)
          then AppDataCore.RamLog.AddError('SubscribeToIncomingFileIntents: '+ E.ClassName +' - '+ E.Message);
      END;
    end);
end;


{ Reads the activity's launch intent (set when Android started us, before any onNewIntent).
  This is the COLD-start path — user tapped a .LSN file when our app was NOT running.

  After processing we clear the intent's data via setData(nil) so subsequent calls
  (e.g. if FormCreate runs twice, or if a later code path also peeks at getIntent)
  see no URI.

  TEST-ON-DEVICE: confirm MainActivity.getIntent returns the actual ACTION_VIEW launch
  intent (and not e.g. a cached MAIN intent) on cold start. If it returns wrong/empty,
  cold-start import will silently no-op. Diagnose by adding a temporary
  AppData.RamLog.AddInfo line below to log getAction/getData on every call. }
procedure ProcessLaunchIntent(const AOnFileReceived: TFileSelectedEvent);
var
  Intent: JIntent;
  Path  : string;
begin
  TRY
    if MainActivity = nil then EXIT;
    Intent:= MainActivity.getIntent;
    if Intent = nil then EXIT;

    Path:= ExtractFileFromIntent(Intent);
    if Path = '' then EXIT;

    // Clear the URI so we don't re-process it. setAction(MAIN) would also work but
    // setData(nil) is enough — ExtractFileFromIntent returns '' when getData is nil.
    Intent.setData(nil);

    if Assigned(AOnFileReceived)
    then AOnFileReceived(Path);
  EXCEPT
    on E: Exception do
      if Assigned(AppDataCore) AND Assigned(AppDataCore.RamLog)
      then AppDataCore.RamLog.AddError('ProcessLaunchIntent: '+ E.ClassName +' - '+ E.Message);
  END;
end;
{$ENDIF}



{-------------------------------------------------------------------------------------------------------------
   PATHS
-------------------------------------------------------------------------------------------------------------}

{ Returns the public Pictures folder path.
  ANDROID 10+ (API 29) WARNING: scoped storage prevents direct file writes to this path
  via TFile/TFileStream — they will silently fail or throw SecurityException.
  On API 29+, use MediaStore (ContentResolver.insert with MediaStore.Images.Media.EXTERNAL_CONTENT_URI)
  to write images, then call ScanMediaFile if the file was added via direct path on legacy API.
  This function is safe for DISPLAY only on Android 10+; do NOT pass the result to write APIs. }
function GetPublicPicturesFolder: string;
begin
  {$IFDEF MSWINDOWS}
  Result:= Trail(TPath.GetPicturesPath);
  {$ELSEIF DEFINED(ANDROID)}
  Result:= Trail(TPath.GetSharedPicturesPath);
  {$ELSE}
  Result:= Trail(TPath.GetDocumentsPath);
  {$ENDIF}
end;


{$IFDEF IOS}
// Frees iOS picker singletons. FINALIZATION is normally avoided per project rule, but
// here the singletons (TIosImagePickerBridge + TIosDocumentPickerDelegate) must outlive
// any single picker invocation AND be released at app exit so the registered Objective-C
// class for the document delegate is unregistered cleanly.
finalization
  FreeAndNil(GIosImagePickerBridge);
  FreeAndNil(GIosDocumentPickerBridge);
{$ENDIF}

end.
