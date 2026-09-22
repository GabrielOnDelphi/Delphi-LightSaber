UNIT LightCore.CamUtils;

{=============================================================================================================
   2026.09.10
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Camera / gallery / file-picker utilities - the half that needs no framework.

   Runtime permission requests, the media scanner, the picker-result callbacks, the incoming
   ACTION_VIEW intent subscription, and the public Pictures folder. Nothing here touches the VCL
   or FMX, so a console tool, a DUnitX test or a plain Android build can use it.

   The other half stays in LightFmx.Common.CamUtils.pas because it needs FMX: AddToPhotosAlbum
   (IFMXPhotoLibrary), PickImageFromGallery and PickAnyFileFromStorage (FMX.Platform.Android.MainActivity,
   plus the whole iOS picker bridge) and ProcessLaunchIntent (MainActivity). An application that picks
   images needs BOTH units in its USES clause.

   Usage instructions, the Android manifest permissions and the iOS Info.plist keys are in the
   header of LightFmx.Common.CamUtils.pas.

   References (authoritative sources for the platform claims made in this unit):
   - MediaScannerConnection.scanFile is fire-and-forget when the listener parameter is null
     (Android internally creates a self-disconnecting connection):
        https://developer.android.com/reference/android/media/MediaScannerConnection
   - ACTION_MEDIA_SCANNER_SCAN_FILE deprecated in API 29 (Android 10), silently ignored on newer:
        https://developer.android.com/reference/android/content/Intent#ACTION_MEDIA_SCANNER_SCAN_FILE
   - Android runtime permission model + READ_MEDIA_IMAGES split at API 33 (Android 13+):
        https://docwiki.embarcadero.com/RADStudio/Athens/en/Android_Permission_Model
        https://developer.android.com/about/versions/13/behavior-changes-13#granular-media-permissions
   - TJavaObjectArray<T> is a plain TObject (not ARC, not interface) and must be Freed.
     Hierarchy: TJavaObjectArray<T> → TJavaArray<T> → TJavaBasicArray (TObject).
     Destructor calls TJNIResolver.DeleteGlobalRef — leaks the JNI global ref if not freed.
        Local: $(BDS)\source\rtl\android\Androidapi.JNIBridge.pas (the TJavaObjectArray declaration; the destructor is TJavaArray<T>.Destroy)
        Local: $(BDS)\source\fmx\FMX.AddressBook.Android.pas (the RTL's own try..finally Free pattern)
   - TOSVersion.Major on Android is parsed from Build.VERSION.RELEASE (the human "13", "10" string),
     NOT the API level. So TOSVersion.Check(13) = Android 13 = API 33:
        Local: $(BDS)\source\rtl\common\System.SysUtils.pas (TOSVersion implementation)
==============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.IOUtils, System.Messaging, System.Types, System.Permissions
  {$IFDEF ANDROID}
  , Androidapi.JNI.GraphicsContentViewText                                  // JIntent, for ExtractFileFromIntent below
  {$ENDIF};


CONST
  { Activity request codes. Public because PickImageFromGallery and PickAnyFileFromStorage stayed in
    LightFmx.Common.CamUtils and must pass the same codes that the callbacks in this unit match on. }
  REQUEST_PICK_IMAGE = 1;
  REQUEST_PICK_FILE  = 2;


TYPE
  { System.Messaging only names this type TMessageSubscriptionId from Delphi 12 Athens on.
    Before that, TMessageManager.SubscribeToMessage returned a plain Integer, and the width
    changed with the rename (Integer -> Int64), so an older compiler cannot just be handed the
    new name. Use TSubscriptionId everywhere in this unit instead of the RTL name. }
  {$IF CompilerVersion >= 36}
  TSubscriptionId = System.Messaging.TMessageSubscriptionId;
  {$ELSE}
  TSubscriptionId = Integer;
  {$IFEND}

  TImageSelectedEvent = procedure(const Path: string) of object;
  TFileSelectedEvent  = procedure(const Path: string) of object;

{$IFDEF IOS}
TYPE
  { iOS picker results — posted by the bridge after a pick/cancel completes.
    SetupImagePickerCallback/SetupAnyFilePickerCallback subscribe to these on iOS. }
  TMessageImagePickerResult = class(TMessage<string>);
  TMessageFilePickerResult  = class(TMessage<string>);
{$ENDIF}

{ A refusal is written to AppDataCore's log and AOnDenied is called if the caller supplied one.
  Nothing appears on screen: only the caller knows whether a refusal deserves a message. }
procedure RequestCameraPermission(const AOnGranted: TProc; const AOnDenied: TProc= NIL);
procedure RequestStorageReadPermission(const AOnGranted: TProc; const AOnDenied: TProc= NIL);       // For image picking on Android 13+

procedure ScanMediaFile(const AFileName: string);                      // If manually saving files

// Paths
function  GetPublicPicturesFolder: string;

{ Picker result subscription — cross-platform (Android + iOS).
  Returns the subscription ID.
  Caller MUST unsubscribe (TMessageManager.DefaultManager.Unsubscribe) when the subscribing object is destroyed, to prevent callbacks firing on freed memory.
  On Windows/macOS desktop / Linux: returns 0 and never fires (no async picker on those platforms). }
function SetupImagePickerCallback  (const AOnImageSelected: TImageSelectedEvent): TSubscriptionId;
function SetupAnyFilePickerCallback(const AOnFileSelected : TFileSelectedEvent ): TSubscriptionId;


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
function SubscribeToIncomingFileIntents(const AOnFileReceived: TFileSelectedEvent): TSubscriptionId;

{ Reads the URI out of an ACTION_VIEW JIntent and copies the bytes to /cache/. See the full comment
  on the implementation below. Public only because ProcessLaunchIntent, which stayed in
  LightFmx.Common.CamUtils, calls it - it was a unit-private helper before the split. }
function ExtractFileFromIntent(const AIntent: JIntent): string;
{$ENDIF}



IMPLEMENTATION

USES
  LightCore.IO, LightCore.AppData
  {$IFDEF ANDROID}
  , Androidapi.Helpers, Androidapi.JNI.Os, Androidapi.JNI.JavaTypes, Androidapi.JNI.Net, Androidapi.JNI.App, Androidapi.JNI.Media, Androidapi.JNI.Provider, Androidapi.JNIBridge
  {$ENDIF};



procedure RequestCameraPermission(const AOnGranted: TProc; const AOnDenied: TProc= NIL);
begin
{$IFDEF ANDROID}
  var CameraPermission := JStringToString(TJManifest_permission.JavaClass.CAMERA);
  PermissionsService.RequestPermissions([CameraPermission],
      procedure(const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray)
      begin
        if (Length(AGrantResults) = 1)
        AND (AGrantResults[0] = TPermissionStatus.Granted)
        then begin
          if Assigned(AOnGranted)
          then AOnGranted;
        end
        else
          begin
            AppDataCore.LogWarn('RequestCameraPermission: the user did not grant the CAMERA permission.');
            if Assigned(AOnDenied)
            then AOnDenied;
          end;
      end);
{$ELSE}
  { iOS: NSCameraUsageDescription must be in Info.plist.
    The runtime authorization prompt is triggered automatically by FMX capture APIs (IFMXCameraService) on first use, so no preemptive request is required here.
    macOS Desktop / Windows: no runtime permission system for these APIs. }
  if Assigned(AOnGranted) then AOnGranted;
{$ENDIF}
end;


procedure RequestStorageReadPermission(const AOnGranted: TProc; const AOnDenied: TProc= NIL);
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
          begin
            AppDataCore.LogWarn('RequestStorageReadPermission: the user did not grant the storage-read permission.');
            if Assigned(AOnDenied)
            then AOnDenied;
          end;
      end);
{$ELSE}
  { iOS: NSPhotoLibraryUsageDescription must be in Info.plist.
    The runtime authorization prompt is triggered automatically by IFMXPhotoLibrary on first use, so no preemptive request is required here.
    macOS Desktop / Windows: no runtime permission system for these APIs. }
  if Assigned(AOnGranted) then AOnGranted;
{$ENDIF}
end;


procedure ScanMediaFile(const AFileName: string);
begin
{$IFDEF ANDROID}
  { ACTION_MEDIA_SCANNER_SCAN_FILE was deprecated in API 29 (Android 10) and is silently ignored on newer devices.
    Use MediaScannerConnection.scanFile instead on API 29+. }
  if TOSVersion.Check(10) then
  begin
    var Paths: TJavaObjectArray<JString>;
    Paths:= TJavaObjectArray<JString>.Create(1);
    try
      Paths.Items[0]:= StringToJString(AFileName);
      TJMediaScannerConnection.JavaClass.scanFile(TAndroidHelper.Context, Paths, nil, nil);
    finally
      FreeAndNil(Paths);
    end;
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

        { 3. Loop and copy. Java InputStream.read returns -1 at EOF; 0 means "no bytes currently available" on PipeInputStream-backed providers (some FileProvider impls).
          Don't treat 0 as EOF — only -1. }
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
  CALLER MUST store the returned TSubscriptionId and unsubscribe in FormDestroy (or
  equivalent), to prevent the callback firing on freed memory. Unsubscribe always takes the
  message class first (there is no single-argument overload on TMessageManager), and it must
  be the class that was subscribed, which differs per platform:
    Android: TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, Id);
    iOS    : TMessageManager.DefaultManager.Unsubscribe(TMessageImagePickerResult, Id);
  Do NOT call multiple times without unsubscribing — duplicate subscriptions accumulate.

  Cross-platform behavior:
    Android: subscribes to TMessageResultNotification (intent result).
    iOS    : subscribes to TMessageImagePickerResult (posted by TIosImagePickerBridge).
    Other  : returns 0; callback never fires (no async picker on Win/macOS desktop). }
function SetupImagePickerCallback(const AOnImageSelected: TImageSelectedEvent): TSubscriptionId;
begin
  Result:= 0;
{$IFDEF ANDROID}
  Result:= TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification,
    procedure(const Sender: TObject; const M: TMessage)
    var
      Msg: TMessageResultNotification;
      Path: string;
    begin
      { Hard try/except: this anonymous proc is dispatched by FMX.Platform.Android outside any application-level handler.
        An exception that escapes here kills the process silently on Android (no madExcept, no dialog). }
      TRY
        Msg:= TMessageResultNotification(M);
        if Msg.RequestCode = REQUEST_PICK_IMAGE then
        begin
          if Msg.ResultCode = TJActivity.JavaClass.RESULT_OK then
          begin
            var Uri := Msg.Value.getData;
            { We MUST copy the file to a local path (Cache) because TBitmap.LoadFromFile cannot read 'content://' URIs directly. }
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
function SetupAnyFilePickerCallback(const AOnFileSelected: TFileSelectedEvent): TSubscriptionId;
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

  { Filter by action: only ACTION_VIEW carries a file URI we should import.
    Push notifications, MAIN launcher events, etc. would also raise TMessageReceivedNotification and must be ignored here. }
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
function SubscribeToIncomingFileIntents(const AOnFileReceived: TFileSelectedEvent): TSubscriptionId;
begin
  Result:= TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification,
    procedure(const Sender: TObject; const M: TMessage)
    var
      Msg : TMessageReceivedNotification;
      Path: string;
    begin
      { Hard try/except: this fires on FMX dispatch path; an unhandled exception would kill the process on Android.
        Log and swallow. }
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


end.
