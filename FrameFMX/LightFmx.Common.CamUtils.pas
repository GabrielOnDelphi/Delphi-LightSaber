UNIT LightFmx.Common.CamUtils;

{=============================================================================================================
   2026.04.25
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Android Camera and Gallery Utilities

   Usage Instructions:
   1. Add necessary permissions to Android manifest:
        <uses-permission android:name="android.permission.CAMERA" />                (camera capture)
        <uses-permission android:name="android.permission.READ_MEDIA_IMAGES" />     (gallery, Android 13+)
        or <uses-permission android:name="android.permission.READ_EXTERNAL_STORAGE" /> for older devices.
      iOS Info.plist keys:
        NSCameraUsageDescription       (camera capture)
        NSPhotoLibraryUsageDescription (gallery picking / save-to-album)
   2. Before calling PickImageFromGallery, request permission:
        RequestStorageReadPermission(procedure begin PickImageFromGallery; end);
   3. In your form's OnCreate:
        FPickerSubId:= SetupImagePickerCallback(procedure(const Path: string) begin if not Path.IsEmpty then ProcessImage(Path); end);
        // In FormDestroy: TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, FPickerSubId);   // Android
   4. To save: AddToPhotosAlbum(MyBitmap);

   Generic File Picker (ACTION_OPEN_DOCUMENT, Storage Access Framework):
   - PickAnyFileFromStorage opens the system Documents UI for any file type.
   - Goes through SAF, so READ_MEDIA_IMAGES / READ_EXTERNAL_STORAGE are NOT required.
   - Pair with SetupAnyFilePickerCallback (separate request code from image picker).

   References (authoritative sources for the platform claims made in this unit):
   - MediaScannerConnection.scanFile is fire-and-forget when the listener parameter is null
     (Android internally creates a self-disconnecting connection):
        https://developer.android.com/reference/android/media/MediaScannerConnection
   - ACTION_MEDIA_SCANNER_SCAN_FILE deprecated in API 29 (Android 10), silently ignored on newer:
        https://developer.android.com/reference/android/content/Intent#ACTION_MEDIA_SCANNER_SCAN_FILE
   - Android runtime permission model + READ_MEDIA_IMAGES split at API 33 (Android 13+):
        https://docwiki.embarcadero.com/RADStudio/Athens/en/Android_Permission_Model
        https://developer.android.com/about/versions/13/behavior-changes-13#granular-media-permissions
   - IFMXTakenImageService.TakeImageFromLibrary (gallery picker, iOS bridge target):
        https://docwiki.embarcadero.com/Libraries/Athens/en/FMX.MediaLibrary.IFMXTakenImageService.TakeImageFromLibrary
   - UIDocumentPickerViewController.initWithDocumentTypes deprecated in iOS 14
     (use initForOpeningContentTypes with UTType — bindings not yet stable in Delphi RTL):
        https://developer.apple.com/documentation/uikit/uidocumentpickerviewcontroller/1618678-initwithdocumenttypes
   - UIApplication.keyWindow deprecated in iOS 13 (still functional on a single-scene app):
        https://developer.apple.com/documentation/uikit/uiapplication/1622924-keywindow
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
  System.SysUtils, System.IOUtils, System.Messaging, System.Types, System.Permissions,
  FMX.Graphics, FMX.MediaLibrary, FMX.Platform,
  LightCore.CamUtils;   { The framework-free half of this unit: TFileSelectedEvent, REQUEST_PICK_IMAGE / REQUEST_PICK_FILE, ExtractFileFromIntent, and the iOS TMessageImagePickerResult / TMessageFilePickerResult classes the bridges below post }


procedure AddToPhotosAlbum(const ABitmap: TBitmap);                    // Saves to gallery, handles indexing

procedure PickImageFromGallery;                                        // Opens the Photos/Gallery picker (Android, iOS)

procedure PickAnyFileFromStorage(const MimeType: string = '*/*');      // Opens system file UI: SAF on Android, UIDocumentPickerViewController on iOS


{$IFDEF ANDROID}
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


procedure AddToPhotosAlbum(const ABitmap: TBitmap);
VAR PhotoLibrary: IFMXPhotoLibrary;
begin
  Assert(ABitmap <> nil, 'AddToPhotosAlbum: ABitmap cannot be nil');
  // No need for ScanMediaFile here, as AddImageToSavedPhotosAlbum handles indexing internally on Android
  if TPlatformServices.Current.SupportsPlatformService(IFMXPhotoLibrary, PhotoLibrary)
  then PhotoLibrary.AddImageToSavedPhotosAlbum(ABitmap);
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

  { initWithDocumentTypes is deprecated in iOS 14 (use initForOpeningContentTypes with UTType), but still functional.
    UTType bindings not yet stable in Delphi RTL. }
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


{ Opens the Storage Access Framework document UI. No permission needed — OS grants temporary URI access.
  MimeType: '*/*' for any file, or a specific MIME like 'application/pdf'. For multi-MIME, use EXTRA_MIME_TYPES. }
procedure PickAnyFileFromStorage(const MimeType: string = '*/*');
begin
{$IFDEF ANDROID}
  VAR Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_OPEN_DOCUMENT);
  Intent.addCategory(TJIntent.JavaClass.CATEGORY_OPENABLE);
  Intent.setType(StringToJString(MimeType));
  MainActivity.startActivityForResult(Intent, REQUEST_PICK_FILE);
{$ELSEIF DEFINED(IOS)}
  { MimeType is ignored on iOS — UIDocumentPickerViewController takes UTI strings, not MIME.
    We use 'public.item' (any file) which mirrors '*/*'. Specific UTI filtering is a future enhancement. }
  iOSPickFile;
{$ENDIF}
end;


{$IFDEF ANDROID}
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

    { Clear the URI so we don't re-process it.
      setAction(MAIN) would also work but setData(nil) is enough — ExtractFileFromIntent returns '' when getData is nil. }
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


{$IFDEF IOS}
{ Frees iOS picker singletons.
  FINALIZATION is normally avoided per project rule, but here the singletons (TIosImagePickerBridge + TIosDocumentPickerDelegate) must outlive any single picker invocation AND be released at app exit so the registered Objective-C class for the document delegate is unregistered cleanly. }
finalization
  FreeAndNil(GIosImagePickerBridge);
  FreeAndNil(GIosDocumentPickerBridge);
{$ENDIF}

end.
