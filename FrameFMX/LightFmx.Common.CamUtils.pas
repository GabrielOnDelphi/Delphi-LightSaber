unit LightFmx.Common.CamUtils;

// Usage Instructions:
// 1. Add necessary permissions to Android manifest: <uses-permission android:name="android.permission.READ_MEDIA_IMAGES" /> (for Android 13+)
//    or <uses-permission android:name="android.permission.READ_EXTERNAL_STORAGE" /> for older.
// 2. Before calling PickImageFromGallery, request permission: RequestStorageReadPermission(procedure begin PickImageFromGallery; end);
// 3. In your form's OnCreate: SetupImagePickerCallback(procedure(const Path: string) begin if not Path.IsEmpty then ProcessImage(Path); end);
// 4. To save: AddToPhotosAlbum(MyBitmap);

INTERFACE

USES
  System.SysUtils, System.IOUtils,
  FMX.Graphics, FMX.MediaLibrary, FMX.Platform;


TYPE
  TImageSelectedEvent = procedure(const Path: string) of object;

procedure RequestCameraPermission(const AOnGranted: TProc);
procedure RequestStorageReadPermission(const AOnGranted: TProc);       // For image picking on Android 13+

procedure AddToPhotosAlbum(const ABitmap: TBitmap);                    // Saves to gallery, handles indexing

procedure ScanMediaFile(const AFileName: string);                      // If manually saving files

procedure PickImageFromGallery;                                        // Opens the Photos/Gallery picker

// Paths
function  GetPublicPicturesFolder: string;


{$IFDEF ANDROID}
procedure SetupImagePickerCallback(const AOnImageSelected: TImageSelectedEvent);
{$ENDIF}


IMPLEMENTATION

uses
  LightCore.IO {$IFDEF ANDROID}
  , FMX.Platform.Android, Androidapi.Helpers, Androidapi.JNI.Os, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Net, Androidapi.JNI.App, Androidapi.JNI.Media, Androidapi.JNI.Provider, Androidapi.JNIBridge, FMX.Helpers.Android {$ENDIF};

CONST
  REQUEST_PICK_IMAGE = 1;



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
  if Assigned(AOnGranted) then AOnGranted;  // No permission needed on non-Android platforms
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
  if Assigned(AOnGranted) then AOnGranted;  // No permission needed on non-Android platforms
{$ENDIF}
end;


procedure AddToPhotosAlbum(const ABitmap: TBitmap);
VAR PhotoLibrary: IFMXPhotoLibrary;
begin
  // No need for ScanMediaFile here, as AddImageToSavedPhotosAlbum handles indexing internally on Android
  if TPlatformServices.Current.SupportsPlatformService(IFMXPhotoLibrary, PhotoLibrary)
  then PhotoLibrary.AddImageToSavedPhotosAlbum(ABitmap);
end;


procedure ScanMediaFile(const AFileName: string);
begin
{$IFDEF ANDROID}
  var Intent: JIntent;
  Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_MEDIA_SCANNER_SCAN_FILE);
  Intent.setData(TJnet_Uri.JavaClass.fromFile(TJFile.JavaClass.&init(StringToJString(AFileName))));
  TAndroidHelper.Activity.sendBroadcast(Intent);
{$ENDIF}
end;


// Before calling PickImageFromGallery, request permission: RequestStorageReadPermission(procedure begin PickImageFromGallery; end);
procedure PickImageFromGallery;
begin
{$IFDEF ANDROID}
  VAR Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_PICK);
  Intent.setType(StringToJString('image/*'));
  MainActivity.startActivityForResult(Intent, REQUEST_PICK_IMAGE);
{$ENDIF}
end;


{$IFDEF ANDROID}
function CopyUriToCache(const AUri: Jnet_Uri): string;
var
  InputStream: JInputStream;
  OutputStream: JFileOutputStream;
  CacheFile: JFile;
  BytesRead: Integer;
  JBuffer: TJavaArray<Byte>;
begin
  Result := '';
  OutputStream:= NIL;
  JBuffer:= NIL;

  // 1. Create a safe temp file
  CacheFile := TJFile.JavaClass.createTempFile(StringToJString('picked_'), StringToJString('.jpg'), TAndroidHelper.Context.getCacheDir);
  if CacheFile = nil then Exit; // Failed to create file

  // 2. Try to open the source URI
  InputStream := TAndroidHelper.ContentResolver.openInputStream(AUri);
  if InputStream = nil then 
  begin
    CacheFile.delete; // Clean up empty file
    Exit; // Return empty string
  end;

  try
    OutputStream := TJFileOutputStream.JavaClass.&init(CacheFile);
    try
      // Allocate Java Byte Array ONCE
      JBuffer := TJavaArray<Byte>.Create(4096);
      
      // 3. Loop and copy	  
      BytesRead := InputStream.read(JBuffer);
      while BytesRead > 0 do
      begin
        OutputStream.write(JBuffer, 0, BytesRead);
        BytesRead := InputStream.read(JBuffer);
      end;
      
      // 4. ONLY set result if we actually wrote something or finished without error
      Result := JStringToString(CacheFile.getAbsolutePath);
    finally
      OutputStream.close;
      JBuffer.Free; // Free the wrapper
    end;
  finally
    InputStream.close;
  end;
  
  // 5. Final check: verify file is not 0 bytes
  if  (Result <> '')
  AND (TJFile.JavaClass.&init(StringToJString(Result)).length = 0)
  then Result := '';  // File is empty, treat as failure
end;



// Call this once (e.g., in FormCreate) to handle the picker result asynchronously
// Callback receives the full file path or empty if canceled
procedure SetupImagePickerCallback(const AOnImageSelected: TImageSelectedEvent);
begin
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification,
    procedure(const Sender: TObject; const M: TMessage)
    var
      Msg: TMessageResultNotification absolute M;
      Path: string;
    begin
      if Msg.RequestCode = REQUEST_PICK_IMAGE then
      begin
        if Msg.ResultCode = TJActivity.JavaClass.RESULT_OK then
        begin
          {$IFDEF ANDROID}
          var Uri := Msg.Value.getData;
          // We MUST copy the file to a local path (Cache) because
          // TBitmap.LoadFromFile cannot read 'content://' URIs directly.
          Path := CopyUriToCache(Uri);
          {$ELSE}
          Path := ''; 
          {$ENDIF}
          
          if Assigned(AOnImageSelected)
          then AOnImageSelected(Path);
        end
        else
          if Assigned(AOnImageSelected)
          then AOnImageSelected('');
      end;
    end);
end;
{$ENDIF}



{-------------------------------------------------------------------------------------------------------------
   PATHS
-------------------------------------------------------------------------------------------------------------}

function GetPublicPicturesFolder: string;
begin
  {$IFDEF MSWINDOWS}
  Result := Trail(TPath.GetPicturesPath);
  {$ELSEIF DEFINED(ANDROID)}
  Result := Trail(TPath.GetSharedPicturesPath); // Public folder (Gallery) -  Ensure permissions or use MediaStore API
  {$ELSE}
  Result := Trail(TPath.GetDocumentsPath);
  {$ENDIF}
end;


end.
