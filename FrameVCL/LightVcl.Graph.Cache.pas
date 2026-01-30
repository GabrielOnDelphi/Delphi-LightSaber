UNIT LightVcl.Graph.Cache;

{=============================================================================================================
   Gabriel Moraru
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------

  Thumbnail caching system.
  Creates thumbnails for specified images.
  The thumbs are stored in 'CacheFolder'.

  Features:
     Read images from disk
     Generate thumbnails
     Add/removes images to/from cache
     Access thumbnails

  Used by: ??? (not in Bx)

  External dependencies:
     * CCR Exif via LightVcl.Graph.Loader

-----------------------------------------------------------------------------------------------------------------------}

//ToDo 5: do image loading in a thread with LightVcl.Graph.Loader.Thread

INTERFACE

USES
   System.SysUtils, Classes, Vcl.Graphics, Vcl.Imaging.jpeg;

TYPE
  TCacheObj= class(TObject)
   private
    FCacheFolder: string;
    FThumbWidth,
    FThumbHeight : Integer;
    FLastEntry: Integer;                                                                           { The name of the last added thumbnail }
    DbPicts,                                                                                       { Contains only full file name and path }
    DbThumbs: TStringList;                                                                         { Contains only the short name of the thumbnail }
    procedure setCacheFolder (Value: string);
    procedure setThumbWidth  (Value: Integer);
    procedure setThumbHeight (Value: Integer);
    function  MakeThumb(CONST FileName, ShortThumbName: string): Boolean;
   protected
    function  AddToCache  (const FileName: string): string;                                        { add this image into the cache and return its FULL path }
    function  DeleteImage (CONST FileName: string): Boolean;                                       { Delete the original image from disk then the thumbnail from cache. Returns true if the image was deleted. Search to see if the image exists in cache before deleting it. }
    function  DeleteThumb (CONST ThumbShortName: string): Boolean; overload;                       { Delete only the thumbnail from cache. Returns true if the image was deleted. Search to see if the image exists in cache before deleting it. }
    function  DeleteThumb (CONST Position: integer): Boolean; overload;                            { Delete only the thumbnail from cache. Returns true if the image was deleted. Search to see if the image exists in cache before deleting it. }
    function  FormatName  (iName, NameLength: Integer): string;                                    { format the file name of the new added thumbnail so it will be 9 chars long }
   public
    ThumbsAreBitmaps: Boolean;                                                                     { TRUE = save thumbs as BMP, FALSE = save as JPEG }
    ResamplerQuality: Byte;                                                                        { Reserved for future use: resampling quality from 0 (fast) to 6 (best). Currently not implemented. }
    
    constructor Create(CONST sCacheFolder: string);
    destructor  Destroy; override;
    
    procedure SaveDB;                                                                              { save the cache DB to disk }
    procedure LoadDB;

    function  GetThumbFor (CONST BigPicture: string): string;                                      { If the specified image already has a thumbnail then return its FULL  path, if not, create a thumb for it and return the path }
    function  ImagePosDB (FileName: string; OUT ShortThumPath: string): Integer; overload;         { There is a thumnail for this image in the database? If yes, returns its position in DB }
    function  ImagePosDB (FileName: string): Integer;                            overload;         { There is a thumnail for this image in the database? If yes, returns its position in DB }
    function  ThumbPosDB (ShortThumbName: string): Integer;                                        { Returns the position of this thumbnail in DB }

    function  MaintainCache: Integer;                                                              { Delete all thumbnails from cache for which the original image does not exist anymore. Returns the number of deleted thumbnails }
    procedure ClearCache;                                                                          { Clears the cache. Deletes all thumbnails and resets the database }

    property  CacheFolder: string  read FCacheFolder  write SetCacheFolder;                        { Path used to store the cached files }

    property  ThumbWidth : Integer read FThumbWidth   write SetThumbWidth  default 128;            {TODO: when this property is changed, clear the entire cache }
    property  ThumbHeight: Integer read FThumbHeight  write SetThumbHeight default 96;
  end;


  
IMPLEMENTATION

uses
   LightCore.INIFile, LightVcl.Graph.Loader, LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.Dialogs, LightCore.AppData, LightCore.IO, LightVcl.Common.IO;




{--------------------------------------------------------------------------------------------------
   THUMB POSITION
--------------------------------------------------------------------------------------------------}

{ Returns the FULL path to the thumbnail for the given image.
  If a thumbnail already exists in cache, returns its path.
  If not, generates a new thumbnail and returns its path.
  Returns empty string if the file doesn't exist or is not an image. }
function TCacheObj.GetThumbFor (CONST BigPicture: string): string;
VAR ThumPath: string;
begin
 Result:= '';
 if NOT FileExistsMsg(BigPicture) then EXIT;
 if NOT IsImage(BigPicture) then EXIT;

 if ImagePosDB(BigPicture, ThumPath) > -1
 then Result:= CacheFolder+ ThumPath
 else
  begin
   ThumPath:= AddToCache(BigPicture);
   if ThumPath <> ''
   then Result:= CacheFolder+ ThumPath;
  end;
end;


{ Checks if a thumbnail exists for the given image in the database.
  Returns the position in DB (>=0) if found, -1 if not found.
  ShortThumPath receives the short thumbnail filename.
  If the thumbnail file was deleted from disk, it recreates the cache entry. }
function TCacheObj.ImagePosDB (FileName: string; OUT ShortThumPath: string): Integer;
VAR CurEntry: Integer;
begin
 Result:= -1;
 ShortThumPath:= '';

 FileName:= LowerCase(FileName);
 for CurEntry:= 0 to DbPicts.Count-1 DO
  if FileName= DbPicts[CurEntry] then
   begin
    ShortThumPath:= DbThumbs[CurEntry];

    if NOT FileExists(CacheFolder+ ShortThumPath) then                                             { The thumbnail is in DB but was manually deleted from disk }
     begin
      DeleteThumb(CurEntry);                                                                       { Delete stale entry from DB }
      ShortThumPath:= AddToCache(FileName);                                                        { Regenerate thumbnail }
      { After re-adding, the new entry is at the end of the list }
      if ShortThumPath <> ''
      then Result:= DbPicts.Count - 1
      else Result:= -1;
     end
    else
      Result:= CurEntry;                                                                           { Found and valid }
    Break;
   end;
end;


function TCacheObj.ImagePosDB (FileName: string): Integer;                                        { There is a thumnail for this image in the database? If yes, returns its position in DB }
VAR CurEntry: Integer;
begin
 Result:= -1;
 FileName:= LowerCase(FileName);
 for CurEntry:= 0 to DbPicts.Count-1 DO
  if FileName= DbPicts[CurEntry] then
   begin
    Result:= CurEntry;
    Break;
   end;
end;


function TCacheObj.ThumbPosDB (ShortThumbName: string): Integer;                                   { returns the position of this thum in DB }
VAR CurEntry: Integer;
begin
 Result:= -1;
 ShortThumbName:= LowerCase(ShortThumbName);
 for CurEntry:= 0 to DbThumbs.Count-1 DO
  if ShortThumbName= DbThumbs[CurEntry] then
   begin
    Result:= CurEntry;
    Break;
   end;
end;




{--------------------------------------------------------------------------------------------------
   ADD
--------------------------------------------------------------------------------------------------}

{ Generate thumbnail and save to cache as JPG }
function TCacheObj.MakeThumb(CONST FileName, ShortThumbName: string): Boolean;
VAR BMP: TBitmap;
    JPG: TJPEGImage;
    Dummy: Integer; Dummy2: Cardinal;
begin
 BMP:= LightVcl.Graph.Loader.ExtractThumbnail(FileName, ThumbWidth, Dummy, Dummy, Dummy2);

 Result:= BMP <> Nil;
 if NOT Result then Exit;

 TRY
   if ThumbsAreBitmaps
   then
     BMP.SaveToFile(CacheFolder+ ShortThumbName)   { save it to disk as BMP }
   else
     begin
       JPG:= TJPEGImage.Create;
       try
         TRY
           JPG.Assign(BMP);
         EXCEPT
           on E: Exception do
            begin
             AppDataCore.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
             Exit(False);
            end;
         end;

         JPG.SaveToFile(CacheFolder+ ShortThumbName);   { save it to disk as JPG }
       finally
         FreeAndNil(JPG);
       end;
     end;
 FINALLY
    FreeAndNil(BMP);
 END;
end;


{ Generates a thumbnail for the given image and adds it to the cache.
  Returns the short thumbnail filename on success, or empty string on failure. }
function TCacheObj.AddToCache(const FileName: string): string;
VAR ShortThumbName: string;
begin
 Result:= '';
 if NOT FileExists(FileName) then EXIT;

 inc(FLastEntry);
 ShortThumbName:= FormatName(FLastEntry, 9);                                             { Format thumbnail filename to be 9 chars long (e.g., 000000001.JPG) }
 if MakeThumb(FileName, ShortThumbName) then                                             { Generate and write thumbnail to disk }
  begin
   DbPicts .Add(FileName);
   DbThumbs.Add(ShortThumbName);
   Result:= ShortThumbName;
  end;
end;





{--------------------------------------------------------------------------------------------------
   DELETE
--------------------------------------------------------------------------------------------------}
{ Deletes the original image from disk then removes its thumbnail from cache.
  Returns TRUE if the image was successfully deleted. }
function TCacheObj.DeleteImage (CONST FileName: string): Boolean;
VAR Position: Integer;
begin
 Result:= DeleteFile(FileName);
 Position:= ImagePosDB(FileName);
 if Position >= 0
 then DeleteThumb(Position);
end;


{ Deletes the thumbnail from cache by its short name.
  Returns TRUE if successfully deleted. Raises exception if thumbnail not found in DB. }
function TCacheObj.DeleteThumb (CONST ThumbShortName: string): Boolean;
VAR Position: Integer;
begin
 Position:= ThumbPosDB(ThumbShortName);
 if Position < 0
 then raise Exception.Create('TCacheObj.DeleteThumb: Thumbnail not found in DB: ' + ThumbShortName);

 DbPicts .Delete(Position);
 DbThumbs.Delete(Position);

 Result:= DeleteFile(CacheFolder+ ThumbShortName);
end;


{ Deletes the thumbnail at the specified position from cache.
  Returns TRUE if successfully deleted. Raises exception for invalid position. }
function TCacheObj.DeleteThumb (CONST Position: integer): Boolean;
VAR ThumbName: string;
begin
 if Position < 0
 then raise Exception.Create('TCacheObj.DeleteThumb: Cannot delete thumb at position '+ IntToStr(Position));
 if DbThumbs.Count <= 0
 then raise Exception.Create('TCacheObj.DeleteThumb: Cannot delete thumb at position '+ IntToStr(Position)+ '. The DB is empty.');
 if Position >= DbThumbs.Count
 then raise Exception.Create('TCacheObj.DeleteThumb: Position '+ IntToStr(Position)+ ' is out of range. DB has '+ IntToStr(DbThumbs.Count)+ ' entries.');

 ThumbName:= DbThumbs[Position];
 DbPicts .Delete(Position);
 DbThumbs.Delete(Position);
 Result:= DeleteFile(CacheFolder+ ThumbName);
end;




{--------------------------------------------------------------------------------------------------
   SAVE/LOAD
--------------------------------------------------------------------------------------------------}
{ Saves the cache database (picture paths and thumbnail names) to disk }
procedure TCacheObj.SaveDB;
VAR CacheSettings: TIniFileEx;
begin
 TRY
  DbPicts .SaveToFile(CacheFolder+ 'CacheDBInput');
  DbThumbs.SaveToFile(CacheFolder+ 'CacheDBOutput');
 EXCEPT
  on E: EFCreateError do
    MessageError('Cannot create cache database file: '+ E.Message);
  on E: EFOpenError do
    MessageError('Cannot open cache database file: '+ E.Message);
  on E: EWriteError do
    MessageError('Cannot write to cache database file: '+ E.Message);
  on E: Exception do
    MessageError('Cannot save cache database: '+ E.Message);
 END;

 CacheSettings:= TIniFileEx.Create('Cache', CacheFolder+ 'CacheSettings.ini');
 TRY
   CacheSettings.Write('ThumbWidth' , FThumbWidth);
   CacheSettings.Write('ThumbHeight', FThumbHeight);
   CacheSettings.Write('LastEntry'  , FLastEntry);
 FINALLY
   FreeAndNil(CacheSettings);
 END;
end;


procedure TCacheObj.LoadDB;                                                                        { Load the cache DB from disk }
VAR CacheSettings: TIniFileEx;
begin
 if FileExists(CacheFolder+ 'CacheDBInput')
 then DbPicts .LoadFromFile(CacheFolder+ 'CacheDBInput');

 if FileExists(CacheFolder+ 'CacheDBOutput')
 then DbThumbs.LoadFromFile(CacheFolder+ 'CacheDBOutput');

 CacheSettings:= TIniFileEx.Create('Cache', CacheFolder+ 'CacheSettings.ini');
 TRY
   FThumbWidth := CacheSettings.Read('ThumbWidth' , 128);
   FThumbHeight:= CacheSettings.Read('ThumbHeight',  96);
   FLastEntry  := CacheSettings.Read('LastEntry'  ,   1);
 FINALLY
   FreeAndNil(CacheSettings);
 END;
end;


{ Sets the cache folder path after validating write permissions.
  Creates the directory if it doesn't exist. }
procedure TCacheObj.setCacheFolder(Value: string);
begin
 if Value = ''
 then raise Exception.Create('TCacheObj.setCacheFolder: Value parameter cannot be empty');

 ForceDirectoriesMsg(Value);                                                                        { Create folder if needed, show error on failure }
 if CanWriteToFolder(Value) then                                                                    { Check if this folder has write permissions }
   FCacheFolder:= Trail(Value)
 else MessageError('You don''t have write permissions for this folder.'+ CRLFw+ Value);
end;







{--------------------------------------------------------------------------------------------------
   CLEAR/MAINTAIN
--------------------------------------------------------------------------------------------------}

{ Clears the entire cache. Deletes all thumbnails and resets the database. }
procedure TCacheObj.ClearCache;
begin
 DbPicts.Clear;
 DbThumbs.Clear;
 EmptyDirectory(CacheFolder);                                                               { Delete all files in the specified folder, but don't delete the folder itself. It will search also in subfolders }
 SaveDB;
end;


function TCacheObj.MaintainCache: Integer;                                                         { Delete all thumbnails from cache for which the original image does not exist anymore. Returns the number of deleted thumbnails }
VAR i, DbIndex, CurThumb: Integer;
    AllThumbs: TStringList;
    Exista: Boolean;
    sFileType: string;
begin
 Result:= 0;
 if DbThumbs.Count<> DbPicts.Count                                                                 { check }
 then MessageError('Number of entries in DB does not match! You might fix this by resetting your cache.');

 { Delete thumbs for non existing images }
 for i:= DbPicts.Count-1 DOWNTO 0 DO
  if NOT (FileExists(DbPicts[i]) AND FileExists(CacheFolder+ DbThumbs[i]))
  then
   begin
    DeleteThumb(i);
    inc(Result);
   end;

 { Delete thumbs that are located on disk but they are no longer present in DB }
 if ThumbsAreBitmaps
 then sFileType:= '*.bmp'
 else sFileType:= '*.jpg';

 AllThumbs:= ListFilesOf(CacheFolder, sFileType, FALSE, FALSE);                                       { Returns all files with the 'Extension' from the current folder }
 TRY
   if DbThumbs.Count= 0                                                                              { If I have no files in DB }
   then EmptyDirectory(CacheFolder)                                                                  { Delete all thumbs on disk }
   else
     { Check each thumbnail file on disk to see if it exists in the DB }
     for CurThumb:= 0 TO AllThumbs.Count-1 DO
      begin
       Exista:= FALSE;
       for DbIndex:= 0 TO DbThumbs.Count-1 DO
         if AllThumbs[CurThumb]= DbThumbs[DbIndex] then
           begin
             Exista:= TRUE;
             Break;
           end;
       { If thumbnail file on disk is NOT in DB, delete the orphan file directly }
       if NOT Exista then
         begin
           DeleteFile(CacheFolder+ AllThumbs[CurThumb]);
           Inc(Result);
         end;
      end;
 FINALLY
   FreeAndNil(AllThumbs);
 END;
end;



{--------------------------------------------------------------------------------------------------
   THUMBNAIL SIZE
--------------------------------------------------------------------------------------------------}
procedure TCacheObj.setThumbWidth  (Value: Integer);
begin
 if Value<> FThumbWidth then
  begin
   FThumbWidth:= Value;
   ClearCache;                                                                                     { all images in cache are invalid now }
  end;
end;


procedure TCacheObj.setThumbHeight (Value: Integer);
begin
 if Value<> FThumbHeight then
  begin
   FThumbHeight:= Value;
   ClearCache;                                                                                     { all images in cache are invalid now }
  end;
end;


{--------------------------------------------------------------------------------------------------
   CREATE/DESTROY
--------------------------------------------------------------------------------------------------}
constructor TCacheObj.Create(CONST sCacheFolder: string);
begin
 inherited Create;
 if sCacheFolder = ''
 then raise Exception.Create('TCacheObj.Create: sCacheFolder parameter cannot be empty');

 CacheFolder:= sCacheFolder;
 DbPicts  := TStringList.Create{(AOwner: TComponent)};
 DbThumbs := TStringList.Create{(AOwner: TComponent)};
 FThumbWidth := 128;
 FThumbHeight:=  96;
 ThumbsAreBitmaps:= FALSE;                                                                         { the thumbs are saved in JPEG or BMP format? }
 ResamplerQuality:= 6;
end;


destructor TCacheObj.Destroy;
begin
 SaveDB;
 FreeAndNil(DbPicts);
 FreeAndNil(DbThumbs);
 inherited;
end;


{--------------------------------------------------------------------------------------------------
   UTILITIES
--------------------------------------------------------------------------------------------------}

{ Formats the thumbnail filename with leading zeros to ensure consistent sorting.
  Example: FormatName(42, 9) returns '000000042.JPG' (or .BMP if ThumbsAreBitmaps is TRUE) }
function TCacheObj.FormatName(iName, NameLength: Integer): string;
begin
 if ThumbsAreBitmaps                                                          
 then Result:= LeadingZeros(IntToStr(iName), NameLength)+ '.BMP'
 else Result:= LeadingZeros(IntToStr(iName), NameLength)+ '.JPG';
end;


end.
