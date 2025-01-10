UNIT cGraphCache;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
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
     * CCR Exif via cGraphLoader

-----------------------------------------------------------------------------------------------------------------------}

//ToDo 5: do image loading in a thread with cGraphLoader.Thread

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
    function  DeleteImage (CONST FileName: string): Boolean;                                       { Delete the original image from disk then the thumnail from cache. Returns true if the image was deleted. Search to see if the image exists in cache before deleting it. }
    function  DeleteThumb (CONST ThumbShortName: string): Boolean; overload;                       { Delete only the thumnail from cache. Returns true if the image was deleted. Search to see if the image exists in cache before deleting it. }
    function  DeleteThumb (CONST Position: integer): Boolean; overload;                            { Delete only the thumnail from cache. Returns true if the image was deleted. Search to see if the image exists in cache before deleting it. }
    function  FormatName  (iName, NameLength: Integer): string;                                    { format the file name of the new added thumbail so it will be 9 chars long }
   public
    ThumbsAreBitmaps: Boolean;                                                                     { the thumbs are saved in JPEG or BMP format? }
    ResamplerQuality: Byte;                                                                        { from 0 to 6 }
    
    constructor Create(CONST sCacheFolder: string);
    destructor  Destroy; override;
    
    procedure SaveDB;                                                                              { save the cache DB to disk }
    procedure LoadDB;

    function  GetThumbFor (CONST BigPicture: string): string;                                      { If the specified image already has a thumbnail then return its FULL  path, if not, create a thumb for it and return the path }
    function  ImagePosDB (FileName: string; OUT ShortThumPath: string): Integer; overload;         { There is a thumnail for this image in the database? If yes, returns its position in DB }
    function  ImagePosDB (FileName: string): Integer;                            overload;         { There is a thumnail for this image in the database? If yes, returns its position in DB }
    function  ThumbPosDB (ShortThumbName: string): Integer;                                        { Returns the position of this thumbnail in DB }

    function  MantainCache: Integer;                                                               { Delete all thumbnails from cache for which the original image doesn not exist anymore. Returns the number of deleted thumbnails }
    procedure ClearCache;                                                                          { CLEAR CACHE. Delete absolutelly everything from cache }

    property  CacheFolder: string  read FCacheFolder  write SetCacheFolder;                        { Path used to store the cached files }

    property  ThumbWidth : Integer read FThumbWidth   write SetThumbWidth  default 128;            {TODO: when this property is changed, clear the entire cache }
    property  ThumbHeight: Integer read FThumbHeight  write SetThumbHeight default 96;
  end;


  
IMPLEMENTATION

uses
   ccINIFile, cbAppDataForm, cGraphLoader, ccCore, cbDialogs, cbAppData, ccIO, cmIO, cmIO.Win;




{--------------------------------------------------------------------------------------------------
   THUMB POSITION
--------------------------------------------------------------------------------------------------}
function TCacheObj.GetThumbFor (CONST BigPicture: string): string;                                 { If the specified image already has a thumbnail then return its FULL path, if not, create a thumb for it and return the path }
VAR ThumPath: string;
begin
 if FileExistsMsg(BigPicture) AND IsImage(BigPicture)
 then
   if (ImagePosDB (BigPicture, ThumPath)> -1)
   then Result:= CacheFolder+ ThumPath
   else Result:= CacheFolder+ addToCache(BigPicture)                                               { ADD TO CACHE }
 else Result:= '';
end;


function TCacheObj.ImagePosDB (FileName: string; OUT ShortThumPath: string): Integer;             { There is a thumnail for this image in the database? If yes, returns its position in DB and its SHORT name }
VAR CurEntry: Integer;
begin
 Result:= -1;

 FileName:= LowerCase(FileName);
 for CurEntry:= 0 to DbPicts.Count-1 DO
  if FileName= DbPicts[CurEntry] then
   begin
    Result:= CurEntry;
    ShortThumPath:= DbThumbs[CurEntry];

    if NOT FileExists(CacheFolder+ ShortThumPath)                                                  { poate thumbnail-ul e in DB dar a fost sters manual de pe disk }
    then
     begin
      DeleteThumb(CurEntry);                                                                       { Delete it from DB - ASTA E CRITIC }
      ShortThumPath:= AddToCache(FileName);                                                       { ADD TO CACHE }
     end;
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
 BMP:= cGraphLoader.ExtractThumbnail(FileName, ThumbWidth, Dummy, Dummy, Dummy2);

 Result:= BMP <> Nil;
 if NOT Result then Exit;

 if ThumbsAreBitmaps
 then
   BMP.SaveToFile(CacheFolder+ ShortThumbName)   { save it to disk as BMP }
 else
   TRY
     JPG:= TJPEGImage.Create;
     try
       TRY
         JPG.Assign(BMP);
       EXCEPT
         on E: Exception do
          begin
           AppData.LogError(E.ClassName+': '+ E.Message + ' - '+ FileName);
           Exit(False);
          end;
       end;

       JPG.SaveToFile(CacheFolder+ ShortThumbName);   { save it to disk as JPG }
     finally
       FreeAndNil(JPG);
     end;

   FINALLY
      FreeAndNil(BMP);
   END;
end;


function TCacheObj.AddToCache(const FileName: string): string;                           { Add this image into the cache and return its path }
begin
 Result:= '';
 //del FileName:= LowerCase(FileName);                                                         { for faster search work only with LowerCase text }
 if NOT FileExists(FileName) then EXIT;

 inc(FLastEntry);
 Result:= FormatName(FLastEntry, 9);                                                     { format the file name of the new added thumbail so it will be 9 chars long }
 if MakeThumb(FileName, Result) then                                                     { convert and write thumb to disk }
  begin
   DbPicts .Add(FileName);
   DbThumbs.Add(Result);
  end;
end;





{--------------------------------------------------------------------------------------------------
   DELETE
--------------------------------------------------------------------------------------------------}
function TCacheObj.DeleteImage (CONST FileName: string): Boolean;                                 { Delete the original image from disk then the thumnail from cache. Returns true if the image was deleted. Search to see if the image exists in cache before deleting it. }
begin
 Result:= FALSE;
 DeleteFile(FileName);
 DeleteThumb(ImagePosDB(FileName));
end;


function TCacheObj.DeleteThumb (CONST ThumbShortName: string): Boolean;                            { Delete only the thumnail from cache. Returns true if the image was deleted. Search to see if the image exists in cache before deleting it. }
VAR Position: Integer;
begin
 Result:= FALSE;
 Position:= ThumbPosDB(ThumbShortName);
 DbPicts .Delete(Position);
 DbThumbs.Delete(Position);

 DeleteFile(CacheFolder+ ThumbShortName);     
end;


function TCacheObj.DeleteThumb (CONST Position: integer): Boolean;                                 { Delete only the thumnail from cache. Returns true if the image was deleted. Search to see if the image exists in cache before deleting it. }
VAR ThumbName: string;
begin
 Result:= FALSE;
 Assert(Position>= 0, 'Cannot delete thumb at position '+ IntToStr(Position));
 Assert(DbThumbs.count > 0, 'Cannot delete thumb '+ IntToStr(Position)+ '! The DB is empty.');

 ThumbName:= DbThumbs[Position];
 DbPicts .Delete(Position);
 DbThumbs.Delete(Position);
 DeleteFile(CacheFolder+ ThumbName);
end;




{--------------------------------------------------------------------------------------------------
   SAVE/LOAD
--------------------------------------------------------------------------------------------------}
procedure TCacheObj.SaveDB;                                                                        { save the cache DB to disk }
VAR CacheSettings: TIniFileEx;
begin
 TRY
  DbPicts .SaveToFile(CacheFolder+ 'CacheDBInput');
  DbThumbs.SaveToFile(CacheFolder+ 'CacheDBOutput');
 except
  //todo 1: trap only specific exceptions
  MesajError('Cannot save cache database!');
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


procedure TCacheObj.LoadDB;                                                                        { save the cache DB to disk }
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


procedure TCacheObj.setCacheFolder(Value: string);
begin
 ForceDirectories(Value);
 if TestWriteAccess(Value) then                                                                    { Check if this folder has write permitions }
  begin
   FCacheFolder:= Trail(Value);
   ForceDirectoriesMsg(Value);
  end
 else MesajError('You don''t have write permissions for this folder.'+ CRLFw+ Value);
end;







{--------------------------------------------------------------------------------------------------
   CLEAR/MANTAIN
--------------------------------------------------------------------------------------------------}
procedure TCacheObj.ClearCache;                                                                    { CLEAR CACHE. Delete absolutelly everything from cache }
begin
 DbPicts.Clear;
 DbThumbs.Clear;
 EmptyDirectory(CacheFolder);                                                               { Delete all files in the specified folder, but don't delete the folder itself. It will search also in subfolders }
 SaveDB;
end;


function TCacheObj.MantainCache: Integer;                                                          { Delete all thumbnails from cache for which the original image doesn not exist anymore. Returns the number of deleted thumbnails }
VAR i, DbIndex, CurThumb: Integer;
    AllThumbs: TStringList;
    Exista: Boolean;
    sFileType: string;
begin
 Result:= 0;
 if DbThumbs.Count<> DbPicts.Count                                                                 { check }
 then MesajError('Number of entries in DB does not match! You maight fix this by reseting your cache.');

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

 AllThumbs:= ListFilesOf(CacheFolder, sFileType, FALSE, FALSE);                                       { intoarce toate fisierele cu extensia 'Extension' din directorul curent}

 if DbThumbs.Count= 0                                                                              { if I have no files in DB }
 then EmptyDirectory(CacheFolder)                                                           { delete all thumbs on disk }
 else
   for CurThumb:= 0 TO AllThumbs.Count-1 DO
    begin
     Exista:= FALSE;
     for DbIndex:= DbThumbs.Count-1 DOWNTO 0 DO                                                    { For all entries in DB. Trebuie neaparat DownTo pt ca de fiecare data cand sterg un thumb, DB ramane cu mai putine intrari, si o dau in bara cand apelez 'DeleteThumb(DbIndex)' }
       if AllThumbs[CurThumb]= DbThumbs[DbIndex] then
         begin
           Exista:= TRUE;
           Break;
         end;
      if NOT Exista AND (DbIndex> -1) then                                                         { DbIndex= -1 apara atunci cand am sters toate intrarile din DB }
       begin
         DeleteThumb(DbIndex);
         Inc(Result);
       end;
    end;

 FreeAndNil( AllThumbs );
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
   x
--------------------------------------------------------------------------------------------------}
function TCacheObj.FormatName(iName, NameLength: Integer): string;                                 { format the file name of the new added thumbail so it will be 9 chars long }
begin
 if ThumbsAreBitmaps                                                          
 then Result:= LeadingZeros(IntToStr(iName), NameLength)+ '.BMP'
 else Result:= LeadingZeros(IntToStr(iName), NameLength)+ '.JPG';
end;


end.
