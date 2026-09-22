unit LightVcl.Visual.DirectoryListBox;

{$R-,T-,H+,X+}
{.$WARN UNIT_PLATFORM OFF}   {Silence the 'W1005 Unit Vcl.FileCtrl is specific to a platform' warning }

{--------------------------------------------------------------------------------------------------
   2026.09.21 FINAL
   www.GabrielMoraru.com

  Features:
   * property ShowHidden - shows/hides hidden folders
   * property ShowSystem - shows/hides system folders
   * property DirectoryTrail - returns Directory with trailing backslash
   * A folder that cannot be opened (protected folder, empty DVD drive, unavailable network share) does not crash:
       - double click / Enter: the user gets a message
       - SetDirectoryMsg: returns FALSE, logs, and the user gets a message
       - TrySetDirectory: returns FALSE and logs, no UI

  Note: 
     Don't set 'Parent:= Owner' in constructor.
     See this for details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
--------------------------------------------------------------------------------------------------}

INTERFACE

USES
   Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.FileCtrl;

TYPE
 TLightDirListBox= class(TDirectoryListBox)
  private
    function  setDirectoryEx(CONST Folder: string; ShowMsg: Boolean): Boolean;
  protected
    FShowHidden : boolean;
    FShowSystem : boolean;
    procedure SetShowSystem(Value: boolean);
    procedure SetShowHidden(Value: boolean);
    function  ReadDirectoryNames(const ParentDirectory: string; DirectoryList: TStringList): Integer;
    function  getDirectory: string;
    procedure DblClick; override;                                                                  { Shows a message instead of crashing when the folder cannot be opened }
    procedure KeyPress(var Key: Char); override;                                                   { Same, for the Enter key }
  public
    Constructor Create(AOwner : TComponent); override;
    procedure BuildList; override;                                                                 { Modified version of original Borland procedure, to support "Show hidden/system folders" }
    function  TrySetDirectory(CONST Folder: string): Boolean;                                      { Same as 'Directory:= Folder' but returns FALSE (and logs) instead of raising when the folder cannot be opened. Shows no UI. }
    function  SetDirectoryMsg(CONST Folder: string): Boolean;                                      { Same as TrySetDirectory, but also tells the user why the folder cannot be opened }
  published
    property ShowHidden: boolean read FShowHidden write SetShowHidden default True;
    property ShowSystem: boolean read FShowSystem write SetShowSystem default True;
    property DirectoryTrail: string read getDirectory;                                             { Same as Directory but followed by a trail \ }
 end;


procedure Register;

IMPLEMENTATION

USES LightCore, LightCore.IO, LightCore.AppData, LightVcl.Common.Dialogs;




Constructor TLightDirListBox.Create(AOwner : TComponent);
begin
 inherited Create(AOwner);
 //  Note: Don't set 'Parent:= Owner' in constructor. See this for details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
 FShowHidden:= True;
 FShowSystem:= True;
end;




procedure TLightDirListBox.SetShowSystem(Value: boolean);
begin
 if FShowSystem= Value then Exit;
 FShowSystem:= Value;
 BuildList;   { Must rebuild the list, not just repaint (Update only repaints) }
end;

procedure TLightDirListBox.SetShowHidden(Value: boolean);
begin
 if FShowHidden= Value then Exit;
 FShowHidden:= Value;
 BuildList;   { Must rebuild the list, not just repaint (Update only repaints) }
end;




{ Appends S to Path, ensuring a single backslash separator }
function SlashSep(const Path, S: String): String;
begin
  if (Path = '') OR (Path[Length(Path)] <> '\')
  then Result := Path + '\' + S
  else Result := Path + S;
end;


{$IFDEF msWindows}
 {$WARN SYMBOL_PLATFORM OFF}
{ Reads all directories in ParentDirectory, adds their names to DirectoryList, and returns the number added.
  Note: FindFirst's Attr parameter controls inclusion, not exclusion. To filter out hidden/system folders,
  we must check each found folder's attributes individually after FindFirst/FindNext returns them. }
function TLightDirListBox.ReadDirectoryNames(const ParentDirectory: string; DirectoryList: TStringList): Integer;
var
  Status: Integer;
  SearchRec: TSearchRec;
begin
  Assert(DirectoryList <> NIL, 'DirectoryList is nil in ReadDirectoryNames');
  Result := 0;

  Status := FindFirst(SlashSep(ParentDirectory, '*.*'), faAnyFile, SearchRec);
  try
   while Status = 0 do
    begin
     if (SearchRec.Attr and faDirectory = faDirectory) then
     begin
       { Skip parent/current directory references }
       if  (SearchRec.Name <> '.')
       AND (SearchRec.Name <> '..')
       AND (SearchRec.Name <> 'System Volume Information')                                         { Excluded - causes "File access denied" when double-clicked }
       then
        begin
         { Filter based on ShowHidden/ShowSystem settings }
         if (NOT FShowHidden) AND ((SearchRec.Attr and faHidden) <> 0) then
           begin
             Status := FindNext(SearchRec);
             Continue;
           end;
         if (NOT FShowSystem) AND ((SearchRec.Attr and faSysFile) <> 0) then
           begin
             Status := FindNext(SearchRec);
             Continue;
           end;

         DirectoryList.Add(SearchRec.Name);
         Inc(Result);
        end;
     end;
     Status := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;
 {$WARN SYMBOL_PLATFORM On}
{$ENDIF}



procedure TLightDirListBox.BuildList;                                                              { Modified version of original Borland procedure, to support "Show hidden/system folders" }
var
  TempPath: string;
  DirName: string;
  IndentLevel, BackSlashPos: Integer;
  VolFlags: DWORD;
  MaxCompLen: DWORD;                                                                               { Output param for GetVolumeInformation - max component length }
  i: Integer;
  Siblings: TStringList;
  NewSelect: Integer;
  Root: string;
  PPreserveCase, PCaseSensitive: PBoolean;                                                         { Pointer workaround to set read-only parent class properties }
begin
  VolFlags:= 0;
  TRY
    Items.BeginUpdate;
    Items.Clear;
    IndentLevel := 0;
    Root := ExtractFileDrive(Directory)+'\';
    GetVolumeInformation(PChar(Root), nil, 0, nil, MaxCompLen, VolFlags, nil, 0);

    { Workaround: PreserveCase and CaseSensitive are read-only properties in the parent class.
      We use pointer access to set them based on volume information. }
    pPreserveCase := @PreserveCase;
    PCaseSensitive:= @CaseSensitive;

    pPreserveCase^  := VolFlags and (FS_CASE_IS_PRESERVED or FS_CASE_SENSITIVE) <> 0;
    PCaseSensitive^ := (VolFlags and FS_CASE_SENSITIVE) <> 0;

    if (Length(Root) >= 2) and (Root[2] = '\') then
    begin
      Items.AddObject(Root, OpenedBMP);
      Inc(IndentLevel);
      TempPath := Copy(Directory, Length(Root)+1, Length(Directory));
    end
    else
      TempPath := Directory;
    if (Length(TempPath) > 0) then
    begin
      if (TempPath = '') OR (TempPath[Length(TempPath)] <> '\') then                               { Unicode-safe check for trailing backslash }
      begin
        BackSlashPos := Pos('\', TempPath);
        while BackSlashPos <> 0 do
        begin
          DirName := Copy(TempPath, 1, BackSlashPos - 1);
          if IndentLevel = 0 then DirName := DirName + '\';
          Delete(TempPath, 1, BackSlashPos);
          Items.AddObject(DirName, OpenedBMP);
          Inc(IndentLevel);
          BackSlashPos := Pos('\', TempPath);
        end;
      end;
      Items.AddObject(TempPath, CurrentBMP);
    end;
    NewSelect := Items.Count - 1;
    Siblings := TStringList.Create;
    TRY
      Siblings.Sorted := True;

      { Read all the dir names into Siblings }
      ReadDirectoryNames(Directory, Siblings);                                                     { Modified to support "Show hidden/system folders" }
      for i := 0 to Siblings.Count - 1 do
        Items.AddObject(Siblings[i], ClosedBMP);
    FINALLY
       FreeAndNil(Siblings);
    end;
  FINALLY
    Items.EndUpdate;
  END;
  if HandleAllocated then
    ItemIndex := NewSelect;
end;


function TLightDirListBox.getDirectory: string;
begin
 Result:= Trail(Directory);
end;




{--------------------------------------------------------------------------------------------------
   OPEN A FOLDER - NEVER CRASH
   Vcl.FileCtrl calls ChDir without first checking that the folder can be entered (ProcessPath, SetDir).
   Under $I+ ChDir raises EInOutError ('File access denied', 'I/O error 21', ...) for a protected folder,
   an empty DVD drive or a network share that is not available. That is a normal condition, not a bug.
--------------------------------------------------------------------------------------------------}

{ Logs why a folder could not be opened and, if ShowMsg, tells the user. Folder is empty when it is not known. }
procedure ReportOpenError(CONST Folder: string; E: EInOutError; ShowMsg: Boolean);
begin
 AppDataCore.LogWarn('Cannot open folder '+ Folder+ ' - '+ E.Message);
 if ShowMsg
 then MessageWarning('Cannot open this folder:'+ CRLF+ Folder+ LBRK+ E.Message+ LBRK+ 'You may not have the right to open it, or the drive or network share is not available.', 'Cannot open folder');
end;


{ Double click opens the folder (TDirectoryListBox.DblClick -> OpenCurrent) }
procedure TLightDirListBox.DblClick;
VAR Folder: string;
begin
 if ItemIndex >= 0
 then Folder:= GetItemPath(ItemIndex);

 TRY
   inherited DblClick;
 EXCEPT
   on E: EInOutError DO ReportOpenError(Folder, E, TRUE);
 END;
end;


{ Enter opens the folder (TDirectoryListBox.KeyPress -> OpenCurrent) }
procedure TLightDirListBox.KeyPress(var Key: Char);
VAR Folder: string;
begin
 if (Word(Key) = VK_RETURN) AND (ItemIndex >= 0)
 then Folder:= GetItemPath(ItemIndex);

 TRY
   inherited KeyPress(Key);
 EXCEPT
   on E: EInOutError DO ReportOpenError(Folder, E, TRUE);
 END;
end;


{ Same as 'Directory:= Folder', but returns FALSE instead of raising when the folder cannot be opened. }
function TLightDirListBox.setDirectoryEx(CONST Folder: string; ShowMsg: Boolean): Boolean;
VAR OldDirectory: string;
begin
 OldDirectory:= Directory;
 TRY
   Directory:= Folder;
   Result:= TRUE;
 EXCEPT
   on E: EInOutError DO
     begin
      if Directory <> OldDirectory
      then Update;   { SetDirectory had already switched the drive (DriveChange) when SetDir failed. Rebuild, so the list shows the folder that Directory now points at. }
      ReportOpenError(Folder, E, ShowMsg);
      Result:= FALSE;
     end;
 END;
end;


{ Shows nothing, only logs: for a caller where a message would be noise (program startup, a path typed letter by letter in an edit box). }
function TLightDirListBox.TrySetDirectory(CONST Folder: string): Boolean;
begin
 Result:= setDirectoryEx(Folder, FALSE);
end;


{ For a folder the user asked to open (a button, a drag and drop). }
function TLightDirListBox.SetDirectoryMsg(CONST Folder: string): Boolean;
begin
 Result:= setDirectoryEx(Folder, TRUE);
end;







procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TLightDirListBox]);
end;

end.


