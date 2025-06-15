unit LightVcl.Visual.DirectoryListBox;

{$R-,T-,H+,X+}
{.$WARN UNIT_PLATFORM OFF}   {Silence the 'W1005 Unit Vcl.FileCtrl is specific to a platform' warning }

{--------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file

  Features:
   * property ShowHidden;
   * property ShowSystem;

 Note: Don't set 'Parent:= Owner' in constructor.
       See this for details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
--------------------------------------------------------------------------------------------------}

INTERFACE

USES
   Winapi.Windows, System.SysUtils, System.AnsiStrings, System.Classes, Vcl.Controls, Vcl.FileCtrl;

TYPE
 TCubicDirListBox= class(TDirectoryListBox)
  protected
    FShowHidden : boolean;
    FShowSystem : boolean;
    procedure SetShowSystem(Value: boolean);
    procedure SetShowHidden(Value: boolean);
    function  ReadDirectoryNames(const ParentDirectory: string; DirectoryList: TStringList): Integer;
    function  getDirectory: string;
  public
    Constructor Create(AOwner : TComponent); override;
    procedure BuildList; override;                                                                 { Modified version of original Borland procedure, to support "Show hidden/system folders" }
  published
    property ShowHidden: boolean read FShowHidden write SetShowHidden default True;
    property ShowSystem: boolean read FShowSystem write SetShowSystem default True;
    property DirectoryTrail: string read getDirectory;                                             { Same as Directory but followed by a trail \ }
 end;


procedure Register;

IMPLEMENTATION

USES ccIO;




Constructor TCubicDirListBox.Create(AOwner : TComponent);
begin
 inherited Create(AOwner);
 //  Note: Don't set 'Parent:= Owner' in constructor. See this for details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
 FShowHidden:= True;
 FShowSystem:= True;
end;




procedure TCubicDirListBox.SetShowSystem(Value: boolean);
begin
 if FShowSystem= Value then Exit;
 FShowSystem:= Value;
 Update;
end;

procedure TCubicDirListBox.SetShowHidden(Value: boolean);
begin
 if FShowHidden= Value then Exit;
 FShowHidden:= Value;
 Update;
end;




function SlashSep(const Path, S: String): String;
begin
  if AnsiLastChar(Path)^ <> '\'
  then Result := Path + '\' + S
  else Result := Path + S;
end;


{$IFDEF msWindows}
 {$WARN SYMBOL_PLATFORM OFF}
{ Reads all directories in ParentDirectory, adds their paths to DirectoryList, and returns the number added }
function TCubicDirListBox.ReadDirectoryNames(const ParentDirectory: string; DirectoryList: TStringList): Integer;
var
  Status: Integer;
  SearchRec: TSearchRec;
  Attrib: integer;
begin
  Result := 0;
  Attrib:= faAnyFile;

  if NOT FShowSystem
  then Attrib:= Attrib XOR faSysFile;
  if NOT FShowHidden
  then Attrib:= Attrib XOR faHidden;

  Status := FindFirst(SlashSep(ParentDirectory, '*.*'), Attrib, SearchRec);
  try
   while Status = 0 do
    begin
     if (SearchRec.Attr and faDirectory = faDirectory) then
     begin
       if  (SearchRec.Name <> '.')
       AND (SearchRec.Name <> '..')
       AND (SearchRec.Name<> 'System Volume Information')                                          { exclus pt ca genera un "File access denied" cand dadeam dublu click pe el }
       then
        begin
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



procedure TCubicDirListBox.BuildList;                                                              { Modified version of original Borland procedure, to support "Show hidden/system folders" }
var
  TempPath: string;
  DirName: string;
  IndentLevel, BackSlashPos: Integer;
  VolFlags: DWORD;
  I: Integer;
  Siblings: TStringList;
  NewSelect: Integer;
  Root: string;
  PPreserveCase,PCaseSensitive: PBoolean;                                                          { MINE! }                              
begin
  TRY
    Items.BeginUpdate;
    Items.Clear;
    IndentLevel := 0;
    Root := ExtractFileDrive(Directory)+'\';
    GetVolumeInformation(PChar(Root), nil, 0, nil, DWORD(i), VolFlags, nil, 0);

    pPreserveCase := @PreserveCase;                                                                { MINE! }
    PCaseSensitive:= @CaseSensitive;                                                               { MINE! }
 
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
      if AnsiLastChar(TempPath)^ <> '\' then
      begin
        BackSlashPos := AnsiPos('\', TempPath);
        while BackSlashPos <> 0 do
        begin
          DirName := Copy(TempPath, 1, BackSlashPos - 1);
          if IndentLevel = 0 then DirName := DirName + '\';
          Delete(TempPath, 1, BackSlashPos);
          Items.AddObject(DirName, OpenedBMP);
          Inc(IndentLevel);
          BackSlashPos := AnsiPos('\', TempPath);
        end;
      end;
      Items.AddObject(TempPath, CurrentBMP);
    end;
    NewSelect := Items.Count - 1;
    Siblings := TStringList.Create;
    TRY
      Siblings.Sorted := True;

      { read all the dir names into Siblings }
      ReadDirectoryNames(Directory, Siblings);                                                     { Modified to support "Show hidden/system folders" }
      for i := 0 to Siblings.Count - 1 do
        Items.AddObject(Siblings[i], ClosedBMP);
    FINALLY
       FreeAndNil(Siblings );
    end;
  FINALLY
    Items.EndUpdate;
  END;
  if HandleAllocated then
    ItemIndex := NewSelect;
end;


function TCubicDirListBox.getDirectory: string;
begin
 Result:= Trail(Directory);
end;







procedure Register;
begin
  RegisterComponents('LightSaber', [TCubicDirListBox]);
end;

end.


