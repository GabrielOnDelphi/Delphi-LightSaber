UNIT LightCore.MRU;

{=============================================================================================================
   2025
   www.GabrielMoraru.com
==============================================================================================================

   MRU (Most Recently Used) File List

   Maintains a list of recently accessed files, automatically persisted to disk.
   - Duplicate files are moved to the top (not added twice)
   - List size is capped at MaxItems
   - OnChanged event fires when list is modified

   Also see:
      \Third party packages\Mru.pas
      AddFile2TaskbarMRU in LightVcl.Common.Shell.pas

=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes;

TYPE
  TMRUList = class(TObject)
  private
    FMaxItems: Integer;
    FIniFile: string;
    FChanged: TNotifyEvent;
    FList: TStringList;
    procedure SetMaxItems(Value: Integer);
    procedure SetFileName(Value: string);
    procedure TrimToMaxItems;
    procedure DoChanged;
  public
    constructor Create;
    destructor Destroy; override;

    function AddToMRU(CONST aFileName: string): Boolean;
    procedure Clear;
    function Count: Integer;
    function GetItem(Index: Integer): string;

    property FileName: string read FIniFile write SetFileName;      { File path for persistence }
    property MaxItems: Integer read FMaxItems write SetMaxItems;    { Maximum entries (default 16) }
    property OnChanged: TNotifyEvent read FChanged write FChanged;  { Fires on list modification }
    property Items: TStringList read FList;                         { Read-only access to list }
  end;


IMPLEMENTATION


{--------------------------------------------------------------------------------------------------
   CONSTRUCTOR / DESTRUCTOR
--------------------------------------------------------------------------------------------------}

constructor TMRUList.Create;
begin
  inherited Create;
  FMaxItems:= 16;
  FList:= TStringList.Create;
end;


destructor TMRUList.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;


{--------------------------------------------------------------------------------------------------
   INTERNAL HELPERS
--------------------------------------------------------------------------------------------------}

procedure TMRUList.DoChanged;
begin
  if Assigned(FChanged)
  then FChanged(Self);
end;


procedure TMRUList.TrimToMaxItems;
begin
  while FList.Count > FMaxItems do
    FList.Delete(FList.Count - 1);
end;


{--------------------------------------------------------------------------------------------------
   PROPERTIES
--------------------------------------------------------------------------------------------------}

procedure TMRUList.SetFileName(Value: string);
begin
  FIniFile:= Value;
  if FileExists(FIniFile)
  then begin
    FList.LoadFromFile(FIniFile);
    TrimToMaxItems;
    DoChanged;
  end;
end;


procedure TMRUList.SetMaxItems(Value: Integer);
VAR
  WasChanged: Boolean;
begin
  if Value < 1 then Value:= 1;

  FMaxItems:= Value;
  WasChanged:= FList.Count > FMaxItems;
  TrimToMaxItems;

  if WasChanged then 
  begin
    if FIniFile <> ''
    then FList.SaveToFile(FIniFile);
    DoChanged;
  end;
end;


{--------------------------------------------------------------------------------------------------
   PUBLIC METHODS
--------------------------------------------------------------------------------------------------}

{ Adds a file to the MRU list (at the top).
  If the file already exists in the list, it's moved to the top.
  Returns True if the file was already in the list (moved), False if newly added. }
function TMRUList.AddToMRU(CONST aFileName: string): Boolean;
VAR i: Integer;
begin
  Result:= FALSE;

  if NOT FileExists(aFileName) then EXIT;

  { Insert at top }
  FList.Insert(0, aFileName);

  { Remove any duplicates (case-insensitive comparison) }
  for i:= FList.Count - 1 downto 1 do
  begin
    if SameText(FList[i], aFileName)
    then begin
      FList.Delete(i);
      Result:= TRUE;  { Was already in list }
    end;
  end;

  { Enforce max items limit }
  TrimToMaxItems;

  { Persist to file }
  if FIniFile <> ''
  then FList.SaveToFile(FIniFile);

  DoChanged;
end;


procedure TMRUList.Clear;
begin
  FList.Clear;

  if FIniFile <> ''
  then FList.SaveToFile(FIniFile);

  DoChanged;
end;


function TMRUList.Count: Integer;
begin
  Result:= FList.Count;
end;


function TMRUList.GetItem(Index: Integer): string;
begin
  if (Index >= 0) AND (Index < FList.Count)
  then Result:= FList[Index]
  else Result:= '';
end;


end.
