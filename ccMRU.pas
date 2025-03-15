UNIT ccMRU;

{=============================================================================================================
   2025
   www.GabrielMoraru.com
==============================================================================================================

   MRU
   A list of the 'Most Recent Used' files.

   Also see:
      \Third party packages\Mru.pas
      AddFile2TaskbarMRU in csShell.pas

=============================================================================================================}

INTERFACE
USES System.SysUtils, System.Classes, System.AnsiStrings;

TYPE
  TMRUList = class(TObject)
   protected
     FMaxItems: Integer;
     FIniFile : string;
     FChanged: TNotifyEvent;
     procedure SetMaxItems(Value: integer);
     procedure setFileName(Value: string);
   public
     List: TStringList;
     constructor Create;
     destructor  Destroy;  override;
     function AddToMRU(aFileName: string): Boolean;                  // Adds a new file to the MRU list
     property FileName : string  read FIniFile  Write setFileName;    // Where we save the list?
     property MaxItems : integer read FMaxItems Write SetMaxItems;
     property OnChanged: TNotifyEvent read FChanged write FChanged;  // Event triggered when the MRU list changes
  end;


IMPLEMENTATION


{--------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------}
constructor TMRUList.Create;
begin
 inherited Create;
 FMaxItems:= 16;
 List:= TStringList.Create;
end;


destructor TMRUList.Destroy;
begin
 FreeAndNil(List);
 inherited;
end;




{--------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------}
procedure TMRUList.setFileName(Value: string);
begin
 FIniFile:= Value;
 if FileExists(FIniFile) then
  begin
   List.LoadFromFile(FIniFile);
   if Assigned(FChanged)
   then FChanged(Self);
  end;
end;


procedure TMRUList.SetMaxItems(Value: integer);
begin
 FMaxItems:= Value;
 WHILE List.Count> FMaxItems
   DO List.Delete(List.Count-1);
 if FIniFile<> ''
 then List.SaveToFile(FIniFile);
end;


{ Returns True if the file already existed in list }
function TMRUList.AddToMRU(aFileName: string): Boolean;
VAR i: Integer;
begin
 Result:=FALSE;
 if FileExists(aFileName) then
  begin
   List.Insert(0, aFileName);
   for i:= List.Count-1 downto 1 DO
    if CompareText(List.Strings[i], aFileName)= 0
    then
     begin
      List.Delete(i);
      Result:= TRUE;
     end;
    SetMaxItems(FMaxItems);
    if Assigned(FChanged) then FChanged(Self);
  end;
end;


end.
