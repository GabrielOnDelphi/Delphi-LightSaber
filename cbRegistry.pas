UNIT cbRegistry;

{=============================================================================================================
   Gabriel Moraru
   2024.06
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

   Allows us to work with registry more easily (without manuall creating and destroying a TRegistry object)

   See ccIO.GetProgramFilesDir for example of usage
=============================================================================================================}

INTERFACE
USES
   Winapi.Windows, System.SysUtils, System.Classes, System.Win.Registry;

CONST
   LazyWrite       = TRUE;
  // InstantWrite    = FALSE;

 function Convert_HKey2Str      (CONST Key: HKEY): string;
 function Convert_Str2HKey      (CONST Key: string): HKEY;

 function RegKeyExist           (CONST Root: HKEY; CONST Key: string): Boolean;
 function RegValueExist         (CONST Root: HKEY; CONST Key, ValueName: string) : Boolean;
 function RegHasSubKeys         (CONST Root: HKEY; CONST Key: string): Boolean;

 function RegDeleteKey          (CONST Root: HKEY; CONST Key: string): Boolean;
 function RegClearKey           (CONST Root: HKEY; CONST Key: string): Boolean;                                                           { Deletes all value/name paires inside the key but don't delete the key }
 function RegDeleteValue        (CONST Root: HKEY; CONST Key, ValueName: string): Boolean;

 function RegWriteString        (CONST Root: HKEY; CONST Key, ValueName,         ValueData: string;  Lazy: Boolean= TRUE): Boolean;       { Writes a string to the specified key in the registry }
 function RegWriteInteger       (CONST Root: HKEY; CONST Key, ValueName: string; ValueData: Integer; Lazy: Boolean= TRUE): Boolean;       { Writes a integer to the specified key in the registry }
 function RegWriteBool          (CONST Root: HKEY; CONST Key, ValueName: string; ValueData: Boolean; Lazy: Boolean= TRUE): Boolean;
 function RegWriteDate          (CONST Root: HKEY; CONST Key, ValueName: string; ValueData: TDate;   Lazy: Boolean= TRUE): Boolean;
 function RegReadDate           (CONST Root: HKEY; CONST Key, ValueName: string; CanCreate: Boolean= FALSE): TDateTime;
 function RegReadBool           (CONST Root: HKEY; CONST Key, ValueName: string; DefValData: Boolean= FALSE): Boolean;
 function RegReadInteger        (CONST Root: HKEY; CONST Key, ValueName: string; DefValData: Integer= 0): Integer;
 function RegReadString         (CONST Root: HKEY; CONST Key, ValueName: string; CONST DefValData: string= ''): string;

 function RegEnumSubKeys        (CONST Root: HKEY; const Key: string): TStringList;   { ! }

 function RegWriteValuePairs    (CONST Root: HKEY; CONST Key: string; Pairs     : TStringList; CONST Delimiter: char; Lazy     : Boolean= TRUE) : Boolean;
 function RegReadValuePairs     (CONST Root: HKEY; CONST Key: string; Pairs     : TStringList; CanCreate: Boolean= FALSE): Boolean;    { Enumerate all name/values pares contained in the specified key }
 function RegReadValueNames     (CONST Root: HKEY; CONST Key: string; ValueNames: TStringList; CanCreate: Boolean= FALSE): Boolean;    { Returns all values contained in the specified key }  //I think the old name was RegReadNames
 function RegReadValueDatas     (CONST Root: HKEY; CONST Key: string; ValueDatas: TStringList; CanCreate: Boolean= FALSE): Boolean;    { Returns all keys contained in the specified path }
 function RegReadMultiSzString  (CONST Root: HKEY; CONST Key, KeyName: string;                 CanCreate: Boolean= FALSE): string;     { Reads a REG_MULTI_SZ value From the Registry. This will return strings separated by ENTER }
 function RegReadMultiSzStringSP(CONST Root: HKEY; CONST Key, KeyName: string;                 CanCreate: Boolean= FALSE): string;     { This will return strings separated by SPACE }


IMPLEMENTATION
USES ccCore;




{--------------------------------------------------------------------------------------------------
   REGISTRY - EXISTS
--------------------------------------------------------------------------------------------------}
function RegKeyExist(CONST Root: HKEY; CONST Key: string): Boolean;
VAR Rg: TRegistry;
begin
 Rg:= TRegistry.Create(KEY_READ);
 TRY
  Rg.RootKey:= Root;
  Result:= RG.KeyExists(Key);
  Rg.CloseKey;
 FINALLY
   FreeAndNil(Rg);
 END;
end;


function RegValueExist(CONST Root: HKEY; CONST Key, ValueName: string): Boolean;
VAR Rg: TRegistry;
begin
 Rg:= TRegistry.Create(KEY_READ);
 TRY
   Rg.RootKey:= Root;
   Rg.OpenKey(Key, FALSE);
   Result:= RG.ValueExists(ValueName);
   Rg.CloseKey;
 FINALLY
   FreeAndNil(Rg);
 END;
end;


function RegHasSubKeys (CONST Root: HKEY; CONST Key: string): Boolean;
VAR Rg: TRegistry;
begin
 Rg:= TRegistry.Create(KEY_READ);
 TRY
   Rg.RootKey:= Root;
   Rg.OpenKey(Key, FALSE);
   Result:= RG.HasSubKeys;
   Rg.CloseKey;
 FINALLY
   FreeAndNil(Rg);
 END;
end;




{--------------------------------------------------------------------------------------------------
   REGISTRY - DELETE
--------------------------------------------------------------------------------------------------}

{ Delete the entire key with all values inside }
function RegDeleteKey(CONST Root: HKEY; CONST Key: string): Boolean;
VAR Rg: TRegistry;
begin
 Rg:= TRegistry.Create(KEY_WRITE);
 TRY
  Rg.RootKey:= Root;
  Result:= Rg.DeleteKey(Key);
  Rg.CloseKey;
 FINALLY
   FreeAndNil(Rg);
 END;
end;


{ Delete the specified value in the specified key }
function RegDeleteValue(CONST Root: HKEY; CONST Key, ValueName: string): Boolean;
VAR Rg: TRegistry;
begin
 Rg:= TRegistry.Create(KEY_ALL_ACCESS);
 TRY
  Result:= FALSE;
  Rg.RootKey:= Root;
  Rg.OpenKey(Key, FALSE);

  if RG.ValueExists(ValueName)
  then Result:= Rg.DeleteValue(ValueName);
  Rg.CloseKey;
 FINALLY
   FreeAndNil(Rg);
 END;
end;


{ Deletes all values inside the key but don't delete the key itself }
function RegClearKey(CONST Root: HKEY; CONST Key: string): Boolean;
VAR Rg: TRegistry;
    s: string;
    TSL: TStringList;
begin
 Rg := TRegistry.Create(KEY_ALL_ACCESS);
 TSL:= TStringList.Create;
 TRY
   Rg.RootKey:= Root;
   Result:= Rg.OpenKey(Key, FALSE);
   if Result then
    begin
     Rg.GetValueNames(TSL);
     for s in TSL
      DO Rg.DeleteValue(s);
    end;
   Rg.CloseKey;
 FINALLY
   FreeAndNil(Rg);
   FreeAndNil(TSL);
 END;
end;






{--------------------------------------------------------------------------------------------------
   REGISTRY - READ/WRITE
--------------------------------------------------------------------------------------------------}

{ Example: RegWriteString(HKEY_CURRENT_USER, 'Software\Microsoft\Defrag', 'Data', Value, True, True) }
function RegWriteString (CONST Root: HKEY; CONST Key, ValueName, ValueData: string; Lazy: Boolean= TRUE): Boolean;
VAR Rg: TRegistry;
begin
 Result:= FALSE;
 Rg:= TRegistry.Create(KEY_WRITE);
 TRY
   Rg.RootKey:= Root;
   Rg.LazyWrite:= Lazy;
   if Rg.OpenKey(Key, TRUE) then
    begin
     Rg.WriteString(ValueName, ValueData);
     Rg.CloseKey;
     Result:= TRUE;
    end;
 FINALLY
   FreeAndNil(Rg);
 END;
end;


function RegWriteDate (CONST Root: HKEY; CONST Key, ValueName: string; ValueData: TDate; Lazy: Boolean= TRUE): Boolean;
VAR Rg: TRegistry;
begin
 Result:= FALSE;
 Rg:= TRegistry.Create(KEY_WRITE);
 Rg.RootKey:= Root;
 Rg.LazyWrite:= Lazy;
 TRY
  if Rg.OpenKey(Key, TRUE) then
  BEGIN
   Rg.WriteDate(ValueName, ValueData);
   Rg.CloseKey;
   Result:= TRUE;
  END
 FINALLY
   FreeAndNil(Rg);
 end;
end;


function RegWriteInteger (CONST Root: HKEY; CONST Key, ValueName: string; ValueData: integer; Lazy: Boolean= TRUE): Boolean;
VAR Rg: TRegistry;
begin
 Result:= FALSE;
 Rg:= TRegistry.Create(KEY_WRITE);
 Rg.RootKey:= Root;
 Rg.LazyWrite:= Lazy;
 TRY
  if Rg.OpenKey(Key, TRUE) then
  BEGIN
   Rg.WriteInteger(ValueName, ValueData);
   Rg.CloseKey;
   Result:= TRUE;
  END
 FINALLY
   FreeAndNil(Rg);
 end;
end;


function RegWriteBool (CONST Root: HKEY; CONST Key, ValueName: string; ValueData: Boolean; Lazy: Boolean= TRUE): Boolean;
VAR Rg: TRegistry;
begin
 Result:= FALSE;
 Rg:= TRegistry.Create(KEY_WRITE);
 Rg.RootKey:= Root;
 Rg.LazyWrite:= Lazy;
 TRY
  if Rg.OpenKey(Key, TRUE) then
  BEGIN
   Rg.WriteBool(ValueName, ValueData);
   Rg.CloseKey;
   Result:= TRUE;
  END
 FINALLY
   FreeAndNil(Rg);
 end;
end;


function RegReadBool (CONST Root: HKEY; CONST Key, ValueName: string; DefValData: Boolean= FALSE): Boolean;
VAR Rg: TRegistry;
begin
 Rg:= TRegistry.Create(KEY_READ);
 Rg.RootKey:= Root;
 TRY
   if  Rg.OpenKey(Key, FALSE)
   AND Rg.ValueExists(ValueName)
   then Result:= Rg.ReadBool(ValueName)
   else Result:= DefValData;
   Rg.CloseKey;
 FINALLY
   FreeAndNil(Rg);
 END;
end;


function RegReadInteger (CONST Root: HKEY; CONST Key, ValueName: string; DefValData: Integer= 0): Integer;
VAR Rg: TRegistry;
begin
 Rg:= TRegistry.Create(KEY_READ);
 Rg.RootKey:= Root;
 TRY
  if  Rg.OpenKey(Key, FALSE)
  AND Rg.ValueExists(ValueName)
  then Result:= Rg.ReadInteger(ValueName)
  else Result:= DefValData;
  Rg.CloseKey;
 FINALLY
   FreeAndNil(Rg);
 end;
end;


function RegReadDate (CONST Root: HKEY; CONST Key, ValueName: string; CanCreate: Boolean= FALSE): TDateTime;
VAR Rg: TRegistry;
begin
 Result:= -1;
 Rg:= TRegistry.Create(KEY_READ);
 Rg.RootKey:= Root;
 TRY
  if  Rg.OpenKey(Key, CanCreate)
  AND Rg.ValueExists(ValueName)
  then Result:= Rg.ReadDate(ValueName);
  Rg.CloseKey;
 FINALLY
   FreeAndNil(Rg);
 end;
end;


function RegReadString (CONST Root: HKEY; CONST Key, ValueName: string; CONST DefValData: String= ''): string;
VAR Rg: TRegistry;
begin
 Result:= '';
 Rg:= TRegistry.Create(KEY_READ);
 TRY
   Rg.RootKey:= Root;
   if  Rg.OpenKey(Key, FALSE)
   AND Rg.ValueExists(ValueName)
   then Result:= Rg.ReadString(ValueName)
   else Result:= DefValData;
   Rg.CloseKey;
 FINALLY
   FreeAndNil(Rg);
 END;
end;







{--------------------------------------------------------------------------------------------------
   REGISTRY - Multiple entries
--------------------------------------------------------------------------------------------------}

{ Returns all 'value name' / 'value data' paires contained in the specified key, separated by '=' }   { old name: RegReadStringEnumerate }
function RegReadValuePairs(CONST Root: HKEY; CONST Key: string; Pairs: TStringList; CanCreate: Boolean= FALSE): Boolean;
VAR Rg: TRegistry;
     i: Integer;
begin
 if Pairs= NIL
 then raise exception.Create('Paires is NIL') at @RegReadValuePairs;

 Rg:= TRegistry.Create(KEY_READ);
 Pairs.NameValueSeparator:= '=';
 TRY
   Rg.RootKey:= Root;
   Result:= Rg.OpenKey(Key, CanCreate);
   if Result then
    begin
     Rg.GetValueNames(Pairs);
     for I:=0 to Pairs.Count-1 DO
      Pairs.Strings[I]:= Pairs.Strings[I]+ '='+ rg.ReadString(Pairs.Strings[I]);  { read the associated values for those keys }
    end;
   Rg.CloseKey;
 FINALLY
   FreeAndNil(Rg);
 END;
end;


{ Not tested! }
function RegWriteValuePairs (CONST Root: HKEY; CONST Key: string; Pairs: TStringList; CONST Delimiter: char; Lazy: Boolean= TRUE): Boolean;
VAR s, ValueName, ValueData: string;
begin
 Result:= FALSE;
 for s in Pairs DO
  begin
   ccCore.SplitString(s, Delimiter, ValueName, ValueData);
   Result:= RegWriteString(Root, Key, ValueName, ValueData, Lazy);
  end;
end;



{ Returns a list of 'value names' contained in the specified key }
function RegReadValueNames(CONST Root: HKEY; CONST Key: string; ValueNames: TStringList; CanCreate: Boolean= FALSE): Boolean;
VAR Rg: TRegistry;
begin
 if ValueNames= NIL
 then raise exception.Create('ValueNames is NIL') at @RegReadValueNames;

 Rg:= TRegistry.Create(KEY_READ);
 TRY
   Rg.RootKey:= Root;
   Result:= Rg.OpenKey(Key, CanCreate);
   if Result
   then Rg.GetValueNames(ValueNames);    { read list of key names - Returns a string list containing the names of all data values associated with the current key. }
   Rg.CloseKey;
 FINALLY
   FreeAndNil(Rg);
 END;
end;


{ Returns the values of sub-keys contained in the specified key. NOTE: the sub-keys must all be of string type else an error occurs! }
function RegReadValueDatas(CONST Root: HKEY; CONST Key: string; ValueDatas: TStringList; CanCreate: Boolean= FALSE): Boolean;
VAR Rg: TRegistry;
     i: Integer;
begin
 if ValueDatas= NIL
 then raise exception.Create('ValueDatas is NIL') at @RegReadValueNames;

 Rg:= TRegistry.Create(KEY_READ);
 TRY
   Rg.RootKey:= Root;
   Result:= Rg.OpenKey(Key, CanCreate);
   if Result then
    begin
     Rg.GetValueNames(ValueDatas);
     for I:=0 to ValueDatas.Count-1
      DO ValueDatas.Strings[I]:= rg.ReadString(ValueDatas.Strings[I]);   { Read the associated values for those sub-keys }
    end;
   Rg.CloseKey;
 FINALLY
   FreeAndNil(Rg);
 END;
end;



{ Tested ok }
function RegEnumSubKeys(CONST Root: HKEY; CONST Key: string): TStringList;
var
  Rg: TRegistry;
begin
  Rg := TRegistry.Create;
  TRY
    Rg.RootKey := Root;
    Rg.OpenKeyReadOnly(Key);
    Result:= TStringList.Create;
    Rg.GetKeyNames(Result);
  FINALLY
   FreeAndNil(Rg);
  End;
end;





{--------------------------------------------------------------------------------------------------
   REGISTRY - MulstiSZ
--------------------------------------------------------------------------------------------------}
{ Reads a REG_MULTI_SZ value From the Registry. This will return strings separated by ENTER.  From here: http://www.swissdelphicenter.ch/torry/showcode.php?id=1431 }
function RegReadMultiSzString;
VAR
  regKey: TRegistry;
  vSize: integer;
begin
 Result:= '';
 regKey:= TRegistry.Create(KEY_READ);
 TRY
   regKey.RootKey:= Root;

   if regKey.OpenKey(Key, FALSE) then
    begin
      vSize:= regKey.GetDataSize(KeyName);

      if vSize > 0 then
       begin
        SetLength(Result, vSize);
        regKey.ReadBinaryData(KeyName, Result[1], vSize);
        ReplaceString(Result, #0, CRLFw);
       end;
   end;
 FINALLY
   FreeAndNil(regKey);
 END;
end;


{ This will return strings separated by SPACE }
function RegReadMultiSzStringSp(CONST Root: HKEY; CONST Key, KeyName: string; CanCreate: Boolean= FALSE): string;
begin
 Result:= RegReadMultiSzString(Root, Key, KeyName, CanCreate);
 Result:= StringReplace(Result, CRLFw, ' ', [rfReplaceAll]);
end;





{--------------------------------------------------------------------------------------------------
   REGISTRY - CONVETORS
--------------------------------------------------------------------------------
    Documentation:
          The registry contains two basic elements: keys and values.
          The keys are container objects similar to folders. Keys may contain values or further keys (sub-keys).
          The values are objects similar to files. The values are split in 'Value name' and 'Value data'

    Examples: http://jedi.grizzlydev.com/www/art_registry.html
--------------------------------------------------------------------------------------------------}

CONST HKEYNames: array[0..6] of string =
    ('HKEY_CLASSES_ROOT',     'HKEY_CURRENT_USER',   'HKEY_LOCAL_MACHINE', 'HKEY_USERS',
     'HKEY_PERFORMANCE_DATA', 'HKEY_CURRENT_CONFIG', 'HKEY_DYN_DATA');

function Convert_HKey2Str(CONST Key: HKEY): string;
begin
 if (Key< HKEY_CLASSES_ROOT) OR (Key> HKEY_CLASSES_ROOT+6)
 then Result := ''
 else Result := HKEYNames[Key - HKEY_CLASSES_ROOT];
end;


function Convert_Str2HKey(CONST Key: string): HKEY;
VAR i: Byte;
begin
 Result:= $0;
 for i:= Low(HKEYNames) TO High(HKEYNames) do
   if SameText(HKEYNames[i], Key)
   then Result:= HKEY_CLASSES_ROOT+ i;
end;



end.
