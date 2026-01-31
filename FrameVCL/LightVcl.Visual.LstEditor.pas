UNIT LightVcl.Visual.LstEditor;

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt

   A TValueListEditor descendant with file persistence (save/load) and clear functionality.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.ValEdit;

TYPE
 TCubicValueListEditor = class(TValueListEditor)
  public
    procedure SaveToFile   (CONST FileName: string);
    procedure LoadFromFile (CONST FileName: string);
    procedure ClearAllRows;
 end;


procedure Register;

IMPLEMENTATION
USES LightCore.TextFile;


procedure TCubicValueListEditor.SaveToFile(CONST FileName: string);
VAR rw, cl: Integer;
    TS: TStringList;
begin
 TS:= TStringList.Create;
 TRY
   TS.Add(IntToStr(RowCount-1));                                                                       { Don't save info about the table header row }
   TS.Add(IntToStr(ColCount));

   for rw:= 1 TO RowCount-1 DO
     for cl:= 0 TO ColCount-1 DO
       TS.Add(Cells[cl, rw]);

   StringToFile(FileName, TS.Text, woOverwrite, wpAuto);
 FINALLY
   FreeAndNil(TS);
 END;
end;



procedure TCubicValueListEditor.LoadFromFile(CONST FileName: string);
VAR rw, cl, Rows, adres: Integer;
    TS: TStringList;
    OldOptions: TKeyOptions;
begin
 ClearAllRows;

 TS:= StringFromFileTSL(FileName);
 TRY
   Rows    := StrToInt(TS.strings[0]);
   ColCount:= StrToInt(TS.strings[1]);

   { Create empty rows }
   OldOptions:= KeyOptions;
   KeyOptions:= [];                                                                                    { If keyUnique is present, can't load two identical keys, so disable it temporarily }
   for rw:= 1 to Rows
     DO InsertRow(' ', ' ', TRUE);                                                                     { Start at 1 because there's already one empty row created by default }
   KeyOptions:= OldOptions;                                                                            { Restore original options }

   { Fill cells with text }
   Adres:= 2;
   for rw:= 1 TO Rows DO
     for cl:= 0 TO ColCount-1 DO
      begin
       Cells[cl, rw]:= TS.Strings[adres];
       inc(adres);
      end;
 FINALLY
   FreeAndNil(TS);
 END;
end;



procedure TCubicValueListEditor.ClearAllRows;
VAR rw: Integer;
begin
 for rw:= RowCount-1 downto 2                                                                          { Start at 2: can't delete header row (0) and the first data row (1) }
   DO DeleteRow(rw);
 Cells[0, 1]:= '';
 Cells[1, 1]:= '';
end;




procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TCubicValueListEditor]);
end;


end.
