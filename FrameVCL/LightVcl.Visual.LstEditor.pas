UNIT LightVcl.Visual.LstEditor;

interface
uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.ValEdit;

TYPE
 TCubicValueListEditor = class(TValueListEditor)
  private
  protected
  public
    procedure SaveToFile   (FileName: string);
    procedure LoadFromFile (FileName: string);
    procedure ClearAllRows;                                                                        { CLEAR }
  published
 end;


procedure Register;

IMPLEMENTATION
USES ccTextFile;







procedure TCubicValueListEditor.SaveToFile (FileName: string);
VAR rw, cl: Integer;
    TS: TStringList;
begin
 TS:= TStringList.Create;
 TS.Add(IntToStr(RowCount-1));                                                                          { nu salvez inforamtii despre capul de tabel }
 TS.Add(IntToStr(ColCount));

 for rw:= 1 TO RowCount-1 DO
  for cl:= 0 TO ColCount-1 DO
   TS.Add(Cells[cl, rw]);

 StringToFile(FileName, TS.Text, woOverwrite, wpAuto);
 FreeAndNil(TS);
end;



procedure TCubicValueListEditor.LoadFromFile(FileName: string);
VAR rw, cl, Rows, adres: Integer;
    TS: TStringList; OldOptions: TKeyOptions;
begin
 ClearAllRows; { CLEAR }

 TS      := StringFromFileTSL(FileName);
 Rows := StrToInt(TS.strings[0]);
 ColCount:= StrToInt(TS.strings[1]);                                                               { CREATE COLUMNS }

 { CREATE EMPTY ROWS }
 OldOptions:= KeyOptions;
 KeyOptions:= [];                                                                                  { daca keyUnique e prezent, atunci nu pot sa incar doua chei identice, asa ca trebuie sa disable asta temporar }
 for rw:= 1 to Rows
   DO InsertRow(' ', ' ', TRUE);                                                                    { 1 pentru ca exista un rand gol creat 'by default' }
 KeyOptions:= OldOptions;                                                                          { restore original options }

 { FILL CELLS WITH TEXT }
 Adres:= 2;
 for rw:= 1 TO Rows DO
   for cl:= 0 TO ColCount-1 DO
    begin
     Cells[cl, rw]:= TS.Strings[adres];
     inc(adres);
    end;
  FreeAndNil(TS {} );
end;



procedure TCubicValueListEditor.ClearAllRows;                                                      { CLEAR }
VAR rw: Integer;
begin
 for rw:= RowCount-1 downto 2
 DO DeleteRow(rw);                                                                                 { sterg randurile existente. Nu pot sa sterg capul de tabel si urmatorul rand dupa el. }
 Cells[0, 1]:= '';
 Cells[1, 1]:= '';
end;








procedure Register;
begin
  RegisterComponents('LightSaber', [TCubicValueListEditor]);
end;


end.












