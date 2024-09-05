UNIT dutWin64;

{ Porting code to 64 bit.
  Checks for:
     * invalid pointer typecasts
     * use of "extended"
     * invalid parameters in WinAPI fuctions }
//ToDo: check for $FF000000 in most cases, on 64 bit it should be NativeUInt($FF000000).

INTERFACE

USES
  System.SysUtils, System.Classes, cmSearchResult, dutBase;

type
  TDutWin64= class(TDUTBase)
   private
    procedure findApiParam(Body: TStringList; const Needle: string);
   public
    // Pointer casts
    procedure FindLongIntCast(RelaxedSearch: Boolean= true);
    procedure FindPointer(Replace: Boolean);
    procedure FindPointerRelax;
    // Extended
    procedure FindPackedExtended;
    procedure FindExtended;
    // WinApi
    procedure FindPerform;
    procedure FindSendPostMessage;
    procedure FindSetWindowLong;
  end;

function Is32bitTypecast(const Line: string; StartPos: Integer): Boolean;

IMPLEMENTATION

USES
   cmPascal, ccCore, ccIO;

{=============================================================================================================
   EXTENDED

   On Win32, the size of System.Extended type is 10 bytes.
   However, on Win64 the System.Extended type is an alias for System.Double, which is only 8 bytes!
   There is no 10-byte equivalent for Extended on 64 bit!
=============================================================================================================}

{ Find packed records that have an 'Extended' fields.
  The "packed" keyword can indicate that the record might be saved to disk. In this case we need to make sure that the size of the data remains the same, no matter if we are on Win32 or Win 64. }
procedure TDutWin64.FindPackedExtended;
const
   MaxRecSize = 100;
var
   TextBody: TStringList;
   iColumn, iLine: Integer;
   RecordStart: Integer;
begin
  TextBody:= StringFromFileTSL(SearchResults.Last.FileName);
  try
     iLine:= 0;
     while iLine < TextBody.Count do
      begin
        // Search for the start of a packed record
        if PosInsensitive('packed record', TextBody[iLine]) > 0 then
          begin
            RecordStart:= iLine;
            Inc(iLine); // next line

            while iLine < TextBody.Count do
             begin
               // End of record?
               if PosInsensitive('end;', TextBody[iLine]) > 0 then break;

               // Safety in case we don't detect end of record correctly!
               if (iLine - RecordStart) >= MaxRecSize then
                begin
                  break;  // Stop if the end of rec is not comming after 100 lines!
                end;

               // Find Extended fields
               iColumn:= PosInsensitive('Extended;', TextBody[iLine]);
               if iColumn > 0
               then SearchResults.Last.AddNewPos(iLine, iColumn, TextBody[iLine], 'Extended in packed records', 'Replace the Extended with Double');

               Inc(iLine);
             end;
          end;
        Inc(iLine);
      end;

  finally
    FreeAndNil(TextBody);
  end;
end;


{ This function looks for occurrences of the Extended type and reports them.
  It is recommended to replace Extended with Double. }
procedure TDutWin64.FindExtended;
var
  TextBody: TStringList;
  iColumn, iLine: Integer;
  sLine: String;
begin
  TextBody := StringFromFileTSL(SearchResults.Last.FileName);
  try
    for iLine := 0 to TextBody.Count - 1 do
    begin
      // Skip commented lines
      sLine:= TextBody[iLine];
      if LineIsAComment(sLine) then Continue;

      // Find Extended type declarations
      iColumn := PosInsensitive('Extended', sLine);
      if iColumn > 0 then
        // Check that it's not a part of a longer word (e.g., "ExtendedType")
        if (iColumn + Length('Extended') <= Length(sLine))
        AND (NOT CharIsLetter(sLine[iColumn + Length('Extended')]) )
        then SearchResults.Last.AddNewPos(iLine, iColumn, sLine, 'Extended type found', 'Replace Extended with Double');
    end;
  finally
    FreeAndNil(TextBody);
  end;
end;


{=============================================================================================================
   POINTER CAST
   Checking for invalid 32 bit pointer casts.
=============================================================================================================}

{  Returns true if an 32 bit typecast was found on this line of code.
   StartPos is the column from which I start the search in this line. }

function Is32bitTypecast(const Line: string; StartPos: Integer): boolean;
begin
  Result:= (PosInsensitive('LongInt(' , Line) > StartPos) or
           (PosInsensitive('Integer(' , Line) > StartPos) or
           (PosInsensitive('Cardinal(', Line) > StartPos);
end;


{ToDo: implement this check:
    if Pos('Move(') > 0 and '*4' or (SizeOf not found) then Found:= true;

    Explanation:
       Correct:
       Move(FSelection[Index + 1], FSelection[Index], (FSelectionCount - Index - 1) * SizeOf(Pointer));

       Wrong:
       Move(FSelection[Index + 1], FSelection[Index], (FSelectionCount - Index - 1) * 4); }


{ Find possible LongInt/PLongInt typecasts.
  On Windows, LongInt is always 32bit! }
procedure TDutWin64.FindLongIntCast(RelaxedSearch: Boolean= true);
var
   TextBody: TStringList;
   sLine: string;
   iLine: Integer;
   iColumn: Integer;
begin
  TextBody:= StringFromFileTSL(SearchResults.Last.FileName);
  TRY
    for iLine:= 0 to TextBody.Count-1 do
      begin
        sLine:= TextBody[iLine];

        if RelaxedSearch
        then
          begin
            { Relaxed search }

            // Search PLongInt first...
            iColumn:= WordPos('PLongInt(', sLine);
            if iColumn > 0
            then SearchResults.Last.AddNewPos(iLine, iColumn, sLine, 'Possible issue with PLongInt cast.', 'Replace with NPativeInt.')
            else
              begin
                // ...and only after that for LongInt, in order to show the correct msg.
                iColumn:= WordPos('LongInt(', sLine);
                if iColumn > 0
                then SearchResults.Last.AddNewPos(iLine, iColumn, sLine, 'Possible issue with LongInt cast.', 'Replace with NativeInt or PByte (in case of pointer arithmetics).');
              end;
          end
        else
          begin
            { Explicit search }

            iColumn:= WordPos('LongInt(Self)', sLine);
            if iColumn > 0
            then SearchResults.Last.AddNewPos(iLine, iColumn, sLine, 'Issue with LongInt cast.', 'Replace the LongInt with NativeInt.');
          end;
      end;

  FINALLY
    FreeAndNil(TextBody);
  END;
end;


{ Searches for "Pointer(Integer(" and similar issues.
  On Win64 we cannot assume anymore that SizeOf(Pointer)=SizeOf(Integer/Cardinal/Longint).
  If Replace is True, the bad code will be automatically fixed. }
procedure TDutWin64.FindPointer(Replace: Boolean);
var
   TextBody: TStringList;
   sLine: string;
   Found: Boolean;
   iLine: Integer;

   procedure Find(var aLine: String; const Offender, Fix: string);
   begin
       var iColumn:= PosInsensitive(Offender, aLine);
       if iColumn < 1 then Exit;

       Found:= True;
       SearchResults.Last.AddNewPos(iLine, iColumn, sLine, 'Win64 incompatible typecast: '+ Offender, ' Use '+ Fix+ ' instead.');

       if Replace then
        begin
         aLine:= ReplaceString(aLine, Offender, Fix);  // This search is insensitive
         TextBody[iLine]:= aLine;
        end;
   end;

begin
  Found:= False;

  TextBody:= StringFromFileTSL(SearchResults.Last.FileName);
  try
    for iLine:= 0 to TextBody.Count-1 do
      begin
        sLine:= TextBody[iLine];
        if LineIsAComment(sLine) then Continue;

        Find(sLine, 'Pointer(PInteger', 'Pointer(PNativeInt');
        Find(sLine, 'Pointer(Integer' , 'Pointer(NativeInt or PByte( for pointer arithmetics.');
        Find(sLine, 'Pointer(Cardinal', 'Pointer(NativeUInt or PByte( for pointer arithmetics.');
        //Don't search for Pointer(LongInt here. We have a dedicatedfunction for it!
      end;

    if Found and Replace then
      begin
        if BackupFile
        then BackupFileBak(SearchResults.Last.FileName);
        StringToFile(SearchResults.Last.FileName, TextBody.Text, woOverwrite, TRUE);
      end;

  finally
    FreeAndNil(TextBody);
  end;
end;


{ Search POSSIBLE pointer typecasts that are invalid based on "Pointer(",
  but ignore all "Pointer(NativeInt" or "Pointer(NativeUInt" occurences.
  This can retun LOTS of fake positive results! }
procedure TDutWin64.FindPointerRelax;
var
   TextBody: TStringList;
   sLine: string;
   iLine: Integer;
begin
  TextBody:= StringFromFileTSL(SearchResults.Last.FileName);
  try
    for iLine:= 0 to TextBody.Count-1 do
      begin
        sLine:= TextBody[iLine];

        // Search ANY pointer typecasts
        var iColumn:= PosInsensitive('Pointer(', sLine);
        if iColumn > 0 then
          // But ignore valid typecasts
          if  (PosInsensitive('Pointer(NativeInt',  sLine) < 1)
          and (PosInsensitive('Pointer(NativeUInt', sLine) < 1) then
              SearchResults.Last.AddNewPos(iLine, iColumn, sLine, 'Chance of invalid pointer typecast: Pointer(', 'Check if the cast is correct.');
      end;

  finally
    FreeAndNil(TextBody);
  end;
end;



{=============================================================================================================
   WIN API ISSUES

   Looks invalid typecasts in the parameters of .Perform(), SendMessage(), PostMessage().
   This could give some false positive results.
=============================================================================================================}

procedure TDutWin64.findApiParam(Body: TStringList; const Needle: string);
var
  sLine: string;
  iColumn: Integer;
begin
for var iLine:= 0 to Body.Count-1 do
  begin
    sLine:= Body[iLine];
    if LineIsAComment(sLine) then Continue;   // Ignore lines that start with a comment symbol:   // { (*

    iColumn:= PosInsensitive(Needle, sLine);
    if iColumn > 0 then
     begin
       // We ignore cases where the WParam/LParam are 0.
       // Ex: SendMessage(Handle,CM_Exit,0,0);
       if Pos('0,0)' , sLine, iColumn) > 1 then Continue;
       if Pos('0, 0)', sLine, iColumn) > 1 then Continue;

       // We ignore cases where both WParam and LParam are found.
       if  (PosInsensitive('WPARAM', sLine)> 1)
       AND (PosInsensitive('LPARAM', sLine)> 1) then Continue;

       // We specifically look for 32 bit typecasts
       // Example: SendMessage(hWnd, WM_SETTEXT, 0, Integer(@MyCharArray));
       if Is32bitTypecast(sLine, iColumn)
       then SearchResults.Last.AddNewPos(iLine, iColumn, sLine, 'Possible invalid LongInt, Integer or Cardinal cast in SendMessage API function!', 'Use WParam/LParam instead!');
     end;
  end;
end;


{ Signature: SendMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM); }
procedure TDutWin64.FindSendPostMessage;   // old name: FindSendMessage
begin
 var TextBody:= StringFromFileTSL(SearchResults.Last.FileName);
 try
   findApiParam(TextBody, 'SendMessage(');
   findApiParam(TextBody, 'PostMessage(');
 finally
   FreeAndNil(TextBody);
 end;
end;


{ Check for valid parameters in the TComponent.Perform method.
  Signature:
       function Perform(Msg: Cardinal; WParam: WPARAM; LParam: LPARAM): LRESULT;
       function Perform(Msg: Cardinal; WParam: WPARAM; LParam: PChar) : LRESULT;
       function Perform(Msg: Cardinal; WParam: WPARAM; var LParam: TRect): LRESULT;  }
procedure TDutWin64.FindPerform;
begin
 var TextBody:= StringFromFileTSL(SearchResults.Last.FileName);
 try
   findApiParam(TextBody, '.Perform(');
 finally
   FreeAndNil(TextBody);
 end;
end;


{ Search for SetWindowLong and GetWindowLong. Use SetWindowLongPtr instead. }
procedure TDutWin64.FindSetWindowLong;
var
   TextBody: TStringList;
   sLine: string;
   iLine: Integer;
begin
  TextBody:= StringFromFileTSL(SearchResults.Last.FileName);
  try
    for iLine:= 0 to TextBody.Count-1 do
      begin
        sLine:= TextBody[iLine];
        var iColumn:= RelaxedSearchI(sLine, 'etWindowLong(');
        if iColumn > 0
        then SearchResults.Last.AddNewPos(iLine, iColumn, sLine, 'GetWindowLong is for Win32 only!', 'Use GetWindowLongPtr instead.');
      end;
  finally
    FreeAndNil(TextBody);
  end;
end;


end.
