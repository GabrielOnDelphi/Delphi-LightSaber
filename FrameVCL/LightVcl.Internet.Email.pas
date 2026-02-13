UNIT LightVcl.Internet.Email;

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt

   Functions for working with email addresses (email validation, etc)

   Resurse utile:
      Send email:                  http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_22095910.html?sfQueryTermInfo=1+address+email#a18190296
      Find Current Email Program:  http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_10106205.html?sfQueryTermInfo=1+address+email


   See this page:
        http://en.wikipedia.org/wiki/Country_code_top-level_domain
        http://en.wikipedia.org/wiki/Generic_top-level_domain
        http://en.wikipedia.org/wiki/Unsponsored_top-level_domain
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
   Winapi.Windows, Winapi.MAPI, Winapi.ShellAPI{ Required by OpenDefaultEmail },
   System.SysUtils, System.StrUtils, System.Classes, System.Win.Registry,
   Vcl.Forms,
   LightCore, LightCore.Types, LightVcl.Common.Dialogs;

CONST
  SeparatorsEmail= [' ', '~', '`', '!', '#', '$', '%', '^', '&', '*', '(', ')', '+', '=', '[', ']', '{', '}', ';', ':', '''', '"', '<', '>', ',', '/', '?', '\', '|', #10, #13, #9];


 { SYSTEM }
 function  GetDefaultEmailAddress: String;                                                                  { Works only if the user uses Outlook as email client }
 function  OpenDefaultEmail(CONST Recipient, Subject, Mesaj: String): Cardinal;                             { This will open the default email program in 'Compose' mode }
 function  OpenDefaultEmailEx(CONST Subject, Body, FileName, SenderName, SenderEMail, RecipientName, RecipientEMail: AnsiString): Integer;

 { EXTRACTION }
 function  ExtractEmailEngine   (CONST SmallText : string; StartPos: integer; var EndPos: integer): string; { extract the first email address encountered from a string  }
 function  ExtractFirstEmailAdr (CONST SmallText : string): string;                                         { extract the first email address encountered from a string  }
 function  ExtractAllEmailAdr   (HugeString: string; AdreseExtrase: TStringlist): Integer;
 function  ExtractEmailFromThunderbirdFile(CONST HugeText: string; CONST ToField, FromField, CcField, BccField: Boolean; OutputList: TStringList): Integer;

 { VALIDATION }
 function  ValidateEmailAddress (      Email: String; OUT FailCode, FailPosition: Integer) : Boolean; overload;   { Returns nothing if the email is valid else return the reason. SuggestCorrection=TRUE, the program will try to suggest a corrected version of this address }
 function  ValidateEmailAddress (CONST Email : string; SuggestCorrection: boolean): string;             overload;   { Returns nothing if the email is valid else return the reason. SuggestCorrection=TRUE, the program will try to suggest a corrected version of this address }
 function  ValidateEmailAddress (CONST Email : string): Boolean;                                        overload;
 function  CorrectEmailAddress  (      Email : String; OUT Suggestion: String; MaxCorrections : Integer = 5) : Boolean;
 function  EmailHasManyNumbers  (CONST Email : string; CONST Ratio: integer): Boolean;
 function  FailCode2Str         (Code  : Integer) : string;                                                { convert the integer fail codes to understandeble message strings }
 function  CheckIfThunderbirdFile(CONST HugeText: String): Boolean;

 { UTILS }
 procedure SplitEmailAddress    (CONST EmailAddress: string; out user, domain: string);                    { Returns the 'user' and 'domain' part of an email address - the @ charecter is not included }
 function  EmailSortByDomain    (CONST InptList: TStrings): TStringList;                     { SORT }
 procedure SendEmail            (CONST sTo, sSubject, sBody: string);





IMPLEMENTATION

Uses
   LightCore.Math, LightVcl.Common.ExecuteShell;




{--------------------------------------------------------------------------------------------------
                                     EMAIL
--------------------------------------------------------------------------------------------------}
procedure SendEmail(CONST sTo, sSubject, sBody: string);
VAR s: string;
begin
  s := 'mailto:'+sTo+'?subject=' + sSubject + '&body=' + sBody;
  ExecuteURL(s);
end;



{ Sorts a list of email addresses alphabetically by domain name.
  Uses selection sort algorithm - finds the smallest domain and moves it to result.
  Note: Caller is responsible for freeing the returned TStringList. }
function EmailSortByDomain(CONST InptList: TStrings): TStringList;
VAR TmpList: TStringList;
    CurAddr, xPozition: integer;
    CurDomain, CurUser, xDomain, xUserName: string;
begin
 Assert(InptList <> NIL, 'InptList parameter cannot be nil');

 Result   := TStringList.Create;
 TmpList:= TStringList.Create;
 TRY
  TmpList.Assign(InptList);

  WHILE TmpList.Count > 1 DO
   begin
    xDomain:= 'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ';
    xPozition:= TmpList.Count-1;

    for CurAddr:= TmpList.Count-1 downto 0 DO                                                     { pick an element from the end of the TMP list and compare it with items in the Output list }
     begin
      { Get current addreess }
      SplitEmailAddress(TmpList.Strings[CurAddr], CurUser, CurDomain);

      if CompareText(CurDomain, xDomain) < 0 then                                                     { Compares two strings by ordinal value without case sensitivity. CompareText compares S1 and S2 and returns 0 if they are equal. If S1 is greater than S2, CompareText returns an integer greater than 0. If S1 is less than S2, CompareText returns an integer less than 0. CompareText is not case sensitive and is not affected by the current Locate }
       begin
        xDomain  := CurDomain;
        xUserName:= CurUser;
        xPozition:= CurAddr;                                                                        { adresa la care a fost gasit cel mai scurt sir }
       end;
     end;

    Result.Add(xUserName+'@'+xDomain);                                                             { recunstruct the email address and add it to NewTSL }
    TmpList.Delete(xPozition);
   end;

  if TmpList.Count > 0
  then Result.Add(TmpList[0]);                                                                     { add also last line, remained unprocessed  }
 FINALLY
  FreeAndNil(TmpList);
 END;
end;




procedure SplitEmailAddress(CONST EmailAddress: string; OUT user, domain: string);                 { Returns the 'user' and 'domain' part of an email address - the @ charecter is not included }
VAR Poz: Integer;
begin
 Poz:= Pos('@', EmailAddress);
 user  := CopyTo (EmailAddress, 1, Poz-1);
 domain:= system.COPY (EmailAddress, Poz+1, High(Integer));
end;


function OpenDefaultEmail(CONST recipient, subject, mesaj: String): Cardinal;
VAR MailBody : String;
begin
 MailBody:= 'mailto:'+ recipient+ '?subject='+ subject+ '&body='+ mesaj;
 Result:= Winapi.ShellAPI.ShellExecute(vcl.Forms.Application.Handle, 'open', PChar(MailBody), NIL, NIL, SW_Normal);
(*
 Sleep(500);                                                                                       {give mail prog time to open}
 keybd_Event(VK_MENU, 0, 0, 0);                                                                    {get email prog menu}
 keybd_Event(ord('S'), 0, 0, 0);                                                                   {s for send}
 keybd_Event(ord('S'), 0, KEYEVENTF_KEYUP, 0);                                                     {click}
 keybd_Event(VK_MENU, 0, KEYEVENTF_KEYUP, 0);                                                      {exit menu}
*)
end;


function OpenDefaultEmailEx(CONST Subject, Body, FileName, SenderName, SenderEMail, RecipientName, RecipientEMail: AnsiString): Integer;
{
  This allows you to also add attachments
  You must add the MAPI unit in USES-clause
  Use it like this: SendMail('Re: mailing from Delphi', 'Welcome to www.test.com'#13#10'Dany', 'c:\autoexec.bat', 'your name', 'your@address.com', 'Dany', 'test@test.com')
}
VAR
  Message: TMapiMessage;
  lpSender, lpRecipient: TMapiRecipDesc;
  FileAttach: TMapiFileDesc;

  SM: TFNMapiSendMail;
  MAPIModule: HModule;
begin
  FillChar(Message, SizeOf(Message), 0);

  if (Subject <> '')
  then Message.lpszSubject := PAnsiChar(Subject);

  if (Body <> '')
  then Message.lpszNoteText := PAnsiChar(Body);

  if (SenderEmail <> '') then
   begin
    lpSender.ulRecipClass := MAPI_ORIG;
    if (SenderName = '')
    then lpSender.lpszName := PAnsiChar(SenderEMail)
    else lpSender.lpszName := PAnsiChar(SenderName);
    lpSender.lpszAddress := PAnsiChar(SenderEmail);
    lpSender.ulReserved := 0;
    lpSender.ulEIDSize := 0;
    lpSender.lpEntryID := nil;
    Message.lpOriginator := @lpSender;
   end;

  if (RecipientEmail <> '') then
   begin
    lpRecipient.ulRecipClass := MAPI_TO;
    if (RecipientName = '')
    then lpRecipient.lpszName := PAnsiChar(RecipientEMail)
    else lpRecipient.lpszName := PAnsiChar(RecipientName);

    lpRecipient.lpszAddress := PAnsiChar(RecipientEmail);
    lpRecipient.ulReserved := 0;
    lpRecipient.ulEIDSize := 0;
    lpRecipient.lpEntryID := nil;
    Message.nRecipCount := 1;
    Message.lpRecips := @lpRecipient;
   end
  else
    Message.lpRecips := nil;

  if (FileName = '') then
   begin
    Message.nFileCount := 0;
    Message.lpFiles := nil;
   end
  else
   begin
    FillChar(FileAttach, SizeOf(FileAttach), 0);
    FileAttach.nPosition := Cardinal($FFFFFFFF);
    FileAttach.lpszPathName := PAnsiChar(FileName);

    Message.nFileCount := 1;
    Message.lpFiles := @FileAttach;
   end;

  MAPIModule := LoadLibrary(PChar(MAPIDLL));
  if MAPIModule = 0
  then Result := -1
  else
    TRY
      @SM := GetProcAddress(MAPIModule, 'MAPISendMail');
      if @SM <> NIL
      then Result := SM(0, Application.Handle, Message, MAPI_DIALOG or MAPI_LOGON_UI, 0)
      else Result := 1;
    FINALLY
      FreeLibrary(MAPIModule);
    END;

  if Result <> 0
  then MessageError('Error sending mail (' + IntToStr(Result) + ').');
end;













{--------------------------------------------------------------------------------------------------
                               EMAIL - CORRECT ADDRESS
--------------------------------------------------------------------------------

 The CorrectEmailAddress function:
 The function takes three parameters:
 Email – The e-mail address to check and correct
 Suggestion – This string passed by reference contains the functions result
 MaxCorrections – The maximum amount of corrections to attempt before stopping (defaults to 5)

 This function simply loops up to MaxCorrection times, validating the e-mail address then using the FailCode to decide what kind of correction to make, and repeating this until it find a match, determines the address can’t be fixed, or has looped more than MaxCorrection times.
 The following corrections are performed, based on the FailCode (see description above):
   flUnknown                – Simply stops corrections, as there is no generic way to correct this problem.
   flNoSeperator            – When this error is encountered the system performs a simple but powerful function, it will navigate the e-mail address until it finds the last 2, and then convert it to an @ symbol. This will correct most genuine transposition errors. If it converts a 2 that was not really an @ chances are it has completely invalidated the e-mail address.
   flToSmall                - Simply stops corrections, as there is no generic way to correct this problem.
   flUserNameToLong         – Simply stops corrections, as there is no generic way to correct this problem.
   flDomainNameToLong       – Simply stops corrections, as there is no generic way to correct this problem.
   flInvalidChar            – In this case the offending character is simply deleted.
   flMissingUser            – Simply stops corrections, as there is no generic way to correct this problem.
   flMissingDomain          – Simply stops corrections, as there is no generic way to correct this problem.
   flMissingDomainSeperator – Simply stops corrections, as there is no generic way to correct this problem.
   flMissingGeneralDomain   – Simply stops corrections, as there is no generic way to correct this problem.
   flToManyAtSymbols        – Simply stops corrections, as there is no generic way to correct this problem.

 While only a small portion of errors can be corrected the function can correct the most common errors encountered when working with list of e-mail addresses, specifically when the data is entered by the actual e-mail address account holder.
 -David Lederman InterentToolsCorp.com }

CONST
  flUnknown               = 0;     // These constants represent the various errors validation errors (known) that can occur.
  flNoSeperator           = 1;
  flToSmall               = 2;
  flUserNameToLong        = 3;
  flDomainNameToLong      = 4;
  flInvalidChar           = 5;
  flMissingUser           = 6;
  flMissingDomain         = 7;
  flMissingDomainSeperator= 8;
  flMissingGeneralDomain  = 9;
  flToManyAtSymbols       = 10;

function CorrectEmailAddress (Email : String; OUT Suggestion: String; MaxCorrections : Integer = 5) : Boolean;  { MaxCorrections – The maximum amount of corrections to attempt before stopping (defaults to 5) }
VAR
   RevPos, CorrectionAttempt, FailCode, FailPosition, LastAt : Integer;
begin
 Email:= Trim(Email);
 Result := False;
 try
  Suggestion := Email;                                                          // Reset the suggestion
  // Loop up to MaxCorrections attempts to correct the email
  for CorrectionAttempt := 1 to MaxCorrections do
   begin
     if ValidateEmailAddress(Suggestion, FailCode, FailPosition)            // Now try to validate the address
     then Exit(true);

     // Otherwise, try to correct it
     case FailCode of
       flUnknown: exit;
       flNoSeperator:
        begin
          LastAt := 0;                                                           // This error can possibly be fixed by finding the last 2 (which was most likely transposed for an @)
          for RevPos := 1 to Length(Suggestion) DO
            if Suggestion[RevPos] = '2'
            then LastAt := RevPos;                                               // Look for the 2
          if LastAt = 0 then exit;                                              // Now see if we found an 2
          Suggestion[LastAt] := '@';                                             // Now convert the 2 to an @ and continue
        end;
       flToSmall: Exit;
       flUserNameToLong: exit;                                                  // The situation can't get better so exit
       flDomainNameToLong: Exit;
       flInvalidChar: Delete(Suggestion, FailPosition, 1);                      // Simply delete the offending char
       flMissingUser:  Exit;
       flMissingDomain: Exit;
       flMissingDomainSeperator: Insert('.', Suggestion, Length(Suggestion)- 2); // The best correction we can make here is to go back three spaces and insert a dot. Instead of checking the length of the string, we'll let an exception shoot since at this point we can't make things any better (suggestion wise)
       flMissingGeneralDomain: Exit;
       flToManyAtSymbols: Exit;
     end;                                                                       // case
   end;                                                                         // for
  //Result := False; // If we got here fail
 except
   on E: ERangeError do
     Result:= False;  { String index out of bounds during correction attempt }
   on E: EStringListError do
     Result:= False;  { String manipulation error during correction }
 end;
end;



{--------------------------------------------------------------------------------------------------
                               EMAIL - VALIDATE
--------------------------------------------------------------------------------------------------}
CONST                                                                           // This is a list of error descriptions, it's kept in the implementation section as it's not needed directlly from outside this unit, and can be accessed using the ValidationErrorString which does range checking.
  ErrorDescriptions : array[0..10] of String =
  ('Unknown error occured!', 'Missing @ symbol!',  'Data to small!',
   'User name to long!', 'Domain name to long!',   'Invalid character!',
   'Missing user name!', 'Missing domain name!',   'Missing domain portion (.com,.net,etc)',
   'Invalid general domain!',   'To many @ symbols!');
  AllowedEmailChars  =  ['A'..'Z','a'..'z','0','1','2','3','4','5','6','7','8','9','@','-','.','_', '''', '+', '$', '/', '%', 'ä', 'ö', 'ü', 'ß'];
  MaxUsernamePortion = 64;                                                      // Per RFC 821
  MaxDomainPortion   = 256;                                                     // Per RFC 821


function FailCode2Str(Code : Integer) : String;                                 // This function returns the error string from the constant array, and makes sure that the error code is valid, if not it returns an invalid error code string.
begin
 Result:= '';
 if (Code < Low(ErrorDescriptions))
 OR (Code > High(ErrorDescriptions)) then                                       // Make sure a valid error code is passed
  begin
   Result := 'Invalid error code!';
   exit;
  end;
 Result:= ErrorDescriptions[Code];                                              // Get the error description from the constant array
end;



function ValidateEmailAddress (CONST Email: string; SuggestCorrection: Boolean): string;           { Returns nothing if the email is valid else returns the reason. SuggestCorrection=TRUE, the program will try to suggest a corrected version of this address }
VAR FailCode, FailPos: Integer;
    CorrectedEmail, Suggestion: string;
begin
 Result:= '';
 if NOT ValidateEmailAddress(Email, FailCode, FailPos)
 then
  begin
   if SuggestCorrection AND CorrectEmailAddress(Email, CorrectedEmail, 5)
   then Suggestion:= CRLF+ 'Suggested corrections: '+ CorrectedEmail;

   Result:= Email+ '  is an invalid email address! ' + FailCode2Str(FailCode) + ' at position '+ IntToStr(FailPos) + ' '+ Suggestion;
  end
end;


function ValidateEmailAddress (CONST Email: string): Boolean;
VAR
   FailCode, FailPos: Integer;
begin
 Result:= ValidateEmailAddress(Email, FailCode, FailPos);
end;



function ValidateEmailAddress(Email: String; OUT FailCode, FailPosition: Integer): Boolean;     {  This function will validate an address, much further than simply verifying the syntax as the RFC (821) requires }
VAR
  DataLen, SepPos, Itt, DomainStrLen, UserStrLen, LastSep, SepCount, PrevSep : Integer;
  UserStr, DomainStr, SubDomain : String;
begin
 email:= Trim(email);
 TRY
   DataLen := Length(Email);                                                                       // Get the data length
   if DataLen = 0 then                                                                             // Make sure that the string is not blank
   begin
     FailCode := flToSmall;                                                                        // Set the result and exit
     Result := False;
     Exit;
   end;
   SepPos := Pos('@', Email);                                                                      // First real validation, ensure the @ seperator
   if SepPos = 0 then
   begin
     FailCode := flNoSeperator;                                                                    // Set the result and exit
     Result := False;
     Exit;
   end;

   // Now verify that only the allowed characters are in the system
   for Itt := 1 to DataLen do                                                                      // Iterate
   begin
     if not CharInSet(Email[Itt], AllowedEmailChars) then                                          // Make sure the character is allowed
     begin                                                                                         // Report an invalid char error and the location
       FailCode := flInvalidChar;
       FailPosition := Itt;
       result := False;
       exit;
     end;
   end;                                                                                            // for

   // Now split the string into the two elements: user and domain
   UserStr  := system.COPY(Email, 1, SepPos -1);
   DomainStr:= system.COPY(Email, SepPos + 1, DataLen);

   // If either the user or domain is missing then there's an error
   if (UserStr = '') then
   begin                                                                                           // Report a missing section and exit
     FailCode := flMissingUser;
     Result := False;
     exit;
   end;
   if (DomainStr = '') then
   begin
     FailCode := flMissingDomain;                                               // Report a missing section and exit
     Result := False;
     exit;
   end;

   DomainStrLen := Length(DomainStr);  // Now get the lengths of the two portions
   UserStrLen := Length(UserStr);

   // Ensure that either one of the sides is not to large (per the standard)
   if DomainStrLen > MaxDomainPortion then
   begin
     FailCode := flDomainNameToLong;
     Result := False;
     exit;
   end;
   if UserStrLen > MaxUserNamePortion then
   begin
     FailCode := flUserNameToLong;
     Result := False;
     exit;
   end;

   // Now verify the user portion of the email address
   // Ensure that the period is neither the first or last char (or the only char)
   // Check first char
   if (UserStr[1] = '.') then
   begin
     // Report a missing section and exit
     FailCode := flInvalidChar;
     Result := False;
     FailPosition := 1;
     exit;
   end;

   // Check end char
   if (UserStr[UserStrLen] = '.') then
   begin
     FailCode := flInvalidChar;      // Report a missing section and exit
     Result := False;
     FailPosition := UserStrLen;
     EXIT;
   end;

   // No direct checking for a single char is needed since the previous two
   // checks would have detected it.
   // Ensure no subsequent periods
   for Itt := 1 to UserStrLen do    // Iterate
   begin
     if UserStr[Itt] = '.' then
     begin
       // Check the next char, to make sure it's not a .
       if UserStr[Itt + 1] = '.' then
       begin
         // Report the error
         FailCode := flInvalidChar;
         Result := False;
         FailPosition := Itt;
         exit;
       end;
     end;
   end;    // for

   { At this point, we've validated the user name, and will now move into the domain.}
   // Ensure that the period is neither the first or last char (or the only char)
   // Check first char
   if (DomainStr[1] = '.') then
   begin
     // Report a missing section and exit
     FailCode := flInvalidChar;
     Result := False;
     // The position here needs to have the user name portion added to it
     // to get the right number, + 1 for the now missing @
     FailPosition := UserStrLen + 2;
     exit;
   end;

   // Check end char
   if (DomainStr[DomainStrLen] = '.') then
   begin    // Report a missing section and exit
     FailCode := flInvalidChar;
     Result := False;
     // The position here needs to have the user name portion added to it
     // to get the right number, + 1 for the now missing @
     FailPosition := UserStrLen + 1 + DomainStrLen;
     exit;
   end;

   { No direct checking for a single char is needed since the previous two checks would have detected it. Ensure no subsequent periods, and while in the loop count the periods, and record the last one, and while checking items, verify that the domain and subdomains to dont start or end with a - }
   SepCount := 0;
   LastSep := 0;
   PrevSep := 1; // Start of string
   for Itt := 1 to DomainStrLen do    // Iterate
   begin
     if DomainStr[Itt] = '.' then
     begin
       // Check the next char, to make sure it's not a .
       if DomainStr[Itt + 1] = '.' then
       begin     // Report the error
         FailCode := flInvalidChar;
         Result := False;
         FailPosition := UserStrLen + 1 + Itt;
         exit;
       end;
       // Up the count, record the last sep
       Inc(SepCount);
       LastSep := Itt;
       // Now verify this domain
       SubDomain := system.COPY(DomainStr, PrevSep, (LastSep) - PrevSep);
       // Make sure it doens't start with a -
       if SubDomain[1] = '-' then
       begin
         FailCode := flInvalidChar;
         Result := False;
         FailPosition := UserStrLen + 1 + (PrevSep);
         exit;
       end;
       // Make sure it doens't end with a -
       if SubDomain[Length(SubDomain)] = '-' then
       begin
         FailCode := flInvalidChar;
         Result := False;
         FailPosition := (UserStrLen + 1) +  LastSep - 1;
         exit;
       end;
       PrevSep := LastSep + 1;  // Update the pointer
     end
     else
     begin
       if DomainStr[Itt] = '@' then
       begin   // Report an error
         FailPosition := UserStrLen + 1 + Itt;
         FailCode := flToManyAtSymbols;
         result := False;
         exit;
       end;
     end;
   end;    // for

   // Verify that there is at least one .
   if SepCount < 1 then
   begin
     FailCode := flMissingDomainSeperator;
     Result := False;
     exit;
   end;

   // Now do some extended work on the final domain the most general (.com)
   // Verify that the lowest level is at least 2 chars
   SubDomain := system.COPY(DomainStr, LastSep, DomainStrLen);
   if Length(SubDomain) < 2 then
   begin
     FailCode := flMissingGeneralDomain;
     Result := False;
     exit;
   end;

   // Well after all that checking, we should now have a valid address
   Result := True;
 except
   on E: ERangeError do
    begin
     Result:= False;
     FailCode:= flUnknown;  { String index out of bounds during validation }
    end;
 END;
end;






{===============================================================================
                                 EXTRACT  EMAIL
===============================================================================}

{ Core engine for extracting email addresses from text.
  Algorithm:
    1. Find the @ symbol starting from StartPos
    2. Scan backwards from @ to find the start of the email (stop at separators)
    3. Scan forwards from @ to find the end of the email (stop at separators)
    4. Extract and return the substring

  Parameters:
    SmallText - The text to search for email addresses
    StartPos  - Position to start searching from (1-based)
    EndPos    - Returns the position where the email ends (for chained calls)

  Returns: The extracted email address, or empty string if not found }
function ExtractEmailEngine (CONST SmallText : string; StartPos: integer; var EndPos: integer): string;
var
  AtPos: integer;
  i, len: integer;
begin
 Result:= '';
 AtPos:= PosEx('@', SmallText, StartPos);
 len:= length(SmallText);
 if (AtPos< 2) OR (len< 3) then Exit;                                          { Need at least x@y format }
 EndPos:= len;

 { Scan backwards from @ to find email start }
 for i:= AtPos downto StartPos do
   if CharInSet(SmallText[i], SeparatorsEmail) then
    begin
     StartPos:= i+1;
     break;
    end;

 { Scan forwards from @ to find email end }
 for i:= AtPos to len DO
   if CharInSet(SmallText[i], SeparatorsEmail) then
    begin
     EndPos:= i-1;
     break;
    end;

 Result:= LightCore.CopyTo(SmallText, StartPos, EndPos);
end;


function ExtractFirstEmailAdr (CONST SmallText : string): string;                                  { extract the first email address encountered from a string  }
Var EndPos: integer;
begin
 EndPos:= length(SmallText);
 Result:= ExtractEmailEngine (SmallText, 1, EndPos);
end;


function ExtractAllEmailAdr (HugeString: string; AdreseExtrase: TStringlist): integer;             {Doesn't clear the log}
Var StartPos, EndPos: integer; s: string;
begin
 Assert(AdreseExtrase <> NIL, 'AdreseExtrase parameter cannot be nil');

 { Preprocessing. Replace the "<A title=3D" and "Email=3D" string with space. This is related to a quite often apparition of this string in Thunderbird's email files. I don't know why. }
{$IFDEF UNICODE}
 HugeString:= ReplaceText(HugeString, '<A title=3D', ' ');
 HugeString:= ReplaceText(HugeString, 'Email=3D', ' ');
{$ELSE}
 HugeString:= ReplaceString(HugeString, '<A title=3D', ' ');
 HugeString:= ReplaceString(HugeString, 'Email=3D', ' ');
{$ENDIF}

 Result:= 0;
 StartPos:=  1; { conteaza }
 while (StartPos< length(HugeString)) DO                                                           { (StartPos< EndPos+1) OR }
  BEGIN
   s:= ExtractEmailEngine (HugeString, StartPos, EndPos);
   if s<> '' then
    begin
     AdreseExtrase.Add( s );
     StartPos:= EndPos+1;
     inc(result);
    end
   else Break;
  END;
end;


function ExtractEmailFromThunderbirdFile(CONST HugeText: string; CONST ToField, FromField, CcField, BccField: Boolean; OutputList: TStringList): Integer;  {Returns the total number of vaild addresses}
VAR I: Integer;
TextLine: string;
InputList: TStringList;
const
 ctMailTo    = 'To:';
 ctReplyFrom = 'From:';
 ctCc        = 'Cc:';
 ctBcc       = 'BCC:';
begin
 Assert(OutputList <> NIL, 'OutputList parameter cannot be nil');

 InputList:= TStringList.Create;
 TRY
  InputList.Text:= HugeText;
  Result:= 0;

  if CheckIfThunderbirdFile(HugeText) then                                                         { Verify if the file is a Thunderbird file }
   for I := 0 to InputList.count-1 do
    begin
     TextLine:= InputList[i];

     if ToField
     AND  (Pos(ctMailTo, Textline)= 1)                                                             {Verify if "To:" is on this line}
     then Result:= Result + ExtractAllEmailAdr(TextLine, OutputList)

     else
      if FromField
      AND (Pos(ctReplyFrom, TextLine)= 1)
      then Result:= Result+ ExtractAllEmailAdr(TextLine, OutputList)                               {Verify if "From:" is on this line}

      else
       if CcField
       AND (Pos(ctCc, TextLine)= 1)
       then Result:= Result + ExtractAllEmailAdr(TextLine, OutputList)                             {Verify if "Cc:" is on this line}

       else
        if BccField
        AND (Pos(ctBcc, TextLine)= 1)
        then Result:= Result + ExtractAllEmailAdr(TextLine, OutputList)                            {Verify if "BCC:" is on this line}
    end;
 FINALLY
  FreeAndNil(InputList);
 END;
end;


function CheckIfThunderbirdFile(CONST HugeText: String): Boolean;
CONST ThunderbirdFile= 'X-Mozilla-Status';                                                         { Search string to know if is a Thunderbird file. Which is found on the 4th line of each mail }
begin
 Result:= Pos(ThunderbirdFile, HugeText)> 1;
end;


{ Remove emails that contains too many numbers
  Ration parameter is the propostion of invalid characters (numbers) I need to consider the email invalid. For example 30 means that if more than 30% of the chars are numbers, then the email is declared invalid.

  Example:

           002FA4A5F3A5434F9B47467FA9008C261C99B4@PHSXMB32.partners.org         0K0U00F7XNYGY380@mxout11.netvision.net.il

           10598.77.8.136.123.1207438486.squirrel@www.CeBiTec.Uni-Bielefeld.DE  0K7D00D7TR7ARN20@out1.ub.edu

           15134833.6352441215831846235.JavaMail.weblogic@mail.kongju.ac.kr     20080319101051.BCY26812@m4500-01.uchicago.edu

           E1K6Tbd-0008Pp-6k@biz31.inmotionhosting.com   }

function EmailHasManyNumbers (CONST Email: string; CONST Ratio: integer): Boolean;
VAR at, i, Total: integer;
    user: string;
begin
 Result:= FALSE;
 Total:= 0;

 { Extract user name }
 at:= Pos('@', Email);
 if at< 8 then EXIT;                                                                               { ingnor short usernames }
 user:= system.COPY(Email, 1, at-1);

 for i:= 1 to Length(user) DO
 if CharInSet(user[i], Numbers) then
  begin
   inc(Total);
  end;

 Result:= ProcentRepresent(Total, Length(user)) > Ratio;
end;


{ Returns the default email address from Windows registry.
  Works only if the user uses Outlook as email client.
  Returns empty string if the registry key doesn't exist or cannot be read. }
function GetDefaultEmailAddress: String;
VAR Registry: TRegistry;
begin
 Result:= '';
 Registry:= TRegistry.Create;
 TRY
   Registry.RootKey:= HKEY_CURRENT_USER;
   if Registry.OpenKeyReadOnly('Software\Microsoft\Internet Account Manager\Accounts\00000001')
   then Result:= Registry.ReadString('SMTP Email Address');
 FINALLY
   FreeAndNil(Registry);
 END;
end;














end.
