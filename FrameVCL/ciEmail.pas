UNIT ciEmail;

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2023.06
   www.GabrielMoraru.com
   See Copyright file

   Functions for working with email addresses (email validation, etc)

   Resurse utile:
      Send email:                  http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_22095910.html?sfQueryTermInfo=1+address+email#a18190296
      Find Current Email Program:  http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_10106205.html?sfQueryTermInfo=1+address+email


   See this page:
        http://en.wikipedia.org/wiki/Country_code_top-level_domain
        http://en.wikipedia.org/wiki/Generic_top-level_domain
        http://en.wikipedia.org/wiki/Unsponsored_top-level_domain
-------------------------------------------------------------------------------------------------------------}

INTERFACE                                                                                                     {$WARN GARBAGE OFF}   {Silence the: 'W1011 Text after final END' warning }

USES
   Winapi.Windows, Winapi.MAPI, Winapi.ShellAPI{ Required by OpenDefaultEmail },
   System.SysUtils, System.StrUtils, System.Classes, System.Win.Registry,
   Vcl.Forms,
   ccCore, cbDialogs;

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
   ccMath, csExecuteShell;




{--------------------------------------------------------------------------------------------------
                                     EMAIL
--------------------------------------------------------------------------------------------------}
procedure SendEmail(CONST sTo, sSubject, sBody: string);
VAR s: string;
begin
  s := 'mailto:'+sTo+'?subject=' + sSubject + '&body=' + sBody;
  ExecuteShell(s);
end;



function EmailSortByDomain(CONST InptList: TStrings): TStringList;                   { SORT }
VAR TmpList: TStringList;
    CurAddr, xPozition: integer;
    CurDomain, CurUser, xDomain, xUserName: string;
begin
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

  Result.Add(TmpList[0]);                                                                        { add also last line, remained unprocessed  }
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
  with Message DO
   begin
     if (Subject <> '') then
       lpszSubject := PAnsiChar(Subject);

     if (Body <> '') then
       lpszNoteText := PAnsiChar(Body);

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
       lpOriginator := @lpSender;
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
       nRecipCount := 1;
       lpRecips := @lpRecipient;
      end
     else
       lpRecips := nil;

     if (FileName = '') then
      begin
       nFileCount := 0;
       lpFiles := nil;
      end
     else
      begin
       FillChar(FileAttach, SizeOf(FileAttach), 0);
       FileAttach.nPosition := Cardinal($FFFFFFFF);
       FileAttach.lpszPathName := PAnsiChar(FileName);

       nFileCount := 1;
       lpFiles := @FileAttach;
      end;
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
   Itteration, RevITT, i, FailCode, FailPosition, LastAt : Integer;
begin
 Email:= Trim(Email);
 Result := False;
 try
  Suggestion := Email;                                                          // Reset the suggestion
  Itteration := 1;
  // Now loop through to the max depth
  for i := Itteration to MaxCorrections do                                        // Iterate
   begin
     if ValidateEmailAddress(Suggestion, FailCode, FailPosition)            // Now try to validate the address
     then Exit(true);

     // Otherwise, try to correct it
     case FailCode of
       flUnknown: exit;
       flNoSeperator:
        begin
          LastAt := 0;                                                           // This error can possibly be fixed by finding the last 2 (which was most likely transposed for an @)
          for RevITT := 1 to Length(Suggestion) DO
            if Suggestion[RevITT] = '2'
            then LastAt := RevITT;                                               // Look for the 2
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
   //todo 1: trap only specific exceptions
   Result := false;                                                        // Just return false
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


(*del
function FailCode2Str (FailCode: Integer): string;
begin
 Result:= '';
 case FailCode of
    flUnknown: Result                := 'Unknown error';
    flNoSeperator: Result            := '@ not found';
    flToSmall: Result                := 'The email address is too short';
    flUserNameToLong: Result         := 'The email address is too long';
    flDomainNameToLong: Result       := 'The domain name is too long';
    flInvalidChar: Result            := 'Invalid character detected';
    flMissingUser: Result            := 'Missing user';
    flMissingDomain: Result          := 'Missing domain';
    flMissingDomainSeperator: Result := 'Missing domain separator (dot)';
    flMissingGeneralDomain: Result   := 'Domain root is too short';
    flToManyAtSymbols: Result        := 'To many symbols';
 end;
end;  *)



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
   //todo 1: trap only specific exceptions
   Result := False;
   FailCode := -1;
 END;
end;






{===============================================================================
                                 EXTRACT  EMAIL
===============================================================================}

function ExtractEmailEngine (CONST SmallText : string; StartPos: integer; var EndPos: integer): string;                                { extract the first email address encountered from a string  }
var
  AtPos: integer;
  i, len: integer;
begin
 Result:= '';                                                                   // SmallText:= CopyTo(SmallText, StartPos, EndPos);
 AtPos:= PosEx('@', SmallText, StartPos);
 len:= length(SmallText);
 if (AtPos< 2) OR (len< 3) then Exit;
 EndPos:= len;

 for i:= AtPos downto StartPos do
   if CharInSet(SmallText[i], SeparatorsEmail) then
    begin
     StartPos:= i+1;
     break;
    end;
 for i:= AtPos to len DO
   if CharInSet(SmallText[i], SeparatorsEmail) then
    begin
     EndPos:= i-1;
     break;
    end;

 Result:= ccCore.CopyTo(SmallText, StartPos, EndPos);
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
 ThunderbirdFile = 'X-Mozilla-Status';
begin
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


function GetDefaultEmailAddress: String;                                                           { Works only if the user uses Outlook as email client }
VAR Registry: TRegistry;
begin
 Registry := TRegistry.Create;
 TRY
  TRY
    Registry.RootKey := HKEY_CURRENT_USER;                                                         { False because we do not want to create it if it doesn’t exist}
    Registry.OpenKey('Software\Microsoft\Internet Account Manager\Accounts\00000001', FALSE);
    Result := registry.ReadString('SMTP Email Address');
   FINALLY
    FreeAndNil(Registry);
   END;
 except
   //todo 1: trap only specific exceptions
   Result:= '';
 END;
end;













 end.(* ============================================================================



    Question/Problem/Abstract:
    Have you ever needed to verify that an e-mail address is correct, or have you had to work with a list of e-mail addresses and realized that some had simple problems that you could easily correct by hand?
    Answer:

    Article Updated 9/20/2000
    Please note: I have made a correction to the e-mail validation function to correct a bug with handling small e-mail addresses like a@a.com (not a very common address). If you would like to be notified of any future enhancements or bug fixes, please e-mail me. – Thanks to Simon for catching this bug!
    Thanks, enjoy!!!


    Extended E-mail Address Verification and Correction

    Have you ever needed to verify that an e-mail address is correct, or have you had to work with a list of e-mail addresses and realized that some had simple problems that you could easily correct by hand? Well the functions I present here are designed to do just that. In this article I present two functions, one to check that an e-mail address is valid, and another to try to correct an incorrect e-mail address.

    Just what is a correct e-mail address?
    The majority of articles I’ve seen on e-mail address verification use an over-simplified approach. For example, the most common approach I’ve seen is to ensure that an ‘@’ symbol is present, or that it’s a minimum size (ex. 7 characters), or a combination of both.  And a better, but less used method is to verify that only allowed characters (based on the SMTP standard) are in the address.

    The problem with these approaches is that they only can tell you at the highest level that an address is POSSIBLY correct, for example:

    The address: ------@--------
    Can be considered a valid e-mail address, as it does contain an @, is at least 7 characters long and contains valid characters.

    To ensure an address is truly correct, you must verify that all portions of the e-mail address are valid. The function I present performs the following checks:
    a) Ensure an address is not blank
    b) Ensure an @ is present
    c) Ensure that only valid characters are used
    Then splits the validation to the two individual sections:  username (or mailbox) and domain
    Validation for the username:
    a) Ensure it is not blank
    b) Ensure the username is not longer than the current standard (RFC 821)
    c) Ensures that periods (.) are used properly, specifically there can not be sequential periods (ex. David..Lederman is not valid) nor can there be a period in the first or last character of an e-mail address
    Validation for the domain name:
    a) Ensure it is not blank
    b) Ensure the domain name is not longer than the current standard
    d) Ensure that periods (.) are used properly, specifically there can not be sequential periods (ex. World..net is not valid) nor can there a period in the first or last character of the domain segment
    e) Domain segments need to be checked  (ex. in someplace.somewhere.com, someplace, somewhere, and com are considered segments) to ensure that they do not start or end with a hyphen (-) (ex. somewhere.-someplace.com, is not valid)
    f) Ensure that at least two domain segments exists (ex. someplace.com is valid, .com is not valid)
    g) Ensure that there are no additional @ symbols in the domain portion

    With the steps above most syntactically valid e-mail address that are not correct can be detected and invalidated.

    The VerifyEmailAddress function:
    This function takes 3 parameters:
    Email – The e-mail address to check
    FailCode – The error code reported by the function if it can’t validate an address
    FailPosition – The position of the character (if available) where the validation failure occurred

    The function returns a Boolean value that returns True if the address is valid, and False if it is invalid. If a failure does occur the FailCode can be used to determine the exact error that caused the problem:

      flUnknown – An unknown error occurred, and was trapped by the exception handler.
      flNoSeperator – No @ symbol was found.
      flToSmall – The email address was blank.
      flUserNameToLong – The user name was longer than the SMTP standard allows.
      flDomainNameToLong – The domain name was longer than the SMTP standard allows.
      flInvalidChar – An invalid character was found. (FailPosition returns the location of the character)
      flMissingUser – The username section is not present.
      flMissingDomain – The domain name section is not present
      flMissingDomainSeperator – No domain segments where found
      flMissingGeneralDomain – No top-level domain was found
      flToManyAtSymbols – More than one @ symbol was found

    For simple validation there is no use for FailCode and FailPosition, but can be used to display an error using the ValidationErrorString which takes the FailCode as a parameter and returns a text version of the error which can then be displayed.

    E-mail Address Correction
    Since the e-mail validation routine returns detailed error information an automated system to correct common e-mail address mistakes can be easily created.  The following common mistakes can all be corrected automatically:

    example2.aol.com – The most common error (at least in my experience) is when entering an e-mail address a user doesn’t hold shift properly and instead enters a 2.
    example@.aol.com - This error is just an extra character entered by the user, of course example@aol.com was the intended e-mail address.

    example8080 @ aol .com – In this case another common error, spaces.
    A Cool Screen name@AOL.com – In this case the user entered what they thought was their e-mail address, except_ while AOL allows screen names to contain spaces, the Internet does not.
    myaddress@ispcom - In this case the period was not entered between ISP and Com.




    Comments to this article:



    Chris Bray (Sep 22 2000 6:57AM)
    You asked for suggestions for improvements to your code.... so here goes.
    You have an awful lot of 'Result := False' calls in your code.  I was always taught to define a default result (normally failure) as the first item in a function, and define the opposing result (normally success) as the last item.
    This would mean that your function returns true if the code successfully reaches the end.  If it hits one of your errors it exits without the need to reset the result.
    This would save quite a lot of lines of code, and reduce the possibility of coding errors if you have to add another check at a later date and forget to define the result..........


    Chris Bray.
    The best illustration ;)
        Andrey Sorokin (Sep 20 2000 7:37AM)
    It seems as THE BEST illustration for my article
    E-mail address syntax checking ;)))
    Your code is good and usefull, but just take a look at the problem from another point of view - read article noticed above..
    I call it 'birds eye view' at text strings analyzing..


    Respond
        RE: The best illustration ;)
        David Lederman (Sep 20 2000 10:16AM)
        Agreed, but as mentioned your article only provides basic validation, and does not help prevent data entry stupidity. Plus, we use these functions in an application that delivers millions of messages a day for numerous clients, which get data from disparate sources and must be validated and if possible corrected.
        Respond


    Cool, but ...
        Durin (Sep 20 2000 6:30AM)

    Seems to be the right subject for writing a LLR(1) grammar and a parser based on it. Ever tried (just thought about it reading your article, but - pity - no time doing it myself)? Would reduce code substantially.
    Nevertheless, cool! }







    function ValidateEmailAddress (Email : String; var FailCode, FailPosition : Integer) : Boolean;
    begin
     Result:= UnitEmailVerifier.ValidateEmailAddress(Email, FailCode, FailPosition);
    end;
    function ValidateEmailAndReturnMessage(Email: string): string;                                 { Returns nothing if the email is valid else return the reason }
    begin
     Result:= UnitEmailVerifier.ValidateEmailAndReturnMessage(Email);
    end;













{--------------------------------------------------------------------------------------------------
                              GET IP ADDRESS
--------------------------------------------------------------------------------------------------}
{.$IFNDEF Unicode}
function GetLocalIPAddress : string;                                                               { ALTERNATIVE:      Or else use third-party code that does it for you (Indy 10 loads GetAddrInfo/W(), for instance).}
VAR wsdata : TWSAData;
    HostAddress : PHostEnt;
    ss : pchar;
    ip : TInAddr;
    i  : cardinal;
    co : string;
begin
 Result:= '';
 i := MAX_COMPUTERNAME_LENGTH + 1;
 SetLength(co,i);
 WinApi.Windows.GetComputerName(PChar(co),i);
 WSAStartup(MakeWord(1, 1), wsdata);
 HostAddress := GetHostByName(pchar(co));
 if HostAddress <> NIL then
  begin
   ip.S_addr:= integer(pointer(HostAddress^. h_addr_list^)^);
   ss:= inet_ntoa(ip);
   Result:= string(ss);
  end;
 WSACleanup;
end;


function GetLocalIPAddress2: string;
VAR WSAData: TWSAData;
    HostAddress: PHostEnt;
    HostName, Address: string;
begin
 Result:= '';
 WSAStartup(2, WSAData);
 HostAddress := GetHostByName(PChar(HostName));
 with HostAddress^  DO
 Address:=  Format('%d.%d.%d.%d',[
    Byte(h_addr^[0]), Byte(h_addr^[1]),
    Byte(h_addr^[2]), Byte(h_addr^[3])]);
 WSACleanup;
 Result:= Address;
end;


function GetLocalIPAddress3: string;
TYPE
  TaPInAddr = array [0..10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
VAR
  HostAddress: PHostEnt;
  pptr: PaPInAddr;
  Buffer: array [0..63] of ansi char;
  i: Integer;
  GInitData: TWSADATA;
begin
  WSAStartup($101, GInitData);
  Result := '';
  Winsock.GethostName(Buffer, SizeOf(Buffer));                                                     { eu am functia 'CubicGetHostName' }
  HostAddress := GetHostByName(Buffer);
  if HostAddress = nil then Exit;
  pptr := PaPInAddr(HostAddress^.h_addr_list);
  i := 0;
  while pptr^[i] <> nil do
   begin
    Result:= StrPas(inet_ntoa(pptr^[i]^));
    Inc(i);
   end;
  WSACleanup;
end;



function GetLocalIPAddress4 (VAR HostName, IPaddr, WSAErr: string): Boolean;                       { uses Winsock; }
TYPE
  Name = array[0..100] of ansi Char;
  PName = ^Name;
var
  HEnt: pHostEnt;
  HName: PName;
  WSAData: TWSAData;
  i: Integer;
begin
  Result := False;
  if WSAStartup($0101, WSAData) <> 0 then begin
    WSAErr := 'Winsock is not responding."';
    Exit;
  end;
  IPaddr := '';
  New(HName);
  if GetHostName(HName^, SizeOf(Name)) = 0 then
  begin
    HostName := StrPas(HName^);
    HEnt := GetHostByName(HName^);
    for i := 0 to HEnt^.h_length - 1 do
     IPaddr :=
      Concat(IPaddr,
      IntToStr(Ord(HEnt^.h_addr_list^[i])) + '.');
    SetLength(IPaddr, Length(IPaddr) - 1);
    Result := True;
  end
  else begin
   case WSAGetLastError of
    WSANOTINITIALISED:WSAErr:='WSANotInitialised';
    WSAENETDOWN      :WSAErr:='WSAENetDown';
    WSAEINPROGRESS   :WSAErr:='WSAEInProgress';
   end;
  end;
  Dispose(HName);
  WSACleanup;
end;
{$ENDIF}
