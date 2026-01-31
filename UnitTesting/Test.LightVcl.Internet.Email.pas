unit Test.LightVcl.Internet.Email;

{=============================================================================================================
   2026.01.30
   Unit tests for LightVcl.Internet.Email
   Tests email validation, extraction, correction, and utility functions

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  LightVcl.Internet.Email;

type
  [TestFixture]
  TTestInternetEmail = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { ValidateEmailAddress Tests - Boolean overload }
    [Test]
    [TestCase('Valid_Simple', 'test@example.com,True')]
    [TestCase('Valid_Subdomain', 'user@mail.example.com,True')]
    [TestCase('Valid_Numbers', 'user123@example.com,True')]
    [TestCase('Valid_Dots', 'first.last@example.com,True')]
    [TestCase('Valid_Plus', 'user+tag@example.com,True')]
    [TestCase('Valid_Hyphen', 'user@my-domain.com,True')]
    [TestCase('Valid_Underscore', 'user_name@example.com,True')]
    [TestCase('Invalid_NoAt', 'userexample.com,False')]
    [TestCase('Invalid_Empty', ',False')]
    [TestCase('Invalid_MultipleAt', 'user@@example.com,False')]
    [TestCase('Invalid_AtStart', '@example.com,False')]
    [TestCase('Invalid_AtEnd', 'user@,False')]
    [TestCase('Invalid_DotAtStart', '.user@example.com,False')]
    [TestCase('Invalid_DotAtEnd', 'user.@example.com,False')]
    [TestCase('Invalid_DoubleDot', 'user..name@example.com,False')]
    [TestCase('Invalid_NoDomain', 'user@.com,False')]
    [TestCase('Invalid_NoDomainDot', 'user@example,False')]
    [TestCase('Invalid_Space', 'user name@example.com,False')]
    procedure TestValidateEmailAddress_Boolean(const Email: string; Expected: Boolean);

    { ValidateEmailAddress Tests - With FailCode }
    [Test]
    procedure TestValidateEmailAddress_FailCode_NoSeparator;

    [Test]
    procedure TestValidateEmailAddress_FailCode_TooSmall;

    [Test]
    procedure TestValidateEmailAddress_FailCode_MissingUser;

    [Test]
    procedure TestValidateEmailAddress_FailCode_MissingDomain;

    [Test]
    procedure TestValidateEmailAddress_FailCode_InvalidChar;

    [Test]
    procedure TestValidateEmailAddress_FailCode_TooManyAt;

    { ValidateEmailAddress Tests - String overload with suggestion }
    [Test]
    procedure TestValidateEmailAddress_String_Valid;

    [Test]
    procedure TestValidateEmailAddress_String_Invalid;

    { CorrectEmailAddress Tests }
    [Test]
    procedure TestCorrectEmailAddress_TransposedAt;

    [Test]
    procedure TestCorrectEmailAddress_InvalidChar;

    [Test]
    procedure TestCorrectEmailAddress_ValidEmail;

    [Test]
    procedure TestCorrectEmailAddress_Uncorrectable;

    { SplitEmailAddress Tests }
    [Test]
    [TestCase('Simple', 'user@domain.com,user,domain.com')]
    [TestCase('WithDots', 'first.last@mail.example.com,first.last,mail.example.com')]
    [TestCase('WithPlus', 'user+tag@example.com,user+tag,example.com')]
    procedure TestSplitEmailAddress(const Email, ExpectedUser, ExpectedDomain: string);

    [Test]
    procedure TestSplitEmailAddress_NoAt;

    { ExtractFirstEmailAdr Tests }
    [Test]
    [TestCase('Simple', 'Contact: test@example.com for info,test@example.com')]
    [TestCase('InText', 'Please email john@test.org today,john@test.org')]
    [TestCase('AtStart', 'admin@site.com is the admin,admin@site.com')]
    [TestCase('AtEnd', 'Send to: info@company.net,info@company.net')]
    [TestCase('NoEmail', 'No email here,')]
    [TestCase('EmptyString', ',')]
    procedure TestExtractFirstEmailAdr(const Text, Expected: string);

    { ExtractAllEmailAdr Tests }
    [Test]
    procedure TestExtractAllEmailAdr_MultipleEmails;

    [Test]
    procedure TestExtractAllEmailAdr_NoEmails;

    [Test]
    procedure TestExtractAllEmailAdr_SingleEmail;

    { ExtractEmailEngine Tests }
    [Test]
    procedure TestExtractEmailEngine_FromMiddle;

    [Test]
    procedure TestExtractEmailEngine_ChainedExtraction;

    { EmailHasManyNumbers Tests }
    [Test]
    [TestCase('FewNumbers_30', 'user123@example.com,30,False')]
    [TestCase('ManyNumbers_30', '12345678@example.com,30,True')]
    [TestCase('AllNumbers_30', '123456789012@example.com,30,True')]
    [TestCase('NoNumbers_30', 'username@example.com,30,False')]
    [TestCase('ShortUser', 'a1@example.com,30,False')]
    procedure TestEmailHasManyNumbers(const Email: string; Ratio: Integer; Expected: Boolean);

    { FailCode2Str Tests }
    [Test]
    [TestCase('Unknown', '0,Unknown error occured!')]
    [TestCase('NoSeparator', '1,Missing @ symbol!')]
    [TestCase('TooSmall', '2,Data to small!')]
    [TestCase('UserTooLong', '3,User name to long!')]
    [TestCase('DomainTooLong', '4,Domain name to long!')]
    [TestCase('InvalidChar', '5,Invalid character!')]
    [TestCase('MissingUser', '6,Missing user name!')]
    [TestCase('MissingDomain', '7,Missing domain name!')]
    [TestCase('MissingDomainDot', '8,Missing domain portion (.com,.net,etc)')]
    [TestCase('InvalidGenDomain', '9,Invalid general domain!')]
    [TestCase('TooManyAt', '10,To many @ symbols!')]
    procedure TestFailCode2Str(Code: Integer; const Expected: string);

    [Test]
    procedure TestFailCode2Str_InvalidCode;

    { CheckIfThunderbirdFile Tests }
    [Test]
    procedure TestCheckIfThunderbirdFile_Valid;

    [Test]
    procedure TestCheckIfThunderbirdFile_Invalid;

    { EmailSortByDomain Tests }
    [Test]
    procedure TestEmailSortByDomain_MultipleEmails;

    [Test]
    procedure TestEmailSortByDomain_SingleEmail;

    [Test]
    procedure TestEmailSortByDomain_EmptyList;

    [Test]
    procedure TestEmailSortByDomain_SameDomain;
  end;

implementation

const
  { Fail codes from the unit }
  flUnknown               = 0;
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

procedure TTestInternetEmail.Setup;
begin
  { No setup needed }
end;

procedure TTestInternetEmail.TearDown;
begin
  { No teardown needed }
end;

{ ValidateEmailAddress Tests - Boolean }

procedure TTestInternetEmail.TestValidateEmailAddress_Boolean(const Email: string; Expected: Boolean);
begin
  Assert.AreEqual(Expected, ValidateEmailAddress(Email));
end;

{ ValidateEmailAddress Tests - With FailCode }

procedure TTestInternetEmail.TestValidateEmailAddress_FailCode_NoSeparator;
VAR
  FailCode, FailPos: Integer;
begin
  Assert.IsFalse(ValidateEmailAddress('userexample.com', FailCode, FailPos));
  Assert.AreEqual(flNoSeperator, FailCode);
end;

procedure TTestInternetEmail.TestValidateEmailAddress_FailCode_TooSmall;
VAR
  FailCode, FailPos: Integer;
begin
  Assert.IsFalse(ValidateEmailAddress('', FailCode, FailPos));
  Assert.AreEqual(flToSmall, FailCode);
end;

procedure TTestInternetEmail.TestValidateEmailAddress_FailCode_MissingUser;
VAR
  FailCode, FailPos: Integer;
begin
  Assert.IsFalse(ValidateEmailAddress('@example.com', FailCode, FailPos));
  Assert.AreEqual(flMissingUser, FailCode);
end;

procedure TTestInternetEmail.TestValidateEmailAddress_FailCode_MissingDomain;
VAR
  FailCode, FailPos: Integer;
begin
  Assert.IsFalse(ValidateEmailAddress('user@', FailCode, FailPos));
  Assert.AreEqual(flMissingDomain, FailCode);
end;

procedure TTestInternetEmail.TestValidateEmailAddress_FailCode_InvalidChar;
VAR
  FailCode, FailPos: Integer;
begin
  Assert.IsFalse(ValidateEmailAddress('user name@example.com', FailCode, FailPos));
  Assert.AreEqual(flInvalidChar, FailCode);
end;

procedure TTestInternetEmail.TestValidateEmailAddress_FailCode_TooManyAt;
VAR
  FailCode, FailPos: Integer;
begin
  Assert.IsFalse(ValidateEmailAddress('user@domain@example.com', FailCode, FailPos));
  Assert.AreEqual(flToManyAtSymbols, FailCode);
end;

{ ValidateEmailAddress Tests - String overload }

procedure TTestInternetEmail.TestValidateEmailAddress_String_Valid;
begin
  Assert.AreEqual('', ValidateEmailAddress('test@example.com', False));
end;

procedure TTestInternetEmail.TestValidateEmailAddress_String_Invalid;
VAR
  Msg: string;
begin
  Msg:= ValidateEmailAddress('invalid', False);
  Assert.IsTrue(Length(Msg) > 0, 'Should return error message for invalid email');
  Assert.IsTrue(Pos('invalid', Msg) > 0, 'Error message should contain the email');
end;

{ CorrectEmailAddress Tests }

procedure TTestInternetEmail.TestCorrectEmailAddress_TransposedAt;
VAR
  Suggestion: string;
begin
  { When user types 2 instead of @ }
  Assert.IsTrue(CorrectEmailAddress('user2example.com', Suggestion));
  Assert.AreEqual('user@example.com', Suggestion);
end;

procedure TTestInternetEmail.TestCorrectEmailAddress_InvalidChar;
VAR
  Suggestion: string;
begin
  { When there's an invalid character that can be removed }
  Assert.IsTrue(CorrectEmailAddress('user name@example.com', Suggestion));
  Assert.AreEqual('username@example.com', Suggestion);
end;

procedure TTestInternetEmail.TestCorrectEmailAddress_ValidEmail;
VAR
  Suggestion: string;
begin
  { Valid email should return true immediately }
  Assert.IsTrue(CorrectEmailAddress('user@example.com', Suggestion));
  Assert.AreEqual('user@example.com', Suggestion);
end;

procedure TTestInternetEmail.TestCorrectEmailAddress_Uncorrectable;
VAR
  Suggestion: string;
begin
  { Some emails cannot be corrected }
  Assert.IsFalse(CorrectEmailAddress('', Suggestion));
end;

{ SplitEmailAddress Tests }

procedure TTestInternetEmail.TestSplitEmailAddress(const Email, ExpectedUser, ExpectedDomain: string);
VAR
  User, Domain: string;
begin
  SplitEmailAddress(Email, User, Domain);
  Assert.AreEqual(ExpectedUser, User);
  Assert.AreEqual(ExpectedDomain, Domain);
end;

procedure TTestInternetEmail.TestSplitEmailAddress_NoAt;
VAR
  User, Domain: string;
begin
  SplitEmailAddress('nodomain', User, Domain);
  { When no @, user gets everything before position 0-1=-1, domain gets everything after }
  Assert.AreEqual('', User);
  Assert.AreEqual('nodomain', Domain);
end;

{ ExtractFirstEmailAdr Tests }

procedure TTestInternetEmail.TestExtractFirstEmailAdr(const Text, Expected: string);
begin
  Assert.AreEqual(Expected, ExtractFirstEmailAdr(Text));
end;

{ ExtractAllEmailAdr Tests }

procedure TTestInternetEmail.TestExtractAllEmailAdr_MultipleEmails;
VAR
  Emails: TStringList;
  Count: Integer;
begin
  Emails:= TStringList.Create;
  TRY
    Count:= ExtractAllEmailAdr('Contact a@b.com and c@d.com for help', Emails);
    Assert.AreEqual(2, Count);
    Assert.AreEqual(2, Emails.Count);
    Assert.AreEqual('a@b.com', Emails[0]);
    Assert.AreEqual('c@d.com', Emails[1]);
  FINALLY
    Emails.Free;
  END;
end;

procedure TTestInternetEmail.TestExtractAllEmailAdr_NoEmails;
VAR
  Emails: TStringList;
  Count: Integer;
begin
  Emails:= TStringList.Create;
  TRY
    Count:= ExtractAllEmailAdr('No emails here!', Emails);
    Assert.AreEqual(0, Count);
    Assert.AreEqual(0, Emails.Count);
  FINALLY
    Emails.Free;
  END;
end;

procedure TTestInternetEmail.TestExtractAllEmailAdr_SingleEmail;
VAR
  Emails: TStringList;
  Count: Integer;
begin
  Emails:= TStringList.Create;
  TRY
    Count:= ExtractAllEmailAdr('Contact: support@company.com', Emails);
    Assert.AreEqual(1, Count);
    Assert.AreEqual('support@company.com', Emails[0]);
  FINALLY
    Emails.Free;
  END;
end;

{ ExtractEmailEngine Tests }

procedure TTestInternetEmail.TestExtractEmailEngine_FromMiddle;
VAR
  EndPos: Integer;
  Email: string;
begin
  EndPos:= 100;
  Email:= ExtractEmailEngine('Hello test@example.com world', 1, EndPos);
  Assert.AreEqual('test@example.com', Email);
end;

procedure TTestInternetEmail.TestExtractEmailEngine_ChainedExtraction;
VAR
  Text: string;
  EndPos: Integer;
  Email1, Email2: string;
begin
  Text:= 'Email a@b.com and c@d.com today';
  EndPos:= Length(Text);

  Email1:= ExtractEmailEngine(Text, 1, EndPos);
  Assert.AreEqual('a@b.com', Email1);

  { Continue from after first email }
  Email2:= ExtractEmailEngine(Text, EndPos + 1, EndPos);
  Assert.AreEqual('c@d.com', Email2);
end;

{ EmailHasManyNumbers Tests }

procedure TTestInternetEmail.TestEmailHasManyNumbers(const Email: string; Ratio: Integer; Expected: Boolean);
begin
  Assert.AreEqual(Expected, EmailHasManyNumbers(Email, Ratio));
end;

{ FailCode2Str Tests }

procedure TTestInternetEmail.TestFailCode2Str(Code: Integer; const Expected: string);
begin
  Assert.AreEqual(Expected, FailCode2Str(Code));
end;

procedure TTestInternetEmail.TestFailCode2Str_InvalidCode;
begin
  Assert.AreEqual('Invalid error code!', FailCode2Str(-1));
  Assert.AreEqual('Invalid error code!', FailCode2Str(999));
end;

{ CheckIfThunderbirdFile Tests }

procedure TTestInternetEmail.TestCheckIfThunderbirdFile_Valid;
begin
  Assert.IsTrue(CheckIfThunderbirdFile('Header'#13#10'X-Mozilla-Status: 0000'#13#10'Content'));
end;

procedure TTestInternetEmail.TestCheckIfThunderbirdFile_Invalid;
begin
  Assert.IsFalse(CheckIfThunderbirdFile('Just some random text'));
  Assert.IsFalse(CheckIfThunderbirdFile(''));
end;

{ EmailSortByDomain Tests }

procedure TTestInternetEmail.TestEmailSortByDomain_MultipleEmails;
VAR
  Input, Output: TStringList;
begin
  Input:= TStringList.Create;
  TRY
    Input.Add('charlie@zebra.com');
    Input.Add('alice@apple.com');
    Input.Add('bob@microsoft.com');

    Output:= EmailSortByDomain(Input);
    TRY
      Assert.AreEqual(3, Output.Count);
      Assert.AreEqual('alice@apple.com', Output[0]);
      Assert.AreEqual('bob@microsoft.com', Output[1]);
      Assert.AreEqual('charlie@zebra.com', Output[2]);
    FINALLY
      Output.Free;
    END;
  FINALLY
    Input.Free;
  END;
end;

procedure TTestInternetEmail.TestEmailSortByDomain_SingleEmail;
VAR
  Input, Output: TStringList;
begin
  Input:= TStringList.Create;
  TRY
    Input.Add('user@domain.com');

    Output:= EmailSortByDomain(Input);
    TRY
      Assert.AreEqual(1, Output.Count);
      Assert.AreEqual('user@domain.com', Output[0]);
    FINALLY
      Output.Free;
    END;
  FINALLY
    Input.Free;
  END;
end;

procedure TTestInternetEmail.TestEmailSortByDomain_EmptyList;
VAR
  Input, Output: TStringList;
begin
  Input:= TStringList.Create;
  TRY
    Output:= EmailSortByDomain(Input);
    TRY
      Assert.AreEqual(0, Output.Count);
    FINALLY
      Output.Free;
    END;
  FINALLY
    Input.Free;
  END;
end;

procedure TTestInternetEmail.TestEmailSortByDomain_SameDomain;
VAR
  Input, Output: TStringList;
begin
  Input:= TStringList.Create;
  TRY
    Input.Add('charlie@example.com');
    Input.Add('alice@example.com');
    Input.Add('bob@example.com');

    Output:= EmailSortByDomain(Input);
    TRY
      Assert.AreEqual(3, Output.Count);
      { All same domain, order depends on which was found first in each pass }
    FINALLY
      Output.Free;
    END;
  FINALLY
    Input.Free;
  END;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestInternetEmail);

end.
