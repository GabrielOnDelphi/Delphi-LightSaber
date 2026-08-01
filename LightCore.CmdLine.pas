UNIT LightCore.CmdLine;

{=============================================================================================================
   Gabriel Moraru
   2026.07.26
--------------------------------------------------------------------------------------------------------------
   Declarative command-line parser for CLI tools.
   The usage text is GENERATED from the registered switches, so the help and the parser cannot drift apart.

   Grammar (kept deliberately small):
     * A switch starts with '--'. Anything else is a positional argument.
     * Value switches take their value from the NEXT parameter: --wait 500
       The next parameter is always consumed as the value, whatever it looks like.
     * Switch names match case-insensitively.
     * STRICT: an unknown switch is a parse error. A CLI tool must fail loudly on a typo
       (--vissible must not be silently ignored).
     * Not supported on purpose: --name:value, /x Windows style, grouped short flags (-abc), subcommands.

   Errors:
     * Bad USER input never raises. Parse returns FALSE and Error holds the message.
     * Bad PROGRAMMER input (duplicate registration, querying an unregistered switch) raises ECmdLineParser.

   Usage:
     Parser:= TCmdLineParser.Create;
     TRY
       Parser.SetPositionals(2, 2, '<url> <outfile.txt>');
       Parser.AddFlag('--visible', 'Show the browser window.');
       Parser.AddInt ('--wait'   , 'Settle time. Default 1200.', 1200, '<ms>');
       if NOT Parser.Parse then ... ShowError(Parser.Error);
       Url := Parser.Positional(1);
       Wait:= Parser.Int('--wait');
     FINALLY
       FreeAndNil(Parser);
     END;

   Tester:
     c:\Projects\LightSaber\UnitTesting\Test.LightCore.CmdLine.pas
   First user:
     c:\Projects\Projects AI\Uncensored Claude\Uncensored.CmdLine.pas
=============================================================================================================}

INTERFACE

USES
  System.SysUtils;

TYPE
  ECmdLineParser = class(Exception);   { Programmer errors only. User input errors go to TCmdLineParser.Error. }

  TSwitchKind = (skFlag, skStr, skInt);

  { One registered switch. Interface-visible only because the parser class stores an array of it. }
  TSwitchDef = record
    Name      : string;        { '--wait' }
    ValueName : string;        { '<ms>' - shown in the usage text; empty for flags }
    Help      : string;
    Kind      : TSwitchKind;
    StrDefault: string;
    IntDefault: Integer;
    StrValue  : string;
    IntValue  : Integer;
    Given     : Boolean;       { TRUE when the switch appeared on the command line }
  end;

  TCmdLineParser = class
  private
    FSwitches   : array of TSwitchDef;
    FPositionals: array of string;
    FPosNames   : string;      { '<url> <outfile.txt>' - space-separated, used in usage and error messages }
    FPosMin     : Integer;
    FPosMax     : Integer;
    FError      : string;
    function  IndexOf(CONST Name: string): Integer;
    function  MustFind(CONST Name: string; Kind: TSwitchKind): Integer;
    procedure AddSwitch(CONST Name, Help: string; Kind: TSwitchKind; CONST StrDefault: string; IntDefault: Integer; CONST ValueName: string);
    function  MissingPositionalError: string;
  public
    { Registration. Order of registration = order in the usage text. }
    procedure AddFlag(CONST Name, Help: string);
    procedure AddStr (CONST Name, Help: string; CONST Default: string= ''; CONST ValueName: string= '<value>');
    procedure AddInt (CONST Name, Help: string; Default: Integer= 0;       CONST ValueName: string= '<n>');
    procedure SetPositionals(MinCount, MaxCount: Integer; CONST Names: string);

    { Parsing. Can be called again: state is reset on every call. }
    function  Parse: Boolean;                                     overload;  { the real command line }
    function  Parse(CONST Params: array of string): Boolean;      overload;  { for unit tests }

    { Results. Callable before Parse - they return the defaults then. }
    function  Flag (CONST Name: string): Boolean;
    function  Str  (CONST Name: string): string;
    function  Int  (CONST Name: string): Integer;
    function  Given(CONST Name: string): Boolean;                 { Distinguishes 'absent' from 'explicitly set to the default' }
    function  PositionalCount: Integer;
    function  Positional(Index: Integer): string;                 { 1-based like ParamStr. Beyond PositionalCount returns '' (optional positionals).
                                                                    After a FAILED Parse it returns what was collected before the error - callers use
                                                                    this to recover the output file and write the error message into it. }

    function  UsageText: string;                                  { 'Switches:' + one aligned line per registered switch }
    property  Error: string read FError;
  end;


IMPLEMENTATION

USES
  System.StrUtils, System.Classes, LightCore;


{-------------------------------------------------------------------------------------------------------------
   REGISTRATION
-------------------------------------------------------------------------------------------------------------}
procedure TCmdLineParser.AddSwitch(CONST Name, Help: string; Kind: TSwitchKind; CONST StrDefault: string; IntDefault: Integer; CONST ValueName: string);
VAR Last: Integer;
begin
  if NOT StartsText('--', Name)
  then raise ECmdLineParser.Create('Switch name must start with --: '+ Name);

  if IndexOf(Name) >= 0
  then raise ECmdLineParser.Create('Switch registered twice: '+ Name);

  Last:= Length(FSwitches);
  SetLength(FSwitches, Last+1);
  FSwitches[Last].Name      := Name;
  FSwitches[Last].ValueName := ValueName;
  FSwitches[Last].Help      := Help;
  FSwitches[Last].Kind      := Kind;
  FSwitches[Last].StrDefault:= StrDefault;
  FSwitches[Last].IntDefault:= IntDefault;
  FSwitches[Last].StrValue  := StrDefault;
  FSwitches[Last].IntValue  := IntDefault;
  FSwitches[Last].Given     := FALSE;
end;


procedure TCmdLineParser.AddFlag(CONST Name, Help: string);
begin
  AddSwitch(Name, Help, skFlag, '', 0, '');
end;


procedure TCmdLineParser.AddStr(CONST Name, Help: string; CONST Default: string= ''; CONST ValueName: string= '<value>');
begin
  AddSwitch(Name, Help, skStr, Default, 0, ValueName);
end;


procedure TCmdLineParser.AddInt(CONST Name, Help: string; Default: Integer= 0; CONST ValueName: string= '<n>');
begin
  AddSwitch(Name, Help, skInt, '', Default, ValueName);
end;


procedure TCmdLineParser.SetPositionals(MinCount, MaxCount: Integer; CONST Names: string);
begin
  if (MinCount < 0) OR (MaxCount < MinCount)
  then raise ECmdLineParser.Create('Invalid positional counts.');

  FPosMin  := MinCount;
  FPosMax  := MaxCount;
  FPosNames:= Names;
end;


{-------------------------------------------------------------------------------------------------------------
   LOOKUP
-------------------------------------------------------------------------------------------------------------}
function TCmdLineParser.IndexOf(CONST Name: string): Integer;
VAR i: Integer;
begin
  for i:= 0 to High(FSwitches) DO
    if SameText(FSwitches[i].Name, Name)
    then EXIT(i);
  Result:= -1;
end;


function TCmdLineParser.MustFind(CONST Name: string; Kind: TSwitchKind): Integer;
begin
  Result:= IndexOf(Name);
  if Result < 0
  then raise ECmdLineParser.Create('Switch was never registered: '+ Name);
  if FSwitches[Result].Kind <> Kind
  then raise ECmdLineParser.Create('Switch queried with the wrong type: '+ Name);
end;


{-------------------------------------------------------------------------------------------------------------
   PARSING
-------------------------------------------------------------------------------------------------------------}
function TCmdLineParser.Parse: Boolean;
VAR
  Params: array of string;
  i: Integer;
begin
  SetLength(Params, ParamCount);
  for i:= 1 to ParamCount DO
    Params[i-1]:= ParamStr(i);
  Result:= Parse(Params);
end;


function TCmdLineParser.Parse(CONST Params: array of string): Boolean;
VAR
  i, idx: Integer;
  Param: string;
begin
  { Reset - Parse must be callable again }
  FError:= '';
  FPositionals:= NIL;
  for i:= 0 to High(FSwitches) DO
   begin
     FSwitches[i].StrValue:= FSwitches[i].StrDefault;
     FSwitches[i].IntValue:= FSwitches[i].IntDefault;
     FSwitches[i].Given   := FALSE;
   end;

  i:= 0;
  while i <= High(Params) DO
   begin
     Param:= Params[i];

     if StartsText('--', Param)
     then
       begin
         idx:= IndexOf(Param);
         if idx < 0
         then begin FError:= 'Unknown switch: '+ Param; EXIT(FALSE); end;

         FSwitches[idx].Given:= TRUE;
         if FSwitches[idx].Kind <> skFlag then
           begin
             if i = High(Params)
             then begin FError:= Param +' needs a value.'; EXIT(FALSE); end;
             Inc(i);
             case FSwitches[idx].Kind of
               skStr: FSwitches[idx].StrValue:= Params[i];
               skInt: if NOT TryStrToInt(Params[i], FSwitches[idx].IntValue)
                      then begin FError:= Param +' needs a number, got: '+ Params[i]; EXIT(FALSE); end;
             end;
           end;
       end
     else
       begin
         if Length(FPositionals) >= FPosMax then
           begin
             if FPosMax = 0
             then FError:= 'Unexpected argument: '+ Param
             else FError:= 'Too many arguments: '+ Param;
             EXIT(FALSE);
           end;
         SetLength(FPositionals, Length(FPositionals)+1);
         FPositionals[High(FPositionals)]:= Param;
       end;

     Inc(i);
   end;

  if Length(FPositionals) < FPosMin
  then begin FError:= MissingPositionalError; EXIT(FALSE); end;

  Result:= TRUE;
end;


{ Names the first missing positional when FPosNames has a name for it, else a generic count message. }
function TCmdLineParser.MissingPositionalError: string;
VAR Names: TStringList;
begin
  Names:= SplitText(FPosNames, ' ');
  TRY
    if Length(FPositionals) < Names.Count
    then Result:= 'Missing argument: '+ Names[Length(FPositionals)]
    else Result:= 'Expected at least '+ IntToStr(FPosMin) +' arguments, got '+ IntToStr(Length(FPositionals)) +'.';
  FINALLY
    FreeAndNil(Names);
  END;
end;


{-------------------------------------------------------------------------------------------------------------
   RESULTS
-------------------------------------------------------------------------------------------------------------}
function TCmdLineParser.Flag(CONST Name: string): Boolean;
begin
  Result:= FSwitches[MustFind(Name, skFlag)].Given;
end;


function TCmdLineParser.Str(CONST Name: string): string;
begin
  Result:= FSwitches[MustFind(Name, skStr)].StrValue;
end;


function TCmdLineParser.Int(CONST Name: string): Integer;
begin
  Result:= FSwitches[MustFind(Name, skInt)].IntValue;
end;


function TCmdLineParser.Given(CONST Name: string): Boolean;
VAR idx: Integer;
begin
  idx:= IndexOf(Name);
  if idx < 0
  then raise ECmdLineParser.Create('Switch was never registered: '+ Name);
  Result:= FSwitches[idx].Given;
end;


function TCmdLineParser.PositionalCount: Integer;
begin
  Result:= Length(FPositionals);
end;


function TCmdLineParser.Positional(Index: Integer): string;
begin
  if Index < 1
  then raise ECmdLineParser.Create('Positional index is 1-based.');

  if Index <= Length(FPositionals)
  then Result:= FPositionals[Index-1]
  else Result:= '';
end;


{-------------------------------------------------------------------------------------------------------------
   USAGE TEXT
-------------------------------------------------------------------------------------------------------------}
function TCmdLineParser.UsageText: string;
VAR
  i, Widest: Integer;
  LeftCol: string;

  function LeftColumn(CONST Sw: TSwitchDef): string;
  begin
    Result:= Sw.Name;
    if Sw.ValueName <> ''
    then Result:= Result +' '+ Sw.ValueName;
  end;

begin
  Widest:= 0;
  for i:= 0 to High(FSwitches) DO
    if Length(LeftColumn(FSwitches[i])) > Widest
    then Widest:= Length(LeftColumn(FSwitches[i]));

  Result:= 'Switches:';
  for i:= 0 to High(FSwitches) DO
   begin
     LeftCol:= LeftColumn(FSwitches[i]);
     Result:= Result + sLineBreak +'  '+ LeftCol + StringOfChar(' ', Widest - Length(LeftCol) + 2) + FSwitches[i].Help;
   end;
end;


end.
