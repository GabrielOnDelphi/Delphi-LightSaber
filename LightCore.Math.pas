UNIT LightCore.Math;

{=============================================================================================================
   2026.05.26
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================

   Math utility functions:
     - Min/Max for 3 values
     - Range clamping
     - Percent calculations
     - Rounding variants (NOTE: RoundEx/RoundUp designed for positive numbers only)
     - Statistical functions (median, mean, interquartile)
     - Factorial and combinations

=============================================================================================================}

INTERFACE

USES
   System.Math, System.Types, System.Generics.Collections;



{==================================================================================================
   MIN/MAX
==================================================================================================}
 function  Min3            (CONST a, b, c: integer): Integer;
 function  Min3S           (CONST a, b, c: Single): Single;
 function  Find_Max        (CONST a, b, c: integer): Integer; overload;
 function  Find_Max        (CONST a, b, c: Cardinal): Cardinal; overload;
 function  Find_Max        (CONST a, b, c: Double): Double ; overload;


{==================================================================================================
   RANGE
==================================================================================================}
 procedure NotHigherThan   (VAR iInput: Integer; MaxVal: Integer);          inline;              { Clamps iInput to be no greater than MaxVal }
 procedure NotSmallerThan  (VAR iInput: Integer; MinVal: Integer);          overload; inline;    { Clamps iInput to be no less than MinVal }
 procedure NotSmallerThan  (VAR iInput: Real   ; MinVal: Integer);          overload; inline;

 procedure EnsureZero      (VAR i: Integer);                                inline;
 procedure EnsureRange     (VAR i: Integer; CONST Min, Max: Integer);       inline;


{==================================================================================================
   PERCENT
==================================================================================================}
 function  ProcentNormal   (CONST WhatIs, From: Extended): Extended;                             { What is 10% of 20? Answer: 2}
 function  ProcentRepresent(CONST xRepresent, From: Int64) : Extended; overload;
 function  ProcentRepresent(CONST xRepresent, From: Extended): Extended; overload;


{==================================================================================================
   ROUND
   See also: System.Math.RoundTo Frac
==================================================================================================}
 function  RoundEx      (CONST X: Extended): LongInt;                                            { If fractional part is >= 0.5 then the number is rounded up, else down. }
 function  RoundUp      (CONST X: Extended): LongInt;                                            { This will round up a rational number }
 function  RoundDown    (CONST X: Extended): Longint;
 function  RoundTo      (CONST X: Extended; ToCategory: Integer): Longint;                       { Rounds a number up to the specified category. For example RoundTo(72, 5)=75 and RoundTo(72, 10)=80 }

 function  BeautifyPrice(Total: Extended): Extended;                                             {Example: Rounds 341 to 344.9 and 346 to 349.9 }

 function  SameValue    (R1, R2: Real): Boolean;


{==================================================================================================
   MEDIAN/INTERQ
   NOTE: Interq10/25/75/90 use a fast index-based approximation (Length div N), not exact
   textbook percentile interpolation. Use them for rough distribution shape, not statistical reports.
==================================================================================================}
 function  Median   (MX: TDoubleDynArray): Double;  overload;                                    { Use it like this: Median(TDoubleDynArray.Create(4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2)) }
 function  Median   (MX: System.Types.TIntegerDynArray): Integer;   overload;
 function  Mean     (MX: System.Types.TIntegerDynArray): Integer;                                { Average of numbers }
 function  Interq10 (MX: System.Types.TIntegerDynArray): Integer;                                { Approximate 10th percentile }
 function  Interq25 (MX: System.Types.TIntegerDynArray): Integer;                                { Approximate 25th percentile (Q1) }
 function  Interq75 (MX: System.Types.TIntegerDynArray): Integer;                                { Approximate 75th percentile (Q3) }
 function  Interq90 (MX: System.Types.TIntegerDynArray): Integer;                                { Approximate 90th percentile }


{==================================================================================================
   BASIC MATH
==================================================================================================}
 function  Factorial       (CONST n: byte): Int64;                                               { WARNING: n must be <= 20 to avoid Int64 overflow.  Formula: n! = n*(n-1)*(n-2)*...*3*2*1 }
 function  Combinations    (CONST n, r: integer): Int64;                                         { Combinations of n taken r at the time= n! / (n-r)! * r! }
 {$IFDEF CPUX86}
 function  FastModulo(const X, Y: Integer): Integer; assembler; { https://forum.lazarus.freepascal.org/index.php/topic,36342.15.html }
 {$ENDIF}


{==================================================================================================
   BINARY
==================================================================================================}
 {$IF Defined(CPUX86)}
 function MixBytes(FG, BG, BlendPower: byte): Byte; { This function mixes two bytes According to value of TRANS. The value of TRANS is between 0 (result then will be equal to FG) and 255 (result then will be equal to BG) }
 {$ENDIF}


{==================================================================================================
   OTHERS
==================================================================================================}
 function  GenerateRandomBoolean: Boolean;



IMPLEMENTATION



{-------------------------------------------------------------------------------------------------------------
   ENSURE RANGE
-------------------------------------------------------------------------------------------------------------}
procedure EnsureZero(VAR i: Integer); inline;
begin
 if i < 0 then i:= 0;
end;


procedure EnsureRange(VAR i: Integer; CONST Min, Max: Integer); inline;
begin
 if i < Min then i:= Min;
 if i > Max then i:= Max;
end;





procedure NotHigherThan(VAR iInput: Integer; MaxVal: Integer); inline;  { Makes sure that the value of iInput is not bigger MaxVal }
begin
 if iInput > MaxVal
 then iInput:= MaxVal;
end;


procedure NotSmallerThan(VAR iInput: Integer; MinVal: Integer); inline; { Makes sure that the value of iInput is not < than MinVal }
begin
 if iInput < MinVal
 then iInput:= MinVal;
end;


procedure NotSmallerThan(VAR iInput: Real; MinVal: Integer); inline;
begin
 if iInput < MinVal
 then iInput:= MinVal;
end;





{-------------------------------------------------------------------------------------------------------------
   MIN / MAX
-------------------------------------------------------------------------------------------------------------}

function Min3(CONST a, b, c: integer): Integer;
VAR min: integer;
begin
 min:= a;
 if (b < min) then min:= b;
 if (c < min) then min:= c;
 Result:= min;
end;


function Min3S(CONST a, b, c: Single): Single;                                                     { Old name: Find_min }
VAR min: Single;
begin
 min:= a;
 if (b < min) then min:= b;
 if (c < min) then min:= c;
 Result:= min;
end;


function Find_Max(CONST a, b, c: Double): Double; overload;
begin
 Result:= a;
 if (b > Result) then Result:= b;
 if (c > Result) then Result:= c;
end;


function Find_Max(CONST a, b, c: Integer): Integer;
{$IFDEF CPUx86}
 { Win32 register convention: 1st param -> EAX, 2nd -> EDX, 3rd -> ECX. Integer Result -> EAX.
   See https://docwiki.embarcadero.com/RADStudio/en/Program_Control
       https://docwiki.embarcadero.com/RADStudio/en/Assembly_Procedures_and_Functions }
 asm                                                { Does not work on 64 bit platforms! }
    cmp eax, edx
    jl @less       // edx > eax
    cmp eax, ecx
    jl @less2      // eax > edx but less than ecx. ecx is max.  2 compares, 1 assignment, 1 jump
    ret            // eax is max.  2 compares, 0 assignments, 0 jumps

    @less:         // edx > eax.. Is edx greater than ecx?
    cmp edx, ecx
    jl  @less2     // ecx is max.  2 compares, 1 assignment, 2 jumps
    mov eax, edx   // edx is max.  2 compares, 1 assignment, 1 jump
    ret

    @less2:
    mov eax, ecx   // ecx is max
 end;
{$ELSE}
 begin
  Result:= a;
  if b > Result then Result:= b;
  if c > Result then Result:= c;
 end;
{$ENDIF}


function Find_Max(CONST a, b, c: Cardinal): Cardinal;
begin
 Result:= a;
 if b > Result then Result:= b;
 if c > Result then Result:= c;
end;



{-------------------------------------------------------------------------------------------------------------
   PERCENT
--------------------------------------------------------------------------------------------------------------
  Example:
    ProcentRepresent(5  , 20 )= 25%
    ProcentRepresent(25 , 100)= 25%
    ProcentRepresent(100, 50 )= 200%

    ProcentNormal(5%  , 20 )= 1
    ProcentNormal(10% , 20 )= 2
    ProcentNormal(25% , 100)= 25
    ProcentNormal(100%, 50 )= 50
-------------------------------------------------------------------------------------------------------------}

function ProcentNormal(CONST WhatIs, From: Extended): Extended;                                   { Example1: What is 10% of 20? Answer: 2}
begin                                                                                             { Example2: What is 25% of 50? Answer: 12.5 }
 Result:= (WhatIs* From) / 100;
end;


function ProcentRepresent(CONST xRepresent, From: int64): Extended;                               { Example1: 10 is what percent of 20 ?   Answer: 50}
begin
 Assert(From> 0, 'Percent error. 0 divider.');                                                    { Example2: 25 is what percent of 50 ?   Answer: 50}
 Result:= (xRepresent* 100) / From;
end;


function ProcentRepresent(CONST xRepresent, From: Extended): Extended;
begin
 Assert(From > 0, 'Percent error. Divider must be > 0');
 Result:= (xRepresent* 100) / From;
end;









{-------------------------------------------------------------------------------------------------------------
   ROUND
-------------------------------------------------------------------------------------------------------------}

{ Standard "round half up" for POSITIVE numbers: if fractional part >= 0.5 then rounds up.
  Avoids System.Round's "banker's rounding" (round half to even), where Round(2.5)=2 and Round(3.5)=4.
  RoundEx(2.5)=3 and RoundEx(3.5)=4 — both round up at exactly 0.5.
  Source: https://docwiki.embarcadero.com/Libraries/en/System.Round
  NOTE: For negative numbers, Frac() returns negative values, so this function
  behaves as "round toward positive infinity" (e.g., RoundEx(-2.5) = -2). }
function RoundEx(CONST X: Extended): LongInt;
begin
 Result:= Trunc(x);
 if Frac(x) >= 0.50
 then Result:= Result+ 1;
end;


function RoundUp(CONST X: Extended): LongInt;
begin
 Result:= Trunc(x);
 if Frac(x) > 0
 then Result:= Result+ 1;
end;


function RoundDown(CONST X: Extended): LongInt;
begin
 Result:= Trunc(X);
end;


{ Rounds a number UP to the specified category. For example RoundTo(72, 5)=75 and RoundTo(72, 10)=80
  ToCategory must be > 0 to avoid division by zero. }
function RoundTo(CONST X: Extended; ToCategory: Integer): Longint;
begin
 Assert(ToCategory > 0, 'RoundTo: ToCategory must be greater than 0');
 Result:= ToCategory * RoundUp(x / ToCategory);
end;


{ Rounds price UP to the next multiple of 5, then subtracts 0.1 to give a "pretty" value ending in .9.
  Examples: 341 -> 345 - 0.1 = 344.9 ; 346 -> 350 - 0.1 = 349.9
  NOTE: Always rounds UP (uses RoundTo which is RoundUp-based), never to nearest. }
function BeautifyPrice(Total: Extended): Extended;
begin
  Result:= RoundTo(Total, 5) - 0.1;
end;




{-------------------------------------------------------------------------------------------------------------
   BASIC MATH
-------------------------------------------------------------------------------------------------------------}

{ Calculates n! (n factorial).
  Formula: n! = n * (n-1) * (n-2) * ... * 3 * 2 * 1
  Special case: 0! = 1 by definition.
  WARNING: n must be <= 20 to avoid Int64 overflow (21! > MaxInt64). }
function Factorial(CONST n: byte): Int64;
VAR
  i: Integer;
begin
  Assert(n <= 20, 'Factorial: n must be <= 20 to avoid overflow');

  if n <= 1
  then EXIT(1);  { 0! = 1 and 1! = 1 }

  Result:= n;
  for i:= 1 to n - 1 do
    Result:= Result * (n - i);
end;


{ Win32 register convention: X -> EAX, Y -> EDX, Integer Result -> EAX.
  Algorithm: sign-extend EAX into EDX:EAX via CDQ (which destroys EDX),
  then IDIV by ECX (copy of Y), then move remainder (EDX) into EAX.
  See https://docwiki.embarcadero.com/RADStudio/en/Program_Control }
{$IFDEF CPUX86}
function FastModulo(const X, Y: Integer): Integer; assembler;
asm
  mov ecx, edx   // ECX := Y (preserve before CDQ clobbers EDX)
  cdq            // sign-extend EAX into EDX:EAX
  idiv ecx       // EAX := quotient, EDX := remainder
  mov eax, edx   // Result (EAX) := remainder
end;
{$ENDIF}


{ FORMULA:  Combinations of n taken r at a time = n! / ((n-r)! * r!)
  Also known as "n choose r" or binomial coefficient C(n,r).
  Preconditions: n >= r >= 0, and n <= 20 (factorial overflow limit). }
function Combinations(CONST n, r: integer): Int64;
begin
  Assert(n >= 0, 'Combinations: n must be >= 0');
  Assert(r >= 0, 'Combinations: r must be >= 0');
  Assert(n >= r, 'Combinations: n must be >= r');
  Assert(n <= 20, 'Combinations: n must be <= 20 to avoid factorial overflow');

  Result:= round(Factorial(n) / ( Factorial(n-r) * Factorial(r)));
end;



{-------------------------------------------------------------------------------------------------------------
    MEDIAN / QUARTILE on arrays
-------------------------------------------------------------------------------------------------------------}

{ Calculates median of an array of doubles.
  Usage: Median(TDoubleDynArray.Create(4.1, 5.6, 7.2, 1.7, 9.3)) }
function Median(MX: TDoubleDynArray): Double;
VAR
   Middle: Integer;
begin
  Assert(Length(MX) > 0, 'Median: Array cannot be empty');

  TArray.Sort<Double>(MX);

  Middle:= Length(MX) div 2;
  if Odd(Length(MX))
  then Result:= MX[Middle]
  else Result:= (MX[Middle - 1] + MX[Middle]) / 2;
end;


{ Calculates median of an array of integers (returns rounded result). }
function Median(MX: System.Types.TIntegerDynArray): Integer;
VAR
   Middle: Integer;
begin
  Assert(Length(MX) > 0, 'Median: Array cannot be empty');

  TArray.Sort<Integer>(MX);

  Middle:= Length(MX) div 2;
  if Odd(Length(MX))
  then Result:= MX[Middle]
  else Result:= RoundEx((MX[Middle - 1] + MX[Middle]) / 2);
end;


{ Calculates the arithmetic mean (average) of an integer array.
  Returns rounded result. Array must not be empty. }
function Mean(MX: System.Types.TIntegerDynArray): Integer;
VAR i: Integer;
begin
 Assert(Length(MX) > 0, 'Mean: Array cannot be empty');

 Result:= 0;
 for i in MX DO Result:= Result+ i;
 Result:= RoundEx( Result / Length(MX));
end;


{ Approximate 10th percentile of the array (uses Length div 10 as the index — not exact textbook percentile).
  For small arrays (< 10 elements), returns a reasonable approximation. }
function Interq10(MX: System.Types.TIntegerDynArray): Integer;
VAR
   Quarter10: Integer;
begin
  Assert(Length(MX) > 0, 'Interq10: Array cannot be empty');

  TArray.Sort<Integer>(MX);

  Quarter10:= Length(MX) div 10;
  if Odd(Length(MX))
  then Result:= MX[Quarter10]
  else
    if Quarter10 = 0
    then Result:= MX[0]    { Happens when array has fewer than 10 elements }
    else Result:= RoundEx( (MX[Quarter10 - 1] + MX[Quarter10]) / 2 );
end;


{ Approximate 25th percentile / Q1 (uses Length div 4 as the index — not exact textbook quartile).
  For small arrays (< 4 elements), returns a reasonable approximation. }
function Interq25(MX: System.Types.TIntegerDynArray): Integer;
VAR
   Quarter25: Integer;
begin
  Assert(Length(MX) > 0, 'Interq25: Array cannot be empty');

  TArray.Sort<Integer>(MX);

  Quarter25:= Length(MX) div 4;
  if Odd(Length(MX))
  then Result:= MX[Quarter25]
  else
    if Quarter25 = 0
    then Result:= MX[0]                                                                            { Happens when array has fewer than 4 elements }
    else Result:= RoundEx( (MX[Quarter25 - 1] + MX[Quarter25]) / 2 );
end;


{ Approximate 75th percentile / Q3 (uses Length - Length div 4 as the index — not exact textbook quartile).
  For small arrays, returns a reasonable approximation. }
function Interq75(MX: System.Types.TIntegerDynArray): Integer;
VAR
   Quarter25, Quarter75: Integer;
begin
  Assert(Length(MX) > 0, 'Interq75: Array cannot be empty');

  TArray.Sort<Integer>(MX);

  Quarter25:= Length(MX) div 4;
  Quarter75:= Length(MX) - Quarter25;
  if Quarter75 > High(MX)
  then Quarter75:= High(MX);

  if Odd(Length(MX)) OR (Quarter75 = 0)
  then Result:= MX[Quarter75]
  else Result:= RoundEx( (MX[Quarter75 - 1] + MX[Quarter75]) / 2 );
end;


{ Approximate 90th percentile (uses Length - Length div 10 as the index — not exact textbook percentile).
  For small arrays (< 10 elements), returns a reasonable approximation. }
function Interq90(MX: System.Types.TIntegerDynArray): Integer;
VAR
   Quarter10, Quarter90: Integer;
begin
  Assert(Length(MX) > 0, 'Interq90: Array cannot be empty');

  TArray.Sort<Integer>(MX);

  Quarter10:= Length(MX) div 10;
  Quarter90:= Length(MX) - Quarter10;
  if Quarter90 > High(MX)
  then Quarter90:= High(MX);

  if Odd(Length(MX)) OR (Quarter90 = 0)
  then Result:= MX[Quarter90]
  else Result:= RoundEx( (MX[Quarter90 - 1] + MX[Quarter90]) / 2 );
end;



{-------------------------------------------------------------------------------------------------------------
   BYTE BLENDING (x86 only)
-------------------------------------------------------------------------------------------------------------}
{ Linearly interpolates (blends) two bytes based on BlendPower.
  BlendPower=0 returns FG (foreground), BlendPower=255 returns BG (background).
  Formula (exact, rounded): Result = (FG * (255 - BlendPower) + BG * BlendPower + 127) div 255
  Fixed 2026.07.06: the old ASM computed (FG*Blend + BG*(255-Blend)) shr 8, which INVERTED the
  documented BlendPower direction (0 gave ~BG) and, because the weights sum to 255 but shr 8
  divides by 256, returned 49 even for MixBytes(50,50,anything).
  TestMixBytes pinned the documented contract and failed. }
function MixBytes(FG, BG, BlendPower: Byte): Byte;
begin
  Result:= (FG * (255 - BlendPower) + BG * BlendPower + 127) DIV 255;   // +127 = round to nearest; max intermediate 65152 fits easily in Integer
end;



function SameValue(R1, R2: Real): Boolean;
begin
 Result:= System.Math.SameValue(R1, R2, 0.01);
end;


function GenerateRandomBoolean: Boolean;
begin
 Result:= Random(2)= 1;
end;


end.
