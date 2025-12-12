UNIT LightCore.Math;

{=============================================================================================================
   2023.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================

   Math functions

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
 procedure NotHigherThan   (VAR iInput: Integer; MaxVal: Integer);                               { Makes sura that the value of iInput is not bigger MaxVal }
 procedure NotSmallerThan  (VAR iInput: Integer; MinVal: Integer);         overload;             { Makes sura that the value of iInput is not < than MinVal }
 procedure NotSmallerThan  (VAR iInput: Real   ; MinVal: Integer);         overload;

 procedure EnsureZero      (VAR i: Integer);
 procedure EnsureRange     (VAR i: Integer; CONST Min, Max: Integer);


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
==================================================================================================}
 function  Median   (MX: TDoubleDynArray): Double;  overload;                                    { Use it like this: Median(TDoubleDynArray.Create(4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2)) }
 function  Median   (MX: System.Types.TIntegerDynArray): Integer;   overload;
 function  Mean     (MX: System.Types.TIntegerDynArray): Integer;                                { Media numerelor }
 function  Interq10 (MX: System.Types.TIntegerDynArray): Integer;
 function  Interq25 (MX: System.Types.TIntegerDynArray): Integer;
 function  Interq75 (MX: System.Types.TIntegerDynArray): Integer;
 function  Interq90 (MX: System.Types.TIntegerDynArray): Integer;


{==================================================================================================
   BASIC MATH
==================================================================================================}
 function  Factorial       (CONST n: byte): Int64;                                               { ATENTIE: din cauza numerelor gigantice pe care mi le da, parametrul 'n' nu poate fi mai mare de 20.  Formula n!= n*(n-1)*(n-2)*...*3*2*1 }
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
procedure EnsureZero(VAR i: Integer);
begin
 if i < 0 then i:= 0;
end;


procedure EnsureRange(VAR i: Integer; CONST Min, Max: Integer);
begin
 if i < Min then i:= Min;
 if i > Max then i:= Max;
end;





procedure NotHigherThan(VAR iInput: Integer; MaxVal: Integer);  { Makes sure that the value of iInput is not bigger MaxVal }
begin
 if iInput > MaxVal
 then iInput:= MaxVal;
end;


procedure NotSmallerThan(VAR iInput: Integer; MinVal: Integer); { Makes sure that the value of iInput is not < than MinVal }
begin
 if iInput < MinVal
 then iInput:= MinVal;
end;


procedure NotSmallerThan(VAR iInput: Real; MinVal: Integer);
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
{$IFDEF CPUx86}                      { See this for details: http://docwiki.embarcadero.com/RADStudio/XE8/en/Conditional_compilation_%28Delphi%29 }
 // eax : first param & Result
 // ecx : second param
 // edx : third param
 asm                                                { Does not work on 64 bit platforms! }
    cmp eax, ecx
    jl @less       // ecx > eax
    cmp eax, edx
    jl @less2      // eax > ecx but less than edx. edx is max.  2 compares, 1, assignment, 1 jump
    ret            // eax is max.  2 compares, 0 assignments, 0 jumps

    @less:         // ecx > eax.. Is ecx greater than edx?
    cmp ecx, edx
    jl  @less2     // edx is max.  2 compares, 1 assignment,  2 jumps
    mov eax, ecx   // ecx is max.  2 compares, 1 assignment, 1 jump
    ret

    @less2:
    mov eax, edx   // edx is max
 end;
{$ELSE }
 begin   { Also see:  http://www.mail-archive.com/delphi@elists.org/msg03076.html }
  if A > B
  then
    if A > C
    then Result := A
    else Result := C
  else
    if B > C
    then Result := B
    else Result := C;
 end;
{$ENDIF}


function Find_Max(CONST a, b, c: Cardinal): Cardinal;
begin
 if A > B
 then
   if A > C
   then Result := A
   else Result := C
 else
   if B > C
   then Result := B
   else Result := C;
end;



{-------------------------------------------------------------------------------------------------------------
  PRECENT
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
 Assert(From> 0);
 Result:= (xRepresent* 100) / From;
end;









{-------------------------------------------------------------------------------------------------------------
   ROUND
-------------------------------------------------------------------------------------------------------------}

{ If fractional part is >= 0.5 then the number is rounded up, else down. "Bank" algorithm example: Round(25.5) = 26 but Round(26.5) = 26 }
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


{ Rounds a number UP to the specified category. For example RoundTo(72, 5)=75 and RoundTo(72, 10)=80 }
function RoundTo(CONST X: Extended; ToCategory: Integer): Longint;
begin
 Result:= ToCategory * RoundUp(x / ToCategory);
end;


{ Example: Rounds 341 to 344.9 and 346 to 349.9 }
function BeautifyPrice(Total: Extended): Extended;
begin
 Result:= RoundTo(Total, 5) - 0.1;      { !DO NOT CHANGE the '5' cosntant }
end;




{-------------------------------------------------------------------------------------------------------------
   BASIC MATH
-------------------------------------------------------------------------------------------------------------}

{ WARNING: because of the giant numbers that result, the 'n' parameter cannot be greater than 20 }
function Factorial(CONST n: byte): Int64;
VAR i: Integer;                                                   { FORMULA:    n!= n*(n-1)*(n-2)*...*3*2*1 }
begin
 Result:= n;
 for i:= 1 TO n-1 DO
   Result:= Result*(n-i);
end;


{ Source: https://forum.lazarus.freepascal.org/index.php/topic,36342.15.html }
{$IFDEF CPUX86}
function FastModulo(const X, Y: Integer): Integer; assembler;
asm
  mov eax, ecx
  mov ecx, edx
  cdq
  idiv ecx
  mov eax, edx
end;
{$ENDIF}


{ FORMULA:  Combinations of n taken r at the time= n! / (n-r)! * r! }
function Combinations(CONST n, r: integer): Int64;
begin
  Result:= round(Factorial(n) / ( Factorial(n-r) * Factorial(r)));
end;



{-------------------------------------------------------------------------------------------------------------
    MEDIAN / QUARTILE on arrays
-------------------------------------------------------------------------------------------------------------}

{ Use it like this: Median(TDoubleDynArray.Create(4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2)) }
function Median(MX: TDoubleDynArray): Double;
VAR
   Middle: Integer;
begin
  TArray.Sort<Double>(MX);

  Middle := Length(MX) div 2;
  if Odd(Length(MX))
  then Result := MX[Middle]
  else Result := (MX[Middle - 1] + MX[Middle]) / 2;
end;


function Median(MX: System.Types.TIntegerDynArray): Integer;
VAR
   Middle: Integer;
begin
  TArray.Sort<Integer>(MX);

  Middle := Length(MX) div 2;
  if Odd(Length(MX))
  then Result := MX[Middle]
  else Result := RoundEx( (MX[Middle - 1] + MX[Middle]) / 2 );
end;


function Mean(MX: System.Types.TIntegerDynArray): Integer;  { Average numbers }
VAR i: Integer;
begin
 Assert(Length(MX) > 0);

 Result:= 0;
 for i in MX DO Result:= Result+ i;
 Result:= RoundEx( Result / Length(MX));
end;


function Interq10(MX: System.Types.TIntegerDynArray): Integer;
VAR
   Quarter10: Integer;
begin
  TArray.Sort<Integer>(MX);

  Quarter10 := Length(MX) div 10;
  if Odd(Length(MX))
  then Result:= MX[Quarter10]
  else
    if Quarter10= 0
    then Result:= MX[0]    { It happens when I have less than 10 elements in the matrix }
    else Result:= RoundEx( (MX[Quarter10 - 1] + MX[Quarter10]) / 2 );
end;


function Interq25(MX: System.Types.TIntegerDynArray): Integer;
VAR
   Quarter25: Integer;
begin
  TArray.Sort<Integer>(MX);

  Quarter25 := Length(MX) div 4;
  if Odd(Length(MX))
  then Result := MX[Quarter25]
  else
    if Quarter25= 0
    then Result:= MX[0]                                                                            { It happens when I have less than 4 elements in the matrix }
    else Result := RoundEx( (MX[Quarter25 - 1] + MX[Quarter25]) / 2 );
end;


function Interq75(MX: System.Types.TIntegerDynArray): Integer;
VAR
   Quarter25, Quarter75: Integer;
begin
  TArray.Sort<Integer>(MX);

  Quarter25 := Length(MX) div 4;
  Quarter75:= Length(MX) - Quarter25;
  if Quarter75> High(MX) then Quarter75:= High(MX);

  if Odd(Length(MX))
  then Result := MX[Quarter75]
  else Result := RoundEx( (MX[Quarter75 - 1] + MX[Quarter75]) / 2 );
end;


function Interq90(MX: System.Types.TIntegerDynArray): Integer;
VAR
   Quarter10, Quarter90: Integer;
begin
  TArray.Sort<Integer>(MX);

  Quarter10:= Length(MX) div 10;
  Quarter90:= Length(MX) - Quarter10;
  if Quarter90> High(MX) then Quarter90:= High(MX);

  if Odd(Length(MX))
  then Result := MX[Quarter90]
  else Result := RoundEx( (MX[Quarter90 - 1] + MX[Quarter90]) / 2 );
end;



{-------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------}
{$IF Defined(CPUX86)}
{ This function mixes two bytes According to value of TRANS.
 The value of TRANS is between 0 (result then will be equal to FG) and 255 (result then will be equal to BG) }
function MixBytes(FG, BG, BlendPower: Byte): Byte;
asm
  push bx      // push some regs
  push cx
  push dx
  mov DH,BlendPower // remembering Transparency value (or Opacity - as you like)
  mov BL,FG    // filling registers with our values
  mov AL,DH    // BL = ForeGround (FG)
  mov CL,BG    // CL = BackGround (BG)
  xor AH,AH    // Clear High-order parts of regs
  xor BH,BH
  xor CH,CH
  mul BL       // AL=AL*BL
  mov BX,AX    // BX=AX
  xor AH,AH
  mov AL,DH
  xor AL,$FF   // AX=(255-TRANS)
  mul CL       // AL=AL*CL
  add AX,BX    // AX=AX+BX
  shr AX,8     // Fine! Here we have mixed value in AL
  pop dx       // Hm... No rubbish after us, ok?
  pop cx
  pop bx       // Bye, dear Assembler - we go home to Delphi!
end;
{$ENDIF}



function SameValue(R1, R2: Real): Boolean;
begin
 Result:= System.Math.SameValue(R1, R2, 0.01);
end;


function GenerateRandomBoolean: Boolean;
begin
 Result:= Random(2)= 1;
end;


end.
