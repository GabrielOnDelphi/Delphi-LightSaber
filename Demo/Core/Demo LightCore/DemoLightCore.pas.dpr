program FuzzyStringTest;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  LightCore in '..\..\..\LightCore.pas';


procedure RunTests;
var
  Pairs: array[1..20] of array[1..2] of string;
  I: Integer;
  Similarity: Integer;
  S1, S2: string;
begin
  // Define 20 test pairs
  Pairs[1][1] := 'John';                          Pairs[1][2] := 'John';                           // Expected: 100
  Pairs[2][1] := 'John';                          Pairs[2][2] := 'Jon';                            // Expected: 75
  Pairs[3][1] := 'Jim';                           Pairs[3][2] := 'James';                          // Expected: 40
  Pairs[4][1] := 'Luke Skywalker';                Pairs[4][2] := 'Darth Vader';                    // Expected: 0
  Pairs[5][1] := '';                              Pairs[5][2] := '';                               // Expected: 0
  Pairs[6][1] := '';                              Pairs[6][2] := 'abc';                            // Expected: 0
  Pairs[7][1] := 'abc';                           Pairs[7][2] := '';                               // Expected: 0
  Pairs[8][1] := 'abc';                           Pairs[8][2] := 'abc';                            // Expected: 100
  Pairs[9][1] := 'abc';                           Pairs[9][2] := 'abd';                            // Expected: ~66
  Pairs[10][1] := 'hello';                        Pairs[10][2] := 'hallo';                         // Expected: ~80
  Pairs[11][1] := 'test';                         Pairs[11][2] := 'testing';                       // Expected: ~57
  Pairs[12][1] := 'abcdefghijklmnopqrstuvwxyz';   Pairs[12][2] := 'abcdefghijklmnopqrstuvwxyz';    // Expected: 100
  Pairs[13][1] := 'abcdefghijklmnopqrstuvwxyz';   Pairs[13][2] := 'abcdegfhijklmnopqrstuvwxyz';    // Expected: high, missing 'f'
  Pairs[14][1] := 'The quick brown fox';          Pairs[14][2] := 'The quick brown fox jumps';     // Expected: ~76
  Pairs[15][1] := 'Delphi programming';           Pairs[15][2] := 'Delphi programing';             // Expected: ~94 (missing 'm')
  Pairs[16][1] := 'Case Sensitive';               Pairs[16][2] := 'case sensitive';                // Expected: 0 (case-sensitive)
  Pairs[17][1] := StringOfChar('a', 300);         Pairs[17][2] := StringOfChar('a', 300);          // Long identical, >255: Expected: 100
  Pairs[18][1] := StringOfChar('a', 30);          Pairs[18][2] := StringOfChar('b', 30);           // Long different: Expected: 0
  Pairs[19][1] := StringOfChar('a', 256) + 'b' + StringOfChar('c', 100);      Pairs[19][2] := StringOfChar('a', 200) + 'd' + StringOfChar('c', 150); // Long with some matches
  Pairs[20][1] := 'For input';                    Pairs[20][2] := 'Input';

  // Run and output tests
  for I := 1 to Length(Pairs) do
  begin
    Similarity:= -1;
    S1 := Pairs[I][1];
    S2 := Pairs[I][2];
    try
      Similarity := FuzzyStringCompare(S1, S2);
    except
      Writeln('Error: ', I, ': "', S1, '" and "', S2, '" = ', Similarity, '%')
    end;

    if Length(S1) > 50 then S1 := Copy(S1, 1, 50) + '...'; // Truncate long strings in output
    if Length(S2) > 50 then S2 := Copy(S2, 1, 50) + '...';
    Writeln('Pair ', I, ': "', S1, '" and "', S2, '" = ', Similarity, '%');
  end;
end;

begin
  try
    RunTests;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln; // Wait for input to keep console open
end.