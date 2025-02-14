UNIT cmStrBuilder;

{=============================================================================================================
   2023.01
   www.GabrielMoraru.com
   See Copyright file
==============================================================================================================

  Replacement for TStringBuilder which can be slow.
  360 times faster than Result:= Result + s[i].

  ------------------------

  Speed test:
     500x loop;
     test file: TesterForm.pas 2.7K;
     wrap after 20 chars;

     This                    ->   31ms
     SysUtils.WrapText       ->  484ms
     Result:= Result+ s[i]   -> 5788ms

  If you want to make it even faster, integrate it directly in your code. Example in ccCore.RemoveNumbers
  ------------------------

  Also exists:
    System.SysUtils.WrapText
    It will nicely wrap the text by words, like in a Word document.
    This means that some lines will be shorter than MaxRowLenght intentionally in order to keep the words unbreaked.
    BUT it wraps the text also when it encounters a separator char (like space).
    If the separator is not encountered, the text is never wrapped !!
    So, it is great for splitting actual "text" (like in books) hence its name, and not "strings".

=============================================================================================================}

INTERFACE

TYPE
 TCStringBuilder = class(TObject)
  private
   s: string;
   CurBuffLen, BuffPos: Integer;
  public
   BuffSize: Integer;
   constructor Create(InitialBuffSize: Integer= 10000);
   procedure AddChar(Ch: Char);
   procedure AddEnter;

   function  AsText: string;
   procedure Clear;
 end;



IMPLEMENTATION



constructor TCStringBuilder.Create(InitialBuffSize: Integer= 10000);
begin
 BuffSize:= InitialBuffSize;
 Clear;
end;


procedure TCStringBuilder.Clear;
begin
 BuffPos:= 1;
 CurBuffLen:= 0;
 s:= '';
end;






function TCStringBuilder.AsText: string;
begin
 SetLength(s, BuffPos-1);  { Cut down the pre-allocated buffer that we haven't used }
 Result:= s;
end;


procedure TCStringBuilder.AddChar(Ch: Char);
begin
 if BuffPos > CurBuffLen then
  begin
   SetLength(s, CurBuffLen+ BuffSize);
   CurBuffLen:= Length(s)
  end;

 s[BuffPos]:= Ch;
 Inc(BuffPos);
end;


procedure TCStringBuilder.AddEnter;
begin
 if BuffPos+1 > CurBuffLen then    { +1 because we enter two characters into the string instead of 1 }
  begin
   SetLength(s, CurBuffLen+ BuffSize);
   CurBuffLen:= Length(s)
  end;

 s[BuffPos  ]:= #13;  //CR
 s[BuffPos+1]:= #10;  //LF

 Inc(BuffPos, 2);
end;


end.
