UNIT LightVcl.Visual.RichEdit;

{=============================================================================================================
   Gabriel Moraru
   2024.09
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
  Features:
    Adds a 'OnVScroll' property.
    CurrentLine         (the one containing the cursor).
    GetFirstVisibleLine (index of the top visible line).
    Allows us to scroll at the end of the text

  Tester:
    C:\Projects\LightSaber\Demo\Tester All Visual Controls

  Also see:
    LightVcl.Visual.RichEditResize.pas
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, Winapi.Messages, Winapi.RichEdit,
  System.Classes, Vcl.ComCtrls, Vcl.Controls;

TYPE
  TCubicRichEdit = class(TRichEdit)
   private
     FOnVScroll: TNotifyEvent;
   protected
     procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
     // procedure CreateWindowHandle(const Params: TCreateParams); override;
   public
     procedure AddFormated(s: string);           { Accepts string that contain enters }
     procedure CopyAll;
     procedure Add(s: string);                   { Use it instead of Self.Lines.Add() }

     procedure ScrollToBottom;                   { Scroll at the end of the text }
     procedure ScrollTo(Line: Integer);

     function  GetCurrentLine: Integer;          { The line containing the caret }
     function  GetFirstVisibleLine: Integer;
   published
     property OnClick;
     property OnDblClick;
     property OnResize;
     property OnVScroll: TNotifyEvent read FOnVScroll write FOnVScroll;
  end;


procedure Register;



IMPLEMENTATION

USES
   ccCore;


{
procedure TCubicRichEdit.CreateWindowHandle(const Params: TCreateParams);
var
  NewParams: TCreateParams;
begin
  NewParams := Params;
  // Set to use RichEdit50W for Rich Edit 4.1 (which supports images)
  NewParams.WinClassName := 'RichEdit50W';
  inherited CreateWindowHandle(NewParams);
end; }





function TCubicRichEdit.GetCurrentLine: Integer;
begin
  Result:= SendMessage(Handle, EM_EXLINEFROMCHAR, 0, SelStart);
end;

function TCubicRichEdit.GetFirstVisibleLine: Integer;
begin
  Result:= Perform(EM_GETFIRSTVISIBLELINE, 0, 0); // Get the top visible line
end;





procedure TCubicRichEdit.CopyAll;
begin
  SelectAll;                                                                              { This is mandatory else it won't copy nothing }
  CopyToClipboard;                                                                        { Copy selection }
end;


procedure TCubicRichEdit.Add(s: string);
begin
  s:= ReplaceEnters(s, ' ');
  AddFormated(s);
end;


procedure TCubicRichEdit.AddFormated(s: string);                                         { Accepts string that contain enters }
begin
  ReplaceChar(s, #0, ' ');  { Fix bug in TRichEdit: RichEdit behaves erratically when you try to store the #0 character }
  { Details:
           http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_26783874.html
           http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_26812899.html#a34864849 }

  {todo 3: fix tihs: TRichEdit does not support ENTER at the beginning of the line! }
  Lines.Add(s);
end;


procedure TCubicRichEdit.ScrollToBottom;                                                     { Scroll at the end of the text }
begin
  PostMessage(Handle, WM_VSCROLL, SB_BOTTOM, 0);                                          { from here: http://www.delphi3000.com/articles/article_845.asp?SK= }
end;


procedure TCubicRichEdit.ScrollTo(Line: Integer);
begin
  Perform(EM_LINESCROLL, 0, Line - GetCurrentLine);
  //del
  //if Line < 0 then Line:= 0;
  //SelStart := Perform(EM_LINEINDEX, Line, 0);
  //Perform(EM_SCROLLCARET, 0, 0);
end;


procedure TCubicRichEdit.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnVScroll)
  then FOnVScroll(Self);
end;





procedure Register;
begin
  RegisterComponents('LightSaber', [TCubicRichEdit]);
end;



end.
