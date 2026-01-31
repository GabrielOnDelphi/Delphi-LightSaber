UNIT LightVcl.Visual.RichEdit;

{=============================================================================================================
   Gabriel Moraru
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
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
   LightCore;


{ DISABLED: CreateWindowHandle override to use RichEdit50W for Rich Edit 4.1 (image support).
  This was causing compatibility issues. Keep for reference if image embedding is needed later.
  procedure TCubicRichEdit.CreateWindowHandle(const Params: TCreateParams);
  var NewParams: TCreateParams;
  begin
    NewParams:= Params;
    NewParams.WinClassName:= 'RichEdit50W';
    inherited CreateWindowHandle(NewParams);
  end; }





{ Returns the zero-based index of the line containing the caret (SelStart). }
function TCubicRichEdit.GetCurrentLine: Integer;
begin
  Result:= SendMessage(Handle, EM_EXLINEFROMCHAR, 0, SelStart);
end;

{ Returns the zero-based index of the topmost visible line in the control. }
function TCubicRichEdit.GetFirstVisibleLine: Integer;
begin
  Result:= Perform(EM_GETFIRSTVISIBLELINE, 0, 0);
end;





procedure TCubicRichEdit.CopyAll;
begin
  SelectAll;                                                                              { This is mandatory else it won't copy nothing }
  CopyToClipboard;                                                                        { Copy selection }
end;


{ Adds text as a single line. Line breaks (CR/LF) are replaced with spaces. }
procedure TCubicRichEdit.Add(s: string);
begin
  s:= ReplaceEnters(s, ' ');
  AddFormated(s);
end;


{ Adds text that may contain line breaks (CR/LF). The string is added as-is.
  Bug fix: #0 characters are replaced with spaces to prevent erratic TRichEdit behavior.
  References:
    http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_26783874.html
    http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_26812899.html#a34864849
  TODO: TRichEdit does not support ENTER at the beginning of the line - needs investigation. }
procedure TCubicRichEdit.AddFormated(s: string);
begin
  ReplaceChar(s, #0, ' ');
  Lines.Add(s);
end;


procedure TCubicRichEdit.ScrollToBottom;                                                     { Scroll at the end of the text }
begin
  PostMessage(Handle, WM_VSCROLL, SB_BOTTOM, 0);                                          { from here: http://www.delphi3000.com/articles/article_845.asp?SK= }
end;


{ Scrolls the RichEdit so that the specified line becomes visible.
  Line: Zero-based line index.
  Note: Uses relative scrolling from current caret position via EM_LINESCROLL. }
procedure TCubicRichEdit.ScrollTo(Line: Integer);
begin
  Perform(EM_LINESCROLL, 0, Line - GetCurrentLine);
end;


procedure TCubicRichEdit.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnVScroll)
  then FOnVScroll(Self);
end;





procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TCubicRichEdit]);
end;



end.
