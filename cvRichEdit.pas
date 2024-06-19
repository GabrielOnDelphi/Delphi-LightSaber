
UNIT cvRichEdit;

INTERFACE

USES
  Winapi.Windows, Winapi.Messages, System.Classes, Vcl.ComCtrls;

TYPE
  TCubicRichEdit = class(TRichEdit)
   private
   protected
   public
     procedure AddFormated(s: string);                                                             { Accepts string that contain enters }
     procedure CopyAll;
     procedure Add(s: string);                                                                     { Use it instead of Self.Lines.Add() }
     procedure ScrollDown;                                                                         { Scroll at the end of the text }
   published
     property OnClick;   
     property OnDblClick;
     property OnResize;
  end;

procedure Register;



IMPLEMENTATION
{$WARN GARBAGE OFF}                                                                                     {Silence the: 'W1011 Text after final END' warning }

USES
   ccCore;



procedure TCubicRichEdit.CopyAll;
begin
 SelectAll;                                                                                        { This is mandatory else it won't copy nothing }
 CopyToClipboard;                                                                                  { Copy selection }
end;


procedure TCubicRichEdit.Add(s: string);
begin
 s:= ReplaceEnters(s, ' ');
 AddFormated(s);
end;


procedure TCubicRichEdit.AddFormated(s: string);                                                   { Accepts string that contain enters }
begin
 ReplaceChar(s, #0, ' ');  { Fix bug in TRichEdit: RichEdit behaves erratically when you try to store the #0 character }
 { Details:
          http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_26783874.html
          http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_26812899.html#a34864849 }

 {todo 3: fix tihs: TRichEdit does not support ENTER at the beginning of the line! }
 Lines.Add(s);
end;


procedure TCubicRichEdit.ScrollDown;                                                               { Scroll at the end of the text }
begin
 PostMessage(Handle, WM_VSCROLL, SB_BOTTOM, 0);                                                    { from here: http://www.delphi3000.com/articles/article_845.asp?SK= }
end;



procedure Register;
begin
  RegisterComponents('LightSaber', [TCubicRichEdit]);
end;



end.============================================================================





