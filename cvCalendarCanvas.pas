UNIT cvCalendarCanvas;
{-------------------------------------------------------------------------------------------------------------
  Freeware
  Calendar descended from TComponent
  Draws the calendar on the provided canvas.

  This is my addaptation from c:\Myprojects\Packages\Third party packages\CalendarPanel.pas
--------------------------------------------------------------------------------------------------------------

  -Calendar-Header-Buttons now are BitBtns and support hints (If you want other Hint-Texted, Change the Procedure ShowButtons)
  -ColWeekend splitted to ColSaturday,ColSunday
  -property ShowWeeks added (Shows the Weeknumbers!)
  -property ShowButtonHints added
  -DblClick works now only in the Calendar-Area
  -Functions for Weekhandling (This functions should help building Querys for filtering/searching WeekNo)
   Example:
    I would like to query all custumers of LastWeek:
     -Take the now-TDateTime and sub 7 (7 Days)
     -Week1stDay:=WeeksFirstDay(aDate: TDateTime) : TDateTime; to get the TDateTime of Week-Beginning
     -WeekLastDay:=WeeksLastDay(aDate: TDateTime) : TDateTime; to get the TDateTime of Week-Ending
     now you have 2 TDateTime-Objects for using in a Query!!!
     (LIKE:  SELECT * FROM Customer
             WHERE Date>=:Param1(To fill with Week1stDay)
             AND Date <=:Param2(To fill with WeekLastDay)) //EXAMPLE ONLY-NOT TESTED!!!

  Font: Big deal! Actually, the point is that the Font can be changed (typically the size would be changed) when TCalendarCanvas is Resized (OnResize).

  Reset date:
    The code, as it has been written to prevent a user entering an invalid date, which can happen with a ScrollBar.
    If the date highlighted is 31 August, and the user scrolls to September, the CalendarDate.Day is reset to the DaysInMonth (ie, 30), to prevent an error.
    If you use 'MMMM DD YYYY' format in your Win International settings, ie US users, then the example above would use August 31. In other words, the code is 'Internationalized', to that extent.

  TODO-Section:
    -Year2k-Test - I Think it works
    -Optimizing to reduce the Code
    -Size-Checking to avoid strange Effects
    -Find Bugs and Remove!!!

  Based on Calpnl.pas (freeware) written by Peter Crain, Robert Vivrette, Roland Weinschuetz, Harri Kasulke, Harri.Kasulke@Okay.net
  
  Tester: c:\MyProjects\Project Testers\Calendar tester\
-------------------------------------------------------------------------------------------------------------}

//del {.$R CalendarCanvas.RES}         //Includes the Resource-File for TBitBtn Glyphs

INTERFACE

USES
  System.SysUtils, System.Types, WinApi.Windows, System.Classes, Vcl.Graphics;

CONST
  BORDER = 2;
  DAYS_IN_MONTH: array[1..12] of Integer = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
  BUTTON_WIDTH = 16;

TYPE
 TDayWidth = (dw1Char, dw2Char, dw3Char);
 TPaintJob = (All, Header, Dates);

 TDateType = record
  aYear, aMonth, aDay : Word;
 end;

 TCalendarCanvas = class(TObject)
 private
  FCanvas: TCanvas;
  g_PrevYear      : Word;
  g_PrevMonth     : Word;
  g_DateArray     : array[1..42] of string;{ Changed for showing days of next and prev month}
  g_CurrDateIndex : Integer;
  g_PrevDateIndex : Integer;
  g_WeekNumbers   : Array[0..5] of string; { WeekNo } // index +1; Sunday is copied to last index
  g_DayTitles     : Array[0..7] of string; { moved from const to enable Int ShortDayNames}
  FOnDateChange   : TNotifyEvent;
  FDateInHeader   : Boolean;
  FUseLongDate    : Boolean;
  CellHeight      : Integer;
  CellWidth       : Integer;
  HeadingRect     : TRect;                 { The area where we draw the header }
  CalendarRect    : TRect;                 { The area where we draw the dates }
  FMonth          : Integer;
  FDay            : Integer;
  FYear           : Integer;
  FDayWidth       : TDayWidth;
  FCalendarDate   : TDateTime;
  FGermanDate     : Boolean;               { Boolean to switch to german date }
  FShowWeeks      : Boolean;               { Show WeekNo and Complete Weeks}
  FColHoliday     : TColor;
  FColSunday      : TColor;
  FColSaturday    : TColor;                { to add sep. Color to Staturday}
  FColMarked      : TColor;
  FHolidays       : TStrings;              // Property for storing holidays and special days as strings format: dd.mm.
  FMarkdays       : TStrings;
  FWidth          : Integer;
  FColor          : Integer;
  FSolidBkg       : Boolean;
  FHeight         : Integer;
  FLeft           : Integer;
  FTop            : Integer;               // Property for storing holidays and special days as strings format: dd.mm.
  procedure SetCalendarDate (aDate: TDateTime);
  procedure SetMonth      (Value: Integer);
  procedure SetDay        (Value: Integer);
  procedure SetYear       (Value: Integer);
  function  JulDate1stWeek(JD : TDateTime) : TDateTime;
  function  WeekNo        (JDate : TDateTime): Integer;
  function  GetWeekNumber:Integer;
  function  GetDayOfYear: Integer;
  function  GetDaysInYear:Integer;
  procedure SetGermanDate (Value: Boolean);     // RW: this one sets the german date
  procedure SetShowWeeks  (Value: Boolean);    // RW: adapted DayOfWeek-function to fit german date
  function  rDayOfWeek    (vDate: TDateTime) : Integer;
  procedure SetColHoliday (Value: TColor);
  procedure SetColSunday  (Value: TColor);
  procedure SetColSaturday(Value: TColor);
  procedure SetColMarked  (Value: TColor);
  procedure SetHolidays   (Value: TStrings);    // RW: build string lists
  procedure SetMarkdays   (Value: TStrings);
  function  CheckHoliday  (DateList: TStrings; sd: string; m: integer) : Boolean;
  procedure SetFont       (const Value: TFont);
  function  GetFont: TFont;     // RW: returns TRUE if parameter denotes a special day
 protected
  procedure DateChange;
  procedure DrawDateInHeader;
  procedure DrawDaysHeader;
  procedure DrawDates;
  procedure DrawWeeks;
  procedure DrawFocusFrame    (nIndex : Integer);
  procedure LoadDateArray;
  function  GetMonthBegin: Integer;
  function  IsLeapYear  (AYear: Integer): Boolean;
  function  SetDate     (nDays : Integer): Boolean;
  function  GetRectFromIndex  (nIndex : Integer): TRect;
  function  GetIndexFromDate: Integer;
  function  GetIndexFromPoint (nLeft : Integer ; nTop : Integer) : Integer;
  function  ValidDate   (aDate: TDateType) : Boolean;
 public
  constructor Create(Canvas : TCanvas);  { We don't use 'overload' because we want to hide the original Create(AOwner) constructor. We always want to pass the second parameter.  http://docwiki.embarcadero.com/RADStudio/Sydney/en/Methods_(Delphi). c:\MyProjects\My books\Building cross-platform applications\Demo projects\Methods and polymorphism\PolyTest.dpr }
  destructor Destroy; override;

  function DaysInMonth  (nMonth, nYear : Integer): Integer;
  function IsValidDate  (yy,mm,dd: Integer): Boolean;
  function WeeksFirstDay(aDate: TDateTime): TDateTime;      { This function helps to Filter/Query a Table by Weeks}
  function WeeksLastDay (aDate: TDateTime): TDateTime;      { This function helps to Filter/Query a Table by Weeks}

  property Day          : Integer      read FDay   write SetDay;
  property Month        : Integer      read FMonth write SetMonth;
  property Year         : Integer      read FYear  write SetYear;
  property CalendarDate : TDateTime    read FCalendarDate write SetCalendarDate; // A TDateTime property that you can read or write to programmatically. The fractional part of CalendarDate, i.e. the time, is not stored.
  property WeekNumber   : Integer      read GetWeekNumber;                       // An integer representing the... Week number of the TCalendarCanvas.Year.
  property DayOfYear    : Integer      read GetDayOfYear;                        // value for days that have passed, in the current (CalendarDate) year.
  property DaysInYear   : Integer      read GetDaysInYear;                       // can be either 365 or 366. It could have just as easily been Boolean (it calls the Boolean IsLeapYear protected Function), but it suited my project.
  procedure Paint;
 public
  property Left         : Integer      read FLeft         write FLeft           default 0;
  property Top          : Integer      read FTop          write FTop            default 0;
  property Height       : Integer      read FHeight       write FHeight         default 350;
  property Width        : Integer      read FWidth        write FWidth          default 300;

  property Color        : Integer      read FColor        write FColor          default clWhite;
  property Font         : TFont        read GetFont       write SetFont;
  property DateInHeader : Boolean      read FDateInHeader write FDateInHeader   default FALSE;  // Shows\Hides the 'MMMMM YYYY' display above the abbreviated day names at the top. The Months or Years can then be changed programmatically by ScrollBars or similar.
  property UseLongDate  : Boolean      read FUseLongDate  write FUseLongDate    default TRUE;
  property DayWidth     : TDayWidth    read FDayWidth     write FDayWidth       default dw2Char;   // Use 1 to 3 characters (M, Mo, Mon) to define the day Name.
  property OnDateChange : TNotifyEvent read FOnDateChange write FOnDateChange;  // A centralized event that allows users to change Labels, ScrollBars, Graphs or ProgressBars when the CalendarDate property is changed, internally or externally.
  property WkStartMonday: Boolean      read FGermanDate   write SetGermanDate;
  property ShowWeeks    : Boolean      read FShowWeeks    write SetShowWeeks    default FALSE; // Show week of the year
  property ColHoliday   : TColor       read FColHoliday   write SetColHoliday;
  property ColSunday    : TColor       read FColSunday    write SetColSunday    default $0025289A;
  property ColSaturday  : TColor       read FColSaturday  write SetColSaturday  default $0025289A;
  property ColMarked    : TColor       read FColMarked    write SetColMarked;   // to mark important dates
  property Holidays     : TStrings     read FHolidays     write SetHolidays;    // for storing holidays and special days as strings.
  property Markdays     : TStrings     read FMarkdays     write SetMarkdays;    // for storing holidays and special days as strings.
  property SolidBkg     : Boolean      read FSolidBkg     write FSolidBkg       default FALSE;
 end;


function  CalculateDayOfYear(y, m, d : Word): Integer;


IMPLEMENTATION
USES cGraphText;






{-------------------------------------------------------------------------------------------------
  TCalendarCanvas
--------------------------------------------------------------------------------------------------}
constructor TCalendarCanvas.Create(Canvas : TCanvas);
var
 iCount: Integer;
 aY, aM, aD: Word;
 FormatSettings: System.SysUtils.TFormatSettings;
begin
 inherited Create;

 FColSunday    := $0025289A;
 FColSaturday  := $0025289A;
 FHolidays     := TStringList.Create;   // Create the stringlists for special days
 FMarkdays     := TStringList.Create;
 FHeight       := 350;
 FWidth        := 300;
 FSolidBkg     := FALSE;                // Fill the calendar with solid color
 FormatSettings:= TFormatSettings.Create;

 for iCount:= 0 to 6
  DO g_DayTitles[iCount] := FormatSettings.ShortDayNames[iCount +1];

 FUseLongDate   := TRUE;
 DayWidth       := dw2Char;
 g_DayTitles[7] := FormatSettings.ShortDayNames[1];   // Copy sunday to index 7 for german date
 FCalendarDate  := Date;
 FDateInHeader      := FALSE;  // Toggle the 'MMMMM YYYY' display above the abbreviated day names at the top. The Months or Years can then be changed programmatically by ScrollBars or similar.
 DecodeDate(FCalendarDate, aY, aM, aD );
 FMonth         := Integer(aM);
 FDay           := Integer(aD);
 FYear          := Integer(aY);
 g_PrevDateIndex:= 0;
 LoadDateArray;
 SetDate(0);

 FCanvas:= Canvas;  // Now we can start
 FCanvas.Font.Size:= 12;
 FCanvas.Font.Style:= [fsBold];
end;


destructor TCalendarCanvas.Destroy;
begin
  FHolidays.Free;
  FMarkdays.Free;
  inherited Destroy;
end;






{-------------------------------------------------------------------------------------------------------------
   DRAW
-------------------------------------------------------------------------------------------------------------}
CONST iInnerSpace= 0;   { iInnerSpace = the border, including bevels, on 1 side }
//todo: expose this iInnerSpace a property


procedure TCalendarCanvas.Paint;
VAR
  dd,
  iWBorder, iHBorder,
  iInnerW, innerH,
  iLMargin, iLinesH: Integer;
begin
 if FCanvas= NIL then EXIT;

 if FShowWeeks
 then dd:=8
 else dd:=7;

 iInnerW := Width - (iInnerSpace * 2);
 iWBorder:= iInnerW div 100;

 { ClientWidth is a product of useable space, not all space }
 { clear space less a border both sides, makes ClientWidth narrower }
 CellWidth := (iInnerW - (iWBorder * 2)) div dd;
 innerH := Height - (iInnerSpace * 2);
 iHBorder := innerH div 100;

 if DateInHeader                       // Toggle the 'MMMMM YYYY' display above the abbreviated day names at the top. The Months or Years can then be changed programmatically by ScrollBars or similar.
 then iLinesH := 8
 else iLinesH := 7;

 { take out 2 iHBorder for spacing at top }
 CellHeight:= (innerH  - (iHBorder * 2)) div iLinesH;
 iLMargin  := (iInnerW - (CellWidth * dd)) div 2;

 HeadingRect.Top   := Top  + iInnerSpace + iHBorder;
 HeadingRect.Left  := Left + iInnerSpace + iLMargin ;
 HeadingRect.Right := HeadingRect.Left + (CellWidth * dd);
 HeadingRect.Bottom:= Height;

 if DateInHeader            // Toggle the 'MMMMM YYYY' display above the abbreviated day names at the top. The Months or Years can then be changed programmatically by ScrollBars or similar.
 then HeadingRect.Bottom := HeadingRect.Top + (CellHeight * 2)
 else HeadingRect.Bottom := HeadingRect.Top + CellHeight;

 CalendarRect := HeadingRect;  // Copy Left/Right. Recalculate Top/Btm
 CalendarRect.Top := HeadingRect.Bottom;
 CalendarRect.Bottom := CalendarRect.Top + (CellHeight * 6); // THIS IS UNUSED!   //6 is the number of Rows?

 if FShowWeeks
 then CalendarRect.Left := HeadingRect.Left + CellWidth; //showing WeekNo (week of the year)

 if SolidBkg then
  begin
   FCanvas.Brush.Color := clBtnFace;
   FCanvas.FillRect(CalendarRect);
  end;

 g_CurrDateIndex := FDay + GetMonthBegin - 1;

 if DateInHeader
 then DrawDateInHeader;    // Toggle the 'MMMMM YYYY' display above the abbreviated day names at the top. The Months or Years can then be changed programmatically by ScrollBars or similar.

 DrawDaysHeader;

 // Draw the actual day numbers
 DrawDates;

 if FShowWeeks    //showing WeekNo (week of the year)
 then DrawWeeks;

 DrawFocusFrame(g_CurrDateIndex);
end;


// Draw the 'MMMMM YYYY' display above the abbreviated day names at the top. The Months or Years can then be changed programmatically by ScrollBars or similar.
procedure TCalendarCanvas.DrawDateInHeader;
var
   iRectHt, iSpaces, iIndent: Integer;
   sMonth : String;
   TempRect : TRect;
begin
 with FCanvas do
   begin
    Font.Color := clBlack;
    Font.Style := [fsBold];
    if UseLongDate
    then sMonth := FormatDateTime( 'dd mmmm yyyy', FCalendarDate )
    else sMonth := FormatDateTime( 'dd mmm yyyy' , FCalendarDate );

    TempRect := HeadingRect;
    iRectHt := HeadingRect.Bottom - HeadingRect.Top;
    iIndent := (TempRect.Right - TempRect.Left) div 20;
    iSpaces := (iRectHt div 20) * BORDER;
    if iSpaces = 0
    then iSpaces:= 1;

    TempRect.Top   := TempRect.Top   + iSpaces ;
    TempRect.Bottom:= TempRect.Top   + CellHeight ;
    TempRect.Left  := TempRect.Left  + iIndent + BUTTON_WIDTH + 1;
    TempRect.Right := TempRect.Right - (iIndent + BUTTON_WIDTH + 1);
    Brush.Color    := clBtnFace;
    Brush.Style    := bsClear;
    {todo: if FSolidBkg
     Brush.Style := bsxxx;
     FillRect( TempRect ); }
    DrawText( Handle, sMonth, Length( sMonth ), TempRect,( DT_CENTER or DT_TOP or DT_SINGLELINE ) );
   end;
end;


procedure TCalendarCanvas.DrawDaysHeader;
var
   i,iDayWidth: Integer;
   ARect: TRect;
   pDay: string;
   pCW: string;
begin
  Case DayWidth of
    dw1Char : iDayWidth := 1;
    dw2Char : iDayWidth := 2;
    dw3Char : iDayWidth := 3;
   else
     iDayWidth := 1;
  end;

  ARect := HeadingRect;
  ARect.Right := ARect.Left + CellWidth;

  if DateInHeader        // Toggle the 'MMMMM YYYY' display above the abbreviated day names at the top. The Months or Years can then be changed programmatically by ScrollBars or similar.
  then ARect.Top := ARect.Top + CellHeight ;
  { Cycle through the days }

  {  Showing WeekNo (week of the year) }
  if fShowWeeks then
   begin
    if FGermanDate
    then pCW:= 'KW'
    else pCW:= 'CW';

    FCanvas.Font.Color := clTeal;
    DrawText( FCanvas.Handle, pCW, 2, ARect,( DT_CENTER or DT_VCENTER or DT_SINGLELINE ) );

    ARect.Left := ARect.Right;
    ARect.Right := ARect.Right + CellWidth;
   end;

  FCanvas.Font.Style := [fsBold]; {make Days Bold}

  for i := 0 to 6 do
   begin
    if not FGermanDate   // german date: (i=5) or (i=6)
      then
       begin
         FCanvas.Font.Color := clBlack;
         if (i = 0) then FCanvas.Font.Color := ColSunday;
         if (i = 6) then FCanvas.Font.Color := ColSaturday;
         pDay:= g_DayTitles[i];
         //orig code: StrCopy( pDay, Copy(g_DayTitles[i], 1, iDayWidth));
       end
      else
       begin
         FCanvas.Font.Color := clBlack;
         if (i = 6) then FCanvas.Font.Color := ColSunday;
         if (i = 5) then FCanvas.Font.Color := ColSaturday;
         pDay:= g_DayTitles[i+1];
         //orig code: StrCopy( pDay, Copy(g_DayTitles[i+1], 1, iDayWidth));
       end;

      DrawText( FCanvas.Handle, pDay, iDayWidth, ARect, ( DT_CENTER or DT_VCENTER or DT_SINGLELINE ) );
      ARect.Left := ARect.Right;
      ARect.Right := ARect.Right + CellWidth;
   end;
   FCanvas.Font.Color := clBlack;
   FCanvas.Font.Style := [];  {reset Days <> Bold}

   { Draw line below days }
   with FCanvas do
      begin
         ARect.Top := CalendarRect.Top - 4;
         ARect.Left := HeadingRect.Left;
         ARect.Right := HeadingRect.Right;
         Pen.Color := clBtnHighlight;
         MoveTo( ARect.Left , ARect.Top);
         LineTo( ARect.Right, ARect.Top );
         Pen.Color := clBtnShadow;
         MoveTo( ARect.Left,  ARect.Top + 1 );
         LineTo( ARect.Right, ARect.Top + 1  );
      end;
end;


procedure TCalendarCanvas.DrawDates;
VAR
   nIndex, nWeek, nDay: Integer;
   pDate1, pDate2, pDate: string;
   x,y: Integer;
CONST
   ShowGridLines: Boolean= FALSE;
begin
 With FCanvas DO
  begin
   { Define normal font }
   Font.Style := [];
   Pen.Color := clBlack;
   Brush.Style:= bsClear;   //todo: if FSolidBkg

   { Cycle through the weeks }
   for nWeek := 1 to 6 do
    begin
     { Cycle through the days }
     for nDay := 1 to 7 Do
      begin
       nIndex := nDay + ( ( nWeek - 1 ) * 7 );

       pDate1:= g_DateArray[nIndex];
       pDate2:= pDate1;
       Delete(pDate1, 1, 1);
       pDate:= pDate1;
       if pDate= '  ' then Break;

       Font.Color := clBlack;
       if not FGermanDate
       then
        begin
         if nDay = 1 then Font.Color := ColSunday;
         if nDay = 7 then Font.Color := ColSaturday;
        end
       else
        begin
         if nDay = 7 then Font.Color := ColSunday;
         if nDay = 6 then Font.Color := ColSaturday;
        end;

        if CheckHoliday(Holidays, pDate, FMonth)
        then Font.Color := ColHoliday;
        if CheckHoliday(Markdays, pDate, FMonth)
        then FCanvas.Font.Color := ColMarked;

        // Write with shadow (a)
        x:= CalendarRect.Left + (CellWidth  * (nDay - 1));
        y:= CalendarRect.Top  + (CellHeight * (nWeek -1));

        if ShowGridLines then
         begin
          Pen.Color:= clGray;
          Rectangle(x,y, x+CellWidth, y+ CellHeight);
         end;

        // Center the text
        x:= x+ (CellWidth  - TextWidth (pDate)) DIV 2;
        y:= y+ (CellHeight - TextHeight(pDate)) DIV 2;

        // Write with shadow (b)
        Font.Color:= clSilver;
        Font.Style := [];
        cGraphText.DrawTextShadow3DSoft(FCanvas, pDate, x, y, clTextShadow);

        // put color back
        //Font.Color := clBlack; // necessary? no!
      end;
    end;
   end;
end;


{ New procedure to Draw the WeekNo in the Calendar}
procedure TCalendarCanvas.DrawWeeks;
var
   nWeek, nDays: Integer;
   pWeek: string;
   TempRect: Trect;
   d       : TDateTime;
   x,i   : Integer;
   tWeek, nWeeks :Integer;
   rtop  : Integer;
begin
 nDays:=DaysInMonth(FMonth,FYear);
 tWeek:=0;
 i:=0;
 for x:=1 to nDays do
 begin
  d:=EncodeDate(FYear,FMonth,x);
  nWeek:=WeekNo(d);
  if nWeek<>tWeek then
   begin
    g_WeekNumbers[i]:= IntToStr(nWeek);
    i:=i+1;
    tWeek:=nWeek;
   end;
 end;

 nWeeks:=i-1;
 TempRect := CalendarRect;
 TempRect.Left:= TempRect.Left-CellWidth;
 rtop:=TempRect.Top;

 WITH FCanvas DO
  begin
   Font.Style := [];
   Pen.Color := clBlack;
   Font.Color := clTeal;
   for x:=0 to nWeeks do
    begin
     pWeek:= g_WeekNumbers[x];
     With TempRect Do
     begin
      Top := rtop + (CellHeight * (x));
      Bottom := Top +  CellHeight ;
      Right := Left + CellWidth;
     end;
     DrawText( Handle, pWeek, Length( g_WeekNumbers[x] ), TempRect, ( DT_CENTER or DT_VCENTER or DT_TOP or DT_SINGLELINE ) );
    end;
   Font.Color := clBlack;    {Restore}
  end;
end;



procedure TCalendarCanvas.DrawFocusFrame( nIndex: Integer);
type
  ByteSet = set of Byte;
var
  TempRect : TRect;
  pDate  : string;
  setSun : ByteSet;
  setSat : ByteSet;
  pDate1 : string;
  pDate2 : string;
begin
 setSun := [];
 setSat := [];

 // RW: this set is used throughout the rest of the function
 // RW: so german date has to be checked but once
 if not FGermanDate
 then
  begin
   setSun := setSun + [1, 8, 15, 22, 29, 36];
   setSat := setSat + [7, 14, 21, 28, 35, 42];
  end
 else
  begin
   setSun := setSun + [7, 14, 21, 28, 35, 42];
   setSat := setSat + [6, 13, 20, 27, 34, 41];
  end;

 If ( nIndex > 0 ) and ( nIndex < 42 ) then
 begin
 //following line works, but may affect DblClick
 //if nIndex = g_PrevDateIndex then exit;
  If g_PrevDateIndex > 0 Then
  begin
   FCanvas.Font.Color := clBlack;
   if g_PrevDateIndex in setSun
   then FCanvas.Font.Color := ColSunday;
   if g_PrevDateIndex in setSat
   then FCanvas.Font.Color := ColSaturday;

   FCanvas.Font.Style := [];

   pDate1:=g_DateArray[g_PrevDateIndex];
   pDate2:=pDate1;
   delete(pDate1,1,1);
   pDate:= pDate1;

   if CheckHoliday(Holidays, pDate, FMonth) then
    FCanvas.Font.Color := ColHoliday;
   if CheckHoliday(Markdays, pDate, FMonth) then
    FCanvas.Font.Color := ColMarked;

   if pDate2[1]='-'
   then FCanvas.Font.Color:=clBtnShadow;

   FCanvas.Brush.Color := clBtnFace;
   TempRect := GetRectFromIndex(g_PrevDateIndex);
   FCanvas.FillRect(TempRect);
   DrawText( FCanvas.Handle, pDate, (Length( g_DateArray[g_PrevDateIndex])-1), TempRect, ( DT_CENTER or DT_VCENTER or DT_TOP or DT_SINGLELINE ) );
  end;

  {Draw the Date in Bold font}
  FCanvas.Font.Color := clBlack;
  if nIndex in setSun then
   FCanvas.Font.Color := ColSunday;
  if nIndex in setSat then
   FCanvas.Font.Color := ColSaturday;

  FCanvas.Font.Style := [fsBold];

  pDate1:= g_DateArray[nIndex];
  pDate2:= pDate1;
  delete(pDate1,1,1);
  pDate:= pDate1;

  // RW: check for holiday once more
  if CheckHoliday(Holidays, pDate, FMonth)
  then FCanvas.Font.Color := ColHoliday;
  if CheckHoliday(Markdays, pDate, FMonth)
  then FCanvas.Font.Color := ColMarked;

  if pDate2[1]= '-'
  then FCanvas.Font.Color:= clBtnShadow;

  TempRect := GetRectFromIndex(nIndex);
  DrawText( FCanvas.Handle, pDate, (Length( g_DateArray[nIndex])-1 ), TempRect, ( DT_CENTER or DT_VCENTER or DT_TOP or DT_SINGLELINE ) );

  { Draw frame arround current day }
  { Frame date with Shadow }
  FCanvas.Pen.Color := clBtnShadow;      {clGray}
  FCanvas.MoveTo( TempRect.Left, TempRect.Bottom - 1 );
  FCanvas.LineTo( TempRect.Left, TempRect.Top );
  FCanvas.LineTo( TempRect.Right - 1, TempRect.Top );
  { Frame date with Highlight }
  FCanvas.Pen.Color := clBtnHighlight;    {clWhite}
  FCanvas.LineTo( TempRect.Right - 1, TempRect.Bottom - 1 );
  FCanvas.LineTo( TempRect.Left, TempRect.Bottom - 1 );


  { Restore FCanvas settings}
  FCanvas.Pen.Color := clBlack;
  FCanvas.Font.Style := [];
 end;
end;


function TCalendarCanvas.GetRectFromIndex(nIndex : Integer): TRect;  {1}
var

  nWeek : Integer;
  nDay : Integer;
begin
  case nIndex of      //here after compile, losing ones place!
     1..7 :  nWeek := 1;
     8..14:  nWeek := 2;
     15..21: nWeek := 3;
     22..28: nWeek := 4;
     29..35: nWeek := 5;
     36..42: nWeek := 6;
   else
      nWeek := 1;    //if not initialized bloody Syntax checker returns cursor
  end;
  nDay := nIndex - ((nWeek-1) *7);

  Result.Left  := CalendarRect.Left + (CellWidth  * (nDay  -1));
  Result.Top   := CalendarRect.Top  + (CellHeight * (nWeek -1)); //          - (CellHeight DIV 2);
  Result.Bottom:= Result.Top  +  CellHeight;
  Result.Right := Result.Left +  CellWidth;
end;


function TCalendarCanvas.GetIndexFromDate : Integer;
begin
  Result := FDay + GetMonthBegin;
end;


function TCalendarCanvas.GetIndexFromPoint(nLeft : Integer ; nTop : Integer) : Integer;  // Unused
var
  nIndex, nWeek, nDay, iHorizontal, iTopOfCal: Integer;
  TempRect: Trect;
begin
  TempRect := CalendarRect;
  iTopOfCal := TempRect.Top;
  nIndex := -1;
  {Is point in the calendar rectangle?}
  if ( nLeft > TempRect.Left ) and ( nTop > TempRect.Top ) and
      ( nLeft < TempRect.Right ) and ( nTop < TempRect.Bottom ) then
     begin
        iHorizontal := (( nTop - iTopOfCal ) div CellHeight) + 1;
        if iHorizontal <= 0 then iHorizontal := 1; {if its in the CalenRect then its valid}
        nWeek := iHorizontal;
        TempRect.Top := TempRect.Top + ( ( nWeek - 1 ) * CellHeight );
        TempRect.Bottom := TempRect.Top + CellHeight;
        TempRect.Right := TempRect.Left + CellWidth;
        { Determine the day number of the selected date }
        for nDay := 1 to 7 do        {Cycle through the days}
           begin
              nIndex := nDay + ( ( nWeek - 1 ) * 7 );
              if ( nLeft >= TempRect.Left ) and ( nLeft <= TempRect.Right )
              then break
              else
                 begin
                   TempRect.Left := TempRect.Right;
                   TempRect.Right := TempRect.Left + CellWidth;
                 end;
           end;
     end;
  Result := nIndex;
end;















{-------------------------------------------------------------------------------------------------------------
   DATE RELATED FUNCTIONS
-------------------------------------------------------------------------------------------------------------}
procedure TCalendarCanvas.LoadDateArray;
var
  nIndex : Integer;
  nBeginIndex, nEndIndex : Integer;
  { Added BEGIN}
  aYear,aMonth,aDay : word;
  d1,d2,dd1,dd2 : TDateTime;
  dx1,dx2       : Integer;
  { Add-END}
begin
  nBeginIndex := GetMonthBegin;
  nEndIndex := nBeginIndex + DaysInMonth(FMonth, FYear) - 1;
  { Added BEGIN}
  d1:=EncodeDate(FYear,FMonth,1);
  DecodeDate(d1-1,aYear,aMonth,aDay);
  dd1:=WeeksFirstDay(d1);
  d2:=EncodeDate(FYear,FMonth,DaysInMonth(FMonth, FYear));
  dd2:=WeeksLastDay(d2);
  dx1:=trunc(d1-dd1);
  dx2:=trunc(dd2-d2);
  { Add-END}

  for nIndex := 1 to 42 do
  begin
  { Changed BEGIN}

    If ( nIndex < nBeginIndex ) or ( nIndex > nEndIndex )
    then
    begin
     if ( nIndex < nBeginIndex )
     then
     begin
      if FShowWeeks    //showing WeekNo (week of the year)
      then g_DateArray[nIndex] := '-'+IntToStr(aDay-dx1+nIndex)
      else g_DateArray[nIndex] := '   ';
     end
     else
     begin
      if nIndex-nEndIndex<dx2+1
      then
       if FShowWeeks     //showing WeekNo (week of the year)
       then g_DateArray[nIndex] := '-'+IntToStr(nIndex-nEndIndex)
       else g_DateArray[nIndex] := '   '
      else
        g_DateArray[nIndex] := '   ';
     end;
    end
    else
     g_DateArray[nIndex] := '+'+IntToStr((nIndex-nBeginIndex)+1);
  { Change-END}
  end;
end;


function TCalendarCanvas.GetMonthBegin: Integer;
var
  FirstDate: TDateTime;
begin
  FirstDate := EncodeDate( FYear, FMonth, 1 );
  // RW: took me long time to find it: central point to adapt date-format
  Result := rDayOfWeek( FirstDate )
end;


function TCalendarCanvas.DaysInMonth(nMonth, nYear : Integer): Integer;
begin
  Result := DAYS_IN_MONTH[nMonth]; { leap-year Feb is special }
  if ( nMonth = 2 ) and IsLeapYear(nYear) then Inc( Result );
end;


function TCalendarCanvas.IsLeapYear(AYear: Integer): Boolean;
begin
  Result := (AYear mod 4 = 0) and ((AYear mod 100 <> 0) or (AYear mod 400 = 0));
end;


function TCalendarCanvas.SetDate(nDays : Integer): Boolean;
var
  aY, aM, aD: Word;
  PrevDay: Word;
begin
 Result := True;
 try
  {Save current date information}
  g_PrevDateIndex := g_CurrDateIndex;
  DecodeDate(FCalendarDate, g_PrevYear, g_PrevMonth, PrevDay);
  {Change the date and update member variables}
  FCalendarDate := FCalendarDate + nDays;
  DecodeDate(FCalendarDate, aY, aM, aD);
  g_CurrDateIndex := ( aD + GetMonthBegin ) - 1;
  {Reload Date Array & paint ONLY if month or year changed}
  If (aM <> g_PrevMonth) or (aY <> g_PrevYear)Then
   begin
    FMonth := aM;
    FYear := aY;
    LoadDateArray;
   end;
  FDay := aD;
 except
  MessageBeep(MB_ICONEXCLAMATION);
  Result := False;
 end;
end;


Function TCalendarCanvas.ValidDate(aDate: TDateType) : Boolean;
Begin       {is cool as no exception is generated by invalid date}
 ValidDate := True;
  With aDate do
   Begin
    If (aMonth > 12) Or (aMonth < 1) Or (aDay < 1) or (aYear < 1) or (aYear > 9999) then
     Begin
      ValidDate := False;
      Exit;
     End;
    If (aMonth = 2) And IsLeapYear(Integer(aYear))
    then Dec(aDay);
    If aDay > DaysInMonth(aMonth, aYear)
    then ValidDate := False;
   End;
End;


Function TCalendarCanvas.IsValidDate(yy,mm,dd: integer) : Boolean;
var d: TDateType;
begin
 d.aYear:=yy; d.aMonth:=mm; d.aDay:=dd;
 result:=ValidDate(d);
end;


function TCalendarCanvas.WeeksFirstDay(aDate: TDateTime) : TDateTime;
begin
 result:=aDate-(rDayOfWeek(aDate)-1);
end;


function TCalendarCanvas.WeeksLastDay(aDate: TDateTime) : TDateTime;
begin
 result:=aDate+(7-rDayOfWeek(aDate));
end;


procedure TCalendarCanvas.SetCalendarDate(aDate: TDateTime);
var
 aYear, aMonth, aDay: Word;
begin
try
 if FCalendarDate <> aDate then
  begin
   DecodeDate(aDate, aYear, aMonth, aDay);
   FCalendarDate := aDate;
   FYear := Integer(aYear);
   FMonth := Integer(aMonth);
   FDay := Integer(aDay);
   LoadDateArray;
   DateChange;
   Paint;
  end;
except
  MessageBeep(MB_ICONEXCLAMATION);
 end;
end;


procedure TCalendarCanvas.SetMonth(Value: Integer);
var
 mDate : TDateType;
 wValue, aY, aM, aD: Word;
 iDaysInM : word;
begin {no test for new <> old as that would fail at startup}
 if (Value < 1) or (Value > 12) then
  begin                                              {first test}
   MessageBeep(MB_ICONEXCLAMATION);
   Exit;
  end;

 wValue := Word(Value);
 iDaysInM := DaysInMonth(wValue, FYear);
 if iDaysInM < FDay
 then FDay := iDaysInM;

 with mDate do
  begin
   aMonth := wValue;
   aDay := Word(FDay);
   aYear := Word(FYear);
  end;

 if ValidDate(mDate) then  {2nd test}
  begin
   FCalendarDate := EncodeDate(Word(FYear), wValue, Word(FDay));
   DecodeDate( FCalendarDate, aY, aM, aD);
   g_CurrDateIndex := ( aD + GetMonthBegin ) - 1;
   FYear := Integer(aY);
   FMonth := Integer(aM);
   FDay := Integer(aD);
   DateChange;
   LoadDateArray;
   Paint;
  end
 else
   MessageBeep(MB_ICONEXCLAMATION);
end;


procedure TCalendarCanvas.SetDay(Value: Integer);
var
 dDate : TDateType;
 wValue, aY, aM, aD: Word;
begin
 if (Value < 1) or (Value > DaysInMonth(FMonth, FYear)) then
  begin    {first test}
   MessageBeep(MB_ICONEXCLAMATION);
   Exit;
  end;
 wValue := Word(Value);
 with dDate do
  begin
   aMonth := Word(FMonth);
   aDay := wValue;
   aYear := Word(FYear);
  end;
 if ValidDate(dDate) then  {2nd test}
  begin
   FCalendarDate := EncodeDate(Word(FYear), Word(FMonth), Value);
   DecodeDate( FCalendarDate, aY, aM, aD);
   g_CurrDateIndex := ( FDay + GetMonthBegin ) - 1;
   FYear := Integer(aY);
   FMonth := Integer(aM);
   FDay := Integer(aD);
   DateChange;
   LoadDateArray;
   Paint;
  end
 else MessageBeep(MB_ICONEXCLAMATION);
end;


procedure TCalendarCanvas.SetYear(Value: Integer);
var
 yDate : TDateType;
 iDaysInM, wValue, aY, aM, aD: Word;
begin
 if (Value < 1) or (Value > 9999) then
  begin    {first test}
   MessageBeep(MB_ICONEXCLAMATION);
   Exit;
  end;
 wValue := Word(Value);

 iDaysInM := DaysInMonth(FMonth, wValue);
 if iDaysInM < FDay
 then FDay := iDaysInM;

 with yDate do
  begin
   aMonth := Word(FMonth);
   aDay := Word(FDay);
   aYear := wValue;
  end;
 if ValidDate(yDate) then  {2nd test}
  begin
   FCalendarDate := EncodeDate(wValue, Word(FMonth), Word(FDay));
   DecodeDate(FCalendarDate, aY, aM, aD);
   g_CurrDateIndex := ( FDay + GetMonthBegin ) - 1;
   FYear := Integer(aY);
   FMonth := Integer(aM);
   FDay := Integer(aD);
   DateChange;
   LoadDateArray;
   Paint;
  end
 else
   MessageBeep(MB_ICONEXCLAMATION);
end;


procedure TCalendarCanvas.DateChange;
begin
 if Assigned(FOnDateChange)
 then FOnDateChange(Self);
end;


{-Return the Date of the first day in the week of Julian Year}
function TCalendarCanvas.JulDate1stWeek(JD : TDateTime) : TDateTime;
var
  aYear, aMonth, aDay : Word;
  JDate     : TDateTime;
begin
  DecodeDate(JD, aYear, aMonth, aDay);
  JDate := EncodeDate(aYear, 1, 1);

  if FGermanDate then
  begin
   if rDayOfWeek(JDate) in [5,6,7]  //1.1. ist Fr, Sa, So
   then JDate:=JDate+(8-rDayOfWeek(JDate))       //Dann gehört diese Woche noch zum letzten Jahr JDate auf nächsten Montag erhöhen
   else
     if rDayOfWeek(JDate)>1
     then JDate:=JDate-(rDayOfWeek(JDate)-1);      //Sonst JDate auf Montag erniedrigen
  end
 else
  begin
   if rDayOfWeek(JDate) in [6,7,1]
   then           //1.1. ist Fr, Sa, So
     begin
      if rDayOfWeek(JDate)>1
      then JDate:=JDate+(8-rDayOfWeek(JDate));    //Dann gehört diese Woche      //noch zum letzten Jahr
     end                                            //JDate auf nächsten Montag erhöhen
   else
     JDate:=JDate-(rDayOfWeek(JDate)-1);           //Sonst JDate auf Montag erniedrigen
  end;
  Result := JDate
end;


function TCalendarCanvas.WeekNo(JDate : TDateTime) : Integer;
var
  W1,W2,W3    : TDatetime;
  d1,d2,d3    : TDatetime;
  yy,mm,dd    : word;
begin
  W1:=JulDate1stWeek(JDate);
  W2:=W1;
  W3:=W1; //Vorbelegen
  DecodeDate(JDate, yy, mm, dd);  // voriges Jahr
  if yy>0 then
  begin
   d2:=EncodeDate(yy-1, 12, 31);
   W2:=JulDate1stWeek(d2);
  end;

  if yy< 9999 then
  begin
   d3:=EncodeDate(yy+1, 1, 1);     // nächstes Jahr
   W3:=JulDate1stWeek(d3);
  end;

  if W3<=JDate then              //Woche gehört schon zum nächsten Jahr
  begin
   d1:=JDate-W3;
   Result := (trunc(d1)div 7)+1;
   Exit;
  end;

  if W1>JDate then              //Woche gehört noch zum vorigem Jahr
  begin
   d1:=JDate-W2;
   Result := (trunc(d1)div 7)+1;
   Exit;
  end;

  d1:=JDate-W1;                 //Normalfall
  Result := (trunc(d1)div 7)+1;
end;


function TCalendarCanvas.GetWeekNumber: Integer;
begin
 Result := WeekNo(EncodeDate(FYear, FMonth, FDay));
end;


function CalculateDayOfYear(y, m, d : Word) : Integer;   { DayOfYear }
VAR
   yy, mm, dd, Tmp1 : LongInt;
begin
  yy := y;
  mm := m;
  dd := d;
  Tmp1 := (mm + 10) div 13;
  result :=  3055 * (mm + 2) div 100 - Tmp1 * 2 - 91 +
                  (1 - (yy - yy div 4 * 4 + 3) div 4 +
                  (yy - yy div 100 * 100 + 99) div 100 -
                  (yy - yy div 400 * 400 + 399) div 400) * Tmp1 + dd
end;


function TCalendarCanvas.GetDayOfYear: Integer;
begin
 result := CalculateDayOfYear(FYear, FMonth, FDay);
end;


function TCalendarCanvas.GetDaysInYear: integer;
begin
 If IsLeapYear(FYear)
 then Result := 366
 else result := 365;
end;


// Toggles start of the week (Sunday or Monday)
procedure TCalendarCanvas.SetGermanDate(Value: Boolean);
begin
  if Value <> FGermanDate then
  begin
    FGermanDate := Value;
    LoadDateArray;
    //Paint;
  end;
end;

procedure TCalendarCanvas.SetShowWeeks(Value: Boolean);   //showing WeekNo (week of the year)
begin
  if Value <> FShowWeeks then
  begin
   FShowWeeks := Value;
   LoadDateArray;                   
   //Paint;
  end;
end;


// Corrected built-in-function to fit german date
function TCalendarCanvas.rDayOfWeek(vDate: TDateTime) : Integer;
begin
  Result := DayOfWeek(vDate);
  if FGermanDate then
  begin
     Result := Result - 1;            // Sonntag abziehen / subtract Sunday
     if Result = 0
     then Result := 7;                // Fehler ausgleichen / error correction
  end;
end;


// functions to set color values
procedure TCalendarCanvas.SetColHoliday(Value: TColor);
begin
  if Value <> FColHoliday then
  begin
     FColHoliday := Value;
     Paint;
  end;
end;


procedure TCalendarCanvas.SetColSunday(Value: TColor);
begin
  if Value <> FColSunday then
  begin
     FColSunday := Value;
     Paint;
  end;
end;


procedure TCalendarCanvas.SetColSaturday(Value: TColor);
begin
  if Value <> FColSaturday then
  begin
     FColSaturday := Value;
     Paint;
  end;
end;


procedure TCalendarCanvas.SetColMarked(Value: TColor);
begin
  if Value <> FColMarked then
  begin
     FColMarked := Value;
     Paint;
  end;
end;


// build a string list for Holidays
procedure TCalendarCanvas.SetHolidays(Value: TStrings);
begin
  Holidays.Assign (Value);
end;


// build a string list for special days
procedure TCalendarCanvas.SetMarkdays(Value: TStrings);
begin
  Markdays.Assign (Value);
end;


procedure TCalendarCanvas.SetFont(const Value: TFont);
begin
  FCanvas.Font:= Value;
end;


function TCalendarCanvas.GetFont: TFont;
begin
  Result:= FCanvas.Font;
end;


// this function compares a given day and month with the strings of a stringlist to find out about holidays and special days
function TCalendarCanvas.CheckHoliday(DateList: TStrings; sd: string; m: integer) : Boolean;
var
  i, z: integer;
  scmp, sm: string;
begin
  // Determine number of list entries
  z := Datelist.Count - 1;
  Result := False;
  scmp := '';
  if (Datelist.Count > 0) and (sd <> ' ') and (m > 0) then
   begin
     // Create compare string
     //Str(m, sm);
     sm:= IntToStr(m);

     if FGermanDate
     then scmp:= sd + '.' + sm + '.'
     else scmp:= sm + '/' + sd + '/';

     // Step through the list and compare all entries
     for i := 0 to z do
      if scmp = Datelist.Strings[i] then
       begin
         Result := True;
         Break;
       end;
   end;
end;



end.
