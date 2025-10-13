UNIT LightVcl.Visual.ThumbViewerM;                                                                            {Multithreaded}

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
  v4 MultiThreaded

  Features:
    Create thumbnails in a separate thread
    Realtime loading - Show thumbs as soon as they are generated
    Customizable cell spacing
    Load BMPs at original size
    Customizable thumb size

  How to use it:
    The user needs to assign a progressbar to it.
    Use LoadFolders to load all images in a folder. This will Clear the content first.
    Use AddFile to load additional images.
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, System.SysUtils, Winapi.Messages, System.Classes, Vcl.Graphics, Vcl.Grids, Vcl.ComCtrls, Vcl.Controls,
  LightVcl.Graph.Loader.Thread, LightVcl.Common.Colors, LightVcl.Graph.Bitmap, LightVcl.Graph.Resize, LightVcl.Graph.ResizeVCL, LightVcl.Graph.ResizeParams;

TYPE
  ThumbPtr = ^TThumb;                                                                              { The record also contains the BMP thumb }
  TThumb   = record
   FileName   : string;
   BMP        : TBitMap;
   //BkgColor   : TColor;                                                                          {ToDo: Implement background color - Highlight images with resolution smaller than threshold in gray and higher than threshold in green }
  END;


  TThumbList= class(TList)                                                                         { A list of Thumb records. Each record contains a filename and a bitmap }
   private
   protected
    procedure setRecord(index: Integer; Pointer: ThumbPtr);
    function  getRecord(index: Integer): ThumbPtr;
    procedure ReleaseBitmap(const Index: Integer);                                                 { UNUSED }{ Release the cache at specified position }
   public
    destructor Destroy;        override;
    procedure  Clear;          override;
    function  GetRamSize (Phumb: ThumbPtr): Integer;                                               { Returns memory requirements fpr specified record }
    function  GetTotalRamSize: Integer;
    property  Phumbs[Index: Integer]: ThumbPtr read GetRecord write SetRecord; default;
  end;


  TCubicThumbs= class(TStringGrid){TBaseStrGrid}
  private
    FThumbReady: TNotifyEvent;
    FBkgThread: TBkgImgLoader;                                                                     { Worker thread }
    ThreadProgress: Integer;                                                                       { I increment this var every time the working thread finishes resizing an image }
   protected
    procedure Resize;        override;
    procedure DrawCell(aCol, aRow: Longint; aRect: TRect; aState: TGridDrawState);  override;
    procedure TopLeftChanged;override;
    procedure CreateWnd;     override;
    function  getActiveItem: Integer;                                                              { Unused }{ Retuns the Index of the seleted thumb }
    procedure setActiveItem(CONST Index: Integer);                                                 { Unused }
  public
    ThumbList   : TThumbList;
    ThumbWidth  : Integer;
    ThumbHeight : Integer;
    CellSpacing : Integer;
    ResizeOpp   : TResizeOp;
    Progress    : TProgressBar;
    {INIT}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;                     override;
    procedure Clear;

    {METRICS}
    procedure SetCellSize;
    procedure SetThumbWidth  (CONST Width : integer);
    procedure SetThumbHeight (CONST Height: integer);
    procedure SetCellSpacing (CONST Value: integer);
    procedure ComputeCellCount;
    function  NecessaryRows: Integer;                                                              { Number of rows necessary to display all images }

    {ACESS}
    function  HasThumb       (CONST Index: Integer): Boolean;                                      { UNUSED }
    function  Thumb          (CONST Index: Integer): TThumb;                                       { UNUSED }
    function  GetPhumb       (CONST ACol, ARow: Integer): ThumbPtr;
    function  GetSelectedPhumb: ThumbPtr;                                                          { Get phumb for selected cell }
    function  SelectedFile: string;
    function  Cells2ListIndex(CONST ACol, ARow: Integer): Integer;
    procedure ListIndex2Cells(Index: Integer; OUT ACol, ARow: Integer);

    {LOAD}
    procedure LoadFolder     (CONST Dir: string);
    procedure AddPicture     (CONST FilePath: string);                                             { Renamed from LoadFromFile } { Append a single thumbnail at the end of an existing list of thumbnails }
    procedure AssignThumbsToCells;                                                                 { Refresh Grid }
    procedure WMThumbnailReady(var AMessage: TMessage); message WM_THUMBNAIL_NOTIFY;               { Receive message from worker thread }
    function  Statistics: string;
  published
    property OnThumbReady: TNotifyEvent read FThumbReady   write FThumbReady;
    property ActiveCell  : Integer      read getActiveItem write setActiveItem;                    { Unused }
  end;


procedure Register;


IMPLEMENTATION
{$WARN GARBAGE OFF}                                                                                {Silence the: 'W1011 Text after final END' warning }

USES LightVcl.Graph.Util, LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.System, LightVcl.Common.SystemTime, LightVcl.Common.Clipboard, LightCore.Math, LightCore.IO;







constructor TCubicThumbs.Create(AOwner: TComponent);
CONST TextHeight= 13;                                                                              { Picture's caption text height. Cannot access the canvas here so I use a constant }
begin
 inherited Create(AOwner);                                                                         // Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
 Name:= 'CubicThumbs';
 Constraints.MinWidth := 60;
 Constraints.MinHeight:= 40;
 ThumbWidth    := 256;
 ThumbHeight   := round(ThumbWidth / 1.777) + TextHeight;                                          { 16:9 ratio = 1.7.  }
 CellSpacing   := 4;
 //ResamplerQual := rsGoodFilter;
 ResizeOpp     := roAutoDetect;
 ThumbList     := TThumbList.Create;
 Clear;
end;



procedure TCubicThumbs.CreateWnd;                                                                  { Note: this is called multiple times (for example on Resize) }
{ About constructors and CreateWnd:
      http://edn.embarcadero.com/article/20569
      http://stackoverflow.com/questions/582903/whats-the-difference-between-Creatend-and-Createindowhandle}
begin
 inherited CreateWnd;
 SetCellSize;
 //CreateWnd can be called more than once:  http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html
end;



destructor TCubicThumbs.Destroy;
begin
 FreeAndNil(ThumbList);
 FreeAndNil(FBkgThread);
 inherited
end;


procedure TCubicThumbs.Clear;
begin
 ThumbList.Clear;                                                                                  { Free existing objects }
 ThreadProgress:= 0;
 TopRow:= 0;                                                                                       { Reset scrollbar }  { Necessary to make sure that the next line won't scroll the Self to the first line. This is futile since we will clear anyway the grid }
 RowCount:= 1;
 ColCount:= 1;
end;




{--------------------------------------------------------------------------------------------------
   IMPORT FILES
--------------------------------------------------------------------------------------------------}
procedure TCubicThumbs.LoadFolder(CONST Dir: string);
VAR  FolderContent: TStringList;                                                                   { Freed by: FBkgThread }
     FileName: string;
     PThumb: ThumbPtr;
begin
 { CHECKS }
 if NOT DirectoryExists(Dir)
 then raise exception.Create('Path does not exist.');

 if Progress= NIL
 then raise exception.Create('The thumb viewer needs a progress bar');

 { Discover files }
 FolderContent:= ListFilesOf(Dir, AllImg, TRUE, NOT DigSubdirectories);                            { Create records for all files in a folder. The thumbs ARE NOT yet created/ They are created on demand. }     { Freed by: FBkgThread }

 { Add rec to list }
 for FileName in FolderContent DO
   begin
    New(PThumb);
    PThumb^.FileName:= FileName;
    PThumb^.BMP:= NIL;                                                                              { The thumbs ARE NOT yet created/ They are created on demand. }
    ThumbList.Add(PThumb);
   end;

 { Recompute cells }
 ComputeCellCount;

 { Progress }
 Progress.Position:= 0;
 Progress.Max:= FolderContent.Count;

 { Release previous worker (if any) }
 if Assigned(FBkgThread) then
  begin
   FBkgThread.Terminate;
   FreeAndNil(FBkgThread);
  end;

 { START WORKER THREAD }
 FBkgThread            := TBkgImgLoader.Create(Handle);
 FBkgThread.Priority   := tpLower;                                                                 { Set the priority to lower than normal }
 FBkgThread.FileList   := FolderContent;
 FBkgThread.Height:= ThumbHeight;
 FBkgThread.Width := ThumbWidth;
 //FBkgThread.ResamplerQual:= ResamplerQual;
 FBkgThread.Start;

 { Generate thumbnails }
 AssignThumbsToCells;
end;



procedure TCubicThumbs.AddPicture(CONST FilePath: string);                                         { Append a single thumbnail at the end of an existing list of thumbnails. Not multithreaded }
VAR PThumb: ThumbPtr;
begin
 if (ThumbWidth <= 0) OR (ThumbHeight<= 0)
 then RAISE exception.Create('Invalid thumb size!');                                               { DEL }

 {DEL
 if LoadOrigSize
 then Result:= cGraphics.LoadGraphEx(FileName)
 else Result:= cGraphics.LoadGraphRsmplProp(FileName, ThumbWidth, ThumbHeight, ResamplerQual); }

 { Add rec to list }
 New(PThumb);
 PThumb^.FileName:= FilePath;
 ThumbList.Add(PThumb);

 { Generate thumbnail }
 PThumb^.BMP:= LoadAndStretch(FilePath, ThumbWidth, ThumbHeight);

 { Increment progress }
 Inc(ThreadProgress);

 ComputeCellCount;                                                                                 { Recompute cells }
 AssignThumbsToCells;                                                                              { Show thumbnail }

 { Event }
 if Assigned(FThumbReady)                                                                          { A new thumbnail was generated for an image (by the worker thread) }
 then FThumbReady(Self);
end;




{--------------------------------------------------------------------------------------------------
   WORKER THREAD
--------------------------------------------------------------------------------------------------}

procedure TCubicThumbs.WMThumbnailReady(VAR AMessage: TMessage);                                   { Receive notifications from worker thread each time a thumbnail is ready }
VAR BMP: TBitmap;
    ACol, ARow: Integer;
begin
 BMP:= FBkgThread.PopPicture;
 ThumbList.Phumbs[ThreadProgress]^.BMP:= BMP;

 { Refresh cell for this new thumbnail }
 ListIndex2Cells(ThreadProgress, ACol, ARow);
 InvalidateCell(ACol, ARow);

 { Increment progress }
 Inc(ThreadProgress);

 Progress.Hint:= IntToStr(Progress.Position+1) + ' of ' + IntToStr(Progress.Max);
 Progress.StepIt;
 Progress.Update;

 { Event }
 if Assigned(FThumbReady)                                                                          { A new thumbnail was generated for an image (by the worker thread) }
 then FThumbReady(Self);
end;






{--------------------------------------------------------------------------------------------------
   ASSIGN THUMBS
--------------------------------------------------------------------------------------------------}
procedure TCubicThumbs.AssignThumbsToCells;                                                        { Refresh images in Grid }
VAR
   Phumb: ThumbPtr;
   CurCell, ACol, ARow: Integer;
begin
 LightVcl.Common.System.CursorBusy;
 TRY
   for CurCell:= 0 to (RowCount* ColCount) -1 DO                                                    { For all grid cells }
    begin
     ListIndex2Cells(CurCell, ACol, ARow);                                                          { Translate scrollbar pos to grid cells }

     if CurCell< ThumbList.Count
     then
       begin
         { Assign record to grid's cell }
         Phumb:= ThumbList.Phumbs[CurCell];
         Objects[ACol, ARow]:= Pointer(Phumb);                                                        { Documentation: http://borland.newsgroups.archived.at/public.delphi.language.delphi.win32/200703/0703207671.html }
       end
     else
       Objects[ACol, ARow]:= NIL;                                                                   { I'm at the end of the grid. I don't have an image to assign to this cell }
    end;

 FINALLY
   CursorNotBusy;
 END;
end;







{--------------------------------------------------------------------------------------------------
   DRAW THUMBS
--------------------------------------------------------------------------------------------------}
procedure TCubicThumbs.DrawCell(ACol, ARow: Longint; aRect: TRect; aState: TGridDrawState);
{HOW TO CENTER TEXT: http://stackoverflow.com/questions/6617058/delphi-canvas-fillrect-in-list-view }
VAR
   X, Y, TextWidth, TextHeight: Integer;
   Phumb: ThumbPtr;
   BMP: TBitmap;
   FName: string;
   TmpRect: TRect;
begin
 inherited;
 if ThumbList.Count= 0 then EXIT;
 if (ACol>= ColCount) OR (ARow>= RowCount) then EXIT;                                              { This happens while the user resizes the form. The aCol parameter may be for example 10 but meantime the user shrinked the grid and now there are only 5 columns so aCol is invalid. }

 Assert(ACol>= 0); {DEL}
 Assert(ARow>= 0); {DEL}

 Phumb:= GetPhumb(ACol, ARow);
 if Phumb= NIL then EXIT;                                                                          { This is necessary when the program starts for the first time and the records are empty }

 { Compute text metrics }
 FName:= ExtractOnlyName(Phumb^.FileName);
 TextWidth := Canvas.TextWidth(FName);
 TextHeight:= Canvas.TextHeight('Tp');

 { Draw thumb }
 BMP:= Phumb^.BMP;
 if BMP= NIL
 then                                                                                              { Make sure I have a thumbnail }
  begin
   TmpRect.Top:= aRect.Top+ 6;
   TmpRect.Left:= aRect.Left+ 6;
   TmpRect.Right:= aRect.Right- 6;
   TmpRect.Bottom:= aRect.Bottom- 6;

   Canvas.Brush.Color := clRedDark;
   Canvas.Brush.Style := bsFDiagonal;
   SetBkColor(Canvas.Handle, clWhite);                                                             { Use this after Brush.Style!  }

   Canvas.FillRect(TmpRect);

   { Restore colors }
   Canvas.Brush.Color:= clWhite;
   Canvas.Brush.Style:= bsSolid;
  end
 else
  begin
   x:= aRect.Left+  (DefaultColWidth  - BMP.Width)  DIV 2;
   y:= aRect.Top + ((DefaultRowHeight - BMP.Height- TextHeight) DIV 2);
   Canvas.Draw(x, y, BMP);
  end;

 { Draw caption }
 if TextWidth< DefaultColWidth
 then X:= aRect.Left+ (DefaultColWidth - TextWidth) DIV 2
 else X:= aRect.Left+ 6;                                                                           { The text is longer than cell's width }
 aRect.Top:= aRect.Bottom - TextHeight -1;                                                         { Default text height is 13 }
 Canvas.TextRect(aRect, x, aRect.Top, FName);
end;








{--------------------------------------------------------------------------------------------------
   Metrics
--------------------------------------------------------------------------------------------------}
(* I GET NASTY 'A call to an OS function failed.' ERROR IF I UNCOMMENT THIS.
Probalby I access the property too early, during the creation of the control!
procedure TCubicThumbs.ComputeScrollBar;                                                           { Call it AFTER ComputeCellCount }
begin
   if NecessaryRows - RowCount> 0                                                                  { Note: scrollbar is indexed in 0.  RowCount represents the last (unscrollable) rows }
   then ScrollBars:= ssVertical
   else ScrollBars:= ssnone;
end; *)


procedure TCubicThumbs.ComputeCellCount;
begin
 //Assert(ThumbList.Count> 0);    del

 ColCount:= Trunc (ClientWidth / (DefaultColWidth + 1));                                           { Thumbs per line. 1 is the width of the line that separates the cells }
 RowCount:= NecessaryRows;
 //ComputeScrollBar;                                                                                 { Call it AFTER ComputeCellCount }
end;


function TCubicThumbs.NecessaryRows: Integer;                                                      { Retunrs the number of rows necessary to display all thumbnails }
begin
 if ColCount= 0                                                                                    { Prevent 'Division by zero'. Happens when the grid is small small tiny }
 then Result:= 0
 else Result:= RoundUp(ThumbList.Count / ColCount);                                                { Number of rows necessary to display all images }
end;


procedure TCubicThumbs.SetCellSize;
begin
 if csCreating in ControlState then EXIT;

 DefaultColWidth := ThumbWidth + (CellSpacing* 2);
 DefaultRowHeight:= ThumbHeight+ (CellSpacing* 2) + Canvas.TextHeight('Tp');
end;








{--------------------------------------------------------------------------------------------------
   USER EVENT - SCROLLBAR CHANGED
--------------------------------------------------------------------------------------------------}
procedure TCubicThumbs.TopLeftChanged;
begin
 inherited;
 {TODO: Make it like this: generate thumbnails only for visible cells (and also few rows down as buffer). The thumb are generated only when the user scrolls down/up. Start a number of threads equal with the number of cores-1

  MaxThreads:= CPUCores-1;
  for i:= VisibleCells do
   if ThreadsBussy = MaxThreads                       all threads are bussy?
   then exit
   else
     for iThread:= 0 to MaxThreads do
       StartThread(FileName);


  When a thread ends the current job it asks the grid if it has more work to be done:

  Procedure OnThreadExit;
  begin
    if Grid.NeedsThumbs then StartThread(Grid.GetFile);
  end;
 }
end;


{--------------------------------------------------------------------------------------------------
   USER EVENT - FORM RESIZE
--------------------------------------------------------------------------------------------------}
procedure TCubicThumbs.Resize;                                                                     { http://stackoverflow.com/questions/14727765/how-do-i-respond-to-a-resize-event-in-my-custom-grid-control }
begin
 inherited Resize;
 if (csCreating in ControlState) OR (ThumbList= NIL) then EXIT;                                    { Design time / No image loaded }

 if ThumbList.Count> 0 then
  begin
   ComputeCellCount;
   AssignThumbsToCells;
  end;
end;


procedure TCubicThumbs.SetThumbWidth(CONST Width: integer);
begin
 ThumbWidth:= Width;
 SetCellSize;
 ComputeCellCount;

 if ThumbList.Count<> 0
 then AssignThumbsToCells;                                                                         { Update Grid }
end;


procedure TCubicThumbs.SetThumbHeight(CONST Height: integer);
begin
 ThumbHeight:= Height;
 SetCellSize;
 ComputeCellCount;

 if ThumbList.Count<> 0
 then AssignThumbsToCells;                                                                         { Update Grid }
end;


procedure TCubicThumbs.SetCellSpacing(CONST Value: integer);
begin
 CellSpacing:= Value;
 SetCellSize;
 ComputeCellCount;

 if ThumbList.Count<> 0
 then AssignThumbsToCells;                                                                         { Update Grid }
end;






{--------------------------------------------------------------------------------------------------
   ITEM INDEX
--------------------------------------------------------------------------------------------------}
function TCubicThumbs.getActiveItem: Integer;                                                      { Retuns the Index of the seleted thumb }
begin
 Result:= Cells2ListIndex(Col, Row);
end;

procedure TCubicThumbs.setActiveItem(CONST Index: Integer);
VAR myRect: TGridRect;
    ACol, ARow: Integer;
begin
 ListIndex2Cells(Index, ACol, ARow);

 myRect.Left  := ACol;
 myRect.Top   := ARow;
 myRect.Right := ACol;
 myRect.Bottom:= ARow;
 Selection := myRect;
end;



function TCubicThumbs.SelectedFile: string;
begin
 if GetSelectedPhumb<> NIL
 then Result:= GetSelectedPhumb^.FileName
 else Result:= '';
end;




{--------------------------------------------------------------------------------------------------
   INDEX 2 CELL CONVERSIONS
--------------------------------------------------------------------------------------------------}
function TCubicThumbs.Cells2ListIndex(CONST ACol, ARow: Integer): Integer;                         { to be verified }
begin
 Result:= (ARow * ColCount) + ACol;
end;


procedure TCubicThumbs.ListIndex2Cells(Index: Integer; OUT ACol, ARow: Integer);
begin
 ARow:= System.Trunc(Index / ColCount);
 ACol:= Index mod ColCount;
end;




{--------------------------------------------------------------------------------------------------
   THUMB ACCESS
--------------------------------------------------------------------------------------------------}

function TCubicThumbs.HasThumb(CONST Index: Integer): boolean;                                     { UNUSED }
begin
 Assert(Index>= 0);
 Assert(Index< ThumbList.Count);
 Result:= ThumbList.Phumbs[Index]^.BMP <> NIL;
end;


function TCubicThumbs.Thumb(CONST Index: Integer): TThumb;
begin
 Assert(Index>= 0);
 Assert(Index< ThumbList.Count);

 Result:= ThumbList.Phumbs[Index]^;
end;


function TCubicThumbs.GetPhumb(CONST ACol, ARow: Integer): ThumbPtr;                               { Get phumb for specified cell }
begin
 Result:= ThumbPtr (Objects[ACol, ARow]);
end;


function TCubicThumbs.GetSelectedPhumb: ThumbPtr;                                                  { Get phumb for selected cell }
begin
 Result:= GetPhumb(Selection.Left, Selection.Top);
end;











function TCubicThumbs.Statistics: string;
begin
 Result:=
   'Necessary rows: ' + IntToStr(NecessaryRows)                         + CRLFw+                         { Number of rows necessary to display all images }
   'ColCount: '       + IntToStr(ColCount)                              + CRLFw+
   'RowCount: '       + IntToStr(RowCount)                              + CRLFw+
   'Current RAM: '    + FormatBytes(ThumbList.GetTotalRamSize, 2)    + CRLFw+
   'ThumbWidth: '     + IntToStr(ThumbWidth)      + CRLFw+
   'ThumbHeight: '    + IntToStr(ThumbHeight)     + CRLFw+
   'CellSpacing: '    + IntToStr(CellSpacing)     + CRLFw+
   'TextHeight: '     + IntToStr(Canvas.TextHeight('Tp'))      + CRLFw;
end;


procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TCubicThumbs]);
end;















{--------------------------------------------------------------------------------------------------
   TThumbList CREATE
--------------------------------------------------------------------------------------------------}
destructor TThumbList.Destroy;
begin
 Clear;                                                                                            { clear the list to dispose of any pointers still stored first }
 inherited Destroy;
end;


procedure TThumbList.Clear;
VAR
   i: Integer;
   Phumb: ThumbPtr;
begin
 for i:= 0 to Pred(Count) DO                                                                       { dispose of the memory pointed to by all pointers in the list that are not Nil }
  begin
   Phumb:= Phumbs[i];
   Assert(Phumb <> NIL);
   FreeAndNil(Phumb^.BMP);
   Dispose(Phumb);
  end;

 inherited Clear;                                                                                  { call the Clear method inherited from TList to set Count to 0 }
end;




{--------------------------------------------------------------------------------------------------
   ITEM ACCESS
--------------------------------------------------------------------------------------------------}
function TThumbList.GetRecord(Index: Integer): ThumbPtr;
begin
 Result:= ThumbPtr(Items[index]);                                                                  { return the pointer in slot index, typecast to PRecord }
end;


procedure TThumbList.SetRecord(Index: Integer; Pointer: ThumbPtr);                                 { http://users.atw.hu/delphicikk/listaz.php?id=2207&oldal=1 }
VAR aThumb: ThumbPtr;
begin
 aThumb:= Phumbs[index];                                                                            { get the pointer currently in slot index }
 if aThumb <> Pointer then
  begin
    if aThumb <> NIL
    then Dispose(aThumb);                                                                           { if it is different from the one we are asked to put into this slot, check if it is <> Nil. If so, dispose of the memory it points at! }
    Items[index] := Pointer;                                                                       { store the passed pointer into the slot }
  end;
end;


procedure TThumbList.ReleaseBitmap(const Index: Integer);                                          { UNUSED } { Release the cache at specified position }
begin
 FreeAndNil(Phumbs[Index]^.BMP);
end;




{--------------------------------------------------------------------------------------------------
   RAM INFO
--------------------------------------------------------------------------------------------------}
function TThumbList.GetRamSize(Phumb: ThumbPtr): Integer;                                          { Returns memory requirements for specified record }
begin
 { Add record size }
 Result:= SizeOf(TThumb);                                                                          {INFO: The SizeOf function returns the storage size, in bytes, of either a Variable or Type. }

 { Add strings for filenames }
 Result:= Result+ GetStringRAMSize(Phumb^.FileName);

 { Add bitmaps }
 if Phumb^.BMP<> NIL
 then Result:= Result+ Integer(GetBitmapRamSize(Phumb^.BMP));
end;


function TThumbList.GetTotalRamSize: Integer;                                                      { Returns memory requirements. May be a bit slow size I load each image into a string! }
VAR Phumb: ThumbPtr;
begin
 Result:= 0;
 for Phumb in Self
  DO Result:= Result+ GetRamSize(Phumb);
end;





end.{----------------------------------------------------------------------------------------------







procedure TCubicThumbs.GenerateThumbs(CONST StartFrom: Integer);                                   { Generates an entire row of thumbnails, starting from position ListIndex }
VAR Index, EndAt: Integer;
begin
 EndAt:= StartFrom + ColCount;
 if EndAt >= ThumbList.Count
 then EndAt:= ThumbList.Count-1;

 for Index:= StartFrom to EndAt DO
   if NOT HasThumb(Index)
   then ThumbList.Phumbs[Index]^.BMP:= GenerateThumbnail(Thumb(Index).FileName, ThumbWidth, ThumbHeight);
end;


