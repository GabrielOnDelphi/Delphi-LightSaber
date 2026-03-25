UNIT LightVcl.Visual.PathEdit;

{=============================================================================================================
   www.GabrielMoraru.com
   2026.03.21
--------------------------------------------------------------------------------------------------------------

  TlightPathEdit - An edit box that allows user to choose/enter a file or folder path.

  Features:
     OnPressEnter  - Event is triggered when the user pressed Enter
     OnApply       - Event is triggered when the user pressed Apply btn
     InputType     - Type of input that the control accepts: File and Folder
     Path          - Path as entered by user
     FileListBox   - Can automatically update the linked FileListBox to navigate to the specified folder

     Backspace     - Lets user go one folder up by pressing a shortcut
     CheckCanWrite - If true, the control will verify if it has write access to the folder selected by the user.


  Tester:
     c:\Projects\Testers\Cubic VCL LightVcl.Visual.PathEdit\LightVcl.Visual.PathEdit_Tester.dpr

  See: What's the difference between CreateWnd and CreateWindowHandle?
       https://stackoverflow.com/questions/582903/whats-the-difference-between-createwnd-and-createwindowhandle
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  Winapi.Windows,
  System.SysUtils, System.Classes, System.IOUtils,
  Vcl.StdCtrls, Vcl.Controls, Vcl.Graphics, Vcl.ImgList, Vcl.CONSTs, Vcl.FileCtrl, Vcl.ExtCtrls,
  LightCore, LightVcl.Common.Dialogs;

TYPE
  TValidity= (vaNone, vaValid, vaInvalid);                   { Normal / Green / Red color }
  TInputType= (itFile, itFolder);                            { What kind of path will the user type in this control: folder or file }

  TlightPathEdit = class(TCustomGroupBox)
   private
     Initialized     : Boolean;
     btnApply        : TButton;
     btnCreate       : TButton;
     btnExplore      : TButton;
     edtPath         : TButtonedEdit;
     FDirListBox     : TDirectoryListBox;
     imgList         : TImageList;
     FFileList       : TFileListBox;
     FInputType      : TInputType;
     FFileFilter     : string;
     FOpenDlgTitle   : string;
     FOpenSrc        : Boolean;
     FShowApply      : Boolean;
     FShowCreate     : Boolean;
     FCheckCanWrite  : Boolean;
     FOnApply        : TNotifyEvent;
     FOnCreateFolder : TNotifyEvent;
     FOnPathChanged  : TNotifyEvent;
     FOnPressEnter   : TNotifyEvent;
     procedure btnCreateClick  (Sender: TObject);
     procedure btnBrowseClick  (Sender: TObject);
     procedure btnExploreClick (Sender: TObject);
     procedure btnApplyClick   (Sender: TObject);
     procedure edtPathChange   (Sender: TObject);  //override;
     procedure edtKeyPress     (Sender: TObject; var Key: Char);
     procedure edtPathOnClick (Sender: TObject);
     procedure edtKeyDown    (Sender: TObject; var Key: Word; Shift: TShiftState);
     procedure setFileList   (const Value: TFileListBox);
     procedure setDirListBox (const Value: TDirectoryListBox);
     function  canWriteToFolder(const Folder: string): Boolean;
     procedure setPath         (const Value: string);
     procedure setInputType    (const Value: TInputType);
     procedure setOpenSrc      (const Value: Boolean);
     procedure setShowCreate   (const Value: Boolean);
     procedure setReadOnly     (const Value: Boolean);
     function  getPath: string;
     function  isFilterStored: Boolean;
     function  getReadOnly: Boolean;
     procedure inputTypeChanged;
     procedure setShowApply(const Value: Boolean);
   protected
     procedure PathChanged;
     procedure CheckPathValidity;           { Makes the control green when the path exists and red when it doesn't exist }
     procedure Click; override;
     procedure CreateWnd; override;
     procedure SetEnabled(Value: Boolean); override;
     procedure Notification(AComponent: TComponent; Operation: TOperation); override;
     procedure NavigateUp;
   public
     constructor Create(aOwner: TComponent);  override;
     function  PathHasValidChars: string;   { Returns '' when the input string (path) is valid and and error msg when it is not valid }
     function  PathIsValid: string;         { Returns '' when the input string (path) is valid and and error msg when it is not valid }
     function  PathIsValidMsg: boolean;     { Returns true when the input string (path) is valid and and error msg when it is not valid }

     function  GetFiles(CONST FileType: string; CONST ReturnFullPath, DigSubdirectories: Boolean; ExcludeFolders: TStrings= nil): TStringList;
   published
     property FileListBox   : TFileListBox read FFileList     write setFileList;
     property Directory     : TDirectoryListBox  read FDirListBox   write setDirListBox;
     property Path          : string       read getPath       write setPath;
     property FileFilter    : string       read FFileFilter   write FFileFilter     stored IsFilterStored;      { Has meaninng ONLY when InputType is 'itFile' }
     property OpenDlgTitle  : string       read FOpenDlgTitle write FOpenDlgTitle;
     property IsReadOnly    : Boolean      read getReadOnly   write setReadOnly     default FALSE;
     property InputType     : TInputType   read FInputType    write setInputType    default itFolder;

     property ShowCreateBtn : Boolean      read FShowCreate   write setShowCreate   default TRUE;
     property ShowOpenSrc   : Boolean      read FOpenSrc      write setOpenSrc      default TRUE;
     property ShowApplyBtn  : Boolean      read FShowApply    write setShowApply    default FALSE;
     property CheckCanWrite : Boolean      read FCheckCanWrite write FCheckCanWrite   default FALSE;

     {EVENTS}
     property OnPressEnter  : TNotifyEvent read FOnPressEnter   write FOnPressEnter;
     property OnPathChanged : TNotifyEvent read FOnPathChanged  write FOnPathChanged;
     property OnCreateFolder: TNotifyEvent read FOnCreateFolder write FOnCreateFolder;
     property OnApplyButton : TNotifyEvent read FOnApply        write FOnApply;

     property Align;
     property Anchors;
     property BiDiMode;
     property Caption;
     property Color;
     property Constraints;
     property Ctl3D;
     property DockSite;
     property DoubleBuffered;
     property DragCursor;
     property DragKind;
     property DragMode;
     property Enabled;
     property Font;
     property Padding;
     property ParentBackground default True;
     property ParentBiDiMode;
     property ParentColor;
     property ParentCtl3D;
     property ParentDoubleBuffered;
     property ParentFont;
     property ParentShowHint;
     property PopupMenu;
     property ShowHint;
     property TabOrder;
     property TabStop;
     property Touch;
     property Visible;
     property StyleElements;
     property OnAlignInsertBefore;
     property OnAlignPosition;
     property OnClick;
     property OnContextPopup;
     property OnDblClick;
     property OnDragDrop;
     property OnDockDrop;
     property OnDockOver;
     property OnDragOver;
     property OnEndDock;
     property OnEndDrag;
     property OnEnter;
     property OnExit;
     property OnGesture;
     property OnGetSiteInfo;
     property OnMouseActivate;
     property OnMouseDown;
     property OnMouseEnter;
     property OnMouseLeave;
     property OnMouseMove;
     property OnMouseUp;
     property OnStartDock;
     property OnStartDrag;
     property OnUnDock;
  end;


procedure Register;

IMPLEMENTATION {$R LightVcl.Visual.PathEdit.res}

USES
   LightVcl.Common.Colors, LightVcl.Common.ExecuteShell, LightCore.IO, LightCore.TextFile, LightVcl.Common.IO;


constructor TlightPathEdit.Create(aOwner: TComponent);
VAR myIcon: TIcon;
begin
 inherited Create(aOwner); // Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
 Height     := 41;
 Width      := 350;
 FileListBox:= NIL;
 FInputType := itFolder;
 FShowCreate:= TRUE;
 FOpenSrc   := TRUE;
 FShowApply := FALSE;                          // Invisible until Changed is triggered. The it is put to visible ONLY if the user set so in ObjectInspector

 ImgList:= TImageList.Create(Self);   { Freed by Self }
 myicon := TIcon.Create;
 TRY
   myicon.LoadFromResourceName(HInstance, 'xFOLDER');         { IMPORTANT: This must start with X. Reason: Icon resources are sorted alphabetically when linked, and the Windows shell uses the first icon for executables. Since VCL assumes 'MAINICON', all other icons must have names that appear after 'MAINICON' in alphabetical order. }
   ImgList.AddIcon(myicon);
   myicon.LoadFromResourceName(HInstance, 'xFOLDER_OPEN');
   ImgList.AddIcon(myicon);
 FINALLY
   FreeAndNil(myicon);
 end;

 edtPath:= TButtonedEdit.Create(Self);
 edtPath.Name                := 'PathEdit';
 edtPath.Text                := '';              // This must be AFTER edtPath.Name because setting edtPath.Name will also set the edtPath.Text
 edtPath.Parent              := Self;            // Here I can set the parent
 edtPath.Images              := imgList;
 edtPath.OnRightButtonClick  := btnBrowseClick;
 edtPath.OnKeyPress          := edtKeyPress;
 edtPath.OnClick             := edtPathOnClick; // Recompute validity color on click
 edtPath.OnChange            := edtPathChange;
 edtPath.OnKeyDown           := edtKeyDown;
 edtPath.RightButton.ImageIndex:= 0;
 edtPath.RightButton.HotImageIndex:= 1;
 edtPath.RightButton.Visible := TRUE;

 btnApply:= TButton.Create(Self);   { The user will write code for its event handler }
 btnApply.Name             := 'ButtonApply';
 btnApply.Parent           := Self;
 btnApply.Visible          := FALSE;                  // Invisible until Changed is triggered. Then it is put to visible ONLY if the user set ShowApplyBtn in Object Inspector
 btnApply.Enabled          := FALSE;                  // Not enabled until Changed is triggered (the user changes the path)
 btnApply.Width            := 48;
 btnApply.Margins.Left     := 1;
 btnApply.Margins.Top      := 1;
 btnApply.Margins.Right    := 1;
 btnApply.Margins.Bottom   := 1;
 btnApply.AlignWithMargins := TRUE;
 btnApply.Align            := alRight;
 btnApply.Hint             := 'Apply changes';
 btnApply.Caption          := 'Apply';
 btnApply.TabOrder         := 1;
 btnApply.OnClick          := btnApplyClick;
 // Note: FreeNotification(Self) only needed if control is parented to something other than Self

 btnExplore:= TButton.Create(Self);
 btnExplore.Name             := 'ButtonExplore';
 btnExplore.Parent           := Self;
 btnExplore.Width            := 28;
 btnExplore.Margins.Left     := 1;
 btnExplore.Margins.Top      := 1;
 btnExplore.Margins.Right    := 1;
 btnExplore.Margins.Bottom   := 1;
 btnExplore.AlignWithMargins := TRUE;
 btnExplore.Align            := alRight;
 btnExplore.Caption          := '^';
 btnExplore.TabOrder         := 2;
 btnExplore.Hint             := 'Locate this file/folder in Windows Explorer';
 btnExplore.OnClick          := btnExploreClick;

 btnCreate:= TButton.Create(Self);
 btnCreate.Name             := 'ButtonCreate';
 btnCreate.Parent           := Self;
 btnCreate.Visible          := FShowCreate;
 btnCreate.Enabled          := FALSE;                 // Not enabled until Changed is triggered
 btnCreate.Align            := alRight;
 btnCreate.Width            := 48;
 btnCreate.Margins.Left     := 1;
 btnCreate.Margins.Top      := 1;
 btnCreate.Margins.Right    := 1;
 btnCreate.Margins.Bottom   := 1;
 btnCreate.AlignWithMargins := TRUE;
 btnCreate.Hint             := 'Create folder';
 btnCreate.Caption          := 'Create';
 btnCreate.TabOrder         := 3;
 btnCreate.OnClick          := btnCreateClick;
end;


{ CreateWnd is called AFTER TIniFileVCL.LoadForm
  CreateWnd can be called more than once:
  http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html }
procedure TlightPathEdit.CreateWnd;
begin
 inherited CreateWnd;

 if NOT Initialized then      { Make sure we don't call this code twice }
  begin
   Initialized             := TRUE;

   edtPath.TabOrder        := 0;
   edtPath.ParentFont      := TRUE;
   edtPath.Align           := alClient;
   edtPath.Margins.Left    := 3;
   edtPath.Margins.Top     := 2;
   edtPath.Margins.Right   := 1;
   edtPath.Margins.Bottom  := 1;
   edtPath.AlignWithMargins:= TRUE;

   InputTypeChanged;          { We set the green/red color }
  end;
end;




{-----------------------------------------------------------------------------------------------------------------------
   USER INPUT
-----------------------------------------------------------------------------------------------------------------------}

{ Event handler for edtPath.OnChange.
  Note: Delphi bug - TEdit.OnChange triggers when Ctrl+A is pressed.
  See: http://stackoverflow.com/questions/42230077 }
procedure TlightPathEdit.edtPathChange(Sender: TObject);
begin
 PathChanged;
end;


{ Called when the path changes. Updates button states, linked controls, and triggers events. }
procedure TlightPathEdit.PathChanged;
VAR
   OldEvent: TNotifyEvent;
begin
 CheckPathValidity;

 btnCreate.Enabled:= PathHasValidChars = '';
 btnApply .Enabled:= PathIsValid = '';
 btnApply .Visible:= ShowApplyBtn;

 { Sync the linked TDirectoryListBox if the path is valid }
 if DirectoryExists(Path) AND Assigned(Directory) then
  begin
   OldEvent:= Directory.OnChange;    // Temporarily disable event to prevent recursion
   Directory.OnChange:= NIL;
   Directory.Directory:= Path;
   Directory.OnChange:= OldEvent;
  end;

 { Trigger the OnPathChanged event }
 if Assigned(FOnPathChanged)
 then FOnPathChanged(Self);

 { Sync the linked TFileListBox }
 if (FFileList <> NIL) AND (InputType = itFolder) then
   if PathIsValid = ''
   then FFileList.Directory:= Path
   else FFileList.Directory:= GetMyDocuments;
end;


{ Event handler for edtPath.OnKeyPress.
  Handles Enter key to trigger folder creation or OnPressEnter event. }
procedure TlightPathEdit.edtKeyPress(Sender: TObject; var Key: Char);
begin
 if (Ord(Key) = VK_RETURN) then
  begin
   if btnCreate.Visible AND btnCreate.Enabled
   then btnCreateClick(Self);

   if Assigned(FOnPressEnter)
   then FOnPressEnter(Self);
 end;
end;


procedure TlightPathEdit.btnBrowseClick(Sender: TObject);
VAR s: string;
begin
 s:= Path;

 case InputType of
  itFile:
     begin
       if PromptToLoadFile(s, FFileFilter, FOpenDlgTitle)
       then Path:= s;
     end;

  itFolder:
     begin
       if SelectAFolder(s, FOpenDlgTitle)
       then Path:= s;
     end;
 end;
end;


procedure TlightPathEdit.InputTypeChanged;
begin
 CheckPathValidity;      { Check validity AFTER the folder was created }

 if InputType = itfolder
 then edtPath.RightButton.Hint:= 'Browse for a folder'
 else edtPath.RightButton.Hint:= 'Browse for a file';

 if InputType = itfolder
 then btnExplore.Hint:= 'Locate this folder in Windows Explorer'
 else btnExplore.Hint:= 'Locate this file in Windows Explorer';

 { Set caption ONLY if the user has not set something else }
 if (Caption = 'Folder') OR (Caption = 'File') then
   if InputType = itfolder
   then Caption:= 'Folder'
   else Caption:= 'File';
end;


procedure TlightPathEdit.setInputType(const Value: TInputType);
begin
 FInputType := Value;
 InputTypeChanged;
end;



function TlightPathEdit.IsFilterStored: Boolean;
begin
  Result:= SDefaultFilter <> FFileFilter;
end;



{-----------------------------------------------------------------------------------------------------------------------
   CHECK VALIDITY
-----------------------------------------------------------------------------------------------------------------------}

procedure TlightPathEdit.CheckPathValidity; { Makes the control green when the path exists and red when it doesn't exist }
begin
 if csLoading in ComponentState then EXIT;

 { Empty path - neutral color instead of red }
 if edtPath.Text = '' then
  begin
   edtPath.Color:= clWindow;
   EXIT;
  end;

 case InputType of
  itFile:
     if FileExists(Path)
     then edtPath.Color:= clGreenWashed
     else edtPath.Color:= clRedFade;
  itFolder:
     if DirectoryExists(Path) then
      begin
       if FCheckCanWrite 
	   AND NOT CanWriteToFolder(Path)
       then edtPath.Color:= clRedFade
       else edtPath.Color:= clGreenWashed;
      end
     else edtPath.Color:= clRedFade;
 end;
end;



{ Works in both modes (itFile/itFolder).
  Does NOT check if the file/folder exists. }
function TlightPathEdit.PathHasValidChars: string;
begin
 Result:= '';

 if edtPath.Text = ''
 then EXIT('The path is empty!');

 if Length(Trail(edtPath.Text)) > MAXPATH
 then EXIT('Path is too long: '+ CRLFw+ edtPath.Text);

 { Valid colon }
 if (InputType= itFolder)
 AND NOT LightVcl.Common.IO.PathHasValidColon(edtPath.Text)
 then EXIT('The path has invalid characters!');

// HasValidFileNameChars only work with file names, not also with full paths
 { Valid chars }
 if (InputType= itFile)
 AND NOT System.IOUtils.TPath.HasValidFileNameChars(extractfilename(edtPath.Text), FALSE)
 then EXIT('The path has invalid characters!');

 { Valid colon for files }
 if (InputType= itFile)
 AND NOT LightVcl.Common.IO.PathHasValidColon(edtPath.Text)
 then EXIT('The path has invalid characters!');

 if (InputType= itFolder) then
  begin
   if NOT PathNameIsValid(edtPath.Text)
   then EXIT('Invalid characters in path!');

   { Disk in drive }
   if NOT DiskInDrive(System.IOUtils.TDirectory.GetDirectoryRoot(edtPath.Text))
   then EXIT('Lock on folder failed. Disk not in drive!');
  end;
end;



function TlightPathEdit.PathIsValid: string;    { Returns '' when the path is valid AND exists }
begin
 Result:= PathHasValidChars;

 { File exists? }
 if (Result = '')
 AND (InputType= itFile)
 AND NOT FileExists(edtPath.Text)
 then EXIT('File does not exist: '+ edtPath.Text);

 { Dir exists? }
 if (Result = '')
 AND (InputType= itFolder)
 AND NOT DirectoryExists(edtPath.Text)
 then EXIT('Folder does not exist: '+ edtPath.Text);

 { Write access? }
 if (Result = '')
 AND FCheckCanWrite
 AND (InputType= itFolder)
 AND DirectoryExists(edtPath.Text)
 AND NOT CanWriteToFolder(edtPath.Text)
 then EXIT('Cannot write to folder: '+ edtPath.Text);
end;



function TlightPathEdit.PathIsValidMsg: boolean;
VAR s: string;
begin
 s:= PathIsValid;
 Result:= s= '';
 if NOT Result
 then MessageError(s);
end;



procedure TlightPathEdit.Click;
begin
  inherited Click;
  CheckPathValidity;   // Recompute color on click - useful when path validity has changed externally
end;


{ Event handler for edtPath.OnClick.
  Recomputes color (red/green) on mouse click. Useful when path validity has changed externally. }
procedure TlightPathEdit.edtPathOnClick(Sender: TObject);
begin
  CheckPathValidity;
end;



procedure TlightPathEdit.setReadOnly(const Value: Boolean);
begin
  edtPath.ReadOnly:= Value
end;

function TlightPathEdit.getReadOnly: Boolean;
begin
 Result:= edtPath.ReadOnly;
end;

procedure TlightPathEdit.SetEnabled(Value: Boolean);
begin
  inherited;
  edtPath.Enabled:= Value;
  btnExplore.Enabled:= Value;
  if Value
  then PathChanged  { Recompute validity-dependent button states }
  else
    begin
      btnCreate.Enabled:= FALSE;
      btnApply.Enabled:= FALSE;
    end;
end;


function TlightPathEdit.getPath: string;
begin
 Result:= edtPath.Text;

 if (InputType= itFolder)
 AND (Result <> '')
 then Result:= Trail(Trim(edtPath.Text));
end;


procedure TlightPathEdit.setPath(const Value: string);
begin
 if csLoading in ComponentState
 then
  begin
   edtPath.OnChange := NIL;
   edtPath.Text:= Value;
   edtPath.OnChange := edtPathChange;
  end
 else
   if InputType= itFolder
   then edtPath.Text:= Trail(Value)
   else edtPath.Text:= Value;
end;



procedure TlightPathEdit.btnCreateClick(Sender: TObject);  { Creates the folder }
VAR s: string;
begin
 s:= PathHasValidChars;

 if s > '' then
  begin
   MessageError(s);
   EXIT;
  end;

 if (InputType= itFolder)
 then ForceDirectoriesMsg(Path)
 else StringToFile(Path, '', woAppend);

 PathChanged;

 if Assigned(FOnCreateFolder )
 then FOnCreateFolder(Self);         { event }
end;



{-----------------------------------------------------------------------------------------------------------------------
   BUTTONS
-----------------------------------------------------------------------------------------------------------------------}

procedure TlightPathEdit.setShowCreate(const Value: Boolean);
begin
 FShowCreate := Value;
 btnCreate.Visible:= FShowCreate;
end;


procedure TlightPathEdit.setOpenSrc(const Value: Boolean);
begin
 FOpenSrc := Value;
 btnExplore.Visible:= FOpenSrc;
end;


procedure TlightPathEdit.setShowApply(const Value: Boolean);
begin
 FShowApply := Value;
 btnApply.Visible:= FShowApply;
end;


procedure TlightPathEdit.btnApplyClick(Sender: TObject);
begin
 btnApply.Enabled:= FALSE;  { Don't let the user press Apply multiple times }
 if Assigned(FOnApply)
 then FOnApply(Sender);
end;


procedure TlightPathEdit.btnExploreClick(Sender: TObject);
begin
 if Path = ''
 then MessageError('No file/folder selected!')
 else
   case InputType of
    itFile:
       if FileExists(Path)
       then ExecuteExplorerSelect(Path)
       else MessageError('File does not exist: '+ Path);
    itFolder: ExecuteExplorer(Path);
   end;
end;




{-----------------------------------------------------------------------------------------------------------------------
   NOTIFICATION / FREE NOTIFICATION
-----------------------------------------------------------------------------------------------------------------------}

procedure TlightPathEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
 inherited;
 if Operation = opRemove then
  begin
   if AComponent = FFileList
   then FFileList:= NIL;
   if AComponent = FDirListBox
   then FDirListBox:= NIL;
  end;
end;


procedure TlightPathEdit.setFileList(const Value: TFileListBox);
begin
 if FFileList <> Value then
  begin
   if Assigned(FFileList)
   then FFileList.RemoveFreeNotification(Self);
   FFileList:= Value;
   if Assigned(FFileList)
   then FFileList.FreeNotification(Self);
  end;
end;


procedure TlightPathEdit.setDirListBox(const Value: TDirectoryListBox);
begin
 if FDirListBox <> Value then
  begin
   if Assigned(FDirListBox)
   then FDirListBox.RemoveFreeNotification(Self);
   FDirListBox:= Value;
   if Assigned(FDirListBox)
   then FDirListBox.FreeNotification(Self);
  end;
end;



{-----------------------------------------------------------------------------------------------------------------------
   NAVIGATE UP (Alt+Up - standard Windows Explorer shortcut)
-----------------------------------------------------------------------------------------------------------------------}

procedure TlightPathEdit.edtKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 if (Key = VK_UP) AND (Shift = [ssAlt]) AND (InputType = itFolder) then
  begin
   NavigateUp;
   Key:= 0;
  end;
end;


procedure TlightPathEdit.NavigateUp;
VAR
  CurrentDir, ParentDir: string;
begin
 if (InputType <> itFolder) OR (edtPath.Text = '') then EXIT;

 CurrentDir:= ExcludeTrailingPathDelimiter(edtPath.Text);
 ParentDir:= ExtractFileDir(CurrentDir);

 if (ParentDir <> '') AND (ParentDir <> CurrentDir)
 then Path:= ParentDir;
end;



{-----------------------------------------------------------------------------------------------------------------------
   WRITE ACCESS CHECK
-----------------------------------------------------------------------------------------------------------------------}

{ Checks write access by creating a temporary file with FILE_FLAG_DELETE_ON_CLOSE.
  No exception swallowing, no leftover temp files. }
function TlightPathEdit.CanWriteToFolder(const Folder: string): Boolean;
VAR
  TempFile: string;
  H: THandle;
begin
 TempFile:= Trail(Folder) + '~pathEdit_writetest.tmp';
 H:= CreateFile(PChar(TempFile), GENERIC_WRITE, 0, NIL, CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0);
 Result:= H <> INVALID_HANDLE_VALUE;
 if Result
 then CloseHandle(H);
end;


function TlightPathEdit.GetFiles(const FileType: string; const ReturnFullPath, DigSubdirectories: Boolean; ExcludeFolders: TStrings): TStringList;
begin
 Result:= ListFilesOf(Path, FileType, ReturnFullPath, DigSubdirectories, ExcludeFolders);
end;





procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TlightPathEdit]);
end;


end.
