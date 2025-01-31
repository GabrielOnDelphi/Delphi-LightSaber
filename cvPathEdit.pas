UNIT cvPathEdit;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

  An edit box that allows user to choose/enter a file or folder.

  Features:
     OnPressEnter  - Event is triggered when the user pressed Enter
     OnApply       - Event is triggered when the user pressed Apply btn
     InputType     - Type of input that the control accepts: File (not implemented yet) and Folder
     Path          - Path as entered by user
     FileListBox   - Can automatically update the linked FileListBox to nanivate to the specified folder

  Tester:
     c:\Myprojects\Project Testers\Cubic VCL cvPathEdit\cvPathEdit_Tester.dpr

  See: What's the difference between CreateWnd and CreateWindowHandle? https://stackoverflow.com/questions/582903/whats-the-difference-between-createwnd-and-createwindowhandle
-------------------------------------------------------------------------------------------------------------}

//ToDo 1: Let user go one folder up by pressing a shortcut (like backspace maybe)
//ToDo 5: Add property CheckCanWrite: boolean; If true, the control will verify if it has write access to the folder selected by the user.

INTERFACE

USES
  System.SysUtils, Winapi.Windows, System.Classes, Vcl.StdCtrls, Vcl.Controls, Vcl.Graphics, vcl.imglist, Vcl.CONSTs, Vcl.FileCtrl,
  Vcl.ExtCtrls, ccCore, cbDialogs;

TYPE
  TValidity= (vaNone, vaValid, vaInvalid);                   { Normal / Green / Red color }
  TInputType= (itFile, itFolder);                            { What kind of path will the user type in this control: folder or file }

  TCubicPathEdit = class(TCustomGroupBox)
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
     procedure edtPatchOnClick (Sender: TObject);
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
   public
     constructor Create(aOwner: TComponent);  override;
     function  PathHasValidChars: string;   { Returns '' when the input string (path) is valid and and error msg when it is not valid }
     function  PathIsValid: string;         { Returns '' when the input string (path) is valid and and error msg when it is not valid }
     function  PathIsValidMsg: boolean;     { Returns true when the input string (path) is valid and and error msg when it is not valid }

     function  GetFiles(CONST FileType: string; CONST ReturnFullPath, DigSubdirectories: Boolean; ExcludeFolders: TStrings= nil): TStringList;
   published
     property FileListBox   : TFileListBox read FFileList     write FFileList;
     property Directory     : TDirectoryListBox  read FDirListBox   write FDirListBox;
     property Path          : string       read getPath       write setPath;
     property FileFilter    : string       read FFileFilter   write FFileFilter     stored IsFilterStored;      { Has meaninng ONLY when InputType is 'itFile' }
     property OpenDlgTitle  : string       read FOpenDlgTitle write FOpenDlgTitle;
     property IsReadOnly    : Boolean      read getReadOnly   write setReadOnly     default FALSE;
     property InputType     : TInputType   read FInputType    write setInputType    default itFolder;

     property ShowCreateBtn : Boolean      read FShowCreate   write setShowCreate   default TRUE;
     property ShowOpenSrc   : Boolean      read FOpenSrc      write setOpenSrc      default TRUE;
     property ShowApplyBtn  : Boolean      read FShowApply    write setShowApply    default FALSE;

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

IMPLEMENTATION {$R cvPathEdit.res}

USES
   ccColors, System.IOUtils, csExecuteShell, ccIO, ccTextFile, cmIO, cmIO.Win;



constructor TCubicPathEdit.Create(aOwner: TComponent);
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
   myicon.LoadFromResourceName(HInstance, 'xFOLDER');         { I M P O R T N A T:    This must start with X. Reason: Due to a rather messed decision from Embarcadero, icon resources are sorted alphabetically when linked. The Windows rule is that the icon used by the shell for an Executeble is the first icon. Delphi VCL code assumes that the application icon is named 'MAINICON'. Put these requirements together and you deduce the all your icons must have names that appear after 'MAINICON' in alphabetical order. }
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
 edtPath.OnClick             := edtPatchOnClick; { Recompute color (red/gree) on mouse click. This is useful for when the file/folder did not existed (clRed) but meanwhile the user created it (now it has to be clGree) }
 edtPath.OnChange            := edtPathChange;
 edtPath.RightButton.ImageIndex:= 0;
 edtPath.RightButton.HotImageIndex:= 1;
 edtPath.RightButton.Visible := TRUE;

 btnApply:= TButton.Create(Self);   { The user will write code for its event handler }
 WITH btnApply DO
  begin
   Name             := 'ButtonApply';
   Parent           := Self;
   Visible          := FALSE;                         // Invisible until Changed is triggered. The it is put to visible ONLY if the user set so in ObjectInspector
   Enabled          := FALSE;                         // Not enabled until Changed is triggered (the user changes the path)
   Width            := 48;
   Margins.Left     := 1;
   Margins.Top      := 1;
   Margins.Right    := 1;
   Margins.Bottom   := 1;
   AlignWithMargins := TRUE;
   Align            := alRight;
   Hint             := 'Apply changes';
   Caption          := 'Apply';
   TabOrder         := 1;
   OnClick          := btnApplyClick;
 end;
 // btnApply.FreeNotification(Self); Only needed if this controls is parented to a control other than self (for example the Label in TLabeledEdit is parented by the form, not by the Edit

 btnExplore:= TButton.Create(Self);
 WITH btnExplore DO
  begin
   Name             := 'ButtonExplore';
   Parent           := Self;
   Width            := 28;
   Margins.Left     := 1;
   Margins.Top      := 1;
   Margins.Right    := 1;
   Margins.Bottom   := 1;
   AlignWithMargins := TRUE;
   Align            := alRight;
   Caption          := '^';
   TabOrder         := 2;
   Hint             := 'Locate this file/folder in Windows Explorer';
   OnClick          := btnExploreClick;
 end;

 btnCreate:= TButton.Create(Self);
 WITH btnCreate DO
  begin
   Name             := 'ButtonCreate';
   Parent           := Self;
   Visible          := FShowCreate;
   Enabled          := FALSE;       { Not enabled until Changed is triggered }
   Align            := alRight;
   Width            := 48;
   Margins.Left     := 1;
   Margins.Top      := 1;
   Margins.Right    := 1;
   Margins.Bottom   := 1;
   AlignWithMargins := TRUE;
   Hint             := 'Create folder';
   Caption          := 'Create';
   TabOrder         := 3;
   OnClick          := btnCreateClick;
 end;
end;



{ CreateWnd is called AFTER TIniFileVCL.LoadForm
  CreateWnd can be called more than once:
  http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html }
procedure TCubicPathEdit.CreateWnd;
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

procedure TCubicPathEdit.edtPathChange(Sender: TObject);
begin
 PathChanged;    { Delphi bug here: TEdit.OnChange triggers when Ctrl+A is pressed.     http://stackoverflow.com/questions/42230077/why-does-tedit-onchange-trigger-when-ctrla-is-pressed/42231303#42231303 }
 inherited;  // if you want to run standard handler
end;


procedure TCubicPathEdit.PathChanged;
VAR
   OldEvent: TNotifyEvent;
begin
 CheckPathValidity;      { Check validity AFTER the folder was created }

 btnCreate.Enabled:= PathHasValidChars= '';
 btnApply .Enabled:= {del PathNameIsValid(edtPath.Text) AND }(PathIsValid = '');
 btnApply .Visible:= ShowApplyBtn;

 if Assigned(FOnPathChanged) then
  begin
   if (DirectoryExists(Path))
   AND Assigned(Directory) then
    begin
     OldEvent:= Directory.OnChange;  // Don't let the Directory do its event
     Directory.OnChange:= NIL;
     Directory.Directory:= Path;
     Directory.OnChange:= OldEvent;  // Restore the event.
    end;

   FOnPathChanged(Self);
  end;

 if (FFileList <> NIL)
 AND (InputType= itFolder) then
   if (PathIsValid= '')
   then FFileList.Directory:= Path
   else FFileList.Directory:= GetMyDocuments;
end;


procedure TCubicPathEdit.edtKeyPress(Sender: TObject; var Key: Char);
begin
 inherited KeyPress(Key);

 if (Ord(Key) = VK_RETURN) then
  begin
   if btnCreate.Visible AND btnCreate.Enabled
   then btnCreateClick(Self);

   if Assigned(FOnPressEnter)
   then FOnPressEnter(Self);
 end;
end;


procedure TCubicPathEdit.btnBrowseClick(Sender: TObject);
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


procedure TCubicPathEdit.InputTypeChanged;
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
   then Caption:= 'Folder'           // https://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
   else Caption:= 'File';

//del if InputType= itFile then ShowCreateBtn:= FALSE;   { We show the 'Create' btn only if we are in itFolder mode }
end;


procedure TCubicPathEdit.setInputType(const Value: TInputType);
begin
 FInputType := Value;
 InputTypeChanged;
end;



function TCubicPathEdit.IsFilterStored: Boolean;
begin
  Result:= SDefaultFilter <> FFileFilter;
end;









{-----------------------------------------------------------------------------------------------------------------------
   CHECK VALIDITY
-----------------------------------------------------------------------------------------------------------------------}

procedure TCubicPathEdit.CheckPathValidity; { Makes the control green when the path exists and red when it doesn't exist }
begin
 if csLoading in ComponentState then EXIT;

 case InputType of
  itFile:
     if FileExists(Path)
     then edtPath.Color:= clGreenWashed
     else edtPath.Color:= clRedFade;
  itFolder:
    begin
     if DirectoryExists(Path)
     then edtPath.Color:= clGreenWashed
     else edtPath.Color:= clRedFade;
    end;
 end;
end;



{ Worsk in both modes (itFile/itFolder)
  Does NOT check if the file/folder exists. }
function TCubicPathEdit.PathHasValidChars: string;
begin
 Result:= '';

 if edtPath.Text = ''
 then EXIT('The path is empty!');

 if Length(Trail(edtPath.Text)) > MAXPATH
 then EXIT('Path is too long: '+ CRLFw+ edtPath.Text);

 { Valid colon }
 if (InputType= itFolder)
 AND NOT PathHasValidColon(edtPath.Text)
 then EXIT('The path has invalid characters!');

// HasValidFileNameChars only work with file names, not also with full paths
 { Valid chars }
 if (InputType= itFile)
 AND NOT System.IOUtils.TPath.HasValidFileNameChars(extractfilename(edtPath.Text), FALSE)
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



function TCubicPathEdit.PathIsValid: string;    { Returns '' when the path is valid AND folder exists }
begin
 Result:= PathHasValidChars;

 { Dir exists? }
 if (Result = '')
 AND (InputType= itFolder)
 AND NOT DirectoryExists(edtPath.Text)
 then EXIT('Folder does not exist: '+ edtPath.Text);
end;



function TCubicPathEdit.PathIsValidMsg: boolean;
VAR s: string;
begin
 s:= PathIsValid;
 Result:= s= '';
 if NOT Result
 then MesajError(s);
end;




procedure TCubicPathEdit.Click;
begin
  inherited Click;
  CheckPathValidity;   { Recompute color (red/gree) on mouse click. This is useful for when the file/folder did not existed (clRed) but meanwhile the user created it (now it has to be clGree) }
end;


procedure TCubicPathEdit.edtPatchOnClick(Sender: TObject);
begin
  inherited;
  CheckPathValidity;   { Recompute color (red/gree) on mouse click. This is useful for when the file/folder did not existed (clRed) but meanwhile the user created it (now it has to be clGree) }
end;





procedure TCubicPathEdit.setReadOnly(const Value: Boolean);
begin
  edtPath.ReadOnly:= Value
end;

function TCubicPathEdit.getReadOnly: Boolean;
begin
 Result:= edtPath.ReadOnly;
end;

procedure TCubicPathEdit.SetEnabled(Value: Boolean);
begin
  inherited;
  edtPath.Enabled:= Value;
  btnCreate.Enabled:= Value;
end;





function TCubicPathEdit.getPath: string;
begin
 Result:= edtPath.Text;

 if (InputType= itFolder)
 AND (Result <> '')
 then Result:= Trail(Trim(edtPath.Text));
end;


procedure TCubicPathEdit.setPath(const Value: string);
begin
 if csLoading in ComponentState
 then
  begin
   edtPath.OnRightButtonClick := NIL;
   edtPath.Text:= Value;
   edtPath.OnRightButtonClick := btnBrowseClick;
  end
 else
   if InputType= itFolder
   then edtPath.Text:= Trail(Value)
   else edtPath.Text:= Value;
end;



procedure TCubicPathEdit.btnCreateClick(Sender: TObject);  { Creates the folder }
VAR s: string;
begin
 s:= PathHasValidChars;

 if s > '' then
  begin
   MesajError(s);
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

procedure TCubicPathEdit.setShowCreate(const Value: Boolean);
begin
 FShowCreate := Value;
 btnCreate.Visible:= FShowCreate;
end;


procedure TCubicPathEdit.setOpenSrc(const Value: Boolean);
begin
 FOpenSrc := Value;
 btnExplore.Visible:= FOpenSrc;
end;


procedure TCubicPathEdit.setShowApply(const Value: Boolean);
begin
 FShowApply := Value;
 btnApply.Visible:= FShowApply;
end;


procedure TCubicPathEdit.btnApplyClick(Sender: TObject);
begin
 btnApply.Enabled:= FALSE;  { Don't let the user press Apply multiple times }
 if Assigned(FOnApply)
 then FOnApply(Sender);
end;


procedure TCubicPathEdit.btnExploreClick(Sender: TObject);
begin
 if Path = ''
 then MesajError('No file/folder selected!')
 else
   case InputType of
    itFile  : ExecuteExplorerSelect (Path);
    itFolder: ExecuteExplorer       (Path);
   end;
end;






procedure Register;
begin
  RegisterComponents('LightSaber', [TCubicPathEdit]);
end;



function TCubicPathEdit.GetFiles(const FileType: string; const ReturnFullPath, DigSubdirectories: Boolean; ExcludeFolders: TStrings): TStringList;
begin
 Result:= ListFilesOf(Path, FileType, ReturnFullPath, DigSubdirectories, ExcludeFolders);
end;

end.
