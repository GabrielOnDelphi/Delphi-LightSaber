UNIT FrameExplorer;

{=============================================================================================================
   2026.01.29
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   WINDOWS EXPLORER FRAME

   A reusable frame that provides file explorer functionality similar to Windows Explorer.
   Contains directory tree, file list, path editor, and filter components.

   COMPONENTS:
     - Directory: TDirectoryListBox - Shows directory tree
     - FileList: TCubicFileList - Shows files in selected directory
     - Path: TCubicPathEdit - Shows/edits current path
     - Filter: TCubicFilterBox - Filter files by extension

   USAGE:
     Drop this frame onto a form to add file browsing capability.
     Set Filter.Filter to customize file type filters.
     Example: Filter.Filter:= LightCore.IO.FilterAllFiles + '|' + LightVcl.Graph.Util.AllImgFlt;

   IDE NOTE:
     If this file is renamed/moved, the IDE will complain that it cannot find the file.
     To fix: remove the component from the palette, then re-add by opening the file at its new location.

   REGISTRATION:
     Registered in 'LightSaber VCL' component palette.
=============================================================================================================}

INTERFACE

USES
  System.Classes, Vcl.Controls, Vcl.Forms, LightVcl.Visual.AppDataForm,Vcl.StdCtrls, LightVcl.Visual.PathEdit, Vcl.FileCtrl, LightVcl.Visual.FileFilter,
  LightVcl.Visual.FileListBox, Vcl.ExtCtrls;

TYPE
  TFrameWinExplorer = class(TFrame)
    btnRefresh : TButton;
    Directory  : TDirectoryListBox;
    FileList   : TCubicFileList;
    Filter     : TCubicFilterBox;
    lblTop     : TLabel;
    Panel2     : TPanel;
    Path       : TCubicPathEdit;
    Splitter   : TSplitter;
    procedure DirectoryChange(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
  private
  public
  end;



procedure Register;


IMPLEMENTATION {$R *.dfm}


{ Called when user selects a different directory in the tree }
procedure TFrameWinExplorer.DirectoryChange(Sender: TObject);
begin
  Path.Path:= Directory.Directory;
end;


{ Refreshes the file list to show current directory contents }
procedure TFrameWinExplorer.btnRefreshClick(Sender: TObject);
begin
  FileList.Update;
end;




procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TFrameWinExplorer]);
end;


end.
