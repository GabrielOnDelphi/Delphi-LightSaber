UNIT FrameExplorer;

{Note: if this file is renamed/move, the IDE will complaing that it cannot find the file.
 So, I have to remove the component from the palete, and re-add by opening the file, at its new location. }

INTERFACE

USES
  System.Classes, Vcl.Controls, Vcl.Forms, LightVcl.Common.AppDataForm,Vcl.StdCtrls, LightVcl.Visual.PathEdit, Vcl.FileCtrl, LightVcl.Visual.FileFilter,
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


{ Example of filter: Filter.Filter:= LightCore.IO.FilterAllFiles + '|' + LightVcl.Graph.Util.AllImgFlt; }


procedure TFrameWinExplorer.DirectoryChange(Sender: TObject);
begin
 Path.Path:= Directory.Directory;
end;

procedure TFrameWinExplorer.btnRefreshClick(Sender: TObject);
begin
 FileList.Update;
end;






procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TFrameWinExplorer]);
end;


end.
