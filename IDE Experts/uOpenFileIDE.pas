UNIT uOpenFileIDE;

{=============================================================================================================
   www.GabrielMoraru.com
   2024
   GitHub.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
 This general purpose unit opens the specified file in the IDE.
  Used by:
    IDE_FileReceiver expert
    IDE_OpenFile expert

  https://stackoverflow.com/questions/24690352
  https://en.delphipraxis.net/topic/7955-how-to-open-a-file-in-the-already-running-ide/?page=3
--------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils,
  Vcl.Dialogs,
  ToolsAPI;

TYPE
  RIDEPosition = record
    Line: Integer;            // Put cursor on this line
    Col : Integer;            // Put cursor on this column
    FileName: String;         // File to open
    Comment: string;          // Text that we insert in the file, at the specified position?
    procedure Default(aFileName: string);
  end;


procedure OpenInIDEEditor(PasRec: RIdePosition);

{ Checks if a file is already open in the IDE and switches to it without changing cursor position.
  Returns True if file was already open and we switched to it, False otherwise. }
function SwitchToOpenFile(const FileName: string): Boolean;

{ Switches to an already-open file and moves cursor to the specified line.
  Returns True if successful. }
function GotoLineInOpenFile(const FileName: string; LineNum: Integer): Boolean;


IMPLEMENTATION



procedure OpenInIDEEditor(PasRec: RIdePosition);
var
  i               : Integer;
  FileName        : String;
  CharPos         : TOTACharPos;
  CursorPos       : TOTAEditPos;
  IActionServices : IOTAActionServices;
  IEditor         : IOTAEditor;
  IEditorServices : IOTAEditorServices60;
  IEditView       : IOTAEditView;
  IEditWriter     : IOTAEditWriter;
  IModule         : IOTAModule;
  IModuleServices : IOTAModuleServices;
  InsertPos       : Longint;
  IServices       : IOTAServices;
  ISourceEditor   : IOTASourceEditor;
begin
  IServices := BorlandIDEServices as IOTAServices;
  if NOT Assigned(IServices) then
    begin
      ShowMessage('IOTAServices not available!');
      EXIT;
    end;

  IServices.QueryInterface(IOTAACtionServices, IActionServices);
  if IActionServices <> NIL then
  begin

    IServices.QueryInterface(IOTAModuleServices, IModuleServices);
    Assert(IModuleServices <> NIL);

    // Close all files open in the IDE
    // IModuleServices.CloseAll;

    if IActionServices.OpenFile(PasRec.FileName) then
    begin

      //  At this point, if the named file has an associated .DFM and we stopped here, the form designer would be in front of the code editor.

      IModule := IModuleServices.Modules[0];
      //  IModule is the one holding our .Pas file and its .Dfm, if any
      //  So, iterate the IModule's editors until we find the one for the .Pas file and then call .Show on it.  This will bring the code editor in front of the form editor.

      ISourceEditor := NIL;

      for i:= 0 to IModule.ModuleFileCount - 1 do
      begin
        IEditor := IModule.ModuleFileEditors[i];
        FileName:= IEditor.FileName;

        if CompareText(ExtractFileExt(IEditor.FileName), '.Pas') = 0
        then
           if ISourceEditor = NIL
           then
             begin
               IEditor.QueryInterface(IOTASourceEditor, ISourceEditor);
               IEditor.Show;
             end
           else
        //else mmo.Lines.Add('Closing editor?');
      end;

      // Next, place the editor caret where we want it ...
      IServices.QueryInterface(IOTAEditorServices, IEditorServices);
      if IEditorServices = NIL
      then ShowMessage('TfrmOTAReceiver - No IEditorServices!')
      else
        begin
          IEditView := IEditorServices.TopView;
          Assert(IEditView <> Nil);
          CursorPos.Line:= PasRec.Line;
          CursorPos.Col := PasRec.Col;
          IEditView.SetCursorPos(CursorPos);
          IEditView.MoveViewToCursor;    // Scroll the IEditView to the caret

          // Insert the comment, if any
          if PasRec.Comment <> '' then
          begin
            Assert(ISourceEditor <> NIL);

            IEditView.ConvertPos(True, CursorPos, CharPos);
            InsertPos:= IEditView.CharPosToPos(CharPos);

            IEditWriter:= ISourceEditor.CreateUnDoableWriter;
            if IEditWriter = NIL
            then ShowMessage('TfrmOTAReceiver - IEditWriter is nil!')
            else
             begin
               IEditWriter.CopyTo(InsertPos);
               IEditWriter.Insert(PAnsiChar(AnsiString(PasRec.Comment)));
               IEditWriter:= NIL;
             end;
          end;
        end;
    end;
  end;
end;



{ RIDEPosition }

procedure RIDEPosition.Default(aFileName: string);
begin
    Line:= 1;
    Col := 1;
    FileName:= aFileName;
    Comment:= '';
end;


{ Checks if a file is already open in the IDE and switches to it without changing cursor position.
  Returns True if file was already open and we switched to it, False otherwise. }
function SwitchToOpenFile(const FileName: string): Boolean;
var
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  IEditor: IOTAEditor;
  i, j: Integer;
begin
  Result:= False;

  if NOT Supports(BorlandIDEServices, IOTAModuleServices, IModuleServices)
  then Exit;

  // Iterate through all open modules to find if the file is already open
  for i:= 0 to IModuleServices.ModuleCount - 1 do
    begin
      IModule:= IModuleServices.Modules[i];
      if IModule = nil then Continue;

      // Check each editor in the module
      for j:= 0 to IModule.ModuleFileCount - 1 do
        begin
          IEditor:= IModule.ModuleFileEditors[j];
          if IEditor = nil then Continue;

          // Compare filenames (case-insensitive)
          if SameText(IEditor.FileName, FileName) then
            begin
              // File is already open - just show it without changing cursor
              IEditor.Show;
              Exit(True);
            end;
        end;
    end;
end;




{ Switches to an already-open file and moves cursor to the specified line.
  Returns True if successful. }
function GotoLineInOpenFile(const FileName: string; LineNum: Integer): Boolean;
var
  ModuleServices: IOTAModuleServices;
  EditorServices: IOTAEditorServices60;
  Module: IOTAModule;
  Editor: IOTAEditor;
  EditView: IOTAEditView;
  CursorPos: TOTAEditPos;
  i, j: Integer;
begin
  Result:= False;

  if NOT Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices)
  then Exit;

  for i:= 0 to ModuleServices.ModuleCount - 1 do
  begin
    Module:= ModuleServices.Modules[i];
    if Module = nil then Continue;

    for j:= 0 to Module.ModuleFileCount - 1 do
    begin
      Editor:= Module.ModuleFileEditors[j];
      if Editor = nil then Continue;

      if SameText(Editor.FileName, FileName) then
      begin
        Editor.Show;

        (BorlandIDEServices as IOTAServices).QueryInterface(IOTAEditorServices, EditorServices);
        if EditorServices <> nil then
        begin
          EditView:= EditorServices.TopView;
          if Assigned(EditView) then
          begin
            CursorPos.Line:= LineNum;
            CursorPos.Col:= 1;
            EditView.SetCursorPos(CursorPos);
            EditView.MoveViewToCursor;
            EXIT(True);
          end;
        end;
      end;
    end;
  end;
end;


end.
