UNIT cmCursorGuard;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Allows you to sets the cursor to crHourglass and then back to crDefault without the need of the classic try-finally block.
   When the procedure where you use TCursorGuard exits, the cursor is put back automatically.

   Example:
   begin
     TCursorGuard.CursorBusy;
     SlowCode;
   end;                 // The interface is automatically released here, at the end of the procedure

=============================================================================================================}

INTERFACE

USES
   Vcl.Controls, Vcl.Forms;

TYPE
  TCursorGuard = class(TInterfacedObject, IUnknown)
  private
    FOldCursor: TCursor;
    constructor Create;
  public
    destructor Destroy; override;
    class function CursorBusy: IUnknown;
  end;


IMPLEMENTATION


constructor TCursorGuard.Create;
begin
  inherited Create;
  FOldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
end;


destructor TCursorGuard.Destroy;
begin
  Screen.Cursor := FOldCursor;
  inherited;
end;


class function TCursorGuard.CursorBusy: IUnknown;
begin
  Result := TCursorGuard.Create;
end;


end.
