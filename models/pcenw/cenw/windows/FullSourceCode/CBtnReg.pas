{$I DFS.INC}

unit CBtnReg;

interface

procedure Register;

implementation

uses
  DFSClrBn, ColorAEd, CBtnForm, DFSAbout, DsgnIntf, Classes;


procedure Register;
begin
  RegisterComponents('ICMS', [TdfsColorButton]);
  RegisterPropertyEditor(TypeInfo(TColorArrayClass), NIL, '',
     TColorArrayProperty);
  RegisterPropertyEditor(TypeInfo(string), TdfsColorButton, 'Version',
     TdfsVersionProperty);
end;


end.
