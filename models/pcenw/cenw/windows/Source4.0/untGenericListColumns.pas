{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Module    : TfrmGenericListColumns                           =
  =                                                              =
  =             Interface routine for users to add columns with  =
  =             the same properties for a whole list.            =
  ================================================================
  = File      : untGenericListColumns.pas                        =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untGenericListColumns;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type TfrmGenericListColumns = class(TForm)
     btnOK: TBitBtn;
     btnCancel: TBitBtn;
     btnHelp: TBitBtn;
     edtColumn: TEdit;
     edt1stRow: TEdit;
     edtConstant: TEdit;
     lblEnterResult: TLabel;
     edtLastRow: TEdit;
     Label1: TLabel;
     Label2: TLabel;
     Label3: TLabel;
     lblIncrement: TLabel;
     edtIncrement: TEdit;
     procedure FormShow(Sender: TObject);
     procedure btnOKClick(Sender: TObject);
{  private
    { Private declarations }
{  public
    { Public declarations }
  end;

var frmGenericListColumns: TfrmGenericListColumns;

implementation

{$R *.DFM}

uses untDeclarations, untMain;

Procedure TfrmGenericListColumns.FormShow(Sender: TObject);
Begin
edtColumn.Text := '';
frmMain.FillEdit(Sender, edt1stRow, List.Trans1stRow, 0);
frmMain.FillEdit(Sender, edtLastRow, List.TransLastRow, 0);
edtConstant.Text := '';
If List.TransUseIncrement then
  Begin
  edtIncrement.Visible := true;
  edtIncrement.Text := '';
  lblIncrement.Visible := true;
  End
Else
  Begin
  edtIncrement.Visible := false;
  lblIncrement.Visible := false;
  End;
List.SetColumn := false;
edtColumn.SetFocus;
End;

procedure TfrmGenericListColumns.btnOKClick(Sender: TObject);
Begin
frmMain.GetInteger(Sender, edtColumn, List.TransSetColumn);
List.TransSetColumn := List.TransSetColumn - 1;
frmMain.GetInteger(Sender, edt1stRow, List.Trans1stRow);
frmMain.GetInteger(Sender, edtLastRow, List.TransLastRow);
if List.DataType[List.TransSetColumn] = S then
   List.TransString := edtConstant.Text
Else
   frmMain.GetEdit(Sender, edtConstant, List.Trans1stNo, 1);
If List.TransUseIncrement then
   frmMain.GetEdit(Sender, edtIncrement, List.TransIncrement, 1);
List.SetColumn := true;
End;

end.
