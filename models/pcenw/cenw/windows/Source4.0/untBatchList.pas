{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmBatchList                                    =
  =                                                              =
  =             Interface routine to nominate                    =
  =             variables that are to be loaded from a batch     =
  =             file during program execution.                   =
  ================================================================
  = File      : untBatchList.PAS                                 =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untBatchList;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     StdCtrls, ExtCtrls, Buttons;
Const BaseComponents = 3;

type
  TfrmBatchList = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    Procedure FormShow(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var frmBatchList: TfrmBatchList;

implementation

{$R *.DFM}

uses untDeclarations, untMiscellaneous;

Procedure TfrmBatchList.FormShow(Sender: TObject);
var cbEnabled: TCheckBox;
    iCount, column, iEntries, HalfEntries: integer;
    BatchVar: BatchOptions;
Begin
while (Self.ComponentCount > BaseComponents) do
      Self.Components[BaseComponents].Free;
iCount := 0;
Column := 1;
frmBatchList.Text := 'Select parameters to read from a batch file';
iEntries := 0;
For BatchVar := B_Dummy to B_EndDummy do
    iEntries := iEntries + 1;
iEntries := iEntries - 2;
If iEntries <= 20 then
    Begin
    frmBatchList.ClientWidth := 280;
    End
Else
    Begin
    frmBatchList.ClientWidth := 400;
    End;
If iEntries > 20 then
   HalfEntries := (iEntries + 1) div 2
Else
   HalfEntries := iEntries;
frmBatchList.Height := 70 + (HalfEntries + 4) * 16;
btnOK.Top := frmBatchList.Height - 60;
btnCancel.Top := btnOK.Top;
btnHelp.Top := btnOK.Top;
iEntries := 0;
For BatchVar := B_Dummy to B_EndDummy do
    if (BatchVar <> B_Dummy) and (BatchVar <> B_EndDummy) then
       Begin
       iCount := iCount + 16;
       iEntries := iEntries + 1;
       cbEnabled := TCheckBox.Create(Self);
       cbEnabled.Parent := Self;
       If iEntries > HalfEntries then
          Begin
          iEntries := 0;
          Column := 2;
          iCount := 16;
          End;
       If Column = 1 then
          cbEnabled.Left := 8
       Else
          cbEnabled.Left := 200;
       cbEnabled.Top := iCount;
       cbEnabled.Width := 200;
       cbEnabled.Caption := BatchVariableNames[BatchVar];
       If Batch.Choose[BatchVar] then
          cbEnabled.Checked := true;
       End;
End;

Procedure TfrmBatchList.btnOKClick(Sender: TObject);
var iCount: integer;
    BatchVar: BatchOptions;
Begin
iCount := 0;
For BatchVar := B_Dummy to B_EndDummy do
    if (BatchVar <> B_Dummy) and (BatchVar <> B_EndDummy) then
        Begin
        Batch.Choose[BatchVar] := TCheckBox(Self.Components[iCount + BaseComponents]).Checked;
        iCount := iCount + 1;
        End;
Control.ProjectHasChanged := true;
End;

End.
