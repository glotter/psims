{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmSensSelect                                   =
  =                                                              =
  =             Interface routine to nominate                    =
  =             variables for a sensitivity analysis.            =
  ================================================================
  = File      : untSensSelect.PAS                               =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untSensSelect;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     StdCtrls, ExtCtrls, Buttons;
Const BaseComponents = 8;

type
  TfrmSensSelect = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    lblRange: TLabel;
    edtRange: TEdit;
    lblPercent: TLabel;
    btnSelectAll: TBitBtn;
    btnSelectNone: TBitBtn;
    Procedure FormShow(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnSelectNoneClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var frmSensSelect: TfrmSensSelect;

implementation

{$R *.DFM}

uses untDeclarations, untMiscellaneous, untMain;

Procedure TfrmSensSelect.FormShow(Sender: TObject);
Const MaxEntries = 37;
      ColWidth = 185;
      MinWidth = 460;
      SmallWidth = 300;
      ExtraHeight = 120;
      MinHeight = 150;
var cbEnabled: TCheckBox;
    iCount, iColumn, nColumns, iEntries, HalfEntries: integer;
    SensSelected, FirstSens, LastSens: SensitivityType;
Begin
while (Self.ComponentCount > BaseComponents) do
      Self.Components[BaseComponents].Free;
If TestSens.Index = 1 then
   Begin
   frmSensSelect.Text := 'Select input pools and variables for a sensitivity test';
   FirstSens := Succ(Dummy);
   LastSens := Pred(EndInputs);
   lblRange.Visible := false;
   lblPercent.Visible := false;
   edtRange.Visible := false;
   btnSelectAll.Visible := true;
   btnSelectNone.Visible := true;
   btnHelp.Left := 360;
   End
Else if TestSens.Index = 2 then
   Begin
   frmSensSelect.Text := 'Select output variables to test';
   FirstSens := Succ(EndInputs);
   LastSens := Pred(EndDummy);
   lblRange.Visible := false;
   lblPercent.Visible := false;
   edtRange.Visible := false;
   btnSelectAll.Visible := true;
   btnSelectNone.Visible := true;
   btnHelp.Left := 360;
   End
Else // if TestSens.Index = 3 then
   Begin
   frmSensSelect.Text := 'Select range for variable testing';
   lblRange.Visible := true;
   lblPercent.Visible := true;
   edtRange.Visible := true;
   btnSelectAll.Visible := false;
   btnSelectNone.Visible := false;
   btnHelp.Left := 186;
   End;
iEntries := 0;
if TestSens.Index < 3 then
   Begin
   For SensSelected := FirstSens to LastSens do
       iEntries := iEntries + 1;
   nColumns := 1 + (iEntries - 1) Div MaxEntries;
   HalfEntries := (iEntries + 1) div nColumns;
   frmSensSelect.ClientWidth  := 50 + ColWidth * nColumns;
   if frmSensSelect.ClientWidth < MinWidth then
      frmSensSelect.ClientWidth := MinWidth;
   frmSensSelect.Height := ExtraHeight + HalfEntries * 18;
   iColumn := 1;
   iEntries := 0;
   iCount := 0;
   For SensSelected := FirstSens to LastSens do
       Begin
       if iEntries > HalfEntries then
          Begin
          iCount := 0;
          iColumn := iColumn + 1;
          iEntries := 0;
          End;
       iEntries := iEntries + 1;
       iCount := iCount + 16;
       cbEnabled := TCheckBox.Create(Self);
       cbEnabled.Parent := Self;
       cbEnabled.Left := 15 + (iColumn - 1) * ColWidth;
       cbEnabled.Top := iCount;
       cbEnabled.Width := 150;
       cbEnabled.Caption := SensitivityNames[SensSelected];
       cbEnabled.Checked := TestSens.Choose[SensSelected];
       End;
   End
Else
   Begin
   frmSensSelect.Height := MinHeight;
   frmSensSelect.ClientWidth := SmallWidth;
   frmMain.FillEdit(Sender, edtRange, TestSens.Range, 100);
   End;
btnOK.Top := frmSensSelect.Height - 60;
btnCancel.Top := btnOK.Top;
btnHelp.Top := btnOK.Top;
btnSelectAll.Top := btnOK.Top;
btnSelectNone.Top := btnOK.Top;
btnOK.SetFocus;
End;

procedure TfrmSensSelect.btnCancelClick(Sender: TObject);
begin
TestSens.Abort := true;
end;

Procedure TfrmSensSelect.btnOKClick(Sender: TObject);
var iCount: integer;
    SensSelected, FirstSens, LastSens: SensitivityType;
Begin
If TestSens.Index = 1 then
   Begin
   FirstSens := Succ(Dummy);
   LastSens := Pred(EndInputs);
   End
Else if TestSens.Index = 2 then
   Begin
   FirstSens := Succ(EndInputs);
   LastSens := Pred(EndDummy);
   End;
iCount := 0;
If TestSens.Index < 3 then
   Begin
   For SensSelected := FirstSens to LastSens do
       Begin
       TestSens.Choose[SensSelected] := TCheckBox(Self.Components[iCount + BaseComponents]).Checked;
       iCount := iCount + 1;
       End;
   End
Else
   frmMain.GetEdit(Sender, edtRange, TestSens.Range, 100);
Control.ProjectHasChanged := true;
End;

procedure TfrmSensSelect.btnSelectAllClick(Sender: TObject);
var iCount: Integer;
    SensSelected, FirstSens, LastSens: SensitivityType;
begin
If TestSens.Index = 1 then
   Begin
   FirstSens := Succ(Dummy);
   LastSens := Pred(EndInputs);
   End
Else //if TestSens.Index = 2 then
   Begin
   FirstSens := Succ(EndInputs);
   LastSens := Pred(EndDummy);
   End;
iCount := 0;
For SensSelected := FirstSens to LastSens do
    Begin
    TCheckBox(Self.Components[iCount + BaseComponents]).Checked := true;
    iCount := iCount + 1;
    End;
end;

procedure TfrmSensSelect.btnSelectNoneClick(Sender: TObject);
var iCount: Integer;
    SensSelected, FirstSens, LastSens: SensitivityType;
begin
If TestSens.Index = 1 then
   Begin
   FirstSens := Succ(Dummy);
   LastSens := Pred(EndInputs);
   End
Else //if TestSens.Index = 2 then
   Begin
   FirstSens := Succ(EndInputs);
   LastSens := Pred(EndDummy);
   End;
iCount := 0;
For SensSelected := FirstSens to LastSens do
    Begin
    TCheckBox(Self.Components[iCount + BaseComponents]).Checked := false;
    iCount := iCount + 1;
    End;
end;

End.
