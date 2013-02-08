{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmFitSelect                                    =
  =                                                              =
  =             Interface routine to nominate                    =
  =             variables for a sensitivity analysis.            =
  ================================================================
  = File      : untFitSelect.PAS                                 =
  =                                                              =
  = Version   : 4.1                                              =
  ================================================================ }

unit untFitSelect;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     StdCtrls, ExtCtrls, Buttons, untDivideValidation, untProcessFittingData;
Const BaseComponents = 14;

type
  TfrmFitSelect = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    btnSelectAll: TBitBtn;
    btnSelectNone: TBitBtn;
    lblMin1: TLabel;
    lblMax1: TLabel;
    lblMin2: TLabel;
    lblCurrent2: TLabel;
    lblCurrent1: TLabel;
    lblMax2: TLabel;
    lblSumOfSquares: TLabel;
    edtSumOfSquaresInitial: TEdit;
    edtSumOfSquaresBest: TEdit;
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

var frmFitSelect: TfrmFitSelect;

implementation

{$R *.DFM}

uses untDeclarations, untMiscellaneous, untMain;

Procedure TfrmFitSelect.FormShow(Sender: TObject);
Const MaxEntries = 37;
      ColWidth1 = 185;
      ColWidth2 = 350;
      MinWidth = 460;
      SmallWidth = 300;
      ExtraHeight = 120;
      MinHeight = 150;
      EditBoxWidth = 50;
      EditBoxShift1 = 150;
      EditBoxShift2 = 200;
      EditBoxShift3 = 250;
var cbEnabled: TCheckBox;
    edtEnabled: TEdit;
    iCount, iColumn, nColumns, iEntries, HalfEntries: integer;
    FitSelected, FirstFit, LastFit: FittingType;
Begin
while (Self.ComponentCount > BaseComponents) do
      Self.Components[BaseComponents].Free;
FirstFit := Succ(F_Dummy);
LastFit := Pred(F_EndDummy);
iEntries := 0;
lblMin1.Visible := false;
lblMax1.Visible := false;
lblCurrent1.Visible := false;
lblMin2.Visible := false;
lblCurrent2.Visible := false;
lblMax2.Visible := false;
If FitParameter.Index = 1 then
   Begin
   frmFitSelect.Text := 'Select variables to optimise';
   btnSelectAll.Visible := true;
   btnSelectNone.Visible := true;
   lblSumOfSquares.Visible := false;
   edtSumOfSquaresInitial.Visible := false;
   edtSumOfSquaresBest.Visible := false;
   btnHelp.Left := 360;
   For FitSelected := FirstFit to LastFit do
       iEntries := iEntries + 1;
   nColumns := 1 + (iEntries - 1) Div MaxEntries;
   HalfEntries := (iEntries + 1) div nColumns;
   frmFitSelect.ClientWidth  := 50 + ColWidth1 * nColumns;
   if frmFitSelect.ClientWidth < MinWidth then
      frmFitSelect.ClientWidth := MinWidth;
   frmFitSelect.Height := ExtraHeight + HalfEntries * 18;
   iColumn := 1;
   iEntries := 0;
   iCount := 0;
   For FitSelected := FirstFit to LastFit do
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
       cbEnabled.Left := 15 + (iColumn - 1) * ColWidth1;
       cbEnabled.Top := iCount;
       cbEnabled.Width := 150;
       cbEnabled.Caption := FittingNames[FitSelected];
       cbEnabled.Checked := FitParameter.Choose[FitSelected];
       End
   End
Else if FitParameter.Index = 2 then
   Begin
   frmFitSelect.Text := 'Select allowed variable ranges';
   lblMin1.Caption := 'Minimum';
   lblMin2.Caption := 'Minimum';
   lblMax1.Caption := 'Maximum';
   lblMax2.Caption := 'Maximum';
   lblCurrent1.Caption := 'Current';
   lblCurrent2.Caption := 'Current';
   lblSumOfSquares.Visible := false;
   edtSumOfSquaresInitial.Visible := false;
   edtSumOfSquaresBest.Visible := false;
   lblMin1.Visible := true;
   lblCurrent1.Visible := true;
   lblMax1.Visible := true;
   lblMin1.Left := EditBoxShift1 + 17;
   lblCurrent1.Left := EditBoxShift2 + 17;
   lblMax1.Left := EditBoxShift3 + 17;
   iEntries := 0;
   For FitSelected := FirstFit to LastFit do
       if FitParameter.Choose[FitSelected] then
          iEntries := iEntries + 1;
   nColumns := 1 + (iEntries - 1) div MaxEntries;
   HalfEntries := (iEntries + 1) div nColumns;
   frmFitSelect.ClientWidth  := 50 + ColWidth2 * nColumns;
   if frmFitSelect.ClientWidth < SmallWidth then
      frmFitSelect.ClientWidth := SmallWidth;
   frmFitSelect.Height := ExtraHeight + HalfEntries * 18;
   btnSelectAll.Visible := false;
   btnSelectNone.Visible := false;
   btnHelp.Left := 185;
   iColumn := 1;
   iCount := 0;
   iEntries := 0;
   For FitSelected := FirstFit to LastFit do
       if FitParameter.Choose[FitSelected] then
          Begin
          iEntries := iEntries + 1;
          iCount := iCount + 16;
          if iEntries > HalfEntries then
             Begin
             iCount := 16;
             iColumn := iColumn + 1;
             iEntries := 0;
             if iColumn = 2 then
                Begin
                lblMin2.Visible := true;
                lblCurrent2.Visible := true;
                lblMax2.Visible := true;
                lblMin2.Left := EditBoxShift1 + 17 + (iColumn - 1) * ColWidth2;
                lblCurrent2.Left := EditBoxShift2 + 17 + (iColumn - 1) * ColWidth2;
                lblMax2.Left := EditBoxShift3 + 17 + (iColumn - 1) * ColWidth2;
                End;
             End;
          cbEnabled := TCheckBox.Create(Self);
          cbEnabled.Parent := Self;
          cbEnabled.Left := 15 + (iColumn - 1) * ColWidth2;
          cbEnabled.Top := iCount;
          cbEnabled.Width := EditBoxShift1 - 10;
          cbEnabled.Caption := FittingNames[FitSelected];
          cbEnabled.Checked := FitParameter.Choose[FitSelected];
          edtEnabled := TEdit.Create(Self);
          edtEnabled.Parent := Self;
          edtEnabled.Left := cbEnabled.Left + EditBoxShift1;
          edtEnabled.Top := cbEnabled.Top;
          edtEnabled.Width := EditBoxWidth;
          frmMain.FillEdit(Sender, edtEnabled, FitParameter.Min[FitSelected] * FitParameter.Multiplier[FitSelected], 1);
          edtEnabled := TEdit.Create(Self);
          edtEnabled.Parent := Self;
          edtEnabled.Left := cbEnabled.Left + EditBoxShift2;
          edtEnabled.Top := cbEnabled.Top;
          edtEnabled.Width := EditBoxWidth;
          frmMain.FillEdit(Sender, edtEnabled, FitParameter.Initial[FitSelected] * FitParameter.Multiplier[FitSelected], 1);
          edtEnabled.ReadOnly := true;
          edtEnabled := TEdit.Create(Self);
          edtEnabled.Parent := Self;
          edtEnabled.Left := cbEnabled.Left + EditBoxShift3;
          edtEnabled.Top := cbEnabled.Top;
          edtEnabled.Width := EditBoxWidth;
          frmMain.FillEdit(Sender, edtEnabled, FitParameter.Max[FitSelected] * FitParameter.Multiplier[FitSelected], 1);
          End;
   FitParameter.ParsToOptimise := iEntries;
   End
Else // if FitParameter.Index = 3 then
   Begin
   frmFitSelect.Text := 'Select parameters to keep';
   lblMin1.Caption := 'Original';
   lblMin2.Caption := 'Original';
   lblMax1.Caption := 'New value';
   lblMax2.Caption := 'New value';
   lblCurrent1.Caption := 'Optimised';
   lblCurrent2.Caption := 'Optimised';
   lblSumOfSquares.Visible := true;
   edtSumOfSquaresInitial.Visible := true;
   edtSumOfSquaresBest.Visible := true;
   lblMin1.Visible := true;
   lblCurrent1.Visible := true;
   lblMax1.Visible := true;
   lblMin1.Enabled := false;
   lblMin2.Enabled := false;
   lblCurrent1.Enabled := false;
   lblCurrent2.Enabled := false;
   lblMin1.Left := EditBoxShift1 + 17;
   lblCurrent1.Left := EditBoxShift2 + 17;
   lblMax1.Left := EditBoxShift3 + 17;
   iEntries := 0;
   For FitSelected := FirstFit to LastFit do
       if FitParameter.Optimised[FitSelected] then
          iEntries := iEntries + 1;
   nColumns := 1 + (iEntries - 1) div MaxEntries;
   HalfEntries := (iEntries + 1) div nColumns;
   frmFitSelect.ClientWidth  := 50 + ColWidth2 * nColumns;
   if frmFitSelect.ClientWidth < SmallWidth then
      frmFitSelect.ClientWidth := SmallWidth;
   frmFitSelect.Height := ExtraHeight + HalfEntries * 18;
   btnSelectAll.Visible := false;
   btnSelectNone.Visible := false;
   btnHelp.Left := 185;
   iColumn := 1;
   iCount := 0;
   iEntries := 0;
   For FitSelected := FirstFit to LastFit do
       if FitParameter.Optimised[FitSelected] then
          Begin
          iEntries := iEntries + 1;
          iCount := iCount + 16;
          if iEntries > HalfEntries then
             Begin
             iCount := 16;
             iColumn := iColumn + 1;
             iEntries := 0;
             if iColumn = 2 then
                Begin
                lblMin2.Visible := true;
                lblCurrent2.Visible := true;
                lblMax2.Visible := true;
                lblMin2.Left := EditBoxShift1 + 17 + (iColumn - 1) * ColWidth2;
                lblCurrent2.Left := EditBoxShift2 + 17 + (iColumn - 1) * ColWidth2;
                lblMax2.Left := EditBoxShift3 + 17 + (iColumn - 1) * ColWidth2;
                End;
             End;
          cbEnabled := TCheckBox.Create(Self);
          cbEnabled.Parent := Self;
          cbEnabled.Left := 15 + (iColumn - 1) * ColWidth2;
          cbEnabled.Top := iCount;
          cbEnabled.Width := EditBoxShift1 - 10;
          cbEnabled.Caption := FittingNames[FitSelected];
          cbEnabled.Checked := FitParameter.Choose[FitSelected];
          edtEnabled := TEdit.Create(Self);
          edtEnabled.Parent := Self;
          edtEnabled.Left := cbEnabled.Left + EditBoxShift1;
          edtEnabled.Top := cbEnabled.Top;
          edtEnabled.Width := EditBoxWidth;
          frmMain.FillEdit(Sender, edtEnabled, FitParameter.Initial[FitSelected] * FitParameter.Multiplier[FitSelected], 1);
          edtEnabled.Enabled := false;
          edtEnabled := TEdit.Create(Self);
          edtEnabled.Parent := Self;
          edtEnabled.Left := cbEnabled.Left + EditBoxShift2;
          edtEnabled.Top := cbEnabled.Top;
          edtEnabled.Width := EditBoxWidth;
          edtEnabled.Enabled := false;
          frmMain.FillEdit(Sender, edtEnabled, FitParameter.Best[FitSelected] * FitParameter.Multiplier[FitSelected], 1);
          edtEnabled := TEdit.Create(Self);
          edtEnabled.Parent := Self;
          edtEnabled.Left := cbEnabled.Left + EditBoxShift3;
          edtEnabled.Top := cbEnabled.Top;
          edtEnabled.Width := EditBoxWidth;
          edtEnabled.Enabled := true;
          frmMain.FillEdit(Sender, edtEnabled, FitParameter.Best[FitSelected] * FitParameter.Multiplier[FitSelected], 1);
          End;
   lblSumOfSquares.Top := cbEnabled.Top + 30;
   edtSumOfSquaresInitial.Top := cbEnabled.Top + 30;
   edtSumOfSquaresBest.Top := cbEnabled.Top + 30;
   edtSumOfSquaresInitial.Left := cbEnabled.Left + EditBoxShift1;
   edtSumOfSquaresBest.Left := cbEnabled.Left + EditBoxShift3 - EditBoxWidth div 2 + 4;
   frmMain.FillEdit(Sender, edtSumOfSquaresInitial, FitParameter.HistoricFit[0], 1);
   frmMain.FillEdit(Sender, edtSumOfSquaresBest, FitParameter.OldSumOfSquares, 1);
   End;
btnOK.Top := frmFitSelect.Height - 60;
btnCancel.Top := btnOK.Top;
btnHelp.Top := btnOK.Top;
btnSelectAll.Top := btnOK.Top;
btnSelectNone.Top := btnOK.Top;
btnOK.SetFocus;
End;

procedure TfrmFitSelect.btnCancelClick(Sender: TObject);
Var FitSelected: FittingType;
begin
FitParameter.Abort := true;
If FitParameter.Index = 3 then
   Begin
   For FitSelected := Succ(F_Dummy) to Pred(F_EndDummy) do
       if FitParameter.Choose[FitSelected] then
          FitParameter.Varied[FitSelected] := FitParameter.Initial[FitSelected];
   SetUpFitParameters (-1);                          // Just to make sure all actual parameters are set to those in 'Varied'
   End;
end;

Procedure TfrmFitSelect.btnOKClick(Sender: TObject);
var iCount: integer;
    FitSelected, FirstFit, LastFit: FittingType;
Begin
iCount := 0;
FirstFit := Succ(F_Dummy);
LastFit := Pred(F_EndDummy);
If FitParameter.Index = 1 then
   Begin
   For FitSelected := FirstFit to LastFit do
       Begin
       FitParameter.Choose[FitSelected] := TCheckBox(Self.Components[iCount + BaseComponents]).Checked;
       iCount := iCount + 1;
       End
   End
Else if FitParameter.Index = 2 then
   Begin
   For FitSelected := FirstFit to LastFit do
       if FitParameter.Choose[FitSelected] then
          Begin
          FitParameter.Choose[FitSelected] := TCheckBox(Self.Components[iCount + BaseComponents]).Checked;
          frmMain.GetEdit(Sender, TEdit(Self.Components[iCount + BaseComponents + 1]), FitParameter.Min[FitSelected], 1);
          frmMain.GetEdit(Sender, TEdit(Self.Components[iCount + BaseComponents + 2]), FitParameter.Initial[FitSelected], 1);
          frmMain.GetEdit(Sender, TEdit(Self.Components[iCount + BaseComponents + 3]), FitParameter.Max[FitSelected], 1);
          FitParameter.Min[FitSelected] := Divide(FitParameter.Min[FitSelected], FitParameter.Multiplier[FitSelected]);
          FitParameter.Initial[FitSelected] := Divide(FitParameter.Initial[FitSelected], FitParameter.Multiplier[FitSelected]);
          FitParameter.Max[FitSelected] := Divide(FitParameter.Max[FitSelected], FitParameter.Multiplier[FitSelected]);
          iCount := iCount + 4;
          End
   End
Else // if FitParameter.Index = 3 then
   Begin
   For FitSelected := FirstFit to LastFit do
       if FitParameter.Optimised[FitSelected] then
          Begin
          frmMain.GetEdit(Sender, TEdit(Self.Components[iCount + BaseComponents + 3]), FitParameter.Varied[FitSelected], 1);
          FitParameter.Varied[FitSelected] := Divide(FitParameter.Varied[FitSelected], FitParameter.Multiplier[FitSelected]);
          iCount := iCount + 4;
          End
       Else
          FitParameter.Varied[FitSelected] := FitParameter.Initial[FitSelected];
   SetUpFitParameters (-1);                          // Just to make sure all actual parameters are set to those in 'Varied'
   End;
Control.ProjectHasChanged := true;
End;

procedure TfrmFitSelect.btnSelectAllClick(Sender: TObject);
var iCount: Integer;
    FitSelected, FirstFit, LastFit: FittingType;
begin
iCount := 0;
FirstFit := Succ(F_Dummy);
LastFit := Pred(F_EndDummy);
For FitSelected := FirstFit to LastFit do
    Begin
    TCheckBox(Self.Components[iCount + BaseComponents]).Checked := true;
    iCount := iCount + 1;
    End;
end;

procedure TfrmFitSelect.btnSelectNoneClick(Sender: TObject);
var iCount: Integer;
    FitSelected, FirstFit, LastFit: FittingType;
begin
iCount := 0;
FirstFit := Succ(F_Dummy);
LastFit := Pred(F_EndDummy);
For FitSelected := FirstFit to LastFit do
    Begin
    TCheckBox(Self.Components[iCount + BaseComponents]).Checked := false;
    iCount := iCount + 1;
    End;
end;

End.
