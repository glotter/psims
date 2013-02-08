{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmProgress                                     =
  =                                                              =
  =             Routines to handle a small progress bar that is  =
  =             displayed during simulation runs                 =
  ================================================================
  = File      : untProgress.PAS                                  =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untProgress;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     ComCtrls, StdCtrls, Buttons, untDeclarations, untDivideValidation, untFieldValidation;

type
  TfrmProgress = class(TForm)
    btnCancel: TBitBtn;
    cbUpdate: TCheckBox;
    cmbUpdate: TComboBox;
    bxDate: TEdit;
    Procedure btnCancelClick(Sender: TObject);
    Procedure FormClose(Sender: TObject; var Action: TCloseAction);
    Procedure cbUpdateClick(Sender: TObject);
    Procedure cmbUpdateChange(Sender: TObject);
    Procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    bStopped: boolean;
    bUpdate: boolean;
    iUpdateRate: integer;
    Procedure Start;
    Procedure Stop;
    Procedure Update;
  end;

var
  frmProgress: TfrmProgress;

implementation

{$R *.DFM}

{ TfrmProgress }

Procedure TfrmProgress.Start;
begin
// initialise and show the progress window
bStopped := false;
Show;
end;

Procedure TfrmProgress.Stop;
begin
  // hide the progress window
  Hide;
end;

Procedure TfrmProgress.Update;
Const MaxWidth = 10;
var St: String;
    Width, Digits: Integer;
begin
If not Control.EquilMode then
  if Control.BatchMode then
     Begin
     Str (Control.BatchCount, St);
     bxDate.Text := St;
     End
  Else if Control.SensitivityTestOn then
     bxDate.Text := SensitivityNames[Control.SensParameter]
  Else if Control.Parfit.FittingMode then
     Begin
     if not Control.Run_on then
        Begin
        GetField(FitParameter.SumOfSquares, MaxWidth, Width, Digits);
        Str (FitParameter.SumOfSquares: Width :Digits, St);
        bxDate.Text := St;
        End
     End
  Else
     bxDate.Text := Control.Date;
end;

Procedure TfrmProgress.btnCancelClick(Sender: TObject);
begin
// user pressed Cancel, so raise a flag for processing to check
If Control.EquilMode then
   Control.EndEquil := true
Else If Control.SensitivityTestOn then
   Control.SensParameter := EndInputs // Set the sens parameter variable to the last (dummy) input and thereby end the run
Else If Control.Parfit.FittingMode then
   FitParameter.Abort := true      // End the parameter fitting routine
Else
   Begin
   bStopped := true;
   Control.EndBatch := true;
   End;
end;

Procedure TfrmProgress.FormClose(Sender: TObject; var Action: TCloseAction);
begin
// if the user closes this window, then stop the run
Action := caHide;
bStopped := true;
end;

Procedure TfrmProgress.cbUpdateClick(Sender: TObject);
begin
bUpdate := cbUpdate.Checked;
bxDate.Text := Control.Date;
end;

Procedure TfrmProgress.cmbUpdateChange(Sender: TObject);
begin
If (not Control.EquilMode) and (not Control.BatchMode) and (not Control.Parfit.FittingMode) then
  Begin
  // get the stored update rate
  iUpdateRate := integer(cmbUpdate.Items.Objects[cmbUpdate.ItemIndex]);
  // limit it to a positive number
  if (iUpdateRate < 1) then iUpdateRate := 1;
  If iUpdateRate <> trunc(Divide(Control.MaxDays, Control.nDisplays)) then
     Begin
     Control.nDisplays := trunc(Divide(Control.MaxDays, iUpdateRate));
     Control.ProjectHasChanged := true;
     End;
  End;
end;

Procedure TfrmProgress.FormShow(Sender: TObject);
var i, StartRate, NextItem, CountIndex: integer;
    s: string;
begin
// clear the list
If ((not Control.EquilMode) and (not Control.Parfit.FittingMode)) then
  Begin
  cmbUpdate.Clear;
  // find the optimum display rate, based on the number of days and the
  // number of display points
  Control.DisplayInterval := trunc(Divide(Control.MaxDays, Control.nDisplays));
  StartRate := Control.DisplayInterval;
  If StartRate < 1 then
     StartRate := 1;
  // add nine options around this display rate
  CountIndex := 0;
  For i := 0 to 10 do
      Begin
      case i of
           0: NextItem := round(StartRate / 100);
           1: NextItem := round(StartRate / 30);
           2: NextItem := round(StartRate / 10);
           3: NextItem := round(StartRate / 5);
           4: NextItem := round(StartRate / 2);
           5: NextItem := StartRate;
           6: NextItem := StartRate * 2;
           7: NextItem := StartRate * 5;
           8: NextItem := StartRate * 10;
           9: NextItem := StartRate * 30;
          10: NextItem := StartRate * 100;
           End;
      if (NextItem >= 1) then
         Begin
         s := 'Every ' + IntToStr(NextItem) + ' days';
         // add the option, and save the rate with it
         cmbUpdate.Items.AddObject(s, pointer(NextItem));
         CountIndex := CountIndex + 1;
         End;
      End;
  // refresh the display
  cbUpdateClick(Nil);
  cmbUpdate.ItemIndex := CountIndex - 6;
  If cmbUpdate.ItemIndex < 1 then
     cmbUpdate.ItemIndex := 1;
  cmbUpdateChange(Nil);
  End
Else if Control.Parfit.FittingMode then
  Begin
  cmbUpdate.Clear;
  s := 'Iteration: ' + IntToStr(Control.ParFit.Iterations + 1);
  cmbUpdate.Items.AddObject(s, pointer(Control.ParFit.Iterations + 1));
  cmbUpdate.ItemIndex := 0;
  End;
end;

end.
