{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmSpatialProgress                              =
  =                                                              =
  =             Routines to handle a small progress bar that is  =
  =             displayed during spatial simulation runs         =
  ================================================================
  = File      : untSpatialProgress.PAS                           =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untSpatialProgress;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Buttons, untDeclarations, untDivideValidation;

type
  TfrmSpatialProgress = class(TForm)
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
  frmSpatialProgress: TfrmSpatialProgress;

implementation

{$R *.DFM}

{ TfrmSpatialProgress }

Procedure TfrmSpatialProgress.Start;
begin
  // initialise and show the progress window
  bStopped := false;
  Show;
end;

Procedure TfrmSpatialProgress.Stop;
begin
  // hide the progress window
  Hide;
end;

Procedure TfrmSpatialProgress.Update;
begin
  // update the progress window
If not Control.EquilMode then
  bxDate.Text := Control.Date;
end;

Procedure TfrmSpatialProgress.btnCancelClick(Sender: TObject);
begin
  // user pressed Cancel, so raise a flag for processing to check
If Not Control.EquilMode then
   Begin
   bStopped := true;
   Control.EndBatch := true;
   End
Else
   Control.EndEquil := true;
end;

Procedure TfrmSpatialProgress.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // if the user closes this window, then stop the run
  Action := caHide;
  bStopped := true;
end;

Procedure TfrmSpatialProgress.cbUpdateClick(Sender: TObject);
begin
  bUpdate := cbUpdate.Checked;
  bxDate.Text := Control.Date;
end;

Procedure TfrmSpatialProgress.cmbUpdateChange(Sender: TObject);
begin
If not Control.EquilMode then
  Begin
  // get the stored update rate
  iUpdateRate := integer(cmbUpdate.Items.Objects[cmbUpdate.ItemIndex]);
  // limit it to a positive number
  if (iUpdateRate < 1) then iUpdateRate := 1;
  If Control.nDisplays <> trunc(Divide(Control.MaxDays, iUpdateRate)) then
     Begin
     Control.nDisplays := trunc(Divide(Control.MaxDays, iUpdateRate));
     Control.ProjectHasChanged := true;
     End;                                  
  End;
end;

Procedure TfrmSpatialProgress.FormShow(Sender: TObject);
var i, StartRate, NextItem, CountIndex: integer;
    s: string;
begin
  // clear the list
If not Control.EquilMode then
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
  End;
end;

end.
