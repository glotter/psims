{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmParFitProgress                               =
  =                                                              =
  =             Routines to handle a progress window for the     =
  =             display of progress during 	                 =
  =		    parameters fitting routine			     =
  ================================================================
  = File      : untParFitProgress.PAS                            =
  =                                                              =
  = Version   : 4.1                                              =
  ================================================================ }

unit untParFitProgress;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     ComCtrls, StdCtrls, Buttons, untDeclarations, untDivideValidation,
     untMain, untFieldValidation;

type
  TfrmParFitProgress = class(TForm)
    btnCancel: TBitBtn;
    edtIteration: TEdit;
    Label1: TLabel;
    lblSumOfSquares: TLabel;
    edtSumOfSquares: TEdit;
    lblBatchNo: TLabel;
    edtBatchNo: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    edtLastSS: TEdit;
    edtBestSS: TEdit;
    Label2: TLabel;
    edtInitialSS: TEdit;
    lblMode: TLabel;
    edtModeData: TEdit;
    edtBestParameter: TEdit;
    Procedure btnCancelClick(Sender: TObject);
    Procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
  frmParFitProgress: TfrmParFitProgress;

implementation

{$R *.DFM}

{ TfrmProgress }

Procedure TfrmParFitProgress.Start;
begin
// initialise and show the progress window
bStopped := false;
Show;
end;

Procedure TfrmParFitProgress.Stop;
begin
  // hide the progress window
  Hide;
end;

Procedure TfrmParFitProgress.Update;
Const MaxWidth = 10;
var St: String;
    Width, Digits: Integer;

    Procedure UserInfo(ModeOut, ParOut: String; ShowMode, ShowBest: Boolean; NumberOut: Real48);
    var Width, Digits: Integer;
        StringOut: String;
    Begin
    lblMode.Caption := ModeOut;
    edtModeData.Visible := ShowMode;
    edtBestParameter.Visible := ShowBest;
    edtBestParameter.Text := ParOut;
    GetField(NumberOut, 12, Width, Digits);
    Str (NumberOut:Width:Digits, StringOut);
    edtModeData.Text := StringOut;
    edtBestParameter.Text := ParOut;
    End;

Begin
Str(Control.ParFit.Iterations:5, St);
edtIteration.Text := St;
if Control.ParFit.Iterations > 0 then
   Begin
   GetField(FitParameter.HistoricFit[Control.ParFit.Iterations - 1], MaxWidth, Width, Digits);
   Str(FitParameter.HistoricFit[Control.ParFit.Iterations - 1]:Width:Digits, St);
   edtLastSS.Text := St;
   GetField(FitParameter.OldSumOfSquares, MaxWidth, Width, Digits);
   Str(FitParameter.OldSumOfSquares:Width:Digits, St);
   edtBestSS.Text := St;
   GetField(FitParameter.HistoricFit[0], MaxWidth, Width, Digits);
   Str(FitParameter.HistoricFit[0]:Width:Digits, St);
   edtInitialSS.Text := St;
   End
Else
   Begin
   edtLastSS.Text := '-1';
   edtBestSS.Text := '-1';
   edtInitialSS.Text := '-1';
   End;
If Obs.RunBatch then
   Begin
   lblBatchNo.Visible := true;
   edtBatchNo.Visible := true;
   lblSumOfSquares.Visible := true;
   edtSumOfSquares.Visible := true;
   Str(Control.BatchCount:4, St);
   if St <> edtBatchNo.Text then
      Begin
      edtBatchNo.Text := St;
      GetField(FitParameter.SumOfSquares, MaxWidth, Width, Digits);
      Str(FitParameter.SumOfSquares:Width:Digits, St);
      edtSumOfSquares.Text := St;
      End;
   End
Else
   Begin
   lblBatchNo.Visible := false;
   edtBatchNo.Visible := false;
   lblSumOfSquares.Visible := false;
   edtSumOfSquares.Visible := false;
   End;
case FitParameter.Mode of
     Initial:      UserInfo('Initial', '', false, false, 0);
     ChooseRandom: UserInfo('Random', '', false, false, 0);
     Matrix:       UserInfo('Matrix', '', true, false, FitParameter.SensMultiplier);
     Individual:   UserInfo('Single parameter', FittingNames[FitParameter.Focus], true, true, FitParameter.deltaX[FitParameter.Focus]);
     Gather:       UserInfo('Gathering data', FittingNames[FitParameter.Focus], false, true, 0);
     End;
end;

Procedure TfrmParFitProgress.btnCancelClick(Sender: TObject);
begin
FitParameter.Abort := true;      // End the parameter fitting routine
end;

Procedure TfrmParFitProgress.FormClose(Sender: TObject; var Action: TCloseAction);
begin
// if the user closes this window, then stop the run
Action := caHide;
bStopped := true;
end;

end.
