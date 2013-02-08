{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmFittingParameters                            =
  =                                                              =
  =             Routines to set up the parameters for the        =
  =             routine to search for equilibrium conditions     =
  ================================================================
  = File      : untFittingParameters.PAS                           =
  =                                                              =
  = Version   : 4.1                                              =
  ================================================================ }

unit untFittingParameters;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, untFileIo, untLoadSaveProject;

type
  TfrmFittingParameters = class(TForm)
    edtMaxIterations: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TBitBtn;
    edtEquilRuns: TEdit;
    grpClimate: TGroupBox;
    edtObsFile: TEdit;
    btnObservation: TButton;
    dlgOpenObservationFile: TOpenDialog;
    chkIncludeWeights: TCheckBox;
    chkRunBatch: TCheckBox;
    Label17: TLabel;
    edtMonteCarloExponent: TEdit;
    Label2: TLabel;
    edtCriterion1: TEdit;
    Label1: TLabel;
    edtMaxToRandomise: TEdit;
    Label3: TLabel;
    edtCriterion2: TEdit;
    chkBestIndividual: TCheckBox;
    Label4: TLabel;
    edtDelta: TEdit;
    lblCriterion3: TLabel;
    edtCriterion3: TEdit;
    Label5: TLabel;
    edtMaxChange: TEdit;
    Procedure FormShow(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
    procedure btnObservationClick(Sender: TObject);
    procedure chkRunBatchClick(Sender: TObject);
    procedure chkBestIndividualClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var frmFittingParameters: TfrmFittingParameters;

implementation
{$R *.DFM}

uses untDeclarations, untMain;

Procedure TfrmFittingParameters.FormShow(Sender: TObject);
begin
// fill form with parameters
frmMain.FillEdit(Sender, edtEquilRuns, Control.ParFit.MaxIterations, 0);
chkIncludeWeights.Checked := Obs.IncludeWeight;
chkRunBatch.Checked := Obs.RunBatch;
chkBestIndividual.Checked := FitParameter.CompareBest;
chkBestIndividualClick(Sender);
frmMain.FillEdit(Sender, edtCriterion3, Control.ParFit.Criterion3, 1);
frmFittingParameters.chkRunBatchClick(Sender);
frmMain.FillEdit(Sender, edtCriterion1, Control.ParFit.Criterion1, 1000);
if (CompareText(ExtractFilePath(Obs.ObservationFile), ExtractFilePath(Control.ProjectFile)) = 0) then
   Obs.ObservationFile := ExtractFileName(Obs.ObservationFile);
edtObsFile.Text := Obs.ObservationFile;
frmMain.FillEdit(Sender, edtMonteCarloExponent, Control.ParFit.MonteCarloExponent, 1);
frmMain.FillEdit(Sender, edtMaxToRandomise, Control.ParFit.ParametersToRandomise, 0);
frmMain.FillEdit(Sender, edtCriterion1, Control.ParFit.Criterion1, 100);
frmMain.FillEdit(Sender, edtCriterion2, Control.ParFit.Criterion2, 1e6);
frmMain.FillEdit(Sender, edtDelta, Control.ParFit.Delta, 100);
frmMain.FillEdit(Sender, edtMaxChange, Control.ParFit.MaxChange, 1);
End;

procedure TfrmFittingParameters.btnObservationClick(Sender: TObject);
begin
// select a new observation file
dlgOpenObservationFile.FileName := edtObsFile.Text;
if (dlgOpenObservationFile.Execute) then
   begin
   edtObsFile.Text := dlgOpenObservationFile.FileName;
   if (CompareText(ExtractFilePath(edtObsFile.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtObsFile.Text := ExtractFileName(edtObsFile.Text);
   SetCurrentDir(Control.ProjectDirectory);
   end;
end;

Procedure TfrmFittingParameters.btnOKClick(Sender: TObject);
var NewFile: FileNameType;

    Function CheckMonteCarloExponentChange(Sender: TObject): Boolean;
    var NewExponent: Real48;
        ShowMessage: Boolean;
        Answer: Integer;
        NewMessage: String;
    begin
    frmMain.GetEdit(Sender, edtMonteCarloExponent, NewExponent, 1);
    if NewExponent = 0 then
       Begin
       NewMessage := 'With the parameter set to ''0'' the random function' + chr(10 ) +
                     'will switch between choosing the upper and lower allowable extremes.' + chr(10) +
                     'That is unlikely to be the most useful.' + chr(10) +
                     'It is recommended to try setting a larger value (1 or greater).' + chr(10) +
                     'Press ''yes'' to accept the current values or ''Cancel'' to change them.';
       ShowMessage := true;
       End
    Else if NewExponent < 0.1 then
       Begin
       NewMessage := 'With the parameter set to a value less than ''0.1'' the random function' + chr(10 ) +
                     'will switch values mainly near the upper and lower allowable extremes.' + chr(10) +
                     'That is unlikely to be the most useful.' + chr(10) +
                     'It is recommended to try setting a larger value (1 or greater).' + chr(10) +
                     'Press ''yes'' to accept the current values or ''Cancel'' to change them.';
       ShowMessage := true;
       End
    Else if NewExponent > 5 then
       Begin
       NewMessage := 'With the parameter set to a value greater than ''5'' the random function' + chr(10 ) +
                     'will be very conservative, selecting values generally very close to the previous best fit.' + chr(10) +
                     'That might be OK if you are optimising a large number of variables' + chr(10) +
                     'and want to randomise a reasonable number of them each time, but it is a conservative option.' + chr(10) +
                     'Press ''yes'' to accept the current values or ''Cancel'' to change them.';
       ShowMessage := true;
       End
    Else
       ShowMessage := false;
    if ShowMessage then
       Answer := MessageDlg(NewMessage, mtInformation, [mbYes, mbCancel], 0);
    if (Answer = mrYes) or (not ShowMessage) then
       CheckMonteCarloExponentChange := true
    Else
       CheckMonteCarloExponentChange := false;
    End; {of Function ''}

    Function CheckRatioIndividualParameters(Sender: TObject): Boolean;
    var NewValue: Real48;
        ShowMessage: Boolean;
        Answer: Integer;
        NewMessage: String;
    begin
    frmMain.GetEdit(Sender, edtCriterion3, NewValue, 1);
    if NewValue < 1 then
       Begin
       NewMessage := 'With the parameter set to less than 1 the function will not' + chr(10 ) +
                     'be most effective in finding new parameters. That is unlikely to be useful.' + chr(10) +
                     'It is recommended to try setting a value greater than 1.' + chr(10) +
                     'Press ''yes'' to accept the current values or ''Cancel'' to change them.';
       ShowMessage := true;
       End
    Else
       ShowMessage := false;
    if ShowMessage then
       Answer := MessageDlg(NewMessage, mtInformation, [mbYes, mbCancel], 0);
    if (Answer = mrYes) or (not ShowMessage) then
       CheckRatioIndividualParameters := true
    Else
       CheckRatioIndividualParameters := false;
    End; {of Function 'CheckRatioIndividualParameters'}

begin
frmMain.GetInteger(Sender, edtEquilRuns, Control.ParFit.MaxIterations);
frmMain.GetEdit(Sender, edtCriterion1, Control.ParFit.Criterion1, 1000);
Obs.IncludeWeight := chkIncludeWeights.Checked;
FitParameter.CompareBest := chkBestIndividual.Checked;
If (CompareText(ExtractFilePath(edtObsFile.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
   edtObsFile.Text := ExtractFileName(edtObsFile.Text);
If UpperCase(ExtractFileExt(edtObsFile.Text)) <> '.OB!' then
   NewFile := edtObsFile.Text + '.OB!'
Else
   NewFile := edtObsFile.Text;
If NewFile <> Obs.ObservationFile then
   Begin
   Obs.ObservationFile := NewFile;
   frmFileIO.GetParameterFile(Obs.ObservationFile, '.OB!');
   End;
ReadObservationalData(Obs.ObservationFile);
frmMain.GetEdit(Sender, edtMonteCarloExponent, Control.ParFit.MonteCarloExponent, 1);
frmMain.GetInteger(Sender, edtMaxToRandomise, Control.ParFit.ParametersToRandomise);
frmMain.GetEdit(Sender, edtCriterion1, Control.ParFit.Criterion1, 100);
frmMain.GetEdit(Sender, edtCriterion2, Control.ParFit.Criterion2, 1e6);
frmMain.GetEdit(Sender, edtDelta, Control.ParFit.Delta, 100);
frmMain.GetEdit(Sender, edtMaxChange, Control.ParFit.MaxChange, 1);
if Control.ParFit.MaxChange > 1 then
   Control.ParFit.MaxChange := 1
Else if Control.ParFit.MaxChange < 0 then
   Control.ParFit.MaxChange := 0;
Control.ProjectHasChanged := true;
If not CheckMonteCarloExponentChange(Sender) then
   Begin
   ModalResult := mrNone;                          // abort save
   frmFittingParameters.FormShow(Sender);
   End
Else
   If FitParameter.CompareBest then
      Begin
      frmMain.GetEdit(Sender, edtCriterion3, Control.ParFit.Criterion3, 1);
      If not CheckRatioIndividualParameters (Sender) then
         Begin
         ModalResult := mrNone;                          // abort save
         frmFittingParameters.FormShow(Sender);
         End
      Else
         ModalResult := mrOK;
      End;
end;


procedure TfrmFittingParameters.chkBestIndividualClick(Sender: TObject);
begin
if chkBestIndividual.Checked then
   Begin
   lblCriterion3.Visible := true;
   edtCriterion3.Visible := true;
   End
Else
   Begin
   lblCriterion3.Visible := false;
   edtCriterion3.Visible := false;
   End;
end;

procedure TfrmFittingParameters.chkRunBatchClick(Sender: TObject);
begin
Obs.RunBatch := chkRunBatch.Checked;
if chkRunBatch.Checked then
   grpClimate.Visible := false
Else
   grpClimate.Visible := true;
end;

end.
