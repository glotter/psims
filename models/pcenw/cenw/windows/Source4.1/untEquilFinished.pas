{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmEquilFinished                                =
  =                                                              =
  =             Routines to display the outcomes of the          =
  =             routine to search for equilibrium conditions     =
  ================================================================
  = File      : untEquilFinished.PAS                             =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

unit untEquilFinished;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfrmEquilFinished = class(TForm)
    btnOK: TButton;
    btnHelp: TButton;
    btnCancel: TButton;
    lblFound: TLabel;
    lblFinal: TLabel;
    lblWhat_to_do: TLabel;
    lblSearchType: TLabel;
    edtTargetValue: TEdit;
    lblLine1: TLabel;
    edtIterations: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtFinalValue: TEdit;
    edtCriterion1: TEdit;
    Label3: TLabel;
    edtConverg1: TEdit;
    edtCriterion2: TEdit;
    Label4: TLabel;
    edtConverg2: TEdit;
    edtCriterion3: TEdit;
    Label5: TLabel;
    edtConverg3: TEdit;
    edtResult: TEdit;
    lblResult: TLabel;
    Label6: TLabel;
    Procedure FormShow(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmEquilFinished: TfrmEquilFinished;

implementation
{$R *.DFM}

uses
  untDeclarations, untMain;

Procedure TfrmEquilFinished.FormShow(Sender: TObject);
Begin
// fill form with parameters
If Derived.Equil.SolutionFound then
   Begin
   lblFound.Caption := 'was found after';
   lblfound.Color := clLime;
   End
Else
   Begin
   lblFound.Caption := 'was not found after';
   lblfound.Color := clRed;
   End;
If Control.Equil.EquilTarget = SOM then
   lblSearchType.Caption := 'Search for soil organic matter'
Else if Control.Equil.EquilTarget = LeafNConc then
   lblSearchType.Caption := 'Search for foliage nitrogen concentration'
Else if Control.Equil.EquilTarget = LeafNitrogen then
   lblSearchType.Caption := 'Search for foliage nitrogen amount'
Else if Control.Equil.EquilTarget = Leafmass then
   lblSearchType.Caption := 'Search for foliage mass';
frmMain.FillEdit(Sender, edtIterations, Derived.Equil.Iterations, 0);
frmMain.FillEdit(Sender, edtCriterion1, Control.Equil.Criterion1, 1);
frmMain.FillEdit(Sender, edtConverg1, Derived.Equil.Converg1, 1);
frmMain.FillEdit(Sender, edtCriterion2, Control.Equil.Criterion2, 1);
frmMain.FillEdit(Sender, edtConverg2, Derived.Equil.Converg2, 1);
frmMain.FillEdit(Sender, edtCriterion3, Control.Equil.Criterion3, 1);
frmMain.FillEdit(Sender, edtConverg3, Derived.Equil.Converg3, 1);
frmMain.FillEdit(Sender, edtTargetValue, Control.Equil.TargetValue, 1);
frmMain.FillEdit(Sender, edtFinalValue, Derived.Equil.SearchValue, 1);
case Control.Equil.EquilParameter of
     BiolNFix:  Begin
                frmMain.FillEdit(Sender, edtResult, Parameter.BiolFix, 1000 * Control.NConversion / Control.CConversion);
                lblResult.Caption := 'biological N fixation';
                End;
     NFraction: Begin
                frmMain.FillEdit(Sender, edtResult, Parameter.Nloss, 1);
                lblResult.Caption := 'fractional N loss';
                End;
     End;
End;

Procedure TfrmEquilFinished.btnOKClick(Sender: TObject);
begin
ModalResult := mrOK;
end;


end.
