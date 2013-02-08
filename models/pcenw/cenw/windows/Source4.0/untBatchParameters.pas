{ ================================================================
  = Project   : Cenw                                             =
  ================================================================
  = Module    : TfrmBatchParameters                              =
  =                                                              =
  =             Routines to give some information before         =
  =             running the program in batch mode.               =
  ================================================================
  = File      : untBatchParameters.PAS                           =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untBatchParameters;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TfrmBatchParameters = class(TForm)
    grpBatch: TGroupBox;
    edtBatch: TEdit;
    btnBatch: TButton;
    dlgOpenBatch: TOpenDialog;
    grpDisplayNum: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TBitBtn;
    edtBatchRuns: TEdit;
    grpLength: TGroupBox;
    lblYears: TLabel;
    lblMonths: TLabel;
    lblDays: TLabel;
    edtYears: TEdit;
    edtMonths: TEdit;
    edtDays: TEdit;
    btnGotoList: TButton;
    Procedure FormShow(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
    Procedure btnBatchClick(Sender: TObject);
    procedure btnGotoListClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var frmBatchParameters: TfrmBatchParameters;

implementation
{$R *.DFM}

uses untDeclarations, untMain, untBatchList;

Procedure TfrmBatchParameters.FormShow(Sender: TObject);
var Years, Months, Days: Integer;
Begin
edtBatch.Text := Control.BatchFile;
Years := Control.StartBatchSave div 365;
Months := (Control.StartBatchSave - 365 * Years) div 30;
if Months < 0 then
   Months := 0;
Days := Control.StartBatchSave - 365 * Years - 30 * Months;
if Days < 0 then
   Days := 0;
frmMain.FillEdit(Sender, edtYears, Years, 0);
frmMain.FillEdit(Sender, edtMonths, Months, 0);
frmMain.FillEdit(Sender, edtDays, Days, 0);
frmMain.FillEdit(Sender, edtBatchRuns, Control.BatchCalcs, 0);
btnOK.SetFocus;
End;

procedure TfrmBatchParameters.btnGotoListClick(Sender: TObject);
begin
frmBatchList.ShowModal;
end;

Procedure TfrmBatchParameters.btnOKClick(Sender: TObject);
var Years, Months, Days: Integer;
Begin
Control.BatchFile := edtBatch.Text;
Control.BatchMode := true;
frmMain.GetInteger(Sender, edtYears, Years);
frmMain.GetInteger(Sender, edtMonths, Months);
frmMain.GetInteger(Sender, edtDays, Days);
Control.StartBatchSave := 365 * Years + 30 * Months + Days;
frmMain.GetInteger(Sender, edtBatchRuns, Control.BatchCalcs);
ModalResult := mrOK;
End;


Procedure TfrmBatchParameters.btnBatchClick(Sender: TObject);
Begin
dlgOpenBatch.FileName := edtBatch.Text;
If (dlgOpenBatch.Execute) then
    Begin
    edtBatch.Text := dlgOpenBatch.FileName;
    If (CompareText(ExtractFilePath(edtBatch.Text), ExtractFilePath(Control.BatchFile)) = 0) then
       edtBatch.Text := ExtractFileName(edtBatch.Text);
    SetCurrentDir(Control.ProjectDirectory);
    End;
End;

End.
