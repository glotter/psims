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
  = Version   : 3.1                                              =
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
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnBatchClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBatchParameters: TfrmBatchParameters;

implementation
{$R *.DFM}

uses
  untDeclarations, untMain;

Procedure TfrmBatchParameters.FormShow(Sender: TObject);
Begin
edtBatch.Text := Control.BatchFile;
frmMain.FillEdit(Sender, edtBatchRuns, Control.BatchCalcs, 0);
End;

Procedure TfrmBatchParameters.btnOKClick(Sender: TObject);
Begin
Control.BatchFile := edtBatch.Text;
Control.BatchMode := true;
frmMain.GetInteger(Sender, edtBatchRuns, Control.BatchCalcs);
Control.ProjectHasChanged := true;
ModalResult := mrOK;
End;


Procedure TfrmBatchParameters.btnBatchClick(Sender: TObject);
Begin
// select a new batch file
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
