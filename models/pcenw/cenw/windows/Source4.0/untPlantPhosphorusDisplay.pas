{ ================================================================
  = Project   : CENW                                             =
  ================================================================
  = Modules   : TfrmPlantPhosphorusDisplay                       =
  =                                                              =
  =             Interface routine to set real-time display       =
  =             of nitrogen pools.                               =
  ================================================================
  = File      : untPlantPhosphorusDisplay.PAS                    =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untPlantPhosphorusDisplay;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, DFSClrBn;

type
  TfrmPlantPhosphorusDisplay = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    lblMin: TLabel;
    lblMax: TLabel;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPlantPhosphorusDisplay: TfrmPlantPhosphorusDisplay;

implementation

{$R *.DFM}

uses
  untDeclarations, untMiscellaneous, untMain;

procedure TfrmPlantPhosphorusDisplay.FormShow(Sender: TObject);
var
  cbEnabled: TCheckBox;
  clrButton: TdfsColorButton;
  edtMin, edtMax: TEdit;
  iCount, iEntries: integer;
  ScreenVar: ScreenOptions;
  Begin
  // kill off the previous form elements - leave only 5 Components
  while (Self.ComponentCount > 5) do
    Self.Components[5].Free;
  iCount := 32;
  iEntries := 0;
  // fill form with parameters
  for ScreenVar := D_NConc to D_NSum do
      iEntries := iEntries + 1;
  frmPlantPhosphorusDisplay.Height := 120 + iEntries * 17;
  btnOK.Top := frmPlantPhosphorusDisplay.Height - 60;
  btnCancel.Top := btnOK.Top;
  btnHelp.Top := btnOK.Top;
  For ScreenVar := D_PConc to D_PMineral do
    Begin
    cbEnabled := TCheckBox.Create(Self);
    cbEnabled.Parent := Self;
    cbEnabled.Left := 8;
    cbEnabled.Top := iCount;
    cbEnabled.Caption := ScreenVariableNames[ScreenVar];
    If ScreenRec.Choose[ScreenVar] then
      cbEnabled.Checked := true;
    edtMin := TEdit.Create(Self);
    edtMin.Parent := Self;
    frmMain.FillEdit(Sender, edtMin, ScreenRec.LowRange[ScreenVar], 1);
    edtMin.Left := 160;
    edtMin.Top := iCount;
    edtMin.Width := 65;
    edtMax := TEdit.Create(Self);
    edtMax.Parent := Self;
    frmMain.FillEdit(Sender, edtMax, ScreenRec.UpRange[ScreenVar], 1);
    edtMax.Left := 232;
    edtMax.Top := iCount;
    edtMax.Width := 65;
    // make the colour selection box
    clrButton := TdfsColorButton.Create(Self);
    clrButton.Parent := Self;
    clrButton.Left := 304;
    clrButton.Top := iCount;
    clrButton.Height := 18;
    clrButton.Color := ScreenRec.Color[ScreenVar];
    iCount := iCount + 17;
    End;
  End;

procedure TfrmPlantPhosphorusDisplay.btnOKClick(Sender: TObject);
var
  iCount, iCol: integer;
  ScreenVar: ScreenOptions;
  Begin
  // save parameters
  iCount := 0;
  For ScreenVar := D_PConc to D_PMineral do
      Begin
      ScreenRec.Choose[ScreenVar] := TCheckBox(Self.Components[iCount * 4 + 5]).Checked;
      frmMain.GetEdit(Sender, TEdit(Self.Components[iCount * 4 + 6]), ScreenRec.LowRange[ScreenVar], 1);
      frmMain.GetEdit(Sender, TEdit(Self.Components[iCount * 4 + 7]), ScreenRec.UpRange[ScreenVar], 1);
      iCol := TdfsColorButton(Self.Components[iCount * 4 + 8]).Color;
      ScreenRec.Color[ScreenVar] := iCol;
      iCount := iCount + 1;
      End;
  Control.ProjectHasChanged := TRUE;
  Control.ScreenHasChanged := TRUE;
  End;

end.
