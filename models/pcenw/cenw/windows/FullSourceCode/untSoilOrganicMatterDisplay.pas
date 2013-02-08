unit untSoilOrganicMatterDisplay;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, DFSClrBn;

type
  TfrmSoilOrganicMatterDisplay = class(TForm)
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
  frmSoilOrganicMatterDisplay: TfrmSoilOrganicMatterDisplay;

implementation

{$R *.DFM}

uses
  untDeclarations, untMiscellaneous, untMain;

procedure TfrmSoilOrganicMatterDisplay.FormShow(Sender: TObject);
var
  cbEnabled: TCheckBox;
  clrButton: TdfsColorButton;
  edtMin, edtMax: TEdit;
  iCount, iEntries: integer;
  Value: real;
  ScreenVar: ScreenOptions;
begin
  // kill off the previous form elements - leave only 5 components
  while (Self.ComponentCount > 5) do
    Self.Components[5].Free;
  iCount := 32;
  iEntries := 0;
  // fill form with parameters
  for ScreenVar:=D_CLeafLitter to D_NLeached do
      iEntries := iEntries + 1;
  frmSoilOrganicMatterDisplay.Height := 120 + iEntries * 17;
  btnOK.Top := frmSoilOrganicMatterDisplay.Height - 60;
  btnCancel.Top := btnOK.Top;
  btnHelp.Top := btnOK.Top;
  for ScreenVar:=D_CLeafLitter to D_NLeached do
    begin
    // make the checkbox
    cbEnabled := TCheckBox.Create(Self);
    cbEnabled.Parent := Self;
    cbEnabled.Left := 8;
    cbEnabled.Top := iCount;
    cbEnabled.Width := 120;
    cbEnabled.Caption := ScreenVariableNames[ScreenVar];
    If ScreenRec.Choose[ScreenVar] then
       cbEnabled.Checked := true;
    // make the min and max boxes
    edtMin := TEdit.Create(Self);
    edtMin.Parent := Self;
{    If ScreenVar in [D_NMineral..D_NLeached, D_NLeafLitter..D_NAll_Litter] then
       edtMin.Digits := 2
    Else if ScreenVar in [D_CLeafLitter..D_CAll_Litter, D_SoilRespn, D_NMetabSurf..D_NResistant] then
       edtMin.Digits := 1
    Else // if "carbon pools" then
       edtMin.Digits := 0;}
    frmMain.FillEdit(Sender, edtMin, ScreenRec.LowRange[ScreenVar], 1);
    edtMin.Left := 140;
    edtMin.Top := iCount;
    edtMin.Width := 65;
    edtMax := TEdit.Create(Self);
    edtMax.Parent := Self;
{    If ScreenVar in [D_NMineral..D_NLeached, D_NLeafLitter..D_NAll_Litter] then
       edtMax.Digits := 2
    Else if ScreenVar in [D_CLeafLitter..D_CAll_Litter, D_SoilRespn, D_NMetabSurf..D_NResistant] then
       edtMax.Digits := 1
    Else // if "carbon pools" then
       edtMax.Digits := 0;}
    frmMain.FillEdit(Sender, edtMax, ScreenRec.UpRange[ScreenVar], 1);
    edtMax.Left := 212;
    edtMax.Top := iCount;
    edtMax.Width := 65;
    // make the colour selection box
    clrButton := TdfsColorButton.Create(Self);
    clrButton.Parent := Self;
    clrButton.Left := 284;
    clrButton.Top := iCount;
    clrButton.Height := 18;
    clrButton.Color := ScreenRec.Color[ScreenVar];
    iCount := iCount + 17;
    end;
end;

procedure TfrmSoilOrganicMatterDisplay.btnOKClick(Sender: TObject);
var
  iCount, iCol: integer;
  Value: real48;
  ScreenVar: ScreenOptions;
begin
  // save parameters
  iCount := 0;
  For ScreenVar := D_CLeafLitter to D_NLeached do
    begin
    ScreenRec.Choose[ScreenVar] := TCheckBox(Self.Components[iCount * 4 + 5]).Checked;
    frmMain.GetEdit(Sender, TEdit(Self.Components[iCount * 4 + 6]), ScreenRec.LowRange[ScreenVar], 1);
    frmMain.GetEdit(Sender, TEdit(Self.Components[iCount * 4 + 7]), ScreenRec.UpRange[ScreenVar], 1);
    iCol := TdfsColorButton(Self.Components[iCount * 4 + 8]).Color;
    ScreenRec.Color[ScreenVar] := iCol;
    iCount := iCount + 1;
    end;
  Control.ProjectHasChanged := TRUE;
  Control.ScreenHasChanged := TRUE;
end;

end.
