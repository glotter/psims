{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmGenericDisplay                               =
  =                                                              =
  =             Interface routine to set run-time display        =
  =             of various parameters.                           =
  ================================================================
  = File      : untGenericDisplay.PAS                            =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untGenericDisplay;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, CheckLst, DFSClrBn;

Const BaseElements = 5;
type
  TfrmGenericDisplay = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    lblMin: TLabel;
    lblMax: TLabel;
    Procedure FormShow(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var frmGenericDisplay: TfrmGenericDisplay;

implementation

{$R *.DFM}

uses untDeclarations, untMain;

Procedure TfrmGenericDisplay.FormShow(Sender: TObject);
var cbEnabled: TCheckBox;
    clrButton: TdfsColorButton;
    edtMin, edtMax: TEdit;
    iCount, iEntries: integer;
    ScreenVar: ScreenOptions;
Begin
while (Self.ComponentCount > BaseElements) do
    Self.Components[BaseElements].Free;
iCount := 32;
iEntries := 0;
For ScreenVar := GenericOutput.FirstDisplay to GenericOutput.LastDisplay do
    iEntries := iEntries + 1;
frmGenericDisplay.Height := 120 + iEntries * 17;
frmGenericDisplay.Text := GenericOutput.Title;
btnOK.Top := frmGenericDisplay.Height - 60;
btnCancel.Top := btnOK.Top;
btnHelp.Top := btnOK.Top;
For ScreenVar := GenericOutput.FirstDisplay to GenericOutput.LastDisplay do
    Begin
    cbEnabled := TCheckBox.Create(Self);
    cbEnabled.Parent := Self;
    cbEnabled.Left := 8;
    cbEnabled.Top := iCount;
    cbEnabled.Caption := ScreenVariableNames[ScreenVar];
    If ScreenRec.Choose[ScreenVar] then
       cbEnabled.Checked := true;
    if ScreenVar = GenericOutput.FirstDisplay then
       cbEnabled.SetFocus;
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
    clrButton := TdfsColorButton.Create(Self);
    clrButton.Parent := Self;
    clrButton.Left := 304;
    clrButton.Top := iCount;
    clrButton.Height := 18;
    clrButton.Color := ScreenRec.Color[ScreenVar];
    iCount := iCount + 17;
    End;
End;

Procedure TfrmGenericDisplay.btnOKClick(Sender: TObject);
var iCount: integer;
    ScreenVar: ScreenOptions;
Begin
iCount := 0;
For ScreenVar := GenericOutput.FirstDisplay to GenericOutput.LastDisplay do
    Begin
    ScreenRec.Choose[ScreenVar] := TCheckBox(Self.Components[iCount * 4 + 5]).Checked;
    frmMain.GetEdit(Sender, TEdit(Self.Components[iCount * 4 + 6]), ScreenRec.LowRange[ScreenVar], 1);
    frmMain.GetEdit(Sender, TEdit(Self.Components[iCount * 4 + 7]), ScreenRec.UpRange[ScreenVar], 1);
    ScreenRec.Color[ScreenVar] := TdfsColorButton(Self.Components[iCount * 4 + 8]).Color;
    iCount := iCount + 1;
    End;
Control.ProjectHasChanged := TRUE;
Control.ScreenHasChanged := TRUE;
end;

end.
