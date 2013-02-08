{ ================================================================
  = Project   : CENW                                             =
  ================================================================
  = Modules   : TfrmWeatherDataDisplay                           =
  =                                                              =
  =             Interface routine to set real-time display       =
  =             of weather-related variables.                    =
  ================================================================
  = File      : untWeatherDataDisplay.PAS                        =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

unit untWeatherDataDisplay;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, DFSClrBn;

type
  TfrmWeatherDataDisplay = class(TForm)
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
  frmWeatherDataDisplay: TfrmWeatherDataDisplay;

implementation

{$R *.DFM}

uses
  untDeclarations, untMiscellaneous, untMain;

procedure TfrmWeatherDataDisplay.FormShow(Sender: TObject);
var
  cbEnabled: TCheckBox;
  clrButton: TdfsColorButton;
  edtMin, edtMax: TEdit;
  iCount, iEntries: integer;
  ScreenVar: ScreenOptions;
begin
  // kill off the previous form elements - leave only 5 Components
  while (Self.ComponentCount > 5) do
    Self.Components[5].Free;
  iCount := 32;
  iEntries := 0;
  // fill form with parameters
  For ScreenVar := D_Tmax to D_Dummy do
      iEntries := iEntries + 1;
  frmWeatherDataDisplay.Height := 120 + iEntries * 17;
  btnOK.Top := frmWeatherDataDisplay.Height - 60;
  btnCancel.Top := btnOK.Top;
  btnHelp.Top := btnOK.Top;
  For ScreenVar := D_Tmax to D_Dummy do
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
{    If ScreenVar in [D_Tmax..D_AbsHum, D_HeatSum..D_Snow] then
       edtMin.Digits := 0
    Else if ScreenVar in [D_RelHum..D_Drainage] then
       edtMin.Digits := 1
    Else // if Dummy variable then
       edtMin.Digits := 3;}
    frmMain.FillEdit(Sender, edtMin, ScreenRec.LowRange[ScreenVar], 1);
    edtMin.Left := 160;
    edtMin.Top := iCount;
    edtMin.Width := 65;
    edtMax := TEdit.Create(Self);
    edtMax.Parent := Self;
{    If ScreenVar in [D_Tmax..D_AbsHum, D_HeatSum..D_Snow] then
       edtMax.Digits := 0
    Else if ScreenVar in [D_RelHum..D_Drainage] then
       edtMax.Digits := 1
    Else // if Dummy variable then
       edtMax.Digits := 3;}
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
end;

procedure TfrmWeatherDataDisplay.btnOKClick(Sender: TObject);
var
  iCount, iCol: integer;
  ScreenVar: ScreenOptions;
begin
  // save parameters
  iCount := 0;
  For ScreenVar := D_Tmax to D_Dummy do
  begin
    ScreenRec.Choose[ScreenVar] := TCheckBox(Self.Components[iCount * 4 + 5]).Checked;
    frmMain.GetEdit(Sender, TEdit(Self.Components[iCount * 4 + 6]), ScreenRec.LowRange[ScreenVar], 1);
    frmMain.GetEdit(Sender, TEdit(Self.Components[iCount * 4 + 7]), ScreenRec.UpRange[ScreenVar], 1);
    iCol := TdfsColorButton(Self.Components[iCount * 4 + 8]).Color;
    ScreenRec.Color[ScreenVar] := iCol;
    iCount := iCount + 1;
  End;
//  SetColours;
  Control.ProjectHasChanged := TRUE;
  Control.ScreenHasChanged := TRUE;
end;

end.
