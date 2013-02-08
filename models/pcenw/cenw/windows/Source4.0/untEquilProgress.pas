{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmEquilProgress                                =
  =                                                              =
  =             Routines to handle a progress window that is     =
  =             displayed during runs to search for              =
  =             equilibrium conditions                           =
  ================================================================
  = File      : untEquilProgress.PAS                             =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untEquilProgress;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Buttons, untDeclarations;

type
  TfrmEquilProgress = class(TForm)
    btnCancel: TBitBtn;
    btnManualUpdate: TButton;
    edtSearchValue: TEdit;
    lblLine1: TLabel;
    edtTargetValue: TEdit;
    Label1: TLabel;
    edtSearchVariable: TEdit;
    edtConverg1: TEdit;
    Label2: TLabel;
    edtLimitConverg1: TEdit;
    Label3: TLabel;
    edtConverg2: TEdit;
    edtLimitConverg2: TEdit;
    Label4: TLabel;
    edtConverg3: TEdit;
    edtLimitConverg3: TEdit;
    edtIterations: TEdit;
    Label5: TLabel;
    edtMaxIterations: TEdit;
    Label6: TLabel;
    edtGoodCount: TEdit;
    edtLimitGoodCount: TEdit;
    edtBoostResistant: TEdit;
    Label7: TLabel;
    lblPTargetInfo: TLabel;
    edtPTargetValue: TEdit;
    edtPlimitTarget: TEdit;
    edtDelta: TEdit;
    Label8: TLabel;
    edtPDelta: TEdit;
    lblP: TLabel;
    edtPRestrict: TEdit;
    lblPRestrict: TLabel;
    lblOscillations: TLabel;
    chkHoldPInput: TCheckBox;
    Procedure btnCancelClick(Sender: TObject);
    Procedure FormClose(Sender: TObject; var Action: TCloseAction);
    Procedure btnManualUpdateClick(Sender: TObject);
    Procedure edtBoostResistantedtNumberChange(Sender: TObject);
    procedure chkHoldPInputClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    bStopped: boolean;
    bUpdate: Boolean;
    iUpdateRate: Integer;
    Procedure Start;
    Procedure Stop;
    Procedure UpdateInfo(Sender: TObject);

  end;

var
  frmEquilProgress: TfrmEquilProgress;

implementation

uses untEquilAdjust, untMain;

{$R *.DFM}

   Procedure SetColour(edtBox: TEdit; Col: ColorOptions);
   Begin
   If Col = Red then
      edtBox.Color := clRed
   Else if Col = Green then
      edtBox.Color := clLime
   Else if Col = Blue then
      edtBox.Color := clAqua
   Else if Col = Yellow then
      edtBox.Color := clYellow
   Else
      edtBox.Color := clWindow;
   End;

   Procedure FillEdit(Sender: TObject; edtBox: TEdit; fValue: Real48; Col: ColorOptions);
   Begin
   SetColour(edtBox, Col);
   frmMain.FillEdit(Sender, edtBox, fValue, 1);
   End;

   Procedure FillInteger(Sender: TObject; edtBox: TEdit; iValue: Integer; Col: ColorOptions);
   Begin
   SetColour(edtBox, Col);
   frmMain.FillEdit(Sender, edtBox, iValue, 0);
   End;

Procedure TfrmEquilProgress.Start;
begin
  // initialise and show the progress window
  bStopped := false;
//  Show;
end;

Procedure TfrmEquilProgress.Stop;
begin
  // hide the progress window
  Hide;
end;

Procedure TfrmEquilProgress.UpdateInfo;
begin
FillEdit(Sender, edtSearchValue, Derived.Equil.SearchValue, Black);
FillEdit(Sender, edtTargetValue, Control.Equil.TargetValue, Black);
If Control.Equil.EquilParameter = BiolNFix then
   FillEdit(Sender, edtSearchVariable, Parameter.BiolFix * 1000 * Control.NConversion / Control.CConversion, Black)
Else // if Control.Equil.EquilParameter = NFraction then
   FillEdit(Sender, edtSearchVariable, Parameter.Nloss, Black);
If abs(Derived.Equil.Converg1) < Control.Equil.Criterion1 then
   FillEdit(Sender, edtConverg1, Derived.Equil.Converg1 * 1000, Green)
Else if abs(Derived.Equil.Converg1) < (3.33 * Control.Equil.Criterion1) then
   FillEdit(Sender, edtConverg1, Derived.Equil.Converg1 * 1000, Blue)
Else if abs(Derived.Equil.Converg1) < (10 * Control.Equil.Criterion1) then
   FillEdit(Sender, edtConverg1, Derived.Equil.Converg1 * 1000, Yellow)
Else
   FillEdit(Sender, edtConverg1, Derived.Equil.Converg1 * 1000, Red);
FillEdit(Sender, edtLimitConverg1, Control.Equil.Criterion1 * 1000, Black);
If abs(Derived.Equil.Converg2) < Control.Equil.Criterion2 then
   FillEdit(Sender, edtConverg2, Derived.Equil.Converg2 * 1000, Green)
Else if abs(Derived.Equil.Converg2) < (3.33 * Control.Equil.Criterion2) then
   FillEdit(Sender, edtConverg2, Derived.Equil.Converg2 * 1000, Blue)
Else if abs(Derived.Equil.Converg2) < (10 * Control.Equil.Criterion2) then
   FillEdit(Sender, edtConverg2, Derived.Equil.Converg2 * 1000, Yellow)
Else
   FillEdit(Sender, edtConverg2, Derived.Equil.Converg2 * 1000, Red);
FillEdit(Sender, edtLimitConverg2, Control.Equil.Criterion2 * 1000, Black);
If abs(Derived.Equil.Converg3) < Control.Equil.Criterion3 then
   FillEdit(Sender, edtConverg3, Derived.Equil.Converg3 * 1000, Green)
Else if abs(Derived.Equil.Converg3) < (3.33 * Control.Equil.Criterion3) then
   FillEdit(Sender, edtConverg3, Derived.Equil.Converg3 * 1000, Blue)
Else if abs(Derived.Equil.Converg3) < (10 * Control.Equil.Criterion3) then
   FillEdit(Sender, edtConverg3, Derived.Equil.Converg3 * 1000, Yellow)
Else
   FillEdit(Sender, edtConverg3, Derived.Equil.Converg3 * 1000, Red);
FillEdit(Sender, edtLimitConverg3, Control.Equil.Criterion3 * 1000, Black);
FillInteger(Sender, edtIterations, Derived.Equil.Iterations, Black);
FillInteger(Sender, edtMaxIterations, Control.Equil.MaxIterations, Black);
If Derived.Equil.GoodCount = 0 then
   FillInteger(Sender, edtGoodCount, Derived.Equil.GoodCount, Red)
Else if Derived.Equil.GoodCount < 4 then
   FillInteger(Sender, edtGoodCount, Derived.Equil.GoodCount, Yellow)
Else if Derived.Equil.GoodCount < 7 then
   FillInteger(Sender, edtGoodCount, Derived.Equil.GoodCount, Blue)
Else
   FillInteger(Sender, edtGoodCount, Derived.Equil.GoodCount, Green);
FillEdit(Sender, edtLimitGoodCount, Control.Equil.MaxGoodCount, Black);
If Derived.Equil.Delta = Control.Equil.DeltaMin then
   FillEdit(Sender, edtDelta, Derived.Equil.Delta, Red)
Else if Derived.Equil.Delta = Control.Equil.DeltaMax then
   FillEdit(Sender, edtDelta, Derived.Equil.Delta, Green)
Else if Derived.Equil.Delta > 0.5 * (Control.Equil.DeltaMax - Control.Equil.DeltaMin) then
   FillEdit(Sender, edtDelta, Derived.Equil.Delta, Blue)
Else
   FillEdit(Sender, edtDelta, Derived.Equil.Delta, Yellow);
if Control.IncludeP then
   Begin
   If Derived.Equil.PDelta = Control.Equil.DeltaMin then
      FillEdit(Sender, edtPDelta, Derived.Equil.PDelta, Red)
   Else if Derived.Equil.PDelta = Control.Equil.DeltaMax then
      FillEdit(Sender, edtPDelta, Derived.Equil.PDelta, Green)
   Else if Derived.Equil.PDelta > 0.5 * (Control.Equil.DeltaMax - Control.Equil.DeltaMin) then
      FillEdit(Sender, edtPDelta, Derived.Equil.PDelta, Blue)
   Else
      FillEdit(Sender, edtPDelta, Derived.Equil.PDelta, Yellow);
   edtPDelta.Visible := true;
   lblP.Visible := true;
   If Derived.Equil.PDelta = Control.Equil.DeltaMin then
      FillEdit(Sender, edtPRestrict, Derived.Equil.PRestrict, Red)
   Else if Derived.Equil.PDelta = Control.Equil.DeltaMax then
      FillEdit(Sender, edtPRestrict, Derived.Equil.PRestrict, Green)
   Else if Derived.Equil.PDelta > 0.5 * (Control.Equil.DeltaMax - Control.Equil.DeltaMin) then
      FillEdit(Sender, edtPRestrict, Derived.Equil.PRestrict, Blue)
   Else
      FillEdit(Sender, edtPRestrict, Derived.Equil.PRestrict, Yellow);
   edtPRestrict.Visible := true;
   lblPRestrict.Visible := true;
   frmEquilProgress.Width := 378;
   End
Else
   Begin
   edtPDelta.Visible := false;
   lblP.Visible := false;
   edtPRestrict.Visible := false;
   lblPRestrict.Visible := false;
   frmEquilProgress.Width := 307;
   End;
FillEdit(Sender, edtBoostResistant, Control.Equil.BoostResistant, Black);
FillEdit(Sender, edtPlimitTarget, Control.Equil.TargetPlimit, Black);
If abs(1 - Control.Equil.TargetPlimit / Derived.Plimit) > 0.3 then
   FillEdit(Sender, edtPTargetValue, Derived.Plimit, Red)
Else if abs(1 - Control.Equil.TargetPlimit / Derived.Plimit) > 0.1 then
   FillEdit(Sender, edtPTargetValue, Derived.Plimit, Yellow)
Else if abs(1 - Control.Equil.TargetPlimit / Derived.Plimit) > 0.05 then
   FillEdit(Sender, edtPTargetValue, Derived.Plimit, Blue)
Else {if abs(1 - Control.Equil.TargetPlimit / Derived.Plimit) > 0.01 then}
   FillEdit(Sender, edtPTargetValue, Derived.Plimit, Green);
chkHoldPInput.Checked := Control.Equil.HoldPInput;
if Control.IncludeP then
   Begin
   lblPTargetInfo.Visible := true;
   edtPTargetValue.Visible := true;
   edtPlimitTarget.Visible := true;
   chkHoldPInput.Visible := true;
   End
Else
   Begin
   lblPTargetInfo.Visible := false;
   edtPTargetValue.Visible := false;
   edtPlimitTarget.Visible := false;
   chkHoldPInput.Visible := false;
   End;
If Derived.Equil.Oscillating then
   lblOscillations.Visible := true
Else
   lblOscillations.Visible := false;
Show;
End;

Procedure TfrmEquilProgress.btnCancelClick(Sender: TObject);
begin
// user pressed Cancel, so raise a flag for processing to check
Control.EndEquil := true;
end;

Procedure TfrmEquilProgress.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // if the user closes this window, then stop the run
  Action := caHide;
  bStopped := true;
end;

Procedure TfrmEquilProgress.btnManualUpdateClick(Sender: TObject);
begin
Application.CreateForm(TfrmEquilAdjust, frmEquilAdjust);
with frmEquilAdjust do
  Begin
  frmMain.FillEdit(Sender, edtAdjustVariable, 100, 1);
  frmMain.FillEdit(Sender, edtAdjustSlow, 100, 1);
  frmMain.FillEdit(Sender, edtAdjustResistant, 100, 1);
  if frmequiladjust.ShowModal=mrOK then
     Begin
     end;
  end;
  frmequiladjust.free;
end;

procedure TfrmEquilProgress.chkHoldPInputClick(Sender: TObject);
begin
Control.Equil.HoldPInput := chkHoldPInput.Checked;
end;

Procedure TfrmEquilProgress.edtBoostResistantedtNumberChange(Sender: TObject);
begin
frmMain.GetEdit(Sender, edtBoostResistant, Control.Equil.BoostResistant, 1);
frmEquilProgress.UpdateInfo(Sender);
end;

end.

