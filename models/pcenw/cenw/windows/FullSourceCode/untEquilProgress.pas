{ ================================================================
  = Project   : CENW                                             =
  ================================================================
  = Modules   : TfrmEquilProgress                                =
  =                                                              =
  =             Routines to handle a progress window that is     =
  =             displayed during runs to search for              =
  =             equilibrium conditions                           =
  ================================================================
  = File      : untEquilProgress.PAS                             =
  =                                                              =
  = Version   : 3.1                                              =
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
    procedure btnCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnManualUpdateClick(Sender: TObject);
    procedure edtBoostResistantedtNumberChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    bStopped: boolean;
    bUpdate: Boolean;
    iUpdateRate: Integer;
    procedure Start;
    procedure Stop;
    procedure UpdateInfo(Sender: TObject);

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

procedure TfrmEquilProgress.Stop;
begin
  // hide the progress window
  Hide;
end;

procedure TfrmEquilProgress.UpdateInfo;
begin
FillEdit(Sender, edtSearchValue, Derived.Equil.SearchValue, Black);
FillEdit(Sender, edtTargetValue, Control.Equil.TargetValue, Black);
If Control.Equil.EquilParameter = BiolNFix then
   FillEdit(Sender, edtSearchVariable, Parameter.BiolFix * 1000 * Control.NConversion / Control.CConversion, Black)
Else // if Control.Equil.EquilParameter = NFraction then
   FillEdit(Sender, edtSearchVariable, Parameter.Nloss, Black);
If abs(Derived.Equil.Converg1) < Control.Equil.Criterion1 then
   FillEdit(Sender, edtConverg1, Derived.Equil.Converg1, Green)
Else if abs(Derived.Equil.Converg1) < (3.33 * Control.Equil.Criterion1) then
   FillEdit(Sender, edtConverg1, Derived.Equil.Converg1, Blue)
Else if abs(Derived.Equil.Converg1) < (10 * Control.Equil.Criterion1) then
   FillEdit(Sender, edtConverg1, Derived.Equil.Converg1, Yellow)
Else
   FillEdit(Sender, edtConverg1, Derived.Equil.Converg1, Red);
FillEdit(Sender, edtLimitConverg1, Control.Equil.Criterion1, Black);
If abs(Derived.Equil.Converg2) < Control.Equil.Criterion2 then
   FillEdit(Sender, edtConverg2, Derived.Equil.Converg2, Green)
Else if abs(Derived.Equil.Converg2) < (3.33 * Control.Equil.Criterion2) then
   FillEdit(Sender, edtConverg2, Derived.Equil.Converg2, Blue)
Else if abs(Derived.Equil.Converg2) < (10 * Control.Equil.Criterion2) then
   FillEdit(Sender, edtConverg2, Derived.Equil.Converg2, Yellow)
Else
   FillEdit(Sender, edtConverg2, Derived.Equil.Converg2, Red);
FillEdit(Sender, edtLimitConverg2, Control.Equil.Criterion2, Black);
If abs(Derived.Equil.Converg3) < Control.Equil.Criterion3 then
   FillEdit(Sender, edtConverg3, Derived.Equil.Converg3, Green)
Else if abs(Derived.Equil.Converg3) < (3.33 * Control.Equil.Criterion3) then
   FillEdit(Sender, edtConverg3, Derived.Equil.Converg3, Blue)
Else if abs(Derived.Equil.Converg3) < (10 * Control.Equil.Criterion3) then
   FillEdit(Sender, edtConverg3, Derived.Equil.Converg3, Yellow)
Else
   FillEdit(Sender, edtConverg3, Derived.Equil.Converg3, Red);
FillEdit(Sender, edtLimitConverg3, Control.Equil.Criterion3, Black);
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
FillEdit(Sender, edtBoostResistant, Control.Equil.BoostResistant, Black);
Show;
End;

procedure TfrmEquilProgress.btnCancelClick(Sender: TObject);
begin
// user pressed Cancel, so raise a flag for processing to check
Control.EndEquil := true;
end;

procedure TfrmEquilProgress.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // if the user closes this window, then stop the run
  Action := caHide;
  bStopped := true;
end;

procedure TfrmEquilProgress.btnManualUpdateClick(Sender: TObject);
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

procedure TfrmEquilProgress.edtBoostResistantedtNumberChange(Sender: TObject);
begin
frmMain.GetEdit(Sender, edtBoostResistant, Control.Equil.BoostResistant, 1);
frmEquilProgress.UpdateInfo(Sender);
end;

end.

