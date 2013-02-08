{ ================================================================
  = Project   : CENW                                             =
  ================================================================
  = Modules   : TfrmInitialPools                                 =
  =                                                              =
  =             Interface routine to allow users to              =
  =             change plant pools at the start of a run.        =
  ================================================================
  = File      : untInitialPools.PAS                              =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

unit untInitialPools;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, untDeclarations;

type

  TfrmInitialPools = class(TForm)
    GroupBox3: TGroupBox;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    lblLine1: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    edtSapWoodC: TEdit;
    Label3: TLabel;
    edtSapwoodN: TEdit;
    Label4: TLabel;
    edtHeartWoodC: TEdit;
    Label5: TLabel;
    edtHeartwoodN: TEdit;
    Label6: TLabel;
    edtLeavesC: TEdit;
    Label7: TLabel;
    edtFoliageN: TEdit;
    Label8: TLabel;
    edtFineRootC: TEdit;
    Label9: TLabel;
    edtFineRootN: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    edtCoarseRootC: TEdit;
    edtCoarseRootN: TEdit;
    Label12: TLabel;
    edtBranchesC: TEdit;
    Label13: TLabel;
    edtBranchN: TEdit;
    Label14: TLabel;
    edtBarkC: TEdit;
    Label15: TLabel;
    edtBarkN: TEdit;
    Label16: TLabel;
    edtFruitC: TEdit;
    Label17: TLabel;
    edtFruitN: TEdit;
    Label18: TLabel;
    edtPollenC: TEdit;
    Label19: TLabel;
    edtPollenN: TEdit;
    Label20: TLabel;
    edtSolubleC: TEdit;
    Label21: TLabel;
    edtSolubleN: TEdit;
    Label22: TLabel;
    edtReservesC: TEdit;
    Label23: TLabel;
    Label24: TLabel;
    edtReservesN: TEdit;
    edtPlantAge: TEdit;
    Label25: TLabel;
    edtStocking: TEdit;
    Label26: TLabel;
    edtStartDay: TEdit;
    Label27: TLabel;
    Label28: TLabel;
    edtStartMonth: TEdit;
    Label29: TLabel;
    edtStartYear: TEdit;
    Label30: TLabel;
    Label31: TLabel;
    edtHeight: TEdit;
    edtDBH: TEdit;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmInitialPools: TfrmInitialPools;

implementation

{$R *.DFM}

uses
  untMiscellaneous, untHelpConsts, untMain;

Procedure TfrmInitialPools.FormShow(Sender: TObject);
Begin
frmMain.FillEdit(Sender, edtSapWoodC, Control.Initial.SapWood[C], Control.CConversion);
frmMain.FillEdit(Sender, edtHeartWoodC, Control.Initial.HeartWood[C], Control.CConversion);
frmMain.FillEdit(Sender, edtCoarseRootC, Control.Initial.CoarseRoot[C], Control.CConversion);
frmMain.FillEdit(Sender, edtFineRootC, Control.Initial.FineRoot[C], Control.CConversion);
frmMain.FillEdit(Sender, edtBranchesC, Control.Initial.Branches[C], Control.CConversion);
frmMain.FillEdit(Sender, edtBarkC, Control.Initial.Bark[C], Control.CConversion);
frmMain.FillEdit(Sender, edtLeavesC, Control.Initial.Leaves[C], Control.CConversion);
frmMain.FillEdit(Sender, edtPollenC, Control.Initial.Pollen[C], Control.CConversion);
frmMain.FillEdit(Sender, edtFruitC, Control.Initial.Fruit[C], Control.CConversion);
frmMain.FillEdit(Sender, edtSolubleC, Control.Initial.Soluble[C], Control.CConversion);
frmMain.FillEdit(Sender, edtReservesC, Control.Initial.Reserves[C], Control.CConversion);
frmMain.FillEdit(Sender, edtSapWoodN, Control.Initial.SapWood[N], 1);
frmMain.FillEdit(Sender, edtHeartWoodN, Control.Initial.HeartWood[N], 1);
frmMain.FillEdit(Sender, edtCoarseRootN, Control.Initial.CoarseRoot[N], 1);
frmMain.FillEdit(Sender, edtFineRootN, Control.Initial.FineRoot[N], 1);
frmMain.FillEdit(Sender, edtBranchN, Control.Initial.Branches[N], 1);
frmMain.FillEdit(Sender, edtBarkN, Control.Initial.Bark[N], 1);
frmMain.FillEdit(Sender, edtFoliageN, Control.Initial.Leaves[N], 1);
frmMain.FillEdit(Sender, edtPollenN, Control.Initial.Pollen[N], 1);
frmMain.FillEdit(Sender, edtFruitN, Control.Initial.Fruit[N], 1);
frmMain.FillEdit(Sender, edtSolubleN, Control.Initial.Soluble[N], 1);
frmMain.FillEdit(Sender, edtReservesN, Control.Initial.Reserves[N], 1);
frmMain.FillEdit(Sender, edtPlantAge, Control.Initial.Age, 0);
frmMain.FillEdit(Sender, edtStocking, Control.Initial.Stocking, 0);
frmMain.FillEdit(Sender, edtHeight, Control.Initial.Height, 1);
frmMain.FillEdit(Sender, edtDBH, Control.Initial.DBH, 1);
frmMain.FillEdit(Sender, edtStartDay, Control.Initial.Days, 0);
frmMain.FillEdit(Sender, edtStartMonth, Control.Initial.Months, 0);
frmMain.FillEdit(Sender, edtStartYear, Control.Initial.Years, 0);
end;

Procedure TfrmInitialPools.btnOKClick(Sender: TObject);
Begin
frmMain.GetEdit(Sender, edtSapWoodC, Control.Initial.SapWood[C], Control.CConversion);
frmMain.GetEdit(Sender, edtHeartWoodC, Control.Initial.HeartWood[C], Control.CConversion);
frmMain.GetEdit(Sender, edtCoarseRootC, Control.Initial.CoarseRoot[C], Control.CConversion);
frmMain.GetEdit(Sender, edtFineRootC, Control.Initial.FineRoot[C], Control.CConversion);
frmMain.GetEdit(Sender, edtBranchesC, Control.Initial.Branches[C], Control.CConversion);
frmMain.GetEdit(Sender, edtBarkC, Control.Initial.Bark[C], Control.CConversion);
frmMain.GetEdit(Sender, edtLeavesC, Control.Initial.Leaves[C], Control.CConversion);
frmMain.GetEdit(Sender, edtPollenC, Control.Initial.Pollen[C], Control.CConversion);
frmMain.GetEdit(Sender, edtFruitC, Control.Initial.Fruit[C], Control.CConversion);
frmMain.GetEdit(Sender, edtSolubleC, Control.Initial.Soluble[C], Control.CConversion);
frmMain.GetEdit(Sender, edtReservesC, Control.Initial.Reserves[C], Control.CConversion);
frmMain.GetEdit(Sender, edtSapWoodN, Control.Initial.SapWood[N], 1);
frmMain.GetEdit(Sender, edtHeartWoodN, Control.Initial.HeartWood[N], 1);
frmMain.GetEdit(Sender, edtCoarseRootN, Control.Initial.CoarseRoot[N], 1);
frmMain.GetEdit(Sender, edtFineRootN, Control.Initial.FineRoot[N], 1);
frmMain.GetEdit(Sender, edtBranchN, Control.Initial.Branches[N], 1);
frmMain.GetEdit(Sender, edtBarkN, Control.Initial.Bark[N], 1);
frmMain.GetEdit(Sender, edtFoliageN, Control.Initial.Leaves[N], 1);
frmMain.GetEdit(Sender, edtPollenN, Control.Initial.Pollen[N], 1);
frmMain.GetEdit(Sender, edtFruitN, Control.Initial.Fruit[N], 1);
frmMain.GetEdit(Sender, edtSolubleN, Control.Initial.Soluble[N], 1);
frmMain.GetEdit(Sender, edtReservesN, Control.Initial.Reserves[N], 1);
frmMain.GetInteger(Sender, edtPlantAge, Control.Initial.Age);
frmMain.GetEdit(Sender, edtStocking, Control.Initial.Stocking, 1);
frmMain.GetEdit(Sender, edtHeight, Control.Initial.Height, 1);
frmMain.GetEdit(Sender, edtDBH, Control.Initial.DBH, 1);
frmMain.GetInteger(Sender, edtStartDay, Control.Initial.Days);
frmMain.GetInteger(Sender, edtStartMonth, Control.Initial.Months);
frmMain.GetInteger(Sender, edtStartYear, Control.Initial.Years);
Control.ProjectHasChanged := TRUE;
End;

procedure TfrmInitialPools.btnHelpClick(Sender: TObject);
begin
  // show the appropriate help page
    Application.HelpContext(idm_ResetPools);
end;

end.
