{ ================================================================
  = Project   : CENW                                             =
  ================================================================
  = Modules   : TfrmAllocationParameters                         =
  =                                                              =
  =             Edit window to change allocation parameters      =
  ================================================================
  = File      : untAllocationParameters.PAS                      =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

unit untAllocationParameters;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TfrmAllocationParameters = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    grpAllocation_Terms: TGroupBox;
    grpAllocation_Ratios: TGroupBox;
    grpStem_Allom_Paras: TGroupBox;
    grpAllometric_H_vs_dbh: TGroupBox;
    grpNRatios: TGroupBox;
    lblLine1: TLabel;
    edtC_FoliageAlloc: TEdit;
    edtC_BranchAlloc: TEdit;
    Label1: TLabel;
    edtC_SapwoodAlloc: TEdit;
    Label2: TLabel;
    edtC_BarkAlloc: TEdit;
    Label3: TLabel;
    edtC_FineRootAlloc: TEdit;
    Label4: TLabel;
    edtC_CoarseRootAlloc: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    edtRootLeafRatio1: TEdit;
    edtRootLeafRatio2: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    edtLeafBranchRatio: TEdit;
    edtWoodBranchRatio: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    edtCoarseRootWoodRatio: TEdit;
    edtBarkWoodRatio: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    edtC_FruitAlloc: TEdit;
    Label13: TLabel;
    edt_CPollenAlloc: TEdit;
    Label14: TLabel;
    edtSexAge: TEdit;
    Label15: TLabel;
    edtExcessNUptake: TEdit;
    Label16: TLabel;
    edtWDSLope: TEdit;
    edtWHSlope: TEdit;
    Label17: TLabel;
    Label18: TLabel;
    edtMinWoodAllocation: TEdit;
    edtHDInter: TEdit;
    Label19: TLabel;
    edtHDSlope: TEdit;
    Label20: TLabel;
    edtmindbh: TEdit;
    Label21: TLabel;
    Label22: TLabel;
    edtWoodRetrans: TEdit;
    edtbWood: TEdit;
    Label23: TLabel;
    edtbBark: TEdit;
    Label24: TLabel;
    edtbFineRoots: TEdit;
    Label25: TLabel;
    Label26: TLabel;
    edtbBranch: TEdit;
    edtbFruit: TEdit;
    Label27: TLabel;
    edtbPollen: TEdit;
    Label28: TLabel;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAllocationParameters: TfrmAllocationParameters;

implementation

{$R *.DFM}

uses
  untDeclarations, untMiscellaneous, untSimulate, untMain;

procedure TfrmAllocationParameters.FormShow(Sender: TObject);

Begin
CalcAllocParams;
frmMain.FillEdit(Sender, edtC_FoliageAlloc, Derived.C_LeafAlloc, 1);
frmMain.FillEdit(Sender, edtC_BranchAlloc, Derived.C_BranchAlloc, 1);
frmMain.FillEdit(Sender, edtC_SapwoodAlloc, Derived.C_SapwoodAlloc, 1);
frmMain.FillEdit(Sender, edtC_BarkAlloc, Derived.C_BarkAlloc, 1);
frmMain.FillEdit(Sender, edtC_FinerootAlloc, Derived.C_FinerootAlloc, 1);
frmMain.FillEdit(Sender, edtC_CoarserootAlloc, Derived.C_CoarserootAlloc, 1);
frmMain.FillEdit(Sender, edtRootLeafRatio1, Parameter.RootLeafRatio1, 1);
frmMain.FillEdit(Sender, edtRootLeafRatio2, Parameter.RootLeafRatio2, 1);
frmMain.FillEdit(Sender, edtBarkWoodRatio, Parameter.BarkWoodRatio, 1);
frmMain.FillEdit(Sender, edtLeafBranchRatio, Parameter.LeafBranchRatio, 1);
frmMain.FillEdit(Sender, edtWoodBranchRatio, Parameter.WoodBranchRatio, 1);
frmMain.FillEdit(Sender, edtCoarseRootWoodRatio, Parameter.CoarseRootWoodRatio, 1);
frmMain.FillEdit(Sender, edtC_FruitAlloc, Parameter.C_FruitAlloc, 1);
frmMain.FillEdit(Sender, edt_CPollenAlloc, Parameter.C_PollenAlloc, 1);
frmMain.FillEdit(Sender, edtSexAge, Parameter.SexAge, 0);
frmMain.FillEdit(Sender, edtExcessNUptake, Parameter.ExcessNUptake, 1);
frmMain.FillEdit(Sender, edtWDSlope, Parameter.WDSlope, 1);
frmMain.FillEdit(Sender, edtWHSlope, Parameter.WHSlope, 1);
frmMain.FillEdit(Sender, edtHDInter, Parameter.HDInter, 1);
frmMain.FillEdit(Sender, edtHDSlope, Parameter.HDSlope, 1);
frmMain.FillEdit(Sender, edtMindbh, Parameter.Mindbh, 1);
frmMain.FillEdit(Sender, edtMinWoodAllocation, Parameter.MinWoodAlloc, 1);
frmMain.FillEdit(Sender, edtbWood, Parameter.bWood, 1);
frmMain.FillEdit(Sender, edtbBark, Parameter.bBark, 1);
frmMain.FillEdit(Sender, edtbFineRoots, Parameter.bRoots, 1);
frmMain.FillEdit(Sender, edtbBranch, Parameter.bBranch, 1);
frmMain.FillEdit(Sender, edtbFruit, Parameter.bFruit, 1);
frmMain.FillEdit(Sender, edtbPollen, Parameter.bPollen, 1);
frmMain.FillEdit(Sender, edtWoodRetrans, Parameter.WoodRetrans, 1);
End;

Procedure TfrmAllocationParameters.btnOKClick(Sender: TObject);
Begin
frmMain.GetEdit(Sender, edtRootLeafRatio1, Parameter.RootLeafRatio1, 1);
frmMain.GetEdit(Sender, edtRootLeafRatio2, Parameter.RootLeafRatio2, 1);
frmMain.GetEdit(Sender, edtLeafBranchRatio, Parameter.LeafBranchRatio, 1);
frmMain.GetEdit(Sender, edtWoodBranchRatio, Parameter.WoodBranchRatio, 1);
frmMain.GetEdit(Sender, edtCoarseRootWoodRatio, Parameter.CoarseRootWoodRatio, 1);
frmMain.GetEdit(Sender, edtBarkWoodRatio, Parameter.BarkWoodRatio, 1);
frmMain.GetEdit(Sender, edtC_FruitAlloc, Parameter.C_FruitAlloc, 1);
frmMain.GetEdit(Sender, edt_CPollenAlloc, Parameter.C_PollenAlloc, 1);
frmMain.GetInteger(Sender, edtSexAge, Parameter.SexAge);
frmMain.GetEdit(Sender, edtExcessNUptake, Parameter.ExcessNUptake, 1);
frmMain.GetEdit(Sender, edtWDSlope, Parameter.WDSlope, 1);
frmMain.GetEdit(Sender, edtWHSlope, Parameter.WHSlope, 1);
frmMain.GetEdit(Sender, edtHDInter, Parameter.HDInter, 1);
frmMain.GetEdit(Sender, edtHDSlope, Parameter.HDSlope, 1);
frmMain.GetEdit(Sender, edtMindbh, Parameter.Mindbh, 1);
frmMain.GetEdit(Sender, edtMinWoodAllocation, Parameter.MinWoodAlloc, 1);
frmMain.GetEdit(Sender, edtbWood, Parameter.bWood, 1);
frmMain.GetEdit(Sender, edtbBark, Parameter.bBark, 1);
frmMain.GetEdit(Sender, edtbFineRoots, Parameter.bRoots, 1);
frmMain.GetEdit(Sender, edtbBranch, Parameter.bBranch, 1);
frmMain.GetEdit(Sender, edtbFruit, Parameter.bFruit, 1);
frmMain.GetEdit(Sender, edtbPollen, Parameter.bPollen, 1);
frmMain.GetEdit(Sender, edtWoodRetrans, Parameter.WoodRetrans, 1);
Control.PlantHasChanged := TRUE;
End;

End.
