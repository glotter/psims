{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmAllocationParameters                         =
  =                                                              =
  =             Edit window to change allocation parameters      =
  ================================================================
  = File      : untAllocationParameters.PAS                      =
  =                                                              =
  = Version   : 4.0                                              =
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
    lblWDSLope: TLabel;
    edtWDSLope: TEdit;
    edtWHSlope: TEdit;
    lblWHSlope: TLabel;
    Label18: TLabel;
    edtMinWoodAllocation: TEdit;
    edtHDInter: TEdit;
    lblHDInter: TLabel;
    edtHDSlope: TEdit;
    lblHDSlope: TLabel;
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
    GroupBox1: TGroupBox;
    Label29: TLabel;
    edtWoodDensity25: TEdit;
    Label30: TLabel;
    edtWoodDensity0: TEdit;
    Label31: TLabel;
    edtWoodDensTemp: TEdit;
    Label32: TLabel;
    edtWoodDensFertility: TEdit;
    Label33: TLabel;
    edtWoodDensStocking: TEdit;
    Label34: TLabel;
    edtmindbh: TEdit;
    chkVariableHD: TCheckBox;
    grpVariableHD: TGroupBox;
    lblHD_Const: TLabel;
    lblHD_Temp: TLabel;
    lblHD_Stocking: TLabel;
    lblHD_Fertil: TLabel;
    edtHD_Temp: TEdit;
    edtHD_Const: TEdit;
    edtHD_Stocking: TEdit;
    edtHD_Fertil: TEdit;
    lblHD_InitialInter: TLabel;
    lblHD_InitialSlope: TLabel;
    edtHD_InitialSlope: TEdit;
    edtHD_InitialInter: TEdit;
    lblCrowdingFactor: TLabel;
    edtCrowdingFactor: TEdit;
    lblCrowdingPower: TLabel;
    edtCrowdingPower: TEdit;
    edtCrowdingMax: TEdit;
    lblCrowdingMax: TLabel;
    Label19: TLabel;
    lblCrowdingOffset: TLabel;
    edtCrowdingOffset: TEdit;
    edtHDSlopeMin: TEdit;
    edtHDSlopeMax: TEdit;
    lblHDSlopeMin: TLabel;
    lblHDSlopeMax: TLabel;
    chkUseAllometrics: TCheckBox;
    lblHD_Age1: TLabel;
    lblHD_Age2: TLabel;
    edtHD_Age1: TEdit;
    edtHD_Age2: TEdit;
    Procedure FormShow(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
    procedure chkVariableHDClick(Sender: TObject);
    procedure chkUseAllometricsClick(Sender: TObject);
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

Procedure TfrmAllocationParameters.FormShow(Sender: TObject);

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
frmMain.FillEdit(Sender, edtWoodDensity0, Parameter.WoodDensity0, 1);
frmMain.FillEdit(Sender, edtWoodDensity25, Parameter.WoodDensity25, 1);
frmMain.FillEdit(Sender, edtWoodDensTemp, Parameter.WoodDensTemp, 1);
frmMain.FillEdit(Sender, edtWoodDensFertility, Parameter.WoodDensFertility, 1);
frmMain.FillEdit(Sender, edtWoodDensStocking, Parameter.WoodDensStocking, 1);
frmMain.FillEdit(Sender, edtHD_Const, Parameter.HD_Const, 1);
frmMain.FillEdit(Sender, edtHD_Temp, Parameter.HD_Temp, 1);
frmMain.FillEdit(Sender, edtHD_Stocking, Parameter.HD_Stocking, 1000);
frmMain.FillEdit(Sender, edtHD_Fertil, Parameter.HD_Fertil, 1);
frmMain.FillEdit(Sender, edtHD_Age1, Parameter.HD_Age1, 1000);
frmMain.FillEdit(Sender, edtHD_Age2, Parameter.HD_Age2, 1e6);
frmMain.FillEdit(Sender, edtHD_InitialSlope, Parameter.HD_InitialSlope, 1);
frmMain.FillEdit(Sender, edtHD_InitialInter, Parameter.HD_InitialInter, 1);
frmMain.FillEdit(Sender, edtCrowdingFactor, Parameter.CrowdingFactor, 1e6);
frmMain.FillEdit(Sender, edtCrowdingPower, Parameter.CrowdingPower, 1);
frmMain.FillEdit(Sender, edtCrowdingOffset, Parameter.CrowdingOffset, 1);
frmMain.FillEdit(Sender, edtCrowdingMax, Parameter.CrowdingMax, 1);
frmMain.FillEdit(Sender, edtHDSlopeMin, Parameter.HDSlopeMin, 1);
frmMain.FillEdit(Sender, edtHDSlopeMax, Parameter.HDSlopeMax, 1);
chkVariableHD.Checked := Parameter.VariableHD;
chkUseAllometrics.Checked := Parameter.UseAllometrics;
frmAllocationParameters.chkVariableHDClick(Sender);
frmAllocationParameters.chkUseAllometricsClick(Sender);
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
frmMain.GetEdit(Sender, edtWoodDensity0, Parameter.WoodDensity0, 1);
frmMain.GetEdit(Sender, edtWoodDensity25, Parameter.WoodDensity25, 1);
frmMain.GetEdit(Sender, edtWoodDensTemp, Parameter.WoodDensTemp, 1);
frmMain.GetEdit(Sender, edtWoodDensFertility, Parameter.WoodDensFertility, 1);
frmMain.GetEdit(Sender, edtWoodDensStocking, Parameter.WoodDensStocking, 1);
frmMain.GetEdit(Sender, edtHD_Const, Parameter.HD_Const, 1);
frmMain.GetEdit(Sender, edtHD_Temp, Parameter.HD_Temp, 1);
frmMain.GetEdit(Sender, edtHD_Stocking, Parameter.HD_Stocking, 1000);
frmMain.GetEdit(Sender, edtHD_Fertil, Parameter.HD_Fertil, 1);
frmMain.GetEdit(Sender, edtHD_Age1, Parameter.HD_Age1, 1000);
frmMain.GetEdit(Sender, edtHD_Age2, Parameter.HD_Age2, 1e6);
frmMain.GetEdit(Sender, edtHD_InitialSlope, Parameter.HD_InitialSlope, 1);
frmMain.GetEdit(Sender, edtHD_InitialInter, Parameter.HD_InitialInter, 1);
frmMain.GetEdit(Sender, edtCrowdingFactor, Parameter.CrowdingFactor, 1e6);
frmMain.GetEdit(Sender, edtCrowdingPower, Parameter.CrowdingPower, 1);
frmMain.GetEdit(Sender, edtCrowdingOffset, Parameter.CrowdingOffset, 1);
frmMain.GetEdit(Sender, edtCrowdingMax, Parameter.CrowdingMax, 1);
frmMain.GetEdit(Sender, edtHDSlopeMin, Parameter.HDSlopeMin, 1);
frmMain.GetEdit(Sender, edtHDSlopeMax, Parameter.HDSlopeMax, 1);
Parameter.VariableHD := chkVariableHD.Checked;
Parameter.UseAllometrics := chkUseAllometrics.Checked;
Control.PlantHasChanged := TRUE;
End;

procedure TfrmAllocationParameters.chkUseAllometricsClick(Sender: TObject);
begin
If chkUseAllometrics.Checked then
   Begin
   grpStem_Allom_Paras.Font.Color := clWindowText;
   edtWHSlope.Enabled := true;
   edtWDSlope.Enabled := true;
   lblWHSlope.Enabled := true;
   lblWDSlope.Enabled := true;
   End
Else
   Begin
   grpStem_Allom_Paras.Font.Color := clGray;
   edtWHSlope.Enabled := false;
   edtWDSlope.Enabled := false;
   lblWHSlope.Enabled := false;
   lblWDSlope.Enabled := false;
   End;
end;

procedure TfrmAllocationParameters.chkVariableHDClick(Sender: TObject);
begin
If chkVariableHD.Checked then
   Begin
   grpVariableHD.Font.Color := clWindowText;
   edtHD_Const.Enabled := true;
   edtHD_Temp.Enabled := true;
   edtHD_Stocking.Enabled := true;
   edtHD_Fertil.Enabled := true;
   edtHD_Age1.Enabled := true;
   edtHD_Age2.Enabled := true;
   edtHD_InitialInter.Enabled := true;
   edtHD_InitialSlope.Enabled := true;
   edtCrowdingFactor.Enabled := true;
   edtCrowdingPower.Enabled := true;
   lblCrowdingOffset.Enabled := true;
   edtCrowdingMax.Enabled := true;
   edtHDSlopeMin.Enabled := true;
   edtHDSlopeMax.Enabled := true;
   lblHD_Const.Enabled := true;
   lblHD_Temp.Enabled := true;
   lblHD_Stocking.Enabled := true;
   lblHD_Fertil.Enabled := true;
   lblHD_Age1.Enabled := true;
   lblHD_Age2.Enabled := true;
   lblHD_InitialInter.Enabled := true;
   lblHD_InitialSlope.Enabled := true;
   lblCrowdingFactor.Enabled := true;
   lblCrowdingPower.Enabled := true;
   lblCrowdingOffset.Enabled := true;
   lblCrowdingMax.Enabled := true;
   lblHDSlopeMin.Enabled := true;
   lblHDSlopeMax.Enabled := true;
   grpAllometric_H_vs_dbh.Font.Color := clGray;
   edtHDSlope.Enabled := false;
   edtHDInter.Enabled := false;
   lblHDSlope.Enabled := false;
   lblHDInter.Enabled := false;
   End
Else
   Begin
   grpVariableHD.Font.Color := clGray;
   edtHD_Const.Enabled := false;
   edtHD_Temp.Enabled := false;
   edtHD_Stocking.Enabled := false;
   edtHD_Fertil.Enabled := false;
   edtHD_Age1.Enabled := false;
   edtHD_Age2.Enabled := false;
   edtHD_InitialInter.Enabled := false;
   edtHD_InitialSlope.Enabled := false;
   edtCrowdingFactor.Enabled := false;
   edtCrowdingPower.Enabled := false;
   edtCrowdingOffset.Enabled := false;
   edtCrowdingMax.Enabled := false;
   edtHDSlopeMin.Enabled := false;
   edtHDSlopeMax.Enabled := false;
   lblHD_Const.Enabled := false;
   lblHD_Temp.Enabled := false;
   lblHD_Stocking.Enabled := false;
   lblHD_Fertil.Enabled := false;
   lblHD_Age1.Enabled := false;
   lblHD_Age2.Enabled := false;
   lblHD_InitialInter.Enabled := false;
   lblHD_InitialSlope.Enabled := false;
   lblCrowdingFactor.Enabled := false;
   lblCrowdingPower.Enabled := false;
   lblCrowdingOffset.Enabled := false;
   lblCrowdingMax.Enabled := false;
   lblHDSlopeMin.Enabled := false;
   lblHDSlopeMax.Enabled := false;
   grpAllometric_H_vs_dbh.Font.Color := clWindowText;
   edtHDSlope.Enabled := true;
   edtHDInter.Enabled := true;
   lblHDSlope.Enabled := true;
   lblHDInter.Enabled := true;
   End;
end;


End.
