{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmPools                                        =
  =                                                              =
  =             Interface routine to show all pools              =
  =             and allow the user to change them.               =
  ================================================================
  = File      : untPools.PAS                                     =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untPools;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, untDeclarations;

type
  OldPlantType = Record
        SapWood, HeartWood, CoarseRoot, FineRoot, Branches, Bark, Pollen,
        Fruit, Soluble, Leaves, Reserves, WeedLeaves, WeedRoots, Dummy: Real48;
        End;

  OldSoilType = Record
        StructLitter, MetabLitter, FineWoodLitter, CoarseWoodLitter,
        Active, Slow, Resistant, Inert: OrganicFlowType;
        End;

  TfrmPools = class(TForm)
    pgcPages: TPageControl;
    btnEditC13: TButton;
    pnlButtons: TPanel;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    rdRescaleSOM: TRadioButton;
    edtChangePerc: TEdit;
    lblPercCurrent: TLabel;
    tbsCarbon: TTabSheet;
    grpPlantPools: TGroupBox;
    ChkResetNPPools: TCheckBox;
    edtSapWoodC: TEdit;
    edtHeartWoodC: TEdit;
    edtCoarseRootC: TEdit;
    edtBarkC: TEdit;
    edtBranchesC: TEdit;
    edtLeavesC: TEdit;
    edtFineRootC: TEdit;
    edtFruitC: TEdit;
    edtPollenC: TEdit;
    edtSolubleC: TEdit;
    edtReservesC: TEdit;
    lbCLayer: TLabel;
    lbStructuralC: TLabel;
    lbMetabolicC: TLabel;
    lbActiveC: TLabel;
    lbSlowC: TLabel;
    lbResistantC: TLabel;
    lbFineWoodC: TLabel;
    lbCoarseWoodC: TLabel;
    lblInert: TLabel;
    lblSapwood: TLabel;
    lblHeartwood: TLabel;
    lblCoarseroot: TLabel;
    lblBark: TLabel;
    lblBranch: TLabel;
    lblFoliage: TLabel;
    lblFineroot: TLabel;
    lblFruit: TLabel;
    lblPollen: TLabel;
    lblCH2O: TLabel;
    lblReserves: TLabel;
    tbsNitrogen: TTabSheet;
    edtSapwoodN: TEdit;
    edtHeartwoodN: TEdit;
    edtCoarseRootN: TEdit;
    edtBarkN: TEdit;
    edtBranchN: TEdit;
    edtFoliageN: TEdit;
    edtFineRootN: TEdit;
    edtFruitN: TEdit;
    edtPollenN: TEdit;
    edtSolubleN: TEdit;
    edtReservesN: TEdit;
    lbLayerN: TLabel;
    lbStructN: TLabel;
    lbMetabolicN: TLabel;
    lbFineWoodN: TLabel;
    lbCoarseWoodN: TLabel;
    lbActiveN: TLabel;
    lbSlowN: TLabel;
    lbResistantN: TLabel;
    lblInertN: TLabel;
    lblSolubleSoilN: TLabel;
    lblSapwoodN: TLabel;
    lblHeartwoodN: TLabel;
    lblCoarserootN: TLabel;
    lblBarkN: TLabel;
    lblBranchN: TLabel;
    lblFoliageN: TLabel;
    lblFinerootN: TLabel;
    lblFruitN: TLabel;
    lblPollenN: TLabel;
    lblSolublePlantN: TLabel;
    lblReservesN: TLabel;
    tbsPhosphorus: TTabSheet;
    edtSapwoodP: TEdit;
    edtHeartwoodP: TEdit;
    edtFinerootP: TEdit;
    edtCoarserootP: TEdit;
    edtBranchP: TEdit;
    edtBarkP: TEdit;
    edtFoliageP: TEdit;
    edtPollenP: TEdit;
    edtFruitP: TEdit;
    edtSolubleP: TEdit;
    edtReservesP: TEdit;
    lblInertP: TLabel;
    lblLayerP: TLabel;
    lblStructP: TLabel;
    lblMetabolicP: TLabel;
    lblCoarseWoodP: TLabel;
    lblFinewoodP: Tlabel;
    lblActiveP: TLabel;
    lblSlowP: TLabel;
    lblResistantP: TLabel;
    lblSolubleSoilP: TLabel;
    lblSolublePlantP: TLabel;
    lblSapwoodP: TLabel;
    lblHeartwoodP: TLabel;
    lblCoarserootP: TLabel;
    lblBarkP: TLabel;
    lblBranchP: TLabel;
    lblFoliageP: TLabel;
    lblFinerootP: TLabel;
    lblFruitP: TLabel;
    lblPollenP: TLabel;
    lblReservesP: TLabel;
    tbsMisc: TTabSheet;
    grpInfo: TGroupBox;
    edtPlantAge: TEdit;
    edtStartDay: TEdit;
    edtStartMonth: TEdit;
    edtStartYear: TEdit;
    edtStocking: TEdit;
    edtSoilWater: TEdit;
    edtSnow: TEdit;
    edtCanopyCover: TEdit;
    edtkex: TEdit;
    edtHeight: TEdit;
    edtDBH: TEdit;
    edtArea: TEdit;
    lblDay: TLabel;
    lblYear: TLabel;
    lblMonth: TLabel;
    lbLignin: TLabel;
    lblAge: TLabel;
    lblStocking: TLabel;
    lblSoilwater: TLabel;
    lblSnow: TLabel;
    lblCover: TLabel;
    lblExtinct: TLabel;
    lblStart: TLabel;
    lblHeight: TLabel;
    lblDBH: TLabel;
    lblBasalArea: TLabel;
    btnCancel: TBitBtn;
    rdRescaleSON: TRadioButton;
    rdRescaleP: TRadioButton;
    edtChangePercN: TEdit;
    edtChangePercP: TEdit;
    lblPercP: TLabel;
    lblPercN: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblWeedLeavesC: TLabel;
    edtWeedLeavesC: TEdit;
    edtWeedRootsC: TEdit;
    lblWeedRootsC: TLabel;
    lblWeedLeavesN: TLabel;
    lblWeedRootsN: TLabel;
    edtWeedLeavesN: TEdit;
    edtWeedRootsN: TEdit;
    lblWeedLeavesP: TLabel;
    lblWeedRootsP: TLabel;
    edtWeedLeavesP: TEdit;
    edtWeedRootsP: TEdit;
    lblWeedHeight: TLabel;
    edtWeedHeight: TEdit;
    Procedure FormShow(Sender: TObject);
    Procedure FillEditBoxDisplay(Sender: TObject; TabSheet: TTabSheet; Width, Left, Top, iLayer: Integer;
              ReadOnlyFlag: Boolean; fValue, Multiplier: Real48; var OldVar, Sum: Real48);
    Procedure CalcSettings (Sender: TObject; var DisplayLayers: Integer);
    Procedure New_NandP (Var NewPool: SoilElements; iLayer: Integer; OldC: real48);
    Procedure New_NandP2 (Var NewPool: TElements; OldC: real48);
    Procedure AdjustNandP(Sender: TObject; DisplayLayers: Integer);
    Procedure LabelDisplay(Sender: TObject; TabSheet: TTabSheet; Caption: String; Width, Left, Top: Integer);
    Procedure FillCarbonPage(Sender: TObject; DisplayLayers: Integer);
    Procedure FillNitrogenPage(Sender: TObject; DisplayLayers: Integer);
    Procedure FillPhosphorusPage(Sender: TObject; DisplayLayers: Integer);
    Procedure FillMiscellaneousPage(Sender: TObject; DisplayLayers: Integer);
    Procedure GetCarbonPage(Sender: TObject; DisplayLayers: Integer);
    Procedure GetNitrogenPage(Sender: TObject; DisplayLayers: Integer);
    Procedure GetPhosphorusPage(Sender: TObject; DisplayLayers: Integer);
    Procedure GetMiscellaneousPage(Sender: TObject; DisplayLayers: Integer);
    Procedure btnOKClick(Sender: TObject);
    Procedure pgcPagesChange(Sender: TObject);
    Procedure btnHelpClick(Sender: TObject);
    Procedure rdRescaleSOMClick(Sender: TObject);
    Procedure btnEditC13Click(Sender: TObject);
    procedure rdRescalePClick(Sender: TObject);
    procedure rdRescaleSONClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var Dummy, SumStruct, SumMetab, SumFine, SumCoarse, SumActive, SumSlow, SumResistant,
    SumInert, SumMineral, SumSecondaryInorganicP, SumRockP, SumOccludedP: Real48;
    frmPools: TfrmPools;
    OldSoil : OldSoilType;
    OldPlant: OldPlantType;
    DisplayLayers: Integer;

implementation

{$R *.DFM}

uses untMiscellaneous, untHelpConsts, untC13Pools, untMain;

Procedure TfrmPools.FillEditBoxDisplay(Sender: TObject; TabSheet: TTabSheet; Width, Left, Top, iLayer: Integer;
          ReadOnlyFlag: Boolean; fValue, Multiplier: Real48; var OldVar, Sum: Real48);
var NextBox: TEdit;
Begin
NextBox := TEdit.Create(TabSheet);
NextBox.Parent := TabSheet;
NextBox.Width := Width;
NextBox.Left := Left;
NextBox.Top := Top;
NextBox.ReadOnly := ReadOnlyFlag;
Oldvar := fValue;
frmMain.FillEdit(Sender, NextBox, fValue, Multiplier);
If iLayer = 0 then
   Sum := fValue
Else
   Sum := Sum + fValue;
End; {of Procedure 'FillEditBoxDisplay'}

Procedure TfrmPools.LabelDisplay(Sender: TObject; TabSheet: TTabSheet; Caption: String; Width, Left, Top: Integer);
var NextLabel: TLabel;
Begin
NextLabel := TLabel.Create(TabSheet);
NextLabel.Parent := TabSheet;
NextLabel.Caption := Caption;
NextLabel.Width := Width;
NextLabel.Left := Left;
NextLabel.Top := Top;
End;

Procedure TfrmPools.New_NandP (Var NewPool: SoilElements; iLayer: Integer; OldC: real48);
Begin
If OldC <> 0 then
   Begin
   NewPool[iLayer, N] := NewPool[iLayer, N] * NewPool[iLayer, C] / OldC;
   NewPool[iLayer, P] := NewPool[iLayer, P] * NewPool[iLayer, C] / OldC;
   End
Else
   {Do nothing and keep NewN_P with old value};
End; {of Procedure 'New_NandP'}

Procedure TfrmPools.New_NandP2 (Var NewPool: TElements; OldC: real48);
Begin
If OldC <> 0 then
   Begin
   NewPool[N] := NewPool[N] * NewPool[C] / OldC;
   NewPool[P] := NewPool[P] * NewPool[C] / OldC;
   End
Else
   {Do nothing and keep NewN_P with old value};
End; {of Procedure 'New_NandP2'}

Procedure TfrmPools.AdjustNandP(Sender: TObject; DisplayLayers: Integer);
var iLayer: Integer;
Begin
For iLayer := 0 to DisplayLayers do
    Begin
    New_NandP(Soil.Struct, iLayer, OldSoil.StructLitter[iLayer]);
    New_NandP(Soil.Metab, iLayer, OldSoil.MetabLitter[iLayer]);
    If iLayer = 0 then
       New_NandP(Soil.FineWood, 0, OldSoil.FineWoodLitter[0]);
    New_NandP(Soil.CoarseWood, iLayer, OldSoil.CoarseWoodLitter[iLayer]);
    New_NandP(Soil.Active, iLayer, OldSoil.Active[iLayer]);
    New_NandP(Soil.Slow, iLayer, OldSoil.Slow[iLayer]);
    New_NandP(Soil.Resistant, iLayer, OldSoil.Resistant[iLayer]);
    New_NandP(Soil.Inert, iLayer, OldSoil.Inert[iLayer]);
    End;
New_NandP2(Plant.SapWood, OldPlant.SapWood);
New_NandP2(Plant.HeartWood, OldPlant.HeartWood);
New_NandP2(Plant.CoarseRoot, OldPlant.CoarseRoot);
New_NandP2(Plant.FineRoot, OldPlant.FineRoot);
New_NandP2(Plant.Branches, OldPlant.Branches);
New_NandP2(Plant.Bark, OldPlant.Bark);
New_NandP2(Plant.Leaves, OldPlant.Leaves);
New_NandP2(Plant.Pollen, OldPlant.Pollen);
New_NandP2(Plant.Fruit, OldPlant.Fruit);
New_NandP2(Plant.Soluble, OldPlant.Soluble);
New_NandP2(Plant.Reserves, OldPlant.Reserves);
New_NandP2(Plant.WeedLeaves, OldPlant.WeedLeaves);
New_NandP2(Plant.WeedRoots, OldPlant.WeedRoots);
End; {of Procedure 'AdjustNandP'}

Procedure TfrmPools.CalcSettings (Sender: TObject; var DisplayLayers: Integer);
Begin
if Control.IncludeP or Control.IncludeWeeds then 
   Begin
   frmPools.Width := 1000;
   pgcPages.Width := 994;
   End
Else
   Begin
   frmPools.Width := 880;
   pgcPages.Width := 874;
   End;
if Control.IncludeWeeds then
   Begin
   lblWeedLeavesC.Visible := true;
   edtWeedLeavesC.Visible := true;
   lblWeedRootsC.Visible := true;
   edtWeedRootsC.Visible := true;
   lblWeedLeavesN.Visible := true;
   edtWeedLeavesN.Visible := true;
   lblWeedRootsN.Visible := true;
   edtWeedRootsN.Visible := true;
   lblWeedLeavesP.Visible := true;
   edtWeedLeavesP.Visible := true;
   lblWeedRootsP.Visible := true;
   edtWeedRootsP.Visible := true;
   lblWeedHeight.Visible := true;
   edtWeedHeight.Visible := true;
   End
Else
   Begin
   lblWeedLeavesC.Visible := false;
   edtWeedLeavesC.Visible := false;
   lblWeedRootsC.Visible := false;
   edtWeedRootsC.Visible := false;
   lblWeedLeavesN.Visible := false;
   edtWeedLeavesN.Visible := false;
   lblWeedRootsN.Visible := false;
   edtWeedRootsN.Visible := false;
   lblWeedLeavesP.Visible := false;
   edtWeedLeavesP.Visible := false;
   lblWeedRootsP.Visible := false;
   edtWeedRootsP.Visible := false;
   lblWeedHeight.Visible := false;
   edtWeedHeight.Visible := false;
   End;
If Control.IncludeIsotopes then
   Begin
   btnEditC13.visible := true;
   btnEditC13.enabled := true;
   End
Else
   Begin
   btnEditC13.visible := false;
   btnEditC13.enabled := false;
   End;
If Control.AllOneLayer then
   DisplayLayers := 1
Else
   DisplayLayers := Soil.nLayers;
If DisplayLayers < 5 then
   Begin
   frmPools.Height := 410;
   frmPools.Top := 165;
   End
Else
   Begin
   frmPools.Height := 300 + 22 * DisplayLayers;
   frmPools.Top := 232 - 11 * DisplayLayers;
   End;
End; {of Procedure 'CalcSettings'}

Procedure FillEdit(Sender: TObject; edtBox: TEdit; fValue, Multiplier: Real48; var OldVar: Real48);
Begin
Oldvar := fValue;
frmMain.FillEdit(Sender, edtBox, fValue, Multiplier);
end;

Procedure SetOldValues(DisplayLayers: Integer);

  Procedure MakeOld(fValue: Real48; var OldVar: Real48);
  begin
    Oldvar := fValue;
  end;

var iLayer: Integer;
Begin
For iLayer := 0 to DisplayLayers do
    Begin
    MakeOld(Soil.Struct[iLayer, C], OldSoil.StructLitter[iLayer]);
    MakeOld(Soil.Metab[iLayer, C], OldSoil.MetabLitter[iLayer]);
    If iLayer = 0 then
       MakeOld(Soil.FineWood[0, C], OldSoil.FineWoodLitter[0]);
    MakeOld(Soil.CoarseWood[iLayer, C], OldSoil.CoarseWoodLitter[iLayer]);
    MakeOld(Soil.Active[iLayer, C], OldSoil.Active[iLayer]);
    MakeOld(Soil.Slow[iLayer, C], OldSoil.Slow[iLayer]);
    MakeOld(Soil.Resistant[iLayer, C], OldSoil.Resistant[iLayer]);
    MakeOld(Soil.Inert[iLayer, C], OldSoil.Inert[iLayer]);
    End;
MakeOld(Plant.SapWood[C], OldPlant.SapWood);
MakeOld(Plant.HeartWood[C], OldPlant.HeartWood);
MakeOld(Plant.CoarseRoot[C], OldPlant.CoarseRoot);
MakeOld(Plant.FineRoot[C], OldPlant.FineRoot);
MakeOld(Plant.Branches[C], OldPlant.Branches);
MakeOld(Plant.Bark[C], OldPlant.Bark);
MakeOld(Plant.Leaves[C], OldPlant.Leaves);
MakeOld(Plant.Pollen[C], OldPlant.Pollen);
MakeOld(Plant.Fruit[C], OldPlant.Fruit);
MakeOld(Plant.Soluble[C], OldPlant.Soluble);
MakeOld(Plant.Reserves[C], OldPlant.Reserves);
MakeOld(Plant.WeedLeaves[C], OldPlant.WeedLeaves);
MakeOld(Plant.WeedRoots[C], OldPlant.WeedRoots);
End; {of Procedure 'SetOldValues'}

Procedure TfrmPools.FillCarbonPage(Sender: TObject; DisplayLayers: Integer);
var iCount, iLayer: Integer;
    LayerInfo: String;
Begin
FillEdit(Sender, edtSapWoodC, Plant.SapWood[C], Control.CConversion, OldPlant.SapWood);
FillEdit(Sender, edtHeartWoodC, Plant.HeartWood[C], Control.CConversion, OldPlant.HeartWood);
FillEdit(Sender, edtCoarseRootC, Plant.CoarseRoot[C], Control.CConversion, OldPlant.CoarseRoot);
FillEdit(Sender, edtFineRootC, Plant.FineRoot[C], Control.CConversion, OldPlant.FineRoot);
FillEdit(Sender, edtBranchesC, Plant.Branches[C], Control.CConversion, OldPlant.Branches);
FillEdit(Sender, edtBarkC, Plant.Bark[C], Control.CConversion, OldPlant.Bark);
FillEdit(Sender, edtLeavesC, Plant.Leaves[C], Control.CConversion, OldPlant.Leaves);
FillEdit(Sender, edtPollenC, Plant.Pollen[C], Control.CConversion, OldPlant.Pollen);
FillEdit(Sender, edtFruitC, Plant.Fruit[C], Control.CConversion, OldPlant.Fruit);
FillEdit(Sender, edtSolubleC, Plant.Soluble[C], Control.CConversion, OldPlant.Soluble);
FillEdit(Sender, edtReservesC, Plant.Reserves[C], Control.CConversion, OldPlant.Reserves);
FillEdit(Sender, edtWeedLeavesC, Plant.WeedLeaves[C], Control.CConversion, OldPlant.WeedLeaves);
FillEdit(Sender, edtWeedRootsC, Plant.WeedRoots[C], Control.CConversion, OldPlant.WeedRoots);
FillEdit(Sender, edtChangePerc, 100, 1, OldPlant.Dummy);
while (frmPools.tbsCarbon.ComponentCount > 0) do
       frmPools.tbsCarbon.Components[0].Free;
iCount := 100;
For iLayer := 0 to DisplayLayers do
    Begin
    iCount := iCount + 22;
    If iLayer = 0 then
       LabelDisplay (Sender, frmPools.tbsCarbon, 'Surface', 40, 35, iCount + 4)
    Else
       Begin
       Str(iLayer:3, LayerInfo);
       LabelDisplay (Sender, frmPools.tbsCarbon, LayerInfo + ':', 25, 53, iCount + 4);
       End;
    FillEditBoxDisplay (Sender, frmPools.tbsCarbon, 50, 85, iCount, iLayer, false, Soil.Struct[iLayer, C], 1, OldSoil.StructLitter[iLayer], SumStruct);
    FillEditBoxDisplay (Sender, frmPools.tbsCarbon, 50, 170, iCount, iLayer, false, Soil.Metab[iLayer, C], 1, OldSoil.MetabLitter[iLayer], SumMetab);
    If iLayer = 0 then
       FillEditBoxDisplay (Sender, frmPools.tbsCarbon, 50, 255, iCount, iLayer, false, Soil.FineWood[0, C], 1, OldSoil.FineWoodLitter[0], SumFine);
    FillEditBoxDisplay (Sender, frmPools.tbsCarbon, 50, 340, iCount, iLayer, false, Soil.CoarseWood[iLayer, C], 1, OldSoil.CoarseWoodLitter[iLayer], SumCoarse);
    FillEditBoxDisplay (Sender, frmPools.tbsCarbon, 50, 425, iCount, iLayer, false, Soil.Active[iLayer, C], 1, OldSoil.Active[iLayer], SumActive);
    FillEditBoxDisplay (Sender, frmPools.tbsCarbon, 50, 510, iCount, iLayer, false, Soil.Slow[iLayer, C], 1, OldSoil.Slow[iLayer], SumSlow);
    FillEditBoxDisplay (Sender, frmPools.tbsCarbon, 50, 595, iCount, iLayer, false, Soil.Resistant[iLayer, C], 1, OldSoil.Resistant[iLayer], SumResistant);
    FillEditBoxDisplay (Sender, frmPools.tbsCarbon, 50, 680, iCount, iLayer, false, Soil.Inert[iLayer, C], 1, OldSoil.Inert[iLayer], SumInert);
    End;
iCount := iCount + 22;
LabelDisplay (Sender, frmPools.tbsCarbon, 'Total:', 40, 40, iCount + 4);
FillEditBoxDisplay (Sender, frmPools.tbsCarbon, 50, 85, iCount, iLayer, true, SumStruct, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsCarbon, 50, 170, iCount, iLayer, true, SumMetab, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsCarbon, 50, 255, iCount, iLayer, true, SumFine, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsCarbon, 50, 340, iCount, iLayer, true, SumCoarse, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsCarbon, 50, 425, iCount, iLayer, true, SumActive, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsCarbon, 50, 510, iCount, iLayer, true, SumSlow, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsCarbon, 50, 595, iCount, iLayer, true, SumResistant, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsCarbon, 50, 680, iCount, iLayer, true, SumInert, 1, Dummy, Dummy);
frmPools.ChkResetNPPools.Top := iCount + 30;
if Control.IncludeP then
   frmPools.ChkResetNPPools.Caption := 'Reset N and P pools'
Else
   frmPools.ChkResetNPPools.Caption := 'Reset N pools';
frmPools.rdRescaleSOM.Top := iCount + 30;
frmPools.rdRescaleSOM.Checked := false;
frmPools.edtChangePerc.Top := iCount + 28;
frmPools.lblPercCurrent.Top := iCount + 30;
End; {of Procedure 'FillCarbonPage'}

Procedure TfrmPools.FillNitrogenPage(Sender: TObject; DisplayLayers: Integer);
var iCount, iLayer: Integer;
    LayerInfo: String;
Begin
while (frmPools.tbsNitrogen.ComponentCount > 0) do
       frmPools.tbsNitrogen.Components[0].Free;
FillEdit(Sender, edtSapWoodN, Plant.SapWood[N], 1, Dummy);
FillEdit(Sender, edtHeartWoodN, Plant.HeartWood[N], 1, Dummy);
FillEdit(Sender, edtCoarseRootN, Plant.CoarseRoot[N], 1, Dummy);
FillEdit(Sender, edtFineRootN, Plant.FineRoot[N], 1, Dummy);
FillEdit(Sender, edtBranchN, Plant.Branches[N], 1, Dummy);
FillEdit(Sender, edtBarkN, Plant.Bark[N], 1, Dummy);
FillEdit(Sender, edtFoliageN, Plant.Leaves[N], 1, Dummy);
FillEdit(Sender, edtPollenN, Plant.Pollen[N], 1, Dummy);
FillEdit(Sender, edtFruitN, Plant.Fruit[N], 1, Dummy);
FillEdit(Sender, edtSolubleN, Plant.Soluble[N], 1, Dummy);
FillEdit(Sender, edtReservesN, Plant.Reserves[N], 1, Dummy);
FillEdit(Sender, edtWeedLeavesN, Plant.WeedLeaves[N], 1, Dummy);
FillEdit(Sender, edtWeedRootsN, Plant.WeedRoots[N], 1, Dummy);
FillEdit(Sender, edtChangePercN, 100, 1, Dummy);
iCount := 100;
For iLayer := 0 to DisplayLayers do
    Begin
    iCount := iCount + 22;
    If iLayer = 0 then
       LabelDisplay (Sender, frmPools.tbsNitrogen, 'Surface', 40, 10, iCount + 4)
    Else
       Begin
       Str(iLayer:3, LayerInfo);
       LabelDisplay (Sender, frmPools.tbsNitrogen,LayerInfo + ':', 25, 28, iCount + 4);
       End;
    FillEditBoxDisplay (Sender, frmPools.tbsNitrogen, 50, 55, iCount, iLayer, false, Soil.Struct[iLayer, N], 1, Dummy, SumStruct);
    FillEditBoxDisplay (Sender, frmPools.tbsNitrogen, 50, 140, iCount, iLayer, false, Soil.Metab[iLayer, N], 1, Dummy, SumMetab);
    If iLayer = 0 then
       FillEditBoxDisplay (Sender, frmPools.tbsNitrogen, 50, 225, iCount, iLayer, false, Soil.FineWood[0, N], 1, Dummy, SumFine);
    FillEditBoxDisplay (Sender, frmPools.tbsNitrogen, 50, 310, iCount, iLayer, false, Soil.CoarseWood[iLayer, N], 1, Dummy, SumCoarse);
    FillEditBoxDisplay (Sender, frmPools.tbsNitrogen, 50, 395, iCount, iLayer, false, Soil.Active[iLayer, N], 1, Dummy, SumActive);
    FillEditBoxDisplay (Sender, frmPools.tbsNitrogen, 50, 480, iCount, iLayer, false, Soil.Slow[iLayer, N], 1, Dummy, SumSlow);
    FillEditBoxDisplay (Sender, frmPools.tbsNitrogen, 50, 565, iCount, iLayer, false, Soil.Resistant[iLayer, N], 1, Dummy, SumResistant);
    FillEditBoxDisplay (Sender, frmPools.tbsNitrogen, 50, 650, iCount, iLayer, false, Soil.Inert[iLayer, N], 1, Dummy, SumInert);
    FillEditBoxDisplay (Sender, frmPools.tbsNitrogen, 50, 735, iCount, iLayer, false, Soil.Soluble[iLayer, N], 1, Dummy, SumMineral);
    End;
iCount := iCount + 22;
LabelDisplay (Sender, frmPools.tbsNitrogen, 'Total:', 40, 15, iCount + 4);
FillEditBoxDisplay (Sender, frmPools.tbsNitrogen, 50, 55, iCount, iLayer, true, SumStruct, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsNitrogen, 50, 140, iCount, iLayer, true, SumMetab, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsNitrogen, 50, 225, iCount, iLayer, true, SumFine, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsNitrogen, 50, 310, iCount, iLayer, true, SumCoarse, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsNitrogen, 50, 395, iCount, iLayer, true, Sumactive, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsNitrogen, 50, 480, iCount, iLayer, true, SumSlow, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsNitrogen, 50, 565, iCount, iLayer, true, SumResistant, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsNitrogen, 50, 650, iCount, iLayer, true, SumInert, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsNitrogen, 50, 735, iCount, iLayer, true, SumMineral, 1, Dummy, Dummy);
frmPools.rdRescaleSON.Top := iCount + 30;
frmPools.rdRescaleSON.Checked := false;
frmPools.edtChangePercN.Top := iCount + 28;
frmPools.lblPercN.Top := iCount + 30;
End; {of Procedure 'FillNitrogenPage'}

Procedure TfrmPools.FillPhosphorusPage(Sender: TObject; DisplayLayers: Integer);
var iCount, iLayer: Integer;
    LayerInfo: String;
Begin
while (frmPools.tbsPhosphorus.ComponentCount > 0) do
       frmPools.tbsPhosphorus.Components[0].Free;
FillEdit(Sender, edtSapWoodP, Plant.SapWood[P], 1, Dummy);
FillEdit(Sender, edtHeartWoodP, Plant.HeartWood[P], 1, Dummy);
FillEdit(Sender, edtCoarserootP, Plant.CoarseRoot[P], 1, Dummy);
FillEdit(Sender, edtFineRootP, Plant.FineRoot[P], 1, Dummy);
FillEdit(Sender, edtBranchP, Plant.Branches[P], 1, Dummy);
FillEdit(Sender, edtBarkP, Plant.Bark[P], 1, Dummy);
FillEdit(Sender, edtFoliageP, Plant.Leaves[P], 1, Dummy);
FillEdit(Sender, edtPollenP, Plant.Pollen[P], 1, Dummy);
FillEdit(Sender, edtFruitP, Plant.Fruit[P], 1, Dummy);
FillEdit(Sender, edtSolubleP, Plant.Soluble[P], 1, Dummy);
FillEdit(Sender, edtReservesP, Plant.Reserves[P], 1, Dummy);
FillEdit(Sender, edtWeedLeavesP, Plant.WeedLeaves[P], 1, Dummy);
FillEdit(Sender, edtWeedRootsP, Plant.WeedRoots[P], 1, Dummy);
FillEdit(Sender, edtChangePercP, 100, 1, Dummy);
iCount := 100;
For iLayer := 0 to DisplayLayers do
    Begin
    iCount := iCount + 22;
    If iLayer = 0 then
       LabelDisplay (Sender, frmPools.tbsPhosphorus, 'Surface', 40, 10, iCount + 4)
    Else
       Begin
       Str(iLayer:3, LayerInfo);
       LabelDisplay (Sender, frmPools.tbsPhosphorus,LayerInfo + ':', 25, 28, iCount + 4);
       End;
    FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 55, iCount, iLayer, false, Soil.Struct[iLayer, P], 1, Dummy, SumStruct);
    FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 130, iCount, iLayer, false, Soil.Metab[iLayer, P], 1, Dummy, SumMetab);
    If iLayer = 0 then
       FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 205, iCount, iLayer, false, Soil.FineWood[0, P], 1, Dummy, SumFine);
    FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 280, iCount, iLayer, false, Soil.CoarseWood[iLayer, P], 1, Dummy, SumCoarse);
    FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 355, iCount, iLayer, false, Soil.Active[iLayer, P], 1, Dummy, SumActive);
    FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 430, iCount, iLayer, false, Soil.Slow[iLayer, P], 1, Dummy, SumSlow);
    FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 505, iCount, iLayer, false, Soil.Resistant[iLayer, P], 1, Dummy, SumResistant);
    FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 580, iCount, iLayer, false, Soil.Inert[iLayer, P], 1, Dummy, SumInert);
    FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 655, iCount, iLayer, false, Soil.Soluble[iLayer, P], 1, Dummy, SumMineral);
    FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 730, iCount, iLayer, false, Soil.SecondaryInorganicP[iLayer, P], 1, Dummy, SumSecondaryInorganicP);
    FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 805, iCount, iLayer, false, Soil.RockP[iLayer, P], 1, Dummy, SumRockP);
    FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 880, iCount, iLayer, false, Soil.OccludedP[iLayer, P], 1, Dummy, SumOccludedP);
    End;
iCount := iCount + 22;
LabelDisplay (Sender, frmPools.tbsPhosphorus, 'Total:', 40, 15, iCount + 4);
FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 55, iCount, iLayer, true, SumStruct, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 130, iCount, iLayer, true, SumMetab, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 205, iCount, iLayer, true, SumFine, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 280, iCount, iLayer, true, SumCoarse, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 355, iCount, iLayer, true, Sumactive, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 430, iCount, iLayer, true, SumSlow, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 505, iCount, iLayer, true, SumResistant, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 580, iCount, iLayer, true, SumInert, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 655, iCount, iLayer, true, SumMineral, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 730, iCount, iLayer, true, SumSecondaryInorganicP, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 805, iCount, iLayer, true, SumRockP, 1, Dummy, Dummy);
FillEditBoxDisplay (Sender, frmPools.tbsPhosphorus, 50, 880, iCount, iLayer, true, SumOccludedP, 1, Dummy, Dummy);
frmPools.rdRescaleP.Top := iCount + 30;
frmPools.rdRescaleP.Checked := false;
frmPools.edtChangePercP.Top := iCount + 28;
frmPools.lblPercP.Top := iCount + 30;
End; {of Procedure 'FillPhosphorusPage'}

Procedure TfrmPools.FillMiscellaneousPage(Sender: TObject; DisplayLayers: Integer);
var iCount, iLayer: Integer;
    LayerInfo: String;
Begin
frmMain.FillEdit(Sender, edtPlantAge, Plant.Age, 0);
frmMain.FillEdit(Sender, edtStocking, Round(Plant.Stocking), 0);
Soil.TotalWater := 0;
For iLayer := 1 to Soil.nLayers do
    Soil.TotalWater := Soil.TotalWater + Soil.WaterLayer[iLayer].WaterContent;
FillEdit(Sender, edtSoilWater, Soil.TotalWater, 1, Dummy);
FillEdit(Sender, edtSnow, Soil.Snow, 1, Dummy);
frmMain.FillEdit(Sender, edtStartDay, Control.ExtraDays, 0);
frmMain.FillEdit(Sender, edtStartMonth, Control.ExtraMonths, 0);
frmMain.FillEdit(Sender, edtStartYear, Control.TotalYears, 0);
FillEdit(Sender, edtHeight, Plant.Height, 1, Dummy);
FillEdit(Sender, edtDBH, Plant.DBH, 1, Dummy);
Plant.Area := Plant.Stocking * pi * Sqr(Plant.DBH / 2) * 1.05;
FillEdit(Sender, edtArea, Plant.Area, 0.0001, Dummy);
Plant.CanopyCover := pi * sqr((0.7544 + 0.2073 * Plant.DBH) / 2) * Plant.Stocking / 10000;
If Plant.CanopyCover > 1 then Plant.CanopyCover := 1;
FillEdit(Sender, edtCanopyCover, Plant.CanopyCover, 1, Dummy);
FillEdit(Sender, edtkex, Plant.kex, 1, Dummy);
FillEdit(Sender, edtWeedHeight, Plant.WeedHeight, 1, Dummy);
// kill off the previous form elements
while (frmPools.tbsMisc.ComponentCount > 0) do
       frmPools.tbsMisc.Components[0].Free;
iCount := 100;
For iLayer := 0 to DisplayLayers do
    Begin
    iCount := iCount + 22;
    If iLayer = 0 then
       LabelDisplay (Sender, frmPools.tbsMisc, 'Surface', 40, 40, iCount + 4)
    Else
       Begin
       Str(iLayer:3, LayerInfo);
       LabelDisplay (Sender, frmPools.tbsMisc, LayerInfo + ':', 25, 68, iCount + 4);
       End;
    FillEditBoxDisplay (Sender, frmPools.tbsMisc, 40, 100, iCount, iLayer, false, Soil.LitterLig[iLayer], 100, Dummy, Dummy);
    If iLayer = 0 then
       FillEditBoxDisplay (Sender, frmPools.tbsMisc, 40, 165, iCount, iLayer, false, Soil.BranchLig[iLayer], 100, Dummy, Dummy);
    FillEditBoxDisplay (Sender, frmPools.tbsMisc, 40, 230, iCount, iLayer, false, Soil.StemLig[iLayer], 100, Dummy, Dummy);
    End;
End; {of Procedure 'FillMiscellaneousPage'}

Procedure TfrmPools.GetCarbonPage(Sender: TObject; DisplayLayers: Integer);
var iLayer, CompoCount: Integer;

    Procedure GetNext(var CompoCount: Integer; var NextElement: Real48);
    Begin
    frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[CompoCount]), NextElement, 1);
    CompoCount := CompoCount + 1;
    End;

Begin
frmMain.GetEdit(Sender, edtSapWoodC, Plant.SapWood[C], Control.CConversion);
frmMain.GetEdit(Sender, edtHeartWoodC, Plant.HeartWood[C], Control.CConversion);
frmMain.GetEdit(Sender, edtCoarseRootC, Plant.CoarseRoot[C], Control.CConversion);
frmMain.GetEdit(Sender, edtFineRootC, Plant.FineRoot[C], Control.CConversion);
frmMain.GetEdit(Sender, edtBranchesC, Plant.Branches[C], Control.CConversion);
frmMain.GetEdit(Sender, edtBarkC, Plant.Bark[C], Control.CConversion);
frmMain.GetEdit(Sender, edtLeavesC, Plant.Leaves[C], Control.CConversion);
frmMain.GetEdit(Sender, edtPollenC, Plant.Pollen[C], Control.CConversion);
frmMain.GetEdit(Sender, edtFruitC, Plant.Fruit[C], Control.CConversion);
frmMain.GetEdit(Sender, edtSolubleC, Plant.Soluble[C], Control.CConversion);
frmMain.GetEdit(Sender, edtReservesC, Plant.Reserves[C], Control.CConversion);
frmMain.GetEdit(Sender, edtWeedLeavesC, Plant.WeedLeaves[C], Control.CConversion);
frmMain.GetEdit(Sender, edtWeedRootsC, Plant.WeedRoots[C], Control.CConversion);
CompoCount := 1;
For iLayer := 0 to DisplayLayers do
    Begin
    GetNext(CompoCount, Soil.Struct[iLayer, C]);
    GetNext(CompoCount, Soil.Metab[iLayer, C]);
    if iLayer = 0 then
       GetNext(CompoCount, Soil.FineWood[iLayer, C]);
    GetNext(CompoCount, Soil.CoarseWood[iLayer, C]);
    GetNext(CompoCount, Soil.Active[iLayer, C]);
    GetNext(CompoCount, Soil.Slow[iLayer, C]);
    GetNext(CompoCount, Soil.Resistant[iLayer, C]);
    GetNext(CompoCount, Soil.Inert[iLayer, C]);
    CompoCount := CompoCount + 1;
    End;
End; {of Procedure 'GetCarbonPage'}

Procedure TfrmPools.GetNitrogenPage(Sender: TObject; DisplayLayers: Integer);
var iLayer, CompoCount: Integer;

    Procedure GetNext(var CompoCount: Integer; var NextElement: Real48);
    Begin
    frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[CompoCount]), NextElement, 1);
    CompoCount := CompoCount + 1;
    End;

Begin
frmMain.GetEdit(Sender, edtSapWoodN, Plant.SapWood[N], 1);
frmMain.GetEdit(Sender, edtHeartWoodN, Plant.HeartWood[N], 1);
frmMain.GetEdit(Sender, edtCoarseRootN, Plant.CoarseRoot[N], 1);
frmMain.GetEdit(Sender, edtFineRootN, Plant.FineRoot[N], 1);
frmMain.GetEdit(Sender, edtBranchN, Plant.Branches[N], 1);
frmMain.GetEdit(Sender, edtBarkN, Plant.Bark[N], 1);
frmMain.GetEdit(Sender, edtFoliageN, Plant.Leaves[N], 1);
frmMain.GetEdit(Sender, edtPollenN, Plant.Pollen[N], 1);
frmMain.GetEdit(Sender, edtFruitN, Plant.Fruit[N], 1);
frmMain.GetEdit(Sender, edtSolubleN, Plant.Soluble[N], 1);
frmMain.GetEdit(Sender, edtReservesN, Plant.Reserves[N], 1);
frmMain.GetEdit(Sender, edtWeedLeavesN, Plant.WeedLeaves[N], 1);
frmMain.GetEdit(Sender, edtWeedRootsN, Plant.WeedRoots[N], 1);
CompoCount := 1;
For iLayer := 0 to DisplayLayers do
    Begin
    GetNext(CompoCount, Soil.Struct[iLayer, N]);
    GetNext(CompoCount, Soil.Metab[iLayer, N]);
    if iLayer = 0 then
       GetNext(CompoCount, Soil.FineWood[iLayer, N]);
    GetNext(CompoCount, Soil.CoarseWood[iLayer, N]);
    GetNext(CompoCount, Soil.Active[iLayer, N]);
    GetNext(CompoCount, Soil.Slow[iLayer, N]);
    GetNext(CompoCount, Soil.Resistant[iLayer, N]);
    GetNext(CompoCount, Soil.Inert[iLayer, N]);
    GetNext(CompoCount, Soil.Soluble[iLayer, N]);
    CompoCount := CompoCount + 1;
    End;
End; {of Procedure 'GetNitrogenPage'}

Procedure TfrmPools.GetPhosphorusPage(Sender: TObject; DisplayLayers: Integer);
var iLayer, CompoCount: Integer;

    Procedure GetNext(var CompoCount: Integer; var NextElement: Real48);
    Begin
    frmMain.GetEdit(Sender, TEdit(frmPools.tbsPhosphorus.Components[CompoCount]), NextElement, 1);
    CompoCount := CompoCount + 1;
    End;

Begin
frmMain.GetEdit(Sender, edtSapWoodP, Plant.SapWood[P], 1);
frmMain.GetEdit(Sender, edtHeartWoodP, Plant.HeartWood[P], 1);
frmMain.GetEdit(Sender, edtCoarseRootP, Plant.CoarseRoot[P], 1);
frmMain.GetEdit(Sender, edtFineRootP, Plant.FineRoot[P], 1);
frmMain.GetEdit(Sender, edtBranchP, Plant.Branches[P], 1);
frmMain.GetEdit(Sender, edtBarkP, Plant.Bark[P], 1);
frmMain.GetEdit(Sender, edtFoliageP, Plant.Leaves[P], 1);
frmMain.GetEdit(Sender, edtPollenP, Plant.Pollen[P], 1);
frmMain.GetEdit(Sender, edtFruitP, Plant.Fruit[P], 1);
frmMain.GetEdit(Sender, edtSolubleP, Plant.Soluble[P], 1);
frmMain.GetEdit(Sender, edtReservesP, Plant.Reserves[P], 1);
frmMain.GetEdit(Sender, edtWeedLeavesP, Plant.WeedLeaves[P], 1);
frmMain.GetEdit(Sender, edtWeedRootsP, Plant.WeedRoots[P], 1);
CompoCount := 1;
For iLayer := 0 to DisplayLayers do
    Begin
    GetNext(CompoCount, Soil.Struct[iLayer, P]);
    GetNext(CompoCount, Soil.Metab[iLayer, P]);
    if iLayer = 0 then
       GetNext(CompoCount, Soil.FineWood[iLayer, P]);
    GetNext(CompoCount, Soil.CoarseWood[iLayer, P]);
    GetNext(CompoCount, Soil.Active[iLayer, P]);
    GetNext(CompoCount, Soil.Slow[iLayer, P]);
    GetNext(CompoCount, Soil.Resistant[iLayer, P]);
    GetNext(CompoCount, Soil.Inert[iLayer, P]);
    GetNext(CompoCount, Soil.Soluble[iLayer, P]);
    GetNext(CompoCount, Soil.SecondaryInorganicP[iLayer, P]);
    GetNext(CompoCount, Soil.RockP[iLayer, P]);
    GetNext(CompoCount, Soil.OccludedP[iLayer, P]);
    CompoCount := CompoCount + 1;
    End;
End; {of Procedure 'GetPhosphorusPage'}

Procedure TfrmPools.GetMiscellaneousPage(Sender: TObject; DisplayLayers: Integer);
var iLayer: Integer;
    WaterIn: Real48;
Begin
frmMain.GetEdit(Sender, TEdit(frmPools.tbsMisc.Components[1]), Soil.LitterLig[0], 100);
frmMain.GetEdit(Sender, TEdit(frmPools.tbsMisc.Components[2]), Soil.BranchLig[0], 100);
frmMain.GetEdit(Sender, TEdit(frmPools.tbsMisc.Components[3]), Soil.StemLig[0], 100);
For iLayer := 1 to DisplayLayers do
    Begin
    frmMain.GetEdit(Sender, TEdit(frmPools.tbsMisc.Components[iLayer * 3 + 2]), Soil.LitterLig[iLayer], 100);
    frmMain.GetEdit(Sender, TEdit(frmPools.tbsMisc.Components[iLayer * 3 + 3]), Soil.StemLig[iLayer], 100);
    End;
frmMain.GetInteger(Sender, edtPlantAge, Plant.Age);
frmMain.GetEdit(Sender, edtStocking, Plant.Stocking, 1);
frmMain.GetEdit(Sender, edtSoilWater, WaterIn, 1);
If WaterIn > Soil.MaxWater then
   Begin
   For iLayer := 1 to DisplayLayers do
       Soil.WaterLayer[iLayer].WaterContent := Soil.WaterLayer[iLayer].MaxWater;
   Soil.TotalWater := Soil.MaxWater;
   End
Else if Soil.MaxWater = 0 then
   For iLayer := 1 to Soil.nLayers do
       Soil.WaterLayer[iLayer].WaterContent := 0
Else if WaterIn <> Soil.TotalWater then
   Begin
   If Soil.TotalWater > 0 then
      For iLayer := 1 to Soil.nLayers do
          Soil.WaterLayer[iLayer].WaterContent := Soil.WaterLayer[iLayer].WaterContent * WaterIn / Soil.TotalWater
   Else
      For iLayer := 1 to Soil.nLayers do
          Soil.WaterLayer[iLayer].WaterContent := Soil.WaterLayer[iLayer].MaxWater * WaterIn / Soil.MaxWater;
   Soil.TotalWater := WaterIn;
   End;
frmMain.GetInteger(Sender, edtStartDay, Control.ExtraDays);
frmMain.GetInteger(Sender, edtStartMonth, Control.ExtraMonths);
frmMain.GetInteger(Sender, edtStartYear, Control.TotalYears);
frmMain.GetEdit(Sender, edtHeight, Plant.Height, 1);
frmMain.GetEdit(Sender, edtDBH, Plant.DBH, 1);
frmMain.GetEdit(Sender, edtCanopyCover, Plant.CanopyCover, 1);
frmMain.GetEdit(Sender, edtkex, Plant.kex, 1);
frmMain.GetEdit(Sender, edtArea, Plant.Area, 0.0001);
frmMain.GetEdit(Sender, edtWeedHeight, Plant.WeedHeight, 1);
End; {of Procedure 'GetMiscellaneousPage'}

Procedure TfrmPools.FormShow(Sender: TObject);
Begin
btnCancel.visible := true;
CalcSettings(Sender, DisplayLayers);
FillCarbonPage(Sender, DisplayLayers);
FillNitrogenPage(Sender, DisplayLayers);
If Control.IncludeP then
   Begin
   frmPools.tbsPhosphorus.TabVisible := true;
   FillPhosphorusPage(Sender, DisplayLayers);
   End
Else
   Begin
   frmPools.tbsPhosphorus.TabVisible := false;
   frmPools.tbsPhosphorus.Enabled := false;
   End;
FillMiscellaneousPage(Sender, DisplayLayers);
End;

Procedure TfrmPools.btnOKClick(Sender: TObject);
var DisplayLayers: Integer;
Begin
If Control.AllOneLayer then
   DisplayLayers := 1
Else
   DisplayLayers := Soil.nLayers;
GetCarbonPage(Sender, DisplayLayers);
GetNitrogenPage(Sender, DisplayLayers);
If Control.IncludeP then
   GetPhosphorusPage(Sender, DisplayLayers);
If ChkResetNPPools.Checked then;
   AdjustNandP (Sender, DisplayLayers);
GetMiscellaneousPage(Sender, DisplayLayers);
Control.InitHasChanged := TRUE;
End;

Procedure TfrmPools.pgcPagesChange(Sender: TObject);
var DisplayLayers: Integer;
Begin
btnCancel.Visible := false;
If Control.AllOneLayer then
   DisplayLayers := 1
Else
   DisplayLayers := Soil.nLayers;
If pgcPages.ActivePage = tbsCarbon then
   Begin
   GetNitrogenPage(Sender, DisplayLayers);
   If Control.IncludeP then
      GetPhosphorusPage(Sender, DisplayLayers);
   GetMiscellaneousPage(Sender, DisplayLayers);
   FillCarbonPage(Sender, DisplayLayers);
   End
Else if pgcPages.ActivePage = tbsNitrogen then
   Begin
   GetCarbonPage(Sender, DisplayLayers);
   If Control.IncludeP then
      GetPhosphorusPage(Sender, DisplayLayers);
   GetMiscellaneousPage(Sender, DisplayLayers);
   If ChkResetNPPools.Checked then
      AdjustNandP(Sender, DisplayLayers);
   SetOldValues(DisplayLayers);
   FillNitrogenPage(Sender, DisplayLayers);
   if Control.IncludeP then
      FillPhosphorusPage(Sender, DisplayLayers);
   End
Else if pgcPages.ActivePage = tbsPhosphorus then
   Begin
   GetCarbonPage(Sender, DisplayLayers);
   GetNitrogenPage(Sender, DisplayLayers);
   GetMiscellaneousPage(Sender, DisplayLayers);
   If ChkResetNPPools.Checked then
      AdjustNandP(Sender, DisplayLayers);
   SetOldValues(DisplayLayers);
   FillPhosphorusPage(Sender, DisplayLayers);
   FillNitrogenPage(Sender, DisplayLayers);
   End
Else If pgcPages.ActivePage = tbsMisc then;
   Begin
   GetCarbonPage(Sender, DisplayLayers);
   GetNitrogenPage(Sender, DisplayLayers);
   If Control.IncludeP then
      GetPhosphorusPage(Sender, DisplayLayers);
   If ChkResetNPPools.Checked then
      AdjustNandP(Sender, DisplayLayers);
   SetOldValues(DisplayLayers);
   FillNitrogenPage(Sender, DisplayLayers);
   if Control.IncludeP then
      FillPhosphorusPage(Sender, DisplayLayers);
   FillMiscellaneousPage(Sender, DisplayLayers);
   End;
End;

Procedure TfrmPools.btnHelpClick(Sender: TObject);
begin
  // show the appropriate help page
  case pgcPages.ActivePageIndex of
    0: Application.HelpContext(idm_CarbonPools);
    1: Application.HelpContext(idm_NitrogenPools);
    2: Application.HelpContext(idm_Miscellaneous);
  end;
end;

Procedure TfrmPools.rdRescalePClick(Sender: TObject);
var ChangeP: Real48;
    iLayer: Integer;

Begin
frmMain.GetEdit(Sender, edtChangePercP, ChangeP, 100);
frmPools.pgcPagesChange(Sender);
for iLayer := 0 to DisplayLayers do
    Begin
    Soil.Struct[iLayer, P] := Soil.Struct[iLayer, P] * ChangeP;
    Soil.Metab[iLayer, P] := Soil.Metab[iLayer, P] * ChangeP;
    Soil.CoarseWood[iLayer, P] := Soil.CoarseWood[iLayer, P] * ChangeP;
    Soil.FineWood[iLayer, P] := Soil.FineWood[iLayer, P] * ChangeP;
    Soil.Active[iLayer, P] := Soil.Active[iLayer, P] * ChangeP;
    Soil.Slow[iLayer, P] := Soil.Slow[iLayer, P] * ChangeP;
    Soil.Resistant[iLayer, P] := Soil.Resistant[iLayer, P] * ChangeP;
    Soil.Inert[iLayer, P] := Soil.Inert[iLayer, P] * ChangeP;
    End;
rdRescaleP.Checked := false;
frmPools.FormShow(Sender);
btnCancel.Visible := false;
end;

Procedure TfrmPools.rdRescaleSOMClick(Sender: TObject);
var ChangeSOM: Real48;

Procedure ScaleSOM (var Pool: SoilElements; ChangeSOM: Real48);
var iLayer: Integer;

Begin
For iLayer := 0 to DisplayLayers do
    Begin
    Pool[iLayer, C] := Pool[iLayer, C] * ChangeSOM;
    If ChkResetNPPools.Checked then
       Begin
       if Control.IncludeP then
          Pool[iLayer, P] := Pool[iLayer, P] * ChangeSOM;
       Pool[iLayer, N] := Pool[iLayer, N] * ChangeSOM;
       End;
    End;
End;

begin
frmMain.GetEdit(Sender, edtChangePerc, ChangeSOM, 100);
frmPools.pgcPagesChange(Sender);
ScaleSOM(Soil.Struct, ChangeSOM);
ScaleSOM(Soil.Metab, ChangeSOM);
ScaleSOM(Soil.CoarseWood, ChangeSOM);
ScaleSOM(Soil.FineWood, ChangeSOM);
ScaleSOM(Soil.Active, ChangeSOM);
ScaleSOM(Soil.Slow, ChangeSOM);
ScaleSOM(Soil.Resistant, ChangeSOM);
ScaleSOM(Soil.Inert, ChangeSOM);
rdRescaleSOM.Checked := false;
frmPools.FormShow(Sender);
btnCancel.Visible := false;
end;

procedure TfrmPools.rdRescaleSONClick(Sender: TObject);
var ChangeN: Real48;
    iLayer: Integer;

Begin
frmMain.GetEdit(Sender, edtChangePercN, ChangeN, 100);
frmPools.pgcPagesChange(Sender);
for iLayer := 0 to DisplayLayers do
    Begin
    Soil.Struct[iLayer, N] := Soil.Struct[iLayer, N] * ChangeN;
    Soil.Metab[iLayer, N] := Soil.Metab[iLayer, N] * ChangeN;
    Soil.CoarseWood[iLayer, N] := Soil.CoarseWood[iLayer, N] * ChangeN;
    Soil.FineWood[iLayer, N] := Soil.FineWood[iLayer, N] * ChangeN;
    Soil.Active[iLayer, N] := Soil.Active[iLayer, N] * ChangeN;
    Soil.Slow[iLayer, N] := Soil.Slow[iLayer, N] * ChangeN;
    Soil.Resistant[iLayer, N] := Soil.Resistant[iLayer, N] * ChangeN;
    Soil.Inert[iLayer, N] := Soil.Inert[iLayer, N] * ChangeN;
    End;
rdRescaleSON.Checked := false;
frmPools.FormShow(Sender);
btnCancel.Visible := false;
end;

Procedure TfrmPools.btnEditC13Click(Sender: TObject);
begin
frmC13Pools.pgcPages.ActivePage := frmC13Pools.tbsCarbon;
if (frmC13Pools.ShowModal = mrOK) then
   // Do nothing
End;
end.
