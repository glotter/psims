{ ================================================================
  = Project   : CENW                                             =
  ================================================================
  = Modules   : TfrmPools                                        =
  =                                                              =
  =             Interface routine to show all pools              =
  =             and allow the user to change them.               =
  ================================================================
  = File      : untPools.PAS                                     =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

unit untPools;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, untDeclarations;

type
  OldPlantType = Record
        SapWood, HeartWood, CoarseRoot, FineRoot, Branches, Bark, Pollen,
        Fruit, Soluble, Leaves, Reserves, Dummy: Real48;
        End;

  OldSoilType = Record
        StructLitter, MetabLitter, FineWoodLitter, CoarseWoodLitter,
        Active, Slow, Resistant, Inert: OrganicFlowType;
        End;

  TfrmPools = class(TForm)
    pgcPages: TPageControl;
    tbsCarbon: TTabSheet;
    grpPlantPools: TGroupBox;
    ChkResetNPools: TCheckBox;
    pnlButtons: TPanel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    tbsNitrogen: TTabSheet;
    tbsMisc: TTabSheet;
    lblLine1: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    grpInfo: TGroupBox;
    GroupBox3: TGroupBox;
    lbStruct1: TLabel;
    lbFineWood1: TLabel;
    lbCoarseWood1: TLabel;
    lbLignin: TLabel;
    lbLayer: TLabel;
    lbCLayer: TLabel;
    lbStructuralC: TLabel;
    lbMetabolicC: TLabel;
    lbActiveC: TLabel;
    lbSlowC: TLabel;
    lbResistantC: TLabel;
    lbFineWoodC: TLabel;
    lbCoarseWoodC: TLabel;
    lbLayerN: TLabel;
    Label3: TLabel;
    lbStructN: TLabel;
    lbMetabolicN: TLabel;
    lbFineWoodN: TLabel;
    lbCoarseWoodN: TLabel;
    Label4: TLabel;
    lbActiveN: TLabel;
    lbSlowN: TLabel;
    lbResistantN: TLabel;
    lbSolubleN: TLabel;
    rdRescaleSOM: TRadioButton;
    btnEditC13: TButton;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    edtPlantAge: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    edtStocking: TEdit;
    edtSoilWater: TEdit;
    Label10: TLabel;
    edtSnow: TEdit;
    Label11: TLabel;
    edtCanopyCover: TEdit;
    Label12: TLabel;
    edtkex: TEdit;
    Label13: TLabel;
    edtStartDay: TEdit;
    Label14: TLabel;
    edtStartMonth: TEdit;
    Label15: TLabel;
    Label16: TLabel;
    edtStartYear: TEdit;
    edtHeight: TEdit;
    Label17: TLabel;
    Label18: TLabel;
    edtDBH: TEdit;
    edtArea: TEdit;
    Label19: TLabel;
    Label20: TLabel;
    edtSapwoodN: TEdit;
    edtHeartwoodN: TEdit;
    Label21: TLabel;
    edtCoarseRootN: TEdit;
    Label22: TLabel;
    edtBarkN: TEdit;
    Label23: TLabel;
    edtBranchN: TEdit;
    Label24: TLabel;
    Label25: TLabel;
    edtFoliageN: TEdit;
    edtFineRootN: TEdit;
    Label26: TLabel;
    edtFruitN: TEdit;
    Label27: TLabel;
    edtPollenN: TEdit;
    Label28: TLabel;
    edtSolubleN: TEdit;
    Label29: TLabel;
    edtReservesN: TEdit;
    Label30: TLabel;
    edtChangePerc: TEdit;
    lblPercCurrent: TLabel;
    edtSapWoodC: TEdit;
    Label32: TLabel;
    edtHeartWoodC: TEdit;
    Label33: TLabel;
    edtCoarseRootC: TEdit;
    Label34: TLabel;
    edtBarkC: TEdit;
    Label35: TLabel;
    edtBranchesC: TEdit;
    Label36: TLabel;
    edtLeavesC: TEdit;
    Label37: TLabel;
    edtFineRootC: TEdit;
    Label38: TLabel;
    edtFruitC: TEdit;
    Label39: TLabel;
    edtPollenC: TEdit;
    Label40: TLabel;
    edtSolubleC: TEdit;
    Label41: TLabel;
    edtReservesC: TEdit;
    Label42: TLabel;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure pgcPagesChange(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure rdRescaleSOMClick(Sender: TObject);
    procedure btnEditC13Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPools: TfrmPools;
  OldSoil : OldSoilType;
  OldPlant: OldPlantType;
  DisplayLayers: Integer;

implementation

{$R *.DFM}

uses
  untMiscellaneous, untHelpConsts, untC13Pools, untMain;

procedure TfrmPools.FormShow(Sender: TObject);
var i, iCount, iLayer: integer;
    Dummy, SumStruct, SumMetab, SumFine, SumCoarse, SumActive, SumSlow,
    SumResistant, SumInert, SumMineral: Real48;
    LayerInfo: String;

  Procedure FillEditBoxDisplay(TabSheet: TTabSheet; Width, Left, Top: Integer;
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
    End;

  procedure FillEdit(Sender: TObject; edtBox: TEdit; fValue, Multiplier: Real48; var OldVar: Real48);
    Begin
    Oldvar := fValue;
    frmMain.FillEdit(Sender, edtBox, fValue, Multiplier);
    end;

  Procedure LabelDisplay(TabSheet: TTabSheet; Caption: String; Width, Left, Top: Integer);
    var NextLabel: TLabel;
    Begin
    NextLabel := TLabel.Create(TabSheet);
    NextLabel.Parent := TabSheet;
    NextLabel.Caption := Caption;
    NextLabel.Width := Width;
    NextLabel.Left := Left;
    NextLabel.Top := Top;
    End;

   Begin
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
      DisplayLayers := SoilOrganic.nLayers;
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
  // carbon pools
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
   FillEdit(Sender, edtChangePerc, 100, 1, OldPlant.Dummy);
   while (frmPools.tbsCarbon.ComponentCount > 0) do
         frmPools.tbsCarbon.Components[0].Free;
  // fill form with parameters
  iCount := 100;
  For iLayer := 0 to DisplayLayers do
     Begin
     iCount := iCount + 22;
     If iLayer = 0 then
        LabelDisplay (frmPools.tbsCarbon, 'Surface', 40, 35, iCount + 4)
     Else
        Begin
        Str(iLayer:3, LayerInfo);
        LabelDisplay (frmPools.tbsCarbon, LayerInfo + ':', 25, 53, iCount + 4);
        End;
     FillEditBoxDisplay (frmPools.tbsCarbon, 50, 85, iCount, false, SoilOrganic.Struct[iLayer, C], 1, OldSoil.StructLitter[iLayer], SumStruct);
     FillEditBoxDisplay (frmPools.tbsCarbon, 50, 170, iCount, false, SoilOrganic.Metab[iLayer, C], 1, OldSoil.MetabLitter[iLayer], SumMetab);
     If iLayer = 0 then
        FillEditBoxDisplay (frmPools.tbsCarbon, 50, 255, iCount, false, SoilOrganic.FineWood[0, C], 1, OldSoil.FineWoodLitter[0], SumFine);
     FillEditBoxDisplay (frmPools.tbsCarbon, 50, 340, iCount, false, SoilOrganic.CoarseWood[iLayer, C], 1, OldSoil.CoarseWoodLitter[iLayer], SumCoarse);
     FillEditBoxDisplay (frmPools.tbsCarbon, 50, 425, iCount, false, SoilOrganic.Active[iLayer, C], 1, OldSoil.Active[iLayer], SumActive);
     FillEditBoxDisplay (frmPools.tbsCarbon, 50, 510, iCount, false, SoilOrganic.Slow[iLayer, C], 1, OldSoil.Slow[iLayer], SumSlow);
     FillEditBoxDisplay (frmPools.tbsCarbon, 50, 595, iCount, false, SoilOrganic.Resistant[iLayer, C], 1, OldSoil.Resistant[iLayer], SumResistant);
     FillEditBoxDisplay (frmPools.tbsCarbon, 50, 680, iCount, false, SoilOrganic.Inert[iLayer, C], 1, OldSoil.Inert[iLayer], SumInert);
     End;
     iCount := iCount + 22;
     LabelDisplay (frmPools.tbsCarbon, 'Total:', 40, 40, iCount + 4);
  FillEditBoxDisplay (frmPools.tbsCarbon, 50, 85, iCount, true, SumStruct, 1, Dummy, Dummy);
  FillEditBoxDisplay (frmPools.tbsCarbon, 50, 170, iCount, true, SumMetab, 1, Dummy, Dummy);
  FillEditBoxDisplay (frmPools.tbsCarbon, 50, 255, iCount, true, SumFine, 1, Dummy, Dummy);
  FillEditBoxDisplay (frmPools.tbsCarbon, 50, 340, iCount, true, SumCoarse, 1, Dummy, Dummy);
  FillEditBoxDisplay (frmPools.tbsCarbon, 50, 425, iCount, true, SumActive, 1, Dummy, Dummy);
  FillEditBoxDisplay (frmPools.tbsCarbon, 50, 510, iCount, true, SumSlow, 1, Dummy, Dummy);
  FillEditBoxDisplay (frmPools.tbsCarbon, 50, 595, iCount, true, SumResistant, 1, Dummy, Dummy);
  FillEditBoxDisplay (frmPools.tbsCarbon, 50, 680, iCount, true, SumInert, 1, Dummy, Dummy);
  frmPools.ChkResetNPools.Top := iCount + 30;
  frmPools.rdRescaleSOM.Top := iCount + 30;
  frmPools.edtChangePerc.Top := iCount + 28;
  frmPools.lblPercCurrent.Top := iCount + 30;
  // nitrogen pools
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
  iCount := 100;
  For iLayer := 0 to DisplayLayers do
     Begin
     iCount := iCount + 22;
     If iLayer = 0 then
        LabelDisplay (frmPools.tbsNitrogen, 'Surface', 40, 10, iCount + 4)
     Else
        Begin
        Str(iLayer:3, LayerInfo);
        LabelDisplay (frmPools.tbsNitrogen,LayerInfo + ':', 25, 28, iCount + 4);
        End;
     FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 55, iCount, false, SoilOrganic.Struct[iLayer, N], 1, Dummy, SumStruct);
     FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 140, iCount, false, SoilOrganic.Metab[iLayer, N], 1, Dummy, SumMetab);
     If iLayer = 0 then
        FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 225, iCount, false, SoilOrganic.FineWood[0, N], 1, Dummy, SumFine);
     FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 310, iCount, false, SoilOrganic.CoarseWood[iLayer, N], 1, Dummy, SumCoarse);
     FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 395, iCount, false, SoilOrganic.Active[iLayer, N], 1, Dummy, SumActive);
     FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 480, iCount, false, SoilOrganic.Slow[iLayer, N], 1, Dummy, SumSlow);
     FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 565, iCount, false, SoilOrganic.Resistant[iLayer, N], 1, Dummy, SumResistant);
     FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 650, iCount, false, SoilOrganic.Inert[iLayer, N], 1, Dummy, SumInert);
     FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 735, iCount, false, SoilOrganic.Soluble[iLayer, N], 1, Dummy, SumMineral);
     End;
  iCount := iCount + 22;
  LabelDisplay (frmPools.tbsNitrogen, 'Total:', 40, 15, iCount + 4);
  FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 55, iCount, true, SumStruct, 1, Dummy, Dummy);
  FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 140, iCount, true, SumMetab, 1, Dummy, Dummy);
  FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 225, iCount, true, SumFine, 1, Dummy, Dummy);
  FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 310, iCount, true, SumCoarse, 1, Dummy, Dummy);
  FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 395, iCount, true, Sumactive, 1, Dummy, Dummy);
  FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 480, iCount, true, SumSlow, 1, Dummy, Dummy);
  FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 565, iCount, true, SumResistant, 1, Dummy, Dummy);
  FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 650, iCount, true, SumInert, 1, Dummy, Dummy);
  FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 735, iCount, true, SumMineral, 1, Dummy, Dummy);
  // miscellaneous
  frmMain.FillEdit(Sender, edtPlantAge, Plant.Age, 0);
  frmMain.FillEdit(Sender, edtStocking, Round(Plant.Stocking), 0);
  SoilWat.TotalWater := 0;
  For i := 1 to SoilWat.nLayers do
      SoilWat.TotalWater := SoilWat.TotalWater + SoilWat.Layer[i].WaterContent;
  FillEdit(Sender, edtSoilWater, SoilWat.TotalWater, 1, Dummy);
  FillEdit(Sender, edtSnow, SoilWat.Snow, 1, Dummy);
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
     // kill off the previous form elements - leave only 5 Components
  while (frmPools.tbsMisc.ComponentCount > 0) do
         frmPools.tbsMisc.Components[0].Free;
  // fill form with parameters
  iCount := 100;
  For iLayer := 0 to DisplayLayers do
     Begin
     iCount := iCount + 22;
     If iLayer = 0 then
        LabelDisplay (frmPools.tbsMisc, 'Surface', 40, 40, iCount + 4)
     Else
        Begin
        Str(iLayer:3, LayerInfo);
        LabelDisplay (frmPools.tbsMisc, LayerInfo + ':', 25, 68, iCount + 4);
        End;
     FillEditBoxDisplay (frmPools.tbsMisc, 40, 100, iCount, false, SoilOrganic.LitterLig[iLayer], 100, Dummy, Dummy);
     If iLayer = 0 then
        FillEditBoxDisplay (frmPools.tbsMisc, 40, 165, iCount, false, SoilOrganic.BranchLig[iLayer], 100, Dummy, Dummy);
     FillEditBoxDisplay (frmPools.tbsMisc, 40, 230, iCount, false, SoilOrganic.StemLig[iLayer], 100, Dummy, Dummy);
     End;

end;

procedure TfrmPools.btnOKClick(Sender: TObject);
var WaterIn: real48;
    DisplayLayers, i, iLayer: Integer;

    Procedure New_NandP (Var NewN_P: Real48; NewC, OldC: real48);
    Begin
    If OldC <> 0 then
       NewN_P := NewN_P * NewC / OldC
    Else
      {Do nothing and keep NewN_P with old value};
    End; {of Procedure 'New_NandP'}

begin
  // carbon
   If Control.AllOneLayer then
      DisplayLayers := 1
   Else
      DisplayLayers := SoilOrganic.nLayers;
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
  frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[1]), SoilOrganic.Struct[0, C], 1);
  frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[2]), SoilOrganic.Metab[0, C], 1);
  frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[3]), SoilOrganic.FineWood[0, C], 1);
  frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[4]), SoilOrganic.CoarseWood[0, C], 1);
  frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[5]), SoilOrganic.Active[0, C], 1);
  frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[6]), SoilOrganic.Slow[0, C], 1);
  frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[7]), SoilOrganic.Resistant[0, C], 1);
  frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[8]), SoilOrganic.Inert[0, C], 1);
  For iLayer := 1 to DisplayLayers do
      Begin
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[iLayer * 8 + 2]), SoilOrganic.Struct[iLayer, C], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[iLayer * 8 + 3]), SoilOrganic.Metab[iLayer, C], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[iLayer * 8 + 4]), SoilOrganic.CoarseWood[iLayer, C], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[iLayer * 8 + 5]), SoilOrganic.Active[iLayer, C], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[iLayer * 8 + 6]), SoilOrganic.Slow[iLayer, C], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[iLayer * 8 + 7]), SoilOrganic.Resistant[iLayer, C], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[iLayer * 8 + 8]), SoilOrganic.Inert[iLayer, C], 1);
      End;
  // nitrogen
  frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[1]), SoilOrganic.Struct[0, N], 1);
  frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[2]), SoilOrganic.Metab[0, N], 1);
  frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[3]), SoilOrganic.FineWood[0, N], 1);
  frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[4]), SoilOrganic.CoarseWood[0, N], 1);
  frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[5]), SoilOrganic.Active[0, N], 1);
  frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[6]), SoilOrganic.Slow[0, N], 1);
  frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[7]), SoilOrganic.Resistant[0, N], 1);
  frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[8]), SoilOrganic.Inert[0, N], 1);
  frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[9]), SoilOrganic.Soluble[0, N], 1);
  For iLayer := 1 to DisplayLayers do
      Begin
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[iLayer * 9 + 2]), SoilOrganic.Struct[iLayer, N], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[iLayer * 9 + 3]), SoilOrganic.Metab[iLayer, N], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[iLayer * 9 + 4]), SoilOrganic.CoarseWood[iLayer, N], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[iLayer * 9 + 5]), SoilOrganic.Active[iLayer, N], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[iLayer * 9 + 6]), SoilOrganic.Slow[iLayer, N], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[iLayer * 9 + 7]), SoilOrganic.Resistant[iLayer, N], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[iLayer * 9 + 8]), SoilOrganic.Inert[iLayer, N], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[iLayer * 9 + 9]), SoilOrganic.Soluble[iLayer, N], 1);
      End;
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
  // reset the N values if necessary - throws away the other changes
  If ChkResetNPools.Checked then;
      Begin
      For iLayer := 0 to DisplayLayers do
          Begin
          New_NandP(SoilOrganic.Struct[iLayer, N], soilorganic.Struct[iLayer, C], OldSoil.StructLitter[iLayer]);
          New_NandP(SoilOrganic.Metab[iLayer, N], soilorganic.Metab[iLayer, C], OldSoil.MetabLitter[iLayer]);
          If iLayer = 0 then
             New_NandP(SoilOrganic.FineWood[0, N], soilorganic.FineWood[0, C], OldSoil.FineWoodLitter[0]);
          New_NandP(SoilOrganic.CoarseWood[iLayer, N], soilorganic.CoarseWood[iLayer, C], OldSoil.CoarseWoodLitter[iLayer]);
          New_NandP(SoilOrganic.Active[iLayer, N], soilorganic.Active[iLayer, C], OldSoil.Active[iLayer]);
          New_NandP(SoilOrganic.Slow[iLayer, N], soilorganic.Slow[iLayer, C], OldSoil.Slow[iLayer]);
          New_NandP(SoilOrganic.Resistant[iLayer, N], soilorganic.Resistant[iLayer, C], OldSoil.Resistant[iLayer]);
          New_NandP(SoilOrganic.Inert[iLayer, N], soilorganic.Inert[iLayer, C], OldSoil.Inert[iLayer]);
          End;
      New_NandP(Plant.SapWood[N], Plant.SapWood[C], OldPlant.SapWood);
      New_NandP(Plant.HeartWood[N], Plant.HeartWood[C], OldPlant.HeartWood);
      New_NandP(Plant.CoarseRoot[N], Plant.CoarseRoot[C], OldPlant.CoarseRoot);
      New_NandP(Plant.FineRoot[N], Plant.FineRoot[C], OldPlant.FineRoot);
      New_NandP(Plant.Branches[N], Plant.Branches[C], OldPlant.Branches);
      New_NandP(Plant.Bark[N], Plant.Bark[C], OldPlant.Bark);
      New_NandP(Plant.Leaves[N], Plant.Leaves[C], OldPlant.Leaves);
      New_NandP(Plant.Pollen[N], Plant.Pollen[C], OldPlant.Pollen);
      New_NandP(Plant.Fruit[N], Plant.Fruit[C], OldPlant.Fruit);
      New_NandP(Plant.Soluble[N], Plant.Soluble[C], OldPlant.Soluble);
      New_NandP(Plant.Reserves[N], Plant.Reserves[C], OldPlant.Reserves);
      End;
  // miscellaneous
  frmMain.GetEdit(Sender, TEdit(frmPools.tbsMisc.Components[1]), SoilOrganic.LitterLig[0], 100);
  frmMain.GetEdit(Sender, TEdit(frmPools.tbsMisc.Components[2]), SoilOrganic.BranchLig[0], 100);
  frmMain.GetEdit(Sender, TEdit(frmPools.tbsMisc.Components[3]), SoilOrganic.StemLig[0], 100);
  For iLayer := 1 to DisplayLayers do
      Begin
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsMisc.Components[iLayer * 3 + 2]), SoilOrganic.LitterLig[iLayer], 100);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsMisc.Components[iLayer * 3 + 3]), SoilOrganic.StemLig[iLayer], 100);
      End;
  frmMain.GetInteger(Sender, edtPlantAge, Plant.Age);
  frmMain.GetEdit(Sender, edtStocking, Plant.Stocking, 1);
  frmMain.GetEdit(Sender, edtSoilWater, WaterIn, 1);
   If WaterIn > SoilWat.MaxWater then
      Begin
      For i := 1 to DisplayLayers do
          SoilWat.Layer[i].WaterContent := SoilWat.Layer[i].MaxWater;
      SoilWat.TotalWater := SoilWat.MaxWater;
      End
   Else if SoilWat.MaxWater = 0 then
      For i := 1 to SoilWat.nLayers do
          SoilWat.Layer[i].WaterContent := 0
   Else if WaterIn <> SoilWat.TotalWater then
      Begin
      If SoilWat.TotalWater > 0 then
         For i := 1 to SoilWat.nLayers do
             SoilWat.Layer[i].WaterContent := SoilWat.Layer[i].WaterContent * WaterIn / SoilWat.TotalWater
      Else
         For i := 1 to SoilWat.nLayers do
             SoilWat.Layer[i].WaterContent := SoilWat.Layer[i].MaxWater * WaterIn / SoilWat.MaxWater;
      SoilWat.TotalWater := WaterIn;
      End;
  frmMain.GetInteger(Sender, edtStartDay, Control.ExtraDays);
  frmMain.GetInteger(Sender, edtStartMonth, Control.ExtraMonths);
  frmMain.GetInteger(Sender, edtStartYear, Control.TotalYears);
  frmMain.GetEdit(Sender, edtHeight, Plant.Height, 1);
  frmMain.GetEdit(Sender, edtDBH, Plant.DBH, 1);
  frmMain.GetEdit(Sender, edtCanopyCover, Plant.CanopyCover, 1);
  frmMain.GetEdit(Sender, edtkex, Plant.kex, 1);
  frmMain.GetEdit(Sender, edtArea, Plant.Area, 0.0001);
  Control.InitHasChanged := TRUE;
end;

procedure TfrmPools.pgcPagesChange(Sender: TObject);
var iCount, iLayer, DisplayLayers: integer;
    Dummy, SumStruct, SumMetab, SumFine, SumCoarse, SumActive, SumSlow, SumResistant, SumMineral, SumInert: Real48;
    LayerInfo: String;

    Procedure New_NandP (Var NewN_P: Real48; NewC, OldC: real48);
    Begin
    If OldC <> 0 then
       NewN_P := NewN_P * NewC / OldC
    Else
       {Do nothing and keep NewN_P with old value};
    End; {of Procedure 'New_NandP'}

  Procedure FillEditBoxDisplay(TabSheet: TTabSheet; Width, Left, Top: Integer;
            ReadOnlyFlag: Boolean; fValue, Multiplier: double; var OldVar, Sum: Real48);
  var NextBox: TEdit;
      FieldWidth, Digits: Integer;
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
    End;

  Procedure FillEditNormal(edtBox: TEdit; fValue: Real48; var Sum: Real48);
    begin
    frmMain.FillEdit(Sender, edtBox, fValue, 1);
    If iLayer = 0 then
       Sum := fValue
    Else
       Sum := Sum + fValue;
    end;

  Procedure MakeOld(fValue: Real48; var OldVar: Real48);
  begin
    Oldvar := fValue;
  end;

  Procedure LabelDisplay(TabSheet: TTabSheet; Caption: String; Width, Left, Top: Integer);
  var NextLabel: TLabel;
    Begin
    NextLabel := TLabel.Create(TabSheet);
    NextLabel.Parent := TabSheet;
    NextLabel.Caption := Caption;
    NextLabel.Width := Width;
    NextLabel.Left := Left;
    NextLabel.Top := Top;
    End;

begin
   If Control.AllOneLayer then
      DisplayLayers := 1
   Else
      DisplayLayers := SoilOrganic.nLayers;
   If ChkResetNPools.Checked and (pgcPages.ActivePage = tbsNitrogen) then
      Begin
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[1]), SoilOrganic.Struct[0, C], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[2]), SoilOrganic.Metab[0, C], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[3]), SoilOrganic.FineWood[0, C], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[4]), SoilOrganic.CoarseWood[0, C], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[5]), SoilOrganic.Active[0, C], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[6]), SoilOrganic.Slow[0, C], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[7]), SoilOrganic.Resistant[0, C], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[8]), SoilOrganic.Inert[0, C], 1);
      For iLayer := 1 to DisplayLayers do
          Begin
          frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[iLayer * 8 + 2]), SoilOrganic.Struct[iLayer, C], 1);
          frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[iLayer * 8 + 3]), SoilOrganic.Metab[iLayer, C], 1);
          frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[iLayer * 8 + 4]), SoilOrganic.CoarseWood[iLayer, C], 1);
          frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[iLayer * 8 + 5]), SoilOrganic.Active[iLayer, C], 1);
          frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[iLayer * 8 + 6]), SoilOrganic.Slow[iLayer, C], 1);
          frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[iLayer * 8 + 7]), SoilOrganic.Resistant[iLayer, C], 1);
          frmMain.GetEdit(Sender, TEdit(frmPools.tbsCarbon.Components[iLayer * 8 + 8]), SoilOrganic.Inert[iLayer, C], 1);
          End;
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
      New_NandP(Plant.SapWood[N], Plant.SapWood[C], OldPlant.SapWood);
      New_NandP(Plant.HeartWood[N], Plant.HeartWood[C], OldPlant.HeartWood);
      New_NandP(Plant.CoarseRoot[N], Plant.CoarseRoot[C], OldPlant.CoarseRoot);
      New_NandP(Plant.FineRoot[N], Plant.FineRoot[C], OldPlant.FineRoot);
      New_NandP(Plant.Branches[N], Plant.Branches[C], OldPlant.Branches);
      New_NandP(Plant.Bark[N], Plant.Bark[C], OldPlant.Bark);
      New_NandP(Plant.Leaves[N], Plant.Leaves[C], OldPlant.Leaves);
      New_NandP(Plant.Pollen[N], Plant.Pollen[C], OldPlant.Pollen);
      New_NandP(Plant.Fruit[N], Plant.Fruit[C], OldPlant.Fruit);
      New_NandP(Plant.Soluble[N], Plant.Soluble[C], OldPlant.Soluble);
      New_NandP(Plant.Reserves[N], Plant.Reserves[C], OldPlant.Reserves);
      For iLayer := 0 to DisplayLayers do
          Begin
          New_NandP(SoilOrganic.Struct[iLayer, N], soilorganic.Struct[iLayer, C], OldSoil.StructLitter[iLayer]);
          New_NandP(SoilOrganic.Metab[iLayer, N], soilorganic.Metab[iLayer, C], OldSoil.MetabLitter[iLayer]);
          If iLayer = 0 then
             New_NandP(SoilOrganic.FineWood[0, N], soilorganic.FineWood[0, C], OldSoil.FineWoodLitter[0]);
          New_NandP(SoilOrganic.CoarseWood[iLayer, N], soilorganic.CoarseWood[iLayer, C], OldSoil.CoarseWoodLitter[iLayer]);
          New_NandP(SoilOrganic.Active[iLayer, N], soilorganic.Active[iLayer, C], OldSoil.Active[iLayer]);
          New_NandP(SoilOrganic.Slow[iLayer, N], soilorganic.Slow[iLayer, C], OldSoil.Slow[iLayer]);
          New_NandP(SoilOrganic.Resistant[iLayer, N], soilorganic.Resistant[iLayer, C], OldSoil.Resistant[iLayer]);
          New_NandP(SoilOrganic.Inert[iLayer, N], soilorganic.Inert[iLayer, C], OldSoil.Inert[iLayer]);
          End;
      while (frmPools.tbsNitrogen.ComponentCount > 0) do
            frmPools.tbsNitrogen.Components[0].Free;
      FillEditNormal(edtSapWoodN, Plant.SapWood[N], Dummy);
      FillEditNormal(edtHeartWoodN, Plant.HeartWood[N], Dummy);
      FillEditNormal(edtCoarseRootN, Plant.CoarseRoot[N], Dummy);
      FillEditNormal(edtFineRootN, Plant.FineRoot[N], Dummy);
      FillEditNormal(edtBranchN, Plant.Branches[N], Dummy);
      FillEditNormal(edtBarkN, Plant.Bark[N], Dummy);
      FillEditNormal(edtFoliageN, Plant.Leaves[N], Dummy);
      FillEditNormal(edtPollenN, Plant.Pollen[N], Dummy);
      FillEditNormal(edtFruitN, Plant.Fruit[N], Dummy);
      FillEditNormal(edtSolubleN, Plant.Soluble[N], Dummy);
      FillEditNormal(edtReservesN, Plant.Reserves[N], Dummy);
      iCount := 100;
      For iLayer := 0 to DisplayLayers do
          Begin
          iCount := iCount + 22;
          If iLayer = 0 then
             LabelDisplay (frmPools.tbsNitrogen, 'Surface', 40, 10, iCount + 4)
          Else
             Begin
             Str(iLayer:3, LayerInfo);
             LabelDisplay (frmPools.tbsNitrogen,LayerInfo + ':', 25, 28, iCount + 4);
             End;
          FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 55, iCount, false, SoilOrganic.Struct[iLayer, N], 1, Dummy, SumStruct);
          FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 140, iCount, false, SoilOrganic.Metab[iLayer, N], 1, Dummy, SumMetab);
          If iLayer = 0 then
             FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 225, iCount, false, SoilOrganic.FineWood[0, N], 1, Dummy, SumFine);
          FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 310, iCount, false, SoilOrganic.CoarseWood[iLayer, N], 1, Dummy, SumCoarse);
          FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 395, iCount, false, SoilOrganic.Active[iLayer, N], 1, Dummy, SumActive);
          FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 480, iCount, false, SoilOrganic.Slow[iLayer, N], 1, Dummy, SumSlow);
          FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 565, iCount, false, SoilOrganic.Resistant[iLayer, N], 1, Dummy, SumResistant);
          FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 650, iCount, false, SoilOrganic.Inert[iLayer, N], 1, Dummy, SumInert);
          FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 735, iCount, false, SoilOrganic.Soluble[iLayer, N], 1, Dummy, SumMineral);
          End;
      iCount := iCount + 22;
      LabelDisplay (frmPools.tbsNitrogen, 'Total:', 40, 15, iCount + 4);
      FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 55, iCount, true, SumStruct, 1, Dummy, Dummy);
      FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 140, iCount, true, SumMetab, 1, Dummy, Dummy);
      FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 225, iCount, true, SumFine, 1, Dummy, Dummy);
      FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 310, iCount, true, SumCoarse, 1, Dummy, Dummy);
      FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 395, iCount, true, Sumactive, 1, Dummy, Dummy);
      FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 480, iCount, true, SumSlow, 1, Dummy, Dummy);
      FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 565, iCount, true, SumResistant, 1, Dummy, Dummy);
      FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 650, iCount, true, SumInert, 1, Dummy, Dummy);
      FillEditBoxDisplay (frmPools.tbsNitrogen, 50, 735, iCount, true, SumMineral, 1, Dummy, Dummy);
      End
  Else If pgcPages.ActivePage = tbsCarbon then;
      Begin
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[1]), SoilOrganic.Struct[0, N], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[2]), SoilOrganic.Metab[0, N], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[3]), SoilOrganic.FineWood[0, N], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[4]), SoilOrganic.CoarseWood[0, N], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[5]), SoilOrganic.Active[0, N], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[6]), SoilOrganic.Slow[0, N], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[7]), SoilOrganic.Resistant[0, N], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[8]), SoilOrganic.Inert[0, N], 1);
      frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[9]), SoilOrganic.Soluble[0, N], 1);
      For iLayer := 1 to DisplayLayers do
          Begin
          frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[iLayer * 9 + 2]), SoilOrganic.Struct[iLayer, N], 1);
          frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[iLayer * 9 + 3]), SoilOrganic.Metab[iLayer, N], 1);
          frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[iLayer * 9 + 4]), SoilOrganic.CoarseWood[iLayer, N], 1);
          frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[iLayer * 9 + 5]), SoilOrganic.Active[iLayer, N], 1);
          frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[iLayer * 9 + 6]), SoilOrganic.Slow[iLayer, N], 1);
          frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[iLayer * 9 + 7]), SoilOrganic.Resistant[iLayer, N], 1);
          frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[iLayer * 9 + 8]), SoilOrganic.Inert[iLayer, N], 1);
          frmMain.GetEdit(Sender, TEdit(frmPools.tbsNitrogen.Components[iLayer * 9 + 9]), SoilOrganic.Soluble[iLayer, N], 1);
          End;
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
      For iLayer := 0 to DisplayLayers do
          Begin
          MakeOld(SoilOrganic.Struct[iLayer, C], OldSoil.StructLitter[iLayer]);
          MakeOld(SoilOrganic.Metab[iLayer, C], OldSoil.MetabLitter[iLayer]);
          If iLayer = 0 then
             MakeOld(SoilOrganic.FineWood[0, C], OldSoil.FineWoodLitter[0]);
          MakeOld(SoilOrganic.CoarseWood[iLayer, C], OldSoil.CoarseWoodLitter[iLayer]);
          MakeOld(SoilOrganic.Active[iLayer, C], OldSoil.Active[iLayer]);
          MakeOld(SoilOrganic.Slow[iLayer, C], OldSoil.Slow[iLayer]);
          MakeOld(SoilOrganic.Resistant[iLayer, C], OldSoil.Resistant[iLayer]);
          MakeOld(SoilOrganic.Inert[iLayer, C], OldSoil.Inert[iLayer]);
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
      End;

end;

procedure TfrmPools.btnHelpClick(Sender: TObject);
begin
  // show the appropriate help page
  case pgcPages.ActivePageIndex of
    0: Application.HelpContext(idm_CarbonPools);
    1: Application.HelpContext(idm_NitrogenPools);
    2: Application.HelpContext(idm_Miscellaneous);
  end;
end;

procedure TfrmPools.rdRescaleSOMClick(Sender: TObject);
var ChangeSOM: Real48;

Procedure ScaleSOM (var Pool: SoilElements; ChangeSOM: Real48);
var iLayer: Integer;

Begin
For iLayer := 0 to DisplayLayers do
    Begin
    Pool[iLayer, C] := Pool[iLayer, C] * ChangeSOM;
    If ChkResetNPools.Checked then
       Pool[iLayer, N] := Pool[iLayer, N] * ChangeSOM;
    End;
End;

begin
frmMain.GetEdit(Sender, edtChangePerc, ChangeSOM, 100);
frmPools.pgcPagesChange(Sender);
ScaleSOM(SoilOrganic.Struct, ChangeSOM);
ScaleSOM(SoilOrganic.Metab, ChangeSOM);
ScaleSOM(SoilOrganic.CoarseWood, ChangeSOM);
ScaleSOM(SoilOrganic.FineWood, ChangeSOM);
ScaleSOM(SoilOrganic.Active, ChangeSOM);
ScaleSOM(SoilOrganic.Slow, ChangeSOM);
ScaleSOM(SoilOrganic.Resistant, ChangeSOM);
ScaleSOM(SoilOrganic.Inert, ChangeSOM);
rdRescaleSOM.Checked := false;
frmPools.FormShow(Sender);
end;

procedure TfrmPools.btnEditC13Click(Sender: TObject);
begin
frmC13Pools.pgcPages.ActivePage := frmC13Pools.tbsCarbon;
if (frmC13Pools.ShowModal = mrOK) then
   // Do nothing
End;
end.
