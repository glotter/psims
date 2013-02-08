{ ================================================================
  = Project   : Cenw                                             =
  ================================================================
  = Module    : TfrmDecompositionParameters                      =
  =                                                              =
  =             Interface routines for parameters that control   =
  =             the organic-matter decomposition module.         =
  ================================================================
  = File      : untDecompositionParameters.PAS                   =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untDecompositionParameters;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, untMain,
  StdCtrls, ExtCtrls, Buttons;

type
  TfrmDecompositionParameters = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    grpLignin: TGroupBox;
    grpCoarseLitter: TGroupBox;
    btnLitterInput: TButton;
    Label0: TLabel;
    edtCriticalCN: TEdit;
    Label1: TLabel;
    edtRateAdjust: TEdit;
    Label2: TLabel;
    edtFertilityAdjust: TEdit;
    Label3: TLabel;
    edtInertOM: TEdit;
    Label4: TLabel;
    edtRelativeCN: TEdit;
    Label5: TLabel;
    edtFineSoil: TEdit;
    Label6: TLabel;
    edtImmobilise: TEdit;
    Label7: TLabel;
    edtImmobiliseInSlow: TEdit;
    Label11: TLabel;
    edtLigninInhibition: TEdit;
    Label15: TLabel;
    edtMinDecomp: TEdit;
    Label14: TLabel;
    edtRelWaterSens: TEdit;
    Label12: TLabel;
    edtBranchRatio: TEdit;
    Label13: TLabel;
    edtWoodRatio: TEdit;
    Label8: TLabel;
    edtLeafLignin: TEdit;
    Label9: TLabel;
    edtRootLignin: TEdit;
    Label10: TLabel;
    edtWoodLignin: TEdit;
    Label18: TLabel;
    Label19: TLabel;
    edtCriticalCPmax: TEdit;
    edtRelativeCP: TEdit;
    edtOMIncorporate: TEdit;
    Label17: TLabel;
    rgLayerOption: TRadioGroup;
    lblWeathering: TLabel;
    edtWeathering: TEdit;
    Label16: TLabel;
    edtOccludedP_rate: TEdit;
    edtLabile_to_Secondary: TEdit;
    edtSecondary_to_Labile: TEdit;
    Label20: TLabel;
    Label21: TLabel;
    edtPhosphatase: TEdit;
    lblPhosphatase: TLabel;
    edtCriticalCPmin: TEdit;
    lblCPmax: TLabel;
    lblCPmin: TLabel;
    Procedure FormShow(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
    Procedure btnLitterInputClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDecompositionParameters: TfrmDecompositionParameters;

implementation

{$R *.DFM}

uses
  untDeclarations, untMiscellaneous, untDivideValidation,
  untGenericListDialogue;

Procedure TfrmDecompositionParameters.FormShow(Sender: TObject);
Begin
frmMain.FillEdit(Sender, edtCriticalCN, Parameter.CriticalCN, 1);
frmMain.FillEdit(Sender, edtRateAdjust, Parameter.RateAdjust, 1);
frmMain.FillEdit(Sender, edtFertilityAdjust, Parameter.FertilityAdjust, 1);
frmMain.FillEdit(Sender, edtRelativeCN, Parameter.RelativeCN, 1);
frmMain.FillEdit(Sender, edtFineSoil, Parameter.FineSoil, 100);
frmMain.FillEdit(Sender, edtImmobilise, Parameter.Immobilise, 1000);
frmMain.FillEdit(Sender, edtImmobiliseInSlow, Parameter.ImmobiliseInSlow, 100);
frmMain.FillEdit(Sender, edtLeafLignin, Parameter.LeafLignin, 100);
frmMain.FillEdit(Sender, edtRootLignin, Parameter.RootLignin, 100);
frmMain.FillEdit(Sender, edtWoodLignin, Parameter.WoodLignin, 100);
frmMain.FillEdit(Sender, edtLigninInhibition, Parameter.LigninInhibition, 1);
frmMain.FillEdit(Sender, edtBranchRatio, Parameter.DecayBranch_StructRatio, 1);
frmMain.FillEdit(Sender, edtWoodRatio, Parameter.DecayWood_StructRatio, 1);
frmMain.FillEdit(Sender, edtInertOM, Parameter.Inert_Resistant_Ratio, 1);
frmMain.FillEdit(Sender, edtRelWaterSens, Parameter.RelWaterSens, 1);
frmMain.FillEdit(Sender, edtMinDecomp, Parameter.MinDecomp, 1);
frmMain.FillEdit(Sender, edtOMIncorporate, Parameter.OMIncorporate, 100);
frmMain.FillEdit(Sender, edtCriticalCPmin, Parameter.CriticalCPmin, 1);
frmMain.FillEdit(Sender, edtCriticalCPmax, Parameter.CriticalCPmax, 1);
frmMain.FillEdit(Sender, edtRelativeCP, Parameter.RelativeCP, 1);
frmMain.FillEdit(Sender, edtWeathering, Parameter.Weathering, 3650000);
frmMain.FillEdit(Sender, edtOccludedP_rate, Parameter.OccludedP_rate, 3650000);
frmMain.FillEdit(Sender, edtLabile_to_Secondary, Parameter.Labile_to_Sec, 100);
frmMain.FillEdit(Sender, edtSecondary_to_Labile, Parameter.Sec_to_Labile, 100);
frmMain.FillEdit(Sender, edtPhosphatase, Parameter.Phosphatase, 36500);
if Control.IncludeP then
   Begin
   frmDecompositionParameters.Width := 950;
   edtCriticalCPmin.Visible := true;
   edtCriticalCPmax.Visible := true;
   edtRelativeCP.Visible := true;
   edtWeathering.Visible := true;
   edtOccludedP_rate.Visible := true;
   edtLabile_to_Secondary.Visible := true;
   edtSecondary_to_Labile.Visible := true;
   edtPhosphatase.Visible := true;
   lblCPmax.Visible := true;
   lblCPmin.Visible := true;
   End
Else
   Begin
   frmDecompositionParameters.Width := 736;
   edtCriticalCPmin.Visible := false;
   edtCriticalCPmax.Visible := false;
   edtRelativeCP.Visible := false;
   edtWeathering.Visible := false;
   edtOccludedP_rate.Visible := false;
   edtLabile_to_Secondary.Visible := false;
   edtSecondary_to_Labile.Visible := false;
   edtPhosphatase.Visible := false;
   lblCPmax.Visible := false;
   lblCPmin.Visible := false;
   End;

If Control.AllOneLayer then
   rgLayerOption.ItemIndex := 0
Else
   rgLayerOption.ItemIndex := 1;
End;

Procedure TfrmDecompositionParameters.btnOKClick(Sender: TObject);

var SumFineRoot, SumCoarseRoot: real48;
    iLayer: Integer;

Begin
frmMain.GetEdit(Sender, edtCriticalCN, Parameter.CriticalCN, 1);
frmMain.GetEdit(Sender, edtRateAdjust, Parameter.RateAdjust, 1);
frmMain.GetEdit(Sender, edtFertilityAdjust, Parameter.FertilityAdjust, 1);
frmMain.GetEdit(Sender, edtRelativeCN, Parameter.RelativeCN, 1);
frmMain.GetEdit(Sender, edtFineSoil, Parameter.FineSoil, 100);
frmMain.GetEdit(Sender, edtImmobilise, Parameter.Immobilise, 1000);
frmMain.GetEdit(Sender, edtImmobiliseInSlow, Parameter.ImmobiliseInSlow, 100);
frmMain.GetEdit(Sender, edtLeafLignin, Parameter.LeafLignin, 100);
frmMain.GetEdit(Sender, edtRootLignin, Parameter.RootLignin, 100);
frmMain.GetEdit(Sender, edtWoodLignin, Parameter.WoodLignin, 100);
frmMain.GetEdit(Sender, edtLigninInhibition, Parameter.LigninInhibition, 1);
frmMain.GetEdit(Sender, edtBranchRatio, Parameter.DecayBranch_StructRatio, 1);
frmMain.GetEdit(Sender, edtWoodRatio, Parameter.DecayWood_StructRatio, 1);
frmMain.GetEdit(Sender, edtInertOM, Parameter.Inert_Resistant_Ratio, 1);
frmMain.GetEdit(Sender, edtRelWaterSens, Parameter.RelWaterSens, 1);
frmMain.GetEdit(Sender, edtMinDecomp, Parameter.MinDecomp, 1);
frmMain.GetEdit(Sender, edtOMIncorporate, Parameter.OMIncorporate, 100);
frmMain.GetEdit(Sender, edtCriticalCPmin, Parameter.CriticalCPmin, 1);
frmMain.GetEdit(Sender, edtCriticalCPmax, Parameter.CriticalCPmax, 1);
frmMain.GetEdit(Sender, edtRelativeCP, Parameter.RelativeCP, 1);
frmMain.GetEdit(Sender, edtWeathering, Parameter.Weathering, 3650000);
frmMain.GetEdit(Sender, edtOccludedP_rate, Parameter.OccludedP_rate, 3650000);
frmMain.GetEdit(Sender, edtLabile_to_Secondary, Parameter.Labile_to_Sec, 100);
frmMain.GetEdit(Sender, edtSecondary_to_Labile, Parameter.Sec_to_Labile, 100);
frmMain.GetEdit(Sender, edtPhosphatase, Parameter.Phosphatase, 36500);
Parameter.Decay8 := Divide(SOMDecay1, Parameter.DecayBranch_StructRatio);
Parameter.Decay9 := Divide(SOMDecay1, Parameter.DecayWood_StructRatio);
Parameter.Decay10 := SOMDecay7 * Parameter.Inert_Resistant_Ratio;
If (rgLayerOption.ItemIndex = 0) and not Control.AllOneLayer then // changed to simulate as single layer
   Begin
   Control.AllOneLayer := true;
   // Ensure that layer 1 gets all the litter from now on, but still remember the relative
   // contributions previously set for the other layers.
   If Soil.FineRootLitterIn[1] = 0 then
      Soil.FineRootLitterIn[1] := 1;
   If Soil.CoarseRootLitterIn[1] = 0 then
      Soil.CoarseRootLitterIn[1] := 1;
   For iLayer := Soil.nLayers downto 1 do
       Begin
       Soil.FineRootLitterIn[iLayer] := Soil.FineRootLitterIn[iLayer] / Soil.FineRootLitterIn[1];
       Soil.CoarseRootLitterIn[iLayer] := Soil.CoarseRootLitterIn[iLayer] / Soil.CoarseRootLitterIn[1];
       End;
   End
Else if (rgLayerOption.ItemIndex = 1) and Control.AllOneLayer then // changed to simulate as multiple layers
   Begin
   Control.AllOneLayer := false;
   // Ensure that the sume of all layers adds to 1 for litter input.
   SumFineRoot := 0;
   SumCoarseRoot := 0;
   For iLayer := 1 to Soil.nLayers do
       Begin
       SumFineRoot := SumFineRoot + Soil.FineRootLitterIn[iLayer];
       SumCoarseRoot := SumCoarseRoot + Soil.CoarseRootLitterIn[iLayer];
       End;
   For iLayer := 1 to Soil.nLayers do
       Begin
       If SumFineRoot > 0 then
          Soil.FineRootLitterIn[iLayer] := Soil.FineRootLitterIn[iLayer] / SumFineRoot
       Else
          Soil.FineRootLitterIn[iLayer] := 1 / Soil.nLayers;
       If SumCoarseRoot > 0 then
          Soil.CoarseRootLitterIn[iLayer] := Soil.CoarseRootLitterIn[iLayer] / SumCoarseRoot
       Else
          Soil.CoarseRootLitterIn[iLayer] := 1 / Soil.nLayers;
       End;
   if (MessageDlg('Do you want to give disk output separately by layer?', mtConfirmation,
      [mbYes, mbNo], 0) = mrYes) then
      Control.OutputByLayer := true;
   End;
Control.PlantHasChanged := TRUE;
Control.SiteHasChanged := TRUE;
Control.ProjectHasChanged := TRUE;
End;

Procedure TfrmDecompositionParameters.btnLitterInputClick(Sender: TObject);
var iLayer: Integer;
    SumFineRoot, SumCoarseRoot: Real48;
Begin
  If (rgLayerOption.ItemIndex = 0) and not Control.AllOneLayer then // changed to simulate as single layer
     Begin
     Control.AllOneLayer := true;
     // Ensure that layer 1 gets all the litter from now on, but still remember the relative
     // contributions previously set for the other layers.
     If Soil.FineRootLitterIn[1] = 0 then
        Soil.FineRootLitterIn[1] := 1;
     If Soil.CoarseRootLitterIn[1] = 0 then
        Soil.CoarseRootLitterIn[1] := 1;
     For iLayer := Soil.nLayers downto 1 do
         Begin
         Soil.FineRootLitterIn[iLayer] := Soil.FineRootLitterIn[iLayer] / Soil.FineRootLitterIn[1];
         Soil.CoarseRootLitterIn[iLayer] := Soil.CoarseRootLitterIn[iLayer] / Soil.CoarseRootLitterIn[1];
         End;
     End
  Else if (rgLayerOption.ItemIndex = 1) and Control.AllOneLayer then // changed to simulate as multiple layers
     Begin
     Control.AllOneLayer := false;
     // Ensure that the sume of all layers adds to 1 for litter input.
     SumFineRoot := 0;
     SumCoarseRoot := 0;
     For iLayer := 1 to Soil.nLayers do
         Begin
         SumFineRoot := SumFineRoot + Soil.FineRootLitterIn[iLayer];
         SumCoarseRoot := SumCoarseRoot + Soil.CoarseRootLitterIn[iLayer];
         End;
     For iLayer := 1 to Soil.nLayers do
         Begin
         If SumFineRoot > 0 then
            Soil.FineRootLitterIn[iLayer] := Soil.FineRootLitterIn[iLayer] / SumFineRoot
         Else
            Soil.FineRootLitterIn[iLayer] := 1 / Soil.nLayers;
         If SumCoarseRoot > 0 then
            Soil.CoarseRootLitterIn[iLayer] := Soil.CoarseRootLitterIn[iLayer] / SumCoarseRoot
         Else
            Soil.CoarseRootLitterIn[iLayer] := 1 / Soil.nLayers;
         End;
     End;
frmMain.mnuSoilLitterClick(Sender);
end;

end.
