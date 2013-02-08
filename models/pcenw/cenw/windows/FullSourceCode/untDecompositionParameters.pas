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
  = Version   : 3.1                                              =
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
    rgLayerOption: TRadioGroup;
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
    Label16: TLabel;
    edtOMTransfer: TEdit;
    Label17: TLabel;
    edtOMIncorporate: TEdit;
    Label8: TLabel;
    edtLeafLignin: TEdit;
    Label9: TLabel;
    edtRootLignin: TEdit;
    Label10: TLabel;
    edtWoodLignin: TEdit;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure rgLayerOptionClick(Sender: TObject);
    procedure btnLitterInputClick(Sender: TObject);
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
frmMain.FillEdit(Sender, edtOMTransfer, Parameter.OMTransfer, 100);
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
frmMain.GetEdit(Sender, edtOMTransfer, Parameter.OMTransfer, 100);
frmMain.GetEdit(Sender, edtOMIncorporate, Parameter.OMIncorporate, 100);
Parameter.Decay8 := Divide(SOMDecay1, Parameter.DecayBranch_StructRatio);
Parameter.Decay9 := Divide(SOMDecay1, Parameter.DecayWood_StructRatio);
Parameter.Decay10 := SOMDecay7 * Parameter.Inert_Resistant_Ratio;
If (rgLayerOption.ItemIndex = 0) and not Control.AllOneLayer then // changed to simulate as single layer
   Begin
   Control.AllOneLayer := true;
   // Ensure that layer 1 gets all the litter from now on, but still remember the relative
   // contributions previously set for the other layers.
   If SoilOrganic.FineRootLitterIn[1] = 0 then
      SoilOrganic.FineRootLitterIn[1] := 1;
   If SoilOrganic.CoarseRootLitterIn[1] = 0 then
      SoilOrganic.CoarseRootLitterIn[1] := 1;
   For iLayer := SoilOrganic.nLayers downto 1 do
       Begin
       SoilOrganic.FineRootLitterIn[iLayer] := SoilOrganic.FineRootLitterIn[iLayer] / SoilOrganic.FineRootLitterIn[1];
       SoilOrganic.CoarseRootLitterIn[iLayer] := SoilOrganic.CoarseRootLitterIn[iLayer] / SoilOrganic.CoarseRootLitterIn[1];
       End;
   End
Else if (rgLayerOption.ItemIndex = 1) and Control.AllOneLayer then // changed to simulate as multiple layers
   Begin
   Control.AllOneLayer := false;
   // Ensure that the sume of all layers adds to 1 for litter input.
   SumFineRoot := 0;
   SumCoarseRoot := 0;
   For iLayer := 1 to SoilOrganic.nLayers do
       Begin
       SumFineRoot := SumFineRoot + SoilOrganic.FineRootLitterIn[iLayer];
       SumCoarseRoot := SumCoarseRoot + SoilOrganic.CoarseRootLitterIn[iLayer];
       End;
   For iLayer := 1 to SoilOrganic.nLayers do
       Begin
       If SumFineRoot > 0 then
          SoilOrganic.FineRootLitterIn[iLayer] := SoilOrganic.FineRootLitterIn[iLayer] / SumFineRoot
       Else
          SoilOrganic.FineRootLitterIn[iLayer] := 1 / SoilOrganic.nLayers;
       If SumCoarseRoot > 0 then
          SoilOrganic.CoarseRootLitterIn[iLayer] := SoilOrganic.CoarseRootLitterIn[iLayer] / SumCoarseRoot
       Else
          SoilOrganic.CoarseRootLitterIn[iLayer] := 1 / SoilOrganic.nLayers;
       End;
   if (MessageDlg('Do you want to give disk output separately by layer?', mtConfirmation,
      [mbYes, mbNo], 0) = mrYes) then
      Control.OutputByLayer := true;
   End;
Control.PlantHasChanged := TRUE;
Control.SiteHasChanged := TRUE;
Control.ProjectHasChanged := TRUE;
End;

procedure TfrmDecompositionParameters.rgLayerOptionClick(Sender: TObject);
begin
  // enable the appropriate edit boxes
  edtOMTransfer.Enabled := (rgLayerOption.ItemIndex = 1);
  Label16.Enabled := (rgLayerOption.ItemIndex = 1);
end;


procedure TfrmDecompositionParameters.btnLitterInputClick(Sender: TObject);
var iLayer: Integer;
    SumFineRoot, SumCoarseRoot: Real48;
begin
  If (rgLayerOption.ItemIndex = 0) and not Control.AllOneLayer then // changed to simulate as single layer
     Begin
     Control.AllOneLayer := true;
     // Ensure that layer 1 gets all the litter from now on, but still remember the relative
     // contributions previously set for the other layers.
     If SoilOrganic.FineRootLitterIn[1] = 0 then
        SoilOrganic.FineRootLitterIn[1] := 1;
     If SoilOrganic.CoarseRootLitterIn[1] = 0 then
        SoilOrganic.CoarseRootLitterIn[1] := 1;
     For iLayer := SoilOrganic.nLayers downto 1 do
         Begin
         SoilOrganic.FineRootLitterIn[iLayer] := SoilOrganic.FineRootLitterIn[iLayer] / SoilOrganic.FineRootLitterIn[1];
         SoilOrganic.CoarseRootLitterIn[iLayer] := SoilOrganic.CoarseRootLitterIn[iLayer] / SoilOrganic.CoarseRootLitterIn[1];
         End;
     End
  Else if (rgLayerOption.ItemIndex = 1) and Control.AllOneLayer then // changed to simulate as multiple layers
     Begin
     Control.AllOneLayer := false;
     // Ensure that the sume of all layers adds to 1 for litter input.
     SumFineRoot := 0;
     SumCoarseRoot := 0;
     For iLayer := 1 to SoilOrganic.nLayers do
         Begin
         SumFineRoot := SumFineRoot + SoilOrganic.FineRootLitterIn[iLayer];
         SumCoarseRoot := SumCoarseRoot + SoilOrganic.CoarseRootLitterIn[iLayer];
         End;
     For iLayer := 1 to SoilOrganic.nLayers do
         Begin
         If SumFineRoot > 0 then
            SoilOrganic.FineRootLitterIn[iLayer] := SoilOrganic.FineRootLitterIn[iLayer] / SumFineRoot
         Else
            SoilOrganic.FineRootLitterIn[iLayer] := 1 / SoilOrganic.nLayers;
         If SumCoarseRoot > 0 then
            SoilOrganic.CoarseRootLitterIn[iLayer] := SoilOrganic.CoarseRootLitterIn[iLayer] / SumCoarseRoot
         Else
            SoilOrganic.CoarseRootLitterIn[iLayer] := 1 / SoilOrganic.nLayers;
         End;
     End;
frmMain.mnuSoilLitterClick(Sender);
end;

end.
