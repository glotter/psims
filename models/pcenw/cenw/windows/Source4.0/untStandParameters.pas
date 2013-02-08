{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmStandParameters                              =
  =                                                              =
  =             Edit window to change stand parameters           =
  ================================================================
  = File      : untStandParameters.PAS                           =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untStandParameters;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TfrmStandParameters = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    grpTypeOfEvap: TGroupBox;
    rgCalc: TRadioGroup;
    rgMortalityType: TRadioGroup;
    grpFoliageSenesc: TGroupBox;
    grpAnnualSenesc: TGroupBox;
    grpUseCarbo: TGroupBox;
    chkAcclimation: TCheckBox;
    chkVariableNFix: TCheckBox;
    edtDeathRatio: TEdit;
    LblDeathRatio: TLabel;
    edtStemDeath: TEdit;
    lblStemDeath: TLabel;
    edt32PowerLaw: TEdit;
    lbl32PowerLaw: TLabel;
    edtLeafSenesc: TEdit;
    Label3: TLabel;
    edtBranchSenesc: TEdit;
    Label4: TLabel;
    edtRootSenesc: TEdit;
    Label5: TLabel;
    edtBarkSenesc: TEdit;
    Label6: TLabel;
    edtFruitSenesc: TEdit;
    Label7: TLabel;
    edtPollenSenesc: TEdit;
    Label8: TLabel;
    edtSapwoodYears: TEdit;
    Label9: TLabel;
    edtSenescLowLight: TEdit;
    Label10: TLabel;
    edtMaxDailySenesc: TEdit;
    Label11: TLabel;
    edtInternalNRatio: TEdit;
    Label12: TLabel;
    edtSenescLeafRatio: TEdit;
    Label13: TLabel;
    edtMicroFract: TEdit;
    Label15: TLabel;
    edtDrySenesc: TEdit;
    Label16: TLabel;
    edtBiolFix: TEdit;
    Label17: TLabel;
    edtStressLimit: TEdit;
    Label18: TLabel;
    edtRespnRatio: TEdit;
    lblRespnRatio: TLabel;
    edtRespFromN: TEdit;
    lblRespFromN: TLabel;
    edtbeta: TEdit;
    lblbeta: TLabel;
    edtRespnOpt: TEdit;
    lblRespnOpt: TLabel;
    edtGrowthRespn: TEdit;
    lblGrowthRespn: TLabel;
    edtRespnAdjust: TEdit;
    lblRespnAdjust: TLabel;
    edtKmGrowthC: TEdit;
    Label25: TLabel;
    edtKmGrowthN: TEdit;
    Label26: TLabel;
    Label1: TLabel;
    edtWaterLogLimit: TEdit;
    Label2: TLabel;
    edtWaterLogSensitivity: TEdit;
    Procedure FormShow(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
    Procedure rgMortalityTypeClick(Sender: TObject);
    Procedure rgCalcClick(Sender: TObject);
    Procedure chkAcclimationClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmStandParameters: TfrmStandParameters;

implementation

{$R *.DFM}

uses
  untDeclarations, untMiscellaneous, untMain;

Procedure TfrmStandParameters.FormShow(Sender: TObject);
var BasicsSelected: Boolean;

Begin
frmMain.FillEdit(Sender, edtDeathRatio, Parameter.DeathRatio, 1);
frmMain.FillEdit(Sender, edtStemDeath, Parameter.StemDeath, 365);
if Parameter.Three_Two_Power_Law <= 0 then
   Parameter.Three_Two_Power_Law := 0.1;
frmMain.FillEdit(Sender, edt32PowerLaw, ln(Parameter.Three_Two_Power_Law), 1);
frmMain.FillEdit(Sender, edtLeafSenesc, Parameter.LeafSenesc, 365);
frmMain.FillEdit(Sender, edtBranchSenesc, Parameter.BranchSenesc, 365);
frmMain.FillEdit(Sender, edtRootSenesc, Parameter.RootSenesc, 365);
frmMain.FillEdit(Sender, edtBarkSenesc, Parameter.BarkSenesc, 365);
frmMain.FillEdit(Sender, edtFruitSenesc, Parameter.FruitSenesc, 365);
frmMain.FillEdit(Sender, edtPollenSenesc, Parameter.PollenSenesc, 365);
frmMain.FillEdit(Sender, edtSapwoodYears, Parameter.SapWoodYears, 0);
frmMain.FillEdit(Sender, edtSenescLowLight, Parameter.SenescLowLight, 1);
frmMain.FillEdit(Sender, edtMaxDailySenesc, Parameter.MaxSenescLowLight, 1);
frmMain.FillEdit(Sender, edtSenescLeafRatio, Parameter.SenescLeafRatio, 1);
frmMain.FillEdit(Sender, edtKmGrowthC, Parameter.KmGrowth[C], 100);
frmMain.FillEdit(Sender, edtKmGrowthN, Parameter.KmGrowth[N], 100);
frmMain.FillEdit(Sender, edtMicroFract, Parameter.MicroFract, 1000);
frmMain.FillEdit(Sender, edtRespFromN, Parameter.RespFromN, 1);
frmMain.FillEdit(Sender, edtbeta, Parameter.Respnbeta, 1000);
frmMain.FillEdit(Sender, edtRespnOpt, Parameter.RespnOpt, 1);
frmMain.FillEdit(Sender, edtRespnAdjust, Parameter.RespnAdjust, 1);
frmMain.FillEdit(Sender, edtRespnRatio, Parameter.RespnRatio, 1);
frmMain.FillEdit(Sender, edtGrowthRespn, Parameter.GrowthRespn, 1);
frmMain.FillEdit(Sender, edtBiolFix, Parameter.BiolFix, 1000 * Control.NConversion / Control.CConversion);
frmMain.FillEdit(Sender, edtInternalNRatio, Parameter.InternalNRatio, 1);
frmMain.FillEdit(Sender, edtWaterLogLimit, Parameter.WaterLogLimit, 1);
frmMain.FillEdit(Sender, edtWaterLogSensitivity, Parameter.WaterLogSensitivity, 1);
frmMain.FillEdit(Sender, edtStressLimit, Parameter.StressLimit, 1);
frmMain.FillEdit(Sender, edtDrySenesc, Parameter.DrySenesc, 100);
chkVariableNFix.Checked := Parameter.VariableNFixation;
If Parameter.RespnTAcclimation then
   chkAcclimation.Checked := true
Else
   chkAcclimation.Checked := false;
If Parameter.RespnType = Ratio then
   Begin
   rgCalc.ItemIndex := 1;
   BasicsSelected := false;
   End
Else {If RespirationType = Basics then}
   Begin
   rgCalc.ItemIndex := 0;
   BasicsSelected := true;
   End;
edtRespFromN.Enabled := BasicsSelected;
lblRespFromN.Enabled := BasicsSelected;
chkAcclimation.enabled := BasicsSelected;
edtRespnAdjust.Enabled := BasicsSelected and chkAcclimation.Checked;
lblRespnAdjust.Enabled := BasicsSelected and chkAcclimation.Checked;
edtGrowthRespn.Enabled := BasicsSelected;
lblGrowthRespn.Enabled := BasicsSelected;
edtbeta.Enabled := BasicsSelected;
lblbeta.Enabled := BasicsSelected;
edtRespnOpt.Enabled := BasicsSelected;
lblRespnOpt.Enabled := BasicsSelected;
edtRespnRatio.Enabled :=  not BasicsSelected;
lblRespnRatio.Enabled :=  not BasicsSelected;
If Parameter.MortalityType = Fraction then
   rgMortalityType.ItemIndex := 0
Else if Parameter.MortalityType = Density then
   rgMortalityType.ItemIndex := 1
Else {if Parameter.MortalityType = 'Both'}
   rgMortalityType.ItemIndex := 2;
End;

Procedure TfrmStandParameters.btnOKClick(Sender: TObject);
Begin
frmMain.GetEdit(Sender, edtDeathRatio, Parameter.DeathRatio, 1);
frmMain.GetEdit(Sender, edtStemDeath, Parameter.StemDeath, 365);
frmMain.GetEdit(Sender, edt32PowerLaw, Parameter.Three_Two_Power_Law, 1);
Parameter.Three_Two_Power_Law := exp(Parameter.Three_Two_Power_Law);
frmMain.GetEdit(Sender, edtLeafSenesc, Parameter.LeafSenesc, 365);
frmMain.GetEdit(Sender, edtBranchSenesc, Parameter.BranchSenesc, 365);
frmMain.GetEdit(Sender, edtRootSenesc, Parameter.RootSenesc, 365);
frmMain.GetEdit(Sender, edtBarkSenesc, Parameter.BarkSenesc, 365);
frmMain.GetEdit(Sender, edtFruitSenesc, Parameter.FruitSenesc, 365);
frmMain.GetEdit(Sender, edtPollenSenesc, Parameter.PollenSenesc, 365);
frmMain.GetInteger(Sender, edtSapwoodYears, Parameter.SapWoodYears);
frmMain.GetEdit(Sender, edtSenescLowLight, Parameter.SenescLowLight, 1);
frmMain.GetEdit(Sender, edtMaxDailySenesc, Parameter.MaxSenescLowLight, 1);
frmMain.GetEdit(Sender, edtSenescLeafRatio, Parameter.SenescLeafRatio, 1);
frmMain.GetEdit(Sender, edtKmGrowthC, Parameter.KmGrowth[C], 100);
frmMain.GetEdit(Sender, edtKmGrowthN, Parameter.KmGrowth[N], 100);
frmMain.GetEdit(Sender, edtMicroFract, Parameter.MicroFract, 1000);
frmMain.GetEdit(Sender, edtRespFromN, Parameter.RespFromN, 1);
frmMain.GetEdit(Sender, edtbeta, Parameter.Respnbeta, 1000);
frmMain.GetEdit(Sender, edtRespnOpt, Parameter.RespnOpt, 1);
Parameter.Respnalpha := ln(1 / exp(Parameter.Respnbeta * 25 * (2 * Parameter.RespnOpt - 25)));
frmMain.GetEdit(Sender, edtRespnAdjust, Parameter.RespnAdjust, 1);
frmMain.GetEdit(Sender, edtGrowthRespn, Parameter.GrowthRespn, 1);
frmMain.GetEdit(Sender, edtRespnRatio, Parameter.RespnRatio, 1);
frmMain.GetEdit(Sender, edtBiolFix, Parameter.BiolFix, 1000 * Control.NConversion / Control.CConversion);
frmMain.GetEdit(Sender, edtInternalNRatio, Parameter.InternalNRatio, 1);
frmMain.GetEdit(Sender, edtWaterLogLimit, Parameter.WaterLogLimit, 1);
frmMain.GetEdit(Sender, edtWaterLogSensitivity, Parameter.WaterLogSensitivity, 1);
frmMain.GetEdit(Sender, edtStressLimit, Parameter.StressLimit, 1);
frmMain.GetEdit(Sender, edtDrySenesc, Parameter.DrySenesc, 100);
Parameter.VariableNFixation := chkVariableNFix.Checked;
If (rgCalc.ItemIndex = 0) then
   Parameter.RespnType := Basics
Else
   Parameter.RespnType := Ratio;
If (rgMortalityType.ItemIndex = 0) then
   Parameter.MortalityType := Fraction
Else if (rgMortalityType.ItemIndex = 1) then
   Parameter.MortalityType := Density
Else
   Parameter.MortalityType := Both;
If chkAcclimation.Checked then
   Parameter.RespnTAcclimation := true
Else
   Parameter.RespnTAcclimation := false;
Control.ProjectHasChanged := TRUE;
Control.PlantHasChanged := TRUE;
Control.SiteHasChanged := TRUE;
End;

Procedure TfrmStandParameters.rgMortalityTypeClick(Sender: TObject);
begin
  // enable the correct controls based on mortality type
  edtStemDeath.Enabled := (rgMortalityType.ItemIndex = 0) or (rgMortalityType.ItemIndex = 2);
  lblStemDeath.Enabled := (rgMortalityType.ItemIndex = 0) or (rgMortalityType.ItemIndex = 2);
  edt32PowerLaw.Enabled := (rgMortalityType.ItemIndex = 1) or (rgMortalityType.ItemIndex = 2);
  lbl32PowerLaw.Enabled := (rgMortalityType.ItemIndex = 1) or (rgMortalityType.ItemIndex = 2);
end;

Procedure TfrmStandParameters.rgCalcClick(Sender: TObject);
var BasicsSelected: Boolean;

  begin
  // enable the correct controls based on respiration calculation type
  If rgCalc.ItemIndex = 0 then
     BasicsSelected := true
  Else
     BasicsSelected := false;
  edtRespFromN.Enabled := BasicsSelected;
  lblRespFromN.Enabled := BasicsSelected;
  chkAcclimation.enabled := BasicsSelected;
  edtRespnAdjust.Enabled := BasicsSelected and chkAcclimation.Checked;
  lblRespnAdjust.Enabled := BasicsSelected and chkAcclimation.Checked;
  edtGrowthRespn.Enabled := BasicsSelected;
  lblGrowthRespn.Enabled := BasicsSelected;
  edtbeta.Enabled := BasicsSelected;
  lblbeta.Enabled := BasicsSelected;
  edtRespnOpt.Enabled := BasicsSelected;
  lblRespnOpt.Enabled := BasicsSelected;
  edtRespnRatio.Enabled :=  not BasicsSelected;
  lblRespnRatio.Enabled :=  not BasicsSelected;
  end;

Procedure TfrmStandParameters.chkAcclimationClick(Sender: TObject);
  begin
  edtRespnAdjust.Enabled := chkAcclimation.Checked;
  lblRespnAdjust.Enabled := chkAcclimation.Checked;
  end;

end.
