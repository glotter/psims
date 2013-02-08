{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmPhotosyntheticParameters                     =
  =                                                              =
  =             Edit window to change photosynthetic parameters  =
  ================================================================
  = File      : untPhotoSyntheticParameters.PAS                  =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untPhotoSyntheticParameters;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TfrmPhotoSyntheticParameters = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    grpLeafPhotosyntheticParameters: TGroupBox;
    Stomatal_Conductance_Parameters: TGroupBox;
    Temperature_Damage: TGroupBox;
    grpTemperature_Response: TGroupBox;
    Age_Decline: TGroupBox;
    Foliage_clumping: TGroupBox;
    edtFoliageClumping: TCheckBox;
    gpConstantLeafN: TGroupBox;
    edtConstantLeafN: TCheckBox;
    rgIncludeAge: TRadioGroup;
    grpIsotopes: TGroupBox;
    rgCalcNewDelta: TRadioGroup;
    grpPhsPathway: TGroupBox;
    rdC3_C4: TRadioGroup;
    grpC4parameters: TGroupBox;
    edtSLA: TEdit;
    lblLine1: TLabel;
    edtAlbedo: TEdit;
    Label1: TLabel;
    edtTransmit: TEdit;
    Label2: TLabel;
    edtkexmax: TEdit;
    Label3: TLabel;
    edtKlowRange: TEdit;
    lblCanopyWidth: TLabel;
    edtN0: TEdit;
    Label5: TLabel;
    edtNcrit: TEdit;
    Label6: TLabel;
    edtNmax: TEdit;
    Label7: TLabel;
    edtTheta: TEdit;
    Label8: TLabel;
    edtAmax: TEdit;
    Label9: TLabel;
    edtBallBerry1: TEdit;
    Label10: TLabel;
    edtBallBerry2: TEdit;
    Label11: TLabel;
    edtTFrost: TEdit;
    Label12: TLabel;
    edtTScorch: TEdit;
    Label13: TLabel;
    edtTSensitivity: TEdit;
    Label14: TLabel;
    edtTRepair: TEdit;
    Label15: TLabel;
    edtMatureAge: TEdit;
    lblMatureAge: TLabel;
    edtMatureSize: TEdit;
    lblMatureSize: TLabel;
    edtAgePower: TEdit;
    lblAgePower: TLabel;
    edtSizePower: TEdit;
    edtConstantLeafNValue: TEdit;
    lblConstantLeafNValue: TLabel;
    edtTmin: TEdit;
    Label4: TLabel;
    edtTmax: TEdit;
    Label17: TLabel;
    edtTopt1: TEdit;
    Label18: TLabel;
    edtTopt2: TEdit;
    Label19: TLabel;
    lblNewDelta: TLabel;
    edtNewDelta: TEdit;
    lblExtraDelta: TLabel;
    edtExtraDelta: TEdit;
    edtRelkPEP: TEdit;
    Label20: TLabel;
    edtbeta: TEdit;
    Label22: TLabel;
    edtphi: TEdit;
    lblphi: TLabel;
    Label23: TLabel;
    edtNMVOC: TEdit;
    Label25: TLabel;
    edtAlpha: TEdit;
    edtP0: TEdit;
    lblP0: TLabel;
    edtPcrit: TEdit;
    edtPmax: TEdit;
    lblPcrit: TLabel;
    lblPmax: TLabel;
    edtCanopyWidthInter: TEdit;
    edtCanopyWidthSlope: TEdit;
    lblKlowRange: TLabel;
    lblIntercept: TLabel;
    lblSlope: TLabel;
    Label16: TLabel;
    edtTMaxRepairTime: TEdit;
    GroupBox1: TGroupBox;
    Label21: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    edtRainDamageLimit: TEdit;
    edtRainDamageSensitivity: TEdit;
    edtRainDamageRepair: TEdit;
    Procedure FormShow(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
    Procedure rgIncludeAgeClick(Sender: TObject);
    Procedure edtConstantLeafNClick(Sender: TObject);
    Procedure edtFoliageClumpingClick(Sender: TObject);
    Procedure rgCalcNewDeltaClick(Sender: TObject);
    Procedure rdC3_C4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPhotoSyntheticParameters: TfrmPhotoSyntheticParameters;

implementation

{$R *.DFM}

uses
  untDeclarations, untMiscellaneous, untMain;

Procedure TfrmPhotoSyntheticParameters.FormShow(Sender: TObject);
Begin
frmMain.FillEdit(Sender, edtSLA, Parameter.SLA, 1 / Control.CConversion);
frmMain.FillEdit(Sender, edtAlbedo, Parameter.Albedo, 100);
frmMain.FillEdit(Sender, edtTransmit, Parameter.Transmit, 100);
frmMain.FillEdit(Sender, edtNMVOC, Parameter.NMVOC, 100);
frmMain.FillEdit(Sender, edtkexmax, Parameter.kexmax, 1);
frmMain.FillEdit(Sender, edtKlowRange, Parameter.KlowRange, 1);
frmMain.FillEdit(Sender, edtCanopyWidthInter, Parameter.CanopyWidthInter, 1);
frmMain.FillEdit(Sender, edtCanopyWidthSlope, Parameter.CanopyWidthSlope, 1);
frmMain.FillEdit(Sender, edtN0, Parameter.N0, 1000 * Control.NConversion / Control.CConversion);
frmMain.FillEdit(Sender, edtNcrit, Parameter.Ncrit, 1000 * Control.NConversion / Control.CConversion);
frmMain.FillEdit(Sender, edtNmax, Parameter.Nmax, 1000 * Control.NConversion / Control.CConversion);
frmMain.FillEdit(Sender, edtAmax, Parameter.Amax, 1);
frmMain.FillEdit(Sender, edtAlpha, Parameter.Alpha, 1);
frmMain.FillEdit(Sender, edtTheta, Parameter.Theta, 1);
frmMain.FillEdit(Sender, edtBallBerry1, Parameter.BallBerry1, 1);
frmMain.FillEdit(Sender, edtBallBerry2, Parameter.BallBerry2, 1);
frmMain.FillEdit(Sender, edtTFrost, Parameter.TFrost, 1);
frmMain.FillEdit(Sender, edtTScorch, Parameter.TScorch, 1);
frmMain.FillEdit(Sender, edtTSensitivity, Parameter.TSensitivity, 1);
frmMain.FillEdit(Sender, edtTRepair, Parameter.TRepair, 1);
frmMain.FillEdit(Sender, edtRainDamageLimit, Parameter.RainDamageLimit, 1);
frmMain.FillEdit(Sender, edtRainDamageSensitivity, Parameter.RainDamageSensitivity, 1);
frmMain.FillEdit(Sender, edtRainDamageRepair, Parameter.RainDamageRepair, 1);
frmMain.FillEdit(Sender, edtTMaxRepairTime, Parameter.TMaxRepairTime, 1);
frmMain.FillEdit(Sender, edtTMin, Parameter.TMinLim, 1);
frmMain.FillEdit(Sender, edtTMax, Parameter.TMaxLim, 1);
frmMain.FillEdit(Sender, edtTOpt1, Parameter.TOpt1, 1);
frmMain.FillEdit(Sender, edtTOpt2, Parameter.TOpt2, 1);
frmMain.FillEdit(Sender, edtConstantLeafNValue, Parameter.ConstantLeafNValue, 1000 * Control.NConversion / Control.CConversion);
frmMain.FillEdit(Sender, edtMatureAge, Parameter.MatureAge, 0);
frmMain.FillEdit(Sender, edtAgePower, Parameter.AgePower, 1);
frmMain.FillEdit(Sender, edtMatureSize, Parameter.MatureSize, 0);
frmMain.FillEdit(Sender, edtSizePower, Parameter.SizePower, 1);
frmMain.FillEdit(Sender, edtrelkPEP, Parameter.relkPEP, 1);
frmMain.FillEdit(Sender, edtbeta, Parameter.beta, 1);
frmMain.FillEdit(Sender, edtphi, Parameter.phi, 1);
If Control.IncludeP then
   Begin
   frmMain.FillEdit(Sender, edtP0, Parameter.P0, 1000 / Control.CConversion);
   frmMain.FillEdit(Sender, edtPcrit, Parameter.Pcrit, 1000 / Control.CConversion);
   frmMain.FillEdit(Sender, edtPmax, Parameter.Pmax, 1000 / Control.CConversion);
   edtP0.Visible := true;
   edtPcrit.Visible := true;
   edtPmax.Visible := true;
   lblP0.Visible := true;
   lblPcrit.Visible := true;
   lblPmax.Visible := true;
   End
Else
   Begin
   edtP0.Visible := false;
   edtPcrit.Visible := false;
   edtPmax.Visible := false;
   lblP0.Visible := false;
   lblPcrit.Visible := false;
   lblPmax.Visible := false;
   End;
If Parameter.Phs = C3 then
   Begin
   grpC4parameters.Enabled := false;
   grpC4parameters.Visible := false;
   rdC3_C4.ItemIndex := 0
   End
Else
   Begin
   grpC4parameters.Enabled := true;
   grpC4parameters.Visible := true;
   rdC3_C4.ItemIndex := 1;
   If Control.IncludeIsotopes then
      Begin
      lblphi.Visible := true;
      edtphi.Visible := true;
      edtphi.Enabled := true;
      grpC4Parameters.Height := 121;
      End
   Else
      Begin
      lblphi.Visible := false;
      edtphi.Visible := false;
      edtphi.Enabled := false;
      grpC4Parameters.Height := 89;
      End;
   End;

If Parameter.AgeDecline and (not Parameter.SizeDecline) then
   Begin
   rgIncludeAge.ItemIndex := 1;
   edtMatureAge.enabled := true;
   lblMatureAge.enabled := true;
   edtAgePower.enabled := true;
   lblAgePower.enabled := true;
   edtMatureSize.enabled := false;
   lblMatureSize.enabled := false;
   edtSizePower.enabled := false;
   End
Else if Parameter.SizeDecline and (not Parameter.AgeDecline) then
   Begin
   rgIncludeAge.ItemIndex := 2;
   edtMatureAge.enabled := false;
   lblMatureAge.enabled := false;
   edtAgePower.enabled := false;
   lblAgePower.enabled := false;
   edtMatureSize.enabled := true;
   lblMatureSize.enabled := true;
   edtSizePower.enabled := true;
   End
Else if Parameter.SizeDecline and Parameter.AgeDecline then
   Begin
   rgIncludeAge.ItemIndex := 3;
   edtMatureAge.enabled := true;
   lblMatureAge.enabled := true;
   edtAgePower.enabled := true;
   lblAgePower.enabled := true;
   edtMatureSize.enabled := true;
   lblMatureSize.enabled := true;
   edtSizePower.enabled := true
   End
Else
   Begin
   rgIncludeAge.ItemIndex := 0;
   edtMatureAge.enabled := false;
   lblMatureAge.enabled := false;
   edtAgePower.enabled := false;
   lblAgePower.enabled := false;
   edtMatureSize.enabled := false;
   lblMatureSize.enabled := false;
   edtSizePower.enabled := false;
   End;

If Parameter.FoliageClumping then
   Begin
   edtFoliageClumping.Checked := true;
   edtKlowRange.enabled := true;
   edtCanopyWidthInter.enabled := true;
   edtCanopyWidthSlope.enabled := true;
   lblKlowRange.enabled := true;
   lblCanopyWidth.enabled := true;
   lblIntercept.enabled := true;
   lblSlope.enabled := true;
   End
Else
   Begin
   edtFoliageClumping.Checked := false;
   edtKlowRange.enabled := false;
   edtCanopyWidthInter.enabled := false;
   edtCanopyWidthSlope.enabled := false;
   lblKlowRange.enabled := false;
   lblCanopyWidth.enabled := false;
   lblIntercept.enabled := false;
   lblSlope.enabled := false;
   End;

If Parameter.ConstantLeafN then
   Begin
   edtConstantLeafN.Checked := true;
   edtConstantLeafNValue.enabled := true;
   lblConstantLeafNValue.enabled := true;
   End
Else
   Begin
   edtConstantLeafN.Checked := false;
   edtConstantLeafNValue.enabled := false;
   lblConstantLeafNValue.enabled := false;
   End;

frmMain.FillEdit(Sender, edtNewDelta, Parameter.NewDelta, 1);
frmMain.FillEdit(Sender, edtExtraDelta, Parameter.ExtraDelta, 1);
If Parameter.SetDeltaType = SetValue then
   Begin
   rgCalcNewDelta.ItemIndex := 0;
   edtNewDelta.enabled := true;
   lblNewDelta.enabled := true;
   edtExtraDelta.enabled := false;
   lblExtraDelta.enabled := false;
   End
Else {if Parameter.SetDeltaType = CalculateValue then}
   Begin
   rgCalcNewDelta.ItemIndex := 1;
   edtNewDelta.enabled := false;
   lblNewDelta.enabled := false;
   edtExtraDelta.enabled := true;
   lblExtraDelta.enabled := true;
   End;
grpIsotopes.Enabled := Control.IncludeIsotopes;
grpIsotopes.Visible := Control.IncludeIsotopes;
If Control.IncludeIsotopes then
   Begin
   grpPhsPathway.Top := 442;
   grpC4parameters.Top := grpPhsPathway.Top;
   btnOK.Top := grpPhsPathway.Top + 62;
   btnCancel.Top := btnOK.Top;
   btnHelp.Top := btnOK.Top;
   frmPhotoSyntheticParameters.Height := 610;
   End
Else
   Begin
   grpPhsPathway.Top := 336;
   grpC4parameters.Top := grpPhsPathway.Top;
   btnOK.Top := grpPhsPathway.Top + 122;
   btnCancel.Top := btnOK.Top;
   btnHelp.Top := btnOK.Top;
   frmPhotoSyntheticParameters.Height := 535;
   End;
End;

Procedure TfrmPhotoSyntheticParameters.btnOKClick(Sender: TObject);
Begin
frmMain.GetEdit(Sender, edtSLA, Parameter.SLA, 1 / Control.CConversion);
frmMain.GetEdit(Sender, edtAlbedo, Parameter.Albedo, 100);
frmMain.GetEdit(Sender, edtTransmit, Parameter.Transmit, 100);
If Parameter.Transmit > 0.95 then
   Parameter.Transmit := 0.95; //Safeguard to keep it to sensible values
frmMain.GetEdit(Sender, edtNMVOC, Parameter.NMVOC, 100);
frmMain.GetEdit(Sender, edtkexMax, Parameter.kexMax, 1);
frmMain.GetEdit(Sender, edtKlowRange, Parameter.KlowRange, 1);
frmMain.GetEdit(Sender, edtCanopyWidthInter, Parameter.CanopyWidthInter, 1);
frmMain.GetEdit(Sender, edtCanopyWidthSlope, Parameter.CanopyWidthSlope, 1);
frmMain.GetEdit(Sender, edtConstantLeafNValue, Parameter.ConstantLeafNValue, 1000 * Control.NConversion / Control.CConversion);
frmMain.GetEdit(Sender, edtN0, Parameter.N0, 1000 * Control.NConversion / Control.CConversion);
frmMain.GetEdit(Sender, edtNcrit, Parameter.Ncrit, 1000 * Control.NConversion / Control.CConversion);
frmMain.GetEdit(Sender, edtNmax, Parameter.Nmax, 1000 * Control.NConversion / Control.CConversion);
frmMain.GetEdit(Sender, edtAmax, Parameter.Amax, 1);
frmMain.GetEdit(Sender, edtalpha, Parameter.alpha, 1);
If Parameter.alpha > 0.1 then
   Parameter.alpha := 0.1; //Safeguard to keep it to sensible values
frmMain.GetEdit(Sender, edtTheta, Parameter.Theta, 1);
frmMain.GetEdit(Sender, edtBallBerry1, Parameter.BallBerry1, 1);
frmMain.GetEdit(Sender, edtBallBerry2, Parameter.BallBerry2, 1);
frmMain.GetEdit(Sender, edtTFrost, Parameter.TFrost, 1);
frmMain.GetEdit(Sender, edtTScorch, Parameter.TScorch, 1);
frmMain.GetEdit(Sender, edtTSensitivity, Parameter.TSensitivity, 1);
frmMain.GetEdit(Sender, edtTRepair, Parameter.TRepair, 1);
frmMain.GetEdit(Sender, edtRainDamageLimit, Parameter.RainDamageLimit, 1);
frmMain.GetEdit(Sender, edtRainDamageSensitivity, Parameter.RainDamageSensitivity, 1);
frmMain.GetEdit(Sender, edtRainDamageRepair, Parameter.RainDamageRepair, 1);
frmMain.GetEdit(Sender, edtTMaxRepairTime, Parameter.TMaxRepairTime, 1);
frmMain.GetEdit(Sender, edtTMin, Parameter.TMinLim, 1);
frmMain.GetEdit(Sender, edtTMax, Parameter.TMaxLim, 1);
frmMain.GetEdit(Sender, edtTOpt1, Parameter.TOpt1, 1);
frmMain.GetEdit(Sender, edtTOpt2, Parameter.TOpt2, 1);
if Control.IncludeP then
   Begin
   frmMain.GetEdit(Sender, edtP0, Parameter.P0, 1000 / Control.CConversion);
   frmMain.GetEdit(Sender, edtPcrit, Parameter.Pcrit, 1000 / Control.CConversion);
   frmMain.GetEdit(Sender, edtPmax, Parameter.Pmax, 1000 / Control.CConversion);
   End;
If rdC3_C4.ItemIndex = 0 then
   Parameter.Phs := C3
Else
   Parameter.Phs := C4;
frmMain.GetEdit(Sender, edtrelkPEP, Parameter.relkPEP, 1);
frmMain.GetEdit(Sender, edtbeta, Parameter.beta, 1);
frmMain.GetEdit(Sender, edtphi, Parameter.phi, 1);
frmMain.GetInteger(Sender, edtMatureAge, Parameter.MatureAge);
frmMain.GetEdit(Sender, edtAgePower, Parameter.AgePower, 1);
frmMain.GetInteger(Sender, edtMatureSize, Parameter.MatureSize);
frmMain.GetEdit(Sender, edtSizePower, Parameter.SizePower, 1);

case rgIncludeAge.ItemIndex of
     0: Begin
        Parameter.AgeDecline := false;
        Parameter.SizeDecline := false;
        End;
     1: Begin
        Parameter.AgeDecline := true;
        Parameter.SizeDecline := false;
        End;
     2: Begin
        Parameter.AgeDecline := false;
        Parameter.SizeDecline := true;
        End;
     3: Begin
        Parameter.AgeDecline := true;
        Parameter.SizeDecline := true;
        End;
     End;

If rgCalcNewDelta.ItemIndex = 0 then
   Parameter.SetDeltaType := SetValue
Else {if rgCalcNewDelta.ItemIndex = 1 then}
   Parameter.SetDeltaType := CalculateValue;
frmMain.GetEdit(Sender, edtNewDelta, Parameter.NewDelta, 1);
frmMain.GetEdit(Sender, edtExtraDelta, Parameter.ExtraDelta, 1);
Parameter.FoliageClumping := edtFoliageClumping.Checked;
Parameter.ConstantLeafN := edtConstantLeafN.Checked;
Control.PlantHasChanged := TRUE;
End;

Procedure TfrmPhotoSyntheticParameters.rgIncludeAgeClick(Sender: TObject);
Begin
case rgIncludeAge.ItemIndex of
     0: Begin
        Parameter.AgeDecline := false;
        Parameter.SizeDecline := false;
        End;
     1: Begin
        Parameter.AgeDecline := true;
        Parameter.SizeDecline := false;
        End;
     2: Begin
        Parameter.AgeDecline := false;
        Parameter.SizeDecline := true;
        End;
     3: Begin
        Parameter.AgeDecline := true;
        Parameter.SizeDecline := true;
        End;
     End;

If Parameter.AgeDecline and (not Parameter.SizeDecline) then
   Begin
   rgIncludeAge.ItemIndex := 1;
   edtMatureAge.enabled := true;
   lblMatureAge.enabled := true;
   edtAgePower.enabled := true;
   lblAgePower.enabled := true;
   edtMatureSize.enabled := false;
   lblMatureSize.enabled := false;
   edtSizePower.enabled := false;
   End
Else if Parameter.SizeDecline and (not Parameter.AgeDecline) then
   Begin
   rgIncludeAge.ItemIndex := 2;
   edtMatureAge.enabled := false;
   lblMatureAge.enabled := false;
   edtAgePower.enabled := false;
   lblAgePower.enabled := false;
   edtMatureSize.enabled := true;
   lblMatureSize.enabled := true;
   edtSizePower.enabled := true;
   End
Else if Parameter.SizeDecline and Parameter.AgeDecline then
   Begin
   rgIncludeAge.ItemIndex := 3;
   edtMatureAge.enabled := true;
   lblMatureAge.enabled := true;
   edtAgePower.enabled := true;
   lblAgePower.enabled := true;
   edtMatureSize.enabled := true;
   lblMatureSize.enabled := true;
   edtSizePower.enabled := true
   End
Else
   Begin
   rgIncludeAge.ItemIndex := 0;
   edtMatureAge.enabled := false;
   lblMatureAge.enabled := false;
   edtAgePower.enabled := false;
   lblAgePower.enabled := false;
   edtMatureSize.enabled := false;
   lblMatureSize.enabled := false;
   edtSizePower.enabled := false;
   End;
End;

Procedure TfrmPhotoSyntheticParameters.edtConstantLeafNClick(Sender: TObject);
Begin
Parameter.ConstantLeafN := edtConstantLeafN.Checked;
If Parameter.ConstantLeafN then
   Begin
   edtConstantLeafN.Checked := true;
   edtConstantLeafNValue.enabled := true;
   lblConstantLeafNValue.enabled := true;
   End
Else
   Begin
   edtConstantLeafN.Checked := false;
   edtConstantLeafNValue.enabled := false;
   lblConstantLeafNValue.enabled := false;
   End;
End;

Procedure TfrmPhotoSyntheticParameters.edtFoliageClumpingClick(Sender: TObject);
Begin
Parameter.FoliageClumping := edtFoliageClumping.Checked;
If Parameter.FoliageClumping then
   Begin
   edtFoliageClumping.Checked := true;
   edtKlowRange.enabled := true;
   edtCanopyWidthInter.enabled := true;
   edtCanopyWidthSlope.enabled := true;
   lblKlowRange.enabled := true;
   lblCanopyWidth.enabled := true;
   lblIntercept.enabled := true;
   lblSlope.enabled := true;
   End
Else
   Begin
   edtFoliageClumping.Checked := false;
   edtKlowRange.enabled := false;
   edtCanopyWidthInter.enabled := false;
   edtCanopyWidthSlope.enabled := false;
   lblKlowRange.enabled := false;
   lblCanopyWidth.enabled := false;
   lblIntercept.enabled := false;
   lblSlope.enabled := false;
   End;
End;

Procedure TfrmPhotoSyntheticParameters.rgCalcNewDeltaClick(Sender: TObject);
Begin
If rgCalcNewDelta.ItemIndex = 0 then
   Parameter.SetDeltaType := SetValue
Else {if rgCalcNewDelta.ItemIndex = 1 then}
   Parameter.SetDeltaType := CalculateValue;
frmMain.GetEdit(Sender, edtNewDelta, Parameter.NewDelta, 1);
If Parameter.SetDeltaType = SetValue then
   Begin
   rgCalcNewDelta.ItemIndex := 0;
   edtNewDelta.enabled := true;
   lblNewDelta.enabled := true;
   edtExtraDelta.enabled := false;
   lblExtraDelta.enabled := false;
   End
Else {if Parameter.SetDeltaType = CalculateValue then}
   Begin
   rgCalcNewDelta.ItemIndex := 1;
   edtNewDelta.enabled := false;
   lblNewDelta.enabled := false;
   edtExtraDelta.enabled := true;
   lblExtraDelta.enabled := true;
   End;
End;

Procedure TfrmPhotoSyntheticParameters.rdC3_C4Click(Sender: TObject);
Begin
If rdC3_C4.ItemIndex = 0 then
   Parameter.Phs := C3
Else
   Parameter.Phs := C4;
frmMain.GetEdit(Sender, edtrelkPEP, Parameter.relkPEP, 1);
frmMain.GetEdit(Sender, edtbeta, Parameter.beta, 1);
If Parameter.Phs = C3 then
   Begin
   grpC4parameters.Enabled := false;
   grpC4parameters.Visible := false;
   grpTemperature_Response.Enabled := true;
   grpTemperature_Response.Visible := true;
   rdC3_C4.ItemIndex := 0
   End
Else
   Begin
   grpC4parameters.Enabled := true;
   grpC4parameters.Visible := true;
   grpTemperature_Response.Enabled := false;
   grpTemperature_Response.Visible := false;
   rdC3_C4.ItemIndex := 1;
   If Control.IncludeIsotopes then
      Begin
      lblphi.Visible := true;
      edtphi.Visible := true;
      edtphi.Enabled := true;
      grpC4Parameters.Height := 121;
      End
   Else
      Begin
      lblphi.Visible := false;
      edtphi.Visible := false;
      edtphi.Enabled := false;
      grpC4Parameters.Height := 89;
      End;
   End;
End;

End.
