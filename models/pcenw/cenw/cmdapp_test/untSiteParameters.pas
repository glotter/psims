{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmSiteParameters                               =
  =                                                              =
  =             Edit window to change site parameters            =
  ================================================================
  = File      : untSiteParameters.PAS                            =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untSiteParameters;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, untFieldValidation;

type
  TfrmSiteParameters = class(TForm)
    grpSystemNutrientDynamics: TGroupBox;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    grpTypeOfEvap: TGroupBox;
    rgCalc: TRadioGroup;
    edtCanopyResist: TEdit;
    lblLine1: TLabel;
    Label1: TLabel;
    edtMulching: TEdit;
    Label2: TLabel;
    edtLitterWHC: TEdit;
    Label3: TLabel;
    edtMaxSoilEvap: TEdit;
    lblSlope: TLabel;
    edtSlopeIntercept: TEdit;
    lblFraction: TLabel;
    edtFractionLost: TEdit;
    Label7: TLabel;
    edtTLAISensitivity: TEdit;
    Label8: TLabel;
    edtMaxTBoost: TEdit;
    Label9: TLabel;
    edtSnowInsulation: TEdit;
    Label10: TLabel;
    edtResistSoil: TEdit;
    Label11: TLabel;
    edtRadnMelt: TEdit;
    Label12: TLabel;
    edtWarmMelt: TEdit;
    lblReleaseFertiliser: TLabel;
    edtDailyReleaseRate: TEdit;
    edtLeachingFraction: TEdit;
    lblLeaching: TLabel;
    edtAtmos_N: TEdit;
    Label15: TLabel;
    Label16: TLabel;
    edtVolatilisation: TEdit;
    edtAtmos_P: TEdit;
    lblAtmos_P: TLabel;
    Procedure FormShow(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
    Procedure rgCalcClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSiteParameters: TfrmSiteParameters;

implementation

{$R *.DFM}

uses untDeclarations, untMiscellaneous, untMain;

Procedure TfrmSiteParameters.FormShow(Sender: TObject);

  Procedure FillEdit(edtBox: TEdit; fValue: Real48);
  var Width, Digits: Integer;
      s: String;
  Begin
  GetField(fValue, MaxedtFieldWidth, Width, Digits);
  Str (fValue:Width:Digits, s);
  edtBox.Text := s;
  edtBox.MaxLength := Width;
  End;

begin
  frmMain.FillEdit(Sender, edtWarmMelt, Parameter.SnowMelt, 1);
  frmMain.FillEdit(Sender, edtRadnMelt, Parameter.RadnMelt, 1);
  frmMain.FillEdit(Sender, edtResistSoil, Parameter.SoilTResist, 1);
  frmMain.FillEdit(Sender, edtSnowInsulation, Parameter.SnowInsulate, 1);
  frmMain.FillEdit(Sender, edtVolatilisation, Parameter.Nloss, 1);
  frmMain.FillEdit(Sender, edtAtmos_N, Parameter.Atmos_N, 365 * Control.NConversion);
  frmMain.FillEdit(Sender, edtLeachingFraction, Parameter.Leaching, 1);
  frmMain.FillEdit(Sender, edtDailyReleaseRate, Parameter.FertiliserRelease, 1);
  frmMain.FillEdit(Sender, edtMaxSoilEvap, Parameter.SoilEvap, 1);
  frmMain.FillEdit(Sender, edtFractionLost, Parameter.DirectEvapFract, 1);
  frmMain.FillEdit(Sender, edtLitterWHC, Parameter.LitterWHC, 1);
  frmMain.FillEdit(Sender, edtMulching, Parameter.Mulching, 1);
  frmMain.FillEdit(Sender, edtSlopeIntercept, Parameter.DirectEvapSlope, 1);
  frmMain.FillEdit(Sender, edtMaxTBoost, Parameter.MaxTBoost, 1);
  frmMain.FillEdit(Sender, edtTLAISensitivity, Parameter.TLAISensitivity, 1);
  frmMain.FillEdit(Sender, edtCanopyResist, Parameter.AeroResist, 1);
  frmMain.FillEdit(Sender, edtAtmos_P, Parameter.Atmos_P, 365);
  if Control.IncludeP then
     Begin
     edtAtmos_P.Visible := true;
     lblAtmos_P.Visible := true;
     edtLeachingFraction.Top := 99;
     edtDailyReleaseRate.Top := 126;
     lblLeaching.Top := 103;
     lblReleaseFertiliser.Top := 129;
     grpSystemNutrientDynamics.Height := 157;
     End
  Else
     Begin
     edtAtmos_P.Visible := false;
     lblAtmos_P.Visible := false;
     edtLeachingFraction.Top := 72;
     edtDailyReleaseRate.Top := 99;
     lblLeaching.Top := 75;
     lblReleaseFertiliser.Top := 103;
     grpSystemNutrientDynamics.Height := 135;
     End;
  If (Parameter.DirectEvapType = 'C') then
    rgCalc.ItemIndex := 1 else
    rgCalc.ItemIndex := 0;
  rgCalcClick(Nil);
end;

Procedure TfrmSiteParameters.btnOKClick(Sender: TObject);

  Procedure GetEdit(edtBox: TEdit; var fValue: Real48; fScale: Real48);
  var s: String;
      Num: Real48;
      Code: Integer;
  Begin
  s := edtBox.Text;
  Val (s, Num, Code);
  If Code <> 0 then
     MessageDlg('Caution: Invalid numeric format.' + chr(10) +
                'The previous value is not modified.' +
                'Re-enter the dialogue box if you' + chr(10) +
                'want to enter a valid number.',
                 mtInformation, [mbOK], 0)
  Else
     fValue := Num / fScale;
  End;

begin
  frmMain.GetEdit(Sender, edtWarmMelt, Parameter.SnowMelt, 1);
  frmMain.GetEdit(Sender, edtRadnMelt, Parameter.RadnMelt, 1);
  frmMain.GetEdit(Sender, edtResistSoil, Parameter.SoilTResist, 1);
  frmMain.GetEdit(Sender, edtSnowInsulation, Parameter.SnowInsulate, 1);
  frmMain.GetEdit(Sender, edtVolatilisation, Parameter.Nloss, 1);
  frmMain.GetEdit(Sender, edtAtmos_N, Parameter.Atmos_N, 365 * Control.NConversion);
  frmMain.GetEdit(Sender, edtLeachingFraction, Parameter.Leaching, 1);
  frmMain.GetEdit(Sender, edtDailyReleaseRate, Parameter.FertiliserRelease, 1);
  frmMain.GetEdit(Sender, edtMaxSoilEvap, Parameter.SoilEvap, 1);
  frmMain.GetEdit(Sender, edtFractionLost, Parameter.DirectEvapFract, 1);
  frmMain.GetEdit(Sender, edtLitterWHC, Parameter.LitterWHC, 1);
  frmMain.GetEdit(Sender, edtMulching, Parameter.Mulching, 1);
  frmMain.GetEdit(Sender, edtSlopeIntercept, Parameter.DirectEvapSlope, 1);
  frmMain.GetEdit(Sender, edtMaxTBoost, Parameter.MaxTBoost, 1);
  frmMain.GetEdit(Sender, edtTLAISensitivity, Parameter.TLAISensitivity, 1);
  frmMain.GetEdit(Sender, edtCanopyResist, Parameter.AeroResist, 1);
  frmMain.GetEdit(Sender, edtAtmos_P, Parameter.Atmos_P, 365);
  if (rgCalc.ItemIndex = 1) then
    Parameter.DirectEvapType := 'C' else
    Parameter.DirectEvapType := 'L';
  Control.PlantHasChanged := TRUE;
  Control.SiteHasChanged := TRUE;
end;

Procedure TfrmSiteParameters.rgCalcClick(Sender: TObject);
begin
  // enable the appropriate edit boxes
  edtSlopeIntercept.Enabled := (rgCalc.ItemIndex = 0);
  lblSlope.Enabled := (rgCalc.ItemIndex = 0);
  edtFractionLost.Enabled := (rgCalc.ItemIndex = 1);
  lblFraction.Enabled := (rgCalc.ItemIndex = 1);
end;

end.
