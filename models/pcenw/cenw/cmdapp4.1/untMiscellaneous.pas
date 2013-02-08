{ ================================================================
  = Project   : Cenw                                             =
  ================================================================
  = Module    : AdjustFertility                                  =
  =             ResetPlantPools                                  =
  =             CalculateWoodDensity                             =
  =             DoAnnualCalculations                             =
  =             TotalN                                           =
  =             TotalOrganicP                                    =
  =             TotalP                                           =
  =             CalcDate                                         =
  =             SetUpGenericSave                                 =
  =             SetUpGenericDisplay                              =
  =             SetUpSens                                        =
  =             SetUpHarvestDialogue                             =
  =             GetHarvestDialogueInfo                           =
  =             SetUpPestDialogue                                =
  =             GetPestDialogueInfo                              =
  =             SetUpFireDialogue                                =
  =             GetFireDialogueInfo                              =
  =             SetUpFertiliserDialogue                          =
  =             GetFertiliserDialogueInfo                        =
  =             SetUpEnvironmentDialogue                         =
  =             GetEnvironmentDialogueInfo                       =
  =             SetUpSeedingDialogue                             =
  =             GetSeedingDialogueInfo                           =
  =             SetUpPloughDialogue                              =
  =             GetPloughDialogueInfo                            =
  =             SetUpGrazingDialogue                             =
  =             GetGrazingDialogueInfo                           =
  =             SetUpGrazing2Dialogue                            =
  =             GetGrazing2DialogueInfo                          =
  =             SetUpLitterDialogue                              =
  =             GetLitterDialogueInfo                            =
  =             SetUpSoilWaterDialogue                           =
  =             GetSoilWaterDialogueInfo                         =
  =             SetUpPhenologyDialogue                           =
  =             GetPhenologyDialogueInfo                         =
  =             WriteOutSens                                     =
  =             SwapReal                                         =
  =             SwapLong                                         =
  =             SwapBoolean                                      =
  =                                                              =
  =             Routines to include miscellaneous subroutines    =
  =                                                              =
  ================================================================
  = File      : untMiscellaneous.PAS                             =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

Unit untMiscellaneous;

Interface

Uses
  untDeclarations, SysUtils, untFieldValidation, untDivideValidation, untPowerValidation;
  //untGenericSave, untGenericDisplay;  //JL removed assuming not used. Not verified.
Const MaxSensVariableWidth = 20;

Procedure AdjustFertility;
Procedure ResetPlantPools;
Procedure CalculateWoodDensity;
Procedure CalcHt_Diameter;
Procedure DoAnnualCalculations;
Procedure CalcDate(var DateOut: InfoTransferRecord);
Function TotalN: double;
Function TotalOrganicP: double;
Function TotalP: double;
Procedure SetUpGenericSave(First, Last: SaveVariableOptions; SoilLayerInfo: Boolean; Title: String);
Procedure SetUpGenericDisplay(First, Last: ScreenOptions; Title: String);
Procedure InitialiseSensitivity;
Procedure SetUpSens (Index: Integer);
Procedure WriteOutSens;
Procedure EndSensRoutine;
Procedure SetUpHarvestDialogue;
Procedure GetHarvestDialogueInfo;
Procedure SetUpPestDialogue;
Procedure GetPestDialogueInfo;
Procedure SetUpFireDialogue;
Procedure GetFireDialogueInfo;
Procedure SetUpFertiliserDialogue;
Procedure GetFertiliserDialogueInfo;
Procedure SetUpEnvironmentDialogue;
Procedure GetEnvironmentDialogueInfo;
Procedure SetUpPloughDialogue;
Procedure GetPloughDialogueInfo;
Procedure SetUpOMAdditionsDialogue;
Procedure GetOMAdditionsDialogueInfo;
Procedure SetUpSeedingDialogue;
Procedure GetSeedingDialogueInfo;
Procedure SetUpGrazingDialogue;
Procedure GetGrazingDialogueInfo;
Procedure SetUpGrazing2Dialogue;
Procedure GetGrazing2DialogueInfo;
Procedure SetUpLitterDialogue;
Procedure GetLitterDialogueInfo;
Procedure SetUpSoilWaterDialogue;
Procedure GetSoilWaterDialogueInfo;
Procedure SetUpPhenologyDialogue;
Procedure GetPhenologyDialogueInfo;
Procedure SwapReal(var a, b: double);
Procedure SwapLong(var a, b: integer);
Procedure SwapBoolean(var a, b: Boolean);
Function Min(a, b: double): double;
Function Max(a, b: double): double;

Implementation

Uses untSimulate;

Procedure AdjustFertility;
var E: ElementsUsed;
    iLayer: Integer;
    begin
    For E := C to N do
        For iLayer := 0 to Soil.nLayers do
            Begin
            Soil.Active[iLayer, E] := Soil.Active[iLayer, E] * Parameter.FertilityAdjust;
            Soil.Slow[iLayer, E] := Soil.Slow[iLayer, E] * Parameter.FertilityAdjust;
            Soil.Resistant[iLayer, E] := Soil.Resistant[iLayer, E] * Parameter.FertilityAdjust;
            Soil.Soluble[iLayer, E] := Soil.Soluble[iLayer, E] * Parameter.FertilityAdjust;
            End;
    End; {of Procedure 'AdjustFertility'}

Procedure ResetPlantPools;
var E: ElementsUsed;
Begin
For E := C to N do
    Begin
    Plant.SapWood[E] := Control.Initial.SapWood[E];
    Plant.HeartWood[E] := Control.Initial.HeartWood[E];
    Plant.CoarseRoot[E] := Control.Initial.CoarseRoot[E];
    Plant.FineRoot[E] := Control.Initial.FineRoot[E];
    Plant.Branches[E] := Control.Initial.Branches[E];
    Plant.Bark[E] := Control.Initial.Bark[E];
    Plant.Leaves[E] := Control.Initial.Leaves[E];
    Plant.Pollen[E] := Control.Initial.Pollen[E];
    Plant.Fruit[E] := Control.Initial.Fruit[E];
    Plant.Soluble[E] := Control.Initial.Soluble[E];
    Plant.Reserves[E] := Control.Initial.Reserves[E];
    Plant.WeedLeaves[E] := Control.Initial.WeedLeaves[E];
    Plant.WeedRoots[E] := Control.Initial.WeedRoots[E];
    End;
Plant.Age := Control.Initial.Age;
Plant.Height := Control.Initial.Height;
Plant.DBH := Control.Initial.DBH;
Plant.Stocking := Control.Initial.Stocking;
Control.ExtraDays := Control.Initial.Days;
Control.ExtraMonths := Control.Initial.Months;
Control.TotalYears := Control.Initial.Years;
End; {of Procedure 'ResetPlantPools'}

Procedure CalculateWoodDensity;
Var Do_250, Do_Actual, Sheath, DensitySum, WoodSum: double;
    iRing, MaxAge: Integer;
Begin
Do_250 := Parameter.WoodDensity25 + Parameter.WoodDensTemp * (Derived.AnnualMeanTemp - 15)
          + Divide(Parameter.WoodDensFertility, Derived.NLimit)
          - Parameter.WoodDensFertility;
Do_Actual := Parameter.WoodDensity0 + (Do_250 - Parameter.WoodDensity0)
      * Power(0.5412 + 0.02902 * Power(Plant.Stocking, 0.5), Parameter.WoodDensStocking);
MaxAge := Plant.Age;
if MaxAge > 50 then // Assume that these calculations can only be applied to a maximum plant age of 50 years
   MaxAge := 50;
DensitySum := 0;
WoodSum := 0;
If MaxAge = 0 then
   Begin
   Derived.WoodDensity := Parameter.WoodDensity0;    // units of kg m-3
   Derived.GrowthSheathDensity := Derived.WoodDensity;
   End
Else
   Begin
   for iRing := 1 to MaxAge do
       Begin
       Sheath := Parameter.WoodDensity0 + (Do_actual - Parameter.WoodDensity0) * (iRing / 25);
       WoodSum := WoodSum + Plant.AllWoodRings[iRing];
       DensitySum := DensitySum + Plant.AllWoodRings[iRing] * Sheath;
       End;
   If WoodSum = 0 then
      Derived.WoodDensity := Parameter.WoodDensity0
   Else
      Derived.WoodDensity := Divide(DensitySum, WoodSum);
   Derived.GrowthSheathDensity := Sheath;
   End;
End; {of Procedure 'CalculateWoodDensity'}

Procedure CalcHt_Diameter;
{The Ht_Diameter parameter is completely prescribed by the other parameters and therefore
 does not need to be set by the user.}
Const DeviationLimit = 0.001;
      CorrectionLimit = 0;
var NewHeight, MinHeight, Height0, NewDBH, NewHDSlope, BaseHDSlope, BasalDiam, Crowding, TargetHeight, Deviation: double;
Begin
if Parameter.VariableHD then // only need to do these calculations if HD is variable; otherwise they are done once at the beginning of the run
   Begin
   if Plant.DBH < Parameter.MinDBH then
      Begin
      Derived.HDSlope := Parameter.HD_InitialSlope;
      Parameter.HDInter := Parameter.HD_InitialInter;
      if Plant.DBH > CorrectionLimit then   // This routine checks whether we are actually following the allometric relationship and takes corrective action if we are not
         Begin
         TargetHeight := exp(Parameter.HD_InitialInter + Parameter.HD_InitialSlope * ln(Plant.DBH));
         Deviation := (TargetHeight - Plant.Height) / Plant.Height;
         if abs(Deviation) > DeviationLimit then
            Begin
            Derived.HDSlope := Derived.HDSlope * (1 + 10 * (Plant.DBH - CorrectionLimit) * Deviation);
            If Derived.HDSlope < Parameter.HDSlopeMin then
               Derived.HDSlope := Parameter.HDSlopeMin
            Else if Derived.HDSlope > Parameter.HDSlopeMax then
              Derived.HDSlope := Parameter.HDSlopeMax;
            End;
         End;
      End
   Else // if Plant.DBH > Parameter.MinDBH then   For larger plants we use a variable allometric relationship
      Begin
      Crowding := Plant.Stocking * Plant.Height * Power(Plant.DBH, Parameter.CrowdingPower) * Parameter.CrowdingFactor;
      if Crowding > Parameter.CrowdingMax then
         Crowding := Parameter.CrowdingMax;
      BaseHDSlope := Parameter.HD_Const + Parameter.HD_Temp * Derived.SpringMaxTemp
                   + Parameter.HD_Stocking * Plant.Stocking + Parameter.HD_Fertil * Derived.NLimit
                   + Parameter.HD_Age1 * Plant.Age + Parameter.HD_Age2 * Square(Plant.Age);
      if BaseHDSlope > 5 then
         BaseHDSlope := 5
      Else if BaseHDSlope < 0.01 then
         BaseHDSlope := 0.01;  
      Height0 := Exp(Parameter.HDInter + Parameter.HD_InitialSlope * ln(Parameter.MinDBH));
      NewDBH := 1.01 * Plant.DBH;        // A notional change in dbh from which to work out the h/d relationship if we were not constrained by historical growth
      NewHeight := Height0 * Exp(ln(NewDBH / Parameter.MinDBH) * BaseHDSlope + Crowding + Parameter.CrowdingOffset);
      NewHDSlope := (ln(NewHeight) - ln(Plant.Height)) / (ln(NewDBH)-(ln(Plant.DBH)));
      Derived.HDSlope := NewHDSlope;
      If Derived.HDSlope < Parameter.HDSlopeMin then
         Derived.HDSlope := Parameter.HDSlopeMin
      Else if Derived.HDSlope > Parameter.HDSlopeMax then
         Derived.HDSlope := Parameter.HDSlopeMax;
      End;
   Derived.Dummy := Derived.HDSlope;
   If Parameter.Mindbh > 1 then
      Begin
      MinHeight := exp(Parameter.HDInter + Derived.HDSlope * ln(Parameter.Mindbh));
      If MinHeight > 1.3 then
         Begin
         BasalDiam := Parameter.Mindbh * MinHeight / (MinHeight - 1.3);
         Parameter.Ht_Diameter := 100 * MinHeight / BasalDiam;
         End
      Else
         Parameter.Ht_Diameter := 10;   // to prevent a crash
      End
   Else
      Begin
      Parameter.Mindbh := 1;
      Parameter.Ht_Diameter := 10;   // more precautions against a crash
      End;
   End;
End; {of Procedure 'CalcHt_Diameter'}

Procedure DoAnnualCalculations;
var WoodNConc: double;
    iYear, iDay, iCount: Integer;
Begin
Control.Count := 1;
Plant.Age := Plant.Age + 1;
Derived.AnnualMeanTemp := 0;
iCount := 0;
if Control.CountersSaved or (Control.TotalDays >= 365) then
   Begin
   for iDay := 1 to 365 do
      Derived.AnnualMeanTemp := Derived.AnnualMeanTemp + Derived.Temperature[iDay] / 365
   End
Else
   Begin
   for iDay := 1 to 365 do
      Begin
      Derived.AnnualMeanTemp := Derived.AnnualMeanTemp + Derived.Temperature[iDay];
      if Derived.Temperature[iDay] <> 0 then  // This assumes that values of '0' are missing values - introduces a small error if the true temperature actually happens to be 0 degrees
         iCount := iCount + 1;
      End;
   if iCount > 0 then  // I can't see how that could happen, but it's there as a safeguard anyway
      Derived.AnnualMeanTemp := Derived.AnnualMeanTemp / iCount
   Else
      Derived.AnnualMeanTemp := Weather.TMean;
   End;
WoodNConc := Divide(Plant.SapWood[N], Plant.SapWood[C]);
Plant.HeartWood[C] := Plant.HeartWood[C] + Plant.SapWoodAmount[Parameter.SapWoodYears];
If Control.IncludeIsotopes then
   Dilute (Plant.HeartWood[C13], Plant.HeartWood[C], Plant.SapWood[C13], Plant.SapWoodAmount[Parameter.SapWoodYears]);
Plant.HeartWood[N] := Plant.HeartWood[N] + Plant.SapWoodAmount[Parameter.SapWoodYears]
                    * Parameter.WoodRetrans * WoodNConc;
Plant.Soluble[N] := Plant.Soluble[N] + Plant.SapWoodAmount[Parameter.SapWoodYears]
                    * (1 - Parameter.WoodRetrans) * WoodNConc;
Plant.SapWood[N] := Plant.SapWood[N] - Plant.SapWoodAmount[Parameter.SapWoodYears]* WoodNConc;
Plant.SapWood[C] := Plant.SapWood[C] - Plant.SapWoodAmount[Parameter.SapWoodYears];
If Plant.SapWood[N] < 0 then Plant.SapWood[N] := 0;
   If Plant.SapWood[C] < 0 then Plant.SapWood[C] := 0;
For iYear := Parameter.SapWoodYears downto 2 do
    Plant.SapWoodAmount[iYear] := Plant.SapWoodAmount[iYear - 1];
Plant.SapWoodAmount[1] := Plant.SapWood[C];      {Initialise before subtracting later year's contributions}
For iYear := Parameter.SapWoodYears downto 2 do
    Plant.SapWoodAmount[1] := Plant.SapWoodAmount[1] - Plant.SapWoodAmount[iYear]; {Find last year's contribution to total sapwood}
Plant.Allwoodrings[Plant.Age] := Plant.SapWoodAmount[1];
CalculateWoodDensity;
End; {of Procedure 'DoAnnualCalculations'}

Procedure CalcDate(var DateOut: InfoTransferRecord);
var Ps, ps1, Ps2, ps3: string;
    LeapYear: Boolean;
    LeapDay: 0..1;
    i : integer;
    Begin
    If (Control.TotalYears mod 4 = 0) and
       ((Control.TotalYears mod 100 <> 0) or (Control.TotalYears mod 500 = 0)) then
       Begin
       LeapYear := true;
       LeapDay := 1;
       End
    Else
       Begin
       LeapYear := false;
       LeapDay := 0;
       End;
    For i := 1 to 1 do
        Begin
        Control.ExtraDays := Control.ExtraDays + 1;
        If Control.ExtraMonths in [1, 3, 5, 7, 8, 10, 12] then
           Begin
           If Control.ExtraDays > 31 then
              Begin
              Control.ExtraDays := 1;
              Control.ExtraMonths := Control.ExtraMonths + 1;
              End;
           End
        Else if Control.ExtraMonths in [4, 6, 9, 11] then
           Begin
           If Control.ExtraDays > 30 then
              Begin
              Control.ExtraDays := 1;
              Control.ExtraMonths := Control.ExtraMonths + 1;
              End;
           End
        Else if Control.ExtraMonths = 2 then
           Begin
           If ((not LeapYear and (Control.ExtraDays > 28)) or
              (LeapYear and (Control.ExtraDays > 29))) then
              Begin
              Control.ExtraDays := 1;
              Control.ExtraMonths := Control.ExtraMonths + 1;
              End;
           End;
        If Control.ExtraMonths = 13 then
           Begin
           Control.ExtraMonths := 1;
           Control.TotalYears := Control.TotalYears + 1;
           End;
        End; {of 'For i := 1 to Control.Interval do'}
       Str(Control.ExtraDays:2, Ps1);
    If Control.ExtraDays < 10 then
       Ps1 := ' ' + Ps1;
    Str(Control.ExtraMonths:2, Ps2);
    If Control.ExtraMonths < 10 then
       Ps2 := ' ' + Ps2;
    Str(Control.TotalYears:4, Ps3);
    Ps := Ps1 + '/' + Ps2 + '/' + Ps3;
    strpcopy(DateOut, Ps);
    Case Control.ExtraMonths of
         1: Control.JulianDay := Control.ExtraDays;
         2: Control.JulianDay := Control.ExtraDays + 31;
         3: Control.JulianDay := Control.ExtraDays + 59 + LeapDay;
         4: Control.JulianDay := Control.ExtraDays + 90 + LeapDay;
         5: Control.JulianDay := Control.ExtraDays + 120 + LeapDay;
         6: Control.JulianDay := Control.ExtraDays + 151 + LeapDay;
         7: Control.JulianDay := Control.ExtraDays + 181 + LeapDay;
         8: Control.JulianDay := Control.ExtraDays + 212 + LeapDay;
         9: Control.JulianDay := Control.ExtraDays + 243 + LeapDay;
        10: Control.JulianDay := Control.ExtraDays + 273 + LeapDay;
        11: Control.JulianDay := Control.ExtraDays + 304 + LeapDay;
        12: Control.JulianDay := Control.ExtraDays + 334 + LeapDay;
        End;

    End; {of Procedure 'CalcDate'}

Function TotalN: double;
var iLayer: Integer;
    NSum: double;
Begin
NSum := Plant.SapWood[N] + Plant.HeartWood[N] + Plant.CoarseRoot[N] + Plant.FineRoot[N] + Plant.Reserves[N] +
        Plant.Branches[N] + Plant.Bark[N] + Plant.Pollen[N] + Plant.Fruit[N] + Plant.Soluble[N] +
        Plant.Leaves[N] + Plant.Reserves[N] + Derived.ExcessN + Plant.WeedLeaves[N] + Plant.WeedRoots[N] +
        Litter.CoarseWood[N] + Litter.FineWood[N] + Litter.CoarseRoot[N] +
        Litter.FineRoot[N] + Litter.Leaves[N] + Litter.Other[N];
For iLayer := 0 to Soil.nLayers do
    NSum := NSum +
            Soil.FineWood[iLayer, N] + Soil.CoarseWood[iLayer, N] +
            Soil.Struct[iLayer, N] + Soil.Metab[iLayer, N] +
            Soil.Slow[iLayer, N] + Soil.Active[iLayer, N] +
            Soil.Resistant[iLayer, N] + Soil.Inert[iLayer, N];
TotalN := NSum;
End; {of Function 'TotalN'}

Function TotalOrganicP: double;
var iLayer: Integer;
    PSum: double;
Begin
PSum := Plant.SapWood[P] + Plant.HeartWood[P] + Plant.CoarseRoot[P] + Plant.FineRoot[P] + Plant.Reserves[P] +
        Plant.Branches[P] + Plant.Bark[P] + Plant.Pollen[P] + Plant.Fruit[P] + Plant.Soluble[P] +
        Plant.Leaves[P] + Plant.Reserves[P] + Plant.WeedLeaves[P] + Plant.WeedRoots[P] +
        Litter.CoarseWood[P] + Litter.FineWood[P] + Litter.CoarseRoot[P] +
        Litter.FineRoot[P] + Litter.Leaves[P] + Litter.Other[P];
For iLayer := 0 to Soil.nLayers do
    PSum := PSum +
            Soil.FineWood[iLayer, P] + Soil.CoarseWood[iLayer, P] +
            Soil.Struct[iLayer, P] + Soil.Metab[iLayer, P] +
            Soil.Slow[iLayer, P] + Soil.Active[iLayer, P] +
            Soil.Resistant[iLayer, P] + Soil.Inert[iLayer, P];
TotalOrganicP := PSum;
End; {of Function 'TotalOrganicP'}

Function TotalP: double;
var iLayer: Integer;
    PSum: double;
Begin
PSum := TotalOrganicP;
For iLayer := 0 to Soil.nLayers do
    PSum := PSum +
            Soil.RockP[iLayer, P] + Soil.OccludedP[iLayer, P] +
            Soil.SecondaryInorganicP[iLayer, P];
TotalP := PSum;
End; {of Function 'TotalP'}

Procedure SetUpGenericSave(First, Last: SaveVariableOptions; SoilLayerInfo: Boolean; Title: String);
Begin
GenericOutput.FirstSave := First;
GenericOutput.LastSave := Last;
GenericOutput.IncludeSoilLayerInfo := SoilLayerInfo;
GenericOutput.Title := Title;
//frmGenericSave.ShowModal; //JL removed. Not verified
End; {of Procedure 'SetUpGenericSave'}

Procedure SetUpGenericDisplay(First, Last: ScreenOptions; Title: String);
Begin
GenericOutput.FirstDisplay := First;
GenericOutput.LastDisplay := Last;
GenericOutput.Title := Title;
//frmGenericDisplay.ShowModal;  //JL removed. Not verified.
End; {of Procedure 'SetUpGenericDisplay'}

Procedure SetUpHarvestDialogue;
var iRow, iDays, iMonths, iYears: Integer;
Begin
List.Caption := 'Harvest or Thinning';
List.MaxRows := MaxHarvestEvents;
List.nEntries := 9;
List.StrOptions := 2;
List.StringConst[1] := 'Adjust';
List.StringConst[2] := 'Same';
If Event.HarvestUnits = '%' then
   List.Header := 1
Else if Event.HarvestUnits = 'W' then
   List.Header := 2
Else if Event.HarvestUnits = 'V' then
   List.Header := 3
Else
   Begin
   Event.HarvestUnits := '%';
   List.Header := 1;
   End;
List.DataType[0] := I;
List.DataType[1] := I;
List.DataType[2] := I;
List.DataType[3] := R;
List.DataType[4] := R;
List.DataType[5] := S;
List.DataType[6] := R;
List.DataType[7] := R;
List.DataType[8] := R;
List.Width[0] := 51;
List.Width[1] := 51;
List.Width[2] := 54;
List.Width[3] := 71;
List.Width[4] := 57;
List.Width[5] := 72;
List.Width[6] := 72;
List.Width[7] := 74;
List.Width[8] := 64;
List.Text[1, 0] := 'Day';
List.Text[1, 1] := 'Month';
List.Text[1, 2] := 'Year';
List.Text[1, 3] := 'Wood cut  (%)';
List.Text[1, 4] := 'Rel. size of cut trees';
List.Text[1, 5] := 'Stocking (Same or Adjust)';
List.Text[1, 6] := 'Branches cut (%)';
List.Text[1, 7] := 'Wood removed (%)';
List.Text[1, 8] := 'Fine material removed (%)';
List.Text[2, 0] := 'Day';
List.Text[2, 1] := 'Month';
List.Text[2, 2] := 'Year';
List.Text[2, 3] := 'Wood cut (tDW ha-1)';
List.Text[2, 4] := 'Rel. size of cut trees';
List.Text[2, 5] := 'Stocking (Same or Adjust)';
List.Text[2, 6] := 'Branches cut (tDW ha-1)';
List.Text[2, 7] := 'Wood removed (tDW ha-1)';
List.Text[2, 8] := 'Fine material removed (tDW ha-1)';
List.Text[3, 0] := 'Day';
List.Text[3, 1] := 'Month';
List.Text[3, 2] := 'Year';
List.Text[3, 3] := 'Wood cut (m3 ha-1)';
List.Text[3, 4] := 'Rel. size of cut trees';
List.Text[3, 5] := 'Stocking (Same or Adjust)';
List.Text[3, 6] := 'Branches cut (m3 ha-1)';
List.Text[3, 7] := 'Wood removed (m3 ha-1)';
List.Text[3, 8] := 'Fine material removed (m3 ha-1)';
List.nRows := Event.nHarvests;
For iRow := 1 to List.nRows do
    Begin
    If Event.RelativeSize[iRow] = 0 then // probably from an older data set
       Event.RelativeSize[iRow] := 1;
    If (Event.HarvestTimes[iRow, 1] = 0) and (Event.HarvestTimes[iRow,2] = 0) and (Event.HarvestTimes[iRow,3] = 0) then
       Begin
       iDays := Event.HarvestTimes[iRow, 4];
       iYears := Trunc((0.5 + iDays) / 365.25);
       iDays := Trunc(iDays - iYears * 365.25);
       iMonths := iDays div 30;
       End
    Else
       Begin
       iDays := Event.HarvestTimes[iRow, 1];
       iMonths := Event.HarvestTimes[iRow, 2];
       iYears := Event.HarvestTimes[iRow, 3];;
       while iDays > 30 do
           Begin
           iMonths := iMonths + 1;
           iDays := iDays - 30;
           End;
       while iMonths > 12 do
           Begin
           iYears := iYears + 1;
           iMonths := iMonths - 12;
           End;
       End;
    List.Data[iRow, 0] := iDays;
    List.Data[iRow, 1] := iMonths;
    List.Data[iRow, 2] := iYears;
    List.Data[iRow, 3] := Event.WoodCut[iRow];
    List.Data[iRow, 4] := Event.RelativeSize[iRow];
    if (Event.AdjustStocking[iRow]) then
        List.Data[iRow, 5] := 1
    Else
        List.Data[iRow, 5] := 2;
    List.Data[iRow, 6] := Event.BranchesCut[iRow];
    List.Data[iRow, 7] := Event.WoodRemoval[iRow];
    List.Data[iRow, 8] := Event.FineRemoval[iRow];
    If Event.HarvestUnits = '%' then
       Begin
       List.Data[iRow, 3] := List.Data[iRow, 3] * 100;
       List.Data[iRow, 4] := List.Data[iRow, 4] * 100;
       List.Data[iRow, 6] := List.Data[iRow, 6] * 100;
       List.Data[iRow, 7] := List.Data[iRow, 7] * 100;
       List.Data[iRow, 8] := List.Data[iRow, 8] * 100;
       End;
    End;
List.nRadios := 2;
List.RadioOptions[1] := 2;
List.RadioHeading[1] := 'Meaning of times';
List.RadioText[1, 1] := 'Calendar date';
List.RadioText[1, 2] := 'Time from beginning of run';
If (Control.Harvest_DateType = 'D') then
   List.RbtnSelected[1] := 0
Else
   List.RbtnSelected[1] := 1;
List.RadioOptions[2] := 3;
List.RadioHeading[2] := 'Harvest expressed as:';
If (Event.HarvestUnits = '%') then
   List.RbtnSelected[2] := 0
Else if (Event.HarvestUnits = 'W') then
   List.RbtnSelected[2] := 1
Else {if Event.HarvestUnits = 'V' then}
   List.RbtnSelected[2] := 2;
List.RadioText[2, 1] := 'Fractional removal';
List.RadioText[2, 2] := 'Removal in tDW';
List.RadioText[2, 3] := 'Removal in m3';
List.FileExt := 'hv!';
List.FileComment := 'Harvest/ thinning sequence';
List.HelpContext := 8400;
List.TextBox := false;
List.RedrawOption := true;
List.HasChanged := false;
End; {of Procedure 'SetUpHarvestDialogue'}

Procedure GetHarvestDialogueInfo;
var iRow, iDays, iMonths, iYears, iCount2: Integer;
    TotalDays: integer;
    fDate1, fDate2: TDateTime;
Begin
Event.nHarvests := List.nRows;
For iRow := 1 to List.nRows do
    Begin
    If List.RbtnSelected[2] = 0 then
       Begin
       List.Data[iRow, 3] := List.Data[iRow, 3] * 0.01;
       List.Data[iRow, 4] := List.Data[iRow, 4] * 0.01;
       List.Data[iRow, 6] := List.Data[iRow, 6] * 0.01;
       List.Data[iRow, 7] := List.Data[iRow, 7] * 0.01;
       List.Data[iRow, 8] := List.Data[iRow, 8] * 0.01;
       End;
    iDays := Round(List.Data[iRow, 0]);
    iMonths := Round(List.Data[iRow, 1]);
    iYears := Round(List.Data[iRow, 2]);
    TotalDays := Round(365.25 * iYears + 30.4375 * iMonths + iDays);
    Event.HarvestTimes[iRow, 1] := iDays;
    Event.HarvestTimes[iRow, 2] := iMonths;
    Event.HarvestTimes[iRow, 3] := iYears;
    Event.HarvestTimes[iRow, 4] := TotalDays;
    Event.WoodCut[iRow] := List.Data[iRow, 3];
    Event.RelativeSize[iRow] := List.Data[iRow, 4];
    Event.BranchesCut[iRow] := List.Data[iRow, 6];
    Event.WoodRemoval[iRow] := List.Data[iRow, 7];
    Event.FineRemoval[iRow] := List.Data[iRow, 8];
    If List.Data[iRow, 5] = 1 then
       Event.AdjustStocking[iRow] := true
    Else
       Event.AdjustStocking[iRow] := False;
    end;
// now sort them based on start time - use BubbleSort method
For iRow := 1 to List.nRows - 1 do
    Begin
    For iCount2 := List.nRows - 1 downto iRow do
        Begin
        fDate1 := Event.HarvestTimes[iCount2, 4];
        fDate2 := Event.HarvestTimes[iCount2+1, 4];
        if (fDate1 > fDate2) then
           begin
           SwapLong(Event.HarvestTimes[iCount2, 1], Event.HarvestTimes[iCount2+1, 1]);
           SwapLong(Event.HarvestTimes[iCount2, 2], Event.HarvestTimes[iCount2+1, 2]);
           SwapLong(Event.HarvestTimes[iCount2, 3], Event.HarvestTimes[iCount2+1, 3]);
           SwapLong(Event.HarvestTimes[iCount2, 4], Event.HarvestTimes[iCount2+1, 4]);
           SwapReal(Event.WoodCut[iCount2], Event.WoodCut[iCount2+1]);
           SwapReal(Event.RelativeSize[iCount2], Event.RelativeSize[iCount2+1]);
           SwapBoolean(Event.AdjustStocking[iCount2], Event.AdjustStocking[iCount2+1]);
           SwapReal(Event.BranchesCut[iCount2], Event.BranchesCut[iCount2+1]);
           SwapReal(Event.WoodRemoval[iCount2], Event.WoodRemoval[iCount2+1]);
           SwapReal(Event.FineRemoval[iCount2], Event.FineRemoval[iCount2+1]);
           End;
        End;
    End;
If List.RbtnSelected[1] = 0 then
    Control.Harvest_DateType := 'D'
else
    Control.Harvest_DateType := 'B';
If List.RbtnSelected[2] = 0 then
   Event.HarvestUnits := '%'
Else if List.RbtnSelected[2] = 1 then
   Event.HarvestUnits := 'W'
Else {if List.RbtnSelected[2] = 2 then}
   Event.HarvestUnits := 'V';
Control.ProjectHasChanged := true;
End; {of Procedure 'GetHarvestDialogueInfo'}

Procedure SetUpPestDialogue;
var iRow, iDays, iMonths, iYears: Integer;
    CumDays: integer;
    PestMortality: double;
Begin
// Event Manager - pests and diseases
List.Caption := 'Set incidences of pests (insects or diseases)';
List.nEntries := 10;
List.MaxRows := MaxPestEvents;
List.StrOptions := 0;
If Event.PestDamageUnits = '%' then
   List.Header := 1
Else //   If Event.PestDamageUnits = '%' then
   List.Header := 2;
List.DataType[0] := I;
List.DataType[1] := I;
List.DataType[2] := I;
List.DataType[3] := I;
List.DataType[4] := R;
List.DataType[5] := R;
List.DataType[6] := R;
List.DataType[7] := R;
List.DataType[8] := R;
List.DataType[9] := R;
List.Width[0] := 36;
List.Width[1] := 41;
List.Width[2] := 43;
List.Width[3] := 52;
List.Width[4] := 70;
List.Width[5] := 72;
List.Width[6] := 70;
List.Width[7] := 64;
List.Width[8] := 64;
List.Width[9] := 76;
List.Text[1, 0] := 'Day';
List.Text[1, 1] := 'Month';
List.Text[1, 2] := 'Year';
List.Text[1, 3] := 'Duration (days)';
List.Text[1, 4] := 'defoliated    (% per day)';
List.Text[1, 5] := 'sap sucked (% per day)';
List.Text[1, 6] := 'induc. senesc (% per day)';
List.Text[1, 7] := 'reduced phs (%)';
List.Text[1, 8] := 'mortality (% per event)';
List.Text[1, 9] := 'size ratio of killed trees (%)';
List.Text[2, 0] := 'Day';
List.Text[2, 1] := 'Month';
List.Text[2, 2] := 'Year';
List.Text[2, 3] := 'Duration (days)';
List.Text[2, 4] := 'defoliated    (kgDW / day)';
List.Text[2, 5] := 'sap sucked (kgDW / day)';
List.Text[2, 6] := 'induc. senesc (kgDW / day)';
List.Text[2, 7] := 'reduced phs (%)';
List.Text[2, 8] := 'mortality (% per event)';
List.Text[2, 9] := 'size ratio of killed trees (%)';
List.nRows := Event.nPests;
For iRow := 1 to List.nRows do
    Begin
    If (Event.PestTimes[iRow,1] = 0) and (Event.PestTimes[iRow,2] = 0) and (Event.PestTimes[iRow,3] = 0) then
       Begin
       CumDays := Event.PestTimes[iRow, 4];
       iYears := Trunc((0.5 + CumDays) / 365.25);
       CumDays := Trunc(CumDays - iYears * 365.25);
       iMonths := CumDays div 30;
       iDays := CumDays - 30* iMonths;
       End
    Else
       Begin
       iDays := Event.PestTimes[iRow, 1];
       iMonths := Event.PestTimes[iRow, 2];
       iYears := Event.PestTimes[iRow, 3];
       End;
    List.Data[iRow, 0] := iDays;
    List.Data[iRow, 1] := iMonths;
    List.Data[iRow, 2] := iYears;
    List.Data[iRow, 3] := Event.PestTimes[iRow, 5];
    List.Data[iRow, 4] := Event.LeafDamage[iRow];
    List.Data[iRow, 5] := Event.SolubleDamage[iRow];
    List.Data[iRow, 6] := Event.SenescenceDamage[iRow];
    List.Data[iRow, 7] := 100* Event.PhotosynthesisFraction[iRow];
    If Event.PestTimes[iRow, 5] > 0 then
       PestMortality := 100 * (1 - Power(1 - Event.PestMortality[iRow], Event.PestTimes[iRow, 5]))
    Else
       PestMortality := 0;
    If PestMortality > 100 then
       PestMortality := 100;
    List.Data[iRow, 8] := PestMortality;
    List.Data[iRow, 9] := 100 * Event.PestDeathRatio[iRow];
    If Event.PestDamageUnits = '%' then
       Begin
       List.Data[iRow, 4] := List.Data[iRow, 4] * 100;
       List.Data[iRow, 5] := List.Data[iRow, 5] * 100;
       List.Data[iRow, 6] := List.Data[iRow, 6] * 100;
       End;
    End;
List.nRadios := 2;
List.RadioOptions[1] := 2;
List.RadioHeading[1] := 'Meaning of times';
List.RadioText[1, 1] := 'Calendar date';
List.RadioText[1, 2] := 'Time from beginning of run';
If (Control.Pest_DateType = 'D') then
   List.RbtnSelected[1] := 0
Else
   List.RbtnSelected[1] := 1;
List.RadioOptions[2] := 2;
List.RadioHeading[2] := 'Damage expressed as:';
If (Event.PestDamageUnits = '%') then
   List.RbtnSelected[2] := 0
Else {if Event.PestDamageUnits = 'W' then}
   List.RbtnSelected[2] := 1;
List.RadioText[2, 1] := 'Daily fractional damage';
List.RadioText[2, 2] := 'Damage in daily amounts';
List.FileExt := 'ps!';
List.FileComment := 'Pest outbreak sequence';
List.HelpContext := 8500;
List.TextBox := false;
List.RedrawOption := true;
List.HasChanged := false;
End; {of Procedure 'SetUpPestDialogue'}

Procedure GetPestDialogueInfo;
var iRow, iCount2, iDay, iMonth, iYear, iDuration: integer;
    CumDays: integer;
    fDate1, fDate2: TDateTime;
    Killed: double;
Begin
Event.nPests := List.nRows;
If List.RbtnSelected[1] = 0 then
   Control.Pest_DateType := 'D'
Else
   Control.Pest_DateType := 'B';
If List.RbtnSelected[2] = 0 then
   Event.PestDamageUnits := '%'
Else
   Event.PestDamageUnits := 'W';
For iRow := 1 to List.nRows do
    Begin
    If List.RbtnSelected[2] = 0 then
       Begin
       List.Data[iRow, 4] := List.Data[iRow, 4] * 0.01;
       List.Data[iRow, 5] := List.Data[iRow, 5] * 0.01;
       List.Data[iRow, 6] := List.Data[iRow, 6] * 0.01;
       End;
    If List.Data[iRow, 0] = -1 then
       iDay := -1
    Else
       iDay := Round(List.Data[iRow, 0]);
    If List.Data[iRow, 1] = -1 then
       iMonth := -1
    Else
       iMonth := Round(List.Data[iRow, 1]);
    If List.Data[iRow, 2] = -1 then
       iYear := -1
    Else
       iYear := Round(List.Data[iRow, 2]);
    If List.Data[iRow, 3] = -1 then
       iduration := -1
    Else
       iduration := Round(List.Data[iRow, 3]);
    If (((iMonth >= 0) and (iYear >= 0) and (iDay >= 0)) and    // make sure none is negative and
        ((iMonth > 0) or (iYear > 0) or (iDay > 0))) then       // at least one of them is positive
       CumDays := Round(365.25 * iYear + 30.4375 * iMonth + iDay)
    Else
       Begin
       If iRow > 1 then
          CumDays := Event.PestTimes[iRow - 1, 4] + Event.PestTimes[iRow - 1, 5]
       Else
          CumDays := 0;
       End;
    Killed := List.Data[iRow, 8];
    If Killed <> 0 then
       Event.PestDeathRatio[iRow] := 0.01 * List.Data[iRow, 9]
    Else
       Event.PestDeathRatio[iRow] := 1;
    Event.PestTimes[iRow, 1] := iDay;
    Event.PestTimes[iRow, 2] := iMonth;
    Event.PestTimes[iRow, 3] := iYear;
    Event.PestTimes[iRow, 4] := CumDays;
    Event.PestTimes[iRow, 5] := iDuration;
    If iDuration > 0 then
       Event.PestMortality[iRow] := 1 - Power((1 - 0.01 * Killed), 1 / iDuration)
    Else
       Event.PestMortality[iRow] := 0;
    Event.PhotosynthesisFraction[iRow] := 0.01 * List.Data[iRow, 7];
    Event.LeafDamage[iRow] := List.Data[iRow, 4];
    Event.SolubleDamage[iRow] := List.Data[iRow, 5];
    Event.SenescenceDamage[iRow] := List.Data[iRow, 6];
    End;
// now sort them based on start time - use BubbleSort method
For iRow := 1 to List.nRows - 1 do
    Begin
    For iCount2 := List.nRows - 1 downto iRow do
        Begin
        fDate1 := Event.PestTimes[iCount2, 4];
        fDate2 := Event.PestTimes[iCount2+1, 4];
        if (fDate1 > fDate2) then
           begin
           SwapLong(Event.PestTimes[iCount2, 1], Event.PestTimes[iCount2+1, 1]);
           SwapLong(Event.PestTimes[iCount2, 2], Event.PestTimes[iCount2+1, 2]);
           SwapLong(Event.PestTimes[iCount2, 3], Event.PestTimes[iCount2+1, 3]);
           SwapLong(Event.PestTimes[iCount2, 4], Event.PestTimes[iCount2+1, 4]);
           SwapLong(Event.PestTimes[iCount2, 5], Event.PestTimes[iCount2+1, 5]);
           SwapReal(Event.LeafDamage[iCount2], Event.LeafDamage[iCount2+1]);
           SwapReal(Event.SolubleDamage[iCount2], Event.SolubleDamage[iCount2+1]);
           SwapReal(Event.SenescenceDamage[iCount2], Event.SenescenceDamage[iCount2+1]);
           SwapReal(Event.PhotosynthesisFraction[iCount2], Event.PhotosynthesisFraction[iCount2+1]);
           SwapReal(Event.PestMortality[iCount2], Event.PestMortality[iCount2+1]);
           SwapReal(Event.PestDeathRatio[iCount2], Event.PestDeathRatio[iCount2+1]);
           End;
        End;
    End;
If Event.nPests > 0 then
  If Event.PestTimes[Event.nPests, 5] > 0 then
   // Create a notional final pest event to reset everything back to zero
   Begin
   Event.nPests := Event.nPests + 1;
   Event.PestTimes[Event.nPests, 4] := Event.PestTimes[Event.nPests - 1, 4] + Event.PestTimes[Event.nPests - 1, 5];
   Event.PestTimes[Event.nPests, 5] := -1;
   CumDays := Event.PestTimes[Event.nPests, 4];
   iYear := Trunc((0.5 + CumDays) / 365.25);
   CumDays := Trunc(CumDays - iYear * 365.25);
   iMonth := CumDays div 30;
   iDay := CumDays - 30* iMonth;
   Event.PestTimes[Event.nPests, 1] := iDay;
   Event.PestTimes[Event.nPests, 2] := iMonth;
   Event.PestTimes[Event.nPests, 3] := iYear;
   Event.LeafDamage[Event.nPests] := 0;;
   Event.SolubleDamage[Event.nPests] := 0;
   Event.SenescenceDamage[Event.nPests] := 0;
   Event.PhotosynthesisFraction[Event.nPests] := 0;
   Event.PestMortality[Event.nPests] := 0;
   Event.PestDeathRatio[Event.nPests] := 1;
   Event.PestTimes[Event.nPests - 1, 5] := -1;
   End;
Control.ProjectHasChanged := true;
End; {of Procedure 'GetPestDialogueInfo'}

Procedure SetUpFireDialogue;
var iRow, iMonths, iYears: Integer;
    iDays: integer;
Begin
// Event Manager - fire
List.Caption := 'Set times and characteristics of fires';
If Control.IncludeP then
   List.nEntries := 11
Else
   List.nEntries := 10;
List.MaxRows := MaxFireEvents;
List.StrOptions := 0;
List.Header := 1;
List.DataType[0] := I;
List.DataType[1] := I;
List.DataType[2] := R;
List.DataType[3] := R;
List.DataType[4] := R;
List.DataType[5] := R;
List.DataType[6] := R;
List.DataType[7] := R;
List.DataType[8] := R;
List.DataType[9] := R;
List.DataType[10] := R;
List.Width[0] := 51;
List.Width[1] := 51;
List.Width[2] := 65;
List.Width[3] := 65;
List.Width[4] := 65;
List.Width[5] := 65;
List.Width[6] := 65;
List.Width[7] := 65;
List.Width[8] := 65;
List.Width[9] := 65;
List.Width[10] := 65;
List.Text[1, 0] := 'Months';
List.Text[1, 1] := 'Years';
List.Text[1, 2] := 'Foliage burnt (%)';
List.Text[1, 3] := 'Stems burnt (%)';
List.Text[1, 4] := 'Litter burnt (%)';
List.Text[1, 5] := 'Foliage killed (%)';
List.Text[1, 6] := 'Trees killed (%)';
List.Text[1, 7] := 'Wood to char (%)';
List.Text[1, 8] := 'Foliage + litter to char (%)';
List.Text[1, 9] := 'Combustion N:C ratio';
if Control.IncludeP then
   List.Text[1, 10] := 'Combustion P:C ratio';
List.nRows := Event.nFires;
For iRow := 1 to List.nRows do
    Begin
    If (Event.FireTimes[iRow,1] = 0) and (Event.FireTimes[iRow,2] = 0) and (Event.FireTimes[iRow,3] = 0) then
       Begin
       iDays := Event.FireTimes[iRow, 4];
       iYears := Trunc((0.5 + iDays) / 365.25);
       iDays := Trunc(iDays - iYears * 365.25);
       iMonths := iDays div 30;
       End
    Else
       Begin
       iMonths := Event.FireTimes[iRow, 2];
       iYears := Event.FireTimes[iRow, 3];;
       End;
    List.Data[iRow, 0] := iMonths;
    List.Data[iRow, 1] := iYears;
    List.Data[iRow, 2] := 100 * Event.LeafBurn[iRow];
    List.Data[iRow, 3] := 100 * Event.WoodBurn[iRow];
    List.Data[iRow, 4] := 100 * Event.LitterBurn[iRow];
    List.Data[iRow, 5] := 100 * Event.LeafBurnSenesc[iRow];
    List.Data[iRow, 6] := 100 * Event.WoodBurnSenesc[iRow];
    List.Data[iRow, 7] := 100 * Event.WoodToChar[iRow];
    List.Data[iRow, 8] := 100 * Event.FineToChar[iRow];
    List.Data[iRow, 9] := Event.Burn_N_CRatio[iRow];
    if Control.IncludeP then
       List.Data[iRow, 10] := Event.Burn_P_CRatio[iRow];
    End;
List.nRadios := 1;
List.RadioOptions[1] := 2;
List.RadioHeading[1] := 'Meaning of times';
List.RadioText[1, 1] := 'Calendar date';
List.RadioText[1, 2] := 'Time from beginning of run';
If (Control.Fire_DateType = 'D') then
   List.RbtnSelected[1] := 0
Else
   List.RbtnSelected[1] := 1;
List.FileExt := 'fr!';
List.FileComment := 'Fire sequence';
List.HelpContext := 8600;
List.TextBox := false;
List.RedrawOption := false;
End; {of Procedure 'SetUpFireDialogue'}

Procedure GetFireDialogueInfo;
var iRow, iCount2, iDay, iMonth, iYear: integer;
    fDate1, fDate2: TDateTime;
    SumFates: double;
Begin
Event.nFires := List.nRows;
If List.RbtnSelected[1] = 0 then
   Control.Fire_DateType := 'D'
Else
   Control.Fire_DateType := 'B';
For iRow := 1 to List.nRows do
    Begin
      iMonth := Round(List.Data[iRow, 0]);
      iYear := Round(List.Data[iRow, 1]);
      iDay := Round(365.25 * iYear + 30.4375 * iMonth);
      Event.FireTimes[iRow, 1] := 1;
      Event.FireTimes[iRow, 2] := iMonth;
      Event.FireTimes[iRow, 3] := iYear;
      Event.FireTimes[iRow, 4] := iDay;
      Event.LeafBurn[iRow] := 0.01 * List.Data[iRow, 2];
      Event.WoodBurn[iRow] := 0.01 * List.Data[iRow, 3];
      Event.LitterBurn[iRow] := 0.01 * List.Data[iRow, 4];
      Event.LeafBurnSenesc[iRow] := 0.01 * List.Data[iRow, 5];
      Event.WoodBurnSenesc[iRow] := 0.01 * List.Data[iRow, 6];
      Event.WoodtoChar[iRow] := 0.01 * List.Data[iRow, 7];
      Event.FinetoChar[iRow] := 0.01 * List.Data[iRow, 8];
      Event.Burn_N_CRatio[iRow] := List.Data[iRow, 9];
      if Control.IncludeP then
         Event.Burn_P_CRatio[iRow] := List.Data[iRow, 10];
      If (Event.WoodBurn[iRow] + Event.WoodtoChar[iRow] + Event.WoodBurnSenesc[iRow]) > 1 then // The sum of fates of wood to be burnt, killed or turned into charcoal cannot exceed '1'
         Begin
         SumFates := Event.WoodBurn[iRow] + Event.WoodtoChar[iRow] + Event.WoodBurnSenesc[iRow];
         Event.WoodBurn[iRow] := Event.WoodBurn[iRow] / SumFates;
         Event.WoodtoChar[iRow] := Event.WoodtoChar[iRow] / SumFates;
         Event.WoodBurnSenesc[iRow] := Event.WoodBurnSenesc[iRow] / SumFates;
         End;
      If (Event.LeafBurn[iRow] + Event.FinetoChar[iRow] + Event.LeafBurnSenesc[iRow]) > 1 then // The sum of fates of foliage to be burnt, killed or turned into charcoal cannot exceed '1'
         Begin
         SumFates := Event.LeafBurn[iRow] + Event.FinetoChar[iRow] + Event.LeafBurnSenesc[iRow];
         Event.LeafBurn[iRow] := Event.LeafBurn[iRow] / SumFates;
         Event.FinetoChar[iRow] := Event.FinetoChar[iRow] / SumFates;
         Event.LeafBurnSenesc[iRow] := Event.LeafBurnSenesc[iRow] / SumFates;
         End;
      If (Event.LitterBurn[iRow] + Event.FinetoChar[iRow]) > 1 then // The sum of fates of litter to be burnt or turned into charcoal cannot exceed '1'
         Event.LitterBurn[iRow] := 1 - Event.FinetoChar[iRow]; // Changes to FineToChar are constrained by foliage considerations which are given precedence
    End;
// now sort them based on start time - use BubbleSort method
For iRow := 1 to List.nRows - 1 do
    Begin
    For iCount2 := List.nRows - 1 downto iRow do
        Begin
        fDate1 := Event.FireTimes[iCount2, 4];
        fDate2 := Event.FireTimes[iCount2+1, 4];
        if (fDate1 > fDate2) then
           begin
          SwapLong(Event.FireTimes[iCount2, 1], Event.FireTimes[iCount2+1, 1]);
          SwapLong(Event.FireTimes[iCount2, 2], Event.FireTimes[iCount2+1, 2]);
          SwapLong(Event.FireTimes[iCount2, 3], Event.FireTimes[iCount2+1, 3]);
          SwapLong(Event.FireTimes[iCount2, 4], Event.FireTimes[iCount2+1, 4]);
          SwapReal(Event.LeafBurn[iCount2], Event.LeafBurn[iCount2+1]);
          SwapReal(Event.WoodBurn[iCount2], Event.WoodBurn[iCount2+1]);
          SwapReal(Event.LitterBurn[iCount2], Event.LitterBurn[iCount2+1]);
          SwapReal(Event.LeafBurnSenesc[iCount2], Event.LeafBurnSenesc[iCount2+1]);
          SwapReal(Event.WoodBurnSenesc[iCount2], Event.WoodBurnSenesc[iCount2+1]);
          SwapReal(Event.WoodToChar[iCount2], Event.WoodToChar[iCount2+1]);
          SwapReal(Event.FineToChar[iCount2], Event.FineToChar[iCount2+1]);
          SwapReal(Event.Burn_N_CRatio[iCount2], Event.Burn_N_CRatio[iCount2+1]);
          SwapReal(Event.Burn_P_CRatio[iCount2], Event.Burn_P_CRatio[iCount2+1]);
           End;
        End;
    End;
Control.ProjectHasChanged := true;
End; {of Procedure 'GetFireDialogueInfo'}

Procedure SetUpFertiliserDialogue;
var iRow: Integer;
Begin
// Event Manager - fertiliser application
List.Caption := 'Fertiliser application';
if Control.IncludeP then
   List.nEntries := 5
Else
   List.nEntries := 4;
List.MaxRows := MaxFertiliseEvents;
List.StrOptions := 0;
List.Header := 1;
List.DataType[0] := I;
List.DataType[1] := I;
List.DataType[2] := I;
List.DataType[3] := R;
List.DataType[4] := R;
List.Width[0] := 70;
List.Width[1] := 70;
List.Width[2] := 70;
List.Width[3] := 70;
List.Width[4] := 70;
List.Text[1, 0] := 'Day';
List.Text[1, 1] := 'Month';
List.Text[1, 2] := 'Year';
List.Text[1, 3] := 'N Fertiliser added';
List.Text[1, 4] := 'P Fertiliser added';
List.nRows := Event.nFertilisations;
For iRow := 1 to List.nRows do
    Begin
    List.Data[iRow, 0] := Event.FertiliseTimes[iRow, 1];
    List.Data[iRow, 1] := Event.FertiliseTimes[iRow, 2];
    List.Data[iRow, 2] := Event.FertiliseTimes[iRow, 3];
    List.Data[iRow, 3] := Event.FertiliseAmount[iRow, N];
    List.Data[iRow, 4] := Event.FertiliseAmount[iRow, P];
    End;
List.nRadios := 1;
List.RadioOptions[1] := 2;
List.RadioHeading[1] := 'Meaning of times';
List.RadioText[1, 1] := 'Calendar date';
List.RadioText[1, 2] := 'Time from beginning of run';
If Control.Fertilise_DateType = 'D' then
   List.RbtnSelected[1] := 0
Else
   List.RbtnSelected[1] := 1;
List.FileExt := 'fl!';
List.FileComment := 'Fertiliser sequence';
List.HelpContext := 8100;
List.TextBox := false;
List.HasChanged := false;
List.RedrawOption := false;
End; {of Procedure 'SetUpFertiliserDialogue'}

Procedure GetFertiliserDialogueInfo;
var iRow, iCount2, iCount3: integer;
    fDate1, fDate2: integer;
Begin
Event.nFertilisations := List.nRows;
If List.RbtnSelected[1] = 0 then
   Control.Fertilise_DateType := 'D'
Else
   Control.Fertilise_DateType := 'B';
For iRow := 1 to List.nRows do
    Begin
    Event.FertiliseTimes[iRow, 1] := Round(List.Data[iRow, 0]);
    Event.FertiliseTimes[iRow, 2] := Round(List.Data[iRow, 1]);
    Event.FertiliseTimes[iRow, 3] := Round(List.Data[iRow, 2]);
    Event.FertiliseAmount[iRow, N] := List.Data[iRow, 3];
    if Control.IncludeP then
       Event.FertiliseAmount[iRow, P] := List.Data[iRow, 4];
    End;
// now sort them based on start time - use BubbleSort method
For iRow := 1 to List.nRows - 1 do
    Begin
    For iCount2 := List.nRows - 1 downto iRow do
        Begin
        fDate1 := 365 * Event.FertiliseTimes[iCount2, 3] + 30 * Event.FertiliseTimes[iCount2, 2] + Event.FertiliseTimes[iCount2, 1];
        fDate2 := 365 * Event.FertiliseTimes[iCount2 + 1, 3] + 30 * Event.FertiliseTimes[iCount2 + 1, 2] + Event.FertiliseTimes[iCount2 + 1, 1];
        if (fDate1 > fDate2) then
           Begin
           For iCount3 := 1 to 3 do
               SwapLong(Event.FertiliseTimes[iCount2, iCount3], Event.FertiliseTimes[iCount2 + 1, iCount3]);
           SwapReal(Event.FertiliseAmount[iCount2, N], Event.FertiliseAmount[iCount2 + 1, N]);
           SwapReal(Event.FertiliseAmount[iCount2, P], Event.FertiliseAmount[iCount2 + 1, P]);
           End;
        End;
    End;
Control.ProjectHasChanged := true;
End; {of Procedure 'GetFertiliserDialogueInfo'}

Procedure SetUpEnvironmentDialogue;
var iRow, iDays, iMonths, iYears: Integer;
Begin
// Event Manager - environment changes
List.Caption := 'Change weather conditions';
List.nEntries := 7;
List.MaxRows := MaxEnvironmentEvents;
List.StrOptions := 0;
List.Header := 1;
List.DataType[0] := I;
List.DataType[1] := I;
List.DataType[2] := R;
List.DataType[3] := R;
List.DataType[4] := R;
List.DataType[5] := R;
List.DataType[6] := R;
List.Width[0] := 70;
List.Width[1] := 70;
List.Width[2] := 70;
List.Width[3] := 70;
List.Width[4] := 70;
List.Width[5] := 70;
List.Width[6] := 70;
List.Text[1, 0] := 'Month';
List.Text[1, 1] := 'Year';
List.Text[1, 2] := 'CO2';
List.Text[1, 3] := 'Temperature';
List.Text[1, 4] := 'Precipitation';
List.Text[1, 5] := 'Vapour pressure';
List.Text[1, 6] := 'Radiation';
List.nRows := Event.nEnvironments;
For iRow := 1 to List.nRows do
    Begin
    iDays := Event.EnvironmentTimes[iRow];
    iYears := Trunc((0.5 + iDays) / 365.25);
    iDays := Trunc(iDays - iYears * 365.25);
    iMonths := iDays div 30;
    List.Data[iRow, 0] := iMonths;
    List.Data[iRow, 1] := iYears;
    List.Data[iRow, 2] := Event.CO2[iRow];
    List.Data[iRow, 3] := Event.Temperature[iRow];
    List.Data[iRow, 4] := Event.Rainfall[iRow];
    List.Data[iRow, 5] := Event.VP[iRow];
    List.Data[iRow, 6] := Event.Radn[iRow];
    End;
List.TextBox := false;
List.nRadios := 1;
List.RadioOptions[1] := 2;
List.RadioHeading[1] := 'Adjustment of VP';
List.RadioText[1, 1] := 'Adjust VP with T';
List.RadioText[1, 2] := 'No auto adjustment';
If Event.AdjustVP then
   List.RbtnSelected[1] := 0
Else
   List.RbtnSelected[1] := 1;
List.FileExt := 'en!';
List.FileComment := 'Environment sequence';
List.HelpContext := 8300;
List.TextBox := false;
List.HasChanged := false;
List.RedrawOption := false;
End; {of Procedure 'SetUpEnvironmentDialogue'}

Procedure GetEnvironmentDialogueInfo;
var iRow, iMonth, iYear, iDay, iCount2: integer;
    fDate1, fDate2: TDateTime;
Begin
Event.nEnvironments := List.nRows;
For iRow := 1 to List.nRows do
    Begin
    iMonth := Round(List.Data[iRow, 0]);
    iYear := Round(List.Data[iRow, 1]);
    iDay := Round(365.25 * iYear + 30.4375 * iMonth);
    Event.EnvironmentTimes[iRow] := iDay;
    Event.CO2[iRow] := List.Data[iRow, 2];
    Event.Temperature[iRow] := List.Data[iRow, 3];
    Event.Rainfall[iRow] := List.Data[iRow, 4];
    if List.Data[iRow, 5] >= 0 then
    // Extra safeguard if null values are entered (especially by reading old *.en! files
       Event.VP[iRow] := List.Data[iRow, 5]
    Else
       Event.VP[iRow] := 1;
    if List.Data[iRow, 6] >= 0 then
    // Extra safeguard if null values are entered (especially by reading old *.en! files
       Event.Radn[iRow] := List.Data[iRow, 6]
    Else
       Event.Radn[iRow] := 1;
    End;
// now sort them based on start time - use BubbleSort method
For iRow := 1 to List.nRows - 1 do
    Begin
    For iCount2 := List.nRows - 1 downto iRow do
        Begin
        fDate1 := Event.EnvironmentTimes[iCount2];
        fDate2 := Event.EnvironmentTimes[iCount2 + 1];
        if (fDate1 > fDate2) then
           Begin
           SwapLong(Event.EnvironmentTimes[iCount2], Event.EnvironmentTimes[iCount2 + 1]);
           SwapReal(Event.CO2[iCount2], Event.CO2[iCount2 + 1]);
           SwapReal(Event.Temperature[iCount2], Event.Temperature[iCount2 + 1]);
           SwapReal(Event.Rainfall[iCount2], Event.Rainfall[iCount2 + 1]);
           SwapReal(Event.VP[iCount2], Event.VP[iCount2 + 1]);
           SwapReal(Event.Radn[iCount2], Event.Radn[iCount2 + 1]);
           End;
        End;
    End;
If List.RbtnSelected[1] = 0 then
   Event.AdjustVP := true
Else
   Event.AdjustVP := false;
Control.ProjectHasChanged := true;
End; {of Procedure 'GetEnvironmentDialogueInfo'}

Procedure SetUpPloughDialogue;
var iRow, iMonths, iYears: Integer;
    iDays: integer;
Begin
// Event Manager - ploughing
List.Caption := 'Times and characteristics of ploughing';
List.nEntries := 4;
List.MaxRows := MaxPloughEvents;
List.StrOptions := 0;
List.Header := 1;
List.DataType[0] := I;
List.DataType[1] := I;
List.DataType[2] := I;
List.DataType[3] := R;
List.Width[0] := 60;
List.Width[1] := 60;
List.Width[2] := 60;
List.Width[3] := 60;
List.Text[1, 0] := 'Month';
List.Text[1, 1] := 'Year';
List.Text[1, 2] := 'Plough depth';
List.Text[1, 3] := 'Disturbance';
List.nRows := Event.nPloughing;
For iRow := 1 to List.nRows do
    Begin
    If (Event.PloughTimes[iRow, 1] = 0) and (Event.PloughTimes[iRow, 2] = 0) and (Event.PloughTimes[iRow, 3] = 0) then
       Begin
       iDays := Event.PloughTimes[iRow, 4];
       iYears := Trunc((0.5 + iDays) / 365.25);
       iDays := Trunc(iDays - iYears * 365.25);
       iMonths := iDays div 30;
       End
    Else
       Begin
       iMonths := Event.PloughTimes[iRow, 2];
       iYears := Event.PloughTimes[iRow, 3];;
       End;
    List.Data[iRow, 0] := iMonths;
    List.Data[iRow, 1] := iYears;
    List.Data[iRow, 2] := Event.PloughDepth[iRow];
    List.Data[iRow, 3] := Event.Disturbance[iRow];
    End;
List.nRadios := 1;
List.RadioOptions[1] := 2;
List.RadioHeading[1] := 'Meaning of times';
List.RadioText[1, 1] := 'Calendar date';
List.RadioText[1, 2] := 'Time from beginning of run';
If (Control.Plough_DateType = 'D') then
   List.RbtnSelected[1] := 0
Else
   List.RbtnSelected[1] := 1;
List.FileExt := 'pg!';
List.FileComment := 'Plough sequence';
List.HelpContext := 8700;
List.TextBox := true;
List.TextBoxCaption := 'Decay of disturbance stimulation of OM decomposition (months)';
List.TextBoxEntry := Divide(1, (30.5 * (1 - Event.PloughDisturbanceRecovery)));
List.HasChanged := false;
List.RedrawOption := false;
End; {of Procedure 'SetUpPloughDialogue'}

Procedure GetPloughDialogueInfo;
var iRow, iMonth, iYear, iDay, iCount2: integer;
    fDate1, fDate2: TDateTime;
Begin
Event.nPloughing := List.nRows;
For iRow := 1 to List.nRows do
    Begin
    iMonth := Round(List.Data[iRow, 0]);
    iYear := Round(List.Data[iRow, 1]);
    iDay := Round(365.25 * iYear + 30.4375 * iMonth);
    Event.PloughTimes[iRow, 1] := 1;
    Event.PloughTimes[iRow, 2] := iMonth;
    Event.PloughTimes[iRow, 3] := iYear;
    Event.PloughTimes[iRow, 4] := iDay;
    Event.PloughDepth[iRow] := Round(List.Data[iRow, 2]);
    Event.Disturbance[iRow] := List.Data[iRow, 3];
    End;
If List.RbtnSelected[1] = 0 then
   Control.Plough_DateType := 'D'
Else
   Control.Plough_DateType := 'B';
If List.TextBoxEntry < 0 then
   List.TextBoxEntry := -List.TextBoxEntry;
Event.PloughDisturbanceRecovery := 1 - Divide(1, (30.5 * List.TextBoxEntry));
If List.TextBoxEntry < 0 then
   List.TextBoxEntry := -List.TextBoxEntry;
If Event.PloughDisturbanceRecovery > 1 then
   Event.PloughDisturbanceRecovery := 1
Else if Event.PloughDisturbanceRecovery < 0 then
   Event.PloughDisturbanceRecovery := 0;
// now sort them based on start time - use BubbleSort method
For iRow := 1 to List.nRows - 1 do
    Begin
    For iCount2 := List.nRows - 1 downto iRow do
        Begin
        fDate1 := Event.PloughTimes[iCount2, 4];
        fDate2 := Event.PloughTimes[iCount2 + 1, 4];
        If (fDate1 > fDate2) then
           Begin
           SwapLong(Event.PloughTimes[iCount2, 1], Event.PloughTimes[iCount2 + 1, 1]);
           SwapLong(Event.PloughTimes[iCount2, 2], Event.PloughTimes[iCount2 + 1, 2]);
           SwapLong(Event.PloughTimes[iCount2, 3], Event.PloughTimes[iCount2 + 1, 3]);
           SwapLong(Event.PloughTimes[iCount2, 4], Event.PloughTimes[iCount2 + 1, 4]);
           SwapLong(Event.PloughDepth[iCount2], Event.PloughDepth[iCount2 + 1]);
           SwapReal(Event.Disturbance[iCount2], Event.Disturbance[iCount2 + 1]);
           End;
        End;
    End;
Control.ProjectHasChanged := true;
End; {of Procedure 'GetPloughDialogueInfo'}

Procedure SetUpOMAdditionsDialogue;
var iRow, iMonths, iYears: Integer;
    iDays: integer;
Begin
// Event Manager - OMAdditions
List.Caption := 'Times and characteristics of OMAdditions';
List.MaxRows := MaxOMAdditions;
List.StrOptions := 0;
List.Header := 1;
List.DataType[0] := I;
List.DataType[1] := I;
List.DataType[2] := R;
List.DataType[3] := R;
List.DataType[4] := R;
List.DataType[5] := R;
List.DataType[6] := R;
List.Width[0] := 60;
List.Width[1] := 60;
List.Width[2] := 70;
List.Width[3] := 70;
List.Width[4] := 70;
List.Width[5] := 70;
List.Width[6] := 70;
List.Text[1, 0] := 'Month';
List.Text[1, 1] := 'Year';
List.Text[1, 2] := 'C (t ha-1)';
List.Text[1, 3] := 'N (kg ha-1)';
if Control.IncludeP then
   Begin
   List.nEntries := 7;
   List.Text[1, 4] := 'P (kg ha-1)';
   List.Text[1, 5] := 'Water (t ha-1)';
   List.Text[1, 6] := 'Lignin (%)';
   End
Else
   Begin
   List.nEntries := 6;
   List.Text[1, 4] := 'Water (t ha-1)';
   List.Text[1, 5] := 'Lignin (%)';
   End;
List.nRows := Event.nOMAdditions;
For iRow := 1 to List.nRows do
    Begin
    If (Event.OMAdditionTimes[iRow, 1] = 0) and (Event.OMAdditionTimes[iRow, 2] = 0) and (Event.OMAdditionTimes[iRow, 3] = 0) then
       Begin
       iDays := Event.OMAdditionTimes[iRow, 4];
       iYears := Trunc((0.5 + iDays) / 365.25);
       iDays := Trunc(iDays - iYears * 365.25);
       iMonths := iDays div 30;
       End
    Else
       Begin
       iMonths := Event.OMAdditionTimes[iRow, 2];
       iYears := Event.OMAdditionTimes[iRow, 3];;
       End;
    List.Data[iRow, 0] := iMonths;
    List.Data[iRow, 1] := iYears;
    List.Data[iRow, 2] := Event.OMAddition[iRow, C] / 1000;
    List.Data[iRow, 3] := Event.OMAddition[iRow, N];
    if Control.IncludeP then
       Begin
       List.Data[iRow, 4] := Event.OMAddition[iRow, P];
       List.Data[iRow, 5] := 10 * Event.OMExtraH2O[iRow];
       List.Data[iRow, 6] := Event.OMLignin[iRow] * 100;
       End
    Else
       Begin
       List.Data[iRow, 4] := 10 * Event.OMExtraH2O[iRow];
       List.Data[iRow, 5] := Event.OMLignin[iRow] * 100;
       End
    End;
List.TextBox := false;
List.nRadios := 1;
List.RadioOptions[1] := 2;
List.RadioHeading[1] := 'Meaning of times';
List.RadioText[1, 1] := 'Calendar date';
List.RadioText[1, 2] := 'Time from beginning of run';
If (Control.OMAdditions_DateType = 'D') then
   List.RbtnSelected[1] := 0
Else
   List.RbtnSelected[1] := 1;
List.FileExt := 'pg!';
List.FileComment := 'OMAdditions sequence';
List.HelpContext := 8700;
List.TextBox := false;
List.HasChanged := false;
List.RedrawOption := false;

End;

Procedure GetOMAdditionsDialogueInfo;
var iRow, iMonth, iYear, iDay, iCount2: integer;
    fDate1, fDate2: TDateTime;
Begin
Event.nOMAdditions := List.nRows;
For iRow := 1 to List.nRows do
    Begin
    iMonth := Round(List.Data[iRow, 0]);
    iYear := Round(List.Data[iRow, 1]);
    iDay := Round(365.25 * iYear + 30.4375 * iMonth);
    Event.OMAdditionTimes[iRow, 1] := 1;
    Event.OMAdditionTimes[iRow, 2] := iMonth;
    Event.OMAdditionTimes[iRow, 3] := iYear;
    Event.OMAdditionTimes[iRow, 4] := iDay;
    Event.OMAddition[iRow, C] := List.Data[iRow, 2] * 1000;
    Event.OMAddition[iRow, N] := List.Data[iRow, 3];
    if Control.IncludeP then
       Begin
       Event.OMAddition[iRow, P] := List.Data[iRow, 4];
       Event.OMExtraH2O[iRow] := List.Data[iRow, 5] / 10;
       Event.OMLignin[iRow] := List.Data[iRow, 6] / 100;
       End
    Else // if not includeP
       Begin
       Event.OMExtraH2O[iRow] := List.Data[iRow, 4] / 10;
       Event.OMLignin[iRow] := List.Data[iRow, 5] / 100;
       End;
    End;
If List.RbtnSelected[1] = 0 then
   Control.OMAdditions_DateType := 'D'
Else
   Control.OMAdditions_DateType := 'B';
// now sort them based on start time - use BubbleSort method
For iRow := 1 to List.nRows - 1 do
    Begin
    For iCount2 := List.nRows - 1 downto iRow do
        Begin
        fDate1 := Event.OMAdditionTimes[iCount2, 4];
        fDate2 := Event.OMAdditionTimes[iCount2 + 1, 4];
        If (fDate1 > fDate2) then
           Begin
           SwapLong(Event.OMAdditionTimes[iCount2, 1], Event.OMAdditionTimes[iCount2 + 1, 1]);
           SwapLong(Event.OMAdditionTimes[iCount2, 2], Event.OMAdditionTimes[iCount2 + 1, 2]);
           SwapLong(Event.OMAdditionTimes[iCount2, 3], Event.OMAdditionTimes[iCount2 + 1, 3]);
           SwapLong(Event.OMAdditionTimes[iCount2, 4], Event.OMAdditionTimes[iCount2 + 1, 4]);
           Swapreal(Event.OMAddition[iCount2,C], Event.OMAddition[iCount2 + 1,C]);
           Swapreal(Event.OMAddition[iCount2,N], Event.OMAddition[iCount2 + 1,N]);
           Swapreal(Event.OMAddition[iCount2,P], Event.OMAddition[iCount2 + 1,P]);
           Swapreal(Event.OMExtraH2O[iCount2], Event.OMExtraH2O[iCount2 + 1]);
           Swapreal(Event.OMLignin[iCount2], Event.OMLignin[iCount2 + 1]);
           End;
        End;
    End;
Control.ProjectHasChanged := true;
End; {of Procedure 'GetOMAdditionsDialogueInfo'}

Procedure SetUpSeedingDialogue;
var iRow, iMonths, iYears: Integer;
    iDays: integer;
Begin
// Event Manager - Seeding
List.Caption := 'Times and characteristics of Seeding (new plants)';
List.MaxRows := MaxSeedings;
List.StrOptions := 0;
List.Header := 1;
List.DataType[0] := I;
List.DataType[1] := I;
List.DataType[2] := I;
List.DataType[3] := I;
List.DataType[4] := R;
List.DataType[5] := R;
List.DataType[6] := R;
List.Width[0] := 60;
List.Width[1] := 60;
List.Width[2] := 70;
List.Width[3] := 70;
List.Width[4] := 70;
List.Width[5] := 70;
List.Width[6] := 70;
List.Text[1, 0] := 'Day';
List.Text[1, 1] := 'Month';
List.Text[1, 2] := 'Year';
List.Text[1, 3] := 'New plants (n ha-1)';
List.Text[1, 4] := 'C (kg ha-1)';
List.Text[1, 5] := 'N (kg ha-1)';
List.Text[1, 6] := 'P (kg ha-1)';
if Control.IncludeP then
   Begin
   List.nEntries := 7;
   List.Text[1, 6] := 'P (kg ha-1)';
   End
Else
   List.nEntries := 6;
List.nRows := Event.nSeedings;
For iRow := 1 to List.nRows do
    Begin
    If (Event.SeedingTimes[iRow, 1] = 0) and (Event.SeedingTimes[iRow, 2] = 0) and (Event.SeedingTimes[iRow, 3] = 0) then
       Begin
       iDays := Event.SeedingTimes[iRow, 4];
       iYears := Trunc((0.5 + iDays) / 365.25);
       iDays := Trunc(iDays - iYears * 365.25);
       iMonths := iDays div 30;
       End
    Else
       Begin
       iDays := Event.SeedingTimes[iRow, 1];
       iMonths := Event.SeedingTimes[iRow, 2];
       iYears := Event.SeedingTimes[iRow, 3];;
       while iDays > 30 do
           Begin
           iMonths := iMonths + 1;
           iDays := iDays - 30;
           End;
       while iMonths > 12 do
           Begin
           iYears := iYears + 1;
           iMonths := iMonths - 12;
           End;
       End;
    List.Data[iRow, 0] := iDays;
    List.Data[iRow, 1] := iMonths;
    List.Data[iRow, 2] := iYears;
    List.Data[iRow, 3] := Round(Event.PlantsAdded[iRow]);
    List.Data[iRow, 4] := Event.BiomassAdded[iRow, C];
    List.Data[iRow, 5] := Event.BiomassAdded[iRow, N];
    if Control.IncludeP then
       List.Data[iRow, 6] := Event.BiomassAdded[iRow, P];
    End;
List.TextBox := false;
List.nRadios := 1;
List.RadioOptions[1] := 2;
List.RadioHeading[1] := 'Meaning of times';
List.RadioText[1, 1] := 'Calendar date';
List.RadioText[1, 2] := 'Time from beginning of run';
If (Control.Seedings_DateType = 'D') then
   List.RbtnSelected[1] := 0
Else
   List.RbtnSelected[1] := 1;
List.FileExt := 'sd!';
List.FileComment := 'Seedings sequence';
List.HelpContext := 8720;
List.TextBox := false;
List.HasChanged := false;
List.RedrawOption := false;
End; {of Procedure 'SetUpSeedingDialogue'}

Procedure GetSeedingDialogueInfo;
var iRow, iDay, iMonth, iYear, AllDays, iCount2: integer;
    fDate1, fDate2: TDateTime;
Begin
Event.nSeedings := List.nRows;
For iRow := 1 to List.nRows do
    Begin
    iDay := Round(List.Data[iRow, 0]);
    iMonth := Round(List.Data[iRow, 1]);
    iYear := Round(List.Data[iRow, 2]);
    AllDays := Round(365.25 * iYear + 30.4375 * iMonth + iDay);
    Event.SeedingTimes[iRow, 1] := iDay;
    Event.SeedingTimes[iRow, 2] := iMonth;
    Event.SeedingTimes[iRow, 3] := iYear;
    Event.SeedingTimes[iRow, 4] := AllDays;
    Event.PlantsAdded[iRow] := List.Data[iRow, 3];
    Event.BiomassAdded[iRow, C] := List.Data[iRow, 4];
    Event.BiomassAdded[iRow, N] := List.Data[iRow, 5];
    if Control.IncludeP then
       Event.BiomassAdded[iRow, P] := List.Data[iRow, 6];
    End;
If List.RbtnSelected[1] = 0 then
   Control.Seedings_DateType := 'D'
Else
   Control.Seedings_DateType := 'B';
// now sort them based on start time - use BubbleSort method
For iRow := 1 to List.nRows - 1 do
    Begin
    For iCount2 := List.nRows - 1 downto iRow do
        Begin
        fDate1 := Event.SeedingTimes[iCount2, 4];
        fDate2 := Event.SeedingTimes[iCount2 + 1, 4];
        If (fDate1 > fDate2) then
           Begin
           SwapLong(Event.SeedingTimes[iCount2, 1], Event.SeedingTimes[iCount2 + 1, 1]);
           SwapLong(Event.SeedingTimes[iCount2, 2], Event.SeedingTimes[iCount2 + 1, 2]);
           SwapLong(Event.SeedingTimes[iCount2, 3], Event.SeedingTimes[iCount2 + 1, 3]);
           SwapLong(Event.SeedingTimes[iCount2, 4], Event.SeedingTimes[iCount2 + 1, 4]);
           Swapreal(Event.PlantsAdded[iCount2], Event.PlantsAdded[iCount2 + 1]);
           Swapreal(Event.BiomassAdded[iCount2, C], Event.BiomassAdded[iCount2 + 1, C]);
           Swapreal(Event.BiomassAdded[iCount2, N], Event.BiomassAdded[iCount2 + 1, N]);
           Swapreal(Event.BiomassAdded[iCount2, P], Event.BiomassAdded[iCount2 + 1, P]);
           End;
        End;
    End;
Control.ProjectHasChanged := true;
End; {of Procedure 'GetSeedingDialogueInfo'}

Procedure SetUpGrazingDialogue;
var iRow, iDays, iMonths, iYears: Integer;
    LongDays: integer;
Begin
// Event Manager - Grazing
List.Caption := 'Times and characteristics of animal grazing';
List.MaxRows := MaxGrazings;
List.StrOptions := 0;
List.Header := 2;
List.DataType[0] := I;
List.DataType[1] := I;
List.DataType[2] := I;
List.DataType[3] := R;
List.DataType[4] := R;
List.DataType[5] := R;
List.DataType[6] := R;
List.DataType[7] := R;
List.DataType[8] := R;
List.DataType[9] := R;
List.DataType[10] := R;
List.Width[0] := 40;
List.Width[1] := 40;
List.Width[2] := 40;
List.Width[3] := 70;
List.Width[4] := 80;
List.Width[5] := 80;
List.Width[6] := 70;
List.Width[7] := 70;
List.Width[8] := 70;
List.Width[9] := 70;
List.Width[10] := 70;
List.Width[11] := 70;
List.Text[1, 0] := 'Day';
List.Text[1, 1] := 'Month';
List.Text[1, 2] := 'Year';
List.Text[1, 3] := 'DW grazed (%)';
List.Text[1, 4] := 'DW suppl. (kgDW ha-1)';
List.Text[1, 5] := 'Nitro suppl. (kgN ha-1)';
List.Text[1, 6] := 'C fract removed (%)';
List.Text[1, 7] := 'C fract respired (%)';
List.Text[1, 8] := 'C fract to CH4 (%)';
List.Text[1, 9] := 'N fract removed (%)';
List.Text[1, 10] := 'N fract leached (%)';
List.Text[1, 11] := 'Phosph. suppl. (kgP ha-1)';
List.Text[1, 12] := 'P fract removed (%)';
List.Text[2, 0] := 'Day';
List.Text[2, 1] := 'Month';
List.Text[2, 2] := 'Year';
List.Text[2, 3] := 'DW grazed (kgDW ha-1)';
List.Text[2, 4] := 'DW suppl. (kgDW ha-1)';
List.Text[2, 5] := 'Nitro suppl. (kgN ha-1)';
List.Text[2, 6] := 'C fract removed (%)';
List.Text[2, 7] := 'C fract respired (%)';
List.Text[2, 8] := 'C fract to CH4 (%)';
List.Text[2, 9] := 'N fract removed (%)';
List.Text[2, 10] := 'N fract leached (%)';
List.Text[2, 11] := 'Phosph. suppl. (kgP ha-1)';
List.Text[2, 12] := 'P fract removed (%)';
if Control.IncludeP then
   List.nEntries := 13
Else
   List.nEntries := 11;
If Event.GrazingUnits = '%' then
   List.Header := 1
Else //   If Event.GrazingUnits = 'W' then
   List.Header := 2;
List.nRows := Event.nGrazings;
For iRow := 1 to List.nRows do
    Begin
    If (Event.GrazingTimes[iRow, 1] = 0) and (Event.GrazingTimes[iRow, 2] = 0) and (Event.GrazingTimes[iRow, 3] = 0) then
       Begin
       iYears := Trunc((0.5 + iDays) / 365.25);
       iDays := Trunc(iDays - iYears * 365.25);
       iMonths := iDays div 30;
       iDays := iDays mod 30;
       End
    Else
       Begin
       iDays := Event.GrazingTimes[iRow, 1];
       iMonths := Event.GrazingTimes[iRow, 2];
       iYears := Event.GrazingTimes[iRow, 3];
       while iDays > 30 do
           Begin
           iMonths := iMonths + 1;
           iDays := iDays - 30;
           End;
       while iMonths > 12 do
           Begin
           iYears := iYears + 1;
           iMonths := iMonths - 12;
           End;
       End;
    List.Data[iRow, 0] := iDays;
    List.Data[iRow, 1] := iMonths;
    List.Data[iRow, 2] := iYears;
    if Event.GrazingUnits = '%' then
       List.Data[iRow, 3] := Event.GrazingAmount[iRow] * 100
    Else
       List.Data[iRow, 3] := Event.GrazingAmount[iRow];
    List.Data[iRow, 4] := Event.GrazingSupplement[iRow, C];
    List.Data[iRow, 5] := Event.GrazingSupplement[iRow, N];
    List.Data[iRow, 6] := Event.GrazingFractionRemoved[iRow, C] * 100;
    List.Data[iRow, 7] := Event.GrazingFractionRespired[iRow] * 100;
    List.Data[iRow, 8] := Event.GrazingMethaneFraction[iRow] * 100;
    List.Data[iRow, 9] := Event.GrazingFractionRemoved[iRow, N] * 100;
    List.Data[iRow, 10] := Event.GrazingFractionLeached[iRow, N] * 100;
    if Control.IncludeP then
       Begin
       List.Data[iRow, 11] := Event.GrazingSupplement[iRow, P];
       List.Data[iRow, 12] := Event.GrazingFractionRemoved[iRow, P] * 100;
       End;
    End;
List.nRadios := 1;
List.RadioOptions[1] := 2;
List.RadioHeading[1] := 'Grazing expressed as:';
If (Event.GrazingUnits = '%') then
   List.RbtnSelected[1] := 0
Else {if Event.GrazingUnits = 'W' then}
   List.RbtnSelected[1] := 1;
List.RadioText[1, 1] := 'Daily fraction grazed';
List.RadioText[1, 2] := 'Daily amount grazed';
List.FileExt := 'gz!';
List.FileComment := 'Grazing sequence';
List.HelpContext := 8750;
List.TextBox := false;
List.HasChanged := false;
List.RedrawOption := false;
End; {of 'SetUpGrazingDialogue'}

Procedure GetGrazingDialogueInfo;
var iRow, iMonth, iYear, iDay, iCount2: integer;
    LongDay: integer;
    fDate1, fDate2: TDateTime;
Begin
Event.nGrazings := List.nRows;
For iRow := 1 to List.nRows do
    Begin
    iDay := Round(List.Data[iRow, 0]);
    iMonth := Round(List.Data[iRow, 1]);
    iYear := Round(List.Data[iRow, 2]);
    LongDay := Round(365.25 * iYear + 30.4375 * iMonth + iDay);
    Event.GrazingTimes[iRow, 1] := iDay;
    Event.GrazingTimes[iRow, 2] := iMonth;
    Event.GrazingTimes[iRow, 3] := iYear;
    Event.GrazingTimes[iRow, 4] := LongDay;
    If Event.GrazingUnits = '%' then
       Event.GrazingAmount[iRow] := List.Data[iRow, 3] * 0.01
    Else
       Event.GrazingAmount[iRow] := List.Data[iRow, 3];
    Event.GrazingSupplement[iRow, C] := List.Data[iRow, 4];
    Event.GrazingSupplement[iRow, N] := List.Data[iRow, 5];
    Event.GrazingFractionRemoved[iRow, C] := List.Data[iRow, 6] * 0.01;
    Event.GrazingFractionRespired[iRow] := List.Data[iRow, 7] * 0.01;
    Event.GrazingMethaneFraction[iRow] := List.Data[iRow, 8] * 0.01;
    Event.GrazingFractionRemoved[iRow, N] := List.Data[iRow, 9] * 0.01;
    Event.GrazingFractionLeached[iRow, N] := List.Data[iRow, 10] * 0.01;
    if Control.IncludeP then
       Begin
       Event.GrazingSupplement[iRow, P] := List.Data[iRow, 11];
       Event.GrazingFractionRemoved[iRow, P] := List.Data[iRow, 12] * 0.01;
       End
    Else
       Begin
       if Event.GrazingSupplement[iRow, P] = 0 then      // if the P cycle is not used and the current value is 0, hence probably no previous value been input
          Event.GrazingSupplement[iRow, P] := Event.GrazingSupplement[iRow, N] * 0.1;      // use a default N:P ratio of 10 just to have a value in case the user later decides to include the P cycle
       if Event.GrazingFractionRemoved[iRow, P] = 0 then // if the P cycle is not used and the current value is 0, hence probably no previous value been input
          Event.GrazingFractionRemoved[iRow, P] := Event.GrazingFractionRemoved[iRow, N];  // default setting if we laetr decide to include P
       End;
    End;
If List.RbtnSelected[1] = 0 then
   Event.GrazingUnits := '%'
Else
   Event.GrazingUnits := 'W';
// now sort them based on start time - use BubbleSort method
For iRow := 1 to List.nRows - 1 do
    Begin
    For iCount2 := List.nRows - 1 downto iRow do
        Begin
        fDate1 := Event.GrazingTimes[iCount2, 4];
        fDate2 := Event.GrazingTimes[iCount2 + 1, 4];
        If (fDate1 > fDate2) then
           Begin
           SwapLong(Event.GrazingTimes[iCount2, 1], Event.GrazingTimes[iCount2 + 1, 1]);
           SwapLong(Event.GrazingTimes[iCount2, 2], Event.GrazingTimes[iCount2 + 1, 2]);
           SwapLong(Event.GrazingTimes[iCount2, 3], Event.GrazingTimes[iCount2 + 1, 3]);
           SwapLong(Event.GrazingTimes[iCount2, 4], Event.GrazingTimes[iCount2 + 1, 4]);
           SwapReal(Event.GrazingAmount[iCount2], Event.GrazingAmount[iCount2 + 1]);
           SwapReal(Event.GrazingSupplement[iCount2, C], Event.GrazingSupplement[iCount2 + 1, C]);
           SwapReal(Event.GrazingSupplement[iCount2, N], Event.GrazingSupplement[iCount2 + 1, N]);
           SwapReal(Event.GrazingSupplement[iCount2, P], Event.GrazingSupplement[iCount2 + 1, P]);
           SwapReal(Event.GrazingFractionRespired[iCount2], Event.GrazingFractionRespired[iCount2 + 1]);
           SwapReal(Event.GrazingMethaneFraction[iCount2], Event.GrazingMethaneFraction[iCount2 + 1]);
           SwapReal(Event.GrazingFractionRemoved[iCount2, C], Event.GrazingFractionRemoved[iCount2 + 1, C]);
           SwapReal(Event.GrazingFractionRemoved[iCount2, N], Event.GrazingFractionRemoved[iCount2 + 1, N]);
           SwapReal(Event.GrazingFractionRemoved[iCount2, P], Event.GrazingFractionRemoved[iCount2 + 1, P]);
           SwapReal(Event.GrazingFractionLeached[iCount2, C], Event.GrazingFractionLeached[iCount2 + 1, C]);
           SwapReal(Event.GrazingFractionLeached[iCount2, N], Event.GrazingFractionLeached[iCount2 + 1, N]);
           SwapReal(Event.GrazingFractionLeached[iCount2, P], Event.GrazingFractionLeached[iCount2 + 1, P]);
           End;
        End;
    End;
Control.ProjectHasChanged := true;
End; {of Procedure 'GetGrazingDialogueInfo'}

Procedure SetUpGrazing2Dialogue;
var iRow, iDays, iMonths, iYears: Integer;
    LongDays: integer;
Begin
// Event Manager - Grazing using a set formula
List.Caption := 'Times to start and end animal grazing by formula';
List.MaxRows := MaxGrazings;
List.StrOptions := 0;
List.Header := 1;
List.DataType[0] := I;
List.DataType[1] := I;
List.DataType[2] := I;
List.DataType[3] := R;
List.DataType[4] := R;
List.DataType[5] := R;
List.DataType[6] := R;
List.DataType[7] := R;
List.DataType[8] := R;
List.DataType[9] := R;
List.DataType[10] := R;
List.Width[0] := 40;
List.Width[1] := 40;
List.Width[2] := 40;
List.Width[3] := 70;
List.Width[4] := 80;
List.Width[5] := 80;
List.Width[6] := 70;
List.Width[7] := 70;
List.Width[8] := 70;
List.Width[9] := 70;
List.Width[10] := 70;
List.Text[1, 0] := 'Day';
List.Text[1, 1] := 'Month';
List.Text[1, 2] := 'Year';
List.Text[1, 3] := 'DW to start grazing (kgDW ha-1)';
List.Text[1, 4] := 'DW left after grazing (kgDW ha-1)';
List.Text[1, 5] := 'C fract removed (%)';
List.Text[1, 6] := 'C fract respired (%)';
List.Text[1, 7] := 'C fract to CH4 (%)';
List.Text[1, 8] := 'N fract removed (%)';
List.Text[1, 9] := 'N fract leached (%)';
List.Text[1, 10] := 'P fract removed (%)';
if Control.IncludeP then
   List.nEntries := 11
Else
   List.nEntries := 10;
List.Header := 1;
List.nRows := Event.nGrazings2;
For iRow := 1 to List.nRows do
    Begin
    If (Event.Grazing2Times[iRow, 1] = 0) and (Event.Grazing2Times[iRow, 2] = 0) and (Event.Grazing2Times[iRow, 3] = 0) then
       Begin
       iYears := Trunc((0.5 + iDays) / 365.25);
       iDays := Trunc(iDays - iYears * 365.25);
       iMonths := iDays div 30;
       iDays := iDays mod 30;
       End
    Else
       Begin
       iDays := Event.Grazing2Times[iRow, 1];
       iMonths := Event.Grazing2Times[iRow, 2];
       iYears := Event.Grazing2Times[iRow, 3];
       while iDays > 30 do
           Begin
           iMonths := iMonths + 1;
           iDays := iDays - 30;
           End;
       while iMonths > 12 do
           Begin
           iYears := iYears + 1;
           iMonths := iMonths - 12;
           End;
       End;
    List.Data[iRow, 0] := iDays;
    List.Data[iRow, 1] := iMonths;
    List.Data[iRow, 2] := iYears;
    List.Data[iRow, 3] := Event.Grazing2UpLimit[iRow];
    List.Data[iRow, 4] := Event.Grazing2LowLimit[iRow];
    List.Data[iRow, 5] := Event.Grazing2FractionRemoved[iRow, C] * 100;
    List.Data[iRow, 6] := Event.Grazing2FractionRespired[iRow] * 100;
    List.Data[iRow, 7] := Event.Grazing2MethaneFraction[iRow] * 100;
    List.Data[iRow, 8] := Event.Grazing2FractionRemoved[iRow, N] * 100;
    List.Data[iRow, 9] := Event.Grazing2FractionLeached[iRow, N] * 100;
    if Control.IncludeP then
       Begin
       List.Data[iRow, 10] := Event.Grazing2FractionRemoved[iRow, P] * 100;
       End;
    End;
List.nRadios := 0;
List.FileExt := 'gf!';
List.FileComment := 'Grazing sequence (formula)';
List.HelpContext := 8755;
List.TextBox := false;
List.HasChanged := false;
List.RedrawOption := false;
End; {of 'SetUpGrazing2Dialogue'}

Procedure GetGrazing2DialogueInfo;
var iRow, iMonth, iYear, iDay, iCount2: integer;
    LongDay: integer;
    fDate1, fDate2: TDateTime;
Begin
Event.nGrazings2 := List.nRows;
For iRow := 1 to List.nRows do
    Begin
    iDay := Round(List.Data[iRow, 0]);
    iMonth := Round(List.Data[iRow, 1]);
    iYear := Round(List.Data[iRow, 2]);
    LongDay := Round(365.25 * iYear + 30.4375 * iMonth + iDay);
    Event.Grazing2Times[iRow, 1] := iDay;
    Event.Grazing2Times[iRow, 2] := iMonth;
    Event.Grazing2Times[iRow, 3] := iYear;
    Event.Grazing2Times[iRow, 4] := LongDay;
    Event.Grazing2UpLimit[iRow] := List.Data[iRow, 3];
    Event.Grazing2LowLimit[iRow] := List.Data[iRow, 4];
    Event.Grazing2FractionRemoved[iRow, C] := List.Data[iRow, 5] * 0.01;
    Event.Grazing2FractionRespired[iRow] := List.Data[iRow, 6] * 0.01;
    Event.Grazing2MethaneFraction[iRow] := List.Data[iRow, 7] * 0.01;
    Event.Grazing2FractionRemoved[iRow, N] := List.Data[iRow, 8] * 0.01;
    Event.Grazing2FractionLeached[iRow, N] := List.Data[iRow, 9] * 0.01;
    if Control.IncludeP then
       Begin
       Event.Grazing2FractionRemoved[iRow, P] := List.Data[iRow, 10] * 0.01;
       End
    Else
       Begin
       if Event.Grazing2FractionRemoved[iRow, P] = 0 then // if the P cycle is not used and the current value is 0, hence probably no previous value been input
          Event.Grazing2FractionRemoved[iRow, P] := Event.GrazingFractionRemoved[iRow, N];  // default setting if we laetr decide to include P
       End;
    End;
// now sort them based on start time - use BubbleSort method
For iRow := 1 to List.nRows - 1 do
    Begin
    For iCount2 := List.nRows - 1 downto iRow do
        Begin
        fDate1 := Event.Grazing2Times[iCount2, 4];
        fDate2 := Event.Grazing2Times[iCount2 + 1, 4];
        If (fDate1 > fDate2) then
           Begin
           SwapLong(Event.Grazing2Times[iCount2, 1], Event.GrazingTimes[iCount2 + 1, 1]);
           SwapLong(Event.Grazing2Times[iCount2, 2], Event.GrazingTimes[iCount2 + 1, 2]);
           SwapLong(Event.Grazing2Times[iCount2, 3], Event.GrazingTimes[iCount2 + 1, 3]);
           SwapLong(Event.Grazing2Times[iCount2, 4], Event.GrazingTimes[iCount2 + 1, 4]);
           SwapReal(Event.Grazing2UpLimit[iCount2], Event.Grazing2UpLimit[iCount2 + 1]);
           SwapReal(Event.Grazing2LowLimit[iCount2], Event.Grazing2LowLimit[iCount2 + 1]);
           SwapReal(Event.Grazing2FractionRespired[iCount2], Event.Grazing2FractionRespired[iCount2 + 1]);
           SwapReal(Event.Grazing2MethaneFraction[iCount2], Event.Grazing2MethaneFraction[iCount2 + 1]);
           SwapReal(Event.Grazing2FractionRemoved[iCount2, C], Event.Grazing2FractionRemoved[iCount2 + 1, C]);
           SwapReal(Event.Grazing2FractionRemoved[iCount2, N], Event.Grazing2FractionRemoved[iCount2 + 1, N]);
           SwapReal(Event.Grazing2FractionRemoved[iCount2, P], Event.Grazing2FractionRemoved[iCount2 + 1, P]);
           SwapReal(Event.Grazing2FractionLeached[iCount2, C], Event.Grazing2FractionLeached[iCount2 + 1, C]);
           SwapReal(Event.Grazing2FractionLeached[iCount2, N], Event.Grazing2FractionLeached[iCount2 + 1, N]);
           SwapReal(Event.Grazing2FractionLeached[iCount2, P], Event.Grazing2FractionLeached[iCount2 + 1, P]);
           End;
        End;
    End;
Control.ProjectHasChanged := true;
End; {of Procedure 'GetGrazing2DialogueInfo'}

Procedure SetUpLitterDialogue;
var iRow: Integer;
Begin
// Event Manager - litter input
List.Caption := 'Set litter-input and OM-transfer rates';
List.nEntries := 4;
List.MaxRows := MaxSoilLayers;
List.StrOptions := 0;
List.Header := 1;
List.DataType[0] := R;
List.DataType[1] := R;
List.DataType[2] := R;
List.DataType[3] := R;
List.Width[0] := 80;
List.Width[1] := 80;
List.Width[2] := 80;
List.Width[3] := 80;
List.Text[1, 0] := 'Depth (cm)';
List.Text[1, 1] := 'Relative fine root input (%)';
List.Text[1, 2] := 'Relative coarse root input (%)';
List.Text[1, 3] := 'OM transfer (%/cm/yr)';
List.nRows := Soil.nLayers;
For iRow := 1 to List.nRows do
    Begin
    List.Data[iRow, 0] := Soil.WaterLayer[iRow].Depth;
    List.Data[iRow, 1] := 100 * Soil.FineRootLitterIn[iRow];
    List.Data[iRow, 2] := 100 * Soil.CoarseRootLitterIn[iRow];
    List.Data[iRow, 3] := 100 * Soil.OMTransfer[iRow];
    End;
List.TextBox := false;
List.nRadios := 0;
List.FileExt := 'lt!';
List.FileComment := 'Litter input and OM transfer by layer';
List.HelpContext := 3750;
List.HasChanged := false;
List.RedrawOption := true;
End; {of Procedure 'SetUpLitterDialogue'}

Procedure GetLitterDialogueInfo;
var iRow, iCount: integer;
    TotalOM, SumFineRoot, SumCoarseRoot: double;
Begin
Soil.nLayers := List.nRows;
For iRow := 1 to List.nRows do
    Begin
    Soil.WaterLayer[iRow].Depth := List.Data[iRow, 0];
    Soil.FineRootLitterIn[iRow] := 0.01 * List.Data[iRow, 1];
    Soil.CoarseRootLitterIn[iRow] := 0.01 * List.Data[iRow, 2];
    Soil.OMTransfer[iRow] := 0.01 * List.Data[iRow, 3];
    End;
TotalOM := 0;
// find the total input for the soil as a whole
For iCount := 1 to Soil.nLayers do
    TotalOM := TotalOM + Soil.FineRootLitterIn[iCount];
If (TotalOM <= 0) then
   For iCount := 1 to Soil.nLayers do
       Soil.FineRootLitterIn[iCount] := 1 / Soil.nLayers;
// make sure total extract is valid
If Control.AllOneLayer then
   Begin
   If Soil.FineRootLitterIn[1] = 0 then
      Soil.FineRootLitterIn[1] := 1;
   If Soil.CoarseRootLitterIn[1] = 0 then
      Soil.CoarseRootLitterIn[1] := 1;
   For iCount := Soil.nLayers downto 1 do
       Begin
       Soil.FineRootLitterIn[iCount] := Soil.FineRootLitterIn[iCount] / Soil.FineRootLitterIn[1];
       Soil.CoarseRootLitterIn[iCount] := Soil.CoarseRootLitterIn[iCount] / Soil.CoarseRootLitterIn[1];
       End;
   End
Else  // if not AllOneLayer
   Begin
   SumFineRoot := 0;
   SumCoarseRoot := 0;
   For iCount := 1 to Soil.nLayers do
       Begin
       SumFineRoot := SumFineRoot + Soil.FineRootLitterIn[iCount];
       SumCoarseRoot := SumCoarseRoot + Soil.CoarseRootLitterIn[iCount];
       End;
   For iCount := 1 to Soil.nLayers do
       Begin
       If SumFineRoot > 0 then
          Soil.FineRootLitterIn[iCount] := Soil.FineRootLitterIn[iCount] / SumFineRoot
       Else
          Soil.FineRootLitterIn[iCount] := 1 / Soil.nLayers;
       If SumCoarseRoot > 0 then
          Soil.CoarseRootLitterIn[iCount] := Soil.CoarseRootLitterIn[iCount] / SumCoarseRoot
       Else
          Soil.CoarseRootLitterIn[iCount] := 1 / Soil.nLayers;
       End;
   End;
Control.SiteHasChanged := true;
End; {of Procedure 'GetLitterDialogueInfo'}

Procedure SetUpSoilWaterDialogue;
var iRow: Integer;
Begin
// Event Manager - Soil water
List.Caption := 'Set soil water properties';
If Soil.SeparateSensitivity then
   List.nEntries := 6
Else
   List.nEntries := 5;
List.MaxRows := MaxSoilLayers;
List.StrOptions := 0;
List.Header := 1;
List.DataType[0] := R;
List.DataType[1] := R;
List.DataType[2] := R;
List.DataType[3] := R;
List.DataType[4] := R;
List.DataType[5] := R;
List.Width[0] := 80;
List.Width[1] := 80;
List.Width[2] := 80;
List.Width[3] := 80;
List.Width[4] := 80;
List.Width[5] := 80;
List.Text[1, 0] := 'Depth (cm)';
List.Text[1, 1] := 'Water holding capacity (%)';
List.Text[1, 2] := 'Max water held (mm)';
List.Text[1, 3] := 'Relative water extraction';
If Soil.SeparateSensitivity then
   Begin
   List.Text[1, 4] := 'Stress sensitivity';
   List.Text[1, 5] := 'Relative soil evap';
   End
Else
   List.Text[1, 4] := 'Relative soil evap';
List.nRows := Soil.nLayers;
For iRow := 1 to List.nRows do
    Begin
    List.Data[iRow, 0] := Soil.WaterLayer[iRow].Depth;
    List.Data[iRow, 1] := Soil.WaterLayer[iRow].Pores;
    List.Data[iRow, 2] := Soil.WaterLayer[iRow].MaxWater;
    List.Data[iRow, 3] := Soil.WaterLayer[iRow].ExtractEffort;
    If Soil.SeparateSensitivity then
       Begin
       List.Data[iRow, 4] := Soil.WaterLayer[iRow].StressSensitivity;
       List.Data[iRow, 5] := Soil.WaterLayer[iRow].RelEvap;
       End
    Else
       List.Data[iRow, 4] := Soil.WaterLayer[iRow].RelEvap;
    End;
List.nRadios := 1;
List.RadioOptions[1] := 2;
List.RadioHeading[1] := 'Water-stress sensitivity';
List.RadioText[1, 1] := 'Different by layer';
List.RadioText[1, 2] := 'No separation by layers';
If Soil.SeparateSensitivity then
   List.RbtnSelected[1] := 0
Else
   List.RbtnSelected[1] := 1;
List.FileExt := 'sw!';
List.FileComment := 'Soil water properties by layer';
List.HelpContext := 3700;
List.TextBox := false;
List.HasChanged := false;
List.RedrawOption := true;
End; {of Procedure 'SetUpSoilWaterDialogue'}

Procedure GetSoilWaterDialogueInfo;
var iRow, iCount: integer;
    SumExtract, SumEvap, SumSensitivity: double;
    ToggleSensitivity: Boolean;
Begin
If ((List.RbtnSelected[1] = 0) and (Soil.SeparateSensitivity = false)) OR
   ((List.RbtnSelected[1] = 1) and (Soil.SeparateSensitivity = true)) then
   ToggleSensitivity := true
Else
   ToggleSensitivity := false;
If List.RbtnSelected[1] = 0 then
   Soil.SeparateSensitivity := true
Else
   Soil.SeparateSensitivity := false;
Soil.nLayers := List.nRows;
For iRow := 1 to List.nRows do
    Begin
    Soil.WaterLayer[iRow].Depth := List.Data[iRow, 0];
    Soil.WaterLayer[iRow].Pores := List.Data[iRow, 1];
    Soil.WaterLayer[iRow].MaxWater := List.Data[iRow, 2];
    Soil.WaterLayer[iRow].ExtractEffort := List.Data[iRow, 3];
    If Soil.SeparateSensitivity and Not ToggleSensitivity then // include sensitivity as just a normal column
       Begin
       Soil.WaterLayer[iRow].StressSensitivity := List.Data[iRow, 4];
       Soil.WaterLayer[iRow].RelEvap := List.Data[iRow, 5];
       End;
    If Not Soil.SeparateSensitivity and Not ToggleSensitivity then // don't include sensitivity and nothing has changed
       Soil.WaterLayer[iRow].RelEvap := List.Data[iRow, 4]
    Else if Soil.SeparateSensitivity and ToggleSensitivity then // include sensitivity for the first time. So, no data yet
       Soil.WaterLayer[iRow].RelEvap := List.Data[iRow, 4]
    Else if Not Soil.SeparateSensitivity and ToggleSensitivity then // no longer include sensitivity. Still use the existing data
       Begin
       Soil.WaterLayer[iRow].StressSensitivity := List.Data[iRow, 4];
       Soil.WaterLayer[iRow].RelEvap := List.Data[iRow, 5];
       End;
    End;
// work out soil water summary
Soil.MaxWater := 0;
// make sure relative evaporation rate decreases with depth
List.ShowMessage := false;
For iCount := 1 to Soil.nLayers -1 do
    If (Soil.WaterLayer[iCount].RelEvap * Soil.WaterLayer[iCount + 1].MaxWater) <
       (Soil.WaterLayer[iCount + 1].RelEvap * Soil.WaterLayer[iCount].MaxWater) then
       Begin
       Soil.WaterLayer[iCount + 1].RelEvap := Soil.WaterLayer[iCount].RelEvap *
       (Soil.WaterLayer[iCount + 1].MaxWater / Soil.WaterLayer[iCount].MaxWater);
       List.ShowMessage := true;
       List.Message := 'Invalid sequence for evaporation rate.' + chr(10) +
                       'After correction for water-holding capacity of each layer,' + chr(10) +
                       'upper layers must make a greater or equal relative contribution than lower layers' + chr(10) +
                       'The program is adjusting it!';
       End;
For iCount := 1 to Soil.nLayers do
    Soil.WaterLayer[iCount].MaxWater := Soil.WaterLayer[iCount].Depth * Soil.WaterLayer[iCount].Pores * 0.1;
// get the max maxwater and max extract
SumExtract := 0;
SumEvap := 0;
SumSensitivity := 0;
For iCount := 1 to Soil.nLayers do
    Begin
    Soil.MaxWater := Soil.MaxWater + Soil.WaterLayer[iCount].MaxWater;
    SumExtract := SumExtract + Soil.WaterLayer[iCount].ExtractEffort;
    SumEvap := SumEvap + Soil.WaterLayer[iCount].RelEvap;
    SumSensitivity := SumSensitivity + Soil.WaterLayer[iCount].StressSensitivity;
    End;
// make sure total water is valid
If (Soil.TotalWater > Soil.MaxWater) then
   Begin
   For iCount := 1 to Soil.nLayers do
       Soil.WaterLayer[iCount].WaterContent := Soil.WaterLayer[iCount].MaxWater;
   Soil.TotalWater := Soil.MaxWater;
   End
Else if (Soil.MaxWater = 0) then
   Begin
   For iCount:=1 to Soil.nLayers do
       Soil.WaterLayer[iCount].WaterContent := 0;
   Soil.TotalWater := 0;
   End
Else {TotalWater < Soil.MaxWater then}
   Begin
   For iCount:=1 to Soil.nLayers do
       Soil.WaterLayer[iCount].WaterContent := Soil.WaterLayer[iCount].MaxWater * Soil.TotalWater / Soil.MaxWater;
   Soil.TotalWater := Soil.TotalWater;
   End;
If SumExtract > 0 then
   Begin
   For iCount := 1 to Soil.nLayers do
       Soil.WaterLayer[iCount].ExtractEffort := Soil.WaterLayer[iCount].ExtractEffort / SumExtract;
   End
Else
   Begin
   For iCount := 1 to Soil.nLayers do
       Begin
       If Soil.nLayers = 1 then  {There has only one layer been defined}
          Soil.WaterLayer[iCount].ExtractEffort := 1
       Else if iCount = 1 then  {The top layer gets half}
          Soil.WaterLayer[iCount].ExtractEffort := 0.5
       Else if iCount = Soil.nLayers then  {The bottom layer gets the last bit}
          Soil.WaterLayer[iCount].ExtractEffort := Soil.WaterLayer[iCount - 1].ExtractEffort
       Else    {For the layers between the top and lowest ones, it decreases binomially}
          Soil.WaterLayer[iCount].ExtractEffort := 0.5 * Soil.WaterLayer[iCount - 1].ExtractEffort;
       End;
   End;
If SumEvap > 0 then
   Begin
   For iCount := 1 to Soil.nLayers do
       Soil.WaterLayer[iCount].RelEvap := Soil.WaterLayer[iCount].RelEvap / SumEvap;
   End
Else
   Begin
   Soil.WaterLayer[1].RelEvap := 1;
   For iCount := 2 to Soil.nLayers do
       Soil.WaterLayer[iCount].RelEvap := 0;
   End;
If SumSensitivity > 0 then
   Begin
   For iCount := 1 to Soil.nLayers do
       Soil.WaterLayer[iCount].StressSensitivity := Soil.WaterLayer[iCount].StressSensitivity / SumSensitivity;
   End
Else
   Begin
   For iCount := 1 to Soil.nLayers do
       Soil.WaterLayer[iCount].StressSensitivity := Soil.WaterLayer[iCount].MaxWater / Soil.MaxWater;
   End;
Control.SiteHasChanged := true;
End; {of Procedure 'GetSoilWaterDialogueInfo'}

Procedure SetUpPhenologyDialogue;
var iRow: Integer;
Begin
// Event Manager - Phenology parameters
List.Caption := 'Leaf-growth and senescence parameters';
List.nEntries := 6;
List.MaxRows := 365;
List.StrOptions := 0;
List.Header := 1;
List.DataType[0] := I;
List.DataType[1] := I;
List.DataType[2] := R;
List.DataType[3] := R;
List.DataType[4] := R;
List.DataType[5] := R;
List.Width[0] := 80;
List.Width[1] := 80;
List.Width[2] := 80;
List.Width[3] := 80;
List.Width[4] := 80;
List.Width[5] := 80;
List.Text[1, 0] := 'Day of the year';
List.Text[1, 1] := 'n days';
List.Text[1, 2] := 'Heat sum';
List.Text[1, 3] := 'Day length';
List.Text[1, 4] := 'Leaf fall   (% per day)';
List.Text[1, 5] := 'Leaf growth   (% per day)';
List.nRows := Parameter.Phenology.nChanges;
For iRow := 1 to List.nRows do
    Begin
    If Parameter.Phenology.JulianDay[iRow] = 0 then
       List.Data[iRow, 0] := -1
    Else
       List.Data[iRow, 0] := Parameter.Phenology.JulianDay[iRow];
    If Parameter.Phenology.nDays[iRow] = 0 then
       List.Data[iRow, 1] := -1
    Else
       List.Data[iRow, 1] := Parameter.Phenology.nDays[iRow];
    If Parameter.Phenology.HeatSum[iRow] = 0 then
       List.Data[iRow, 2] := -1
    Else
       List.Data[iRow, 2] := Parameter.Phenology.HeatSum[iRow];
    If Parameter.Phenology.DayLength[iRow] = 0 then
       List.Data[iRow, 3] := -1
    Else
       List.Data[iRow, 3] := Parameter.Phenology.DayLength[iRow];
    List.Data[iRow, 4] := 100 * Parameter.Phenology.Senescence[iRow];
    List.Data[iRow, 5] := 100 * Parameter.Phenology.LeafGrowth[iRow];
    End;
List.nRadios := 0;
List.FileExt := 'ph!';
List.FileComment := 'Phenology sequence';
List.HelpContext := 3900;
List.TextBox := true;
List.TextBoxCaption := 'Threshold temperature for heat sum';
List.TextBoxEntry := Parameter.Phenology.Threshold;
List.HasChanged := false;
List.ShowMessage := false;
List.RedrawOption := true;
End; {of Procedure 'SetUpPhenologyDialogue'}

Procedure GetPhenologyDialogueInfo;
var iRow, CountTriggers: integer;
Begin
Control.NextPhenology := 1;
Parameter.Phenology.nChanges := List.nRows;
For iRow := 1 to List.nRows do
    Begin
    CountTriggers := 0;
    Parameter.Phenology.JulianDay[iRow] := Round(List.Data[iRow, 0]);
    Parameter.Phenology.nDays[iRow] := Round(List.Data[iRow, 1]);
    Parameter.Phenology.HeatSum[iRow] := List.Data[iRow, 2];
    Parameter.Phenology.DayLength[iRow] := List.Data[iRow, 3];
    Parameter.Phenology.Senescence[iRow] := 0.01 * List.Data[iRow, 4];
    Parameter.Phenology.LeafGrowth[iRow] := 0.01 * List.Data[iRow, 5];
    If Parameter.Phenology.JulianDay[iRow] > 0 then
       CountTriggers := CountTriggers + 1;
    If Parameter.Phenology.nDays[iRow] > 0 then
       CountTriggers := CountTriggers + 1;
    If Parameter.Phenology.HeatSum[iRow] > 0 then
       CountTriggers := CountTriggers + 1;
    If Parameter.Phenology.DayLength[iRow] > 0 then
       CountTriggers := CountTriggers + 1;
    If CountTriggers = 0 then
       Begin
       If List.ShowMessage then
          List.Message := 'Multiple invalid triggering events for phenological changes.' + chr(10) +
                       'Please, ensure that each line contains exactly one' + chr(10) +
                       'trigger event to change phenological condition'
       Else
          List.Message := 'No triggering events for phenological changes.' + chr(10) +
                       'Please, ensure that each line contains a' + chr(10) +
                       'trigger event to change phenological condition';
       List.ShowMessage := true;
       End
    Else if CountTriggers > 1 then
       Begin
       If List.ShowMessage then
          List.Message := 'Multiple triggering events for phenological changes.' + chr(10) +
                       'Please, ensure that each line contains exactly one' + chr(10) +
                       'trigger event to change phenological condition'
       Else
          List.Message := 'Too many triggering events for phenological changes.' + chr(10) +
                       'Please, ensure that each line contains only one' + chr(10) +
                       'trigger event to change phenological condition';
       List.ShowMessage := true;
       End;
    End;
For iRow := 1 to List.nRows do
    Begin
    If Parameter.Phenology.JulianDay[iRow] > 0 then
       Parameter.Phenology.Units[iRow] := JulianDay
    Else if Parameter.Phenology.nDays[iRow] > 0 then
       Parameter.Phenology.Units[iRow] := nDays
    Else if Parameter.Phenology.HeatSum[iRow] > 0 then
       Parameter.Phenology.Units[iRow] := HeatSum
    Else if Parameter.Phenology.DayLength[iRow] > 0 then
       Parameter.Phenology.Units[iRow] := DayLength;
    End;
Parameter.Phenology.Threshold := List.TextBoxEntry;
Control.PlantHasChanged := true;
End; {of Procedure 'GetPhenologyDialogueInfo'}

Procedure SwapReal(var a, b: double);
Var x: double;
    Begin
    x := a;
    a := b;
    b := x;
    End {of Procedure 'SwapReal'};

Procedure SwapLong(var a, b: integer);
Var x: integer;
    Begin
    x := a;
    a := b;
    b := x;
    End {of Procedure 'SwapLong'};

Procedure SwapBoolean(var a, b: Boolean);
Var x: Boolean;
    Begin
    x := a;
    a := b;
    b := x;
    End {of Procedure 'SwapBoolean'};

Function Min(a, b: double): double;
    Begin
    If a < b then
       Min := a
    Else
       Min := b;
    End; {of Function 'Min'}

Function Max(a, b: double): double;
    Begin
    If a > b then
       Max := a
    Else
       Max := b;
    End; {of Function 'Max'}


Procedure InitialiseSensitivity;
var SensVar: SensitivityType;
    i: Integer;

    Procedure HeadingOut (Comment: string);
    var StrOut: String;
    Begin
    while Length(Comment) < MaxWidth  do
          Comment := Comment + ' ';
    StrOut := Copy (Comment, 1, MaxWidth);
    Write (Control.SensFile, StrOut: MaxWidth+1);
    End; {of Procedure 'HeadingOut'}

Begin
Control.SensFileOpen := true;
assign (Control.SensFile, 'SensTest.out');
rewrite (Control.SensFile);
Writeln (Control.SensFile, 'Sens test':MaxSensVariableWidth);
Writeln (Control.SensFile, '+/-', 100 * TestSens.Range:MaxSensVariableWidth-1:0, '%');
Writeln (Control.SensFile);
for i := 1 to 2 * (MaxWidth + 2) do
    Write (Control.SensFile, ' ');
For SensVar := CAI to ShowResistantN do
    If TestSens.Choose[SensVar] then
       Case SensVar of
            CAI: HeadingOut('CAI');
            NConc: HeadingOut('N conc');
            LAI: HeadingOut('LAI');
            Wood: HeadingOut('Wood');
            ShowFineRootC: HeadingOut('Fine rtC');
            ShowFineRootN: HeadingOut('Fine rtN');
            ShowBranches: HeadingOut('Branch C');
            ShowLeaves: HeadingOut('Foliage C');
            ShowLeafN: HeadingOut('Foliage N');
            Height: HeadingOut('Height');
            DBH: HeadingOut('DBH');
            CanopyCover: HeadingOut('Cpy cover');
            kex: HeadingOut('k ext.');
            ShowFineWoodSurfC: HeadingOut('Fn wd srf');
            ShowCoarseWoodSurfC: HeadingOut('C wd srf');
            ShowCoarseWoodSoilC: HeadingOut('C wd sl');
            ShowStructSurfC: HeadingOut('Str srf');
            ShowMetabSurfC: HeadingOut('Metab srf');
            ShowStructSurfN: HeadingOut('Str srf');
            ShowMetabSurfN: HeadingOut('Metab srf');
            ShowSlowC: HeadingOut('Slow C ');
            ShowActiveC: HeadingOut('Active C');
            ShowResistantC: HeadingOut('Resist C');
            ShowSlowN: HeadingOut('Slow N');
            ShowActiveN: HeadingOut('Active N');
            ShowResistantN: HeadingOut('Recalc N');
            End;
Writeln (Control.SensFile);
End; {of Procedure 'InitialiseSensitivity'}

Procedure SetUpSens (Index: Integer);

    Procedure SetParameters (Index: Integer; var ChangeVar: double; LowLimit, UpLimit: double);
    Begin
    if Index = -1 then
       Begin
       Control.OldPar := ChangeVar;
       ChangeVar := ChangeVar * (1 - TestSens.Range);
       if ChangeVar < LowLimit then
          ChangeVar := LowLimit;
       End
    Else if Index = 1 then
       Begin
       ChangeVar := Control.OldPar * (1 + TestSens.Range);
       if ChangeVar > UpLimit then
          ChangeVar := UpLimit;
       End
    Else if Index = 0 then
       ChangeVar := Control.OldPar;
    End; {of Procedure 'SetParameters'}

    Procedure SoilParameters (Index: Integer; var ChangeSoilVar: SoilElements; E: ElementsUsed);
    var iLayer: Integer;
    Begin
    if Index = -1 then
       Begin
       Control.OldSoilPar := ChangeSoilVar;
       for iLayer := 1 to Soil.nLayers do
           ChangeSoilVar[iLayer, E] := ChangeSoilVar[iLayer, E] * (1 - TestSens.Range);
       End
    Else if Index = 1 then
       Begin
       for iLayer := 1 to Soil.nLayers do
           ChangeSoilVar[iLayer, E] := Control.OldSoilPar[iLayer, E] * (1 + TestSens.Range);
       End
    Else if Index = 0 then
       ChangeSoilVar := Control.OldSoilPar;
    End; {of Procedure 'SoilParameters'}

Begin
Case Control.SensParameter of
     FineSoil: SetParameters (Index, Parameter.FineSoil, 0, 1);
     MeanSoilTemp: SetParameters (Index, Parameter.MeanSoilTemp, -100, 100);
     CO2: SetParameters (Index, Parameter.CO2Conc, 0, 100000);
     AtmosPressure: SetParameters (Index, Parameter.AtmosPressure, 0 , 100000);
     WoodLignin: SetParameters (Index, Parameter.WoodLignin, 0, 1);
     LeafLignin: SetParameters (Index, Parameter.LeafLignin, 0, 1);
     RootLignin: SetParameters (Index, Parameter.RootLignin, 0, 1);
     AnnualRain: SetParameters (Index, Parameter.AnnualRain, 0 , 100000);
     RainProb: SetParameters (Index, Parameter.RainProb, 0, 1);
     MeanTmax: SetParameters (Index, Parameter.MeanTmax, -100, 100);
     MeanTmin: SetParameters (Index, Parameter.MeanTmin, -100, 100);
     MeanRadn: SetParameters (Index, Parameter.MeanRadn, 0, 100);
     MeanAbsHum: SetParameters (Index, Parameter.MeanAbsHum, 0, 1);
     Latitude: SetParameters (Index, Parameter.Latitude, -90, 90);
     FertiliserRelease: SetParameters (Index, Parameter.FertiliserRelease, 0, 1);
     FertilityAdjust: SetParameters (Index, Parameter.FertilityAdjust, 0, 100);
     RateAdjust: SetParameters (Index, Parameter.RateAdjust, 0, 100);
     TMaxRepairTime: SetParameters (Index, Parameter.TMaxRepairTime, 0, 10000);
     Immobilise: SetParameters (Index, Parameter.Immobilise, 0, 1);
     DirectEvapSlope: SetParameters (Index, Parameter.DirectEvapSlope, 0 , 10000);
     DirectEvapFract: SetParameters (Index, Parameter.DirectEvapFract, 0, 1);
     Decay8: SetParameters (Index, Parameter.Decay8, 0, 10000);
     Decay9: SetParameters (Index, Parameter.Decay9, 0, 10000);
     RelativeCN: SetParameters (Index, Parameter.RelativeCN, 0, 10000);
     CriticalCN: SetParameters (Index, Parameter.CriticalCN, 0, 100);
     Transmit: SetParameters (Index, Parameter.Transmit, 0, 1);
     BallBerry1: SetParameters (Index, Parameter.BallBerry1, 0, 10000);
     BallBerry2: SetParameters (Index, Parameter.BallBerry2, 0, 10000);
     Nloss: SetParameters (Index, Parameter.Nloss, 0, 1);
     Leaching: SetParameters (Index, Parameter.Leaching, 0, 1);
     MicroFract: SetParameters (Index, Parameter.MicroFract, 0, 1);
     Atmos_N: SetParameters (Index, Parameter.Atmos_N, 0, 10000);
     BiolFix: SetParameters (Index, Parameter.BiolFix, 0, 10000);
     GrowthRespn: SetParameters (Index, Parameter.GrowthRespn, 0, 10);
     BarkSenesc: SetParameters (Index, Parameter.BarkSenesc, 0, 10);
     RespFromN: SetParameters (Index, Parameter.RespFromN, 0, 10000);
     StemDeath: SetParameters (Index, Parameter.StemDeath, 0, 1);
     SenescLeafRatio: SetParameters (Index, Parameter.SenescLeafRatio, 0, 1);
     InternalNRatio: SetParameters (Index, Parameter.InternalNRatio, 0, 1);
     LeafSenesc: SetParameters (Index, Parameter.LeafSenesc, 0, 1);
     BranchSenesc: SetParameters (Index, Parameter.BranchSenesc, 0, 1);
     RootSenesc: SetParameters (Index, Parameter.RootSenesc, 0, 1);
     FruitSenesc: SetParameters (Index, Parameter.FruitSenesc, 0, 1);
     PollenSenesc: SetParameters (Index, Parameter.PollenSenesc, 0, 1);
     SenescLowLight: SetParameters (Index, Parameter.SenescLowLight, 0, 1);
     MaxSenescLowLight: SetParameters (Index, Parameter.MaxSenescLowLight, 0, 1);
     RootLeafRatio1: SetParameters (Index, Parameter.RootLeafRatio1, 0, 1000);
     RootLeafRatio2: SetParameters (Index, Parameter.RootLeafRatio2, 0, 1000);
     WoodBranchRatio: SetParameters (Index, Parameter.WoodBranchRatio, 0, 1000);
     LeafBranchRatio: SetParameters (Index, Parameter.LeafBranchRatio, 0, 1000);
     BarkWoodRatio: SetParameters (Index, Parameter.BarkWoodRatio, 0, 1000);
     CoarseRootWoodRatio: SetParameters (Index, Parameter.CoarseRootWoodRatio, 0, 1000);
     C_FruitAlloc: SetParameters (Index, Parameter.C_FruitAlloc, 0, 1);
     C_PollenAlloc: SetParameters (Index, Parameter.C_PollenAlloc, 0, 1);
     SLA: SetParameters (Index, Parameter.SLA, 0, 1000);
     Amax: SetParameters (Index, Parameter.Amax, 0, 1000);
     Theta: SetParameters (Index, Parameter.Theta, 0, 1);
     Kexmax: SetParameters (Index, Parameter.Kexmax, 0, 100);
     KlowRange: SetParameters (Index, Parameter.KlowRAnge, 0, 100);
     Albedo: SetParameters (Index, Parameter.Albedo, 0, 1);
     Temp_Amplitude: SetParameters (Index, Parameter.Temp_Amplitude, 0, 100);
     Radn_Amplitude: SetParameters (Index, Parameter.Radn_Amplitude, 0, 100);
     Daily_Amplitude: SetParameters (Index, Parameter.Daily_Amplitude, 0, 100);
     Humid_Amplitude: SetParameters (Index, Parameter.Humid_Amplitude, 0, 100);
     DrySenesc: SetParameters (Index, Parameter.DrySenesc, 0, 1);
     StressLimit: SetParameters (Index, Parameter.StressLimit, 0, 1);
     SoilEvap: SetParameters (Index, Parameter.SoilEvap, 0, 1000);
     HDInter: SetParameters (Index, Parameter.HDInter, -1000, 1000);
     HDSlope: SetParameters (Index, Parameter.HDSlope, 0, 10);
     WDSlope: SetParameters (Index, Parameter.WDSlope, -1000, 1000);
     WHSlope: SetParameters (Index, Parameter.WHSlope, 0, 10);
     WeedMaxHeight: SetParameters (Index, Parameter.Weed.MaxHeight, 0, 100);
     WeedKMHeight: SetParameters (Index, Parameter.Weed.KMHeight, 0, 1e6);
     WeedFoliageTurnover: SetParameters (Index, Parameter.Weed.Senescence, 0, 1);
     WeedFoliageAllocation: SetParameters (Index, Parameter.Weed.AllocLeaves, 0, 1);
     WeedKmTreeRoots: SetParameters (Index, Parameter.Weed.KMRootPlantNUptake, 0, 1e6);
     WeedKmTreeRootsP: SetParameters (Index, Parameter.Weed.KMRootPlantPUptake, 0, 1e6);
     WoodDensPith: SetParameters (Index, Parameter.WoodDensity0, 0, 1e6);
     WoodDensOuter: SetParameters (Index, Parameter.WoodDensity25, 0, 1e6);
     WoodDensTemperature: SetParameters (Index, Parameter.WoodDensTemp, -1e6, 1e6);
     WoodDensFertility: SetParameters (Index, Parameter.WoodDensFertility, -1e6, 1e6);
     WoodDensStocking: SetParameters (Index, Parameter.WoodDensStocking, -1e6, 1e6);
     HDConst: SetParameters (Index, Parameter.HD_Const, 0, 10);
     HDTemp: SetParameters (Index, Parameter.HD_Temp, -1e6, 1e6);
     HDStocking: SetParameters (Index, Parameter.HD_Stocking, -1e6, 1e6);
     HDFertility: SetParameters (Index, Parameter.HD_Fertil, -1e6, 1e6);
     HDAge1: SetParameters (Index, Parameter.HD_Age1, -1e9, 1e9);
     HDAge2: SetParameters (Index, Parameter.HD_Age2, -1e9, 1e9);
     HDInitSlope: SetParameters (Index, Parameter.HD_InitialSlope, 0, 10);
     HDInitIntercept: SetParameters (Index, Parameter.HD_InitialInter, -1e9, 1e9);
     HDSlopeMin: SetParameters (Index, Parameter.HDSlopeMin, 0, 100);
     HDSlopeMax: SetParameters (Index, Parameter.HDSlopeMax, 0, 100);
     TMinLim: SetParameters (Index, Parameter.TMinLim, -100, 100);
     TOpt1: SetParameters (Index, Parameter.TOpt1, -100, 100);
     TOpt2: SetParameters (Index, Parameter.TOpt2, -100, 100);
     TmaxLim: SetParameters (Index, Parameter.TMaxLim, -100, 100);
     TFrost: SetParameters (Index, Parameter.TFrost, -100, 100);
     TScorch: SetParameters (Index, Parameter.TScorch, -100, 100);
     TSensitivity: SetParameters (Index, Parameter.TSensitivity, 0, 1000);
     TRepair: SetParameters (Index, Parameter.TRepair, 0, 1000);
     WoodRetrans: SetParameters (Index, Parameter.WoodRetrans, 0, 1);
     bRoots: SetParameters (Index, Parameter.bRoots, 0, 100);
     bWood: SetParameters (Index, Parameter.bWood, 0, 100);
     bBark: SetParameters (Index, Parameter.bBark, 0, 100);
     bBranch: SetParameters (Index, Parameter.bBranch, 0, 100);
     bFruit: SetParameters (Index, Parameter.bFruit, 0, 100);
     bPollen: SetParameters (Index, Parameter.bPollen, 0, 100);
     RelWaterSens: SetParameters (Index, Parameter.RelWaterSens, 0, 10);
     N0: SetParameters (Index, Parameter.N0, 0, 100);
     Ncrit: SetParameters (Index, Parameter.NCrit, 0, 100);
     Nmax: SetParameters (Index, Parameter.Nmax, 0, 100);
     SapWoodC: SetParameters (Index, Plant.SapWood[C], 0, 1E8);
     HeartWoodC: SetParameters (Index, Plant.HeartWood[C], 0, 1E8);
     CoarseRootC: SetParameters (Index, Plant.CoarseRoot[C], 0, 1E8);
     FineRootC: SetParameters (Index, Plant.FineRoot[C], 0, 1E8);
     BranchesC: SetParameters (Index, Plant.Branches[C], 0, 1E8);
     BarkC: SetParameters (Index, Plant.Bark[C], 0, 1E8);
     LeavesC: SetParameters (Index, Plant.Leaves[C], 0, 1E8);
     SapWoodN: SetParameters (Index, Plant.SapWood[N], 0, 1E8);
     HeartWoodN: SetParameters (Index, Plant.HeartWood[N], 0, 1E8);
     CoarseRootN: SetParameters (Index, Plant.CoarseRoot[N], 0, 1E8);
     FineRootN: SetParameters (Index, Plant.FineRoot[N], 0, 1E8);
     BranchesN: SetParameters (Index, Plant.Branches[N], 0, 1E8);
     BarkN: SetParameters (Index, Plant.Bark[N], 0, 1E8);
     LeavesN: SetParameters (Index, Plant.Leaves[N], 0, 1E8);
     Stocking: SetParameters (Index, Plant.Stocking, 0, 100000);
     FineWoodSurfC: SetParameters (Index, Soil.FineWood[0, C], 0, 100000);
     FineWoodSurfN: SetParameters (Index, Soil.FineWood[0, N], 0, 100000);
     CoarseWoodSurfC: SetParameters (Index, Soil.CoarseWood[0, C], 0, 100000);
     CoarseWoodSurfN: SetParameters (Index, Soil.CoarseWood[0, N], 0, 100000);
     StructSurfC: SetParameters (Index, Soil.Struct[0, C], 0, 100000);
     StructSurfN: SetParameters (Index, Soil.Struct[0, N], 0, 100000);
     MetabSurfC: SetParameters (Index, Soil.Metab[0, C], 0, 100000);
     MetabSurfN: SetParameters (Index, Soil.Metab[0, N], 0, 100000);
     CoarseWoodSoilC: SoilParameters (Index, Soil.Coarsewood, C);
     StructSoilC: SoilParameters (Index, Soil.Struct, C);
     MetabSoilC: SoilParameters (Index, Soil.Metab, C);
     SlowC: SoilParameters (Index, Soil.Slow, C);
     ActiveC: SoilParameters (Index, Soil.Active, C);
     ResistantC: SoilParameters (Index, Soil.Resistant, C);
     CoarseWoodSoilN: SoilParameters (Index, Soil.Coarsewood, N);
     StructSoilN: SoilParameters (Index, Soil.Struct, N);
     MetabSoilN: SoilParameters (Index, Soil.Metab, N);
     SlowN: SoilParameters (Index, Soil.Slow, N);
     ActiveN: SoilParameters (Index, Soil.Active, N);
     ResistantN: SoilParameters (Index, Soil.Resistant, N);
     End;
If (Control.SensFlag = -1) and (Control.SensParameter = EndInputs) then
   Control.SensitivityTestOn := False;
Control.SensFlag := -Control.SensFlag;
End; {of Procedure 'SetUpSens'}

Procedure WriteOutSens;
var LeafC, LeafN, Sum: double;
    StrOut: String;
    SensVar: SensitivityType;
    iLayer: Integer;

    Procedure WriteOut (VarOut: double);
    var Width, Digits: integer;
        Begin
        GetField(VarOut, MaxWidth, Width, Digits);
        Width := Width + 1;
        Write (Control.SensFile, VarOut: Width: Digits);
        End; {of Procedure 'WriteOut'}

Begin
LeafC := Plant.Leaves[C];
LeafN := Plant.Leaves[N];
if Length(SensitivityNames[Control.SensParameter]) <= MaxSensVariableWidth then
   StrOut := SensitivityNames[Control.SensParameter]
Else
   StrOut := Copy (SensitivityNames[Control.SensParameter], 1, MaxSensVariableWidth);
Write (Control.SensFile, StrOut: MaxSensVariableWidth);
For SensVar := CAI to EndDummy do
    Begin
    If TestSens.Choose[SensVar] then
       Case SensVar of
            CAI: WriteOut(Derived.CAI);
            NConc: WriteOut(Derived.NConc[Over] / Control.CConversion);
            LAI: WriteOut(Derived.LAI[Over]);
            Wood: WriteOut((Plant.SapWood[C] + Plant.HeartWood[C]) * Control.CConversion);
            ShowFineRootC: WriteOut(Plant.FineRoot[C] * Control.CConversion);
            ShowFineRootN: WriteOut(Plant.FineRoot[N]);
            ShowBranches: WriteOut(Plant.Branches[C] * Control.CConversion);
            ShowLeaves: WriteOut(LeafC * Control.CConversion);
            ShowLeafN: WriteOut(LeafN);
            Height: WriteOut(Plant.Height);
            DBH: WriteOut(Plant.DBH);
            CanopyCover: WriteOut(Plant.CanopyCover);
            kex: WriteOut(Plant.kex);
            ShowFineWoodSurfC: WriteOut(Soil.FineWood[0, C]);
            ShowCoarseWoodSurfC: WriteOut(Soil.CoarseWood[0, C]);
            ShowStructSurfC: WriteOut(Soil.Struct[0, C]);
            ShowMetabSurfC: WriteOut(Soil.Metab[0, C]);
            ShowStructSurfN: WriteOut(Soil.Struct[0, N]);
            ShowMetabSurfN: WriteOut(Soil.Metab[0, N]);
            ShowCoarseWoodSoilC: Begin
                                 Sum := 0;
                                 For iLayer := 1 to Soil.nLayers do
                                     Sum := Sum + Soil.CoarseWood[iLayer, C];
                                 WriteOut(Sum);
                                 End;
             ShowSlowC:          Begin
                                 Sum := 0;
                                 For iLayer := 1 to Soil.nLayers do
                                     Sum := Sum + Soil.Slow[iLayer, C];
                                 WriteOut(Sum);
                                 End;
             ShowActiveC:        Begin
                                 Sum := 0;
                                 For iLayer := 1 to Soil.nLayers do
                                     Sum := Sum + Soil.Active[iLayer, C];
                                 WriteOut(Sum);
                                 End;
             ShowResistantC:     Begin
                                 Sum := 0;
                                 For iLayer := 1 to Soil.nLayers do
                                     Sum := Sum + Soil.Resistant[iLayer, C];
                                 WriteOut(Sum);
                                 End;
             ShowSlowN:          Begin
                                 Sum := 0;
                                 For iLayer := 1 to Soil.nLayers do
                                     Sum := Sum + Soil.Slow[iLayer, N];
                                 WriteOut(Sum);
                                 End;
             ShowActiveN:        Begin
                                 Sum := 0;
                                 For iLayer := 1 to Soil.nLayers do
                                     Sum := Sum + Soil.Active[iLayer, N];
                                 WriteOut(Sum);
                                 End;
             ShowResistantN:     Begin
                                 Sum := 0;
                                 For iLayer := 1 to Soil.nLayers do
                                     Sum := Sum + Soil.Resistant[iLayer, N];
                                 WriteOut(Sum);
                                 End;
            End;
    If SensVar = EndDummy then
       Writeln(Control.SensFile);
    End;
End; {of Procedure 'WriteOutSens'}

Procedure EndSensRoutine;
Begin
Control.SensFlag := -1;
If Control.SensFileOpen then
   Close (Control.SensFile);
Control.SensFileOpen := false;
End; {of Procedure 'EndSenesRoutine'}

{ --- end of file Miscellaneous.pas ------------------------------------------ }

End.


