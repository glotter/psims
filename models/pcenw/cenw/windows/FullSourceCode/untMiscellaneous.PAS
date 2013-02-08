{ ================================================================
  = Project   : Cenw                                             =
  ================================================================
  = Module    : AdjustFertility                                  =
  =             TotalN                                           =
  =             CalcDate                                         =
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
  =             SetUpPloughDialogue                              =
  =             GetPloughDialogueInfo                            =
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
  = Version   : 3.1                                              =
  ================================================================ }

Unit untMiscellaneous;

Interface

Uses
  untDeclarations, SysUtils, untFieldValidation, untDivideValidation, untPowerValidation;

Procedure AdjustFertility;
Procedure ResetPlantPools;
Procedure CalcDate(var DateOut: InfoTransferRecord);
Function TotalN: real48;
Procedure SetUpSens;
Procedure WriteOutSens;
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
Procedure SetUpLitterDialogue;
Procedure GetLitterDialogueInfo;
Procedure SetUpSoilWaterDialogue;
Procedure GetSoilWaterDialogueInfo;
Procedure SetUpPhenologyDialogue;
Procedure GetPhenologyDialogueInfo;
Procedure SwapReal(var a, b: real48);
Procedure SwapLong(var a, b: LongInt);
Procedure SwapBoolean(var a, b: Boolean);

Implementation

Procedure AdjustFertility;
var E: ElementsUsed;
    iLayer: Integer;
    begin
    For E := C to N do
        For iLayer := 0 to SoilWat.nLayers do
            Begin
            SoilOrganic.Active[iLayer, E] := SoilOrganic.Active[iLayer, E] * Parameter.FertilityAdjust;
            SoilOrganic.Slow[iLayer, E] := SoilOrganic.Slow[iLayer, E] * Parameter.FertilityAdjust;
            SoilOrganic.Resistant[iLayer, E] := SoilOrganic.Resistant[iLayer, E] * Parameter.FertilityAdjust;
            SoilOrganic.Soluble[iLayer, E] := SoilOrganic.Soluble[iLayer, E] * Parameter.FertilityAdjust;
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
    End;
Plant.Age := Control.Initial.Age;
Plant.Stocking := Control.Initial.Stocking;
Control.ExtraDays := Control.Initial.Days;
Control.ExtraMonths := Control.Initial.Months;
Control.TotalYears := Control.Initial.Years;
End; {of Procedure 'ResetPlantPools'}

Procedure CalcDate(var DateOut: InfoTransferRecord);
var Ps, ps1, Ps2, ps3: string;
    Leap: 0..1;
    i : integer;
    Begin
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
           If (((Control.TotalYears mod 4) <> 0) and (Control.ExtraDays > 28)) or
              (((Control.TotalYears mod 4) = 0) and (Control.ExtraDays > 29)) then
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
    If Control.TotalYears mod 4 = 0 then Leap := 1 Else Leap := 0;
    Case Control.ExtraMonths of
         1: Control.JulianDay := Control.ExtraDays;
         2: Control.JulianDay := Control.ExtraDays + 31;
         3: Control.JulianDay := Control.ExtraDays + 59 + Leap;
         4: Control.JulianDay := Control.ExtraDays + 90 + Leap;
         5: Control.JulianDay := Control.ExtraDays + 120 + Leap;
         6: Control.JulianDay := Control.ExtraDays + 151 + Leap;
         7: Control.JulianDay := Control.ExtraDays + 181 + Leap;
         8: Control.JulianDay := Control.ExtraDays + 212 + Leap;
         9: Control.JulianDay := Control.ExtraDays + 243 + Leap;
        10: Control.JulianDay := Control.ExtraDays + 273 + Leap;
        11: Control.JulianDay := Control.ExtraDays + 304 + Leap;
        12: Control.JulianDay := Control.ExtraDays + 334 + Leap;
        End;

    End; {of Procedure 'CalcDate'}

Function TotalN: real48;
var iLayer: Integer;
    NSum: Real48;
Begin
NSum := Plant.SapWood[N] + Plant.HeartWood[N] + Plant.CoarseRoot[N] + Plant.FineRoot[N] + Plant.Reserves[N] +
        Plant.Branches[N] + Plant.Bark[N] + Plant.Pollen[N] + Plant.Fruit[N] + Plant.Soluble[N] +
        Plant.Leaves[N] + Plant.Reserves[N] + Derived.ExcessN +
        Litter.CoarseWood[N] + Litter.FineWood[N] + Litter.CoarseRoot[N] +
        Litter.FineRoot[N] + Litter.Leaves[N] + Litter.Other[N];
For iLayer := 0 to SoilOrganic.nLayers do
    NSum := NSum +
            SoilOrganic.FineWood[iLayer, N] + SoilOrganic.CoarseWood[iLayer, N] +
            SoilOrganic.Struct[iLayer, N] + SoilOrganic.Metab[iLayer, N] +
            SoilOrganic.Slow[iLayer, N] + SoilOrganic.Active[iLayer, N] +
            SoilOrganic.Resistant[iLayer, N] + SoilOrganic.Inert[iLayer, N];
TotalN := NSum;
End; {of Function 'TotalN'}

Procedure SetUpHarvestDialogue;
var iRow, iDays, iMonths, iYears: Integer;
Begin
// Event Manager - harvest and thinning
List.Caption := 'Harvest or Thinning';
List.MaxRows := MaxHarvestEvents;
List.nEntries := 8;
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
List.DataType[2] := R;
List.DataType[3] := R;
List.DataType[4] := S;
List.DataType[5] := R;
List.DataType[6] := R;
List.DataType[7] := R;
List.Width[0] := 51;
List.Width[1] := 54;
List.Width[2] := 71;
List.Width[3] := 57;
List.Width[4] := 72;
List.Width[5] := 72;
List.Width[6] := 74;
List.Width[7] := 64;
List.Text[1, 0] := 'Months';
List.Text[1, 1] := 'Years';
List.Text[1, 2] := 'Wood cut  (%)';
List.Text[1, 3] := 'Rel. size of cut trees';
List.Text[1, 4] := 'Stocking (Same or Adjust)';
List.Text[1, 5] := 'Branches cut (%)';
List.Text[1, 6] := 'Wood removed (%)';
List.Text[1, 7] := 'Fin material removed (%)';
List.Text[2, 0] := 'Months';
List.Text[2, 1] := 'Years';
List.Text[2, 2] := 'Wood cut (tDW ha-1)';
List.Text[2, 3] := 'Rel. size of cut trees';
List.Text[2, 4] := 'Stocking (Same or Adjust)';
List.Text[2, 5] := 'Branches cut (tDW ha-1)';
List.Text[2, 6] := 'Wood removed (tDW ha-1)';
List.Text[2, 7] := 'Fin material removed (tDW ha-1)';
List.Text[3, 0] := 'Months';
List.Text[3, 1] := 'Years';
List.Text[3, 2] := 'Wood cut (m3 ha-1)';
List.Text[3, 3] := 'Rel. size of cut trees';
List.Text[3, 4] := 'Stocking (Same or Adjust)';
List.Text[3, 5] := 'Branches cut (m3 ha-1)';
List.Text[3, 6] := 'Wood removed (m3 ha-1)';
List.Text[3, 7] := 'Fin material removed (m3 ha-1)';
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
       End;
    List.Data[iRow, 0] := iMonths;
    List.Data[iRow, 1] := iYears;
    List.Data[iRow, 2] := Event.WoodCut[iRow];
    List.Data[iRow, 3] := Event.RelativeSize[iRow];
    if (Event.AdjustStocking[iRow]) then
        List.Data[iRow, 4] := 1
    Else
        List.Data[iRow, 4] := 2;
    List.Data[iRow, 5] := Event.BranchesCut[iRow];
    List.Data[iRow, 6] := Event.WoodRemoval[iRow];
    List.Data[iRow, 7] := Event.FineRemoval[iRow];
    If Event.HarvestUnits = '%' then
       Begin
       List.Data[iRow, 2] := List.Data[iRow, 2] * 100;
       List.Data[iRow, 3] := List.Data[iRow, 3] * 100;
       List.Data[iRow, 5] := List.Data[iRow, 5] * 100;
       List.Data[iRow, 6] := List.Data[iRow, 6] * 100;
       List.Data[iRow, 7] := List.Data[iRow, 7] * 100;
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
    fDate1, fDate2: TDateTime;
Begin
Event.nHarvests := List.nRows;
For iRow := 1 to List.nRows do
    Begin
    If List.RbtnSelected[2] = 0 then
       Begin
       List.Data[iRow, 2] := List.Data[iRow, 2] * 0.01;
       List.Data[iRow, 3] := List.Data[iRow, 3] * 0.01;
       List.Data[iRow, 5] := List.Data[iRow, 5] * 0.01;
       List.Data[iRow, 6] := List.Data[iRow, 6] * 0.01;
       List.Data[iRow, 7] := List.Data[iRow, 7] * 0.01;
       End;
    iMonths := Round(List.Data[iRow, 0]);
    iYears := Round(List.Data[iRow, 1]);
    iDays := Round(365.25 * iYears + 30.4375 * iMonths);
    Event.HarvestTimes[iRow, 1] := 1;
    Event.HarvestTimes[iRow, 2] := iMonths;
    Event.HarvestTimes[iRow, 3] := iYears;
    Event.HarvestTimes[iRow, 4] := iDays;
    Event.WoodCut[iRow] := List.Data[iRow, 2];
    Event.RelativeSize[iRow] := List.Data[iRow, 3];
    Event.BranchesCut[iRow] := List.Data[iRow, 5];
    Event.WoodRemoval[iRow] := List.Data[iRow, 6];
    Event.FineRemoval[iRow] := List.Data[iRow, 7];
    If List.Data[iRow, 4] = 1 then
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
    CumDays: LongInt;
    PestMortality: Real48;
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
       CumDays := Trunc(iDays - iYears * 365.25);
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
    CumDays: LongInt;
    fDate1, fDate2: TDateTime;
    Defoliated, Sucked, Senesced, reducedPHS, Killed, DeathRatio: Real48;
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
    If (iMonth > 0) and (iYear > 0) then
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
var iRow, iDays, iMonths, iYears: Integer;
    CumDays: LongInt;
Begin
// Event Manager - fire
List.Caption := 'Set times and characteristics of fires';
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
       iDays := Event.FireTimes[iRow, 1];
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
    SumFates: Real48;
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
           End;
        End;
    End;
Control.ProjectHasChanged := true;
End; {of Procedure 'GetFireDialogueInfo'}

Procedure SetUpFertiliserDialogue;
var iRow, iDays, iMonths, iYears: Integer;
    CumDays: LongInt;
Begin
// Event Manager - fire
List.Caption := 'Fertiliser application';
List.nEntries := 4;
List.MaxRows := MaxFertiliseEvents;
List.StrOptions := 0;
List.Header := 1;
List.DataType[0] := I;
List.DataType[1] := I;
List.DataType[2] := I;
List.DataType[3] := R;
List.Width[0] := 70;
List.Width[1] := 70;
List.Width[2] := 70;
List.Width[3] := 70;
List.Text[1, 0] := 'Day';
List.Text[1, 1] := 'Month';
List.Text[1, 2] := 'Year';
List.Text[1, 3] := 'Fertiliser added';
List.nRows := Event.nFertilisations;
For iRow := 1 to List.nRows do
    Begin
    List.Data[iRow, 0] := Event.FertiliseTimes[iRow, 1];
    List.Data[iRow, 1] := Event.FertiliseTimes[iRow, 2];
    List.Data[iRow, 2] := Event.FertiliseTimes[iRow, 3];
    List.Data[iRow, 3] := Event.FertiliseAmount[iRow];
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
    fDate1, fDate2: TDateTime;
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
    Event.FertiliseAmount[iRow] := List.Data[iRow, 3];
    End;
// now sort them based on start time - use BubbleSort method
For iRow := 1 to List.nRows - 1 do
    Begin
    For iCount2 := List.nRows - 1 downto iRow do
        Begin
        fDate1 := EncodeDate(Event.FertiliseTimes[iCount2, 3], Event.FertiliseTimes[iCount2, 2], Event.FertiliseTimes[iCount2, 1]);
        fDate2 := EncodeDate(Event.FertiliseTimes[iCount2+1, 3], Event.FertiliseTimes[iCount2+1, 2], Event.FertiliseTimes[iCount2+1, 1]);
        if (fDate1 > fDate2) then
           Begin
           For iCount3 := 1 to 3 do
               SwapLong(Event.FertiliseTimes[iCount2, iCount3], Event.FertiliseTimes[iCount2 + 1, iCount3]);
           SwapReal(Event.FertiliseAmount[iCount2], Event.FertiliseAmount[iCount2 + 1]);
           End;
        End;
    End;
Control.ProjectHasChanged := true;
End; {of Procedure 'GetFertiliserDialogueInfo'}

Procedure SetUpEnvironmentDialogue;
var iRow, iDays, iMonths, iYears: Integer;
    CumDays: LongInt;
    PestMortality: Real48;
Begin
// Event Manager - environment changes
List.Caption := 'Change weather conditions';
List.nEntries := 5;
List.MaxRows := MaxEnvironmentEvents;
List.StrOptions := 0;
List.Header := 1;
List.DataType[0] := I;
List.DataType[1] := I;
List.DataType[2] := R;
List.DataType[3] := R;
List.DataType[4] := R;
List.Width[0] := 70;
List.Width[1] := 70;
List.Width[2] := 70;
List.Width[3] := 70;
List.Width[4] := 70;
List.Text[1, 0] := 'Month';
List.Text[1, 1] := 'Year';
List.Text[1, 2] := 'CO2';
List.Text[1, 3] := 'Temperature';
List.Text[1, 4] := 'Precipitation';
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
    End;
List.nRadios := 0;
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
           End;
        End;
    End;
Control.ProjectHasChanged := true;
End; {of Procedure 'GetEnvironmentDialogueInfo'}

Procedure SetUpPloughDialogue;
var iRow, iDays, iMonths, iYears: Integer;
Begin
// Event Manager - ploughing
List.Caption := 'Times and characteristics of ploughing';
List.nEntries := 3;
List.MaxRows := MaxPloughEvents;
List.StrOptions := 0;
List.Header := 1;
List.DataType[0] := I;
List.DataType[1] := I;
List.DataType[2] := I;
List.Width[0] := 60;
List.Width[1] := 60;
List.Width[2] := 60;
List.Text[1, 0] := 'Month';
List.Text[1, 1] := 'Year';
List.Text[1, 2] := 'Plough depth';
List.nRows := Event.nPloughing;
For iRow := 1 to List.nRows do
    Begin
    If (Event.PloughTimes[iRow,1] = 0) and (Event.PloughTimes[iRow,2] = 0) and (Event.PloughTimes[iRow,3] = 0) then
       Begin
       iDays := Event.PloughTimes[iRow, 4];
       iYears := Trunc((0.5 + iDays) / 365.25);
       iDays := Trunc(iDays - iYears * 365.25);
       iMonths := iDays div 30;
       End
    Else
       Begin
       iDays := Event.PloughTimes[iRow, 1];
       iMonths := Event.PloughTimes[iRow, 2];
       iYears := Event.PloughTimes[iRow, 3];;
       End;
    List.Data[iRow, 0] := iMonths;
    List.Data[iRow, 1] := iYears;
    List.Data[iRow, 2] := Event.PloughDepth[iRow];
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
List.TextBox := false;
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
    End;
If List.RbtnSelected[1] = 0 then
   Control.Plough_DateType := 'D'
Else
   Control.Plough_DateType := 'B';
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
           End;
        End;
    End;
Control.ProjectHasChanged := true;
End; {of Procedure 'GetPloughDialogueInfo'}

Procedure SetUpLitterDialogue;
var iRow, iDays, iMonths, iYears: Integer;
Begin
// Event Manager - litter input
List.Caption := 'Set relative litter input rates';
List.nEntries := 3;
List.MaxRows := MaxSoilLayers;
List.StrOptions := 0;
List.Header := 1;
List.DataType[0] := R;
List.DataType[1] := R;
List.DataType[2] := R;
List.Width[0] := 80;
List.Width[1] := 80;
List.Width[2] := 80;
List.Text[1, 0] := 'Depth (cm)';
List.Text[1, 1] := 'Relative fine-root input';
List.Text[1, 2] := 'Relative coarse-root input';
List.nRows := SoilWat.nLayers;
For iRow := 1 to List.nRows do
    Begin
    List.Data[iRow, 0] := SoilWat.Layer[iRow].Depth;
    List.Data[iRow, 1] := SoilOrganic.FineRootLitterIn[iRow];
    List.Data[iRow, 2] := SoilOrganic.CoarseRootLitterIn[iRow];
    End;
List.nRadios := 0;
List.FileExt := 'lt!';
List.FileComment := 'Litter input by layer';
List.HelpContext := 3750;
List.HasChanged := false;
List.RedrawOption := true;
End; {of Procedure 'SetUpLitterDialogue'}

Procedure GetLitterDialogueInfo;
var iRow, iCount: integer;
    TotalOM, SumFineRoot, SumCoarseRoot: Real48;
Begin
SoilWat.nLayers := List.nRows;
For iRow := 1 to List.nRows do
    Begin
    SoilWat.Layer[iRow].Depth := List.Data[iRow, 0];
    SoilOrganic.FineRootLitterIn[iRow] := List.Data[iRow, 1];
    SoilOrganic.CoarseRootLitterIn[iRow] := List.Data[iRow, 2];
    End;
TotalOM := 0;
// find the total input for the soil as a whole
For iCount := 1 to SoilWat.nLayers do
    TotalOM := TotalOM + SoilOrganic.FineRootLitterIn[iCount];
If (TotalOM <= 0) then
   For iCount := 1 to SoilWat.nLayers do
       SoilOrganic.FineRootLitterIn[iCount] := 1 / SoilWat.nLayers;
// make sure total extract is valid
If Control.AllOneLayer then
   Begin
   If SoilOrganic.FineRootLitterIn[1] = 0 then
      SoilOrganic.FineRootLitterIn[1] := 1;
   If SoilOrganic.CoarseRootLitterIn[1] = 0 then
      SoilOrganic.CoarseRootLitterIn[1] := 1;
   For iCount := SoilWat.nLayers downto 1 do
       Begin
       SoilOrganic.FineRootLitterIn[iCount] := SoilOrganic.FineRootLitterIn[iCount] / SoilOrganic.FineRootLitterIn[1];
       SoilOrganic.CoarseRootLitterIn[iCount] := SoilOrganic.CoarseRootLitterIn[iCount] / SoilOrganic.CoarseRootLitterIn[1];
       End;
   End
Else  // if not AllOneLayer
   Begin
   SumFineRoot := 0;
   SumCoarseRoot := 0;
   For iCount := 1 to SoilWat.nLayers do
       Begin
       SumFineRoot := SumFineRoot + SoilOrganic.FineRootLitterIn[iCount];
       SumCoarseRoot := SumCoarseRoot + SoilOrganic.CoarseRootLitterIn[iCount];
       End;
   For iCount := 1 to SoilWat.nLayers do
       Begin
       If SumFineRoot > 0 then
          SoilOrganic.FineRootLitterIn[iCount] := SoilOrganic.FineRootLitterIn[iCount] / SumFineRoot
       Else
          SoilOrganic.FineRootLitterIn[iCount] := 1 / SoilWat.nLayers;
       If SumCoarseRoot > 0 then
          SoilOrganic.CoarseRootLitterIn[iCount] := SoilOrganic.CoarseRootLitterIn[iCount] / SumCoarseRoot
       Else
          SoilOrganic.CoarseRootLitterIn[iCount] := 1 / SoilWat.nLayers;
       End;
   End;
Control.SiteHasChanged := true;
End; {of Procedure 'GetLitterDialogueInfo'}

Procedure SetUpSoilWaterDialogue;
var iRow, iDays, iMonths, iYears: Integer;
Begin
// Event Manager - Soil water
List.Caption := 'Set soil water properties';
If SoilWat.SeparateSensitivity then
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
If SoilWat.SeparateSensitivity then
   Begin
   List.Text[1, 4] := 'Stress sensitivity';
   List.Text[1, 5] := 'Relative soil evap';
   End
Else
   List.Text[1, 4] := 'Relative soil evap';
List.nRows := SoilWat.nLayers;
For iRow := 1 to List.nRows do
    Begin
    List.Data[iRow, 0] := SoilWat.Layer[iRow].Depth;
    List.Data[iRow, 1] := SoilWat.Layer[iRow].Pores;
    List.Data[iRow, 2] := SoilWat.Layer[iRow].MaxWater;
    List.Data[iRow, 3] := SoilWat.Layer[iRow].ExtractEffort;
    If SoilWat.SeparateSensitivity then
       Begin
       List.Data[iRow, 4] := SoilWat.Layer[iRow].StressSensitivity;
       List.Data[iRow, 5] := SoilWat.Layer[iRow].RelEvap;
       End
    Else
       List.Data[iRow, 4] := SoilWat.Layer[iRow].RelEvap;
    End;
List.nRadios := 1;
List.RadioOptions[1] := 2;
List.RadioHeading[1] := 'Water-stress sensitivity';
List.RadioText[1, 1] := 'Different by layer';
List.RadioText[1, 2] := 'No separation by layers';
If SoilWat.SeparateSensitivity then
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
    TotalWater, TotalExtract, TotalOM, SumExtract, SumEvap, SumSensitivity: Real48;
    ToggleSensitivity: Boolean;
Begin
If ((List.RbtnSelected[1] = 0) and (SoilWat.SeparateSensitivity = false)) OR
   ((List.RbtnSelected[1] = 1) and (SoilWat.SeparateSensitivity = true)) then
   ToggleSensitivity := true
Else
   ToggleSensitivity := false;
If List.RbtnSelected[1] = 0 then
   SoilWat.SeparateSensitivity := true
Else
   SoilWat.SeparateSensitivity := false;
SoilWat.nLayers := List.nRows;
For iRow := 1 to List.nRows do
    Begin
    SoilWat.Layer[iRow].Depth := List.Data[iRow, 0];
    SoilWat.Layer[iRow].Pores := List.Data[iRow, 1];
    SoilWat.Layer[iRow].MaxWater := List.Data[iRow, 2];
    SoilWat.Layer[iRow].ExtractEffort := List.Data[iRow, 3];
    If SoilWat.SeparateSensitivity and Not ToggleSensitivity then // include sensitivity as just a normal column
       Begin
       SoilWat.Layer[iRow].StressSensitivity := List.Data[iRow, 4];
       SoilWat.Layer[iRow].RelEvap := List.Data[iRow, 5];
       End;
    If Not SoilWat.SeparateSensitivity and Not ToggleSensitivity then // don't include sensitivity and nothing has changed
       SoilWat.Layer[iRow].RelEvap := List.Data[iRow, 4]
    Else if SoilWat.SeparateSensitivity and ToggleSensitivity then // include sensitivity for the first time. So, no data yet
       SoilWat.Layer[iRow].RelEvap := List.Data[iRow, 4]
    Else if Not SoilWat.SeparateSensitivity and ToggleSensitivity then // no longer include sensitivity. Still use the existing data
       Begin
       SoilWat.Layer[iRow].StressSensitivity := List.Data[iRow, 4];
       SoilWat.Layer[iRow].RelEvap := List.Data[iRow, 5];
       End;
    End;
// work out soil water summary
SoilWat.MaxWater := 0;
// make sure relative evaporation rate decreases with depth
List.ShowMessage := false;
For iCount := 1 to SoilWat.nLayers -1 do
    If (SoilWat.Layer[iCount].RelEvap * SoilWat.Layer[iCount + 1].MaxWater) <
       (SoilWat.Layer[iCount + 1].RelEvap * SoilWat.Layer[iCount].MaxWater) then
       Begin
       SoilWat.Layer[iCount + 1].RelEvap := SoilWat.Layer[iCount].RelEvap *
       (SoilWat.Layer[iCount + 1].MaxWater / SoilWat.Layer[iCount].MaxWater);
       List.ShowMessage := true;
       List.Message := 'Invalid sequence for evaporation rate.' + chr(10) +
                       'After correction for water-holding capacity of each layer,' + chr(10) +
                       'upper layers must make a greater or equal relative contribution than lower layers' + chr(10) +
                       'The program is adjusting it!';
       End;
For iCount := 1 to SoilWat.nLayers do
    SoilWat.Layer[iCount].MaxWater := SoilWat.Layer[iCount].Depth * SoilWat.Layer[iCount].Pores * 0.1;
// get the max maxwater and max extract
SumExtract := 0;
SumEvap := 0;
SumSensitivity := 0;
For iCount := 1 to SoilWat.nLayers do
    Begin
    SoilWat.MaxWater := SoilWat.MaxWater + SoilWat.Layer[iCount].MaxWater;
    SumExtract := SumExtract + SoilWat.Layer[iCount].ExtractEffort;
    SumEvap := SumEvap + SoilWat.Layer[iCount].RelEvap;
    SumSensitivity := SumSensitivity + SoilWat.Layer[iCount].StressSensitivity;
    End;
// make sure total water is valid
If (TotalWater > SoilWat.MaxWater) then
   Begin
   For iCount := 1 to SoilWat.nLayers do
       SoilWat.Layer[iCount].WaterContent := SoilWat.Layer[iCount].MaxWater;
   SoilWat.TotalWater := SoilWat.MaxWater;
   End
Else if (SoilWat.MaxWater = 0) then
   Begin
   For iCount:=1 to SoilWat.nLayers do
       SoilWat.Layer[iCount].WaterContent := 0;
   SoilWat.TotalWater := 0;
   End
Else {TotalWater < SoilWat.MaxWater then}
   Begin
   For iCount:=1 to SoilWat.nLayers do
       SoilWat.Layer[iCount].WaterContent := SoilWat.Layer[iCount].MaxWater * TotalWater / SoilWat.MaxWater;
   SoilWat.TotalWater := TotalWater;
   End;
If SumExtract > 0 then
   Begin
   For iCount := 1 to SoilWat.nLayers do
       SoilWat.Layer[iCount].ExtractEffort := SoilWat.Layer[iCount].ExtractEffort / SumExtract;
   End
Else
   Begin
   For iCount := 1 to SoilWat.nLayers do
       Begin
       If SoilWat.nLayers = 1 then  {There has only one layer been defined}
          SoilWat.Layer[iCount].ExtractEffort := 1
       Else if iCount = 1 then  {The top layer gets half}
          SoilWat.Layer[iCount].ExtractEffort := 0.5
       Else if iCount = SoilWat.nLayers then  {The bottom layer gets the last bit}
          SoilWat.Layer[iCount].ExtractEffort := SoilWat.Layer[iCount - 1].ExtractEffort
       Else    {For the layers between the top and lowest ones, it decreases binomially}
          SoilWat.Layer[iCount].ExtractEffort := 0.5 * SoilWat.Layer[iCount - 1].ExtractEffort;
       End;
   End;
If SumEvap > 0 then
   Begin
   For iCount := 1 to SoilWat.nLayers do
       SoilWat.Layer[iCount].RelEvap := SoilWat.Layer[iCount].RelEvap / SumEvap;
   End
Else
   Begin
   SoilWat.Layer[1].RelEvap := 1;
   For iCount := 2 to SoilWat.nLayers do
       SoilWat.Layer[iCount].RelEvap := 0;
   End;
If SumSensitivity > 0 then
   Begin
   For iCount := 1 to SoilWat.nLayers do
       SoilWat.Layer[iCount].StressSensitivity := SoilWat.Layer[iCount].StressSensitivity / SumSensitivity;
   End
Else
   Begin
   For iCount := 1 to SoilWat.nLayers do
       SoilWat.Layer[iCount].StressSensitivity := SoilWat.Layer[iCount].MaxWater / SoilWat.MaxWater;
   End;
Control.SiteHasChanged := true;
End; {of Procedure 'GetSoilWaterDialogueInfo'}

Procedure SetUpPhenologyDialogue;
var iRow, iDays, iMonths, iYears: Integer;
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

Procedure SwapReal(var a, b: real48);
Var x: real48;
    Begin
    x := a;
    a := b;
    b := x;
    End {of procedure 'SwapReal'};

Procedure SwapLong(var a, b: LongInt);
Var x: LongInt;
    Begin
    x := a;
    a := b;
    b := x;
    End {of procedure 'SwapLong'};

Procedure SwapBoolean(var a, b: Boolean);
Var x: Boolean;
    Begin
    x := a;
    a := b;
    b := x;
    End {of procedure 'SwapBoolean'};

Procedure SetUpSens;
Var DummyVar: real48;
    iLayer: Integer;

    Procedure ProcessParameters (var Par: real48; LowLimit, UpLimit: real48; Comment: string);
    Const SensitivityTest = 0.25;
          CommentLen = 38;
    var LeafN, LeafC: real48;
        i: integer;
        SensVar: sensitivityType;

    Procedure WriteOut (VarOut: real48; Comment: string);
    var Width, Digits: integer;
        Begin
        GetField(VarOut, 7, Width, Digits);
        Writeln (Comment: CommentLen-15, VarOut:Width:Digits);
        GetField(VarOut, 10, Width, Digits);
        Writeln (Control.SensFile, Comment: CommentLen, VarOut:Width:Digits);
        End; {of Procedure 'WriteOut'}

    Procedure HeadingOut (Comment: string);
        Begin
        Write (Comment: 8);
        Write (Control.SensFile, Comment: 11);
        End; {of Procedure 'HeadingOut'}

    Begin
    LeafC := Plant.Leaves[C];
    LeafN := Plant.Leaves[N];
    If (Control.SensParameter = Dummy) then
       Begin
       Control.SensFileOpen := true;
       assign (Control.SensFile, 'SensTest.out');
       rewrite (Control.SensFile);
       Writeln ('                  Sensitivity Test');
       Writeln (Control.SensFile, '                  Sensitivity Test');
       Writeln ('Each parameter variable altered by +/-', 100*SensitivityTest:1:0, '%');
       Writeln (Control.SensFile, 'Each parameter variable altered by +/-', 100*SensitivityTest:1:0, '%');
       Writeln ('Values at beginning of run: ');
       Writeln (Control.SensFile, 'Values at beginning of run: ');
       For SensVar := CAI to ShowResistantN do
           If TestSens.Choose[SensVar] then
              Case SensVar of
                   CAI: WriteOut(0, 'Current annual increment (n/a): ');
                   NConc: WriteOut(0, 'N concentration (n/a): ');
                   LAI: WriteOut(0, 'LAI (n/a): ');
                   Wood: WriteOut((Plant.SapWood[C] + Plant.HeartWood[C]) * Control.CConversion,
                         'Total wood: ');
                   ShowFineRootC: WriteOut(Plant.FineRoot[C] * Control.CConversion, 'Fine root C: ');
                   ShowFineRootN: WriteOut(Plant.FineRoot[C], 'Fine root N: ');
                   ShowBranches: WriteOut(Plant.Branches[C] * Control.CConversion, 'Branch C: ');
                   ShowLeaves: WriteOut(LeafC * Control.CConversion, 'Foliage C: ');
                   ShowLeafN: WriteOut(LeafN, 'Foliage N: ');
                   Height: WriteOut(Plant.Height, 'Stand height: ');
                   DBH: WriteOut(Plant.DBH, 'DBH: ');
                   CanopyCover: WriteOut(Plant.CanopyCover, 'Canopy cover: ');
                   kex: WriteOut(Plant.kex, 'Light ext. coeff.: ');
                   End;
       Write ('Parameter or pool changed':CommentLen-15, ' ');
       Write (Control.SensFile, 'Parameter or pool changed':CommentLen, ' ');
       For i := 1 to 2 do
         For SensVar := CAI to ShowResistantN do
           If TestSens.Choose[SensVar] then
              Case SensVar of
                   CAI: HeadingOut('CAI');
                   NConc: HeadingOut('N conc');
                   LAI: HeadingOut('LAI');
                   Wood: HeadingOut('Wood');
                   ShowFineRootC: HeadingOut('Fine rt C');
                   ShowFineRootN: HeadingOut('Fine rt N');
                   ShowBranches: HeadingOut('Branch C');
                   ShowLeaves: HeadingOut('Foliage C');
                   ShowLeafN: HeadingOut('Foliage N');
                   Height: HeadingOut('Height');
                   DBH: HeadingOut('DBH');
                   CanopyCover: HeadingOut('Canopy cover');
                   kex: HeadingOut('Light ext. coeff.');
                   End;
       Writeln;
       Writeln (Control.SensFile);
       End
    Else
       Begin
       If (Control.SensFlag = -1) then  {First test for variable with decreased value}
          Begin
          Control.OldPar := Par;
          If Control.SensParameter <> EndDummy then
             Begin
             Write (Comment:CommentLen-15, ' ');
             Write (Control.SensFile, Comment:CommentLen, ' ');
             End;
          End
       Else If Control.SensParameter <> EndInputs then
          Par := Control.OldPar;
       End;
    Par := (1 + Control.SensFlag * SensitivityTest) * Control.OldPar;
    If Par < LowLimit then
       Begin
       Write (' (L) ');
       Par := LowLimit;
       End;
    If Par > UpLimit then
       Begin
       Write (' (U) ');
       Par := UpLimit;
       End;
    End; {of Procedure 'ProcessParameters'}

    Procedure ProcessSoilPools (var Par: real48; iLayer: Integer; LowLimit, UpLimit: real48; Comment: string);
    Const SensitivityTest = 0.25;
          CommentLen = 38;
    var i: integer;
        SensVar: sensitivityType;

    Procedure WriteOut (VarOut: real48; Comment: string);
    var Width, Digits: integer;
        Begin
        GetField(VarOut, 7, Width, Digits);
        Writeln (Comment: CommentLen-15, VarOut:Width:Digits);
        GetField(VarOut, 10, Width, Digits);
        Writeln (Control.SensFile, Comment: CommentLen, VarOut:Width:Digits);
        End; {of Procedure 'WriteOut'}

    Procedure HeadingOut (Comment: string);
        Begin
        Write (Comment: 8);
        Write (Control.SensFile, Comment: 11);
        End; {of Procedure 'HeadingOut'}

    Begin
    If (Control.SensParameter = Dummy) then
       Begin
       Control.SensFileOpen := true;
       assign (Control.SensFile, 'SensTest.out');
       rewrite (Control.SensFile);
       Writeln ('                  Sensitivity Test');
       Writeln (Control.SensFile, '                  Sensitivity Test');
       Writeln ('Each parameter variable altered by +/-', 100*SensitivityTest:1:0, '%');
       Writeln (Control.SensFile, 'Each parameter variable altered by +/-', 100*SensitivityTest:1:0, '%');
       Writeln ('Values at beginning of run: ');
       Writeln (Control.SensFile, 'Values at beginning of run: ');
       For SensVar := CAI to ShowResistantN do
           If TestSens.Choose[SensVar] then
              Case SensVar of
                   ShowFineWoodSurfC: WriteOut(SoilOrganic.FineWood[iLayer, C], 'Fine wood surface litter: ');
                   ShowCoarseWoodSurfC: WriteOut(SoilOrganic.CoarseWood[iLayer, C], 'Coarse wood surface litter: ');
                   ShowCoarseWoodSoilC: WriteOut(SoilOrganic.CoarseWood[iLayer, C], 'Coarse wood soil litter: ');
                   ShowStructSurfC: WriteOut(SoilOrganic.Struct[iLayer, C], 'Structural surface litter: ');
                   ShowMetabSurfC: WriteOut(SoilOrganic.Metab[iLayer, C], 'Metabolic surface litter: ');
                   ShowStructSurfN: WriteOut(SoilOrganic.Struct[iLayer, C], 'Structural soil litter: ');
                   ShowMetabSurfN: WriteOut(SoilOrganic.Metab[iLayer, C], 'Metabolic surface litter N: ');
                   ShowSlowC: WriteOut(SoilOrganic.Slow[iLayer, C], 'Slow organic C: ');
                   ShowActiveC: WriteOut(SoilOrganic.Active[iLayer, C], 'Active organic C: ');
                   ShowResistantC: WriteOut(SoilOrganic.Resistant[iLayer, C], 'Resistant organic C: ');
                   ShowSlowN: WriteOut(SoilOrganic.Slow[iLayer, N], 'Slow organic N: ');
                   ShowActiveN: WriteOut(SoilOrganic.Active[iLayer, N], 'Active organic N at beginning of run: ');
                   ShowResistantN: WriteOut(SoilOrganic.Resistant[iLayer, N], 'Resistant organic N: ');
                   End;
       Write ('Parameter or pool changed':CommentLen-15, ' ');
       Write (Control.SensFile, 'Parameter or pool changed':CommentLen, ' ');
       For i := 1 to 2 do
         For SensVar := CAI to ShowResistantN do
           If TestSens.Choose[SensVar] then
              Case SensVar of
                   ShowFineWoodSurfC: HeadingOut('Fn wd srf');
                   ShowCoarseWoodSurfC: HeadingOut('Crs wd srf');
                   ShowCoarseWoodSoilC: HeadingOut('Crs wd sl');
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
       Writeln;
       Writeln (Control.SensFile);
       End
    Else
       Begin
       If (Control.SensFlag = -1) then  {First test for variable with decreased value}
          Begin
          Control.OldPar := Par;
          If Control.SensParameter <> EndDummy then
             Begin
             Write (Comment:CommentLen-15, ' ');
             Write (Control.SensFile, Comment:CommentLen, ' ');
             End;
          End
       Else If Control.SensParameter <> EndInputs then
          Par := Control.OldPar;
       End;
    Par := (1 + Control.SensFlag * SensitivityTest) * Control.OldPar;
    If Par < LowLimit then
       Begin
       Write (' (L) ');
       Par := LowLimit;
       End;
    If Par > UpLimit then
       Begin
       Write (' (U) ');
       Par := UpLimit;
       End;
    End; {of Procedure 'ProcessSoilPools'}

    Begin
    {This routine starts with the values of 'SensFlag' = -1 and 'SensParameter' = Dummy.
     'SensParameter' indicates which variable is to be tested and 'SensFlag' indicates whether
     the variable is to be decreased (-1) or increased (+1). The routine below firstly resets
     the variable to its previous value (held in 'OldVar'). 'SensParameter' is then incremented to
     the next variable that is to be tested (with a repeat .. statement. The last variable that is
     notionally to be tested is 'EndDummy'. The program then calls the procedure 'ProcessParameters'
     which then checks whether the variable is either the notionally first (Dummy) or last (EndDummy)
     or a particular other variable, does some change in the value of the variable, including range
     checking and handles some output. Finally, this routine checks whether the end of the run
     has been reached, and if so sets 'SensitivityTestOn' to false. Otherwise, it merely toggles
     'SensFlag' between the values of +1 and -1.

     The routine has been changed numerous times to accomodate changes made to the program,
     most significantly to introduce multi-layered soil organic matter.
     Changes were made to stop the program returning run-time errors, but the workings
     this routine have not been checked, and it is likely that there will be errors in it.}
     If Control.SensFlag = -1 then
       Begin
       Case Control.SensParameter of
         FineSoil: Parameter.FineSoil := Control.OldPar;
         MeanSoilTemp: Parameter.MeanSoilTemp := Control.OldPar;
         CO2: Parameter.CO2Conc := Control.OldPar;
         AtmosPressure: Parameter.AtmosPressure := Control.OldPar;
         WoodLignin: Parameter.WoodLignin := Control.OldPar;
         LeafLignin: Parameter.LeafLignin := Control.OldPar;
         RootLignin: Parameter.RootLignin := Control.OldPar;
         AnnualRain: Parameter.AnnualRain := Control.OldPar;
         RainProb: Parameter.RainProb := Control.OldPar;
         MeanTmax: Parameter.MeanTmax := Control.OldPar;
         MeanTmin: Parameter.MeanTmin := Control.OldPar;
         MeanRadn: Parameter.MeanRadn := Control.OldPar;
         MeanAbsHum: Parameter.MeanAbsHum := Control.OldPar;
         Latitude: Parameter.Latitude := Control.OldPar;
         FertiliserRelease:Parameter.FertiliserRelease := Control.OldPar;
         FertilityAdjust: Parameter.FertilityAdjust := Control.OldPar;
         RateAdjust: Parameter.RateAdjust := Control.OldPar;
         TMaxRepairTime: Parameter.TMaxRepairTime := Control.OldPar;
         Immobilise: Parameter.Immobilise := Control.OldPar;
         DirectEvapSlope: Parameter.DirectEvapSlope := Control.OldPar;
         DirectEvapFract: Parameter.DirectEvapFract := Control.OldPar;
         Decay8: Parameter.Decay8 := Control.OldPar;
         Decay9: Parameter.Decay9 := Control.OldPar;
         RelativeCN: Parameter.RelativeCN := Control.OldPar;
         CriticalCN: Parameter.CriticalCN := Control.OldPar;
         Transmit: Parameter.Transmit := Control.OldPar;
         BallBerry1: Parameter.BallBerry1 := Control.OldPar;
         BallBerry2: Parameter.BallBerry2 := Control.OldPar;
         Nloss: Parameter.Nloss := Control.OldPar;
         Leaching: Parameter.Leaching := Control.OldPar;
         MicroFract: Parameter.MicroFract := Control.OldPar;
         Atmos_N: Parameter.Atmos_N := Control.OldPar;
         BiolFix: Parameter.BiolFix := Control.OldPar;
         GrowthRespn: Parameter.GrowthRespn := Control.OldPar;
         BarkSenesc: Parameter.BarkSenesc := Control.OldPar;
         RespFromN: Parameter.RespFromN := Control.OldPar;
         StemDeath: Parameter.StemDeath := Control.OldPar;
         SenescLeafRatio: Parameter.SenescLeafRatio := Control.OldPar;
         InternalNRatio: Parameter.InternalNRatio := Control.OldPar;
         LeafSenesc: Parameter.LeafSenesc := Control.OldPar;
         BranchSenesc: Parameter.BranchSenesc := Control.OldPar;
         RootSenesc: Parameter.RootSenesc := Control.OldPar;
         FruitSenesc: Parameter.FruitSenesc := Control.OldPar;
         PollenSenesc: Parameter.PollenSenesc := Control.OldPar;
         SenescLowLight: Parameter.SenescLowLight := Control.OldPar;
         MaxSenescLowLight: Parameter.MaxSenescLowLight := Control.OldPar;
         RootLeafRatio1: Parameter.RootLeafRatio1 := Control.OldPar;
         RootLeafRatio2: Parameter.RootLeafRatio2 := Control.OldPar;
         WoodBranchRatio: Parameter.WoodBranchRatio := Control.OldPar;
         LeafBranchRatio: Parameter.LeafBranchRatio := Control.OldPar;
         BarkWoodRatio: Parameter.BarkWoodRatio := Control.OldPar;
         CoarseRootWoodRatio: Parameter.CoarseRootWoodRatio := Control.OldPar;
         C_FruitAlloc: Parameter.C_FruitAlloc := Control.OldPar;
         C_PollenAlloc: Parameter.C_PollenAlloc := Control.OldPar;
         SLA: Parameter.SLA := Control.OldPar;
         Amax: Parameter.Amax := Control.OldPar;
         Theta: Parameter.Theta := Control.OldPar;
         Kexmax: Parameter.Kexmax := Control.OldPar;
         KlowRange: Parameter.KlowRAnge := Control.OldPar;
         Albedo: Parameter.Albedo := Control.OldPar;
         Temp_Amplitude: Parameter.Temp_Amplitude := Control.OldPar;
         Radn_Amplitude: Parameter.Radn_Amplitude := Control.OldPar;
         Daily_Amplitude: Parameter.Daily_Amplitude := Control.OldPar;
         Humid_Amplitude: Parameter.Humid_Amplitude := Control.OldPar;
         DrySenesc: Parameter.DrySenesc := Control.OldPar;
         StressLimit: Parameter.StressLimit := Control.OldPar;
         SoilEvap: Parameter.SoilEvap := Control.OldPar;
         HDInter: Parameter.HDInter := Control.OldPar;
         HDSlope: Parameter.HDSlope := Control.OldPar;
         WDSlope: Parameter.WDSlope := Control.OldPar;
         WHSlope: Parameter.WHSlope := Control.OldPar;
         TMinLim: Parameter.TMinLim := Control.OldPar;
         TOpt1: Parameter.TOpt1 := Control.OldPar;
         TOpt2: Parameter.TOpt2 := Control.OldPar;
         TmaxLim: Parameter.TMaxLim := Control.OldPar;
         TFrost: Parameter.TFrost := Control.OldPar;
         TScorch: Parameter.TScorch := Control.OldPar;
         TSensitivity: Parameter.TSensitivity := Control.OldPar;
         TRepair: Parameter.TRepair := Control.OldPar;
         WoodRetrans: Parameter.WoodRetrans := Control.OldPar;
         bRoots: Parameter.bRoots := Control.OldPar;
         bWood: Parameter.bWood := Control.OldPar;
         bBark: Parameter.bBark := Control.OldPar;
         bBranch: Parameter.bBranch := Control.OldPar;
         bFruit: Parameter.bFruit := Control.OldPar;
         bPollen: Parameter.bPollen := Control.OldPar;
         RelWaterSens: Parameter.RelWaterSens := Control.OldPar;
         N0: Parameter.N0 := Control.OldPar;
         Ncrit: Parameter.NCrit := Control.OldPar;
         Nmax: Parameter.Nmax := Control.OldPar;
         SapWoodC: Plant.SapWood[C] := Control.OldPar;
         HeartWoodC: Plant.HeartWood[C] := Control.OldPar;
         CoarseRootC: Plant.CoarseRoot[C] := Control.OldPar;
         FineRootC: Plant.FineRoot[C] := Control.OldPar;
         BranchesC: Plant.Branches[C] := Control.OldPar;
         BarkC: Plant.Bark[C] := Control.OldPar;
         LeavesC: Plant.Leaves[C] := Control.OldPar;
         SapWoodN: Plant.SapWood[N] := Control.OldPar;
         HeartWoodN: Plant.HeartWood[N] := Control.OldPar;
         CoarseRootN: Plant.CoarseRoot[N] := Control.OldPar;
         FineRootN: Plant.FineRoot[N] := Control.OldPar;
         BranchesN: Plant.Branches[N] := Control.OldPar;
         BarkN: Plant.Bark[N] := Control.OldPar;
         LeavesN: Plant.Leaves[N] := Control.OldPar;
         Stocking: Plant.Stocking := Control.OldPar;
         FineWoodSurfC:
             For iLayer := 1 to SoilWat.nLayers do
                 SoilOrganic.FineWood[iLayer, C] := Control.OldSoilPar[iLayer];
         CoarseWoodSurfC:
             For iLayer := 1 to SoilWat.nLayers do
                 SoilOrganic.CoarseWood[iLayer, C] := Control.OldSoilPar[iLayer];
         CoarseWoodSoilC:
             For iLayer := 1 to SoilWat.nLayers do
                 SoilOrganic.CoarseWood[iLayer, C] := Control.OldSoilPar[iLayer];
         StructSurfC:
             For iLayer := 1 to SoilWat.nLayers do
                 SoilOrganic.Struct[iLayer, C] := Control.OldSoilPar[iLayer];
         StructSoilC:
             For iLayer := 1 to SoilWat.nLayers do
                 SoilOrganic.Struct[iLayer, C] := Control.OldSoilPar[iLayer];
         MetabSurfC:
             For iLayer := 1 to SoilWat.nLayers do
                 SoilOrganic.Metab[iLayer, C] := Control.OldSoilPar[iLayer];
         MetabSoilC:
             For iLayer := 1 to SoilWat.nLayers do
                 SoilOrganic.Metab[iLayer, C] := Control.OldSoilPar[iLayer];
         SlowC:
             For iLayer := 1 to SoilWat.nLayers do
                 SoilOrganic.Slow[iLayer, C] := Control.OldSoilPar[iLayer];
         ActiveC:
             For iLayer := 1 to SoilWat.nLayers do
                 SoilOrganic.Active[iLayer, C] := Control.OldSoilPar[iLayer];
         ResistantC:
             For iLayer := 1 to SoilWat.nLayers do
                 SoilOrganic.Resistant[iLayer, C] := Control.OldSoilPar[iLayer];
         FineWoodSurfN:
             For iLayer := 1 to SoilWat.nLayers do
                 SoilOrganic.FineWood[iLayer, N] := Control.OldSoilPar[iLayer];
         CoarseWoodSurfN:
             For iLayer := 1 to SoilWat.nLayers do
                 SoilOrganic.CoarseWood[iLayer, N] := Control.OldSoilPar[iLayer];
         CoarseWoodSoilN:
             For iLayer := 1 to SoilWat.nLayers do
                 SoilOrganic.CoarseWood[iLayer, N] := Control.OldSoilPar[iLayer];
         StructSurfN:
             For iLayer := 1 to SoilWat.nLayers do
                 SoilOrganic.Struct[iLayer, N] := Control.OldSoilPar[iLayer];
         StructSoilN:
             For iLayer := 1 to SoilWat.nLayers do
                 SoilOrganic.Struct[iLayer, N] := Control.OldSoilPar[iLayer];
         MetabSurfN:
             For iLayer := 1 to SoilWat.nLayers do
                 SoilOrganic.Metab[iLayer, N] := Control.OldSoilPar[iLayer];
         MetabSoilN:
             For iLayer := 1 to SoilWat.nLayers do
                 SoilOrganic.Metab[iLayer, N] := Control.OldSoilPar[iLayer];
         SlowN:
             For iLayer := 1 to SoilWat.nLayers do
                 SoilOrganic.Slow[iLayer, N] := Control.OldSoilPar[iLayer];
         ActiveN:
             For iLayer := 1 to SoilWat.nLayers do
                 SoilOrganic.Active[iLayer, N] := Control.OldSoilPar[iLayer];
         ResistantN:
             For iLayer := 1 to SoilWat.nLayers do
                 SoilOrganic.Resistant[iLayer, N] := Control.OldSoilPar[iLayer];
         End;
      Repeat
         Control.SensParameter := Succ(Control.SensParameter);
      Until TestSens.Choose[Control.SensParameter];
      End;
    If Control.SensitivityTestOn then
      Case Control.SensParameter of
         Dummy: ProcessParameters (DummyVar, 0, 1, '');
         FineSoil: ProcessParameters (Parameter.FineSoil, 0, 1, 'FineSoil');
         MeanSoilTemp: ProcessParameters (Parameter.MeanSoilTemp, -100, 100, 'MeanSoilTemp');
         CO2: ProcessParameters (Parameter.CO2Conc, 0, 1000000, 'CO2 concentration');
         AtmosPressure: ProcessParameters (Parameter.AtmosPressure, 0, 100000, 'Atmos. pressure');
         WoodLignin: ProcessParameters (Parameter.WoodLignin, 0, 1, 'Wood lignin');
         LeafLignin: ProcessParameters (Parameter.LeafLignin, 0, 1, 'Leaf lignin');
         RootLignin: ProcessParameters (Parameter.RootLignin, 0, 1, 'Root lignin');
         AnnualRain: ProcessParameters (Parameter.AnnualRain, 0, 100000, 'Annual rain');
         RainProb: ProcessParameters (Parameter.RainProb, 0, 1, 'Rain probability') ;
         MeanTmax: ProcessParameters (Parameter.MeanTmax, -100, 100, 'Annual mean temperature');
         MeanTmin: ProcessParameters (Parameter.MeanTmin, -100, 100, 'Annual mean minimum temperature');
         MeanRadn: ProcessParameters (Parameter.MeanRadn, 0, 100, 'Annual mean daily radiation');
         MeanAbsHum: ProcessParameters (Parameter.MeanAbsHum, 0, 100, 'Annual mean absolute humidity');
         Latitude: ProcessParameters (Parameter.Latitude, -90, 90, 'Latitude');
         FertiliserRelease: ProcessParameters(Parameter.FertiliserRelease, 0, 1, 'Fertiliser release rate');
         FertilityAdjust: ProcessParameters(Parameter.FertilityAdjust, 0, 100, 'Fertility adjustment');
         RateAdjust: ProcessParameters(Parameter.RateAdjust, 0, 100, 'Decomposition rate adjustment');
         TMaxRepairTime: ProcessParameters(Parameter.TMaxRepairTime, 0, 10000, 'Maximum length for T damage repair');
         Immobilise: ProcessParameters(Parameter.Immobilise, 0, 1, 'Fraction of mineral N immobilised');
         DirectEvapSlope: ProcessParameters (Parameter.DirectEvapSlope, 0, 10000, 'Slope of foliage evaporation function');
         DirectEvapFract: ProcessParameters(Parameter.DirectEvapFract, 0, 1, 'Fraction of rain evaporating');
         Decay8: ProcessParameters (Parameter.Decay8, 0, 10000, 'Decomposition constant 8');
         Decay9: ProcessParameters (Parameter.Decay9, 0, 10000, 'Decomposition constant 9');
         RelativeCN: ProcessParameters (Parameter.RelativeCN, 0, 10000, 'Structural-metabolic rel. C:N');
         CriticalCN: ProcessParameters (Parameter.CriticalCN, 0, 100, 'Critical C:N');
         Transmit: ProcessParameters (Parameter.Transmit, 0, 1, 'Light transmission');
         BallBerry1: ProcessParameters (Parameter.BallBerry1, 0, 100000, 'Ball-Berry constant unstressed');
         BallBerry2: ProcessParameters (Parameter.BallBerry2, 0, 100000, 'Ball-Berry constant stressed');
         Nloss: ProcessParameters (Parameter.Nloss, 0, 1, 'N-loss fraction');
         Leaching: ProcessParameters (Parameter.Leaching, 0, 1, 'Leaching loss fraction');
         MicroFract: ProcessParameters (Parameter.MicroFract, 0, 1, 'Mycorrhizal uptake fraction');
         Atmos_N: ProcessParameters (Parameter.Atmos_N, 0, 100000, 'Atmospheric nitrogen input');
         BiolFix: ProcessParameters (Parameter.BiolFix, 0, 1, 'Biological nitrogen fixation term');
         GrowthRespn: ProcessParameters (Parameter.GrowthRespn, 0, 10, 'Growth respiration term');
         BarkSenesc: ProcessParameters (Parameter.BarkSenesc, 0, 10, 'Bark senescence');
         RespFromN: ProcessParameters (Parameter.RespFromN, 0, 100000, 'Maintenance respiration term');
         StemDeath: ProcessParameters (Parameter.StemDeath, 0, 1, 'Mortality term');
         SenescLeafRatio: ProcessParameters (Parameter.SenescLeafRatio, 0, 1, 'Leaf senescence [N] ratio');
         InternalNRatio: ProcessParameters (Parameter.InternalNRatio, 0, 1, 'Crown [N] ratio');
         LeafSenesc: ProcessParameters (Parameter.LeafSenesc, 0, 1, 'Leaf senescence rate');
         BranchSenesc: ProcessParameters (Parameter.BranchSenesc, 0, 1, 'Branch senescence rate');
         RootSenesc: ProcessParameters (Parameter.RootSenesc, 0, 1, 'Root senescence rate');
         FruitSenesc: ProcessParameters (Parameter.FruitSenesc, 0, 1, 'Fruit senescence rate');
         PollenSenesc: ProcessParameters (Parameter.PollenSenesc, 0, 1, 'Pollen senescence rate');
         SenescLowLight: ProcessParameters (Parameter.SenescLowLight, 0, 1000, 'Threshold for low-light senescence');
         MaxSenescLowLight: ProcessParameters (Parameter.MaxSenescLowLight, 0, 1, 'Maximum low-light senescence rate');
         RootLeafRatio1: ProcessParameters (Parameter.RootLeafRatio1, 0, 1000, 'Root:leaf ratio unstressed');
         RootLeafRatio2: ProcessParameters (Parameter.RootLeafRatio2, 0, 1000, 'Root:leaf ratio stressed');
         WoodBranchRatio: ProcessParameters (Parameter.WoodBranchRatio, 0, 1000, 'Wood:branch ratio');
         LeafBranchRatio: ProcessParameters (Parameter.LeafBranchRatio, 0, 1000, 'Leaf:branch ratio');
         BarkWoodRatio: ProcessParameters (Parameter.BarkWoodRatio, 0, 1000, 'Bark:wood ratio');
         CoarseRootWoodRatio: ProcessParameters (Parameter.CoarseRootWoodRatio, 0, 1000, 'Coarse root:wood ratio');
         C_FruitAlloc: ProcessParameters (Parameter.C_FruitAlloc, 0, 1, 'Fruit allocation');
         C_PollenAlloc: ProcessParameters (Parameter.C_PollenAlloc, 0, 1, 'Pollen allocation');
         SLA: ProcessParameters (Parameter.SLA, 0, 1000, 'Specific leaf area');
         Amax: ProcessParameters (Parameter.Amax, 0, 1000, 'Maximum assimilation rate');
         Theta: ProcessParameters (Parameter.Theta, 0, 1, 'Curvature term in assimilation');
         Kexmax: ProcessParameters (Parameter.Kexmax, 0, 100, 'Max. light extinction coefficient');
         KlowRange: ProcessParameters (Parameter.KlowRange, 0, 100, 'Max. decrease of max. light extinction coefficient');
         Albedo: ProcessParameters (Parameter.Albedo, 0, 1, 'Albedo');
         Temp_Amplitude: ProcessParameters (Parameter.Temp_Amplitude, 0, 100, 'Annual temperature amplitude');
         Radn_Amplitude: ProcessParameters (Parameter.Radn_Amplitude, 0, 100, 'Annual radiation amplitude');
         Daily_Amplitude: ProcessParameters (Parameter.Daily_Amplitude, 0, 100, 'Diurnal temperature amplitude');
         Humid_Amplitude: ProcessParameters (Parameter.Humid_Amplitude, 0, 100, 'Annual absolute humidity amplitude');
         DrySenesc: ProcessParameters (Parameter.DrySenesc, 0, 1, 'Maximum drought senescence rate');
         StressLimit: ProcessParameters (Parameter.StressLimit, 0, 1, 'Soil water content that starts stress');
         SoilEvap: ProcessParameters (Parameter.SoilEvap, 0, 1000, 'Soil evaporation scaling term');
         HDInter: ProcessParameters (Parameter.HDInter, -1000, 1000, 'Intercept in height-diameter rel.');
         HDSlope: ProcessParameters (Parameter.HDSlope, 0, 10, 'Slope in height-diameter rel.');
         WDSlope: ProcessParameters (Parameter.WDSlope, -1000, 1000, 'Slope in diameter-weight rel.');
         WHSlope: ProcessParameters (Parameter.WHSlope, 0, 10, 'Slope in weight-height rel.');
         TMinLim: ProcessParameters (Parameter.TMinLim, -100, 100, 'Minimum T for photosynthesis');
         TOpt1: ProcessParameters (Parameter.TOpt1, -100, 100, 'Lower optimum T for photosynthesis');
         TOpt2: ProcessParameters (Parameter.TOpt2, -100, 100, 'Upper optimum T for photosynthesis');
         TmaxLim: ProcessParameters (Parameter.TMaxLim, -100, 100, 'Maximum T for photosynthesis');
         TFrost: ProcessParameters (Parameter.TFrost, -100, 100, 'Threshold for frost damage');
         TScorch: ProcessParameters (Parameter.TScorch, -100, 100, 'Threshold for high temperature damage');
         TSensitivity: ProcessParameters (Parameter.TSensitivity, 0, 1000, 'Sensitivity to temperature damage');
         TRepair: ProcessParameters (Parameter.TRepair, 0, 1000, 'Repair speed of temperature damage');
         WoodRetrans: ProcessParameters (Parameter.WoodRetrans, 0, 1, 'Heartwood-sapwood [N] ratio');
         bRoots: ProcessParameters (Parameter.bRoots, 0, 100, '[N] ratio roots:leaves');
         bWood: ProcessParameters (Parameter.bWood, 0, 100, '[N] ratio wood:leaves');
         bBark: ProcessParameters (Parameter.bBark, 0, 100, '[N] ratio bark:leaves');
         bBranch: ProcessParameters (Parameter.bBranch, 0, 100, '[N] ratio branches:leaves');
         bFruit: ProcessParameters (Parameter.bFruit, 0, 100, '[N] ratio fruits:leaves');
         bPollen: ProcessParameters (Parameter.bPollen, 0, 100, '[N] ratio pollen:leaves');
         RelWaterSens: ProcessParameters (Parameter.RelWaterSens, 0, 10, 'Decomp/plant resp. to water stress');
         N0: ProcessParameters (Parameter.N0, 0, 100, 'minimum foliar [N] to start photosynth');
         Ncrit: ProcessParameters (Parameter.NCrit, 0, 100, 'foliar [N] that saturates photosynth');
         Nmax: ProcessParameters (Parameter.Nmax, 0, 100, 'maximum possible foliar [N]');
         SapWoodC: ProcessParameters (Plant.SapWood[C], 0, 1E8, 'Sapwood carbon');
         HeartWoodC: ProcessParameters (Plant.HeartWood[C], 0, 1E8, 'Heartwood carbon');
         CoarseRootC: ProcessParameters (Plant.CoarseRoot[C], 0, 1E8, 'Coarseroot carbon');
         FineRootC: ProcessParameters (Plant.FineRoot[C], 0, 1E8, 'Fineroot carbon');
         BranchesC: ProcessParameters (Plant.Branches[C], 0, 1E8, 'Branch carbon');
         BarkC: ProcessParameters (Plant.Bark[C], 0, 1E8, 'Bark carbon');
         LeavesC: ProcessParameters (Plant.Leaves[C], 0, 1E8, 'Foliage carbon');
         SapWoodN: ProcessParameters (Plant.SapWood[N], 0, 1E8, 'Sapwood nitrogen');
         HeartWoodN: ProcessParameters (Plant.HeartWood[N], 0, 1E8, 'Heartwood nitrogen');
         CoarseRootN: ProcessParameters (Plant.CoarseRoot[N], 0, 1E8, 'Coarseroot nitrogen');
         FineRootN: ProcessParameters (Plant.FineRoot[N], 0, 1E8, 'Fineroot nitrogen');
         BranchesN: ProcessParameters (Plant.Branches[N], 0, 1E8, 'Branch nitrogen');
         BarkN: ProcessParameters (Plant.Bark[N], 0, 1E8, 'Bark nitrogen');
         LeavesN: ProcessParameters (Plant.Leaves[N], 0, 1E8, 'Foliage nitrogen');
         Stocking: ProcessParameters (Plant.Stocking, 0, 1000000, 'Stocking');
         FineWoodSurfC:
             For iLayer := 1 to SoilWat.nLayers do
                 ProcessSoilPools (SoilOrganic.FineWood[iLayer, C], iLayer, 0, 1E8, 'Fine wood surface litter carbon');
         CoarseWoodSurfC:
             For iLayer := 1 to SoilWat.nLayers do
                 ProcessParameters (SoilOrganic.CoarseWood[iLayer, C], 0, 1E8, 'Coarse wood surface litter carbon');
         CoarseWoodSoilC:
             For iLayer := 1 to SoilWat.nLayers do
                 ProcessParameters (SoilOrganic.CoarseWood[iLayer, C], 0, 1E8, 'Coarse wood soil litter carbon');
         StructSurfC:
             For iLayer := 1 to SoilWat.nLayers do
                 ProcessParameters (SoilOrganic.Struct[iLayer, C], 0, 1E8, 'Structural surface litter carbon');
         StructSoilC:
             For iLayer := 1 to SoilWat.nLayers do
                 ProcessParameters (SoilOrganic.Struct[iLayer, C], 0, 1E8, 'Structural soil litter carbon');
         MetabSurfC:
             For iLayer := 1 to SoilWat.nLayers do
                 ProcessParameters (SoilOrganic.Metab[iLayer, C], 0, 1E8, 'Metabolic surface litter carbon');
         MetabSoilC:
             For iLayer := 1 to SoilWat.nLayers do
                 ProcessParameters (SoilOrganic.Metab[iLayer, C], 0, 1E8, 'Metabolic soil litter carbon');
         SlowC:
             For iLayer := 1 to SoilWat.nLayers do
                 ProcessParameters (SoilOrganic.Slow[iLayer, C], 0, 1E8, 'Slow soil organic carbon');
         ActiveC:
             For iLayer := 1 to SoilWat.nLayers do
                 ProcessParameters (SoilOrganic.Active[iLayer, C], 0, 1E8, 'Active soil organic carbon');
         ResistantC:
             For iLayer := 1 to SoilWat.nLayers do
                 ProcessParameters (SoilOrganic.Resistant[iLayer, C], 0, 1E8, 'Resistant soil organic carbon');
         FineWoodSurfN:
             For iLayer := 1 to SoilWat.nLayers do
                 ProcessParameters (SoilOrganic.FineWood[iLayer, N], 0, 1E8, 'Fine wood surface litter nitrogen');
         CoarseWoodSurfN:
             For iLayer := 1 to SoilWat.nLayers do
                 ProcessParameters (SoilOrganic.CoarseWood[iLayer, N], 0, 1E8, 'Coarse wood surface litter nitrogen');
         CoarseWoodSoilN:
             For iLayer := 1 to SoilWat.nLayers do
                 ProcessParameters (SoilOrganic.CoarseWood[iLayer, N], 0, 1E8, 'Coarse wood soil litter nitrogen');
         StructSurfN:
             For iLayer := 1 to SoilWat.nLayers do
                 ProcessParameters (SoilOrganic.Struct[iLayer, N], 0, 1E8, 'Structural surface litter nitrogen');
         StructSoilN:
             For iLayer := 1 to SoilWat.nLayers do
                 ProcessParameters (SoilOrganic.Struct[iLayer, N], 0, 1E8, 'Structural soil litter nitrogen');
         MetabSurfN:
             For iLayer := 1 to SoilWat.nLayers do
                 ProcessParameters (SoilOrganic.Metab[iLayer, N], 0, 1E8, 'Metabolic surface litter nitrogen');
         MetabSoilN:
             For iLayer := 1 to SoilWat.nLayers do
                 ProcessParameters (SoilOrganic.Metab[iLayer, N], 0, 1E8, 'Metabolic soil litter nitrogen');
         SlowN:
             For iLayer := 1 to SoilWat.nLayers do
                 ProcessParameters (SoilOrganic.Slow[iLayer, N], 0, 1E8, 'Slow soil organic nitrogen');
         ActiveN:
             For iLayer := 1 to SoilWat.nLayers do
                 ProcessParameters (SoilOrganic.Active[iLayer, N], 0, 1E8, 'Active soil organic nitrogen');
         ResistantN:
             For iLayer := 1 to SoilWat.nLayers do
                 ProcessParameters (SoilOrganic.Resistant[iLayer, N], 0, 1E8, 'Resistant soil organic nitrogen');
         EndDummy:  ProcessParameters (Parameter.Nmax, -10000, 10000, '');
         End;
    If (Control.SensFlag = -1) and (Control.SensParameter = EndInputs) then
       Control.SensitivityTestOn := False;
    Control.SensFlag := -Control.SensFlag;
    End; {of Procedure 'SetUpSens'}

Procedure WriteOutSens;
    var LeafC, LeafN: real48;
        SensVar: SensitivityType;
        iLayer: Integer;

    Procedure WriteOut (VarOut: real48);
    var Width, Digits: integer;
        Begin
        GetField(VarOut, 7, Width, Digits);
        Width := Width + 1;
        Write (VarOut: Width: Digits);
        GetField(VarOut, 10, Width, Digits);
        Width := Width + 1;
        Write (Control.SensFile, VarOut: width: Digits);
        End; {of Procedure 'WriteOut'}

    Begin
    LeafC := Plant.Leaves[C];
    LeafN := Plant.Leaves[N];
    If (Control.SensParameter <> Dummy) and (Control.SensParameter <> EndInputs) then
       For SensVar := CAI to ShowResistantN do
           If TestSens.Choose[SensVar] then
              Case SensVar of
                   CAI: WriteOut(Derived.CAI);
                   NConc: WriteOut(Derived.NConc / Control.CConversion);
                   LAI: WriteOut(Derived.LAI);
                   Wood: WriteOut((Plant.SapWood[C] + Plant.HeartWood[C]) * Control.CConversion);
                   ShowFineRootC: WriteOut(Plant.FineRoot[C] * Control.CConversion);
                   ShowFineRootN: WriteOut(Plant.FineRoot[C] * Control.CConversion);
                   ShowBranches: WriteOut(Plant.Branches[C] * Control.CConversion);
                   ShowLeaves: WriteOut(LeafC * Control.CConversion);
                   ShowLeafN: WriteOut(LeafN);
                   Height: WriteOut(Plant.Height);
                   DBH: WriteOut(Plant.DBH);
                   CanopyCover: WriteOut(Plant.CanopyCover);
                   kex: WriteOut(Plant.kex);
                   ShowFineWoodSurfC:
                       For iLayer := 1 to SoilWat.nLayers do
                           WriteOut(SoilOrganic.FineWood[iLayer, C]);
                   ShowCoarseWoodSurfC:
                       For iLayer := 1 to SoilWat.nLayers do
                           WriteOut(SoilOrganic.CoarseWood[iLayer, C]);
                   ShowCoarseWoodSoilC:
                       For iLayer := 1 to SoilWat.nLayers do
                           WriteOut(SoilOrganic.CoarseWood[iLayer, C]);
                   ShowStructSurfC:
                       For iLayer := 1 to SoilWat.nLayers do
                           WriteOut(SoilOrganic.Struct[iLayer, C]);
                   ShowMetabSurfC:
                       For iLayer := 1 to SoilWat.nLayers do
                           WriteOut(SoilOrganic.Metab[iLayer, C]);
                   ShowStructSurfN:
                       For iLayer := 1 to SoilWat.nLayers do
                           WriteOut(SoilOrganic.Struct[iLayer, N]);
                   ShowMetabSurfN:
                       For iLayer := 1 to SoilWat.nLayers do
                           WriteOut(SoilOrganic.Metab[iLayer, N]);
                   ShowSlowC:
                       For iLayer := 1 to SoilWat.nLayers do
                           WriteOut(SoilOrganic.Slow[iLayer, C]);
                   ShowActiveC:
                       For iLayer := 1 to SoilWat.nLayers do
                           WriteOut(SoilOrganic.Active[iLayer, C]);
                   ShowResistantC:
                       For iLayer := 1 to SoilWat.nLayers do
                           WriteOut(SoilOrganic.Resistant[iLayer, C]);
                   ShowSlowN:
                       For iLayer := 1 to SoilWat.nLayers do
                           WriteOut(SoilOrganic.Slow[iLayer, N]);
                   ShowActiveN:
                       For iLayer := 1 to SoilWat.nLayers do
                           WriteOut(SoilOrganic.Active[iLayer, N]);
                   ShowResistantN:
                       For iLayer := 1 to SoilWat.nLayers do
                           WriteOut(SoilOrganic.Resistant[iLayer, N]);
                   End;
       If Control.SensFlag = -1 then
          Begin
          Writeln;
          Writeln(Control.SensFile);
          End
    End; {of Procedure 'WriteOutSens'}

{ --- end of file MISCELL.PAS ------------------------------------------ }
    
End.


