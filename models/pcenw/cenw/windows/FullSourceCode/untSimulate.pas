{ ================================================================
  = Project   : Cenw                                             =
  ================================================================
  = Module    : Simulate                                         =
  =                                                              =
  =             All routines to run the simulation of plant      =
  =             metabolism are contained within this module.     =
  ================================================================
  = File      : SIMULATE.PAS                                     =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

Unit untSimulate;

INTERFACE

uses
  SysUtils, untDeclarations, untSathumValidation, untDivideValidation,
  untTrigValidation, untPowerValidation, untNotices, Messages, Dialogs, variants;

     procedure CarbonGain;
     procedure HarvestorFireLoss;
     procedure CarbonLoss;
     procedure NitrogenLoss;
     Procedure CalcAllocParams;
     procedure Allocation;
     procedure WaterIn;
     procedure WaterOut;
     Procedure GetWeather;
     Procedure CalcPhenology;
     Procedure SetPestDamage;
     Procedure EventController;

IMPLEMENTATION

Procedure Dilute (var NewPoolC13: Real48; NewPool, OldPoolC13, FluxIn: Real48);
var IsotopeDiff, DiluteFraction: Real48;
Begin
If NewPool = 0 then
   NewPoolC13 := OldPoolC13
Else if FluxIn > 0 then
   Begin
   IsotopeDiff := OldPoolC13 - NewPoolC13;
   DiluteFraction := FluxIn / NewPool;
   NewPoolC13 := NewPoolC13 + IsotopeDiff * DiluteFraction;
   End;
End; {of Procedure 'Dilute'}

procedure CarbonGain;
    Var TotalWood, q, WoodNConc, NewDelta, Vmax, Q10k, Q10Vmax, kref, kt, dd,
        Vt, GammaStar, ProdTot, CO2Limit, MaxAssim, PEPLimited: Real48;
        Year: Integer;
    Const Convert = 12e-5;   {Converts from umol m-2 to kg C ha-1}

    Function alpha: real48;
    {Based on Kirschbaum and Farquhar (1987) and McMutrie et al. (1992)
    Kirschbaum, M.U.F. & Farquhar, G.D. (1987): Investigation of the CO2 dependence
    of quantum yield and respiration in Eucalyptus pauciflora. Plant Physiology 83 : 1032-1036;
    McMurtrie, R.E., Comins, H.N., Kirschbaum, M.U.F. & Wang, Y.-P. (1992). Modifying existing forest
    growth models to take account of effects of elevated CO2. Australian Journal of Botany 40: 657-677.}
    Begin
    If Parameter.Phs = C3 then
       alpha := Derived.TDamage * Parameter.alpha * (Derived.p_internal - GammaStar) / (Derived.p_internal + 2 * GammaStar)
    Else
       alpha := Derived.TDamage * Parameter.alpha;
    End; {of Function 'alpha'}

    Function TotalPlantSize: real48; // just adds all biomass components
    Begin
    TotalPlantSize := 0.001 * Control.CConversion * (Plant.SapWood[C] + Plant.HeartWood[C] + Plant.CoarseRoot[C]
                    + Plant.FineRoot[C] + Plant.Leaves[C] + Plant.Branches[C] + Plant.Bark[C] + Plant.Pollen[C] + Plant.Fruit[C] + Plant.Soluble[C]);
    End; {of Function 'TotalPlantSize'}

    Function TmpLimit (Temp: Real48): real48;
    Var Limit: real48;
    Begin
    If Control.ClimType = 'U' then {for uniform climate}
       TmpLimit := 1 / (1 + EXP(1.693 - 0.1047 * Temp))   {After Lieth 1973
                                           modified for the additional effect of
                                           the temperature effect on GammaStar; see
                                           Kirschbaum, 1993}
    Else {use hump-function to calculate activity}
       Begin
       If (Temp > Parameter.TMinLim) and (Temp < Parameter.TOpt1) then
          Limit := Divide((Temp - Parameter.TMinLim), (Parameter.TOpt1 - Parameter.TMinLim))
       Else if (Temp >= Parameter.TOpt1) and (Temp <= Parameter.TOpt2) then
          Limit := 1
       Else if (Temp > Parameter.TOpt2) and (Temp < Parameter.TMaxLim) then
          Limit := Divide((Parameter.TMaxLim - Temp), (Parameter.TMaxLim - Parameter.TOpt2))
       Else
          Limit := 0;
       If Limit < 0 then Limit := 0
       Else if Limit > 1 then Limit := 1;
          TmpLimit := Limit;
       End;
    End; {of Function 'TmpLimit'}

    Procedure TDamage(var DamageUnits, TDamage: real48);
    Begin
    If Weather.Tmin < Parameter.TFrost then
       DamageUnits := DamageUnits + (Parameter.TFrost - Weather.TMin);
    If Weather.Tmax > Parameter.TScorch then
       DamageUnits := DamageUnits + (Weather.TMax - Parameter.TScorch);
    DamageUnits := DamageUnits - Parameter.TRepair;
    If DamageUnits < 0 then DamageUnits := 0;
    If Divide(DamageUnits, Parameter.TRepair) > Parameter.TMaxRepairTime then
       DamageUnits := Parameter.TRepair * Parameter.TMaxRepairTime;
    If DamageUnits * Parameter.TSensitivity > 1 then
       TDamage := 0
    Else
       TDamage := 1 - DamageUnits * Parameter.TSensitivity;
    End; {of Procedure 'TDamage'}

    Function gR(q: real48): real48; {See Sands, 1995}
    Begin
    If q < 1 then
       gR := 1 - 4 / (pi * sqrt(1 - sqr(q))) * arctan(sqrt((1 - q) / (1 + q)))
    Else if q = 1 then
       gR := 1 - 2 / pi
    Else
       gR := 1 - 2 / (pi * sqrt(sqr(q) - 1)) * ln((1 + sqrt((q-1)/(q+1))) / (1 - sqrt((q-1)/(q+1))));
    End; {of function 'gR'}

    Function gB(q: real48): real48; {See Sands, 1995}
    Begin
    If q <= 1 then
       gB := q * 2 / pi
    Else
       gB := 1 + 2 / pi * (q - sqrt(sqr(q) - 1) - ArcSin(1 / q));
    End; {of function 'gB'}

    Function g(q, Theta: real48): real48; {See Sands, 1995}
    Begin
    g := gR(q) * Derived.Functionf1 / (1 + (gR(q) / gB(q) - 1) * Derived.Functionf2);
    End; {of function 'g'}

    Function Min(a, b: Real48): Real48;
    Begin
    If a < b then
       Min := a
    Else
       Min := b;
    End; {of Function 'Min'}

    Function Max(a, b: Real48): Real48;
    Begin
    If a > b then
       Max := a
    Else
       Max := b;
    End; {of Function 'Min'}

Procedure DayLength(var nSeconds: LongInt; JulianDay: integer; Lat: real48);
     {Based on Collares-Pereira and Rabl (1979):
     The average distribution of solar radiation - correlation between diffuse
     and hemispherical and between daily and hourly insolation values. Solar Energy 22: 155-164.
     I modified it by replacing the non-circular orbit term.}
var gamma, DCLN, dnlat: real48;
    Begin
    gamma := 2 * pi * (JulianDay + 284) / 365.24;
    DCLN := InvSin(0.3979 * sin(gamma));
    dnlat := tan(LAT * 0.0174533) * tan(DCLN);
    If dnlat >= 1 then
       nSeconds := 86400
    else if dnlat < -1 then
       nSeconds := 0
    Else
       nSeconds := Round(86400 * (1 - InvCos(dnlat) / pi));
    End; {of Procedure 'DayLength'}

    BEGIN
    if Parameter.FoliageClumping then
      Plant.kex := Parameter.Kexmax * ((1-Parameter.KlowRange) * Plant.CanopyCover + Parameter.KlowRange) // Simioni 19/02/2002 effect of foliage clumping on light interception
    else Plant.kex := Parameter.Kexmax;
    Derived.LAI := Parameter.SLA * Plant.Leaves[C] / 10000;   {10000 to convert from m2 to ha}
    Derived.LightInter := (1 - EXP(-Plant.kex * Derived.LAI * (1 - Parameter.Transmit)));
    Derived.LightAbs := Derived.LightInter * Weather.PAR * (1 - Parameter.Albedo);
    If Control.ClimType = 'C' then
       Control.nSeconds := 43200
    Else
       DayLength(Control.nSeconds, Control.JulianDay, Parameter.Latitude);
    if Parameter.ConstantLeafN = false then
       Derived.NConc := Divide(Divide(Plant.Leaves[N], Plant.Leaves[C]),
                          (Parameter.InternalNRatio))
    else Derived.NConc := Parameter.ConstantLeafNValue * Control.CConversion;
       {note: here NConc is leaf N at top of the canopy, Simioni 27/05/2002}
       {Calculate effective [N] for top canopy layer, based on mean [N] and internal N gradient,
        also assuming that there is only one single layer of foliage in the model.}
    IF Derived.NConc < Parameter.N0 then
       Derived.NLimit := 0
    Else if Derived.Nconc < Parameter.Ncrit then
       Derived.NLimit := (Derived.Nconc - Parameter.N0) / (Parameter.Ncrit - Parameter.N0)
    Else
       Derived.NLimit := 1;
    TDamage (Derived.TDamageUnits, Derived.TDamage);
    Derived.p_internal := Weather.CO2 * (1 - Divide(1.6, (Derived.BallBerry * Weather.RelHumidity))) * 0.001 * Parameter.AtmosPressure;
    If (Parameter.Phs = C4) and (Derived.p_internal < (0.2 * Weather.CO2)) then
       Derived.p_internal := 0.2 * Weather.CO2;                          // Extra safeguard to avoid unrealistically low ci 
    ProdTot := 0;  {In case there is no photosynthesis calculated.}
    If Parameter.Phs = C3 then
       Begin
    // GammaStar := 0.042 * EXP(9.46 * (Weather.TDay - 25) / (Weather.TDay + 273.2)) * Parameter.AtmosPressure;
       GammaStar := 0.04275 * EXP(15.261 * (Weather.TDay - 25) / (Weather.TDay + 273.15)) * Parameter.AtmosPressure;
    // New temperature depdendence based on Bernacchi et al.(2001) Improved temperature response functions for models of Rubisco-limited photosynthesis.
    // Plant, Cell and Environment 24, 253-259.
       If Derived.p_internal > Weather.CO2 then
          Derived.p_internal := Weather.CO2
       else if Derived.p_internal < (2 * GammaStar) then // setting a limit to stomatal closure in dry conditions
          Derived.p_internal := 2 * GammaStar;
       CO2Limit := (Derived.p_internal - GammaStar) / ( Derived.p_internal + 2 * GammaStar);
       MaxAssim := Derived.NLimit * Parameter.Amax * Derived.TDamage * Derived.PestPhsFrac
             * Derived.WaterLimit * CO2Limit * TmpLimit(Weather.Tday);
       If Parameter.AgeDecline then // Age and size decline only calculated for C3 photosynthesis because it is only relevant for trees
          MaxAssim := MaxAssim / (1 + Power((Plant.Age / Parameter.MatureAge), Parameter.AgePower));
       If Parameter.SizeDecline then
          MaxAssim := MaxAssim / (1 + Power((TotalPlantSize / Parameter.MatureSize), Parameter.SizePower));
       End
    Else {if Parameter.Phs = C4 then}
       Begin
       // C4 photosynthesis based on the simplified model of Collatz et al. 1992 (Aust. J. Plant Physiol. 19: 519-538)
       Q10k := 2;    // Q10 for the temperature dependence of PEP carboxylase
       Q10Vmax := 2; // Q10 for the temperature dependence of maximum Rubisco activity
       Vmax := Derived.NLimit * Parameter.Amax * Derived.TDamage * Derived.PestPhsFrac * Derived.WaterLimit;
       kref := Parameter.RelkPEP * Vmax;
       Vt := Vmax * power(Q10vmax,(Weather.TDay - 25) / 10) /
            ((1 + exp(0.3 * (13 - Weather.TDay))) * (1 + exp(0.3 * (Weather.TDay - 36))));
       kt := kref * power(Q10k,(Weather.TDay - 25) / 10);
       PEPLimited := Derived.p_internal * kt;
       dd := sqr(Vt + PEPLimited) - 4 * Parameter.Beta * Vt * PEPLimited;
       if Parameter.Beta > 0 then
          MaxAssim := (Vt + PEPLimited - sqrt(dd)) / (2 * Parameter.Beta)
       Else
          MaxAssim := Vt * PEPLimited / (Vt + PEPLimited);
       End;
    If (MaxAssim > 1e-6) and (Control.nSeconds > 0) and (Plant.Leaves[N] > 0) then
       Begin
       {Based on Sands, 1995: "Modelling canopy production. II. From single-leaf photosynthesis
        parameters to daily canopy photosynthesis.  Aust. J. Plant Physiology 22: 603-614.}
       q := pi * Plant.kex * alpha * Weather.PAR * Parameter.gamma * (1 - Parameter.Albedo)
         / (2 * Control.nSeconds * (1 - Parameter.Transmit) * MaxAssim);
       If q > 0 then
          ProdTot := Convert * MaxAssim * Control.nSeconds * g(q, Parameter.Theta)
          * (1 - exp(-Plant.kex * Derived.LAI * (1 - Parameter.Transmit))) / Plant.kex
       Else
          ProdTot := 0;
       End;
    Plant.NewGrowth[C] := ProdTot * (1 - Parameter.NMVOC);
    Plant.Soluble[C] := Plant.Soluble[C] + Plant.NewGrowth[C];
    Derived.CarbonGain := ProdTot;
    Derived.DayCFlux := Derived.CarbonGain;
    Derived.NightCFlux := 0;
    Derived.NEE := Plant.NewGrowth[C];
    {DayCFlux takes the carbon uptake even though a small fraction of that is loss as NMVOC.
     NEE only takes that whihc is fixed net of that which is lost as NMVOC.
     Hence, NEE <> DayCFlux + NightCFlux,
     but NEE = DayCFlux + NightCFlux - [ProdTot * (1 - Parameter.NMVOC)]}
    TotalWood := Plant.SapWood[C] + Plant.HeartWood[C];
    Control.Count := Control.Count + 1;
    If Control.IncludeIsotopes then
       Begin
       If Parameter.SetDeltaType = SetValue then
          NewDelta := Parameter.NewDelta
       Else { If Parameter.SetDeltaType = SetValue then}
          If Parameter.Phs = C3 then
             Begin
             NewDelta := 22.6 * Derived.p_internal / Weather.CO2 + 4.4;
             // From Farquhar et al. 1989; Ann. Rev. Pl. Phys + Pl. Mol. Biol. 40: 503-537
             NewDelta := NewDelta + Parameter.ExtraDelta;
             End
          Else {if Parameter.Phs = C4 then}
             Begin
             NewDelta := 12.2 + (27 * Parameter.phi - 10.1) * Derived.p_internal / Weather.CO2;
             // From Farquhar 1983 Australian Journal of Plant Physiology 10: 205-226.
             NewDelta := NewDelta + Parameter.ExtraDelta;
             End;
       Dilute(Plant.Soluble[C13], Plant.Soluble[C], NewDelta, ProdTot);
       End;
    If Control.Count > 365 then
       Begin
       Control.Count := 1;
       Plant.Age := Plant.Age + 1;
       WoodNConc := Divide(Plant.SapWood[N], Plant.SapWood[C]);
       Plant.HeartWood[C] := Plant.HeartWood[C] + SapWoodAmount[Parameter.SapWoodYears];
       If Control.IncludeIsotopes then
          Dilute (Plant.HeartWood[C13], Plant.HeartWood[C], Plant.SapWood[C13], SapWoodAmount[Parameter.SapWoodYears]);
       Plant.HeartWood[N] := Plant.HeartWood[N] + SapWoodAmount[Parameter.SapWoodYears]
                          * Parameter.WoodRetrans * WoodNConc;
       Plant.Soluble[N] := Plant.Soluble[N] + SapWoodAmount[Parameter.SapWoodYears]
                        * (1 - Parameter.WoodRetrans) * WoodNConc;
       Plant.SapWood[N] := Plant.SapWood[N] - SapWoodAmount[Parameter.SapWoodYears]* WoodNConc;
       Plant.SapWood[C] := Plant.SapWood[C] - SapWoodAmount[Parameter.SapWoodYears];
       If Plant.SapWood[N] < 0 then Plant.SapWood[N] := 0;
       If Plant.SapWood[C] < 0 then Plant.SapWood[C] := 0;
       For Year := Parameter.SapWoodYears downto 2 do
           SapWoodAmount[Year] := SapWoodAmount[Year-1];
       SapWoodAmount[1] := Plant.SapWood[C];      {Initialise before subtracting later year's contributions}
       For Year := Parameter.SapWoodYears downto 2 do
           SapWoodAmount[1] := SapWoodAmount[1] - SapWoodAmount[Year]; {Find last year's contribution
                                                                       to total sapwood}
       End;
       If Control.TotalDays > 365 then
           Derived.CAI := TotalWood - LastYearsWood[Control.Count]
       Else
           Begin
           If Control.TotalDays <> 1 then
              Derived.CAI := (TotalWood - LastYearsWood[0]) * 365 / (Control.TotalDays-1)
           Else
              Derived.CAI := 0;
           End;
       LastYearsWood[Control.Count] := TotalWood;
    End; {of Procedure 'CarbonGain'}

Procedure HarvestorFireLoss;
var j, k, iLayer: integer;
    CombustionRatio, DummyC13, SumCIn, SumC13, V_Dummy, WoodRemoved, FineRemoved,
    BranchesCut, StemsCut, VolumeAdjust, TotalDepth, SumLignin, SumStemLignin: Real48;
    E: ElementsUsed;
    SumStruct, SumMetab, SumActive, SumSlow, SumResistant, SumInert: Array[C13..N] of Real48;
    St: String;

    Procedure AdjustProperty (var NewProperty: real48; SumProperty, SumC: Real48);
    Begin
    If SumC > 0 then
       NewProperty := SumProperty / SumC;
    End; {of Procedure 'AdjustProperty'}

Begin
j := Control.NextHarvest;
If j <= Event.nHarvests then
     If Event.HarvestTimes[j, 4] = Control.TotalDays then
        Begin
        St := 'Harvest carried out on ' + Control.Date;
        AddNotice(St);
        For E := C to N do
          Begin
          If Event.HarvestUnits = '%' then
             Begin
             If (E = N) and Control.IncludeIsotopes then
                Begin
                DummyC13 := Divide((Plant.SapWood[C13] * Plant.SapWood[C] + Plant.HeartWood[C13] + Plant.HeartWood[C]),(Plant.SapWood[C] + Plant.HeartWood[C]));
                Dilute(Litter.CoarseWood[C13], Litter.CoarseWood[C], DummyC13, (Plant.SapWood[C] + Plant.HeartWood[C]) * Event.WoodCut[j] * (1 - Event.WoodRemoval[j]));
                Dilute(Litter.FineWood[C13], Litter.FineWood[C], Plant.Branches[C13], Plant.Branches[C] * Event.BranchesCut[j] * (1 - Event.FineRemoval[j]));
                Dilute(Litter.CoarseRoot[C13], Litter.CoarseRoot[C], Plant.CoarseRoot[C13], Plant.CoarseRoot[C] * Event.WoodCut[j]);
                Dilute(Litter.FineRoot[C13], Litter.FineRoot[C], Plant.FineRoot[C13], Plant.FineRoot[C] * Event.WoodCut[j]);
                Dilute(Litter.Leaves[C13], Litter.Leaves[C], Plant.Leaves[C13], Plant.Leaves[C] * Event.BranchesCut[j] * (1 - Event.FineRemoval[j]));
                DummyC13 := Divide((Plant.Fruit[C13] * Plant.Fruit[C] + Plant.Pollen[C13] * Plant.Pollen[C] + Plant.Reserves[C13] * Plant.Reserves[C] + Plant.Soluble[C13] * Plant.Soluble[C]),
                         (Plant.Fruit[C] + Plant.Pollen[C] + Plant.Reserves[C] + Plant.Soluble[C]));
                Dilute(Litter.Other[C13], Litter.Other[C], DummyC13,
                         (Plant.Fruit[C] + Plant.Pollen[C] + Plant.Reserves[C] + Plant.Soluble[C]) * Event.BranchesCut[j] * (1 - Event.FineRemoval[j]));
                End;
             Litter.CoarseWood[E] := Litter.CoarseWood[E] + (Plant.SapWood[E] + Plant.HeartWood[E])
                                   * Event.WoodCut[j] * (1 - Event.WoodRemoval[j]);
             Litter.FineWood[E] := Litter.FineWood[E] + Plant.Branches[E] * Event.BranchesCut[j]
                                 * (1 - Event.FineRemoval[j]);
             Litter.CoarseRoot[E] := Litter.CoarseRoot[E] + Plant.CoarseRoot[E] * Event.WoodCut[j];
             Litter.FineRoot[E] := Litter.FineRoot[E] + Plant.FineRoot[E] *  Event.WoodCut[j];
             Litter.Leaves[E] := Litter.Leaves[E] + Plant.Leaves[E] * Event.BranchesCut[j]
                                   * (1 - Event.FineRemoval[j]);
             Litter.Other[E] := Litter.Other[E] + ((Plant.Fruit[E] + Plant.Pollen[E] + Plant.Reserves[E] + Plant.Soluble[E])
                             * Event.BranchesCut[j] +
                             Plant.Bark[E] * Event.WoodCut[j]) * (1 - Event.FineRemoval[j]);
                             {Assumes that stems are debarked before being removed from the site}
             Plant.SapWood[E] := Plant.SapWood[E] * (1 - Event.WoodCut[j]);
             Plant.HeartWood[E] := Plant.HeartWood[E] * (1 - Event.WoodCut[j]);
             Plant.Branches[E] := Plant.Branches[E] * (1 - Event.BranchesCut[j]);
             Plant.CoarseRoot[E] := Plant.CoarseRoot[E] * (1 - Event.WoodCut[j]);
             Plant.FineRoot[E] := Plant.FineRoot[E] * (1 - Event.WoodCut[j]);
             Plant.Reserves[E] := Plant.Reserves[E] * (1 - Event.BranchesCut[j]);
             Plant.Leaves[E] := Plant.Leaves[E] * (1 - Event.BranchesCut[j]);
             Plant.Fruit[E] := Plant.Fruit[E] * (1 - Event.BranchesCut[j]);
             Plant.Pollen[E] := Plant.Pollen[E] * (1 - Event.BranchesCut[j]);
             Plant.Soluble[E] := Plant.Soluble[E] * (1 - Event.BranchesCut[j]);
             Plant.Bark[E] := Plant.Bark[E] * (1 - Event.WoodCut[j]);
             End
          Else {if Event.HarvestUnits = 'V' or 'W'}
             Begin
             If E = C then
                Begin
                If Event.HarvestUnits = 'V' then
                   V_Dummy := Parameter.WoodDensity
                Else
                   V_Dummy := 1;
                StemsCut := Divide(Event.WoodCut[j] * V_Dummy, 0.001 * Control.CConversion * (Plant.SapWood[C] + Plant.HeartWood[C]));
                If StemsCut > 1 then
                   Begin
                   StemsCut := 1;
                   MessageDlg('The indicated size of the harvest is ' + chr(10) +
                     'greater than the size of the whole stand.' + chr(10) +
                     'The whole stand is being harvested now.',
                     mtInformation, [mbOK], 0);
                   End;
                WoodRemoved := Divide(Event.WoodRemoval[j] * V_Dummy, 0.001 * StemsCut * Control.CConversion * (Plant.SapWood[C] + Plant.HeartWood[C]));
                If WoodRemoved > 1 then
                   Begin
                   WoodRemoved := 1;
                   MessageDlg('The indicated size of wood removal is' + chr(10) +
                      'greater than the amount that has been cut.' + chr(10) +
                      'All cut wood is being removed now.',
                      mtInformation, [mbOK], 0);
                   End;
                BranchesCut := Divide(Event.BranchesCut[j] * V_Dummy, 0.001 * Control.CConversion * (Plant.Branches[C] + Plant.Leaves[C]));
                If BranchesCut > 1 then
                   Begin
                   BranchesCut := 1;
                   MessageDlg('The indicated amount of branches cut is' + chr(10) +
                      'greater than the actual size of branches (+ foliage).' + chr(10) +
                      'All branches and foliage will be cut at this harvest.',
                      mtInformation, [mbOK], 0);
                   End;
                FineRemoved := Divide(Event.FineRemoval[j] * V_Dummy, 0.001 * Control.CConversion * (Plant.Branches[C] + Plant.Leaves[C] + Plant.Bark[C] * WoodRemoved));
                If FineRemoved > 1 then
                   Begin
                   FineRemoved := 1;
                   MessageDlg('The indicated size of the removal of fine material is' + chr(10) +
                      'greater than the size of all fine material in the whole stand.' + chr(10) +
                      'All fine material is being harvested now.',
                      mtInformation, [mbOK], 0);
                   End;
                End;
             If (E = N) and Control.IncludeIsotopes then
                Begin
                DummyC13 := Divide((Plant.SapWood[C13] * Plant.SapWood[C] + Plant.HeartWood[C13] + Plant.HeartWood[C]),(Plant.SapWood[C] + Plant.HeartWood[C]));
                Dilute(Litter.FineWood[C13], Litter.FineWood[C], Plant.Branches[C13], Plant.Branches[C] * BranchesCut * (1 - FineRemoved));
                Dilute(Litter.CoarseRoot[C13], Litter.CoarseRoot[C], Plant.CoarseRoot[C13], Plant.CoarseRoot[C] * StemsCut);
                Dilute(Litter.FineRoot[C13], Litter.FineRoot[C], Plant.FineRoot[C13], Plant.FineRoot[C] * StemsCut);
                Dilute(Litter.Leaves[C13], Litter.Leaves[C], Plant.Leaves[C13], Plant.Leaves[C] * BranchesCut * (1 - FineRemoved));
                DummyC13 := Divide((Plant.Fruit[C13] * Plant.Fruit[C] + Plant.Pollen[C13] * Plant.Pollen[C] + Plant.Reserves[C13] * Plant.Reserves[C] + Plant.Soluble[C13] * Plant.Soluble[C]),
                         (Plant.Fruit[C] + Plant.Pollen[C] + Plant.Reserves[C] + Plant.Soluble[C]));
                Dilute(Litter.Other[C13], Litter.Other[C], DummyC13,
                         (Plant.Fruit[C] + Plant.Pollen[C] + Plant.Reserves[C] + Plant.Soluble[C]) * BranchesCut * (1 - FineRemoved));
                End;
             Litter.CoarseWood[E] := Litter.CoarseWood[E] + (Plant.SapWood[E] + Plant.HeartWood[E])
                                   * StemsCut * (1 - WoodRemoved);
             Litter.FineWood[E] := Litter.FineWood[E] + Plant.Branches[E] * BranchesCut
                                 * (1 - FineRemoved);
             Litter.CoarseRoot[E] := Litter.CoarseRoot[E] + Plant.CoarseRoot[E] * StemsCut;
             Litter.FineRoot[E] := Litter.FineRoot[E] + Plant.FineRoot[E] *  StemsCut;
             Litter.Leaves[E] := Litter.Leaves[E] + Plant.Leaves[E] * BranchesCut
                                   * (1 - FineRemoved);
             Litter.Other[E] := Litter.Other[E] + ((Plant.Fruit[E] + Plant.Pollen[E] + Plant.Reserves[E] + Plant.Soluble[E]) * BranchesCut
                                + Plant.Bark[E] * StemsCut) * (1 - FineRemoved);
                             {Assumes that stems are debarked before being removed from the site}
             Plant.SapWood[E] := Plant.SapWood[E] * (1 - StemsCut);
             Plant.HeartWood[E] := Plant.HeartWood[E] * (1 - StemsCut);
             Plant.Branches[E] := Plant.Branches[E] * (1 - BranchesCut);
             Plant.CoarseRoot[E] := Plant.CoarseRoot[E] * (1 - StemsCut);
             Plant.FineRoot[E] := Plant.FineRoot[E] * (1 - StemsCut);
             Plant.Reserves[E] := Plant.Reserves[E] * (1 - BranchesCut);
             Plant.Leaves[E] := Plant.Leaves[E] * (1 - BranchesCut);
             Plant.Fruit[E] := Plant.Fruit[E] * (1 - BranchesCut);
             Plant.Pollen[E] := Plant.Pollen[E] * (1 - BranchesCut);
             Plant.Soluble[E] := Plant.Soluble[E] * (1 - BranchesCut);
             Plant.Bark[E] := Plant.Bark[E] * (1 - StemsCut);
             End;
          End; {of 'For E := C to N' statement}
       If Event.HarvestUnits = '%' then
          StemsCut := Event.WoodCut[j];
       For k := 1 to 365 do
           LastYearsWood[k] := LastYearsWood[k] * (1 - StemsCut);
       For k := 1 to Parameter.SapWoodYears do
           SapWoodAmount[k] := SapWoodAmount[k] * (1 - StemsCut);
       If Event.AdjustStocking[j] then
          Begin {Simulate a harvest with reduced stocking}
          If Event.RelativeSize[j] < 0.001 then
             Event.RelativeSize[j] := 0.001; // extra safeguard especially if no previous value had been given
          If Divide(StemsCut, Event.RelativeSize[j]) >= 1 then
             Plant.Stocking := 0
          Else
             Begin
             Plant.Stocking := Plant.Stocking * (1 - StemsCut / Event.RelativeSize[j]);
             VolumeAdjust := (1 - StemsCut) / (1 - StemsCut / Event.RelativeSize[j]);
             If Plant.DBH > Parameter.Mindbh then
                Begin
                Plant.Height := Plant.Height * Power(VolumeAdjust,
                   1 / (Parameter.WHSlope + Parameter.WDSlope / Parameter.HDSlope));
                Plant.DBH := Plant.DBH * Power(VolumeAdjust,
                   1 / (Parameter.HDSlope * Parameter.WHSlope + Parameter.WDSlope));
                End
             Else
                Begin
                Plant.Height := Plant.Height * Power(VolumeAdjust, 1 / 3);
                Plant.DBH := Plant.DBH * Power(VolumeAdjust, 1 / 3);
                End;
             End;
          End
       Else {Simulate start-up of a new crop with the same number, but smaller, plants}
          Begin
          Plant.Height := Plant.Height * Power((1 - StemsCut),
                   1 / (Parameter.WHSlope + Parameter.WDSlope / Parameter.HDSlope));
          Plant.DBH := Plant.DBH * Power((1 - StemsCut),
                   1 / (Parameter.HDSlope * Parameter.WHSlope + Parameter.WDSlope));
          If StemsCut = 1 then
             Plant.Age := 0;
          End;
       Control.NextHarvest := Control.NextHarvest + 1;
       End; {of 'If Event.HarvestTimes[j] = i then' statement}
j := Control.NextFire;
If j <= Event.nFires then
     If Event.FireTimes[j, 4] = Control.TotalDays then
        Begin
        St := 'Fire occurred on ' + Control.Date;
        AddNotice(St);
        If Control.IncludeIsotopes then
          Begin
          SumCIn := Plant.SapWood[C] * Event.WoodToChar[j] + Plant.HeartWood[C] * Event.WoodToChar[j] +
                    Plant.Bark[C] * Event.WoodToChar[j] + Plant.Branches[C] * Event.WoodToChar[j] +
                    SoilOrganic.CoarseWood[0, C] * Event.WoodToChar[j] + Litter.CoarseWood[C] * Event.WoodToChar[j] +
                    Litter.Leaves[C] * Event.FineToChar[j] + Litter.Other[C] * Event.FineToChar[j] +
                    Litter.FineWood[C] * Event.FineToChar[j] + SoilOrganic.FineWood[0, C] * Event.FineToChar[j] +
                    SoilOrganic.Struct[0, C] * Event.FineToChar[j] + SoilOrganic.Metab[0, C] * Event.FineToChar[j] +
                    Plant.Leaves[C] * Event.FineToChar[j] + Plant.Reserves[C] * Event.FineToChar[j] +
                    Plant.Fruit[C] * Event.FineToChar[j] + Plant.Pollen[C] * Event.FineToChar[j];
          SumC13 := Plant.SapWood[C] * Plant.SapWood[C13] * Event.WoodToChar[j] + Plant.HeartWood[C] * Plant.HeartWood[C13] * Event.WoodToChar[j] +
                    Plant.Bark[C] * Plant.Bark[C13] * Event.WoodToChar[j] + Plant.Branches[C] * Plant.Branches[C13] * Event.WoodToChar[j] +
                    SoilOrganic.CoarseWood[0, C] * SoilOrganic.CoarseWood[0, C13] * Event.WoodToChar[j] + Litter.CoarseWood[C] * Litter.CoarseWood[C13] * Event.WoodToChar[j] +
                    Litter.Leaves[C] * Litter.Leaves[C13] * Event.FineToChar[j] + Litter.Other[C] * Litter.Other[C13] * Event.FineToChar[j] +
                    Litter.FineWood[C] * Litter.FineWood[C13] * Event.FineToChar[j] + SoilOrganic.FineWood[0, C] * SoilOrganic.FineWood[0, C13] * Event.FineToChar[j] +
                    SoilOrganic.Struct[0, C] * SoilOrganic.Struct[0, C13] * Event.FineToChar[j] + SoilOrganic.Metab[0, C] * SoilOrganic.Metab[0, C13] * Event.FineToChar[j] +
                    Plant.Leaves[C] * Plant.Leaves[C13] * Event.FineToChar[j] + Plant.Reserves[C] * Plant.Reserves[C13] * Event.FineToChar[j] +
                    Plant.Fruit[C] * Plant.Fruit[C13] * Event.FineToChar[j] + Plant.Pollen[C] * Plant.Pollen[C13] * Event.FineToChar[j];
          DummyC13 := Divide(SumC13, SumCIn);
          Dilute (SoilOrganic.Inert[0, C13], SoilOrganic.Inert[0, C], DummyC13, SumCIn);
          SumCIn := (Plant.SapWood[C] + Plant.HeartWood[C]) * Event.WoodBurnSenesc[j];
          SumC13 := (Plant.SapWood[C] * Plant.SapWood[C13] + Plant.HeartWood[C] * Plant.HeartWood[C13]) * Event.WoodBurnSenesc[j];
          DummyC13 := Divide(SumC13, SumCIn);
          Dilute (Litter.CoarseWood[C13], Litter.CoarseWood[C], DummyC13, SumCIn);
          Dilute (Litter.FineWood[C13], Litter.FineWood[C], Plant.Branches[C13], Plant.Branches[C] * Event.WoodBurnSenesc[j]);
          Dilute (Litter.CoarseRoot[C13], Litter.CoarseRoot[C], Plant.CoarseRoot[C13], Plant.CoarseRoot[C] * Event.WoodBurnSenesc[j]);
          Dilute (Litter.FineRoot[C13], Litter.FineRoot[C], Plant.FineRoot[C13], Plant.FineRoot[C] * Event.WoodBurnSenesc[j]);
          Dilute (Litter.Leaves[C13], Litter.Leaves[C], Plant.Leaves[C13], Plant.Leaves[C] * (Event.WoodBurnSenesc[j] + Event.LeafBurnSenesc[j]));
          SumCIn := (Plant.Fruit[C] + Plant.Pollen[C] + Plant.Reserves[C] + Plant.Soluble[C]) * (Event.WoodBurnSenesc[j] + Event.LeafBurnSenesc[j])
                    + Plant.Bark[C] * Event.WoodBurnSenesc[j];
          SumC13 := (Plant.Fruit[C] * Plant.Fruit[C13] + Plant.Pollen[C] * Plant.Pollen[C13] +
                     Plant.Reserves[C] * Plant.Reserves[C13] + Plant.Soluble[C] * Plant.Soluble[C13]) * (Event.WoodBurnSenesc[j] + Event.LeafBurnSenesc[j])
                    + Plant.Bark[C] * Plant.Bark[C13] * Event.WoodBurnSenesc[j];
          DummyC13 := Divide(SumC13, SumCIn);
          Dilute (Litter.Other[C13], Litter.Other[C], DummyC13, SumCIn);
          End; {of if Control.IncludeIsotopes statement}
        For E := C to N do
          Begin
          If E = C then
             CombustionRatio := 1
          Else if E = N then
             CombustionRatio := Event.Burn_N_CRatio[j];
          SoilOrganic.Inert[0, E] := SoilOrganic.Inert[0, E] * (1 - Event.LitterBurn[j] * CombustionRatio) +
                                    (Plant.Sapwood[E] + Plant.HeartWood[E] + Plant.Bark[E] + Plant.Branches[E] + SoilOrganic.CoarseWood[0, E] + Litter.CoarseWood[E]) * Event.WoodToChar[j] +
                                    (Litter.Leaves[E] + Litter.Other[E] + Litter.FineWood[E] + SoilOrganic.FineWood[0, E] + SoilOrganic.Struct[0, E] + SoilOrganic.Metab[0, E]
                                    + Plant.Leaves[E] + Plant.Reserves[E] + Plant.Fruit[E] + Plant.Pollen[E]) * Event.FineToChar[j];
          Litter.CoarseWood[E] := Litter.CoarseWood[E] + (Plant.SapWood[E] + Plant.HeartWood[E]) * Event.WoodBurnSenesc[j] - Litter.CoarseWood[E] * Event.WoodToChar[j];
          Litter.FineWood[E] := Litter.FineWood[E] + Plant.Branches[E] * Event.WoodBurnSenesc[j] - Litter.FineWood[E]  * Event.FineToChar[j];
          Litter.CoarseRoot[E] := Litter.CoarseRoot[E] + Plant.CoarseRoot[E] * (Event.WoodBurnSenesc[j] + Event.WoodBurn[j]);
          Litter.FineRoot[E] := Litter.FineRoot[E] + Plant.FineRoot[E] * (Event.WoodBurnSenesc[j] + Event.WoodBurn[j]);
          Litter.Leaves[E] := Litter.Leaves[E] * (1- Event.FineToChar[j])
                              + Plant.Leaves[E] * (Event.WoodBurnSenesc[j] + Event.LeafBurnSenesc[j]);
          Litter.Other[E] := Litter.Other[E] * (1 - Event.FineToChar[j])
                             + (Plant.Fruit[E] + Plant.Pollen[E] + Plant.Reserves[E] + Plant.Soluble[E]) *
                             (Event.WoodBurnSenesc[j] + Event.LeafBurnSenesc[j]) + Plant.Bark[E] * Event.WoodBurnSenesc[j];
          Plant.SapWood[E] := Plant.SapWood[E] * (1 - Event.WoodBurn[j] * CombustionRatio - Event.WoodBurnSenesc[j] - Event.WoodToChar[j]);
          Plant.HeartWood[E] := Plant.HeartWood[E] * (1 - Event.WoodBurn[j] * CombustionRatio - Event.WoodBurnSenesc[j] - Event.WoodToChar[j]);
          Plant.Branches[E] := Plant.Branches[E] * (1 - Event.WoodBurn[j] * CombustionRatio - Event.WoodBurnSenesc[j] - Event.WoodToChar[j]);
          Plant.CoarseRoot[E] := Plant.CoarseRoot[E] * (1 - Event.WoodBurn[j] - Event.WoodBurnSenesc[j]);
          Plant.FineRoot[E] := Plant.FineRoot[E] * (1 - Event.WoodBurn[j] - Event.WoodBurnSenesc[j]);
          Plant.Reserves[E] := Plant.Reserves[E] * (1 - (Event.WoodBurn[j] + Event.LeafBurn[j]) * CombustionRatio - Event.WoodBurnSenesc[j] - Event.LeafBurnSenesc[j] - Event.FineToChar[j]);
          Plant.Leaves[E] := Plant.Leaves[E] * (1 - (Event.WoodBurn[j] + Event.LeafBurn[j]) * CombustionRatio - Event.WoodBurnSenesc[j] - Event.LeafBurnSenesc[j] - Event.FineToChar[j]);
          Plant.Fruit[E] := Plant.Fruit[E] * (1 - (Event.WoodBurn[j] + Event.LeafBurn[j]) * CombustionRatio - Event.WoodBurnSenesc[j] - Event.LeafBurnSenesc[j] - Event.FineToChar[j]);
          Plant.Pollen[E] := Plant.Pollen[E] * (1 - (Event.WoodBurn[j] + Event.LeafBurn[j]) * CombustionRatio - Event.WoodBurnSenesc[j] - Event.LeafBurnSenesc[j] - Event.FineToChar[j]);
          Plant.Soluble[E] := Plant.Soluble[E] * (1 - (Event.WoodBurn[j] + Event.LeafBurn[j]) * CombustionRatio - Event.WoodBurnSenesc[j] - Event.LeafBurnSenesc[j]);
          Plant.Bark[E] := Plant.Bark[E] * (1 - Event.WoodBurn[j] * CombustionRatio - Event.WoodBurnSenesc[j] - Event.WoodToChar[j]);
          SoilOrganic.FineWood[0, E] := SoilOrganic.FineWood[0, E] * (1 - Event.LitterBurn[j] * CombustionRatio);
          SoilOrganic.CoarseWood[0, E] := SoilOrganic.CoarseWood[0, E] * (1 - Event.LitterBurn[j] * CombustionRatio);
          SoilOrganic.Struct[0, E] := SoilOrganic.Struct[0, E] * (1 - Event.LitterBurn[j] * CombustionRatio);
          SoilOrganic.Metab[0, E] := SoilOrganic.Metab[0, E] * (1 - Event.LitterBurn[j] * CombustionRatio);
          SoilOrganic.Active[0, E] := SoilOrganic.Active[0, E] * (1 - Event.LitterBurn[j] * CombustionRatio);
          SoilOrganic.Slow[0, E] := SoilOrganic.Slow[0, E] * (1 - Event.LitterBurn[j] * CombustionRatio);
          SoilOrganic.Resistant[0, E] := SoilOrganic.Resistant[0, E] * (1 - Event.LitterBurn[j] * CombustionRatio);
          SoilOrganic.Soluble[0, E] := SoilOrganic.Soluble[0, E] * (1 - Event.LitterBurn[j] * CombustionRatio);
          End; {of 'For E := C to N' statement}
       For k := 1 to 365 do
           LastYearsWood[k] := LastYearsWood[k] * (1 - Event.WoodBurn[j] - Event.WoodBurnSenesc[j]);
       For k := 1 to Parameter.SapWoodYears do
           SapWoodAmount[k] := SapWoodAmount[k] * (1 - Event.WoodBurn[j] - Event.WoodBurnSenesc[j]);
       Plant.Stocking := Plant.Stocking * (1 - Event.WoodBurn[j] - Event.WoodBurnSenesc[j]);
       Plant.Area := Plant.Area * (1 - Event.WoodBurn[j] - Event.WoodBurnSenesc[j]);
       Control.NextFire := Control.NextFire + 1;
       End; {of 'If Event.FireTimes[j] = i then' statement}
j := Control.NextPlough;
If j <= Event.nPloughing then
     If (Event.PloughTimes[j, 4] = Control.TotalDays) and (Event.PloughDepth[j] > 0) then
        Begin
        St := 'Soil ploughed on ' + Control.Date;
        AddNotice(St);
        TotalDepth := 0;
        For iLayer := 1 to Event.PloughDepth[j] do
            TotalDepth := TotalDepth + SoilWat.Layer[iLayer].Depth;
        SumLignin := 0;
        SumStemLignin := 0;
        For E := C13 to N do
            Begin
            // Coarse woody litter assumed not to be included in ploughing
            SumStruct[E] := 0;
            SumMetab[E] := 0;
            SumActive[E] := 0;
            SumSlow[E] := 0;
            SumResistant[E] := 0;
            SumInert[E] := 0;
            For iLayer := 0 to Event.PloughDepth[j] do
               If E <> C13 then
                  Begin
                  SumStruct[E] := SumStruct[E] + SoilOrganic.Struct[iLayer, E]
                                               + SoilOrganic.FineWood[iLayer, E];
                  // 'Fine wood' litter is exclusively former branch material which is assumed not to exist in the soil
                  // and therefore cannot be turned into soil pools. Hence, we add it to the structural litter pool.
                  SumMetab[E] := SumMetab[E] + SoilOrganic.Metab[iLayer, E];
                  SumActive[E] := SumActive[E] + SoilOrganic.Active[iLayer, E];
                  SumSlow[E] := SumSlow[E] + SoilOrganic.Slow[iLayer, E];
                  SumResistant[E] := SumResistant[E] + SoilOrganic.Resistant[iLayer, E];
                  SumInert[E] := SumInert[E] + SoilOrganic.Inert[iLayer, E];
                  If E = C then
                     SumLignin := SumLignin + SoilOrganic.LitterLig[iLayer] * SoilOrganic.Struct[iLayer, C]
                                + SoilOrganic.BranchLig[iLayer] * SoilOrganic.FineWood[iLayer, C];
                  End
               Else
                  Begin
                  SumStruct[C13] := SumStruct[C13] + SoilOrganic.Struct[iLayer, C] * SoilOrganic.Struct[iLayer, C13]
                                                   + SoilOrganic.FineWood[iLayer, C] * SoilOrganic.FineWood[iLayer, C13];
                  SumMetab[C13] := SumMetab[C13] + SoilOrganic.Metab[iLayer, C] * SoilOrganic.Metab[iLayer, C13];
                  SumActive[C13] := SumActive[C13] + SoilOrganic.Active[iLayer, C] * SoilOrganic.Active[iLayer, C13];
                  SumSlow[C13] := SumSlow[C13] + SoilOrganic.Slow[iLayer, C] * SoilOrganic.Slow[iLayer, C13];
                  SumResistant[C13] := SumResistant[C13] + SoilOrganic.Resistant[iLayer, C] * SoilOrganic.Resistant[iLayer, C13];
                  SumInert[C13] := SumInert[C13] + SoilOrganic.Inert[iLayer, C] * SoilOrganic.Inert[iLayer, C13];
                  End;
            For iLayer := 1 to Event.PloughDepth[j] do
                Begin
                SoilOrganic.Struct[iLayer, E] := SumStruct[E] * SoilWat.Layer[iLayer].Depth / TotalDepth;
                SoilOrganic.Metab[iLayer, E] := SumMetab[E] * SoilWat.Layer[iLayer].Depth / TotalDepth;
                SoilOrganic.Active[iLayer, E] := SumActive[E] * SoilWat.Layer[iLayer].Depth / TotalDepth;
                SoilOrganic.Slow[iLayer, E] := SumSlow[E] * SoilWat.Layer[iLayer].Depth / TotalDepth;
                SoilOrganic.Resistant[iLayer, E] := SumResistant[E] * SoilWat.Layer[iLayer].Depth / TotalDepth;
                SoilOrganic.Inert[iLayer, E] := SumInert[E] * SoilWat.Layer[iLayer].Depth / TotalDepth;
                If E = C then
                   Begin
                   AdjustProperty (SoilOrganic.LitterLig[iLayer], SumLignin, SumStruct[C]);
                   AdjustProperty (SoilOrganic.Struct[iLayer, C13], SumStruct[C13], SumStruct[C]);
                   AdjustProperty (SoilOrganic.Metab[iLayer, C13], SumMetab[C13], SumStruct[C]);
                   AdjustProperty (SoilOrganic.Active[iLayer, C13], SumActive[C13], SumStruct[C]);
                   AdjustProperty (SoilOrganic.Slow[iLayer, C13], SumSlow[C13], SumStruct[C]);
                   AdjustProperty (SoilOrganic.Resistant[iLayer, C13], SumResistant[C13], SumStruct[C]);
                   AdjustProperty (SoilOrganic.Inert[iLayer, C13], SumInert[C13], SumInert[C]);
                   End;
                End;
            SoilOrganic.FineWood[0, E] := 0;
            SoilOrganic.Struct[0, E] := 0;
            SoilOrganic.Metab[0, E] := 0;
            SoilOrganic.Active[0, E] := 0;
            SoilOrganic.Slow[0, E] := 0;
            SoilOrganic.Resistant[0, E] := 0;
            SoilOrganic.Inert[0, E] := 0;
            End; {of 'For E := C to N' statement}
       Control.NextPlough := Control.NextPlough + 1;
       End; {of 'If Event.PloughTimes[j] = i then' statement}
End; {of procedure 'HarvestorFireLoss'}

Procedure CarbonLoss;

    procedure Respiration;
    var MaintResp, ActResp: real48;

        Function TempResp(T: real48): real48;
        Begin
        TempResp := exp(Parameter.Respnalpha + Parameter.Respnbeta * T * (2 * Parameter.RespnOpt - T));
//        TempResp := exp(-3.166 + 0.001696 * T * (100 - T));
                 {Eqn based on data of Kirschbaum & Farquhar 1984,
                  but fitted with the equation given in Kirschbaum 1995, with assumed Topt = 50 degrees}
        End; {of function 'TempResp'}
    Begin
    With Plant do
         Begin
         If Parameter.RespnType = Basics then
            Begin
            If Derived.NConc <= Parameter.nCrit then
               ActResp := Parameter.RespFromN * TempResp(Weather.Tmean) * Derived.RespnBase * Derived.WaterLimit
            Else
               ActResp := Parameter.RespFromN * TempResp(Weather.Tmean) * Derived.RespnBase * Derived.WaterLimit
                       * Parameter.nCrit / Derived.NConc;  // If NConc is above the critical value, the excess N is treated as storage rather than metabolic components
            {Acclimation response of respiration to temperature}
            If Parameter.RespnTAcclimation then
               Derived.RespnBase := Derived.RespnBase + Divide((Divide(1, TempResp(Weather.Tmean)) - Derived.RespnBase), Parameter.RespnAdjust)
            Else
               Derived.RespnBase := 1;
            {No maintenance respiration from heartwood and coarse roots}
            MaintResp := ActResp * (SapWood[N] + Fineroot[N] + Branches[N] + Pollen[N] + Fruit[N])
                        + Leaves[N] * Parameter.RespFromN * TempResp(Weather.TNight) * (86400 - Control.nSeconds) / 86400;
           {This makes allowance for the fact that normal foliage respiration only occurs at night,
            whereas daytime respiration is allowed for in net assimilation calculations}
            Derived.NEE := Derived.NEE - MaintResp - Derived.SoilRespn;
            Derived.SoilRespn := Derived.SoilRespn + ActResp * Fineroot[N];
            End
         Else
            Begin
            MaintResp := Derived.CarbonGain * Parameter.RespnRatio;
            Derived.NEE := Derived.NEE - MaintResp - Derived.SoilRespn;
            Derived.SoilRespn := Derived.SoilRespn + MaintResp *
                                 Divide(Fineroot[N], (FineRoot[N] + Leaves[N]));
            End;
         If MaintResp > Soluble[C] then
            MaintResp := 0.5 * MaintResp;      // If the plant has run out of reserves, halve respn and then senesce tissues
         Soluble[C] := Soluble[C] - MaintResp;
         Derived.Rm := MaintResp;
         If Soluble[C] < 0 then
            Begin
            Leaves[C] := Leaves[C] + Soluble[C];
            If Leaves[C] < 1 then Leaves[C] := 1;
            Soluble[C] := 0;
            End;
         End;
    End; {of procedure 'Respiration'}

    Procedure Senescence;
    var DensityMortality, TotalLeafSenesc, MaxWeight, MeanRatio, DeathRatio: real48;
        iYear: Integer;
    Begin
    Derived.nMortality := Derived.DroughtMort + Derived.PestMortality;
    If (Parameter.MortalityType = Fraction) or (Parameter.MortalityType = Both) then
       Derived.nMortality := Derived.nMortality + Parameter.StemDeath;
    If (Parameter.MortalityType = Density) or (Parameter.MortalityType = Both) then
       Begin
       MaxWeight := (Parameter.Three_Two_Power_Law * Power(Plant.Stocking, -1.5)) / Control.CConversion;
       DensityMortality := Divide(Plant.SapWood[C] + Plant.HeartWood[C], Plant.Stocking * MaxWeight) - 1.0;
       If DensityMortality < 0 then
          DensityMortality := 0
       Else if DensityMortality > 0.001 then
          DensityMortality := 0.001;
       {Setting a limit that at most 0.1% of plants can die per day}
       If Derived.nMortality < DensityMortality then
          Derived.nMortality := DensityMortality;
       End;
    If Derived.nMortality = 0 then
       DeathRatio := 0
    Else
       DeathRatio := (Derived.PestDeathRatio * Derived.PestMortality +
                     Parameter.DeathRatio * (Derived.nMortality - Derived.PestMortality)) / Derived.nMortality;
    Plant.Stocking := Plant.Stocking * (1 - Derived.nMortality);
    MeanRatio := (1 - Derived.nMortality * DeathRatio) / (1 - Derived.nMortality);
    // 'MeanRatio' is the ratio of living trees before and after mortality occurred.
    // If dieing trees are on average smaller than surviving trees then the average size of living trees
    // will increase after the smaller dieing ones have been removed from the population.
    // This is used subsequently to adjust height and diameter of the adjusted population.
    Derived.CLossMortality := Derived.nMortality * DeathRatio;
    If ((1 - Derived.LightInter) * Weather.PAR) < Parameter.SenescLowLight then
       TotalLeafSenesc := (Parameter.LeafSenesc + Derived.Desic + Derived.Deciduous +
       Derived.TDamage * Parameter.MaxSenescLowLight + Derived.CLossMortality)
       {Low-light senescence occurs only to the extent that plant activity is not already inhibited
       by temperature damage.}
    Else
       TotalLeafSenesc := (Parameter.LeafSenesc + Derived.Desic + Derived.Deciduous + Derived.CLossMortality);
    Litter.CoarseWood[C] := ((Plant.SapWood[C] + Plant.HeartWood[C]) * Derived.CLossMortality);
    Litter.FineWood[C] := Plant.Branches[C] * (Parameter.BranchSenesc + Derived.CLossMortality);
    Litter.CoarseRoot[C] := Plant.CoarseRoot[C] * Derived.CLossMortality;
    Litter.FineRoot[C] := Plant.FineRoot[C] * (Parameter.RootSenesc + Derived.CLossMortality + Derived.Desic);
    Litter.Leaves[C] := Plant.Leaves[C] * TotalLeafSenesc;
    Litter.Other[C] := Plant.Fruit[C] * (Parameter.FruitSenesc + Derived.CLossMortality) +
                       Plant.Pollen[C] * (Parameter.PollenSenesc + Derived.CLossMortality) +
                       Plant.Bark[C] * (Parameter.BarkSenesc + Derived.CLossMortality);
    Plant.SapWood[C] := Plant.SapWood[C] * (1 - Derived.CLossMortality);
    Plant.HeartWood[C] := Plant.HeartWood[C] * (1 - Derived.CLossMortality);
    Plant.Branches[C] := Plant.Branches[C] * (1 - (Parameter.BranchSenesc + Derived.CLossMortality));
    Plant.CoarseRoot[C] := Plant.CoarseRoot[C] * (1 - Derived.CLossMortality);
    Plant.FineRoot[C] := Plant.FineRoot[C] * (1 - (Parameter.RootSenesc + Derived.CLossMortality + Derived.Desic));
    Plant.Leaves[C] := Plant.Leaves[C] * (1 - TotalLeafSenesc);
    Plant.Height := Plant.Height * Power(MeanRatio,
                   1 / (Parameter.WHSlope + Parameter.WDSlope / Parameter.HDSlope));
    Plant.DBH := Plant.DBH * Power(MeanRatio,
                   1 / (Parameter.HDSlope * Parameter.WHSlope + Parameter.WDSlope));
    If Plant.Leaves[C] < 0 then
       Plant.Leaves[C] := 0;
    Plant.Fruit[C] := Plant.Fruit[C] * (1 - (Parameter.FruitSenesc + Derived.CLossMortality));
    Plant.Pollen[C] := Plant.Pollen[C] * (1 - (Parameter.PollenSenesc + Derived.CLossMortality));
    Plant.Bark[C] := Plant.Bark[C] * (1 - (Parameter.BarkSenesc + Derived.CLossMortality));
    For iYear := 1 to Parameter.SapWoodYears do
        SapWoodAmount[iYear] := SapWoodAmount[iYear] * (1 - Derived.CLossMortality);
    End; {of procedure 'Senescence'}

    Procedure SenesceC13;
    Begin
    Litter.CoarseWood[C13] := Divide((Plant.SapWood[C13] * Plant.SapWood[C] + Plant.HeartWood[C13] * Plant.HeartWood[C]),
                                     (Plant.SapWood[C] + Plant.HeartWood[C]));
    Litter.FineWood[C13] := Plant.Branches[C13];
    Litter.CoarseRoot[C13] := Plant.CoarseRoot[C13];
    Litter.FineRoot[C13] := Plant.FineRoot[C13];
    Litter.Leaves[C13] := Plant.Leaves[C13];
    Litter.Other[C13] := Divide((Plant.Fruit[C13] * Plant.Fruit[C] * (Parameter.FruitSenesc + Derived.CLossMortality) +
                                 Plant.Pollen[C13] * Plant.Pollen[C] * (Parameter.PollenSenesc + Derived.CLossMortality) +
                                 Plant.Bark[C13] * Plant.Bark[C] * (Parameter.BarkSenesc + Derived.CLossMortality)),
                                 (Plant.Fruit[C] * (Parameter.FruitSenesc + Derived.CLossMortality) +
                                 Plant.Pollen[C] * (Parameter.PollenSenesc + Derived.CLossMortality) +
                                 Plant.Bark[C] * (Parameter.BarkSenesc + Derived.CLossMortality)));
    End; {of procedure 'SenesceC13'}

    Procedure PestDamage;
    var CRespired: Real48;
    Begin
    If Control.PestMode then
       Begin
       If Event.PestDamageUnits = '%' then
          Begin
          If Control.IncludeIsotopes then
             Dilute (Litter.Leaves[C13], Litter.Leaves[C], Plant.Leaves[C13], Plant.Leaves[C] * (Derived.PestLeafDamage + Derived.PestSenesc));
          Litter.Leaves[C] := Litter.Leaves[C] + Plant.Leaves[C] * (Derived.PestSenesc);
          CRespired := Plant.Leaves[C] * Derived.PestLeafDamage + Plant.Soluble[C] * Derived.PestSolubleDamage;
          Plant.Leaves[C] := Plant.Leaves[C] * (1 - Derived.PestLeafDamage - Derived.PestSenesc);
          Plant.Soluble[C] := Plant.Soluble[C] * (1 - Derived.PestSolubleDamage);
          End
       Else
          Begin
          If (Derived.PestLeafDamage + Derived.PestSenesc) < Plant.Leaves[C] then
             Begin
             If Control.IncludeIsotopes then
                Dilute (Litter.Leaves[C13], Litter.Leaves[C], Plant.Leaves[C13], Derived.PestSenesc);
             Litter.Leaves[C] := Litter.Leaves[C] + Derived.PestSenesc;
             CRespired := Derived.PestLeafDamage;
             Plant.Leaves[C] := Plant.Leaves[C] - Derived.PestLeafDamage - Derived.PestSenesc;
             End
          Else
             Begin
             If Control.IncludeIsotopes then
                Dilute (Litter.Leaves[C13], Litter.Leaves[C], Plant.Leaves[C13], Plant.Leaves[C]);
             Litter.Leaves[C] := Litter.Leaves[C] + Plant.Leaves[C]; // senesce the last bit of leaves
             If Derived.PestLeafDamage > Plant.Leaves[C] then
                CRespired := Plant.Leaves[C]
             Else
                CRespired := Derived.PestLeafDamage;
             Plant.Leaves[C] := 0;
             End;
          If Derived.PestSolubleDamage < Plant.Soluble[C] then
             Begin
             Plant.Soluble[C] := Plant.Soluble[C] - Derived.PestSolubleDamage;
             CRespired := CRespired + Derived.PestSolubleDamage;
             End
          Else
             Begin
             CRespired := Plant.Soluble[C];
             Plant.Soluble[C] := 0;
             End;
          End;
       Derived.NEE := Derived.NEE - CRespired;
       Derived.DayCFlux := Derived.DayCFlux - CRespired * Control.NSeconds / 86400;
       Derived.NightCFlux := Derived.NightCFlux - CRespired * (86400 - Control.NSeconds) / 86400;
       End; {of 'if Control.PestMode statement}
    End; {of procedure 'PestDamage'}

Begin
Respiration;
Senescence;
If Control.IncludeIsotopes then SenesceC13;
PestDamage;
If Control.TotalDays > 365 then
   Begin
   Derived.NPP := Derived.NPP - NPP[Control.Count];
   NPP[Control.Count] := Derived.CarbonGain - Derived.Rm - Derived.Rg;
   Derived.NPP := Derived.NPP + NPP[Control.Count];
   End
Else
   Begin
   NPP[Control.Count] := Derived.CarbonGain - Derived.Rm - Derived.Rg;
   Derived.NPP := Derived.NPP + NPP[Control.Count];
   End;
End; {of Procedure 'CarbonLoss'}

Procedure NitrogenLoss;
var TotalLeafSenesc: real48;

    Procedure PestNitrogenDamage;
    var LeafDamage, LeafSenesc: real48;
    Begin
    If Control.PestMode then
       If Event.PestDamageUnits = '%' then
          Begin
          Litter.Leaves[N] := Litter.Leaves[N] + Plant.Leaves[N] * (Derived.PestSenesc + Derived.PestLeafDamage);
          // Assumes that C of eaten leaves is respired but N is returned to the soil
          // and leaves senesced due to insect damage do not re-locate nutrients before leaf fall
          Plant.Leaves[N] := Plant.Leaves[N] * (1 - Derived.PestLeafDamage - Derived.PestSenesc);
          Litter.Other[N] := Litter.Other[N] + Plant.Soluble[N] * Derived.PestSolubleDamage;
          Plant.Soluble[N] := Plant.Soluble[N] * (1 - Derived.PestSolubleDamage);
          // Assumes that C of sucked sap is respired but N is returned to the soil
          End
       Else
          Begin
          LeafDamage := Derived.PestLeafDamage * Derived.NConc;
          LeafSenesc := Derived.PestSenesc * Derived.NConc;
          If (LeafDamage + LeafSenesc) < Plant.Leaves[N] then
             Begin
             Litter.Leaves[N] := Litter.Leaves[N] + LeafSenesc + LeafDamage;
             Plant.Leaves[N] := Plant.Leaves[N] - LeafDamage - LeafSenesc;
             End
          Else
             Begin
             Litter.Leaves[N] := Litter.Leaves[N] + Plant.Leaves[N]; // senesce the last bit of leaves
             Plant.Leaves[N] := 0;
             End;
          If Derived.PestSolubleDamage < Plant.Soluble[C] then
             Begin
             Plant.Soluble[N] := Plant.Soluble[N] - Derived.PestSolubleDamage * Divide(Plant.Soluble[N], Plant.Soluble[C]);
             Litter.Other[N] := Litter.Other[N] + Derived.PestSolubleDamage * Divide(Plant.Soluble[N], Plant.Soluble[C]);
             End
          Else
             Begin
             Litter.Other[N] := Litter.Other[N] + Plant.Soluble[N];
             Plant.Soluble[N] := 0;
             End;
          End;
    End; {of procedure 'PestNitrogenDamage'}

Begin
If ((1 - Derived.LightInter) * Weather.PAR) < Parameter.SenescLowLight then
   TotalLeafSenesc := (Parameter.LeafSenesc + Derived.Desic + Derived.Deciduous +
   Derived.TDamage * Parameter.MaxSenescLowLight + Derived.CLossMortality)
// Inclusion of Derived.TDamage prevents senescence during time when foliage is effectively winter-dormant
Else
   TotalLeafSenesc := (Parameter.LeafSenesc + Derived.Desic + Derived.Deciduous + Derived.CLossMortality);
Litter.CoarseWood[N] := (Plant.SapWood[N] + Plant.HeartWood[N])* Derived.CLossMortality;
Litter.FineWood[N] := Plant.Branches[N] * (Parameter.BranchSenesc + Derived.CLossMortality);
Litter.CoarseRoot[N] := Plant.CoarseRoot[N] * Derived.CLossMortality;
Litter.FineRoot[N] := Plant.FineRoot[N] * (Parameter.RootSenesc + Derived.CLossMortality + Derived.Desic);
Litter.Leaves[N] := Plant.Leaves[N] * (TotalLeafSenesc * Parameter.SenescLeafRatio);
Plant.Soluble[N] := Plant.Soluble[N] + Plant.Leaves[N] *
                    (TotalLeafSenesc * (1 - Parameter.SenescLeafRatio));
Litter.Other[N] := Plant.Fruit[N] * (Parameter.FruitSenesc + Derived.CLossMortality) +
                   Plant.Pollen[N] * (Parameter.PollenSenesc + Derived.CLossMortality) +
                   Plant.Bark[N] * (Parameter.BarkSenesc + Derived.CLossMortality);
Plant.SapWood[N] := Plant.SapWood[N] * (1 - Derived.CLossMortality);
Plant.HeartWood[N] := Plant.HeartWood[N] * (1 - Derived.CLossMortality);
Plant.Branches[N] := Plant.Branches[N] * (1 - (Parameter.BranchSenesc + Derived.CLossMortality));
Plant.CoarseRoot[N] := Plant.CoarseRoot[N] * (1 - Derived.CLossMortality);
Plant.FineRoot[N] := Plant.FineRoot[N] * (1 - (Parameter.RootSenesc + Derived.CLossMortality + Derived.Desic));
Plant.Leaves[N] := Plant.Leaves[N] * (1 - TotalLeafSenesc);
Plant.Fruit[N] := Plant.Fruit[N] * (1 - (Parameter.FruitSenesc + Derived.CLossMortality));
Plant.Pollen[N] := Plant.Pollen[N] * (1 - (Parameter.PollenSenesc + Derived.CLossMortality));
Plant.Bark[N] := Plant.Bark[N] * (1 - (Parameter.BarkSenesc + Derived.CLossMortality));
PestNitrogenDamage;
End; {of procedure 'NitrogenLoss'}

Procedure CalcAllocParams;
Var NewNLimit, NRatio, Vegetative, AllocSum, ActLBRatio, InitialNonWoodAlloc, AdjustRatio: real48;
   Begin
   NRatio := Divide(Divide(Plant.Leaves[N], Plant.Leaves[C]),
                      (Parameter.InternalNRatio));
   IF NRatio < Parameter.N0 then
      NewNLimit := 0
   Else if NRatio < Parameter.Ncrit THEN
      NewNLimit := Divide((NRatio - Parameter.N0), (Parameter.Ncrit - Parameter.N0))
   ELSE
      NewNLimit := 1;
   If Plant.Age >= Parameter.SexAge then
      Begin
      Vegetative := 1 - Parameter.C_FruitAlloc - Parameter.C_PollenAlloc;
      Derived.C_ActFruitAlloc := Parameter.C_FruitAlloc;
      Derived.C_ActPollenAlloc := Parameter.C_PollenAlloc;
      End
   Else
      Begin
      Vegetative := 1;
      Derived.C_ActFruitAlloc := 0;
      Derived.C_ActPollenAlloc := 0;
      End;
   If Plant.Height > 0.1 then
      ActLBRatio := Parameter.LeafBranchRatio * 10 / Plant.Height
   Else
      ActLBRatio := Parameter.LeafBranchRatio * 100;
                   {The parameter 'LeafBranchRatio' is for a notionally 10-m high Plant. It
                    is recalculated here for the actual plant height, assuming that allocation to
                    branches and stems is proportional to stem height}
   Parameter.RootLeafRatio := Parameter.RootLeafRatio2 + NewNLimit * (Parameter.RootLeafRatio1 - Parameter.RootLeafRatio2);
   Derived.C_BranchAlloc := Vegetative / (1 + Parameter.WoodBranchRatio * (1 + Parameter.BarkWoodRatio + Parameter.CoarseRootWoodRatio)
                            + ActLBRatio * (1 + Parameter.RootLeafRatio));
   Derived.C_SapWoodAlloc := Derived.C_BranchAlloc * Parameter.WoodBranchRatio;
   Derived.C_LeafAlloc := Derived.C_BranchAlloc * ActLBRatio;
   Derived.C_FineRootAlloc := Derived.C_LeafAlloc * Parameter.RootLeafRatio;
   Derived.C_CoarseRootAlloc := Derived.C_SapWoodAlloc * Parameter.CoarseRootWoodRatio;
   Derived.C_BarkAlloc := Derived.C_SapWoodAlloc * Parameter.BarkWoodRatio;
   If Derived.C_SapWoodAlloc < Parameter.MinWoodAlloc then //less than the minimum specified for wood allocation
      Begin
      InitialNonWoodAlloc := (1 - Derived.C_SapWoodAlloc - Derived.C_CoarseRootAlloc - Derived.C_BarkAlloc - Derived.C_BranchAlloc);
      // Coarse roots, branches and bark are assumed to be so closely tied to wood growth that they are
      // adjusted together with any adjustments to wood allocation. Allocation to other biomass
      // components must then be adjusted pro-rata.
      Derived.C_CoarseRootAlloc := Parameter.MinWoodAlloc * Parameter.CoarseRootWoodRatio;
      Derived.C_BarkAlloc := Parameter.MinWoodAlloc * Parameter.BarkWoodRatio;
      Derived.C_BranchAlloc := Divide(Parameter.MinWoodAlloc, Parameter.WoodBranchRatio);
      Derived.C_SapWoodAlloc := Parameter.MinWoodAlloc;
      AdjustRatio := Divide((1 - Derived.C_SapWoodAlloc - Derived.C_CoarseRootAlloc - Derived.C_BarkAlloc - Derived.C_BranchAlloc), InitialNonWoodAlloc);
      Derived.C_ActFruitAlloc := Derived.C_ActFruitAlloc * AdjustRatio;
      Derived.C_ActPollenAlloc := Derived.C_ActPollenAlloc * AdjustRatio;
      Derived.C_LeafAlloc := Derived.C_LeafAlloc * AdjustRatio;
      Derived.C_FineRootAlloc := Derived.C_FineRootAlloc * AdjustRatio;
      IF Derived.C_LeafAlloc <= 0 then
         Begin
         MessageDlg('UNREALISTIC WOOD ALLOCATION SELECTED' + chr(10) +
             'With the selected minimum wood allocation,' +
             'the allocation to other biomass components would be 0 or negative.' +
             'Please, ensure that the allocation to wood, branches, bark and coarse roots' + chr(10) +
             'do not take more than 100% of carbon allocation.' +
             'Program excution will be stopped.',
              mtError, [mbOK], 0);
         Control.Run_On := false;
         End;
      End;
   Derived.N_LeafAlloc := Derived.C_LeafAlloc;
   Derived.N_SapWoodAlloc := Derived.C_SapWoodAlloc * Parameter.bWood;
   Derived.N_BranchAlloc := Derived.C_BranchAlloc * Parameter.bBranch;
   Derived.N_BarkAlloc := Derived.C_BarkAlloc * Parameter.bBark;
   Derived.N_FineRootAlloc := Derived.C_FineRootAlloc * Parameter.bRoots;
   Derived.N_CoarseRootAlloc := Derived.C_CoarseRootAlloc * Parameter.bWood;
   Derived.N_FruitAlloc := Derived.C_ActFruitAlloc * Parameter.bFruit;
   Derived.N_PollenAlloc := Derived.C_ActPollenAlloc * Parameter.bPollen;
   AllocSum := Derived.N_LeafAlloc + Derived.N_SapWoodAlloc + Derived.N_BranchAlloc + Derived.N_BarkAlloc +
               Derived.N_FineRootAlloc + Derived.N_CoarseRootAlloc + Derived.N_FruitAlloc + Derived.N_PollenAlloc;
   Derived.N_LeafAlloc := Derived.N_LeafAlloc / AllocSum;
   Derived.N_SapWoodAlloc := Derived.N_SapWoodAlloc / AllocSum;
   Derived.N_BranchAlloc := Derived.N_BranchAlloc / AllocSum;
   Derived.N_BarkAlloc := Derived.N_BarkAlloc / AllocSum;
   Derived.N_FineRootAlloc := Derived.N_FineRootAlloc / AllocSum;
   Derived.N_CoarseRootAlloc := Derived.N_CoarseRootAlloc / AllocSum;
   Derived.N_FruitAlloc := Derived.N_FruitAlloc / AllocSum;
   Derived.N_PollenAlloc := Derived.N_PollenAlloc / AllocSum;
End; {of Procedure 'CalcAllocParams'}

Procedure Allocation;
var NewCStruct, NewNStruct, NewNmax, KmC, KmN, AllC, AllN, MaxN,
    NewWood, GrowthRatio, DiamGround, Volume, CanopyWidth: real48;

    Procedure AllocateN(var PlantOrgan: TElements; NFract, CFract, NRatio, NGradient: Real48);
    {This routine checks whether the nitrogen newly allocated would produce new tissue with N concentrations
     that exceed the set maximum N limits for the plant. If it doesn't allocation is simple and based on
     previously calculated allocation ratios. However, if new tissues would have too high an N concentration
     then N is allocated only up to the specified maximum and the excess is added back into the plant soluble N pool.}
    var MaxAllocate: Real48;
    Begin
    MaxAllocate := CFract * NewCStruct * Parameter.Nmax * NRatio * NGradient;
    If (NFract * NewNStruct) < MaxAllocate then
       PlantOrgan[N] := PlantOrgan[N] + NFract * NewNStruct
    Else
       Begin
       PlantOrgan[N] := PlantOrgan[N] + MaxAllocate;
       Plant.Soluble[N] := Plant.Soluble[N] + NFract * NewNStruct - MaxAllocate;
       End;
    End; {of Procedure 'AllocateN'}

Begin
     CalcAllocParams;
     AllC := Plant.Bark[C] + Plant.Branches[C] + Plant.Reserves[C]
             + Plant.CoarseRoot[C] + Plant.FineRoot[C] + Plant.Leaves[C];
     AllN := Plant.Bark[N] + Plant.Branches[N] + Plant.Reserves[N]
             + Plant.CoarseRoot[N] + Plant.FineRoot[N] + Plant.Leaves[N];
     MaxN := Parameter.Nmax *
             (Parameter.InternalNRatio * Plant.Leaves[C]
             + Plant.Bark[C] * Parameter.bBark
             + Plant.Branches[C] * Parameter.bBranch
             + Plant.CoarseRoot[C] * Parameter.bWood
             + Plant.FineRoot[C] * Parameter.bRoots);
     If AllN = 0 then Alln := Plant.Soluble[N];
     KmC := AllC * Parameter.KmGrowth[C];
     KmN := AllN * Parameter.KmGrowth[N];
     Derived.MaxPlantNUptake := Parameter.ExcessNUptake * MaxN - AllN;
     If Derived.MaxPlantNUptake <= 0 then
        Derived.MaxPlantNUptake := 0;
     If Plant.Soluble[N] > Derived.MaxPlantNUptake then
     // During times of large N availability (after fertiliser application, for example), plants can take up
     // only a certain amount which increases with the size of plants and their degree of N starvation. The
     // excess is put back into soil mineral N pools - an indirect way of doing the same as though plants had never taken it up at all.
        Begin
        Derived.ExcessN := Plant.Soluble[N] - Derived.MaxPlantNUptake;
        Plant.Soluble[N] := Derived.MaxPlantNUptake;
        End
     Else
        Derived.ExcessN := 0;
     If (KmC > 0) and (KmN > 0) then
        Begin
        NewCStruct := Derived.WaterLimit * Divide(Sqr(Plant.Soluble[C]), (Plant.Soluble[C] + KmC));
        If NewCStruct > 0.1 * Plant.Soluble[C] then
           NewCStruct := 0.1 * Plant.Soluble[C];
        NewNStruct := Derived.WaterLimit * Divide(Sqr(Plant.Soluble[N]), (Plant.Soluble[N] + KmN));
        End
     Else if KmN > 0 then
        Begin
        NewNStruct := Derived.WaterLimit * Divide(Sqr(Plant.Soluble[N]), (Plant.Soluble[N] + KmN));
        NewCStruct := 0;
        End
     Else
        Begin
        NewCStruct := 0;
        NewNStruct := 0;
        End;
     Plant.Soluble[C] := Plant.Soluble[C] - NewCStruct;
     Plant.Soluble[N] := Plant.Soluble[N] - NewNStruct;
     If Parameter.RespnType = Basics then
        Begin
        Derived.Rg := NewCStruct * Parameter.GrowthRespn / (1 + Parameter.GrowthRespn);
        NewCStruct := NewCStruct / (1 + Parameter.GrowthRespn);
        Derived.NEE := Derived.NEE - Derived.Rg;
        End
     Else
        Derived.Rg := 0;
     Derived.DayCFlux := Derived.DayCFlux - (Derived.Rg + Derived.Rm) * Control.NSeconds / 86400;
     Derived.NightCFlux := Derived.NightCFlux - (Derived.Rg + Derived.Rm) * (86400 - Control.NSeconds) / 86400;
     NewWood := Derived.C_SapWoodAlloc * NewCStruct;
     Plant.SapWood[C] := Plant.SapWood[C] + NewWood;
     If Control.IncludeIsotopes then
        Begin
        Dilute (Plant.SapWood[C13], Plant.SapWood[C], Plant.Soluble[C13], NewWood);
        Dilute (Plant.Bark[C13], Plant.Bark[C], Plant.Soluble[C13], Derived.C_BarkAlloc * NewCStruct);
        Dilute (Plant.Branches[C13], Plant.Branches[C], Plant.Soluble[C13], Derived.C_BranchAlloc * NewCStruct);
        Dilute (Plant.CoarseRoot[C13], Plant.CoarseRoot[C], Plant.Soluble[C13], Derived.C_CoarseRootAlloc * NewCStruct);
        Dilute (Plant.FineRoot[C13], Plant.FineRoot[C], Plant.Soluble[C13], Derived.C_FineRootAlloc * NewCStruct);
        Dilute (Plant.Fruit[C13], Plant.Fruit[C], Plant.Soluble[C13], Derived.C_ActFruitAlloc * NewCStruct);
        Dilute (Plant.Pollen[C13], Plant.Pollen[C], Plant.Soluble[C13], Derived.C_ActPollenAlloc * NewCStruct);
        End;
     Plant.Bark[C] := Plant.Bark[C] + Derived.C_BarkAlloc * NewCStruct;
     Plant.Branches[C] := Plant.Branches[C] + Derived.C_BranchAlloc * NewCStruct;
     Plant.CoarseRoot[C] := Plant.CoarseRoot[C] + Derived.C_CoarseRootAlloc * NewCStruct;
     Plant.FineRoot[C] := Plant.FineRoot[C] + Derived.C_FineRootAlloc * NewCStruct;
     Plant.Reserves[C] := Plant.Reserves[C] + Derived.C_LeafAlloc * NewCStruct;
     Plant.Leaves[C] := Plant.Leaves[C] + Derived.LeafGrowth * Plant.Reserves[C];
     If Control.IncludeIsotopes then
        Begin
        Dilute (Plant.Reserves[C13], Plant.Reserves[C], Plant.Soluble[C13], Derived.C_LeafAlloc * NewCStruct);
        Dilute (Plant.Leaves[C13], Plant.Leaves[C], Plant.Soluble[C13], Derived.LeafGrowth * Plant.Reserves[C]);
        End;
     Plant.Reserves[C] := Plant.Reserves[C] * (1 - Derived.LeafGrowth);
     Plant.Fruit[C] := Plant.Fruit[C] + Derived.C_ActFruitAlloc * NewCStruct;
     Plant.Pollen[C] := Plant.Pollen[C] + Derived.C_ActPollenAlloc * NewCStruct;
     If Parameter.RespnType = Basics then
        Derived.SoilRespn := Derived.SoilRespn + NewCStruct * Parameter.GrowthRespn
           * (Derived.C_FineRootAlloc + Derived.C_CoarseRootAlloc);
     AllocateN (Plant.SapWood, Derived.N_SapWoodAlloc, Derived.C_SapWoodAlloc, Parameter.bWood, 1);
     AllocateN (Plant.Bark, Derived.N_BarkAlloc, Derived.C_BarkAlloc, Parameter.bBark, 1);
     AllocateN (Plant.Branches, Derived.N_BranchAlloc, Derived.C_BranchAlloc, Parameter.bBranch, 1);
     AllocateN (Plant.CoarseRoot, Derived.N_CoarseRootAlloc, Derived.C_CoarseRootAlloc, Parameter.bWood, 1);
     AllocateN (Plant.FineRoot, Derived.N_FineRootAlloc, Derived.C_FineRootAlloc, Parameter.bRoots, 1);
     AllocateN (Plant.Reserves, Derived.N_LeafAlloc, Derived.C_LeafAlloc,1, Parameter.InternalNRatio);
     Plant.Leaves[N] := Plant.Leaves[N] + Derived.LeafGrowth * Plant.Reserves[N];
     Plant.Reserves[N] := Plant.Reserves[N] * (1 - Derived.LeafGrowth);
     AllocateN (Plant.Fruit, Derived.N_FruitAlloc, Parameter.C_FruitAlloc, Parameter.bFruit, 1);
     AllocateN (Plant.Pollen, Derived.N_PollenAlloc, Parameter.C_PollenAlloc, Parameter.bPollen, 1);
     If Plant.Stocking * (Plant.SapWood[C] + Plant.HeartWood[C]) > 0 then
        Begin
        If (Plant.SapWood[C] + Plant.HeartWood[C]) > NewWood then
           GrowthRatio := (Plant.SapWood[C] + Plant.HeartWood[C]) /
                 (Plant.SapWood[C] + Plant.HeartWood[C] - NewWood)
        Else
           GrowthRatio := 1.2;
        If GrowthRatio > 1.2 then
           GrowthRatio := 1.2;
        {RGR of 20% taken as extreme daily maximum}
        If Plant.DBH > Parameter.Mindbh then
           Begin
           Plant.Height := Plant.Height * Power(GrowthRatio,
                   1 / (Parameter.WHSlope + Parameter.WDSlope / Parameter.HDSlope));
           Plant.DBH := Plant.DBH * Power(GrowthRatio,
                   1 / (Parameter.HDSlope * Parameter.WHSlope + Parameter.WDSlope));
        {The above equations are based on Korol, R.L., Running, S.W. and Milner, K.S. (1995):
         Incorporating intertree competition into an ecosystem model. Can J For Res 25: 413-424.
         However, retracing the maths for the Height calculation resulted in the eqn above
         that is slightly different from that of Korol et al. (1995).}
           End
        Else
           Begin
           Parameter.Form := 0.4;
           Volume := (Plant.SapWood[C] + Plant.HeartWood[C]) * Control.CConversion /
                     (1000 * Parameter.WoodDensity * Plant.Stocking);  // m3 tree-1
           Plant.Height := Power(4 * Sqr(Parameter.Ht_Diameter) * Volume / (pi * Parameter.Form), 1/3);
           If Plant.Height < 1.3 then
              Plant.DBH := 0   // by definition - if trees are less than 1.3 tall they cannot have a diameter at 1.3 m
           Else
              Begin
              DiamGround := 100 * Plant.Height / Parameter.Ht_Diameter;
              Plant.DBH := DiamGround * (Plant.Height - 1.3) / Plant.Height;
              End;
           End;
        Plant.Area := Plant.Stocking * pi * Sqr(Plant.DBH / 2) * 1.05;
        {The factor 1.05 is included to make allowance for the fact that the diameters
        of individual trees are not all the same. As basal area is calculated
        from the square of the diameters of trees, larger trees make a larger distribution
        to stand basal area than to mean stand diameter.
        The factor 1.05 translates to a standard deviation of tree diameters of about 23%
        - at BFG it was about 20%}
        CanopyWidth := 0.7544 + 0.2073 * Plant.DBH; // after Leech 1984 (in Madgwick 1994)
        Plant.CanopyCover := (pi * sqr(CanopyWidth / 2))*Plant.Stocking/10000; // added Simioni 19/02/2002
        If Plant.CanopyCover > 1 then
           Plant.CanopyCover := 1;
        End
     Else
        Begin
        Plant.Area := 0; Plant.DBH := 0; Plant.Height := 0; Plant.CanopyCover := 0;
        End;
End; {of procedure 'Allocation}

Procedure WaterIn;
Var Interception, SnowMelt, WaterIn, WaterRatio, LayerLimit: Real48;
    iLayer: integer;

BEGIN
Derived.All_Litter := 0.001 * Control.CConversion * (SoilOrganic.Struct[0, C] + SoilOrganic.Metab[0, C] +
              SoilOrganic.Active[0, C] + SoilOrganic.Slow[0, C] + SoilOrganic.Resistant[0, C]);
SoilWat.Layer[0].MaxWater := Parameter.LitterWHC * Derived.All_Litter * 0.1;
If Parameter.DirectEvapType = 'L' then
   Begin
   Interception := Parameter.DirectEvapSlope * Derived.LAI;   // foliage interception - litter interception treated differently now
   If Interception > (0.75 * Weather.Rain) then {Assume that at most 75% of rain can be intercepted}
      Interception := 0.75 * Weather.Rain;
   End
Else {If Parameter.DirectEvapType = 'C' then}
   Interception := Parameter.DirectEvapFract * Weather.Rain;
// Sequence to calculate snow and water percolation
WaterIn := Weather.Rain + Derived.IrrigateWater - Interception;
Derived.Evaporation := Interception;
If (Weather.Tmean < 0) and (WaterIn > 0) then
      Begin
      SoilWat.Snow := SoilWat.Snow + WaterIn;
      WaterIn := 0;
      End;
If SoilWat.Snow > 0 then
   Begin
   SnowMelt := Parameter.SnowMelt * Weather.TDay + Parameter.RadnMelt * Weather.PAR;
   If SnowMelt > 0 then
      Begin
      If SnowMelt > SoilWat.Snow then
         Begin
         WaterIn := WaterIn + SoilWat.Snow;
         SoilWat.Snow := 0;
         End
      Else {if SnowMelt < SoilWat.Snow then}
         Begin
         WaterIn := WaterIn + SnowMelt;
         SoilWat.Snow := SoilWat.Snow - SnowMelt;
         End;
      End;
   End;
Derived.WaterLimit := 0;
For iLayer := 0 to SoilWat.nLayers do
    Begin
    SoilWat.Layer[iLayer].WaterContent := SoilWat.Layer[iLayer].WaterContent + WaterIn;
    If SoilWat.Layer[iLayer].WaterContent > SoilWat.Layer[iLayer].MaxWater then
       Begin
       WaterIn := SoilWat.Layer[iLayer].WaterContent - SoilWat.Layer[iLayer].MaxWater;
       SoilWat.Layer[iLayer].Percolate := WaterIn;
       SoilWat.Layer[iLayer].WaterContent := SoilWat.Layer[iLayer].MaxWater;
       End
    Else
       WaterIn := 0;
    Derived.Drainage[iLayer] := WaterIn;
    If SoilWat.SeparateSensitivity and (iLayer > 0) then
       Begin
       LayerLimit := Divide(Divide(SoilWat.Layer[iLayer].WaterContent, SoilWat.Layer[iLayer].MaxWater), Parameter.StressLimit);
       If LayerLimit > 1 then LayerLimit := 1;
       Derived.WaterLimit := Derived.WaterLimit + SoilWat.Layer[iLayer].StressSensitivity * LayerLimit;
       End;
    End;
If Not SoilWat.SeparateSensitivity then
   Derived.WaterLimit := Divide(Divide(SoilWat.TotalWater, SoilWat.MaxWater), Parameter.StressLimit);
If Derived.WaterLimit > 1 then
   Derived.WaterLimit := 1;
Derived.BallBerry := (Parameter.BallBerry2 + (Parameter.BallBerry1 - Parameter.BallBerry2) * Derived.WaterLimit);
For iLayer := 0 to SoilWat.nLayers do
    Begin
    If SoilWat.Layer[iLayer].MaxWater > 0 then
       WaterRatio :=  SoilWat.Layer[iLayer].WaterContent / SoilWat.Layer[iLayer].MaxWater
    Else
       WaterRatio := 1;
    If WaterRatio < Parameter.StressLimit then
       Derived.DecompWatLimit[iLayer] := Parameter.MinDecomp + (1 - Parameter.MinDecomp)
              * Power(WaterRatio / Parameter.StressLimit, Parameter.RelWaterSens)
    Else
       Derived.DecompWatLimit[iLayer] := 1;
    End;
End; {of procedure 'WaterIn'}

Procedure WaterOut;
Var LeafW, DeltaW, A, LitterEvaporation, SoilEvaporation, ActualExtract, WaterRatio,
    LitterFract, Sigma, ra, rc, rs, MaxSoil, Rnet, Gamma2, EvapSum: real48;
    iLayer, EvapLayer: integer;
    Extract, Evap: array[0..MaxSoilLayers] of real48;

BEGIN
// Sequence to calculate soil and litter evaporation
LeafW := Divide(Weather.AbsHumidity, Weather.RelHumidity);
DeltaW := LeafW - Weather.AbsHumidity;
If Plant.Newgrowth[C] > 0 then
   Begin
   A := 8333 * Divide(Plant.Newgrowth[C], Control.nSeconds);
   Derived.gs := 1.6 * A / (Weather.CO2 - (1000 * Derived.p_internal / Parameter.AtmosPressure));
   End
Else
   Derived.gs := 0;
{Convert Newgrowth [kg ha-1 d-1] to [umol m-2 s-1] by constant 8333; gs then in units of mol m-2 s-1.
 Calculation of gs uses the Ball-Berry assumption gs = A * hs / cs}
If Derived.gs > 0 then
   rc := 40 / Derived.gs
Else If Derived.LAI > 0.01 then
   Rc := 10000 / Derived.LAI
Else
   rc := 100000;
If Control.nSeconds > 0 then
   Rnet := Derived.LightAbs * 1e6 / Control.nSeconds   {Takes only the directly received radiation into account
                                         after losses for reflection, transmission and pass through canopy}
else
   Rnet := 0;
Gamma2 := GammaSea * Parameter.AtmosPressure / 1000;
Sigma := LeafW * 4097.934e5 / sqr(Weather.Tday + 237.3);
Derived.Transpiration := Control.nSeconds * (Sigma * Rnet + DeltaW * 1e5 * rho * Cp / Parameter.AeroResist) /
	(Latent * (Sigma + Gamma2 * (Parameter.AeroResist + rc) / Parameter.AeroResist));
SoilWat.MaxExtract := 0; ActualExtract := 0; SoilWat.TotalWater := 0;
If (Control.nSeconds = 0) or (SoilWat.Snow > 0) then
//  Evaporation := 0 - this does not have to be specified further
Else
   Begin
   ra := Parameter.AeroResist * 5; {Assume aerodynamic resistance five times as great from forest floor as from canopy}
   LitterFract := 1 - exp(-0.01 * Derived.All_Litter * Parameter.Mulching);
   Rnet := (1 - Derived.LightInter) * Weather.PAR * 1e6 / Control.nSeconds;
   If SoilWat.Layer[0].WaterContent > 1e-6 then
      rs := 800 * (SoilWat.Layer[0].MaxWater / SoilWat.Layer[0].WaterContent) // Litter diffusion resistance - loosely equivalent to stomatal resistance
   Else
      rs := 100000;
// Loosely based on Camillo and Gurney (1986) Soil Science 141: 95-105.
   LitterEvaporation := LitterFract * Control.nSeconds * (Sigma * Rnet + DeltaW * 1e5 * rho * Cp / ra) /
                    (Latent * (Sigma + Gamma2 * (ra + rs) / ra));
   If SoilWat.Layer[1].WaterContent > (0.01 * SoilWat.Layer[1].MaxWater) then
      rs := 800 * (SoilWat.Layer[1].MaxWater / SoilWat.Layer[1].WaterContent) // Litter diffusion resistance - loosely equivalent to stomatal resistance
   Else
      rs := 10000;
   SoilEvaporation := (1 - LitterFract) * Control.nSeconds * (Sigma * Rnet + DeltaW * 1e5 * rho * Cp / ra) /
                    (Latent * (Sigma + Gamma2 * (ra + rs) / ra));
   If SoilWat.Layer[0].MaxWater > 0 then
      Begin
      If LitterEvaporation > SoilWat.Layer[0].WaterContent then
         LitterEvaporation := SoilWat.Layer[0].WaterContent;
      End
   Else
      LitterEvaporation := 0;
   SoilWat.Layer[0].WaterContent := SoilWat.Layer[0].WaterContent - LitterEvaporation;
   Derived.Evaporation := Derived.Evaporation + LitterEvaporation;
   If SoilEvaporation > 0 then
      Begin
      EvapSum := 0;
      EvapLayer := 1;
      While SoilWat.Layer[EvapLayer].RelEvap > 0 do
          Begin
          Evap[EvapLayer] := Parameter.SoilEvap * SoilWat.Layer[EvapLayer].RelEvap * SoilWat.Layer[EvapLayer].WaterContent / SoilWat.Layer[EvapLayer].MaxWater;
          EvapSum := EvapSum + Evap[EvapLayer];
          EvapLayer := EvapLayer + 1;
          End;
      If EvapSum < SoilEvaporation then
         SoilEvaporation := EvapSum;
      If EvapSum > 0 then
         Begin
         For iLayer := 1 to EvapLayer - 1 do
             Begin
             Evap[iLayer] := Evap[iLayer] * SoilEvaporation / EvapSum;
             If Evap[iLayer] < SoilWat.Layer[iLayer].WaterContent then
                Begin
                SoilWat.Layer[iLayer].WaterContent := SoilWat.Layer[iLayer].WaterContent - Evap[iLayer];
                Derived.Evaporation := Derived.Evaporation + Evap[iLayer];
                End
             Else
                Begin
                Derived.Evaporation := Derived.Evaporation + SoilWat.Layer[iLayer].WaterContent;
                SoilWat.Layer[iLayer].WaterContent := 0;
                End;
             End;
         End;
      End;
   End;

// Plant water extraction
For iLayer := 1 to SoilWat.nLayers do
    Begin
    If SoilWat.Layer[iLayer].MaxWater > 0 then
       Begin
       WaterRatio :=  SoilWat.Layer[iLayer].WaterContent / SoilWat.Layer[iLayer].MaxWater;
       If WaterRatio > Parameter.StressLimit then
          Extract[iLayer] := SoilWat.Layer[iLayer].ExtractEffort
       else
          Extract[iLayer] := SoilWat.Layer[iLayer].ExtractEffort * WaterRatio / Parameter.StressLimit;
       ActualExtract := ActualExtract + Extract[iLayer];
       SoilWat.MaxExtract := SoilWat.MaxExtract + SoilWat.Layer[iLayer].ExtractEffort;
       End;
    End;
If SoilWat.MaxExtract > 0 then
   ActualExtract := ActualExtract / SoilWat.MaxExtract
Else
   ActualExtract := 0;
For iLayer := 1 to SoilWat.nLayers do  // no plant water extraction out of litter layer
    If ActualExtract > 0 then
       Extract[iLayer] := Extract[iLayer] / ActualExtract
    Else
       Extract[iLayer] := 0;
For iLayer := 1 to SoilWat.nLayers do
    Begin
    If Extract[iLayer] > 0 then
       SoilWat.Layer[iLayer].WaterContent := SoilWat.Layer[iLayer].WaterContent - Derived.Transpiration *
                                       Extract[iLayer] / SoilWat.MaxExtract;
    If SoilWat.Layer[iLayer].WaterContent < 0 then SoilWat.Layer[iLayer].WaterContent := 0;
    SoilWat.TotalWater := SoilWat.TotalWater + SoilWat.Layer[iLayer].WaterContent;
    End;
IF Derived.WaterLimit < 1 THEN
   Begin
   Derived.Desic := Parameter.DrySenesc * (1 - Derived.WaterLimit);
   If Derived.WaterLimit < Parameter.DeathLimit then
      Derived.DroughtMort := Parameter.DryDeath
   else
      Derived.DroughtMort := 0;
   End
Else
   Begin
   Derived.Desic := 0;
   Derived.DroughtMort := 0;
   End;
End; {of procedure 'WaterOut'}

Procedure GetWeather;
Var   Pd, DCLN, RelDaylen, Q0: real48;
      i, CountLines: Integer;
      St, DataIn, MessageToUser: String;
      FileFound: Boolean;
      WeatherOpts: WeatherFileOptions;

      Procedure WeatherDetails (var DataIn: Real48);
      var NextDatum: Real48;
      Begin
      Read (Control.DefClim, NextDatum);
      DataIn := NextDatum;
      End; {of Procedure 'WeatherDetails'}

     Procedure DayLength(JulianDay: integer; var RelDaylen, DCLN: real48; Lat: real48);
     {Based on Collares-Pereira and Rabl (1979):
     The average distribution of solar radiation - correlation between diffuse
     and hemispherical and between daily and hourly insolation values. Solar Energy 22: 155-164.
     I modified it by replacing the non-circular orbit term.}
     var gamma, dnlat: real48;
         Begin
         gamma := 2 * pi * (JulianDay + 284) / 365.24;
         DCLN := InvSin(0.3979 * sin(gamma));
         dnlat := tan(LAT * 0.0174533) * tan(DCLN);
         If dnlat >= 1 then
            RelDaylen := pi
         else if dnlat < -1 then
            RelDaylen := 0
         Else
           RelDaylen := pi - InvCos(dnlat);
         End; {of Procedure 'DayLength'}

Begin
if (Control.ClimType = 'O') and (not Control.ClimFileOpen) then
   Begin
   FileFound := FileExists(Control.CLIMFILE);
   If not FileFound then
      Begin
      MessageToUser := '        CLIMATE FILE NOT FOUND!' + chr(10) +
           'The climate file may be in the wrong subdirectory' + chr(10) +
           'or you may not have created or copied one' + chr(10) +
           'or deleted one that had been created before.' + chr(10) + chr(10) +
           'The program will run this simulation with simulated climate.' + chr(10) +
           'Choose a valid climate file under ''PARAMETERS'' and ''CONTROL''' + chr(10) +
           'if you want to run a simulation based on observed climate.';
       // show the error message
       ShowMessage(MessageToUser);
       Control.ClimType := 'S';
       Control.ProjectHasChanged := true;
       End;
   End;
If Control.ClimType = 'U' then
   Begin
   Weather.Tmax := Parameter.MeanTmax + Weather.ExtraTemp;
   Weather.Tmin := Weather.LastMin;
   Weather.Lastmin := Parameter.MeanTmin + Weather.ExtraTemp;
   Weather.Tsoil := Parameter.MeanSoilTemp + Weather.ExtraTemp;
   Weather.PAR := Parameter.MeanRadn;
   Weather.Rain := Parameter.AnnualRain / 365;
   If Parameter.MeanAbsHum > 0 then
      Weather.AbsHumidity := Parameter.MeanAbshum
   Else
      Weather.AbsHumidity := Divide(SatHumidity(Weather.Tmin), Parameter.AtmosPressure);
   Derived.WaterLimit := 1;
   Weather.Tmean := 0.5 * (Weather.Tmax + Weather.Tmin);
   Weather.TDay := 0.606 * Weather.Tmax + 0.394 * Weather.Tmin;
   Weather.RelHumidity := Divide(Weather.AbsHumidity, Divide(SatHumidity(Weather.Tday), Parameter.AtmosPressure));
   Weather.LastAbsHum := Weather.AbsHumidity;
   Weather.LastRelHum := Weather.RelHumidity;
   End
ELSE IF Control.ClimType = 'S' THEN
   BEGIN
   Weather.Tmax := Weather.ExtraTemp + Parameter.MeanTmax +
                   Parameter.Temp_Amplitude * SIN(PI * (Control.TotalDays - Parameter.WarmestDay + 91.25)/182.5);
   Weather.Tmin := Weather.LastMin;
   Weather.Lastmin := Weather.ExtraTemp + Parameter.MeanTmin +
                   Parameter.Temp_Amplitude * SIN(PI * (Control.TotalDays - Parameter.WarmestDay + 91.25)/182.5);
   Weather.Tmean := 0.5 * (Weather.Tmax + Weather.Tmin);
   Weather.TDay := 0.606 * Weather.Tmax + 0.394 * Weather.Tmin;
   If Parameter.MeanRadn > 0 then
      Begin
      Weather.PAR := Parameter.MeanRadn +
                  Parameter.RADN_AMPLITUDE * SIN(PI * (Control.TotalDays - Parameter.MostPAR + 91.25) / 182.5);
      IF Weather.PAR < 1 THEN
         Weather.PAR := 1;
      End
   Else
      Weather.PAR := -1;
   If Parameter.MeanAbsHum > 0 then
      Weather.AbsHumidity := Parameter.MeanAbshum + Parameter.Humid_AMPLITUDE
                       * SIN(PI * (Control.TotalDays - Parameter.WarmestDay + 91.25)/182.5)
   Else
      Weather.AbsHumidity := Divide(SatHumidity(Weather.Tmin), Parameter.AtmosPressure);
   If Weather.AbsHumidity < 0 then
      Weather.AbsHumidity := 0;
   Weather.RelHumidity := Divide(Weather.AbsHumidity,
                          Divide(SatHumidity(Weather.Tday), Parameter.AtmosPressure));
   Weather.LastAbsHum := Weather.AbsHumidity;
   Weather.LastRelHum := Weather.RelHumidity;
   END
Else if Control.ClimType = 'O' then
   Begin
       If (not Control.ClimFileOpen) then
          Begin
          CountLines := 0;
          ASSIGN (Control.DefClim, Control.CLIMFILE);
          RESET (Control.DefClim);
          Control.ClimFileOpen := TRUE;
          Repeat
              Readln (Control.DefClim, St);
              CountLines := CountLines + 1;
              If Length(St) = 0 then St := 'Blank line';
          Until St[1] <> '*';   // Count how many comment lines there are
          Repeat
              DataIn := 'Blank';
              For i := 1 to Length(St) do
                  Begin
                  If (St[i] in ['0'..'9', '-', '+', '.']) and (Datain = 'Blank') then
                     DataIn := 'Numeric'
                  Else if (St[i] <> ' ') and (DataIn = 'Blank') then
                     DataIn := 'Text';
                  End;
              If DataIn <> 'Numeric' then
                 Begin
                 Readln (Control.DefClim, St);
                 CountLines := CountLines + 1;
                 End;
          Until DataIn = 'Numeric';
          // Count how many further lines there are that contain non-numeric characters.
          // Numbers starting with a dot, '+' or a '-' are accepted here as numeric
          Close (Control.DefClim);
          ASSIGN (Control.DefClim, Control.CLIMFILE);
          RESET (Control.DefClim);
          For i := 1 to CountLines -1 do
              Readln (Control.DefClim);
          If not Control.StartRun then
             Begin
             St := 'Re-reading climate file' + Control.Date;
             AddNotice(St);
             End;
          End;
       For WeatherOpts := W_Tmax to W_CO2 do
          If WeatherFile[WeatherOpts] then
             Case WeatherOpts of
               W_Tmax:  WeatherDetails(Weather.Tmax);
               W_Tmin:  Begin
                        Weather.Tmin := Weather.LastMin;
                        WeatherDetails(Weather.LastMin);
               // use the minimum temperature from the previous night as that is more effective
               // in controlling humidity during the day than conditions of the following night.
               // The same for humidity if it is to be read - see below
                        End;
               W_Tmean: WeatherDetails(Weather.Tmean);
               W_Tsoil: WeatherDetails(Weather.Tsoil);
               W_PAR:   WeatherDetails(Weather.PAR);
               W_Rain:  WeatherDetails(Weather.Rain);
               W_AbsHum:Begin
                        Weather.AbsHumidity := Weather.LastAbsHum;
                        WeatherDetails(Weather.LastAbsHum);
                        End;
               W_RelHum:Begin
                        Weather.RelHumidity := Weather.LastRelHum;
                        WeatherDetails(Weather.LastRelHum);
                        End;
               W_CO2:   WeatherDetails(Parameter.CO2Conc);
               End;
       Readln (Control.DefClim);
       If eof(Control.Defclim) then
          Begin
          CLOSE (Control.DefClim);
          Control.ClimFileOpen := false;
          End;
   If not WeatherFile[W_PAR] then Weather.PAR := -1;
   Weather.Tmax := Weather.Tmax + Weather.ExtraTemp;
   Weather.Tmin := Weather.Tmin + Weather.ExtraTemp;
   If not WeatherFile[W_TMean] then
      Weather.Tmean := 0.5 * (Weather.Tmax + Weather.Tmin)
   Else
      Weather.Tmean := Weather.Tmean + Weather.ExtraTemp;
   Weather.TDay := 0.606 * Weather.Tmax + 0.394 * Weather.Tmin;
   {Running, S.W., Ramakrishna, R.N. and Hungerford, R.D. (1987): Extrapolation of synoptic
    meteorological data in mountainous terrain and its use for simulating forest evapotranspiration
    and photosynthesis.  Can J. For Res. 17: 472-483.}
   Weather.TNight := 2 * Weather.Tmean - Weather.Tday;
   {Assuming that Tmean is the mean of Tday and Tnight}
   If not WeatherFile[W_AbsHum] then
      Weather.AbsHumidity := Divide(SatHumidity(Weather.Tmin), Parameter.AtmosPressure);
   If not WeatherFile[W_RelHum] then
      Weather.RelHumidity := Divide(Weather.AbsHumidity, Divide(SatHumidity(Weather.Tday), Parameter.AtmosPressure));
   END; {of else if Control.ClimType = 'O' THEN}
IF (Control.ClimType = 'S') or
   ((Control.ClimType = 'O') and not WeatherFile[W_Rain]) THEN
   Begin
   If Parameter.AnnualRain > 0 then
      Begin
      Pd := 365 * Parameter.RainProb / Parameter.AnnualRain;
      Weather.RainProb := Parameter.RainProb;
      End
   else
      Weather.RainProb := 0;
   Weather.Rain := 0;
   If Weather.RainProb > 0 then
        Begin
        If Random(10000) < (10000 * Weather.RainProb) then
          Weather.Rain := Weather.Rain -ln(1 - Random(10000) * 0.0001) / Pd;
          {Assumes that rainfall distribution follows a negative exponential relationship so that
           the probability of getting a rainfall event of intensity 'Rain' mm is given by:
           P(Rain) = P0 * exp(-Rain * Pd), where the constants 'P0'= (Pd * Pr) and 'Pd' can be deduced from
           the given daily rainfall probability and the annual amount of rainfall}
        end
      else
        Weather.Rain := 0;
   End;
If Weather.RelHumidity > 1 then Weather.RelHumidity := 1;
Weather.Rain := Weather.Rain * Weather.ExtraRain;
Weather.CO2 := (Parameter.CO2Conc + Weather.ExtraCO2);
If Weather.PAR < 0 then
   Begin
   DayLength(Control.JulianDay, RelDaylen, DCLN, Parameter.Latitude);
   Q0 := 0.086400 * 1360 * sqr(1 + 0.01705 * cos(2 *pi* (Control.JulianDay-3) / 365.24))
         * SIN(pi * Parameter.Latitude /180) * SIN(DCLN)*(RelDaylen - TAN(RelDaylen)) / pi ;
   If (Weather.Tmax - Weather.Tmin) > 1 then {safe-guard if there is an extremely small diurnal T range}
      Weather.PAR := Q0 * 0.718 * (1 - EXP(-0.121 * (Weather.Tmax - Weather.Tmin)))
               * (1 - 0.34 * (1 - EXP(-0.259 * Weather.Rain)))
   Else
      Weather.PAR := Q0 * (0.147 + 0.750 * (1 - EXP(-0.09))) * (1 - 0.344 * (1 - EXP(-0.331 * Weather.Rain)));
   {This routine (+ parameters) is described in Kirschbaum et al. (2006):
   }
   End;
If Weather.PAR = 0 then Weather.PAR := 0.01;
IF (control.ClimType <> 'U') and not ((Control.ClimType = 'O') and WeatherFile[W_TSoil]) then
   Begin
   Derived.SoilTRadnEffect := 1 + Parameter.MaxTBoost * exp(-Parameter.TLAISensitivity * Derived.LAI);
   If Control.StartRun and not Control.TSoilFound then
      Weather.Tsoil := Weather.Tmean * Derived.SoilTRadnEffect
   Else
      Begin
      If SoilWat.Snow = 0 then
         Weather.Tsoil := Weather.Tsoil + (Weather.Tmean * Derived.SoilTRadnEffect - Weather.Tsoil) / Parameter.SoilTResist
      Else if Weather.Tmean > 0 then
         {If the ground is snow covered then the temperature at the top of the soil cannot be greater
          than 0 degrees and soil warming is delayed. Warming is determined by the lesser of warming by
          0 degree soil surface under the snow layer or by transfer of heat through the soil blanket.}
         Begin
         If (-Weather.Tsoil / Parameter.SoilTResist) <
            ((Weather.Tmean * Derived.SoilTRadnEffect - Weather.Tsoil) / (Parameter.SoilTResist + SoilWat.Snow * Parameter.SnowInsulate)) then
                Weather.Tsoil := Weather.Tsoil - Weather.Tsoil / Parameter.SoilTResist
         Else
             Weather.Tsoil := Weather.Tsoil + (Weather.Tmean * Derived.SoilTRadnEffect - Weather.Tsoil) /
               (Parameter.SoilTResist + SoilWat.Snow * Parameter.SnowInsulate);
         End
      Else
         Weather.Tsoil := Weather.Tsoil + (Weather.Tmean * Derived.SoilTRadnEffect - Weather.Tsoil) /
                         (Parameter.SoilTResist + SoilWat.Snow * Parameter.SnowInsulate);
      End
   End
Else
   Weather.TSoil := Weather.TSoil + Weather.ExtraTemp;
END; {of Procedure 'GetWeather'}

Procedure CalcPhenology;
var NextPhenology: Boolean;
Begin
If (Control.JulianDay = (Parameter.WarmestDay + 183)) or (Control.JulianDay = (Parameter.WarmestDay - 183)) then
   Derived.HeatSum := 0;
If (Weather.Tmean) > Parameter.Phenology.Threshold then
   Derived.HeatSum := Derived.HeatSum + Weather.Tmean - Parameter.Phenology.Threshold;
If Parameter.Phenology.nChanges = 0 then
   Begin
   Derived.Deciduous := 0;
   Derived.LeafGrowth := 1;
   End
Else
   Begin
   NextPhenology := false;
   If Parameter.Phenology.Units[Control.NextPhenology] = JulianDay then
      Begin
      If Control.JulianDay = Parameter.Phenology.JulianDay[Control.NextPhenology] then
         NextPhenology := true;
      End
   Else if Parameter.Phenology.Units[Control.NextPhenology] = nDays then
      Begin
      Control.PhenologyDayCount := Control.PhenologyDayCount + 1;
      If Control.PhenologyDayCount >= Parameter.Phenology.nDays[Control.NextPhenology] then
         NextPhenology := true;
      End
   Else if Parameter.Phenology.Units[Control.NextPhenology] = HeatSum then
      Begin
      If Derived.HeatSum >= Parameter.Phenology.HeatSum[Control.NextPhenology] then
         NextPhenology := true;
      End
   Else if Parameter.Phenology.Units[Control.NextPhenology] = DayLength then
      Begin
      If Control.nSeconds <= (3600 * Parameter.Phenology.DayLength[Control.NextPhenology]) then
         NextPhenology := true;
      End;
   If NextPhenology then
      Begin
      If Parameter.Phenology.Senescence[Control.NextPhenology] >= 0 then
         Derived.Deciduous := Parameter.Phenology.Senescence[Control.NextPhenology];
      If Parameter.Phenology.LeafGrowth[Control.NextPhenology] >= 0 then
         Derived.LeafGrowth := Parameter.Phenology.LeafGrowth[Control.NextPhenology];
      Control.NextPhenology := Control.NextPhenology + 1;
      If Control.NextPhenology > Parameter.Phenology.nChanges then
         Control.NextPhenology := 1;
      if Parameter.Phenology.Units[Control.NextPhenology] = nDays then
         Control.PhenologyDayCount := 0;
      End;
   End;
End; {of Procedure 'CalcPhenology'}

Procedure SetPestDamage;
Var St: String;
Begin
If Control.PestMode then
   If (Event.PestTimes[Control.NextPest + 1, 4] = Control.TotalDays) then
      Begin
      Control.PestMode := false;
      Derived.PestLeafDamage := 0;
      Derived.PestSolubleDamage := 0;
      Derived.PestSenesc := 0;
      Derived.PestMortality := 0;
      Derived.PestPhsFrac := 1;
      Control.NextPest := Control.NextPest + 1;
      End;
If not Control.PestMode and (Control.NextPest <= Event.nPests) then
   If Event.PestTimes[Control.NextPest, 4] <= Control.TotalDays then
      Begin
      Control.PestMode := true;
      If Event.PestDamageUnits = '%' then // Interpret data as fractional losses
         Begin
         Derived.PestLeafDamage := Event.LeafDamage[Control.NextPest] / Control.CConversion;
         Derived.PestSolubleDamage := Event.SolubleDamage[Control.NextPest] / Control.CConversion;
         Derived.PestSenesc := Event.SenescenceDamage[Control.NextPest] / Control.CConversion;
         End
      Else        // Interpret data as absolute losses - need to convert from DW to C
         Begin
         Derived.PestLeafDamage := Event.LeafDamage[Control.NextPest];
         Derived.PestSolubleDamage := Event.SolubleDamage[Control.NextPest];
         Derived.PestSenesc := Event.SenescenceDamage[Control.NextPest];
         End;
      Derived.PestPhsFrac := (1 - Event.PhotosynthesisFraction[Control.NextPest]);
      Derived.PestMortality := Event.PestMortality[Control.NextPest];
      Derived.PestDeathRatio := Event.PestDeathRatio[Control.NextPest];
      If Event.PestTimes[Control.NextPest, 5] > 0 then
         Begin
         St := 'Pest out-break started on ' + Control.Date + ' lasting for ' +
         VarToStr(Event.PestTimes[Control.NextPest, 5]) + ' days';
         End
      Else if (Derived.PestPhsFrac = 1) and (Derived.PestMortality = 0) and (Derived.PestSenesc = 0)
          and (Derived.PestSolubleDamage = 0) and (Derived.PestLeafDamage = 0) then
         St := 'Pest out-break ended on ' + Control.Date
      Else
         St := 'Pest out-break started on ' + Control.Date;
      AddNotice(St);
      End;
End; {of Procedure 'SetPestDamage'}

Procedure EventController;
var i: longint;
    WaterIn: real48;
    St: string[String_t_len];
Begin
     With Event do
          Begin
          Derived.IrrigateWater := 0;
          If Irrigate then
                 Begin
                 DaysSinceIrrigation := DaysSinceIrrigation + 1;
                 If DaysSinceIrrigation = IrrigationInterval then
                    Begin
                    If not Control.IrrigAnnounced then
                       Begin
                       Control.IrrigAnnounced := true;
                       St := 'Irrigation started on ' + Control.Date;
                       AddNotice(St);
                       End;
                    If Control.TotalDays > Control.LastIrrigation then
                       Begin
                       DaysSinceIrrigation := Control.TotalDays - Control.MaxDays;
                       St := 'Irrigation ended on ' + Control.Date;
                       AddNotice(St);
                       End
                    Else
                       DaysSinceIrrigation := 0;
                    If IrrigationType = 'R' then
                       Derived.IrrigateWater := IrrigationAmount
                    Else
                       Begin
                       If (SoilWat.TotalWater / SoilWat.MaxWater) < IrrigationFraction then
                          Derived.IrrigateWater := IrrigationFraction * SoilWat.MaxWater - SoilWat.TotalWater;
                       End; {else (if not (IrrigationType = 'R') then}
                    End; {If DaysSinceIrrigation = IrrigationInterval then }
                 End; {If Irrigate}
          If Control.NextFertilise <= nFertilisations then
             If FertiliseTimes[Control.NextFertilise, 4] = Control.TotalDays then
                    Begin
                    St := 'Fertiliser applied on ' + Control.Date;
                    AddNotice(St);
                    FertiliserAdded := FertiliserAdded + FertiliseAmount[Control.NextFertilise];
                    Control.NextFertilise := Control.NextFertilise + 1;
                    End; {of statement under 'If Control.NextFertilise ..'}
          If Control.NextEnvironment <= nEnvironments then
             If EnvironmentTimes[Control.NextEnvironment] = Control.TotalDays then
                    Begin
                    St := 'New environment from ' + Control.Date;
                    AddNotice(St);
                    Weather.ExtraCO2 := CO2[Control.NextEnvironment];
                    Weather.ExtraTemp := Temperature[Control.NextEnvironment];
                    Weather.ExtraRain := Rainfall[Control.NextEnvironment];
                    Control.NextEnvironment := Control.NextEnvironment + 1;
                    End;
          If Control.NextPest <= nPests then ;
          End;
End; {of Procedure 'EventController'}

End.
