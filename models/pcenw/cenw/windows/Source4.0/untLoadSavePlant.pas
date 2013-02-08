{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : GenPlant                                         =
  =             SavePlant                                        =
  =                                                              =
  =             Routines to read the plant parameters file.      =
  =             The save/retrieve algorithms are set up in such  =
  =             a way that new information can easily be added   =
  =             to the list of parameters saved previously       =
  =             so that saved information can remain valid over  =
  =             successvive upgrades of the program.             =
  ================================================================
  = File      : untLoadSavePlant.PAS                             =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

Unit untLoadSavePlant;

{$V-}

INTERFACE

Uses
  SysUtils, untDeclarations, untDivideValidation, untFieldValidation;

Procedure BooleanPlantVariables(ReadWrite: Char; CommentStr, VarStr: String; var defp: Text);
Procedure RealPlantVariables(ReadWrite: Char; CommentStr, VarStr: String; var defp: Text);
Procedure GenPlant(name : string);
Procedure SavePlant(name : string);

IMPLEMENTATION

Procedure ReadReal (Var Item: Real48; Comment, CommentStr, VarStr: String);
var ErrCode: Integer;
Begin
If CommentStr = Comment then
   val (VarStr, Item, ErrCode);
End; {of Procedure 'ReadReal'}

Procedure RealOut (Datum: real48; Comment: string; var Defp: Text);
var Width, Digits: integer;
Begin
GetField(Datum, ParFileMaxWidth, Width, Digits);
Writeln (Defp, Datum: ParFileMaxWidth: Digits, ' ', Comment);
End;  {of Procedure 'RealOut'}

Procedure ReadBoolean (Var Bool_in: Boolean; ProcessString, CommentStr, VarStr: String);
Begin
If ProcessString = CommentStr then
   Begin
   IF VarStr[1] = 'T' then
      Bool_in := true
   Else
      Bool_in := false;
   End;
End; {of Procedure 'ReadBoolean'}

Procedure BooleanOut (Bool_Out: Boolean; Comment: string; var Defp: Text);
Var Ch: char;
Begin
IF Bool_Out then
   Writeln (Defp, 'True   ', Comment)
Else
   Writeln (Defp, 'False   ', Comment);
End; {of Procedure 'BooleanOut'}

Procedure BooleanPlantVariables(ReadWrite: Char; CommentStr, VarStr: String; var defp: Text);

    Procedure ProcessNext(var NextVar: Boolean; ProcessString: String; DefaultValue: Boolean;
                          CommentStr, VarStr: String; ReadWrite: Char; var Defp: Text);
    Begin
    if ReadWrite = 'R' then
       ReadBoolean (NextVar, ProcessString, CommentStr, VarStr)
    Else if ReadWrite = 'W' then
       BooleanOut (NextVar, ProcessString, Defp)
    Else {if ReadWrite = 'D' then}
       NextVar := DefaultValue;
    End;

Begin
ProcessNext (Parameter.VariableNFixation, 'Calculate biological N fixation as a function of N status?', True, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.VariableHD, 'Recalculate the allometric HD relationship based on on-going data?', False, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.UseAllometrics, 'Use the W=f(h,d) relationship for calculating height and diameter increments (rather than using wood density explicitly)', True, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.RespnTAcclimation, 'Boolean variable to indicate whether to include temperature acclimation for plant respiration', False, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.AgeDecline, 'Boolean variable to indicate whether to include age-related NPP decline', False, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.SizeDecline, 'Boolean variable to indicate whether to include size-related NPP decline', False, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.FoliageClumping, 'Boolean variable to indicate whether to include foliage clumping for plant light interception', False, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.ConstantLeafN, 'Boolean variable to indicate whether to run with constant leaf nitrogen', False, CommentStr, VarStr, ReadWrite, Defp);
End; {of Procedure 'BooleanPlantVariables'}

Procedure RealPlantVariables(ReadWrite: Char; CommentStr, VarStr: String; var defp: Text);

    Procedure ProcessNext(var NextVar: Real48; ProcessString: String; DefaultValue: Real48;
                          CommentStr, VarStr: String; ReadWrite: Char; var Defp: Text);
    Begin
    if ReadWrite = 'R' then
       ReadReal (NextVar, ProcessString, CommentStr, VarStr)
    Else if ReadWrite = 'W' then
       RealOut (NextVar, ProcessString, Defp)
    Else {if ReadWrite = 'D' then}
       NextVar := DefaultValue;
    End;

Begin
ProcessNext (Parameter.kexmax, 'Max. light extinction coefficient', 0.5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.KlowRange, 'Max. decrease of max. light extinction coeff', 0.5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.CanopyWidthInter, 'Intercept in canopy width dbh relationship', 0.5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.CanopyWidthSlope, 'Slope of canopy width dbh relationship', 0.15, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Albedo, 'Leaf albedo', 1, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.SLA, 'Specific leaf area', 10, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.NMVOC, 'Loss rate of non-methyl volatile organic carbon', 0.01, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.BallBerry1, 'Ball-Berry parameter that gives the slope between gs and Ah/c in unstressed plants', 12, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.BallBerry2, 'Ball-Berry parameter that gives the slope between gs and Ah/c in stressed plants', 6, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.MicroFract, 'Fraction of "Active SON" taken up daily through micorrhizal associations', 0.00001, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.BiolFix, 'Amount of nitrogen fixed biologically per unit carbon fixed', 0.001, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.RespFromN, 'Daily respiration rate per unit N', 0.05, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.GrowthRespn, 'Fraction of carbon used up in growth respiration', 0.25, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.RespnAdjust, 'Time constant for adjustments in temperature response of respiration rate', 10, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.SenescLeafRatio, 'Ratio of N in senescing and living leaves', 0.5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.InternalNRatio, 'Ratio of N between average foliage and top foliage layers', 0.5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.DrySenesc, 'Maximum fraction of leaves and branches dying daily during drought', 0.005, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.StressLimit, 'Relative soil water content for the commencement of plant stress', 0.5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.DirectEvapSlope, 'Slope of relationship relating intercepted rain and LAI', 1.0, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.DirectEvapFract, 'Fraction of rain lost to direct evaporation', 0.2, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.BarkSenesc, 'Fraction of bark senescing daily', 0.1 / 365, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.LeafSenesc, 'Fraction of foliage senescing daily', 0.25 / 365, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.BranchSenesc, 'Fraction of branches senescing daily', 0.25 / 365, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.RootSenesc, 'Fraction of roots senescing daily', 0.5 / 365, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.FruitSenesc, 'Fraction of fruit senescing daily', 5 / 365, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.PollenSenesc, 'Fraction of pollen senescing daily (can be greater than 1)', 10 /  365, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.SenescLowLight, 'Low-light level at which foliage senesces', 2, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.MaxSenescLowLight, 'Maximum daily foliage senescence rate due to low light', 0.01, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.RootLeafRatio1, 'Ratio of fine roots to foliage in unstressed plants', 0.5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.RootLeafRatio2, 'Ratio of fine roots to foliage in nutritionally stressed plants', 1.5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.LeafBranchRatio, 'Ratio of foliage to branches', 4, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.WoodBranchRatio, 'Ratio of wood to branches', 5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.CoarseRootWoodRatio, 'Ratio of coarse roots to stem wood', 0.2, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.BarkWoodRatio, 'Ratio of bark to wood', 0.1, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.MinWoodAlloc, 'Minimum amount of carbon allocated to stem-wood production in young trees', 0.1, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.C_FruitAlloc, 'Carbon allocation to fruiting bodies', 0.02, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.C_PollenAlloc, 'Carbon allocation to pollen', 0.02, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Ncrit, 'Nitrogen concentration that is saturating', 0.015, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Nmax, 'Maximum nitrogen concentration to which nitrogen can be taken up by foliage', 0.03, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.N0, 'Nitrogen concentration ''compensation point''', 0.005, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Pcrit, 'Phosphorus concentration that is saturating', 0.005, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Pmax, 'Maximum phosphorus concentration to which phosphorus can be taken up by foliage', 0.01, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.P0, 'Phosphorus concentration ''compensation point''', 0.001, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.KmGrowth[C], 'Km term for the soluble carbohydrate dependence of growth', 0.1, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.KmGrowth[N], 'Km term for the soluble inorganic nitrogen dependence of growth', 0.1, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Nloss, 'Fraction of N volatilised during mineralisation processes', 0.05, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.WoodRetrans, 'ratio of nitrogen concentrations in heartwood and sapwood', 0.5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.bRoots, 'ratio of nitrogen concentrations in roots and foliage', 0.75, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.bWood, 'ratio of nitrogen concentrations in wood and foliage', 0.1, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.bBark, 'ratio of nitrogen concentrations in bark and foliage', 0.4, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.bBranch, 'ratio of nitrogen concentrations in branches and foliage', 0.25, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.bFruit, 'ratio of nitrogen concentrations in fruit and foliage', 0.5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.bPollen, 'ratio of nitrogen concentrations in pollen and foliage', 1, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.WoodLignin, 'Lignin concentration in wood', 0.3, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.LeafLignin, 'Lignin concentration in leaves', 0.3, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.RootLignin, 'Lignin concentration in roots', 0.3, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.LigninInhibition, 'Term that describes the extent by which structural-litter decomposition is inhibited by lignin', 5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Amax, 'Maximum nitrogen use efficiency (Amax / [N])', 30, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.alpha, 'Photosynthetic quantum yield', 0.09, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Theta, 'Theta - curvature term in light dependence of daily carbon gain', 0.9, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.RelkPEP, 'PEP carboxylase activity relative to maximum assimilation rate (for C4 phs)', 0.005, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Beta, 'Curvature term in the transition from CO2 limited to CO2 saturated phs (in C4 phs)', 0.9, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.TMinLim, 'Lower mean daytime temperature limit for plant growth', 0, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.TOpt1, 'Lower mean daytime temperature limit for optimum plant growth', 15, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.TOpt2, 'Upper mean daytime temperature limit for optimum plant growth', 30, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.TMaxLim, 'Upper mean daytime temperature limit for plant growth', 40, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.TFrost, 'Threshold nighttime temperature for leaf damage', 0, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.TScorch, 'Threshold daytime temperature for leaf damage', 40, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.TSensitivity, 'Growth sensitivity to accumulated temperature damage', 0.05, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.TRepair, 'Rate of repair from accumulated temperature damage', 2, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.RainDamageLimit, 'Rain intensity to damage by heavy rain', 200, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.RainDamageSensitivity, 'Sensitivity to damage by heavy rain', 0, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.RainDamageRepair, 'Rate of repair from heavy rain damage', 1, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.HDInter, 'Intercept in the allometric relationship of height versus DBH', 0.6, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.HDSlope, 'Slope in the allometric relationship of height versus DBH', 1.055, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.WDSlope, 'Slope in the allometric relationship of stem weight versus DBH', 1.8, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.WHSlope, 'Slope in the allometric relationship of stem weight versus height', 1.0, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.HD_Const, 'Constant if the allometric relationship of height versus DBH is continually recalculated', 0.9, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.HD_Temp, 'Temperature dependence if the allometric relationship of height versus DBH is continually recalculated', 0.025, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.HD_Stocking, 'Dependence on stocking if the allometric relationship of height versus DBH is continually recalculated', 0.00025, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.HD_Fertil, 'Dependence on nitrogen status if the allometric relationship of height versus DBH is continually recalculated', -0.3, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.HD_Age1, 'Dependence on age (term 1) if the allometric relationship of height versus DBH is continually recalculated', 0, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.HD_Age2, 'Dependence on age (term 2) if the allometric relationship of height versus DBH is continually recalculated', 0.002, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.HD_InitialSlope, 'Initial slope of the height vs DBH allometric relationship up to the defined minimum diameter if the allometric relationship is continually recalculated', 0.52, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.HD_InitialInter, 'Intercept of the height vs DBH allometric relationship up to the defined minimum diameter if the allometric relationship is continually recalculated', 0.52, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.HDSlopeMin, 'Minimum marginal slope of the ht vs dbh allometric relationship', 0.2, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.HDSlopeMax, 'Maximum marginal slope of the ht vs dbh allometric relationship', 2, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Mindbh, 'Minimum dbh at which to apply the allometric relationship', 10, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.WoodDensity, 'Wood density', 0.5, CommentStr, VarStr, ReadWrite, Defp);                // If an old file is loaded - that is then converted to new parameters
ProcessNext (Parameter.WoodDensity0, 'Wood density at age 0', 0.332, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.WoodDensity25, 'Wood density at age 25', 0.47, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.WoodDensTemp, 'Wood density dependence on temperature', 15.9, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.WoodDensFertility, 'Wood density dependence on nutrition', 45, CommentStr, VarStr, ReadWrite, Defp);  // Defaults are valid for P. radiata based on Beets et al. (2007) NZJFS 37: 241-266.
ProcessNext (Parameter.WoodDensStocking, 'Wood density depdendence on stocking', 1, CommentStr, VarStr, ReadWrite, Defp);    // A value of '1' means that the Beets et al. values are used. 'O' means no dependence
ProcessNext (Parameter.CrowdingFactor, 'Scaling term for calculating the effect of stand crowding on stem slenderness', 7.5e-6, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.CrowdingPower, 'Power term for calculating the effect of stand crowding on stem slenderness', 0.5, CommentStr, VarStr, ReadWrite, Defp);    //
ProcessNext (Parameter.CrowdingMax, 'Maximal value for crowding term in calculating the effect of stand crowding on stem slenderness', 0.5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.CrowdingOffset, 'Offset needed to compensate for crowding-related averaged increase in stem slenderness', 0.2, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Transmit, 'Leaf transmissivity', 1, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.TMaxRepairTime, 'Maximum number of days for temperature damage to be fully repaired', 30, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.ExcessNUptake, 'Ratio of total nitrogen uptake into foliage and that utilised in new growth', 1, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.WaterLogLimit, 'Soil water content to start water-log limitation', 0.98, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.WaterLogSensitivity, 'Sensitivity to water-logging', 0, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.LitterWHC, 'Water holding capacity of surface litter', 2.0, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Mulching, 'Mulching effect of surface litter', 10, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.AeroResist, 'Canopy aerodynamic resistance (s m-1)', 25, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Respnalpha, 'Parameter alpha in the T-respn relationship', -3.166, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Respnbeta, 'Parameter beta in the T-respn relationship', 0.001696, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.RespnOpt, 'Temperature for maximum respiration rate', 50, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Three_Two_Power_Law, 'Parameter in the 3/2 power law relationship', exp(15.5), CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.RespnRatio, 'Ratio of respiration rate to daily photosynthetic carbon gain', 0.5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.ConstantLeafNValue, 'Constant leaf [N] (if that is to be kept constant)', 0.5 * (Parameter.N0 + Parameter.NCrit), CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.AgePower, 'Power term in age related decline in NPP', 4.0, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.SizePower, 'Power term in size related decline in NPP', 4.0, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.DefaultPlantDelta, 'Default plant carbon isotope ratio', 24, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.NewDelta, 'Set value for carbon isotope discrimination of newly fixed carbon', 24, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.phi, 'CO2 leakage from bundle-sheet cells (only relevant for C4 plants)', 0.2, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Weed.MaxHeight, 'Max height of weed layer', 2, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Weed.KMHeight, 'Half max height of weed layer at weed foliar biomass', 5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Weed.Senescence, 'Turn-over (senescence) of weed foliage', 1/365, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Weed.AllocLeaves, 'Weed carbon allocation to new foliage growth', 0.25, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Weed.KMRootPlantNUptake, 'KM for plant root required for nitrogen uptake', 250, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Weed.KMRootPlantPUptake, 'KM for plant root required for phosphorus uptake', 1000, CommentStr, VarStr, ReadWrite, Defp);
End; {of Procedure 'RealPlantVariables'}

Procedure ProcessLine (Linein: String; var VarStr, CommentStr: String);

Var CharPos: Integer;
    LastChar: String;
Begin
CharPos := 1;
While (CharPos = 1) do {Remove leading spaces}
      Begin
      CharPos := Pos(' ', LineIn);
      If CharPos = 1 then
         LineIn := Copy(LineIn, CharPos + 1, Length(LineIn) - 1);
      End;
CharPos := Pos(' ', LineIn);
If CharPos <> 0 then
   Begin
   VarStr := Copy(LineIn, 1, CharPos -1);
   CommentStr := Copy(LineIn, CharPos + 1, Length(LineIn));
   CharPos := 1;
   While (CharPos = 1) do {Remove leading spaces at start of comment}
         Begin
         CharPos := Pos(' ', CommentStr);
         If CharPos = 1 then
            CommentStr := Copy(CommentStr, CharPos + 1, Length(CommentStr));
         End;
   LastChar := Copy(CommentStr, Length(CommentStr), 1);
   While LastChar = ' ' do {Remove trailing spaces}
         Begin
         CommentStr := Copy(CommentStr, 1, Length(CommentStr) - 1);
         LastChar := Copy(CommentStr, Length(CommentStr), 1);
         End;
   End
Else
   Begin
   VarStr := LineIn;
   CommentStr := '';
   End;
End; {of Procedure 'ProcessLine'}

Procedure GenPlant(name : string);
Var Defp: text;
    Linein, VarStr, CommentStr: string;
    ErrCode: integer;

        Procedure ReadBool (Var Bool_in: Boolean);
           Var Ch: char;
           Begin
           Ch := VarStr[1];
           IF Ch = 'T' then
              Bool_in := true
           Else
              Bool_in := false;
           End; {of Procedure 'ReadBool'}

        Procedure BoolIn (Var Item: Boolean; Comment: String);
           Begin
           If CommentStr = Comment then
              ReadBool (Item);
           End; {of Procedure 'BoolIn'}

        Procedure RealIn (Var Item: Real48; Comment: String);
           Begin
           If CommentStr = Comment then
              val (VarStr, Item, ErrCode);
           End; {of Procedure 'RealIn'}

        Procedure IntegerIn (Var Item: Integer; Comment: String);
           Begin
           If CommentStr = Comment then
              val (VarStr, Item, ErrCode);
           End; {of Procedure 'IntegerIn'}

        Procedure CharIn (Var Item: Char; Comment: String);
           Begin
           If CommentStr = Comment then
              Item := VarStr[1];
           End; {of Procedure 'CharIn'}

        Procedure PhenologyIn (Comment: String);
           var i: Integer;
           Begin
           If CommentStr = Comment then
              Begin
              IntegerIn (Parameter.Phenology.nChanges, Comment);
              Readln(defp, Parameter.Phenology.Threshold);
              For i := 1 to Parameter.Phenology.nChanges do
                  Begin
                  Readln(defp, Linein);
                  ProcessLine (Linein, VarStr, CommentStr);
                  If VarStr = 'Heatsum' then
                     Parameter.Phenology.Units[i] := HeatSum
                  Else if VarStr = 'Julianday' then
                     Parameter.Phenology.Units[i] := JulianDay
                  Else if VarStr = 'Daylength' then
                     Parameter.Phenology.Units[i] := DayLength
                  Else // If VarStr = 'nDays' then
                     Parameter.Phenology.Units[i] := nDays;
                  Readln (defp, Parameter.Phenology.JulianDay[i]);
                  Readln (defp, Parameter.Phenology.nDays[i]);
                  Readln (defp, Parameter.Phenology.HeatSum[i]);
                  Readln (defp, Parameter.Phenology.Daylength[i]);
                  Readln (defp, Parameter.Phenology.Senescence[i]);
                  Readln (defp, Parameter.Phenology.LeafGrowth[i]);
                  End;
              End;
           End;

Begin
Assign (Defp, Name);
Reset (Defp);
while not eof(defp) do
      Begin
      Readln(defp, Linein);
      ProcessLine (Linein, VarStr, CommentStr);
      BooleanPlantVariables('R', CommentStr, VarStr, defp);
      RealPlantVariables('R', CommentStr, VarStr, defp);
      CharIn (Parameter.DirectEvapType, 'Flag whether direct rain evap is calculated as const or f(LAI)');
      IntegerIn (Parameter.SexAge, 'Minimum age for sexual reproduction');
      IntegerIn (Parameter.SapWoodYears, 'Longevity of sapwood before turning into heartwood');
      If CommentStr = 'Photosynthetic pathway' then
         If VarStr = 'C3:' then
            Parameter.Phs := C3
         Else
            Parameter.Phs := C4;
      If CommentStr = 'Type of respiration function' then
         If VarStr = 'Ratio:' then
            Parameter.RespnType := Ratio
         Else if VarStr = 'Basics:' then
            Parameter.RespnType := Basics;
      IntegerIn (Parameter.MatureAge, 'Age of maturity (at which NPP is only 50% of young-tree NPP');
      IntegerIn (Parameter.MatureSize, 'Size of maturity (at which NPP is only 50% of young-tree NPP');
      If Commentstr = 'Type of delta setting for new carbon' then
         If VarStr = 'SetValue:' then
            Parameter.SetDeltaType := SetValue
         Else if VarStr = 'CalculateValue:' then
            Parameter.SetDeltaType := CalculateValue;
      PhenologyIn ('number of changes in plant phenology');
      end; {of 'while not eof(defp)' statement}
Close (defp);
if Parameter.WoodDensity <> 0 then // we are loading information from an old file
   Begin
   Parameter.WoodDensity0 := 1000 * Parameter.WoodDensity;
   Parameter.WoodDensity25 := 1000 * Parameter.WoodDensity;
   End;
End; {of Procedure 'GenPlant'}

Procedure SavePlant(name : string);
    Var Defp: text;
        i: integer;

    Procedure IntegerOut (Datum: Integer; Comment: string);
       Var StrngVar: String;
       Begin
       Str(Datum, StrngVar);
       Writeln (Defp, StrngVar,' ', Comment);
       End;  {of Procedure 'IntegerOut'}

    Procedure LineOut (Datum: real48; Comment: string);
       var Width, Digits: integer;
       Begin
       GetField(Datum, ParFileMaxWidth, Width, Digits);
       Writeln (Defp, Datum: ParFileMaxWidth: Digits, Comment);
       End;  {of Procedure 'LineOut'}

    Begin
    assign (defp, Name);   rewrite (defp);
    Writeln(defp, Control.Version + ' Plant parameters');
    BooleanPlantVariables('W', 'Dummy', 'Dummy', defp);
    RealPlantVariables('W', 'Dummy', 'Dummy', defp);
    LineOut(Parameter.kexmax, ' Max. light extinction coefficient');
    LineOut(Parameter.KlowRange, ' Max. decrease of max. light extinction coeff');
    LineOut(Parameter.Albedo, ' Leaf albedo');
    LineOut(Parameter.SLA, ' Specific leaf area ');
    LineOut(Parameter.NMVOC, ' Loss rate of non-methyl volatile organic carbon');
    LineOut(Parameter.BallBerry1, ' Ball-Berry parameter that gives the slope between gs and Ah/c in unstressed plants');
    LineOut(Parameter.BallBerry2, ' Ball-Berry parameter that gives the slope between gs and Ah/c in stressed plants');
    Writeln (defp, Parameter.DirectEvapType, '       Flag whether direct rain evap is calculated as const or f(LAI)');
    Writeln(defp, Parameter.SexAge:ParFileMaxWidth, ' Minimum age for sexual reproduction');
    Writeln(defp, Parameter.SapWoodYears:ParFileMaxWidth, ' Longevity of sapwood before turning into heartwood');
    If Parameter.RespnType = Ratio then
       Writeln (defp, 'Ratio:        Type of respiration function')
    Else
       Writeln (defp, 'Basics:       Type of respiration function');
    IntegerOut (Parameter.MatureAge, ' Age of maturity (at which NPP is only 50% of young-tree NPP');
    IntegerOut (Parameter.MatureSize, ' Size of maturity (at which NPP is only 50% of young-tree NPP');
    If Parameter.Phs = C3 then
       Writeln (defp, 'C3:     Photosynthetic pathway')
    Else
       Writeln (defp, 'C4:     Photosynthetic pathway');
    If Parameter.SetDeltaType = SetValue then
       Writeln (defp, 'SetValue:     Type of delta setting for new carbon')
    Else {If VarStr = 'CalculateValue' then}
       Writeln (defp, 'CalculateValue:     Type of delta setting for new carbon');
    IntegerOut (Parameter.Phenology.nChanges, ' number of changes in plant phenology');
    Writeln (defp, Parameter.Phenology.Threshold:ParFileMaxWidth, ' Threshold temperature for counting heat sums');
    For i := 1 to Parameter.Phenology.nChanges do
        Begin
        If Parameter.Phenology.Units[i] = HeatSum then
           Writeln (defp, 'Heatsum   units used for controlling phenological events')
        Else if Parameter.Phenology.Units[i] = JulianDay then
           Writeln (defp, 'Julianday   units used for controlling phenological events')
        Else if Parameter.Phenology.Units[i] = DayLength then
           Writeln (defp, 'Daylength   units used for controlling phenological events')
        Else
           Writeln (defp, 'nDays   units used for controlling phenological events');
           IntegerOut (Parameter.Phenology.JulianDay[i], ' Julian day to change phenology');
           IntegerOut (Parameter.Phenology.nDays[i], ' number of days before changing phenology');
           LineOut (Parameter.Phenology.HeatSum[i], ' heat sum needed before changing phenology');
           LineOut (Parameter.Phenology.Daylength[i], ' daylength needed before changing phenology');
           LineOut (Parameter.Phenology.Senescence[i], ' foliage loss rate');
           LineOut (Parameter.Phenology.LeafGrowth[i], ' foliage growth rate (from buds)');
        End;
    close (defp);
End; {of Procedure 'SavePlant'}

{ --- end of file untLoadSavePlant.PAS ------------------------------------------ }

End.

