{ ================================================================
  = Project   : CENW                                             =
  ================================================================
  = Modules   : Declarations                                     =
  =                                                              =
  =             This is where all structured types and           =
  =             variables are declared.                           =
  ================================================================
  = File      : untDeclarations.PAS                              =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

unit untDeclarations;

interface

const MaxSapWoodYears = 100;
      MaxedtFieldWidth = 6;
      MaxFertiliseEvents = 500;
      MaxPestEvents = 100;
      MaxFireEvents = 100;
      MaxHarvestEvents = 500;
      MaxEnvironmentEvents = 500;
      MaxPloughEvents = 100;
      MaxSoilLayers = 20;
      MaxProjects = 10;
      MaxListRows = 500;
      Kelvin = 273.15;
      GammaSea = 65;
      DayDisplay = 1000;
      MonthDisplay = 3700;
      Latent = 2500000.0;
      rho = 1.204;
      cp = 1010.0;
{     SOMDecay1 = 0.0217140000;  // Decay constant (d-1) for structural surface litter
      SOMDecay2 = 0.0800000000;  // Decay constant (d-1) for metabolic surface litter
      The above are the original CENTURY constants that were lower than corresponding soil
      parameter to account for the less favourable environment, esp. with respect to moisture relations.
      In the latest formulation, moisture relations are explictly calculated for the litter layer
      so that it is no longer warranted to use different parameters for the soil and the surface.}
      SOMDecay1 = 0.0268580000;  // Decay constant (d-1) for structural surface litter
      SOMDecay2 = 0.1000000000;  // Decay constant (d-1) for metabolic surface litter
      SOMDecay3 = 0.0268580000;  // Decay constant (d-1) for structural soil litter
      SOMDecay4 = 0.1000000000;  // Decay constant (d-1) for metabolic soil litter
      SOMDecay5 = 0.0400000000;  // Decay constant (d-1) for active organic matter
      SOMDecay6 = 0.0010860000;  // Decay constant (d-1) for slow organic matter
      SOMDecay7 = 0.0000380000;  // Decay constant (d-1) for resistant organic matter

      { Textlength for numerical input in dialogboxes. }
      num_t_len    = 12;
      String_t_len = 80;
      Info_String_len = 80;
      ParFileMaxWidth = 12;
      MaxListEntries = 10;

type
  ElementsUsed = (C13, C, N, P);
  TElements = array[C13..P] of Real48;
  SoilElements = array[0..MaxSoilLayers] of TElements;
  Infotype = array[0..40] of char;
  FileNameType = String[String_t_len];
  InfoTransferRecord = array[0..Info_String_len-1] of char;
  OrganicFlowType = array[0..MaxSoilLayers] of real48;
  MultipleRunsType = array[0..MaxProjects] of FileNameType;
  ColorOptions = (Green, Red, Black, Blue, Yellow);
  ScreenTypes = (Pixel, Line);
  RespirationTypes = (Basics, Ratio);
  MortalityTypes = (Fraction, Density, Both);
  PhenologyUnitsType = (HeatSum, JulianDay, Daylength, nDays);
  EquilOptions = (SOM, LeafNConc, LeafNitrogen, LeafMass, WoodMass);
  EquilParameterOptions = (BiolNFix, NFraction);
  PhotosynthesisTypes = (C3, C4);
  SetDeltaOptions = (SetValue, CalculateValue);

AllOptions = (Sapwood, HeartWood, CoarseRoot, HeartRoot, FineRoot, Branches,
              Leaves1, Leaves2, Leaves3, Pollen, Fruit,
              CoarseWood, FineWood, CoarseRootLitter, FineRootlitter, Leaves, Other,
              StructSurf, StructSoil, MetabSurf, MetabSoil, Slow, Active, Resistant, Soluble,
              Tmax, Tmin, TSoil, PAR, Humidity, StoredWater, WaterLimit, CO2Conc, Rain);

DerivedEquilType = Record
                   SearchValue, Converg1, Converg2, Converg3: Real48;
                   GoodCount, Iterations: Integer;
                   SolutionFound, ManualAdjust: Boolean;
                   End;

DerivedType = Record
              LAI, LightAbs, LightInter, CarbonGain, CAI, NPP, NConc, WaterLimit, p_internal, IrrigateWater,
              NLimit, Rm, Rg, TDamageUnits, TDamage, BallBerry, NBiol,
              PestLeafDamage, PestSolubleDamage, PestSenesc, PestPhsFrac, PestMortality, PestDeathRatio,
              gs, Desic, Transpiration, DroughtMort, nMortality, CLossMortality, MeanNLimit, RespnBase,
              Deciduous, LeafGrowth, MaxPlantNUptake, Functionf1, Functionf2,
              SoilRespn, NUptake, NMineralised, DecompLimit, NEE, DayCFlux, NightCFlux, Evaporation, NLeached,
              HeatSum, ExcessN, All_Litter, SoilTRadnEffect, HeteroRespn,
              C_ActFruitAlloc, C_ActPollenAlloc, N_FruitAlloc, N_PollenAlloc,
              C_SapWoodAlloc, C_BranchAlloc, C_BarkAlloc, C_RootAlloc, C_FineRootAlloc, C_CoarseRootAlloc, C_LeafAlloc,
              N_WoodAlloc, N_SapWoodAlloc, N_BranchAlloc, N_RootAlloc, N_FineRootAlloc, N_BarkAlloc,
              N_CoarseRootAlloc, N_LeafAlloc, Dummy: real48;
              Drainage, DecompWatLimit: array[0..MaxSoilLayers] of real48;
              Equil: DerivedEquilType;
              End;

ListType = Record
           nEntries, nRows, Header, StrOptions, nRadios, HelpContext, MaxRows: Integer;
           TextBoxEntry: Real48;
           Caption, FileExt, FileComment, TextBoxCaption: String;
           RadioOptions: array[1..2] of Integer;
           RadioHeading: array[1..2] of String;
           RadioText: array[1..2, 1..3] of String;
           RbtnSelected: array[1..2] of Integer;
           StringConst: array[1..5] of String;
           Width: array[0..MaxListEntries] of Integer;
           Text: array[1..3, 0..MaxListEntries] of String;
           DataType: array[0..MaxListEntries] of (S, R, I);
           Data: array[1..MaxListRows, 0..MaxListEntries] of Real48;
           HasChanged, ShowMessage, TextBox, Redraw, RedrawOption: Boolean;
           Message: String;
           End;

EquilibriumSetup = Record
                   Maxiterations, MaxGoodCount: Integer;
                   Criterion1, Criterion2, Criterion3, TargetValue, DeltaMin, DeltaMax,
                   MaxChangeRatio, DeltaAdjust, BoostResistant: Real48;
                   EquilTarget: EquilOptions;
                   EquilParameter: EquilParameterOptions;
                   SamePlantPools: Boolean;
                   End;

PhenologyType = Record
                nChanges: Integer;
                Threshold: Real48;
                Units: array[1..365] of PhenologyUnitsType;
                JulianDay, nDays: array[1..365] of Integer;
                HeatSum, DayLength, Senescence, LeafGrowth: array[1..365] of real48;
                End;

Littertype = record
             CoarseWood, FineWood, CoarseRoot, FineRoot, Leaves, Other: TElements;
             end;

ParameterType = Record
        FineSoil, MeanSoilTemp, CO2Conc, AtmosPressure, gamma, alpha,
        AnnualRain, RainProb, MeanTmax, MeanTmin, MeanRadn, MeanAbsHum, Latitude, Longitude,
        FertiliserRelease, FertilityAdjust, RateAdjust, Immobilise, LitterWHC, DirectEvapSlope,
        DirectEvapFract, CriticalCN, Decay8, Decay9, Decay10, DefaultSoilDelta, ImmobiliseInSlow,
        DecayBranch_StructRatio, DecayWood_StructRatio, Inert_Resistant_Ratio,
        RelWaterSens, RelativeCN, OMTransfer, OMIncorporate, Mulching, SoilEvap, MinDecomp,
        Nloss, Leaching, Atmos_N, Temp_Amplitude, Radn_Amplitude, Daily_Amplitude, Humid_Amplitude,
        Snowmelt, RadnMelt, SoilTResist, SnowInsulate, Respnalpha, Respnbeta, RespnOpt,
        RespFromN, SenescLeafRatio, InternalNRatio, LeafSenesc,
        BranchSenesc, RootSenesc, FruitSenesc, PollenSenesc, SenescLowLight, MaxSenescLowLight,
        RootLeafRatio1, RootLeafRatio2, Form, MinWoodAlloc, WoodDensity, ExcessNUptake,
        WoodBranchRatio, RootLeafRatio, LeafBranchRatio, BarkWoodRatio, CoarseRootWoodRatio,
        C_FruitAlloc, C_PollenAlloc,
        SLA, Amax, Theta, Kexmax, KlowRange, ConstantLeafNValue, Albedo, AeroResist,
        DrySenesc, DeathLimit, BiolFix, GrowthRespn, BarkSenesc, MicroFract,
        DryDeath, StressLimit, HDInter, HDSlope, WDSlope, WHSlope, Mindbh, Ht_Diameter,
        TMinLim, TOpt1, TOpt2, TmaxLim, TFrost, TScorch, TSensitivity, TRepair, NMVOC,
        WoodRetrans, bRoots, bWood, bBark, bBranch, bFruit, bPollen,
        N0, Ncrit, Nmax, AgePower, SizePower, NewDelta, ExtraDelta, phi,
        StemDeath, DeathRatio, Three_Two_Power_Law, RespnAdjust,
        WoodLignin, LeafLignin, RootLignin, LigninInhibition, TMaxRepairTime, RespnRatio,
        Transmit, BallBerry1, BallBerry2, DefaultPlantDelta, MaxTBoost, TLAISensitivity,
        relkPEP, Beta: real48;
        Phenology: PhenologyType;
        KmGrowth: TElements;
        WarmestDay, MostPAR: Integer;
        SexAge, SapWoodYears, MatureAge, MatureSize: Integer;
        DirectEvapType: Char;
        SetDeltaType: SetDeltaOptions;
        MortalityType: MortalityTypes;
        RespnType: RespirationTypes;
        Phs: PhotosynthesisTypes;
        AgeDecline, SizeDecline, FoliageClumping, ConstantLeafN, RespnTAcclimation, VariableNFixation: Boolean;
        end;

Planttype = Record
            Sapwood, HeartWood, CoarseRoot, FineRoot, Leaves, Reserves,
            Branches, Bark, Pollen, Fruit, Soluble, NewGrowth: TElements;
            Stocking, Height, Area, DBH, CanopyCover, kex: real48;
            Age: integer;
            end;

InitialType = Record
            Sapwood, HeartWood, CoarseRoot, FineRoot, Leaves, Reserves,
            Branches, Bark, Pollen, Fruit, Soluble: TElements;
            Stocking, Height, DBH: real48;
            Age, Days, Months, Years: integer;
            end;

SaveVariableOptions = (S_Year, S_Month, S_Day,
                       S_CH2O, S_SapWoodC, S_HeartWoodC, S_LeafC, S_FineRootC, S_BarkC,
                       S_CoarseRootC, S_BranchesC, S_ReprodC, S_Reserves,
                       S_SolubleN, S_SapWoodN, S_HeartWoodN, S_LeafN, S_FineRootN, S_BarkN,
                       S_CoarseRootN, S_BranchesN, S_ReprodN, S_ReservesN, S_TotalN,
                       S_LAI, S_Height, S_DBH, S_CanopyCover, S_kex, S_BasalArea, S_Stocking,
                       S_pi, S_CAI, S_NPP, S_NEE, S_CarbonGain, S_Respn, S_DayCFlux, S_NightCFlux,
                       S_TDamage, S_Transpiration, S_NConc,
                       S_CMetabolic, S_NMetabolic, S_CStructural, S_NStructural,
                       S_CWoodyLitter, S_NWoodyLitter, S_CActive, S_NActive,
                       S_CSlow, S_NSlow, S_CResistant, S_NResistant, S_CInert, S_NInert,
                       S_CLitterFall, S_NLitterFall, S_CAgLitterFall, S_NAgLitterFall, S_CLeafLitterFall, S_NLeafLitterFall,
                       S_NMineral, S_NLeached, S_HeteroRespn, S_SoilRespn, S_Tmax, S_Tmin, S_Tmean, S_Tsoil, S_Tday,
                       S_AbsHum, S_RelHum, S_PAR, S_Rain, S_StoredWater, S_WaterLimit, S_RelDecomp, S_Snow,
                       S_Drainage, S_HeatSum, S_Evaporation, S_Dummy);

ScreenOptions = (D_CarbonGain, D_CAI, D_NPP, D_NEE, D_Respn, D_DayCFlux, D_NightCFlux,
                 D_SolubleCH2O, D_LAI, D_pi, D_Wood, D_SapW, D_HeartW,
                 D_Reserves, D_Leaf, D_FineRoot, D_Bark, D_CoarseRoot, D_Branches, D_Reprod,
                 D_Height, D_DBH, D_CanopyCover, D_kex, D_BasalArea, D_Stocking,
                 D_TDamage, D_NConc, D_NConc1, D_SolubleN, D_WoodN, D_SapWN, D_HeartWN,
                 D_ReservesN, D_LeafN, D_FineRootN, D_BarkN,
                 D_CoarseRootN, D_BranchN, D_ReprodN, D_NSum, D_PConc, D_Puptake, D_PSum,
                 D_CLeafLitter, D_CAll_Litter, D_CAg_Litter, D_CMetabSurf, D_CMetabSoil, D_CStructSurf,
                 D_CStructSoil, D_CWoodyLitter, D_CActive, D_CSlow, D_CResistant, D_SoilRespn,
                 D_NLeafLitter, D_NAll_Litter, D_NAg_Litter, D_NMetabSurf, D_NMetabSoil, D_NStructSurf,
                 D_NStructSoil, D_NWoodyLitter, D_NActive, D_NSlow, D_NResistant, D_NMineral, D_NLeached, D_NSoilRespn,
                 D_PLeafLitter, D_PAll_Litter, D_PAg_Litter, D_PMetabSurf, D_PMetabSoil, D_PStructSurf,
                 D_PStructSoil, D_PWoodyLitter, D_PActive, D_PSlow, D_PResistant, D_PMineral, D_PSoilRespn,
                 D_Tmax, D_Tmin, D_Tmean, D_Tsoil, D_Tday, D_PAR, D_CO2, D_Rain, D_StoredWater,
                 D_AbsHum, D_RelHum, D_WaterLimit, D_Transpiration, D_Evaporation,
                 D_Drainage, D_HeatSum, D_Snow, D_Dummy, D_Equil, D_Equil2, D_Equil3);

ScreenRecord = Record
               Choose: array [ScreenOptions] of Boolean;
               UpRange, LowRange: array [ScreenOptions] of Real48;
               Color: array [ScreenOptions] of Longint;
               End;

SensitivityType = (Dummy, FineSoil, MeanSoilTemp, CO2, AtmosPressure, WoodLignin, LeafLignin,
      RootLignin, AnnualRain, RainProb, MeanTmax, MeanTmin, MeanRadn, MeanAbsHum, Latitude,
      FertiliserRelease, FertilityAdjust, RateAdjust, TMaxRepairTime, Immobilise, DirectEvapSlope, DirectEvapFract,
      Decay1, Decay2, Decay3, Decay4, Decay5, Decay6, Decay7, Decay8, Decay9, RelativeCN, CriticalCN, Transmit, BallBerry1,
      BallBerry2, Nloss, Leaching, MicroFract, Atmos_N, BiolFix, GrowthRespn, BarkSenesc,
      RespFromN, StemDeath, SenescLeafRatio, InternalNRatio, LeafSenesc, BranchSenesc, RootSenesc,
      FruitSenesc, PollenSenesc, SenescLowLight, MaxSenescLowLight, RootLeafRatio1, RootLeafRatio2,
      WoodBranchRatio, LeafBranchRatio, BarkWoodRatio, CoarseRootWoodRatio, C_FruitAlloc,
      C_PollenAlloc, SLA, Amax, Theta, Kexmax, KlowRange, Albedo, Temp_Amplitude, Radn_Amplitude, Daily_Amplitude,
      Humid_Amplitude, DrySenesc, StressLimit, SoilEvap, HDInter, HDSlope, WDSlope,
      WHSlope, TMinLim, TOpt1, TOpt2, TmaxLim, TFrost, TScorch, TSensitivity, TRepair, WoodRetrans,
      bRoots, bWood, bBark, bBranch, bFruit, bPollen, RelWaterSens, N0, Ncrit, Nmax, SapwoodC, HeartWoodC,
      CoarseRootC, FineRootC, BranchesC, BarkC, LeavesC, SapwoodN, HeartWoodN, CoarseRootN,
      FineRootN, BranchesN, BarkN, LeavesN, Stocking, FineWoodSurfC, CoarseWoodSurfC, CoarseWoodSoilC,
      StructSurfC, StructSoilC, MetabSurfC, MetabSoilC, SlowC, ActiveC, ResistantC, FineWoodSurfN,
      CoarseWoodSurfN, CoarseWoodSoilN, StructSurfN, StructSoilN, MetabSurfN, MetabSoilN, SlowN,
      ActiveN, ResistantN, EndInputs, CAI, NConc, LAI, Wood, ShowFineRootC, ShowFineRootN,
      ShowBranches, ShowLeaves, ShowLeafN, Height, DBH, CanopyCover, kex, ShowFineWoodSurfC, ShowCoarseWoodSurfC,
      ShowCoarseWoodSoilC, ShowStructSurfC, ShowMetabSurfC, ShowStructSurfN, ShowMetabSurfN,
      ShowSlowC, ShowActiveC, ShowResistantC, ShowSlowN, ShowActiveN, ShowResistantN, EndDummy);

Soilorgtype = record
              nLayers: Integer;
              FineWood, CoarseWood, Struct, Metab, Slow, Active, Resistant, Inert, Soluble, Mycorrhizal: SoilElements;
              LitterLig, BranchLig, StemLig, FineRootLitterIn, CoarseRootLitterIn: OrganicFlowType;
              end;

SoilWaterCategories = Record
                      Depth, Pores, MaxWater, ExtractEffort, StressSensitivity, RelEvap, WaterContent, Percolate: Real48;
                      End;

SoilWaterType = Record
                TotalWater, MaxWater, MaxExtract, Snow: real48;
                SeparateSensitivity: Boolean;
                nLayers: integer;
                Layer: array[0..MaxSoilLayers] of SoilWaterCategories;
                End;

WeatherType = Record
              TMax, TMin, TMean, TSoil, TDay, TNight, PAR, Rain, AbsHumidity,
              RelHumidity, Evaporation, CO2, ExtraCO2, ExtraTemp, ExtraRain, RainProb,
              LastMin, LastAbsHum, LastRelHum: real48;
              End;

WeatherFileOptions = (W_Tmax, W_Tmin, W_Tmean, W_Tsoil, W_PAR, W_Rain,
                      W_AbsHum, W_RelHum, W_CO2);

ControlType = Record
              Year, DisplayInterval, NextHarvest, NextEnvironment, NextPest, NextFire, NextPlough, BatchCalcs,
              nYears, nMonths, nDays, JulianDay, BatchCount, nProjects, NextPhenology,
              TotalYears, ExtraMonths, ExtraDays, Count, NextFertilise, SensFlag, nCohorts: integer;
              CConversion, NConversion, OldPar: real48;
              MaxDays, DaysSince, TotalDays, LastIrrigation, nSeconds, PhenologyDayCount,
              DaysSinceDisk, DiskInterval, nDiskOut, nDisplays: LongInt;
              CalcType, ClimType,
              Fertilise_DateType, Harvest_DateType, Pest_DateType, Fire_DateType, Plough_DateType: char;
              DecayOnly, StartRun, NewYear, Run_on, ClimFileOpen, Range, CheckProject,
              SaveProject, IrrigAnnounced, LegendOn, OutputFileOpen, AllOneLayer, ErrorSaving,
              ProjectHasChanged, PlantHasChanged, SiteHasChanged, InitHasChanged, IncludeIsotopes,
              DataToBeSaved, SensitivityTestOn, SensFileOpen, InitGenerated, BatchMode, TSoilFound,
              ScreenHasChanged, CheckSaveInit, AgreeOK, AgreeChecked, EndBatch, OutputByLayer,
              EquilMode, EndEquil, SaveBeforeRun, AbortRun, PestMode, ResetPlantPools, RunWithDefaults: Boolean;
              CenWFileOut, DefClim, ClimChngFile, SensFile, BatchName: TextFile;
              FileOut, ClimFile, ProjectFile, SiteFile, PoolFile, SavePoolFile, BatchFile, MultipleRunPoolFile: FileNameType;
              PlantFile: FileNameType;
              ProjectName, Date: InfoTransferRecord;
              MultipleRuns: MultipleRunsType;
              Initial: InitialType;
              OldSOilPar: array [0..MaxSoilLayers] of real48;
              Version: string[5];
              OldDirectory, ProjectDirectory, Display: string;
              SensParameter: SensitivityType;
              DisplayType: ScreenTypes;
              Equil: EquilibriumSetup;
              End;

const
  SaveVariableNames: array[SaveVariableOptions] of string
                    = ('Year','Month','Day',
                       'CH2O', 'Sapwood C','Heartwood C','Foliage C','Fineroot C', 'Bark C',
                       'Coarseroot C', 'Branch C','Reproductive C','Reserves C',
                       'Soluble N','Sapwood N','Heartwood N','Foliage N','Fineroot N', 'Bark N',
                       'Coarseroot N','Branch N','Reproductive N','Reserves N','Total N',
                       'LAI','Height','DBH','Canopy cover','Extinction coeff.','Basal area', 'Stocking',
                       'pi','CAI','NPP','NEE','Carbon gain','Autotrophic respn','Day C Flux', 'Night C Flux',
                       'Temp. damage','Transpiration','N Concentration',
                       'C Metabolic','N Metabolic', 'C Structural','N Structural',
                       'C Woody litter','N Woody litter','C Active','N Active',
                       'C Slow','N Slow','C Resistant','N Resistant','C Inert','N Inert',
                       'C All litter prod','N All litter prod', 'C A-g Litterfall', 'N A-g Litterfall', 'C Leaf litter fall','N Leaf litter fall',
                       'N Mineral', 'N Leached', 'Heterotrophic respn', 'Soil Respiration',
                       'Tmax','Tmin','Tmean','Tsoil','Tday',
                       'Absolute Humidity','Relative Humidity','PAR','Rain','Stored water','WaterLimit','Rel. decomposition','Snow',
                       'Drainage','Heat sum','Evaporation','Dummy');

  ScreenVariableNames: array[ScreenOptions] of string
                    = ('Carbon gain', 'CAI', 'NPP', 'NEE', 'Respn', 'Day C Flux', 'Night C Flux',
                       'SolubleCH2O', 'LAI', 'pi', 'Wood', 'SapW', 'HeartW',
                       'Reserves', 'Leaf', 'FineRoot', 'Bark', 'CoarseRoot', 'Branches', 'Reprod',
                       'Height', 'DBH', 'CanopyCover', 'kex', 'BasalArea', 'Stocking',
                       'TDamage', 'NConc', 'NConc (top)', 'SolubleN', 'WoodN', 'SapWN', 'HeartWN',
                       'ReservesN', 'LeafN', 'FineRootN', 'Bark N',
                       'CoarseRootN', 'BranchN', 'ReprodN', 'NSum', 'PConc', 'Puptake', 'PSum',
                       'CLeafLitter', 'CAll_Litter', 'CAg_Litter', 'CMetabSurf', 'CMetabSoil', 'CStructSurf',
                       'CStructSoil', 'CWoodyLitter', 'CActive', 'CSlow', 'CResistant', 'SoilRespn',
                       'NLeafLitter', 'NAll_Litter', 'NAg_Litter', 'NMetabSurf', 'NMetabSoil', 'NStructSurf',
                       'NStructSoil', 'NWoodyLitter', 'NActive', 'NSlow', 'NResistant', 'NMineral', 'NLeached', 'NSoilRespn',
                       'PLeafLitter', 'PAll_Litter', 'PAg_Litter', 'PMetabSurf', 'PMetabSoil', 'PStructSurf', 'PStructSoil',
                       'PWoodyLitter', 'PActive', 'PSlow', 'PResistant', 'PMineral', 'PSoilRespn',
                       'Tmax', 'Tmin', 'Tmean', 'Tsoil', 'Tday', 'PAR', 'CO2', 'Rain', 'StoredWater',
                       'AbsHum', 'RelHum', 'WaterLimit', 'Transpiration', 'Evaporation',
                       'Drainage', 'Heat sum', 'Snow', 'Dummy', 'Equil run', 'Slow', 'Resistant');

type
SaveVariableRecord = Record
       Choose: array [SaveVariableOptions] of Boolean;
       End;

SetSensRecord = Record
       Test: Real48;
       Choose: array [SensitivityType] of Boolean;
       End;

EventType = Record
             nFertilisations: integer;
             FertiliseTimes: array[0..MaxFertiliseEvents, 1..4] of LongInt;
             FertiliseAmount: array[0..MaxFertiliseEvents] of real48;
             FertiliserAdded: real48;
             IrrigationInterval, DaysSinceIrrigation, IrrigStartDay, IrrigStartMonth,
             IrrigStartYear, IrrigEndDay, IrrigEndMonth, IrrigEndYear: integer;
             IrrigationAmount, IrrigationFraction: real48;
             Irrigate: Boolean;
             IrrigationType: char;
             HarvestUnits: Char;
             nHarvests: integer;
             HarvestTimes: array[1..MaxHarvestEvents, 1..4] of LongInt;
             WoodCut, RelativeSize, BranchesCut, WoodRemoval, FineRemoval:
                           array[1..MaxHarvestEvents] of real48;
             AdjustStocking: array[1..MaxHarvestEvents] of Boolean;
             nEnvironments: integer;
             EnvironmentTimes: array[1..MaxEnvironmentEvents] of LongInt;
             CO2, Temperature, Rainfall: array[1..MaxEnvironmentEvents] of real48;
             nPests: Integer;
             PestTimes: array[1..MaxPestEvents, 1..5] of LongInt;
             PestDamageUnits: char;
             LeafDamage, SolubleDamage, SenescenceDamage, PhotosynthesisFraction,
             PestMortality, PestDeathRatio: array[1..MaxPestEvents] of real48;
             nFires: Integer;
             FireTimes: array[1..MaxFireEvents, 1..4] of LongInt;
             LeafBurn, WoodBurn, LitterBurn, WoodToChar, FineToChar, LeafBurnSenesc,
             WoodBurnSenesc, Burn_N_CRatio: array[1..MaxFireEvents] of real48;
             nPloughing: Integer;
             PloughTimes: array[1..MaxPloughEvents, 1..4] of LongInt;
             PloughDepth: array[1..MaxPloughEvents] of Integer;
             End;

ScreenHistory = Record
                First: Boolean;
                Lastx, Lasty: integer;
                End;

var
  Derived  : DerivedType;
  Parameter: Parametertype;
  Control  : ControlType;
  OldScreen: array[ScreenOptions] of ScreenHistory;
  ScreenRec: ScreenRecord;
  SaveVar: SaveVariableRecord;
  TestSens: SetSensRecord;
  WeatherFile: array[WeatherFileOptions] of Boolean;
  MaxWidth, DecompCount: integer;
  List: ListType;
  LastYearsWood, LastYearsDecomp, NPP: array[0..365] of real48;
  SapWoodAmount: array[0..MaxSapWoodYears] of real48;
  Plant: Planttype;
  Litter: Littertype;
  Soilorganic: soilorgtype;
  Weather: WeatherType;
  Event: EventType;
  SoilWat: SoilWaterType;

implementation

end.
