{ ================================================================
  = Project   : Cenw                                             =
  ================================================================
  = Module    : ProcessFittingData                               =
  =                                                              =
  ================================================================
  = File      : untProcessFittingData.PAS                        =
  =                                                              =
  = Version   : 4.1                                              =
  ================================================================ }

Unit untProcessFittingData;

Interface

Uses untDeclarations, SysUtils, untFieldValidation, untMiscellaneous, untRun, untPowerValidation,
     untDivideValidation, untRunOptions, untLoadSaveProject, untProgress, untFileIO;

Procedure SetUpFitParameters (Index: Integer);
Procedure ProcessFittingData;
Procedure AddToSumOfSquares;
Procedure GetDerivatives;
Procedure FormulateMatrix;

Implementation

Procedure SetUpFitParameters (Index: Integer);
var SetFitVar: FittingType;

    Procedure SetParameters (Index: Integer; var ChangeVar: Real48; LowLimit, UpLimit, Multiplier: Real48);

        Procedure HeadingOut (Heading: string);
        Begin
        If Length(Heading) > MaxWidth then
           Heading := Copy(Heading, 1, MaxWidth);
        Write (Control.CenwFileOut, Heading: MaxWidth, ' ');
        End;  {of Procedure 'HeadingOut'}

    Begin  //Codes
    // -2: Set variable back to the last one before it had been changed
    // -1: Set variable to the next one to be changed
    // 0:  Set variable back to initial values
    // 1:  Initial parse to set 'Initial' to variable
    // 2:  Process data when it comes back from parameter setting by the user
    if Index = 1 then
       Begin
       HeadingOut (FittingNames[SetFitVar]);
       FitParameter.Initial[SetFitVar] := ChangeVar;
       FitParameter.Varied[SetFitVar] := ChangeVar;
       FitParameter.Best[SetFitVar] := ChangeVar;
       FitParameter.Multiplier[SetFitVar] := Multiplier;
       End
    Else if Index = 2 then
       Begin
       ChangeVar := FitParameter.Initial[SetFitVar];
       FitParameter.Varied[SetFitVar] := ChangeVar;
       FitParameter.Last[SetFitVar] := ChangeVar;
       if FitParameter.Min[SetFitVar] < LowLimit then
          FitParameter.Min[SetFitVar] := LowLimit;
       if FitParameter.Max[SetFitVar] > UpLimit then
          FitParameter.Max[SetFitVar] := UpLimit;
       if FitParameter.Max[SetFitVar] < FitParameter.Initial[SetFitVar] then
          FitParameter.Max[SetFitVar] := FitParameter.Initial[SetFitVar];
       if FitParameter.Min[SetFitVar] > FitParameter.Initial[SetFitVar] then
          FitParameter.Min[SetFitVar] := FitParameter.Initial[SetFitVar];
       End
    Else if Index = -1 then
       Begin
       ChangeVar := FitParameter.Varied[SetFitVar];
       End
    Else if Index = -2 then
       Begin
       ChangeVar := FitParameter.Last[SetFitVar];
       FitParameter.Varied[SetFitVar] := FitParameter.Last[SetFitVar];
       End
    Else // if Index = 0 then
       Begin
       ChangeVar := FitParameter.Initial[SetFitVar];
       if Control.OutputFileOpen then
          Begin
          Close (Control.CenWFileOut);
          Control.OutputFileOpen := false;
          End;
       End;
    End; {of Procedure 'SetParameters'}

Begin
If Index = 1 then
   Begin
   if Control.OutputFileOpen then
      Close (Control.CenWFileOut);
   assign (Control.CenwFileOut, Control.FileOut);
   rewrite (Control.CenwFileOut);
   Control.OutputFileOpen := true;
   Write (Control.CenwFileOut, 'Sum Squ. ');
   End;
For SetFitVar := Succ(F_Dummy) to Pred(F_EndDummy) do
    if FitParameter.Choose[SetFitVar] then
       Case SetFitVar of
            F_FineSoil: SetParameters (Index, Parameter.FineSoil, 0, 1, 100);
            F_WoodLignin: SetParameters (Index, Parameter.WoodLignin, 0, 1, 100);
            F_LeafLignin: SetParameters (Index, Parameter.LeafLignin, 0, 1, 100);
            F_RootLignin: SetParameters (Index, Parameter.RootLignin, 0, 1, 100);
            F_FertilityAdjust: SetParameters (Index, Parameter.FertilityAdjust, 0, 100, 1);
            F_RateAdjust: SetParameters (Index, Parameter.RateAdjust, 0, 100, 1);
            F_TMaxRepairTime: SetParameters (Index, Parameter.TMaxRepairTime, 0, 10000, 1);
            F_Immobilise: SetParameters (Index, Parameter.Immobilise, 0, 1, 1000);
            F_DirectEvapSlope: SetParameters (Index, Parameter.DirectEvapSlope, 0 , 10000, 1);
            F_DirectEvapFract: SetParameters (Index, Parameter.DirectEvapFract, 0, 1, 100);
            F_RelativeCN: SetParameters (Index, Parameter.RelativeCN, 0, 10000, 1);
            F_CriticalCN: SetParameters (Index, Parameter.CriticalCN, 0, 100, 1);
            F_Transmit: SetParameters (Index, Parameter.Transmit, 0, 1, 100);
            F_BallBerry1: SetParameters (Index, Parameter.BallBerry1, 0, 10000, 1);
            F_BallBerry2: SetParameters (Index, Parameter.BallBerry2, 0, 10000, 1);
            F_Nloss: SetParameters (Index, Parameter.Nloss, 0, 1, 100);
            F_Leaching: SetParameters (Index, Parameter.Leaching, 0, 1, 100);
            F_MicroFract: SetParameters (Index, Parameter.MicroFract, 0, 1, 1000);
            F_Atmos_N: SetParameters (Index, Parameter.Atmos_N, 0, 10000, 365);
            F_BiolFix: SetParameters (Index, Parameter.BiolFix, 0, 10000, 365);
            F_GrowthRespn: SetParameters (Index, Parameter.GrowthRespn, 0, 10, 1);
            F_BarkSenesc: SetParameters (Index, Parameter.BarkSenesc, 0, 10, 365);
            F_RespFromN: SetParameters (Index, Parameter.RespFromN, 0, 10000, 1);
            F_SelfThinning: SetParameters (Index, Parameter.Three_Two_Power_Law, 14, 20, 1);
            F_StemDeath: SetParameters (Index, Parameter.StemDeath, 0, 1, 100);
            F_SenescLeafRatio: SetParameters (Index, Parameter.SenescLeafRatio, 0, 1, 1);
            F_InternalNRatio: SetParameters (Index, Parameter.InternalNRatio, 0, 1, 1);
            F_LeafSenesc: SetParameters (Index, Parameter.LeafSenesc, 0, 1, 365);
            F_BranchSenesc: SetParameters (Index, Parameter.BranchSenesc, 0, 1, 365);
            F_RootSenesc: SetParameters (Index, Parameter.RootSenesc, 0, 1, 365);
            F_FruitSenesc: SetParameters (Index, Parameter.FruitSenesc, 0, 1, 365);
            F_PollenSenesc: SetParameters (Index, Parameter.PollenSenesc, 0, 1, 365);
            F_SenescLowLight: SetParameters (Index, Parameter.SenescLowLight, 0, 1, 1);
            F_MaxSenescLowLight: SetParameters (Index, Parameter.MaxSenescLowLight, 0, 1, 100);
            F_RootLeafRatio1: SetParameters (Index, Parameter.RootLeafRatio1, 0, 1000, 1);
            F_RootLeafRatio2: SetParameters (Index, Parameter.RootLeafRatio2, 0, 1000, 1);
            F_WoodBranchRatio: SetParameters (Index, Parameter.WoodBranchRatio, 0, 1000, 1);
            F_LeafBranchRatio: SetParameters (Index, Parameter.LeafBranchRatio, 0, 1000, 1);
            F_BarkWoodRatio: SetParameters (Index, Parameter.BarkWoodRatio, 0, 1000, 1);
            F_CoarseRootWoodRatio: SetParameters (Index, Parameter.CoarseRootWoodRatio, 0, 1000, 1);
            F_C_FruitAlloc: SetParameters (Index, Parameter.C_FruitAlloc, 0, 1, 100);
            F_C_PollenAlloc: SetParameters (Index, Parameter.C_PollenAlloc, 0, 1, 100);
            F_SLA: SetParameters (Index, Parameter.SLA, 0, 1000, 1);
            F_Amax: SetParameters (Index, Parameter.Amax, 0, 1000, 1);
            F_Theta: SetParameters (Index, Parameter.Theta, 0, 1, 1);
            F_Kexmax: SetParameters (Index, Parameter.Kexmax, 0, 100, 1);
            F_KlowRange: SetParameters (Index, Parameter.KlowRAnge, 0, 100, 1);
            F_Albedo: SetParameters (Index, Parameter.Albedo, 0, 1, 100);
            F_DrySenesc: SetParameters (Index, Parameter.DrySenesc, 0, 1, 100);
            F_StressLimit: SetParameters (Index, Parameter.StressLimit, 0, 1, 1);
            F_SoilEvap: SetParameters (Index, Parameter.SoilEvap, 0, 1000, 1);
            F_HDInter: SetParameters (Index, Parameter.HDInter, -1000, 1000, 1);
            F_HDSlope: SetParameters (Index, Parameter.HDSlope, 0, 10, 1);
            F_WDSlope: SetParameters (Index, Parameter.WDSlope, -1000, 1000, 1);
            F_WHSlope: SetParameters (Index, Parameter.WHSlope, 0, 10, 1);
            F_WeedInitC: SetParameters (Index, Control.Initial.WeedLeaves[C], 0, 10000, 1);
            F_WeedInitN: SetParameters (Index, Control.Initial.WeedLeaves[N], 0, 500, 1);
            F_WeedMaxHeight: SetParameters (Index, Parameter.Weed.MaxHeight, 0, 100, 1);
            F_WeedKMHeight: SetParameters (Index, Parameter.Weed.KMHeight, 0, 1e6, 1);
            F_WeedFoliageTurnover: SetParameters (Index, Parameter.Weed.Senescence, 0, 1, 100);
            F_WeedFoliageAllocation: SetParameters (Index, Parameter.Weed.AllocLeaves, 0, 1, 1);
            F_WeedKmTreeRoots: SetParameters (Index, Parameter.Weed.KMRootPlantNUptake, 0, 1e6, 1);
            F_WeedKmTreeRootsP: SetParameters (Index, Parameter.Weed.KMRootPlantPUptake, 0, 1e6, 1);
            F_WoodDensPith: SetParameters (Index, Parameter.WoodDensity0, 0, 1e6, 1);
            F_WoodDensOuter: SetParameters (Index, Parameter.WoodDensity25, 0, 1e6, 1);
            F_WoodDensTemperature: SetParameters (Index, Parameter.WoodDensTemp, -1e6, 1e6, 1);
            F_WoodDensFertility: SetParameters (Index, Parameter.WoodDensFertility, -1e6, 1e6, 1);
            F_WoodDensStocking: SetParameters (Index, Parameter.WoodDensStocking, -1e6, 1e6, 1);
            F_HDConst: SetParameters (Index, Parameter.HD_Const, 0, 10, 1);
            F_HDTemp: SetParameters (Index, Parameter.HD_Temp, -1e6, 1e6, 1);
            F_HDStocking: SetParameters (Index, Parameter.HD_Stocking, -1e6, 1e6, 1);
            F_HDFertility: SetParameters (Index, Parameter.HD_Fertil, -1e6, 1e6, 1);
            F_HDAge1: SetParameters (Index, Parameter.HD_Age1, -1e9, 1e9, 1);
            F_HDAge2: SetParameters (Index, Parameter.HD_Age2, -1e9, 1e9, 1);
            F_HDInitSlope: SetParameters (Index, Parameter.HD_InitialSlope, 0, 10, 1);
            F_HDInitIntercept: SetParameters (Index, Parameter.HD_InitialInter, -1e9, 1e9, 1);
            F_HDSlopeMin: SetParameters (Index, Parameter.HDSlopeMin, 0, 100, 1);
            F_HDSlopeMax: SetParameters (Index, Parameter.HDSlopeMax, 0, 100, 1);
            F_MatureAge: SetParameters (Index, Parameter.MatureAge, 0, 1000, 1);
            F_MatureSize: SetParameters (Index, Parameter.MatureSize, 0, 10000, 1);
            F_TMinLim: SetParameters (Index, Parameter.TMinLim, -100, 100, 1);
            F_TOpt1: SetParameters (Index, Parameter.TOpt1, -100, 100, 1);
            F_TOpt2: SetParameters (Index, Parameter.TOpt2, -100, 100, 1);
            F_TmaxLim: SetParameters (Index, Parameter.TMaxLim, -100, 100, 1);
            F_TFrost: SetParameters (Index, Parameter.TFrost, -100, 100, 1);
            F_TScorch: SetParameters (Index, Parameter.TScorch, -100, 100, 1);
            F_TSensitivity: SetParameters (Index, Parameter.TSensitivity, 0, 1000, 1);
            F_TRepair: SetParameters (Index, Parameter.TRepair, 0, 1000, 1);
            F_WoodRetrans: SetParameters (Index, Parameter.WoodRetrans, 0, 1, 1);
            F_bRoots: SetParameters (Index, Parameter.bRoots, 0, 100, 1);
            F_bWood: SetParameters (Index, Parameter.bWood, 0, 100, 1);
            F_bBark: SetParameters (Index, Parameter.bBark, 0, 100, 1);
            F_bBranch: SetParameters (Index, Parameter.bBranch, 0, 100, 1);
            F_bFruit: SetParameters (Index, Parameter.bFruit, 0, 100, 1);
            F_bPollen: SetParameters (Index, Parameter.bPollen, 0, 100, 1);
            F_RelWaterSens: SetParameters (Index, Parameter.RelWaterSens, 0, 10, 1);
            F_N0: SetParameters (Index, Parameter.N0, 0, 100, 1000);
            F_Ncrit: SetParameters (Index, Parameter.NCrit, 0, 100, 1000);
            F_Nmax: SetParameters (Index, Parameter.Nmax, 0, 100, 1000);
     End;
If (Index = 0) then             // code for the end of the fitting routine
   Control.Parfit.FittingMode := False
Else if Index = 1 then          // Code for set-up of the fitting routine
   Writeln (Control.CenwFileOut);
End; {of Procedure 'SetUpFitParameters'}

Procedure RunSequence;
var BatchFileOK: Boolean;
Begin
SetUpFitParameters (-1);
FitParameter.SumOfSquares := 0;
FitParameter.Deviations := 0;
if Obs.RunBatch then
   Begin
   frmFileIO.GetParameterFile(Control.BatchFile, EXT_BATCH);
   BatchVariableList ('S', BatchFileOK);
   BatchFileOK := true;
   Control.BatchCount := 1;
   Control.EndBatch := false;
   While ((not FitParameter.Abort) and (((Control.BatchCount <= Control.BatchCalcs) or (Control.BatchCalcs < 0))
         and (not Control.EndBatch) and BatchFileOK and Control.BatchFileOpen)) do
         Begin
         BatchVariableList ('B', BatchFileOK);
         ReadObservationalData(Obs.ObservationFile);
         frmFileIO.GetParameterFile(Control.PoolFile, EXT_INITIAL);
         If Control.ResetPlantPools then
            ResetPlantPools;
         AdjustFertility;
         ControlRun;
         if (eof(Control.BatchName) or (Control.BatchCount = Control.BatchCalcs)) then
            Control.EndBatch := true;
         Control.BatchCount := Control.BatchCount + 1;
         if eof(Control.BatchName) or (Control.BatchCount = Control.BatchCalcs) then
            Control.EndBatch := true;
         End;
   BatchVariableList ('R', BatchFileOK);
   End
Else
   Begin
   frmFileIO.GetParameterFile(Control.PoolFile, EXT_INITIAL);
   If Control.ResetPlantPools then
      ResetPlantPools;
   AdjustFertility;
   ControlRun;
   End;
frmprogress.Update;
End; {of Procedure 'RunSequence'}

Procedure CompareResults;
Begin
if (FitParameter.OldSumOfSquares > FitParameter.SumOfSquares) then
    Begin
    FitParameter.OldSumOfSquares := FitParameter.SumOfSquares;
    FitParameter.Best := FitParameter.Varied;
    BestFitParameter := FitParameter;
    FitParameter.BetterFit := true;
    end
Else
    Begin
    FitParameter.BetterFit := false;
    FitParameter.Varied := FitParameter.Best;
    End;
End; {of Procedure 'CompareResults'}

Procedure ProcessFittingData;
Const ChangeLimit = 1e-6;
var SetFitVar: FittingType;

    Procedure MonteCarlo(SetFitVar: FittingType);
    var RandomNo: Real48;
    Begin
    RandomNo := 2 * (Random - 0.5);
    If RandomNo > 0 then
       RandomNo := Power(RandomNo, Control.ParFit.MonteCarloExponent)
    Else
       RandomNo := -Power(-RandomNo, Control.ParFit.MonteCarloExponent);
    if FitParameter.Best[SetFitVar] = FitParameter.Max[SetFitVar] then  // if we are at the maximum value already there is only a point in looking for lower possible values
       RandomNo := -abs(RandomNo)
    Else if FitParameter.Best[SetFitVar] = FitParameter.Min[SetFitVar] then// if we are at the minimum value already there is only a point in looking for higher possible values
       RandomNo := abs(RandomNo);
    if RandomNo > 0 then
       FitParameter.Varied[SetFitVar] := FitParameter.Best[SetFitVar] + RandomNo * (FitParameter.Max[SetFitVar] - FitParameter.Best[SetFitVar])
    Else
       FitParameter.Varied[SetFitVar] := FitParameter.Best[SetFitVar] + RandomNo * (FitParameter.Best[SetFitVar] - FitParameter.Min[SetFitVar]);
    End;  {of Procedure 'MonteCarlo'}

    Procedure SelectVariablesToRandomise;
    Const RandomLimit = 1e-4;
    var SetFitVar: FittingType;
        nSelected: Integer;
        NextRand: Real48;
    Begin
    if Control.ParFit.ParametersToRandomise >= FitParameter.ParsToOptimise then // RandomiseAll
       for SetFitVar := Succ(F_Dummy) to Pred(F_EndDummy) do
           FitParameter.Randomise[SetFitVar] := true
    Else   // Only randomise sum of those that have to be optimised - need to chhose which ones
       Begin
       nSelected := 0;
       repeat
           for SetFitVar := Succ(F_Dummy) to Pred(F_EndDummy) do
               if FitParameter.Choose[SetFitVar] then
                  Begin
                  NextRand := Random;
                  if (NextRand < RandomLimit) and (nSelected < Control.ParFit.ParametersToRandomise) then
                      Begin
                      FitParameter.Randomise[SetFitVar] := true;
                      nSelected := nSelected + 1;
                      End;
                  End
       until (nSelected = Control.ParFit.ParametersToRandomise);
       End;
    End; {of Procedure 'SelectVariablesToRandomise'}

    Procedure DatumOut (Datum: real48);
    var Width, Digits: integer;
    Begin
    GetField(Datum, MaxWidth, Width, Digits);
    Write (Control.CenwFileOut, Datum: Width: Digits, ' ');
    End;  {of Procedure 'DatumOut'}

    Function CheckOptimised(SetFitVar: FittingType): Boolean;
    var IsItOptimised: Boolean;
        Varied: Real48;
    Begin
    if abs(FitParameter.Initial[SetFitVar]) > Control.ParFit.Criterion1 then
       Varied := abs(Divide((FitParameter.Initial[SetFitVar] - FitParameter.Best[SetFitVar]), FitParameter.Initial[SetFitVar]))
    Else
       Varied := abs(FitParameter.Initial[SetFitVar] - FitParameter.Best[SetFitVar]);
    if (Varied > Control.ParFit.Criterion1) and FitParameter.Choose[SetFitVar] then
       IsItOptimised := true
    Else
       IsItOptimised := false;
    Write (Control.CenwFileOut, FittingNames[SetFitVar]:25, ' ');
    DatumOut (FitParameter.Initial[SetFitVar]);
    DatumOut (FitParameter.Best[SetFitVar]);
    Writeln (Control.CenwFileOut);
    CheckOptimised := IsItOptimised;
    End; {of Function 'CheckOptimised'}

Begin
FitParameter.Best := FitParameter.Varied;
Control.ParFit.Iterations := -1;
Control.SpatialFileOpen := false;
Repeat
   Control.ParFit.Iterations := Control.ParFit.Iterations + 1;
   If ((Control.ParFit.Iterations > 0) and FitParameter.BetterFit) then
       GetDerivatives
   Else if Control.ParFit.Iterations > 0 then // matrix did not find better fit - try random
      Begin
      FitParameter.Mode := ChooseRandom;
      SelectVariablesToRandomise;
      for SetFitVar := Succ(F_Dummy) to Pred(F_EndDummy) do
          Begin
          if FitParameter.Choose[SetFitVar] then
             Begin
             If FitParameter.Randomise[SetFitVar] then
                MonteCarlo(SetFitVar);
             FitParameter.Last[SetFitVar] := FitParameter.Varied[SetFitVar];
             End;
          End;
      RunSequence;
      CompareResults;
      End
   Else // For first iteration
      Begin
      FitParameter.Mode := Initial;
      RunSequence;
      CompareResults;
      End;
   FitParameter.HistoricFit[Control.ParFit.Iterations] := FitParameter.OldSumOfSquares;
   if abs(Divide((FitParameter.HistoricFit[Control.ParFit.Iterations] - FitParameter.HistoricFit[Control.ParFit.Iterations - 1]), FitParameter.HistoricFit[Control.ParFit.Iterations - 1])) < ChangeLimit then
      FitParameter.BetterFit := false;                               // The fit did not really improve - so go to random routine
   DatumOut (FitParameter.SumOfSquares);
   For SetFitVar := Succ(F_Dummy) to Pred(F_EndDummy) do
       If FitParameter.Choose[SetFitVar] then
          DatumOut (FitParameter.Varied[SetFitVar]);
   Writeln (Control.CenwFileOut);
Until (Control.ParFit.Iterations >= Control.ParFit.MaxIterations) or  // reached max iterations
      (FitParameter.SumOfSquares = 0) or                              // found good fit - need a better definition of it
      FitParameter.Abort or Control.ParFit.EndFit;                    // user wants to exit
Writeln (Control.CenwFileOut);
Write (Control.CenwFileOut, 'Final Sum Of Squares:':25, ' ');
DatumOut (FitParameter.OldSumOfSquares);
Writeln (Control.CenwFileOut);
Writeln (Control.CenwFileOut, '                           Initial   Final');
For SetFitVar := Succ(F_Dummy) to Pred(F_EndDummy) do
    if FitParameter.Choose[SetFitVar] then
       FitParameter.Optimised[SetFitVar] := CheckOptimised(SetFitVar)
    Else
       FitParameter.Optimised[SetFitVar] := false;
End; {of Procedure 'ProcessFittingData'}

Procedure AddToSumOfSquares;
var iDataSet: Integer;
    Width, Digits, iLayer: Integer;
    Sum: Real48;
    ValidResult: Boolean;
    ModelledDatum: Real48;

    Procedure CheckDatum (iDataSet: Integer; ObservedDatum: Real48);

        Procedure DataOK (Datum, Multiplier: Real48);
        Begin
        ModelledDatum := Datum * Multiplier;
        ValidResult := true;
        End; {of Procedure 'DataOK'}

        Function Sum (TypeIdentifier: Char; iLayer: Integer; E: ElementsUsed): Real48;
        Begin
        Case TypeIdentifier of
             'S': Sum := Soil.CoarseWood[iLayer, E] +
                         Soil.FineWood[iLayer, E] +
                         Soil.Struct[iLayer, E] +
                         Soil.Metab[iLayer, E] +
                         Soil.Active[iLayer, E] +
                         Soil.Slow[iLayer, E] +
                         Soil.Resistant[iLayer, E] +
                         Soil.Inert[iLayer, E] +
                         Soil.Soluble[iLayer, E];
             'L': Sum := Soil.Struct[iLayer, E] +
                         Soil.Metab[iLayer, E];
             'W': Sum := Soil.CoarseWood[iLayer, E] +
                         Soil.FineWood[iLayer, E];
             'F': Sum := Litter.Leaves[E] + Litter.CoarseWood[E] + Litter.FineWood[E]  + Litter.Other[E];
             'B': Sum := Sum('A', 0, C) + Plant.FineRoot[E] + Plant.CoarseRoot[E];
             'A': Sum := Plant.SapWood[E] + Plant.HeartWood[E] + Plant.Leaves[E] + Plant.Branches[E] +
                         Plant.Bark[E] + Plant.Pollen[E] + Plant.Fruit[E] + Plant.Reserves[E] + Plant.Soluble[E];
             End;
        End; {of Function 'Sum'}

        Function SumLayers (DataType: Char): Real48;
        var iLayer: Integer;
            TempSum: Real48;
        Begin
        TempSum := 0;
        for iLayer := 0 to Soil.nLayers do
            Begin
            case DataType of
                 'N': TempSum := TempSum + Soil.Soluble[iLayer, N];
                 End;
            End;
        SumLayers := TempSum;
        End; {of Function 'SumLayers'}

    Begin  {of Procedure 'CheckDatum'}
    ValidResult := False;
    case Obs.DataType[iDataSet] of
         O_Dummy: ; // need nothing doing - loaded unrecognised data type
         O_SapWoodC: DataOK(Plant.Sapwood[C], Control.CConversion * 0.001);
         O_HeartWoodC: DataOK(Plant.Heartwood[C], Control.CConversion * 0.001);
         O_Wood: DataOK(Plant.Sapwood[C]  + Plant.HeartWood[C], Control.CConversion * 0.001);
         O_LeafC: DataOK(Plant.Leaves[C], Control.CConversion * 0.001);
         O_FineRootC: DataOK(Plant.FineRoot[C], Control.CConversion * 0.001);
         O_BarkC: DataOK(Plant.Bark[C], Control.CConversion * 0.001);
         O_CoarseRootC: DataOK(Plant.CoarseRoot[C], Control.CConversion * 0.001);
         O_BranchesC: DataOK(Plant.Branches[C], Control.CConversion * 0.001);
         O_ReprodC: DataOK(Plant.Fruit[C] + Plant.Pollen[C], Control.CConversion * 0.001);
         O_BiomassC: DataOK(Sum('B', 0, C), Control.CConversion * 0.001);
         O_AgBiomassC: DataOK(Sum('A', 0, C), Control.CConversion * 0.001);
         O_SapWoodN: DataOK(Plant.Sapwood[N], Control.NConversion);
         O_HeartWoodN: DataOK(Plant.Heartwood[N], Control.NConversion);
         O_WoodN: DataOK(Plant.SapWood[N] + Plant.HeartWood[N], Control.NConversion);
         O_LeafN: DataOK(Plant.Leaves[N], Control.NConversion);
         O_FineRootN: DataOK(Plant.FineRoot[N], Control.NConversion);
         O_BarkN: DataOK(Plant.Bark[N], Control.NConversion);
         O_CoarseRootN: DataOK(Plant.CoarseRoot[N], Control.NConversion);
         O_BranchesN: DataOK(Plant.Branches[N], Control.NConversion);
         O_ReprodN: DataOK(Plant.Fruit[N] + Plant.Pollen[N], Control.NConversion);
         O_TotalN: DataOK(TotalN, Control.NConversion);
         O_LAI: DataOK(Derived.LAI[Over], 1);
         O_Height: DataOK(Plant.Height, 1);
         O_DBH: DataOK(Plant.DBH, 1);
         O_BasalArea: DataOK(Plant.Area, 0.0001);
         O_Stocking: DataOK(Plant.Stocking, 1);
         O_pi: DataOK(Derived.p_internal, 1);
         O_CAI: DataOK(Derived.CAI, Control.CConversion * 0.001);
         O_NPP: DataOK(Derived.NPP, Control.CConversion * 0.001);
         O_NEE: DataOK(Derived.NEE, Control.CConversion * 0.001);
         O_CarbonGain: DataOK(Derived.CarbonGain, Control.CConversion);
         O_Respn: DataOK(Derived.Rm + Derived.Rg, Control.CConversion);
         O_DayCFlux: DataOK(Derived.DayCFlux, Control.CConversion);
         O_NightCFlux: DataOK(Derived.NightCFlux, Control.CConversion);
         O_EvapoTranspiration: DataOK(Derived.Transpiration + Derived.Evaporation, 1);
         O_NConc: DataOK(1000 * Derived.NConc[Over], Control.NConversion / Control.CConversion);
         O_CLitterSurf: DataOK(Sum('L', 0, C), Control.CConversion * 0.001);
         O_NLitterSurf: DataOK(Sum('L', 0, N), Control.NConversion);
         O_CWoodyLitter: DataOK(Sum('W', 0, C), Control.CConversion * 0.001);
         O_NWoodyLitter: DataOK(Sum('W', 0, N), Control.NConversion);
         O_SOM0: DataOK(Sum('S', 0, C), Control.CConversion * 0.001);
         O_SON0: DataOK(Sum('S', 0, N), Control.NConversion);
         O_SOM1: DataOK(Sum('S', 1, C), Control.CConversion * 0.001);
         O_SON1: DataOK(Sum('S', 1, N), Control.NConversion);
         O_SOM2: DataOK(Sum('S', 2, C), Control.CConversion * 0.001);
         O_SON2: DataOK(Sum('S', 2, N), Control.NConversion);
         O_SOM3: DataOK(Sum('S', 3, C), Control.CConversion * 0.001);
         O_SON3: DataOK(Sum('S', 3, N), Control.NConversion);
         O_SOM4: DataOK(Sum('S', 4, C), Control.CConversion * 0.001);
         O_SON4: DataOK(Sum('S', 4, N), Control.NConversion);
         O_SOM5: DataOK(Sum('S', 5, C), Control.CConversion * 0.001);
         O_SON5: DataOK(Sum('S', 5, N), Control.NConversion);
         O_CLitterFall: DataOK(Sum('F', 0, C), Control.CConversion * 0.001);
         O_NLitterFall: DataOK(Sum('F', 0, N), Control.NConversion);
         O_CLeafLitterFall: DataOK(Litter.Leaves[C], Control.CConversion * 0.001);
         O_NLeafLitterFall: DataOK(Litter.Leaves[N], Control.NConversion);
         O_NMineral: DataOK(SumLayers('N'), Control.NConversion);
         O_NLeached: DataOK(Derived.NLeached, Control.NConversion);
         O_SoilRespn: DataOK(Derived.SoilRespn, Control.CConversion);
         O_StoredWater: DataOK(Soil.TotalWater, 1);
         O_Drainage: DataOK(Derived.Drainage[Soil.nLayers], 1);
         End;
    End; {of Procedure 'CheckDatum'}

Begin    {of Procedure 'AddToSumOfSquares;'}
for iDataSet := 1 to Obs.nDataSets do
    Begin
    if Obs.TimePoint[iDataSet, Obs.PointCounter[iDataSet]] = Control.TotalDays then
       Begin
       CheckDatum (iDataSet, Obs.Data[iDataSet, Obs.PointCounter[iDataSet]]);
       if ValidResult then
          Begin
          if Obs.IncludeWeight then
             Begin
             FitParameter.SumOfSquares := FitParameter.SumOfSquares + Obs.Weight[iDataSet, Obs.PointCounter[iDataSet]] *
                       Sqr(ModelledDatum - Obs.Data[iDataSet, Obs.PointCounter[iDataSet]]);
             FitParameter.Deviations := FitParameter.Deviations + Obs.Weight[iDataSet, Obs.PointCounter[iDataSet]] *
                       (ModelledDatum - Obs.Data[iDataSet, Obs.PointCounter[iDataSet]]);
             End
          Else
             Begin
             FitParameter.SumOfSquares := FitParameter.SumOfSquares +
                       Sqr(ModelledDatum - Obs.Data[iDataSet, Obs.PointCounter[iDataSet]]);
             FitParameter.Deviations := FitParameter.Deviations +
                       (ModelledDatum - Obs.Data[iDataSet, Obs.PointCounter[iDataSet]]);
             End;
          End
       Else // the date was right, but something did not match, so record an error code
          Obs.Modelled[iDataSet, Obs.PointCounter[iDataSet]] := -999;
       Obs.PointCounter[iDataSet] := Obs.PointCounter[iDataSet] + 1;
       End;
    End;
End; {of Procedure 'AddToSumOfSquares'}

Procedure GetDerivatives;
Label GetOut;
var SetFitVar: FittingType;
    i, j, ii:  Integer;
    Width, Digits: Integer;
Begin
FitParameter.Varied := FitParameter.Best;
FitParameter.Last := FitParameter.Best;
ii := 0;
for i := 1 to FitParameter.ParsToOptimise do
    Begin
    for j := 1 to FitParameter.ParsToOptimise do
        FitParameter.Matrix[i, j] := 0;
    End;
FitParameter.Mode := Gather;
for SetFitVar := Succ(F_Dummy) to Pred(F_EndDummy) do
    Begin
    if FitParameter.Choose[SetFitVar] then
       Begin
       ii := ii + 1;
       FitParameter.OldxVar[ii] := FitParameter.Varied[SetFitVar];
       FitParameter.Varied[SetFitVar] := FitParameter.Varied[SetFitVar] * (1 + FitParameter.deltaX[SetFitVar]);
       FitParameter.Focus := SetFitVar;
       RunSequence;
       if FitParameter.Abort then
          Goto GetOut;
       FitParameter.LastSS[SetFitVar] := FitParameter.SumOfSquares;
       FitParameter.DiffYX[ii]:= -Divide((FitParameter.SumOfSquares - FitParameter.HistoricFit[Control.ParFit.Iterations - 1]), (FitParameter.Varied[SetFitVar] * FitParameter.deltaX[SetFitVar]));
       if FitParameter.DiffYX[ii] > 0 then
          FitParameter.deltaX[SetFitVar] := abs(FitParameter.deltaX[SetFitVar])
       Else
          FitParameter.deltaX[SetFitVar] := -abs(FitParameter.deltaX[SetFitVar]);
       FitParameter.Varied[SetFitVar] := FitParameter.OldxVar[ii];
       End;
    End;
for i := 1 to FitParameter.ParsToOptimise do
    Begin
    for j := 1 to FitParameter.ParsToOptimise do
        Begin
        FitParameter.Matrix[i, j] := FitParameter.DiffYX[i] * FitParameter.DiffYX[j];
        End;
    End;
FormulateMatrix;
GetOut:
End;

Procedure FormulateMatrix;
var btt: Real48;
    i, j, iii: Integer;
    Width, Digits: Integer;
    MatrixImprovedFit: Boolean;

    Procedure MatrixInversion;
    var i, j, k: Integer;
        mm, m: Real48;
    Begin
    for i := 1 to FitParameter.ParsToOptimise do
        Begin
        for j := 1 to FitParameter.ParsToOptimise do
            Begin
            if i = j then
               FitParameter.InverseMatrix[i, FitParameter.ParsToOptimise + j] := 1
            Else
               FitParameter.InverseMatrix[i, FitParameter.ParsToOptimise + j] := 0;
            End
        End;
    for i := 1 to FitParameter.ParsToOptimise do
        Begin
        mm := FitParameter.InverseMatrix[i, i];
        for k := 2 * FitParameter.ParsToOptimise downto i do
            Begin
            FitParameter.InverseMatrix[i, k] := Divide(FitParameter.InverseMatrix[i, k], mm);
            End;
        for k := 1 to FitParameter.ParsToOptimise do
            Begin
            if (i <> k)  then
               Begin
               m := FitParameter.InverseMatrix[k, i];
               for j := i to 2 * FitParameter.ParsToOptimise do
                   FitParameter.InverseMatrix[k, j] := FitParameter.InverseMatrix[k, j] - m * FitParameter.InverseMatrix[i, j];
               End;
            End;
        End;
    End;  {of Procedure 'MatrixInversion'}

    Procedure AdjustVariables;
    var i, j: integer;
        SetFitVar: FittingType;
        adelta: Array [1..MaxParametersToFit] of Real48;
        UpLimit, DownLimit: Real48;
    Begin
    for i := 1 to FitParameter.ParsToOptimise do
        Begin
        adelta[i] := 0;
        btt := 0;
        for j := 1 to FitParameter.ParsToOptimise do
            adelta[i] := adelta[i] + FitParameter.InverseMatrix[i, FitParameter.ParsToOptimise + j] * FitParameter.DiffYX[j];
        FitParameter.NewxVar[i] := FitParameter.OldxVar[i] + FitParameter.SensMultiplier * adelta[i];   // New values of the variables
        btt := btt + abs(adelta[i] - FitParameter.NewxVar[i]);
        end;
    iii:=0 ;
    for SetFitVar := Succ(F_Dummy) to Pred(F_EndDummy) do
        Begin
        if FitParameter.Choose[SetFitVar] then
           Begin
           iii := iii + 1;
           UpLimit := FitParameter.Last[SetFitVar] + Control.ParFit.MaxChange * (FitParameter.Max[SetFitVar] - FitParameter.Last[SetFitVar]);
           DownLimit := FitParameter.Last[SetFitVar] - Control.ParFit.MaxChange * (FitParameter.Last[SetFitVar] - FitParameter.Min[SetFitVar]);
           FitParameter.Varied[SetFitVar] := FitParameter.Newxvar[iii];
           if FitParameter.Varied[SetFitVar] > UpLimit  then
              FitParameter.Varied[SetFitVar] := UpLimit
           Else if FitParameter.Varied[SetFitVar] < DownLimit  then
              FitParameter.Varied[SetFitVar] := DownLimit;
           End;
        End;
    End; {of Procedure 'AdjustVariables'}

    Procedure CompareBestIndividual;
    var SetFitVar: FittingType;
        InitialImproved: Boolean;
        BestSS: Real48;
        UpLimit, DownLimit: Real48;
    Begin
    FitParameter.Mode := Individual;
    BestSS := FitParameter.OldSumOfSquares;
    for SetFitVar := Succ(F_Dummy) to Pred(F_EndDummy) do
        if FitParameter.Choose[SetFitVar] then
           Begin
           if FitParameter.LastSS[SetFitVar] < BestSS then
              Begin
              BestSS := FitParameter.LastSS[SetFitVar];
              FitParameter.Focus := SetFitVar;
              End;
           End;
    if BestSS < FitParameter.OldSumOfSquares then  // one individual parameter achieved greater improvement than the matrix
       Begin
       UpLimit := FitParameter.Last[FitParameter.Focus] + Control.ParFit.MaxChange * (FitParameter.Max[FitParameter.Focus] - FitParameter.Last[FitParameter.Focus]);
       DownLimit := FitParameter.Last[FitParameter.Focus] - Control.ParFit.MaxChange * (FitParameter.Last[FitParameter.Focus] - FitParameter.Min[FitParameter.Focus]);
       FitParameter.Varied := FitParameter.Last;
       FitParameter.Varied[FitParameter.Focus] := FitParameter.Varied[FitParameter.Focus] * (1 + FitParameter.deltaX[FitParameter.Focus]);
       If FitParameter.Varied[FitParameter.Focus] > UpLimit then
          FitParameter.Varied[FitParameter.Focus] := UpLimit
       Else if FitParameter.Varied[FitParameter.Focus] < DownLimit then
          FitParameter.Varied[FitParameter.Focus] := DownLimit;
       RunSequence;
       CompareResults;
       InitialImproved := true;
       while FitParameter.BetterFit and (not FitParameter.Abort) do
             Begin
             FitParameter.Varied[FitParameter.Focus] := FitParameter.Varied[FitParameter.Focus] * (1 + FitParameter.deltaX[FitParameter.Focus]);
             FitParameter.deltaX[FitParameter.Focus] := FitParameter.deltaX[FitParameter.Focus] * Control.ParFit.Criterion3;
             If FitParameter.Varied[FitParameter.Focus] > UpLimit then
                FitParameter.Varied[FitParameter.Focus] := UpLimit
             Else if FitParameter.Varied[FitParameter.Focus] < DownLimit then
                FitParameter.Varied[FitParameter.Focus] := DownLimit;
             RunSequence;
             CompareResults;
             End;
       FitParameter.Varied := FitParameter.Best;
       if FitParameter.deltaX[FitParameter.Focus] > 0 then
          FitParameter.deltaX[FitParameter.Focus] := Control.ParFit.Delta
       Else
          FitParameter.deltaX[FitParameter.Focus] := -Control.ParFit.Delta;
       if MatrixImprovedFit or InitialImproved then
          FitParameter.BetterFit := true;
       End;
    End; {of Procedure 'CompareBestIndividual'}

Begin {Start of Procedure 'FormulateMatrix'}
for i := 1 to FitParameter.ParsToOptimise do
    Begin
    for j := 1 to FitParameter.ParsToOptimise do
        FitParameter.InverseMatrix[i, j] := FitParameter.Matrix[i, j];
    End;
MatrixInversion;
// Routine to find optimum SensMultiplier
FitParameter.Mode := Matrix;
AdjustVariables;
RunSequence;
CompareResults;
MatrixImprovedFit := FitParameter.BetterFit;
if MatrixImprovedFit then // go the path of seeing whether a larger relative change would give us a bigger gain
   Begin
   repeat
       FitParameter.SensMultiplier := FitParameter.SensMultiplier * 2;
       AdjustVariables;
       RunSequence;
       CompareResults;
   until ((not FitParameter.BetterFit) or FitParameter.Abort);
   FitParameter.BetterFit := MatrixImprovedFit;
   End
Else if FitParameter.SensMultiplier > Control.ParFit.Criterion2 then // matrix did not improve fit - maybe the change was too big
   Begin
   Repeat
       FitParameter.SensMultiplier := FitParameter.SensMultiplier / 5;
       AdjustVariables;
       RunSequence;
       CompareResults;
   Until (FitParameter.BetterFit or (FitParameter.SensMultiplier < Control.ParFit.Criterion2) or FitParameter.Abort);
   MatrixImprovedFit := FitParameter.BetterFit;
   End
Else
   FitParameter.SensMultiplier := Control.ParFit.Criterion2;     // Did not improve fit, and we were already at the lower allowed limit
if FitParameter.CompareBest then    // Include the option to compare the matrix improvement with that of the best individual parameter
   CompareBestIndividual;
if btt < 0.01 then   // Check if the minimum error criterion is reached
   Begin
       // Write and display the new variables in xvar[i] and stop run
   End;
End; {of Procedure 'FormulateMatrix'}

End.

{ --- end of file ProcessFittingData.pas ------------------------------------------ }








