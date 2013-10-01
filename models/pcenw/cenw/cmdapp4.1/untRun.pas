{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : EndRun                                           =
  =             ControlRun                                       =
  =             Initialise                                       =
  =                                                              =
  =             Routines to control the running of the program   =
  ================================================================
  = File      : untRun.PAS                                       =
  =                                                              =
  = Version   : 4.1                                              =
  ================================================================ }

unit untRun;

interface

Procedure EndRun;
Procedure ControlRun;
Procedure Initialise;

implementation

uses SysUtils, Math, untDeclarations, untSimulate, untSimSoil, untDivideValidation,
  untMiscellaneous, untDiskOut{, TeEngine};
//  Forms, Windows, untParFitProgress, untProcessFittingData,untGraph, TeEngine, Series, untProgress, untEquilProgress, untNotices//JL removed. Not verified.

Procedure Initialise;
Var E: ElementsUsed;
    Days: 1..365;
    Years: 0..MaxSapWoodYears;
    s: array[0..255] of char;
    st: string[255];
    ScreenVar : ScreenOptions;
    StartNewGraph: Boolean;
    i, iLayer, ErrorNo, MaxDisplay, iDataSet: integer;
    SumStruct, SumStructN, SumMetab, SumMetabN, SumFine, SumCoarse, SumCoarseN, SumActive,
    SumActiveN, SumSlow, SumSlowN, SumResistant, SumResistantN, SumMineral: double;

    Function f1(theta: double): double; {See Sands, 1995; these are needed for photosynthesis calculations}
    Const a1 = 0.22; b1 = 0.74;
    Begin
    f1 := 1 + a1 * theta * (1 - theta) + b1 * sqr(theta) * sqr(1 - theta);
    End; {of Function 'f1'}

    Function f2(theta: double): double;  {See Sands, 1995}
    Const  a2 = -0.18; b2 = 0.5;
    Begin
    f2 := a2 * theta + b2 * sqr(theta) + (1 - a2 - b2) * Power(theta, 3);
    End; {of Function 'f2'}

    Procedure Initial_CalcHt_Diameter;
    {The Ht_Diameter parameter is completely prescribed by the other parameters and therefore
     should not be set by the user. If a constant HT_Diamater parameter is used it does not need to be recalculated at each iteration, either.
     So, the calculation is done here once at the beginning of each run.}
    var Height, BasalDiam: double;
    Begin
    Derived.HDSlope := Parameter.HD_InitialSlope;
    If Parameter.Mindbh > 1 then
       Begin
       Height := exp(Parameter.HDInter + Derived.HDSlope * ln(Parameter.Mindbh));
       If Height > 1.3 then
          Begin
          BasalDiam := Parameter.Mindbh * Height / (Height - 1.3);
          Parameter.Ht_Diameter := 100 * Height / BasalDiam;
          End
       Else
          Parameter.Ht_Diameter := 10;   // to prevent a crash
       End
    Else
       Begin
       Parameter.Mindbh := 1;
       Parameter.Ht_Diameter := 10;   // more precautions against a crash
       End;
    End; {of Procedure 'Initial_CalcHt_Diameter'}

{    Procedure FirstPixel;
    var series: TFastLineSeries;
    Begin
    // create a new series
    series := TFastLineSeries.Create(frmGraph.chtGraph);
    series.ColorEachPoint := false;
    series.Title := 'Spatial representation';
    series.SeriesColor := 0;
    series.AddXY(1, 1);
    // add the series to the graph
    frmGraph.chtGraph.AddSeries(series);
    // add the series to the combobox too, recording which screen item is being graphed
    frmGraph.cmbSeries.Items.AddObject(series.Title, pointer(D_Equil));
    End; {of Procedure 'FirstPixel'}
} //JL commented

Begin
Derived.Functionf1 := f1(Parameter.Theta);
Derived.Functionf2 := f2(Parameter.Theta);
Initial_CalcHt_Diameter;
CalculateWoodDensity;
if Control.IncludeWeeds then
   Plant.WeedHeight := Parameter.Weed.MaxHeight * Divide(Plant.WeedLeaves[C], (Plant.WeedLeaves[C] + Parameter.Weed.KMHeight / Control.CConversion))
Else                         // if we don't include weeds, make sure the litter amounts are set to 0
   Begin
   Plant.WeedHeight := 0;
   Derived.Intercept[Under] := 0;
   For E := C to P do
       Begin
       Litter.WeedLeaves[E] := 0;
       Litter.WeedRoots[E] := 0;
       Plant.NewWeedGrowth[E] := 0;
       End;
   End;
If Control.AllOneLayer then // just check that the litter input amounts are sensible. Can be a problem with reading old files
   Begin
   If Soil.FineRootLitterIn[1] <> 1 then
      Soil.FineRootLitterIn[1] := 1;
   If Soil.CoarseRootLitterIn[1] <> 1 then
      Soil.CoarseRootLitterIn[1] := 1;
   End
Else
   Begin
   SumFine := 0;  SumCoarse := 0;
   For iLayer := 1 to Soil.nLayers do
       Begin
       SumFine := SumFine + Soil.FineRootLitterIn[iLayer];
       SumCoarse := SumCoarse + Soil.CoarseRootLitterIn[iLayer];
       End;
   If SumFine = 0 then
      SumFine := 1;
   If SumCoarse = 0 then
      SumCoarse := 1;
   if abs(SumCoarse - 1) < 1e-6 then
      SumCoarse := 1;                  // need to make no adjustment and want to avoid making spurious changes
   if abs(SumFine - 1) < 1e-6 then
      SumFine := 1;                  // need to make no adjustment and want to avoid making spurious changes
   For iLayer := 1 to Soil.nLayers do
       Begin
       Soil.FineRootLitterIn[iLayer] := Soil.FineRootLitterIn[iLayer] / SumFine;
       Soil.CoarseRootLitterIn[iLayer] := Soil.CoarseRootLitterIn[iLayer] / SumCoarse;
       End;
   End;
If (not Control.EquilMode) and (not Control.Parfit.FittingMode) then
   SaveAnyInfo
Else
   Control.DataToBeSaved := false;
CalcDate (Control.Date);
Control.Run_on := true;
If (Control.DataToBeSaved and not Control.OutputFileOpen) or
    Control.SpatialMode then
    Begin
    {$I-}
    assign (Control.CenwFileOut, Control.FileOut);
    rewrite (Control.CenwFileOut);
    {$I+}
    ErrorNo := IOResult;
    If ErrorNo <> 0 then
        Begin
        If ErrorNo = 32 then
           Begin
           st := '              Close the file' + chr(10) +
                 '       in the other application' + chr(10) +
                 'before re-running the simulation' + chr(10);
           strpcopy(s,st);
//           Application.MessageBox(s, 'Output file already open!', MB_OK);
           End
        Else
           Begin
           st := '    Change the output filename' + chr(10) +
                 '           under menu option' + chr(10) +
                 '           PARAMETERS' + chr(10) +
                 '           CONTROL...' + chr(10);
           strpcopy(s,st);
 //          Application.MessageBox(s, 'Incorrect output file name selected!', MB_OK);
           End;
        Control.Run_on := false;
        if Control.BatchMode then
           Control.EndBatch := true;
        end
    else
        Begin
        SaveAtStart (Control.FileOut);
        Control.OutputFileOpen := true;
        End;
    End;
For ScreenVar := D_CarbonGain to D_Dummy do
    OldScreen[ScreenVar].First := true;
Control.UseSimulated := false;
Control.StartRun := true;
Control.IrrigAnnounced := false;
Control.ClimFileOpen := false;
Control.GrazingFormulaOn := false;
Control.NewYear := true;
Control.DaysSince := 0;
Control.TotalDays := 0;
If Control.NextPhenology < 1 then
   Control.NextPhenology := 1
Else if Control.NextPhenology > Parameter.Phenology.nChanges then
   Control.NextPhenology := 1;
If Control.PhenologyDayCount < 0 then
   Control.PhenologyDayCount := 0;
DecompCount := 0;
for iDataSet := 1 to Obs.nDataSets do
    Obs.PointCounter[iDataSet] := 1;
Plant.NewGrowth[C] := 0; Plant.NewGrowth[N] := 0;
Control.MaxDays := round(365.25 * Control.nYears + 30.4375 * Control.nMonths + Control.nDays);
Control.DisplayInterval := trunc(Divide(Control.MaxDays, Control.nDisplays));
If Control.nDiskOut < 2 then
   Control.DiskInterval := Control.MaxDays
else
   Control.DiskInterval := trunc(Divide(Control.MaxDays, Control.nDiskOut));
Control.DaysSinceDisk := Control.DiskInterval;
Control.NextFertilise := 1;
Control.NextHarvest := 1;
Control.NextOMAdditions := 1;
Control.NextPest := 1;
Control.NextFire := 1;
Control.NextPlough := 1;
Control.NextEnvironment := 1;
Control.NextGrazing := 1;
Control.NextGrazing2 := 1;
Control.NextSeeding := 1;
Control.PestMode := false;
Derived.PestMortality := 0;
Derived.PestLeafDamage := 0;
Derived.PestSolubleDamage := 0;
Derived.PestSenesc := 0;
Derived.PestPhsFrac := 0;
CalculateWoodDensity;
If Event.Irrigate then
     Begin
     If Event.IrrigStartYear <= 0 then
        Event.DaysSinceIrrigation := 0
     Else
        Event.DaysSinceIrrigation := -trunc(365.25 * (Event.IrrigStartYear - Control.TotalYears)
                                          + 30.4375 * (Event.IrrigStartMonth - Control.ExtraMonths)
                                          + Event.IrrigStartDay - Control.ExtraDays - 1);
     If Event.IrrigEndYear <= 0 then
        Control.LastIrrigation := Control.MaxDays
     Else
        Control.LastIrrigation := trunc(365.25 * (Event.IrrigEndYear - Control.TotalYears)
                                          + 30.4375 * (Event.IrrigEndMonth - Control.ExtraMonths)
                                          + Event.IrrigEndDay - Control.ExtraDays - 1);
     End;
For i := 1 to Event.nFertilisations do
    Begin
    If Control.Fertilise_DateType = 'D' then
         Event.FertiliseTimes[i,4] := trunc(365.25 * (Event.FertiliseTimes[i,3] - Control.TotalYears)
                                    + 30.4375 * (Event.FertiliseTimes[i,2] - Control.ExtraMonths)
                                    + Event.FertiliseTimes[i,1] - Control.ExtraDays - 1)
    else
         Event.FertiliseTimes[i,4] := trunc(365.25 * Event.FertiliseTimes[i,3]
                                          + 30.4375 * Event.FertiliseTimes[i,2]
                                                    + Event.FertiliseTimes[i,1]);
    End;
Event.NFertiliserAdded := 0;
Event.PFertiliserAdded := 0;
For i := 1 to Event.nHarvests do
    Begin
    If (Event.HarvestTimes[i,1] <> 0) or (Event.HarvestTimes[i,2] <> 0) or (Event.HarvestTimes[i,3] <> 0) then
       Begin
       If Control.Harvest_DateType = 'D' then
          Event.HarvestTimes[i,4] := trunc(365.25 * (Event.HarvestTimes[i,3] - Control.TotalYears)
                                    + 30.4375 * (Event.HarvestTimes[i,2] - Control.ExtraMonths)
                                    + Event.HarvestTimes[i,1] - Control.ExtraDays - 1)
       else
          Event.HarvestTimes[i,4] := trunc(365.25 * Event.HarvestTimes[i,3]
                                          + 30.4375 * Event.HarvestTimes[i,2]
                                                    + Event.HarvestTimes[i,1]);
       End;
    End;
For i := 1 to Event.nPests do
    Begin
    If (Event.PestTimes[i,2] > 0) or (Event.PestTimes[i,3] > 0) then
      Begin
      If Control.Pest_DateType = 'D' then
         Event.PestTimes[i,4] := trunc(365.25 * (Event.PestTimes[i,3] - Control.TotalYears)
                                    + 30.4375 * (Event.PestTimes[i,2] - Control.ExtraMonths)
                                    + Event.PestTimes[i,1] - Control.ExtraDays - 1)
      else
         Event.PestTimes[i,4] := trunc(365.25 * Event.PestTimes[i,3]
                                          + 30.4375 * Event.PestTimes[i,2]
                                                    + Event.PestTimes[i,1]);
      End
    Else
      Event.PestTimes[i, 4] := Control.MaxDays;
    End;
For i := 1 to Event.nFires do
    If (Event.FireTimes[i,1] <> 0) or (Event.FireTimes[i,2] <> 0) or (Event.FireTimes[i,3] <> 0) then
      If Control.Fire_DateType = 'D' then
         Event.FireTimes[i,4] := trunc(365.25 * (Event.FireTimes[i,3] - Control.TotalYears)
                                    + 30.4375 * (Event.FireTimes[i,2] - Control.ExtraMonths)
                                    + Event.FireTimes[i,1] - Control.ExtraDays - 1)
      else
         Event.FireTimes[i,4] := trunc(365.25 * Event.FireTimes[i,3]
                                          + 30.4375 * Event.FireTimes[i,2]
                                                    + Event.FireTimes[i,1]);
Derived.LastYearsWood[0]:= Plant.SapWood[C] + Plant.HeartWood[C];
If Plant.Age < Parameter.SapWoodYears then
   Begin
   For Years := 1 to Plant.Age do
       Plant.SapWoodAmount[Years] := Plant.SapWood[C] / Plant.Age;
   For Years := Plant.Age to Parameter.SapWoodYears do
       Plant.SapWoodAmount[Years] := 0;
   End
Else
   For Years := 1 to Parameter.SapWoodYears do
       Plant.SapWoodAmount[Years] := Plant.SapWood[C] / Parameter.SapWoodYears;
if Control.CountersSaved and (not Control.ResetPlantPools) then
   Begin
   Derived.NPP := 0;
   For Days := 1 to 365 do
       Derived.NPP := Derived.NPP + Derived.NPPCount[Days];
   End
Else
   Begin
   Derived.NPP := 0;
   For Days := 1 to 365 do
       Begin
       Derived.LastYearsWood[Days] := 0;
       Derived.NPPCount[Days] := 0;
       Derived.LastYearsDecomp[Days] := 0;
       Derived.Temperature[Days] := 0;
       End;
   End;
Control.Count := 0;
Weather.ExtraCO2 := 0;
Weather.ExtraTemp := 0;
Weather.ExtraRain := 1;
Weather.ExtraVP := 1;
Weather.ExtraRadn := 1;
Soil.MaxWater := 0;
If Control.NextPhenology > Parameter.Phenology.nChanges then
   Control.NextPhenology := 1; // In case the phenology set up has changed and we are now outside the range of permissable values
Derived.PestPhsFrac := 1;
If Derived.RespnBase < 0 then // extra safeguard in case they have not been properly
   Derived.RespnBase := 0     // initialised in a previous run
Else if Derived.RespnBase > 10 then
   Derived.RespnBase := 10;
If Parameter.RespnAdjust < 1 then
   Parameter.RespnAdjust := 1;
For iLayer := 1 to Soil.nLayers do
    Soil.MaxWater := Soil.MaxWater + Soil.WaterLayer[iLayer].MaxWater;
If Control.AllOneLayer then {amalgamate all soil layers into the top layer}
     Begin
     SumStruct := 0;     SumStructN := 0;
     SumMetab := 0;      SumMetabN := 0;
     SumCoarse := 0;     SumCoarseN := 0;
     SumActive := 0;     SumActiveN := 0;
     SumSlow := 0;       SumSlowN := 0;
     SumResistant := 0;  SumResistantN := 0;
     SumMineral := 0;
     For iLayer := 1 to Soil.nLayers do
         Begin
         SumStruct := SumStruct + Soil.Struct[iLayer, C];
         Soil.Struct[iLayer, C] := 0;
         SumStructN := SumStructN + Soil.Struct[iLayer, N];
         Soil.Struct[iLayer, N] := 0;
         SumMetab := SumMetab + Soil.Metab[iLayer, C];
         Soil.Metab[iLayer, C] := 0;
         SumMetabN := SumMetabN + Soil.Metab[iLayer, N];
         Soil.Metab[iLayer, N] := 0;
         SumCoarse := SumCoarse + Soil.CoarseWood[iLayer, C];
         Soil.CoarseWood[iLayer, C] := 0;
         SumCoarseN := SumCoarseN + Soil.CoarseWood[iLayer, N];
         Soil.CoarseWood[iLayer, N] := 0;
         SumActive := SumActive + Soil.Active[iLayer, C];
         Soil.Active[iLayer, C] := 0;
         SumActiveN := SumActiveN + Soil.Active[iLayer, N];
         Soil.Active[iLayer, N] := 0;
         SumSlow := SumSlow + Soil.Slow[iLayer, C];
         Soil.Slow[iLayer, C] := 0;
         SumSlowN := SumSlowN + Soil.Slow[iLayer, N];
         Soil.Slow[iLayer, N] := 0;
         SumResistant := SumResistant + Soil.Resistant[iLayer, C];
         Soil.Resistant[iLayer, C] := 0;
         SumResistantN := SumResistantN + Soil.Resistant[iLayer, N];
         Soil.Resistant[iLayer, N] := 0;
         SumMineral := SumMineral + Soil.Soluble[iLayer, N];
         Soil.Soluble[iLayer, N] := 0;
         End;
     Soil.Struct[1, C] := SumStruct;
     Soil.Struct[1, N] := SumStructN;
     Soil.Metab[1, C] := SumMetab;
     Soil.Metab[1, N] := SumMetabN;
     Soil.CoarseWood[1, C] := SumCoarse;
     Soil.CoarseWood[1, N] := SumCoarseN;
     Soil.Active[1, C] := SumActive;
     Soil.Active[1, N] := SumActiveN;
     Soil.Slow[1, C] := SumSlow;
     Soil.Slow[1, N] := SumSlowN;
     Soil.Resistant[1, C] := SumResistant;
     Soil.Resistant[1, N] := SumResistantN;
     Soil.Soluble[1, N] := SumMineral;
     End;
If Control.DecayOnly then
     For E := C to P do
         Begin
         Litter.CoarseWood[E] := (Plant.SapWood[E] + Plant.HeartWood[E]);
         Litter.FineWood[E] := Plant.Branches[E];
         Litter.CoarseRoot[E] := Plant.CoarseRoot[E];
         Litter.FineRoot[E] := Plant.FineRoot[E];
         Litter.Leaves[E] := Plant.Leaves[E];
         Litter.WeedLeaves[E] := Plant.WeedLeaves[E];
         Litter.WeedRoots[E] := Plant.WeedRoots[E];
         Litter.Other[E] := 0;
         Derived.DecompLimit := 1;
         End;
If Control.Run_on then
     Begin
     If ((Control.RunType = Standard) or (Control.RunType = Geographic)) then // Normal run
        StartNewGraph := true
     Else if ((Control.RunType = BatchRun) and (OldScreen[D_Batch].First)) then // Normal batch run
        Begin
        StartNewGraph := true;
        OldScreen[D_Batch].First := false;
        End
     Else if ((Control.RunType = EquilRun) and OldScreen[D_Equil].First) then // Equilibrium run
        StartNewGraph := true
     Else if ((Control.RunType = Sensitivity) and (Control.SensParameter = Dummy)) then // Sensitivity test run
                                                                 // This should just stop the output window popping up
                                                                 // during sensitivity tests, but for some reason, it mucks up the whole running procedure
        StartNewGraph := true
     Else if ((Control.RunType = ParFit) and OldScreen[D_ParFit].First) then  // parameter fitting routine
        StartNewGraph := true
     Else
        StartNewGraph := false;
     if StartNewGraph then
        Begin
        If Control.SpatialMode then
           Begin
           Control.Display := 'Spatial representation';
           End
        Else if (Control.EquilMode or Control.Parfit.FittingMode) then
           Begin
           Control.Display := 'Iterations';
           if Control.EquilMode then
              MaxDisplay := Control.Equil.MaxIterations + 1
           Else
              MaxDisplay := Control.ParFit.MaxIterations + 1
           End
        Else if Control.MaxDays < DayDisplay then
           Begin
           Control.Display := 'Days';
           MaxDisplay := Control.MaxDays;
           End
        Else if Control.MaxDays < MonthDisplay then
           Begin
           Control.Display := 'Months';
           MaxDisplay := trunc(((Control.MaxDays - 1) / 30.4375) + 1);
           End
        Else
           Begin
           Control.Display := 'Years';
           MaxDisplay := trunc(((Control.MaxDays - 1) / 365.25) + 1);
           End;
{        // create a new graph
        frmGraph := TfrmGraph.Create(frmMain);
        // add a blank selected series at the front
        frmGraph.cmbSeries.Items.AddObject('None', pointer(-1));
        frmGraph.cmbSeries.ItemIndex := 0;
        // set the graph title
        frmGraph.chtGraph.Title.Visible := true;
        frmGraph.chtGraph.Title.Text.Text := 'Graph started ' + DateTimeToStr(Now);
        // set the width of the graph
        frmGraph.chtGraph.BottomAxis.SetMinMax(0, MaxDisplay);
 }  //JL commented
 End;
     End;
End; {of Procedure 'Initialise'}

Procedure PrintOut;
var ScreenItem: ScreenOptions;
    iSeriesCount: integer;
    DisplayX, Sum, Dummy: double;

{Procedure DrawPixel (Datum: real);
  var x, y, i, j: LongInt;
      series: TFastLineSeries;
  Begin
  Control.DisplayType := Pixel;
  series := TFastLineSeries(frmGraph.chtGraph.Series[1]);
  series.SeriesColor := ScreenRec.Color[Control.Spatial.Draw];
  x := frmMain.Left + trunc(frmMain.Width *
       Divide((Parameter.Longitude - Control.Spatial.LongMin),
      (Control.Spatial.LongMax - Control.Spatial.LongMin)));
  y := frmMain.Top + trunc(frmMain.Height *
       Divide((Parameter.Latitude - Control.Spatial.LatMin),
      (Control.Spatial.LatMax - Control.Spatial.LatMin)));
  For i := x to x + Control.Spatial.xInter do
      For j := y to y + Control.Spatial.yInter do
          series.AddXY(i, j);
  End; //of Procedure 'DrawPixel'
}{
  Procedure DrawItem (Datum, Multiplier: double);
  var y, y1, y2, y3, y4, SOMSum: double;
      iLayer: Integer;
      series: TFastLineSeries;
}{
      Procedure DrawNormalRunInfo;
      var series: TFastLineSeries;
      Begin
      Datum := Datum * Multiplier;
      If Datum > ScreenRec.UpRange[ScreenItem] then
         y := ScreenRec.UpRange[ScreenItem]
      Else if Datum < ScreenRec.LowRange[ScreenItem] then
         y := ScreenRec.LowRange[ScreenItem]
      Else
         y := Datum;
      // rescale it to between 0% and 100% of the range
      if (ScreenRec.UpRange[ScreenItem] <> ScreenRec.LowRange[ScreenItem]) then
         y := (y - ScreenRec.LowRange[ScreenItem]) /
              (ScreenRec.UpRange[ScreenItem] - ScreenRec.LowRange[ScreenItem])
      else
         y := 0;
      // is this the first point?
      If Control.MaxDays < DayDisplay then
         DisplayX := Control.TotalDays
      Else if Control.MaxDays < MonthDisplay then
         DisplayX := (Control.TotalDays / 30.4375)
      Else
         DisplayX := (Control.TotalDays / 365.25);
      If OldScreen[ScreenItem].First then
         Begin
         // yes, so create a new series
         series := TFastLineSeries.Create(frmGraph.chtGraph);
         series.ColorEachPoint := false;
         series.Title := ScreenVariableNames[ScreenItem];
         series.SeriesColor := ScreenRec.Color[ScreenItem];
         series.AddXY(DisplayX, y);
         frmGraph.chtGraph.AddSeries(series);
         frmGraph.cmbSeries.Items.AddObject(series.Title, pointer(ScreenItem));
         OldScreen[ScreenItem].First := False;
         End
      Else
         Begin
         series := TFastLineSeries(frmGraph.chtGraph.Series[iSeriesCount + 1]);
         series.AddXY(DisplayX, y);
         End;
      Inc(iSeriesCount);
      End; //of Procedure 'DrawNormalRunInfo'
}{
      Procedure DrawEquilModeInfo;
      var iLayer: Integer;
      Begin
      DisplayX := Derived.Equil.Iterations + 1;
      If Control.Equil.EquilTarget = SOM then
         Begin
         SOMSum := 0;
         For iLayer := 0 to Soil.nLayers do
             SOMSum := SOMSum + Soil.Slow[iLayer, C] + Soil.Resistant[iLayer, C] +
                       Soil.Active[iLayer, C];
         y := 0.001 * Divide (SOMSum, Control.Equil.TargetValue * 2);
         End
      Else if Control.Equil.EquilTarget = LeafNConc then
          y := 1000 * Divide(Divide (Plant.Leaves[N], Plant.Leaves[C] * Control.CConversion), Control.Equil.TargetValue * 2)
      Else if Control.Equil.EquilTarget = LeafNitrogen then
          y := Divide(Plant.Leaves[N], Control.Equil.TargetValue * 2)
      Else if Control.Equil.EquilTarget = Leafmass then
         y := 0.001 * Divide(Plant.Leaves[C] * Control.CConversion, Control.Equil.TargetValue * 2)
      Else //if Control.Equil.EquilTarget = WoodMass then
          y := 0.001 * Divide((Plant.SapWood[C] + Plant.HeartWood[C]) * Control.CConversion, Control.Equil.TargetValue * 2);
      Sum := 0;
      For iLayer := 0 to Soil.nLayers do
          Sum := Sum + Soil.Slow[iLayer, C];
      if (ScreenRec.UpRange[D_CSlow] <> ScreenRec.LowRange[D_CSlow]) then
         y1 := ((Sum / 1000) - ScreenRec.LowRange[D_CSlow]) /
               (ScreenRec.UpRange[D_CSlow] - ScreenRec.LowRange[D_CSlow])
      else
         y1 := 0.5;
      Sum := 0;
      For iLayer := 0 to Soil.nLayers do
          Sum := Sum + Soil.Resistant[iLayer, C];
      if (ScreenRec.UpRange[D_CResistant] <> ScreenRec.LowRange[D_CResistant]) then
          y2 := ((Sum / 1000) - ScreenRec.LowRange[D_CResistant]) /
                (ScreenRec.UpRange[D_CResistant] - ScreenRec.LowRange[D_CResistant])
      else
         y2 := 0.5;
      if (ScreenRec.UpRange[D_NSum] <> ScreenRec.LowRange[D_NSum]) then
         y3 := ((TotalN) - ScreenRec.LowRange[D_NSum]) /
               (ScreenRec.UpRange[D_NSum] - ScreenRec.LowRange[D_NSum])
      else
         y3 := 0.5;
      If (Control.IncludeP and (ScreenRec.UpRange[D_PSum] <> ScreenRec.LowRange[D_PSum])) then
         y4 := ((TotalP) - ScreenRec.LowRange[D_PSum]) /
               (ScreenRec.UpRange[D_PSum] - ScreenRec.LowRange[D_PSum])
      else
         y4 := 0.5;
      If OldScreen[D_Equil].First then
         Begin
         // create a new series
         series := TFastLineSeries.Create(frmGraph.chtGraph);
         series.ColorEachPoint := false;
         series.Title := 'Equil search';
         series.SeriesColor := Color.Red;
         series.AddXY(DisplayX, y);
         // add the series to the graph
         frmGraph.chtGraph.AddSeries(series);
         // add the series to the combobox too, recording which screen item is being graphed
         frmGraph.cmbSeries.Items.AddObject(series.Title, pointer(D_Equil));
         // reset the flag saying we are at the first point
         series := TFastLineSeries.Create(frmGraph.chtGraph);
         series.ColorEachPoint := false;
         series.Title := 'Slow SOM';
         series.SeriesColor := ScreenRec.Color[D_CSlow];
         series.AddXY(DisplayX, y1);
         frmGraph.chtGraph.AddSeries(series);
         frmGraph.cmbSeries.Items.AddObject(series.Title, pointer(D_Equil));
         series := TFastLineSeries.Create(frmGraph.chtGraph);
         series.ColorEachPoint := false;
         series.Title := 'Resistant';
         series.SeriesColor := ScreenRec.Color[D_CResistant];
         series.AddXY(DisplayX, y2);
         frmGraph.chtGraph.AddSeries(series);
         frmGraph.cmbSeries.Items.AddObject(series.Title, pointer(D_Equil));
         series := TFastLineSeries.Create(frmGraph.chtGraph);
         series.ColorEachPoint := false;
         series.Title := 'N Sum';
         series.SeriesColor := ScreenRec.Color[D_NSum];
         series.AddXY(DisplayX, y3);
         frmGraph.chtGraph.AddSeries(series);
         if Control.IncludeP then
            Begin
            frmGraph.cmbSeries.Items.AddObject(series.Title, pointer(D_Equil));
            series := TFastLineSeries.Create(frmGraph.chtGraph);
            series.ColorEachPoint := false;
            series.Title := 'PSum';
            series.SeriesColor := ScreenRec.Color[D_PSum];
            series.AddXY(DisplayX, y4);
            frmGraph.chtGraph.AddSeries(series);
            End;
         // add the series to the combobox too, recording which screen item is being graphed
         frmGraph.cmbSeries.Items.AddObject(series.Title, pointer(D_Equil));
         OldScreen[D_Equil].First := False;
         End
      Else
         Begin
         series := TFastLineSeries(frmGraph.chtGraph.Series[1]);
         series.AddXY(DisplayX, y);
         series := TFastLineSeries(frmGraph.chtGraph.Series[2]);
         series.AddXY(DisplayX, y1);
         series := TFastLineSeries(frmGraph.chtGraph.Series[3]);
         series.AddXY(DisplayX, y2);
         series := TFastLineSeries(frmGraph.chtGraph.Series[4]);
         series.AddXY(DisplayX, y3);
         if Control.IncludeP then
            Begin
            series := TFastLineSeries(frmGraph.chtGraph.Series[5]);
            series.AddXY(DisplayX, y4);
            End;
         End; //635
      Inc(iSeriesCount);
      End; //of Procedure 'DrawEquilModeInfo'
}{
     Procedure DrawParameterFitModeInfo;
      var iCount: Integer;
          Min, Max: double;
      Begin
      DisplayX := Control.ParFit.Iterations;
      y := 0.9 * Divide(FitParameter.OldSumOfSquares, FitParameter.HistoricFit[0]);
      if y < 0 then
         y := 0
      Else if y > 0.99 then
         y := 0.99;
      If OldScreen[D_ParFit].First then
         Begin
         // create a new series
         series := TFastLineSeries.Create(frmGraph.chtGraph);
         series.ColorEachPoint := false;
         series.Title := 'Parameter fitting';
         series.SeriesColor := Color.Black;
         series.AddXY(DisplayX, y);
         // add the series to the graph
         frmGraph.chtGraph.AddSeries(series);
         // add the series to the combobox too, recording which screen item is being graphed
         frmGraph.cmbSeries.Items.AddObject(series.Title, pointer(D_ParFit));
         // reset the flag saying we are at the first point
         series := TFastLineSeries.Create(frmGraph.chtGraph);
         series.ColorEachPoint := false;
         // add the series to the combobox too, recording which screen item is being graphed
         frmGraph.cmbSeries.Items.AddObject(series.Title, pointer(D_ParFit));
         OldScreen[D_ParFit].First := False;
         End
      Else
         Begin
         series := TFastLineSeries(frmGraph.chtGraph.Series[1]);
         series.AddXY(DisplayX, y);
         End;
      Inc(iSeriesCount);
      End; //of Procedure 'DrawParameterFitModeInfo'
} //JL commented
{
  Begin
  If Control.EquilMode and (not Control.Run_On) then
  //   DrawEquilModeInfo
  Else if ((not Control.EquilMode) and (not Control.Parfit.FittingMode)) then
  //   DrawNormalRunInfo
  Else if Control.Parfit.FittingMode then
     Begin
     if Control.ParFit.Iterations <> Control.ParFit.IterationsDisplayed then
        Begin
        Control.ParFit.IterationsDisplayed := Control.ParFit.Iterations;
 //       DrawParameterFitModeInfo;
        End;
     End;
  End; //of Procedure 'DrawItem'

  Procedure DrawSum(FirstLayer, LastLayer: Integer; Item: SoilElements; E: ElementsUsed; Multiplier: double);
  var Sum: double;
      iLayer: Integer;
  Begin
  Sum := 0;
  For iLayer := FirstLayer to LastLayer do
      Sum := Sum + Item[iLayer, E];
  //DrawItem (Sum, Multiplier);
  End; //of Procedure 'DrawSum'

  Procedure DrawSum2(FirstLayer, LastLayer: Integer; Item1, Item2: SoilElements; E: ElementsUsed; Multiplier: double);
  var Sum: double;
      iLayer: Integer;
  Begin
  Sum := 0;
  For iLayer := FirstLayer to LastLayer do
      Sum := Sum + Item1[iLayer, E] + Item2[iLayer, E];
  //DrawItem (Sum, Multiplier);
  End; //of Procedure 'DrawSum2'
  }

Begin
// j1 :=0;
Control.StartRun := false;
Control.TotalDays := Control.TotalDays + 1;
if (Control.TotalDays > Control.MaxDays) then
   Control.Run_on := false;
Control.DaysSince := Control.DaysSince + 1;
Control.DaysSinceDisk := Control.DaysSinceDisk + 1;
//if Control.ParFit.FittingMode then
//   AddToSumOfSquares;
If (((Control.DaysSinceDisk > Control.DiskInterval)) or (not Control.Run_On)) and
   ((not Control.EquilMode) and (not Control.SensitivityTestOn) and Control.DataToBeSaved) then
   Begin
   If not Control.BatchMode or (Control.TotalDays > Control.StartBatchSave) then
      SaveRegular;
   Control.DaysSinceDisk := Control.DaysSinceDisk - Control.DiskInterval;
   End;
CalcDate (Control.Date);
// Check if we should update the graph now.
// Update it on the first day, the last day (when the run finishes) and
// any day in between based on the update rate from the progress bar
//   DrawItem (Dummy, Dummy)
//Else if Control.SpatialMode and (not Control.Run_on) then
//   DrawPixel (Dummy)
//Else if (((Control.TotalDays = 1) or (not Control.Run_On) or
//        ((Control.DaysSince >= frmProgress.iUpdateRate) and
//         frmProgress.bUpdate)) and
//         ((not Control.SensitivityTestOn) and (not Control.BatchMode) and (not Control.Parfit.FittingMode))) then
{      Begin
      Control.DaysSince := Control.DaysSince - frmProgress.iUpdateRate;
      If Control.DaysSince > (frmProgress.iUpdateRate - 1) then
         Control.DaysSince := frmProgress.iUpdateRate - 1;
      iSeriesCount := 0;
      For ScreenItem := D_CarbonGain to D_Dummy do
        Begin
        If ScreenRec.Choose[ScreenItem] then
          Begin
          Case ScreenItem of
          D_CarbonGain:   DrawItem (Derived.CarbonGain, Control.CConversion);
          D_CAI:          DrawItem (Derived.CAI, Control.CConversion * 0.001);
          D_NPP:          DrawItem (Derived.NPP, Control.CConversion * 0.001);
          D_NEE:          DrawItem (Derived.NEE, Control.CConversion);
          D_Respn:        DrawItem (Derived.Rm + Derived.Rg, Control.CConversion);
          D_DayCFlux:     DrawItem (Derived.DayCFlux, Control.CConversion);
          D_NightCFlux:   DrawItem (Derived.NightCFlux, Control.CConversion);
          D_SolubleCH2O:  DrawItem (Plant.Soluble[C], Control.CConversion * 0.001);
          D_LAI:          DrawItem (Derived.LAI[Over], 1);
          D_pi:           DrawItem (Derived.p_internal, 1);
          D_Wood:         DrawItem (Plant.SapWood[C] + Plant.HeartWood[C], Control.CConversion * 0.001);
          D_SapW:         DrawItem (Plant.SapWood[C], Control.CConversion * 0.001);
          D_HeartW:       DrawItem (Plant.HeartWood[C], Control.CConversion * 0.001);
          D_Bark:         DrawItem (Plant.Bark[C], Control.CConversion * 0.001);
          D_Reserves:     DrawItem (Plant.Reserves[C], Control.CConversion * 0.001);
          D_Leaf:         DrawItem (Plant.Leaves[C], Control.CConversion * 0.001);
          D_FineRoot:     DrawItem (Plant.FineRoot[C], Control.CConversion * 0.001);
          D_CoarseRoot:   DrawItem (Plant.CoarseRoot[C], Control.CConversion * 0.001);
          D_Branches:     DrawItem (Plant.Branches[C], Control.CConversion * 0.001);
          D_Reprod:       DrawItem (Plant.Fruit[C] + Plant.Pollen[C], Control.CConversion * 0.001);
          D_WeedLeaves:   DrawItem (Plant.WeedLeaves[C], Control.CConversion * 0.001);
          D_WeedHeight:   DrawItem (Plant.WeedHeight, 1);
          D_Height:       DrawItem (Plant.Height, 1);
          D_DBH:          DrawItem (Plant.DBH, 1);
          D_CanopyCover:  DrawItem (Plant.CanopyCover, 1);
          D_kex:          DrawItem (Plant.kex, 1);
          D_BasalArea:    DrawItem (Plant.Area, 0.0001);
          D_Stocking:     DrawItem (Plant.Stocking, 1);
          D_TDamage:      DrawItem (Derived.TDamageUnits, 1);
          D_Nlimit:       DrawItem (Derived.Nlimit, 1);
          D_NBiol:        DrawItem (Derived.NBiol, 1);
          D_NSum:         DrawItem (TotalN, Control.NConversion);
          D_NConc:        DrawItem (Divide(Plant.Leaves[N], Plant.Leaves[C]), 1000 * Control.NConversion / Control.CConversion);
          D_NConc1:       DrawItem (Derived.NConc[Over], 1000 * Control.NConversion / Control.CConversion);
          D_SolubleN:     DrawItem (Plant.Soluble[N], Control.NConversion);
          D_WoodN:        DrawItem (Plant.SapWood[N] + Plant.HeartWood[N], Control.NConversion);
          D_SapWN:        DrawItem (Plant.SapWood[N], Control.NConversion);
          D_HeartWN:      DrawItem (Plant.HeartWood[N], Control.NConversion);
          D_BarkN:        DrawItem (Plant.Bark[N], Control.NConversion);
          D_ReservesN:    DrawItem (Plant.Reserves[N], Control.NConversion);
          D_LeafN:        DrawItem (Plant.Leaves[N], Control.NConversion);
          D_FineRootN:    DrawItem (Plant.FineRoot[N], Control.NConversion);
          D_CoarseRootN:  DrawItem (Plant.CoarseRoot[N], Control.NConversion);
          D_BranchN:      DrawItem (Plant.Branches[N], Control.NConversion);
          D_ReprodN:      DrawItem (Plant.Fruit[N] + Plant.Pollen[N], Control.NConversion);
          D_CLeafLitter:  DrawItem (Litter.Leaves[C], Control.CConversion);
          D_CAll_Litter:  DrawItem (Litter.Leaves[C] + Litter.CoarseWood[C] + Litter.FineWood[C]
                                    + Litter.Other[C] + Litter.CoarseRoot[C] + Litter.FineRoot[C], Control.CConversion);
          D_CAg_Litter:   DrawItem (Litter.Leaves[C] + Litter.CoarseWood[C] + Litter.FineWood[C]
                                    + Litter.Other[C], Control.CConversion);
          D_CMetabSurf:   DrawItem (Soil.Metab[0, C], 0.001);
          D_CMetabSoil:   DrawSum (1, Soil.nLayers, Soil.Metab, C, 0.001);
          D_CStructSurf:  DrawItem (Soil.Struct[0, C], 0.001);
          D_CStructSoil:  DrawSum (1, Soil.nLayers, Soil.Struct, C, 0.001);
          D_CWoodyLitter: DrawSum2 (0, Soil.nLayers, Soil.CoarseWood, Soil.FineWood, C, 0.001);
          D_CActive:      DrawSum (1, Soil.nLayers, Soil.Active, C, 0.001);
          D_CSlow:        DrawSum (1, Soil.nLayers, Soil.Slow, C, 0.001);
          D_CResistant:   DrawSum (1, Soil.nLayers, Soil.Resistant, C, 0.001);
          D_CInert:       DrawSum (1, Soil.nLayers, Soil.Inert, C, 0.001);
          D_NMetabSurf:   DrawItem (Soil.Metab[0, N], 1);
          D_NMetabSoil:   DrawSum (1, Soil.nLayers, Soil.Metab, N, 1);
          D_NStructSurf:  DrawItem (Soil.Struct[0, N], 1);
          D_NStructSoil:  DrawSum (1, Soil.nLayers, Soil.Struct, N, 1);
          D_NWoodyLitter: DrawSum2 (0, Soil.nLayers, Soil.CoarseWood, Soil.FineWood, N, 1);
          D_NActive:      DrawSum (0, Soil.nLayers, Soil.Active, N, 1);
          D_NSlow:        DrawSum (0, Soil.nLayers, Soil.Slow, N, 1);
          D_NResistant:   DrawSum (0, Soil.nLayers, Soil.Resistant, N, 1);
          D_NInert:       DrawSum (0, Soil.nLayers, Soil.Inert, N, 1);
          D_NMineral:     DrawItem (Derived.NMineralised, 1);
          D_NLeached:     DrawItem (Derived.NLeached, 1);
          D_NLeafLitter:  DrawItem (Litter.Leaves[N], Control.NConversion);
          D_NAll_Litter:  DrawItem (Litter.Leaves[N] + Litter.CoarseWood[N] + Litter.FineWood[N]
                                    + Litter.Other[N] + Litter.CoarseRoot[N] + Litter.FineRoot[N], Control.NConversion);
          D_NAg_Litter:   DrawItem (Litter.Leaves[N] + Litter.CoarseWood[N] + Litter.FineWood[N]
                                    + Litter.Other[N], Control.NConversion);
          D_SoilRespn:    DrawItem (Derived.SoilRespn, Control.CConversion);
          D_Tmax:         DrawItem (Weather.Tmax, 1);
          D_Tmin:         DrawItem (Weather.Tmin, 1);
          D_Tmean:        DrawItem (Weather.Tmean, 1);
          D_Tsoil:        DrawItem (Weather.Tsoil, 1);
          D_Tday:         DrawItem (Weather.Tday, 1);
          D_Radn:         DrawItem (Weather.Radn, 1);
          D_CO2:          DrawItem (Weather.CO2, 1);
          D_Rain:         DrawItem (Weather.Rain, 1);
          D_StoredWater:  DrawItem (Soil.TotalWater, 1);
          D_Snow:         DrawItem (Soil.Snow, 1);
          D_AbsHum:       DrawItem (Weather.AbsHumidity, 1000);
          D_RelHum:       DrawItem (Weather.RelHumidity, 100);
          D_WaterLimit:   DrawItem (Derived.WaterLimit, 1);
          D_HeatSum:      DrawItem (Derived.HeatSum, 1);
          D_Transpiration:DrawItem (Derived.Transpiration, 1);
          D_Evaporation:  DrawItem (Derived.Evaporation, 1);
          D_Drainage:     DrawItem (Derived.Drainage[Soil.nLayers], 1);
          D_Dummy:        DrawItem (Derived.Dummy, 1);
          End;
          If Control.IncludeP then
             Case ScreenItem of
                D_PConc:        DrawItem (Derived.PConc[Over], 1000 / Control.CConversion);
                D_Puptake:      DrawItem (Derived.PUptake, 1);
                D_Plimit:       DrawItem (Derived.Plimit, 1);
                D_PSum:         DrawItem (TotalP, 1);
                D_PSumOrganic:  DrawItem (TotalOrganicP, 1);
                D_PLeafLitter:  DrawItem (Litter.Leaves[P], 1);
                D_PAll_Litter:  DrawItem (Litter.Leaves[P] + Litter.CoarseWood[P] + Litter.FineWood[P]
                                    + Litter.Other[P] + Litter.CoarseRoot[P] + Litter.FineRoot[P], 1);
                D_PAg_Litter:   DrawItem (Litter.Leaves[P] + Litter.CoarseWood[P] + Litter.FineWood[P]
                                    + Litter.Other[P], 1);
                D_PMetabSurf:   DrawItem (Soil.Metab[0, P], 1);
                D_PMetabSoil:   DrawSum (1, Soil.nLayers, Soil.Metab, P, 1);
                D_PStructSurf:  DrawItem (Soil.Struct[0, P], 1);
                D_PStructSoil:  DrawSum (1, Soil.nLayers, Soil.Struct, P, 1);
                D_PWoodyLitter: DrawSum2 (0, Soil.nLayers, Soil.CoarseWood, Soil.FineWood, P, 1);
                D_PActive:      DrawSum (0, Soil.nLayers, Soil.Active, P, 1);
                D_PSlow:        DrawSum (0, Soil.nLayers, Soil.Slow, P, 1);
                D_PResistant:   DrawSum (0, Soil.nLayers, Soil.Resistant, P, 1);
                D_PInert:       DrawSum (0, Soil.nLayers, Soil.Inert, P, 1);
                D_PMineral:     DrawItem (Derived.PMineralised, 1);
                D_PRock:        DrawSum (0, Soil.nLayers, Soil.RockP, P, 1);
                D_POccluded:    DrawSum (0, Soil.nLayers, Soil.OccludedP, P, 1);
                D_2ndaryP:      DrawSum (0, Soil.nLayers, Soil.SecondaryInorganicP, P, 1);
}
//                End; // Case statement
//          End;       // If ScreenRec.Choose[ScreenItem] then KJ already 859
//        End;         // For ScreenItem := D_CarbonGain to D_Dummy do
//      End;           // Else if (((Control.TotalDays = 1) ..... then
End;  {of Procedure 'PrintOut'}




Procedure EndRun;
Begin
If Control.OutputFileOpen and ((not Control.BatchMode) or Control.EndBatch) and
   (not Control.EquilMode) and (not Control.SpatialMode) and (not Control.SensitivityTestOn) and (not Control.Parfit.FittingMode) then
   Begin
   Close (Control.CenWFileOut);
   Control.OutputFileOpen := false;
   End;
If Control.ClimFileOpen then
   Begin
   Close (Control.DefClim);
   Control.ClimFileOpen := false;
   End;
If Control.BatchFileOpen and (not Control.BatchMode) and (not Control.Parfit.FittingMode) then
   Begin
   Close (Control.BatchName);
   Control.BatchFileOpen := false;
   End;
if Control.UseSimulated and (not Control.BatchMode) then  // use simulated climate only for that one run
   Control.ClimType := 'O';
End; {of Procedure 'TCenWWindow.EndRun'}

Procedure ControlRun;
Begin
 Initialise;
//End;  //KJ
//End.  //KJ

{

if not Control.SpatialMode then
   Initialise;
case Control.RunType of
     EquilRun: frmEquilProgress.Start;
     ParFit: frmParFitProgress.Start;
Else
     frmProgress.Start;
end;
//  try     //This belongs with the 'finally' below
    While Control.Run_on do
      Begin
      InitialiseIteration;
      GetWeather;
      If Control.DecayOnly then
        NutrientGain
      Else //If not Control.DecayOnly then
        Begin
        EventController;
        CalcPhenology;
        SetPestDamage;
        WaterIn;
        CarbonGain;
        WaterOut;
        NutrientGain;
        CarbonLoss;
        NitrogenLoss;
        if Control.IncludeP then
           PhosphorusLoss;
        HarvestorFireLoss;
        Allocation;
        End;
      PrintOut;
      End;
      Application.ProcessMessages;
      case Control.RunType of
          EquilRun: ;  // Do nothing yet
          ParFit: Begin
                  if frmParFitProgress.bStopped or (frmGraph = nil) then
                     break
                  Else
                     frmParFitProgress.Update;
                  End;
          Else
                  Begin
                  if frmProgress.bStopped or (frmGraph = nil) then
                     break
                  Else
                     frmProgress.Update;
                  End;
      end;
      if (Control.EquilMode{ or Control.Parfit.FittingMode) then    //deleted brace after fitting mode
         //do nothing yet
      Else if(frmProgress.bStopped) or (frmGraph = Nil) then
         break
      else
         frmprogress.Update;
    end; //of 'while Control.Run_on do'
    }
//    EndRun;
  {finally
    // make sure the progress window closes
    case Control.RunType of
         EquilRun: frmEquilProgress.Stop;
         ParFit: frmParFitProgress.Stop;
    Else
         frmProgress.Stop;
    end;
  end;  !!End try
}
}
End;  //of Procedure 'ControlRun'

End.  //Have unexpected EOF problem here! JL