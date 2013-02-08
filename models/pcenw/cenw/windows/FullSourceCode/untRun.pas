{ ================================================================
  = Project   : CENW                                             =
  ================================================================
  = Modules   : EndRun                                           =
  =             ControlRun                                       =
  =             Initialise                                       =
  =                                                              =
  =             Routines to control the running of the program   =
  ================================================================
  = File      : untRun.PAS                                       =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

unit untRun;

interface

Procedure EndRun;
Procedure ControlRun;
Procedure Initialise;

implementation

uses
  Forms, SysUtils, Windows, Math, untDeclarations, untSimulate, untSimSoil,
  untFileIO, untFileIO2, untDiskOut, untMiscellaneous, untDivideValidation,
  untGraph, untMain, TeEngine, Series, untProgress, untEquilProgress, untNotices;

Procedure Initialise;
Var
  E: ElementsUsed;
  Days: 1..365;
  Years: 0..MaxSapWoodYears;
  s : array[0..255] of char;
  st : string[255];
  ScreenVar : ScreenOptions;
  i, iLayer, ErrorNo, MaxDisplay: integer;
  SumStruct, SumStructN, SumMetab, SumMetabN, SumFine, SumCoarse, SumCoarseN, SumActive,
  SumActiveN, SumSlow, SumSlowN, SumResistant, SumResistantN, SumMineral: Real48;

    Function f1(theta: real48): real48; {See Sands, 1995; these are needed for photosynthesis calculations}
    Const a1 = 0.22; b1 = 0.74;
    Begin
    f1 := 1 + a1 * theta * (1 - theta) + b1 * sqr(theta) * sqr(1 - theta);
    End; {of Function 'f1'}

    Function f2(theta: real48): real48;  {See Sands, 1995}
    Const  a2 = -0.18; b2 = 0.5;
    Begin
    f2 := a2 * theta + b2 * sqr(theta) + (1 - a2 - b2) * Power(theta, 3);
    End; {of Function 'f2'}

    Procedure CalcHt_Diameter;
    {The Ht_Diameter parameter is completely prescribed by the other parameters and therefore
     should not be set by the user. It does not need to be recalculated at each iteration, either.
     So, the calculation is done here once at the beginning of each run.}
    var Height, BasalDiam: Real48;
    Begin
    If Parameter.Mindbh > 1 then
       Begin
       Height := exp(Parameter.HDInter + Parameter.HDSlope * ln(Parameter.Mindbh));
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
    End; {of Procedure 'CalcHt_Diameter'}

Begin
  Derived.Functionf1 := f1(Parameter.Theta);
  Derived.Functionf2 := f2(Parameter.Theta);
  CalcHt_Diameter;
  If Control.AllOneLayer then // just check that the litter input amounts are sensible. Can be a problem with reading old files
     Begin
     If SoilOrganic.FineRootLitterIn[1] <> 1 then
        SoilOrganic.FineRootLitterIn[1] := 1;
     If SoilOrganic.CoarseRootLitterIn[1] <> 1 then
        SoilOrganic.CoarseRootLitterIn[1] := 1;
     End
  Else
     Begin
     SumFine := 0;  SumCoarse := 0;
     For iLayer := 1 to SoilOrganic.nLayers do
         Begin
         SumFine := SumFine + SoilOrganic.FineRootLitterIn[iLayer];
         SumCoarse := SumCoarse + SoilOrganic.CoarseRootLitterIn[iLayer];
         End;
     If SumFine = 0 then SumFine := 1;
     If SumCoarse = 0 then SumCoarse := 1;
     For iLayer := 1 to SoilOrganic.nLayers do
         Begin
         SoilOrganic.FineRootLitterIn[iLayer] := SoilOrganic.FineRootLitterIn[iLayer] / SumFine;
         SoilOrganic.CoarseRootLitterIn[iLayer] := SoilOrganic.CoarseRootLitterIn[iLayer] / SumCoarse;
         End;
     End;
  If not Control.EquilMode then
     SaveAnyInfo
  Else
     Control.DataToBeSaved := false;
  CalcDate (Control.Date);
  Control.Run_on := true;
  If Control.DataToBeSaved and not Control.OutputFileOpen then
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
           Application.MessageBox(s, 'Output file already open!', MB_OK);
           End
        Else
           Begin
           st := '    Change the output filename' + chr(10) +
                 '           under menu option' + chr(10) +
                 '           PARAMETERS' + chr(10) +
                 '           CONTROL...' + chr(10);
           strpcopy(s,st);
           Application.MessageBox(s, 'Incorrect output file name selected!', MB_OK);
           End;
        Control.Run_on := false;
        end
     else
        Begin
        SaveAtStart (Control.FileOut);
        Control.OutputFileOpen := true;
        End;
     End;
  For ScreenVar := D_CarbonGain to D_Dummy do
      OldScreen[ScreenVar].First := true;
  Control.StartRun := true;
  Control.IrrigAnnounced := false;
  Control.ClimFileOpen := false;
  Control.NewYear := true;
  Control.DaysSince := 0;
  Control.TotalDays := 0;
  Derived.NPP := 0;
  If Control.NextPhenology < 1 then
     Control.NextPhenology := 1
  Else if Control.NextPhenology > Parameter.Phenology.nChanges then
     Control.NextPhenology := 1;
  If Control.PhenologyDayCount < 0 then
     Control.PhenologyDayCount := 0;
  DecompCount := 0;
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
  Control.NextPest := 1;
  Control.NextFire := 1;
  Control.NextPlough := 1;
  Control.NextEnvironment := 1;
  Control.PestMode := false;
  Derived.PestMortality := 0;
  Derived.PestLeafDamage := 0;
  Derived.PestSolubleDamage := 0;
  Derived.PestSenesc := 0;
  Derived.PestPhsFrac := 0;
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
      If Control.Fertilise_DateType = 'D' then
         Event.FertiliseTimes[i,4] := trunc(365.25 * (Event.FertiliseTimes[i,3] - Control.TotalYears)
                                    + 30.4375 * (Event.FertiliseTimes[i,2] - Control.ExtraMonths)
                                    + Event.FertiliseTimes[i,1] - Control.ExtraDays - 1)
      else
         Event.FertiliseTimes[i,4] := trunc(365.25 * Event.FertiliseTimes[i,3]
                                          + 30.4375 * Event.FertiliseTimes[i,2]
                                                    + Event.FertiliseTimes[i,1]);
  Event.FertiliserAdded := 0;
  For i := 1 to Event.nHarvests do
    If (Event.HarvestTimes[i,1] <> 0) or (Event.HarvestTimes[i,2] <> 0) or (Event.HarvestTimes[i,3] <> 0) then
      If Control.Harvest_DateType = 'D' then
         Event.HarvestTimes[i,4] := trunc(365.25 * (Event.HarvestTimes[i,3] - Control.TotalYears)
                                    + 30.4375 * (Event.HarvestTimes[i,2] - Control.ExtraMonths)
                                    + Event.HarvestTimes[i,1] - Control.ExtraDays - 1)
      else
         Event.HarvestTimes[i,4] := trunc(365.25 * Event.HarvestTimes[i,3]
                                          + 30.4375 * Event.HarvestTimes[i,2]
                                                    + Event.HarvestTimes[i,1]);
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
    If (i > 1) and (Event.PestTimes[i - 1, 5] > 0) then
       If Event.PestTimes[i, 4] > (Event.PestTimes[i - 1, 4] + Event.PestTimes[i - 1, 5]) then
          Event.PestTimes[i, 4] := Event.PestTimes[i - 1, 4] + Event.PestTimes[i - 1, 5];
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
  LastYearsWood[0]:= Plant.SapWood[C] + Plant.HeartWood[C];
  If Plant.Age < Parameter.SapWoodYears then
     Begin
     For Years := 1 to Plant.Age do
         SapWoodAmount[Years] := Plant.SapWood[C] / Plant.Age;
     For Years := Plant.Age to Parameter.SapWoodYears do
         SapWoodAmount[Years] := 0;
     End
  Else
     For Years := 1 to Parameter.SapWoodYears do
         SapWoodAmount[Years] := Plant.SapWood[C] / Parameter.SapWoodYears;
  For Days := 1 to 365 do
      LastYearsWood[Days] := 0;
  Control.Count := 0;
  Weather.ExtraCO2 := 0;
  Weather.ExtraTemp := 0;
  Weather.ExtraRain := 1;
  SoilWat.MaxWater := 0;
  If Control.NextPhenology > Parameter.Phenology.nChanges then
     Control.NextPhenology := 1; // In case the phenology set up has changed and we are now outside the range of permissable values
  Derived.PestPhsFrac := 1;
  If Derived.RespnBase < 0 then // extra safeguard in case they have not been properly
     Derived.RespnBase := 0     // initialised in a previous run
  Else if Derived.RespnBase > 10 then
     Derived.RespnBase := 10;
  If Parameter.RespnAdjust < 1 then
     Parameter.RespnAdjust := 1;
  For iLayer := 1 to SoilWat.nLayers do
      SoilWat.MaxWater := SoilWat.MaxWater + SoilWat.Layer[iLayer].MaxWater;
  If Control.AllOneLayer then {amalgamate all soil layers into the top layer}
     Begin
     SumStruct := 0;     SumStructN := 0;
     SumMetab := 0;      SumMetabN := 0;
     SumCoarse := 0;     SumCoarseN := 0;
     SumActive := 0;     SumActiveN := 0;
     SumSlow := 0;       SumSlowN := 0;
     SumResistant := 0;  SumResistantN := 0;
     SumMineral := 0;
     For iLayer := 1 to SoilOrganic.nLayers do
         Begin
         SumStruct := SumStruct + SoilOrganic.Struct[iLayer, C];
         SoilOrganic.Struct[iLayer, C] := 0;
         SumStructN := SumStructN + SoilOrganic.Struct[iLayer, N];
         SoilOrganic.Struct[iLayer, N] := 0;
         SumMetab := SumMetab + SoilOrganic.Metab[iLayer, C];
         SoilOrganic.Metab[iLayer, C] := 0;
         SumMetabN := SumMetabN + SoilOrganic.Metab[iLayer, N];
         SoilOrganic.Metab[iLayer, N] := 0;
         SumCoarse := SumCoarse + SoilOrganic.CoarseWood[iLayer, C];
         SoilOrganic.CoarseWood[iLayer, C] := 0;
         SumCoarseN := SumCoarseN + SoilOrganic.CoarseWood[iLayer, N];
         SoilOrganic.CoarseWood[iLayer, N] := 0;
         SumActive := SumActive + SoilOrganic.Active[iLayer, C];
         SoilOrganic.Active[iLayer, C] := 0;
         SumActiveN := SumActiveN + SoilOrganic.Active[iLayer, N];
         SoilOrganic.Active[iLayer, N] := 0;
         SumSlow := SumSlow + SoilOrganic.Slow[iLayer, C];
         SoilOrganic.Slow[iLayer, C] := 0;
         SumSlowN := SumSlowN + SoilOrganic.Slow[iLayer, N];
         SoilOrganic.Slow[iLayer, N] := 0;
         SumResistant := SumResistant + SoilOrganic.Resistant[iLayer, C];
         SoilOrganic.Resistant[iLayer, C] := 0;
         SumResistantN := SumResistantN + SoilOrganic.Resistant[iLayer, N];
         SoilOrganic.Resistant[iLayer, N] := 0;
         SumMineral := SumMineral + SoilOrganic.Soluble[iLayer, N];
         SoilOrganic.Soluble[iLayer, N] := 0;
         End;
     SoilOrganic.Struct[1, C] := SumStruct;
     SoilOrganic.Struct[1, N] := SumStructN;
     SoilOrganic.Metab[1, C] := SumMetab;
     SoilOrganic.Metab[1, N] := SumMetabN;
     SoilOrganic.CoarseWood[1, C] := SumCoarse;
     SoilOrganic.CoarseWood[1, N] := SumCoarseN;
     SoilOrganic.Active[1, C] := SumActive;
     SoilOrganic.Active[1, N] := SumActiveN;
     SoilOrganic.Slow[1, C] := SumSlow;
     SoilOrganic.Slow[1, N] := SumSlowN;
     SoilOrganic.Resistant[1, C] := SumResistant;
     SoilOrganic.Resistant[1, N] := SumResistantN;
     SoilOrganic.Soluble[1, N] := SumMineral;
     End;
  If Control.DecayOnly then
     For E := C to P do
         Begin
         Litter.CoarseWood[E] := (Plant.SapWood[E] + Plant.HeartWood[E]);
         Litter.FineWood[E] := Plant.Branches[E];
         Litter.CoarseRoot[E] := Plant.CoarseRoot[E];
         Litter.FineRoot[E] := Plant.FineRoot[E];
         Litter.Leaves[E] := 0;
         Litter.Leaves[E] := Litter.Leaves[E] + Plant.Leaves[E];
         Litter.Other[E] := 0;
         Derived.DecompLimit := 1;
         End;
  If Control.Run_on then
     Begin
     ClearNotices;
     If (not Control.BatchMode or (Control.BatchCount = 1)) and
        ((not Control.EquilMode) or OldScreen[D_Equil].First) then
        Begin
        If Control.EquilMode then
           Begin
           Control.Display := 'Iterations';
           MaxDisplay := Control.Equil.MaxIterations + 1;
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
        // create a new graph
        frmGraph := TfrmGraph.Create(frmMain);
        // add a blank selected series at the front
        frmGraph.cmbSeries.Items.AddObject('None', pointer(-1));
        frmGraph.cmbSeries.ItemIndex := 0;
        // set the graph title
        frmGraph.chtGraph.Title.Visible := true;
        frmGraph.chtGraph.Title.Text.Text := 'Graph started ' + DateTimeToStr(Now);
        // set the width of the graph
        frmGraph.chtGraph.BottomAxis.SetMinMax(0, MaxDisplay);
        End;
     End;
End; {of procedure 'Initialise'}

Procedure PrintOut;
var
  ScreenItem: ScreenOptions;
  iSeriesCount, iLayer: integer;
  DisplayX, Sum, Dummy: Real48;

  Procedure DrawItem (Datum, Multiplier: real48);
  Const Red   = 255;
        Green = 1280547;
        Blue = 16711680;
  var
    y, y1, y2, SOMSum: real48;
    iLayer: Integer;
    series: TFastLineSeries;
  Begin
    // get the original value to graph
    If Control.EquilMode and (not Control.Run_On) then
       Begin
       DisplayX := Derived.Equil.Iterations + 1;
       If Control.Equil.EquilTarget = SOM then
          Begin
          SOMSum := 0;
          For iLayer := 0 to SoilOrganic.nLayers do
              SOMSum := SOMSum + SoilOrganic.Slow[iLayer, C] + SoilOrganic.Resistant[iLayer, C] +
                        SoilOrganic.Active[iLayer, C];
          y := 0.001 * Divide (SOMSum, Control.Equil.TargetValue * 2);
          End
       Else if Control.Equil.EquilTarget = LeafNConc then
          y := 1000 * Divide(Divide (Plant.Leaves[N], Plant.Leaves[C] * Control.CConversion), Control.Equil.TargetValue * 2)
       Else if Control.Equil.EquilTarget = LeafNitrogen then
          y := Divide(Plant.Leaves[N], Control.Equil.TargetValue * 2)
       Else if Control.Equil.EquilTarget = Leafmass then
          y := 0.001 * Divide(Plant.Leaves[C] * Control.CConversion, Control.Equil.TargetValue * 2)
       Else {if Control.Equil.EquilTarget = WoodMass then}
          y := 0.001 * Divide((Plant.SapWood[C] + Plant.HeartWood[C]) * Control.CConversion, Control.Equil.TargetValue * 2);
       Sum := 0;
       For iLayer := 0 to SoilOrganic.nLayers do
           Sum := Sum + SoilOrganic.Slow[iLayer, C];
       if (ScreenRec.UpRange[D_CSlow] <> ScreenRec.LowRange[D_CSlow]) then
          y1 := ((Sum / 1000) - ScreenRec.LowRange[D_CSlow]) /
               (ScreenRec.UpRange[D_CSlow] - ScreenRec.LowRange[D_CSlow])
       else
          y1 := 0.5;
       Sum := 0;
       For iLayer := 0 to SoilOrganic.nLayers do
           Sum := Sum + SoilOrganic.Resistant[iLayer, C];
       if (ScreenRec.UpRange[D_CResistant] <> ScreenRec.LowRange[D_CResistant]) then
          y2 := ((Sum / 1000) - ScreenRec.LowRange[D_CResistant]) /
               (ScreenRec.UpRange[D_CResistant] - ScreenRec.LowRange[D_CResistant])
       else
          y2 := 0.5;
       If OldScreen[D_Equil].First then
          Begin
         // create a new series
          series := TFastLineSeries.Create(frmGraph.chtGraph);
          series.ColorEachPoint := false;
          series.Title := 'Equil search';
          series.SeriesColor := Red;
          series.AddXY(DisplayX, y);
         // add the series to the graph
          frmGraph.chtGraph.AddSeries(series);
         // add the series to the combobox too, recording which screen item is being graphed
          frmGraph.cmbSeries.Items.AddObject(series.Title, pointer(D_Equil));
         // reset the flag saying we are at the first point
          series := TFastLineSeries.Create(frmGraph.chtGraph);
          series.ColorEachPoint := false;
          series.Title := 'Slow SOM';
          series.SeriesColor := Green;
          series.AddXY(DisplayX, y1);
          frmGraph.chtGraph.AddSeries(series);
          frmGraph.cmbSeries.Items.AddObject(series.Title, pointer(D_Equil));
          series := TFastLineSeries.Create(frmGraph.chtGraph);
          series.ColorEachPoint := false;
          series.Title := 'Resistant';
          series.SeriesColor := Blue;
          series.AddXY(DisplayX, y2);
         // add the series to the graph
          frmGraph.chtGraph.AddSeries(series);
         // add the series to the combobox too, recording which screen item is being graphed
          frmGraph.cmbSeries.Items.AddObject(series.Title, pointer(D_Equil));
          OldScreen[D_Equil].First := False;
       End Else
          Begin
          series := TFastLineSeries(frmGraph.chtGraph.Series[1]);
          series.AddXY(DisplayX, y);
          series := TFastLineSeries(frmGraph.chtGraph.Series[2]);
          series.AddXY(DisplayX, y1);
          series := TFastLineSeries(frmGraph.chtGraph.Series[3]);
          series.AddXY(DisplayX, y2);
          End;
       End
    Else if (not Control.EquilMode) then
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
       End Else
          Begin
          series := TFastLineSeries(frmGraph.chtGraph.Series[iSeriesCount + 1]);
          series.AddXY(DisplayX, y);
          End;
       End;
  End; {of Procedure 'DrawItem'}

Begin
  Control.StartRun := false;
  Control.TotalDays := Control.TotalDays + 1;
  if (Control.TotalDays > Control.MaxDays) then
    Control.Run_on := false;
  Control.DaysSince := Control.DaysSince + 1;
  Control.DaysSinceDisk := Control.DaysSinceDisk + 1;
  {Check every year}
  If (((Control.DaysSinceDisk > Control.DiskInterval)) or (not Control.Run_On)) and
     (not Control.EquilMode) then
    Begin
    SaveRegular;
    Control.DaysSinceDisk := Control.DaysSinceDisk - Control.DiskInterval;
    End;
  CalcDate (Control.Date);
  // Check if we should update the graph now.
  // Update it on the first day, the last day (when the run finishes) and
  // any day in between based on the update rate from the progress bar
  If Control.EquilMode then
     Begin
     if not Control.Run_on then
        DrawItem (Dummy, Dummy)
     End
  Else if (((Control.TotalDays = 1) or (not Control.Run_On) or
     ((Control.DaysSince >= frmProgress.iUpdateRate) and
      (not Control.SensitivityTestOn) and
      frmProgress.bUpdate)) and (not Control.BatchMode)) then
  Begin
    Control.DaysSince := Control.DaysSince - frmProgress.iUpdateRate;
    If Control.DaysSince > (frmProgress.iUpdateRate - 1) then
       Control.DaysSince := frmProgress.iUpdateRate - 1;
    iSeriesCount := 0;
    For ScreenItem := D_CarbonGain to D_Dummy do
    begin
      If ScreenRec.Choose[ScreenItem] then
      begin
        Case ScreenItem of
          D_CarbonGain:   DrawItem (Derived.CarbonGain, Control.CConversion);
          D_CAI:          DrawItem (Derived.CAI, Control.CConversion * 0.001);
          D_NPP:          DrawItem (Derived.NPP, Control.CConversion * 0.001);
          D_NEE:          DrawItem (Derived.NEE, Control.CConversion);
          D_Respn:        DrawItem (Derived.Rm + Derived.Rg, Control.CConversion);
          D_DayCFlux:     DrawItem (Derived.DayCFlux, Control.CConversion);
          D_NightCFlux:   DrawItem (Derived.NightCFlux, Control.CConversion);
          D_SolubleCH2O:  DrawItem (Plant.Soluble[C], Control.CConversion * 0.001);
          D_LAI:          DrawItem (Derived.LAI, 1);
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
          D_Height:       DrawItem (Plant.Height, 1);
          D_DBH:          DrawItem (Plant.DBH, 1);
          D_CanopyCover:  DrawItem (Plant.CanopyCover, 1);
          D_kex:          DrawItem (Plant.kex, 1);
          D_BasalArea:    DrawItem (Plant.Area, 10000);
          D_Stocking:     DrawItem (Plant.Stocking, 1);
          D_TDamage:      DrawItem (Derived.TDamageUnits, 1);
          D_NSum:         DrawItem (TotalN, Control.NConversion);
          D_NConc:        DrawItem (Divide(Plant.Leaves[N], Plant.Leaves[C]), 1000 * Control.NConversion / Control.CConversion);
          D_NConc1:       DrawItem (Derived.NConc, 1000 * Control.NConversion / Control.CConversion);
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
          D_CMetabSurf:
              Begin
              Sum := 0;
              For iLayer := 0 to 0 do
                  Sum := Sum + SoilOrganic.Metab[iLayer, C];
              DrawItem (Sum, 0.001);
              End;
          D_CMetabSoil:
              Begin
              Sum := 0;
              For iLayer := 1 to SoilOrganic.nLayers do
                  Sum := Sum + SoilOrganic.Metab[iLayer, C];
              DrawItem (Sum, 0.001);
              End;
          D_CStructSurf:
              Begin
              Sum := 0;
              For iLayer := 0 to 0 do
                  Sum := Sum + SoilOrganic.Struct[iLayer, C];
              DrawItem (Sum, 0.001);
              End;
          D_CStructSoil:
              Begin
              Sum := 0;
              For iLayer := 1 to SoilOrganic.nLayers do
                  Sum := Sum + SoilOrganic.Struct[iLayer, C];
              DrawItem (Sum, 0.001);
              End;
          D_CWoodyLitter:
              Begin
              Sum := 0;
              For iLayer := 0 to SoilOrganic.nLayers do
                  Sum := Sum + SoilOrganic.FineWood[iLayer, C] +
                               SoilOrganic.CoarseWood[iLayer, C];
              DrawItem (Sum, 0.001);
              End;
          D_CActive:
              Begin
              Sum := 0;
              For iLayer := 0 to SoilOrganic.nLayers do
                  Sum := Sum + SoilOrganic.Active[iLayer, C];
              DrawItem (Sum, 0.001);
              End;
          D_CSlow:
              Begin
              Sum := 0;
              For iLayer := 0 to SoilOrganic.nLayers do
                  Sum := Sum + SoilOrganic.Slow[iLayer, C];
              DrawItem (Sum, 0.001);
              End;
          D_CResistant:
              Begin
              Sum := 0;
              For iLayer := 0 to SoilOrganic.nLayers do
                  Sum := Sum + SoilOrganic.Resistant[iLayer, C];
              DrawItem (Sum, 0.001);
              End;
          D_NMetabSurf:
              Begin
              Sum := 0;
              For iLayer := 0 to 0 do
                  Sum := Sum + SoilOrganic.Metab[iLayer, N];
              DrawItem (Sum, 1);
              End;
          D_NMetabSoil:
              Begin
              Sum := 0;
              For iLayer := 1 to SoilOrganic.nLayers do
                  Sum := Sum + SoilOrganic.Metab[iLayer, N];
              DrawItem (Sum, 1);
              End;
          D_NStructSurf:
              Begin
              Sum := 0;
              For iLayer := 0 to 0 do
                  Sum := Sum + SoilOrganic.Struct[iLayer, N];
              DrawItem (Sum, 1);
              End;
          D_NStructSoil:
              Begin
              Sum := 0;
              For iLayer := 1 to SoilOrganic.nLayers do
                  Sum := Sum + SoilOrganic.Struct[iLayer, N];
              DrawItem (Sum, 1);
              End;
          D_NWoodyLitter:
              Begin
              Sum := 0;
              For iLayer := 0 to SoilOrganic.nLayers do
                  Sum := Sum + SoilOrganic.FineWood[iLayer, N] +
                               SoilOrganic.CoarseWood[iLayer, N];
              DrawItem (Sum, 1);
              End;
          D_NActive:
              Begin
              Sum := 0;
              For iLayer := 0 to SoilOrganic.nLayers do
                  Sum := Sum + SoilOrganic.Active[iLayer, N];
              DrawItem (Sum, 1);
              End;
          D_NSlow:
              Begin
              Sum := 0;
              For iLayer := 0 to SoilOrganic.nLayers do
                  Sum := Sum + SoilOrganic.Slow[iLayer, N];
              DrawItem (Sum, 1);
              End;
          D_NResistant:
              Begin
              Sum := 0;
              For iLayer := 0 to SoilOrganic.nLayers do
                  Sum := Sum + SoilOrganic.Resistant[iLayer, N];
              DrawItem (Sum, 1);
              End;
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
          D_PAR:          DrawItem (Weather.PAR, 1);
          D_CO2:          DrawItem (Weather.CO2, 1);
          D_Rain:         DrawItem (Weather.Rain, 1);
          D_StoredWater:  DrawItem (SoilWat.TotalWater, 1);
          D_Snow:         DrawItem (SoilWat.Snow, 1);
          D_AbsHum:       DrawItem (Weather.AbsHumidity, 1000);
          D_RelHum:       DrawItem (Weather.RelHumidity, 100);
          D_WaterLimit:   DrawItem (Derived.WaterLimit, 1);
          D_HeatSum:      DrawItem (Derived.HeatSum, 1);
          D_Transpiration:DrawItem (Derived.Transpiration, 1);
          D_Evaporation:  DrawItem (Derived.Evaporation, 1);
          D_Drainage:     DrawItem (Derived.Drainage[SoilWat.nLayers], 1);
          D_Dummy:        DrawItem (Derived.Dummy, 1);
        End;
        Inc(iSeriesCount);
      end;
    end;
  End;
End; {of Procedure 'PrintOut'}

Procedure EndRun;
Begin
  If Control.OutputFileOpen and ((not Control.BatchMode) or Control.EndBatch) and
     (not Control.EquilMode) then
      Begin
      Close (Control.CenWFileOut);
      Control.OutputFileOpen := false;
      End;
  If Control.ClimFileOpen then
     Begin
     Close (Control.DefClim);
     Control.ClimFileOpen := false;
     End;
End; {of Procedure 'TCenWWindow.EndRun'}

Procedure ControlRun;
Begin
Initialise;
If not Control.EquilMode then
   frmProgress.Start
Else
  frmEquilProgress.Start;
  try
    While Control.Run_on do
      Begin
      GetWeather;
      If Control.DecayOnly then
        NitrogenGain
      Else {If not Control.DecayOnly then}
        Begin
        EventController;
        CalcPhenology;
        SetPestDamage;
        WaterIn;
        CarbonGain;
        WaterOut;
        NitrogenGain;
        CarbonLoss;
        NitrogenLoss;
        HarvestorFireLoss;
        Allocation;
        End;
      PrintOut;
      Application.ProcessMessages;
      if Control.EquilMode then
         {do nothing yet}
      Else if(frmProgress.bStopped) or (frmGraph = Nil) then
         break
      else
         frmprogress.Update;
    end; {of 'while Run_on do'}
    EndRun;
  finally
    // make sure the progress window closes
    If (not Control.EquilMode) then
       frmProgress.Stop
    Else if Control.EndEquil then
       frmEquilProgress.Stop;
  end;
end;  {of Procedure 'ControlRun'}

end.
