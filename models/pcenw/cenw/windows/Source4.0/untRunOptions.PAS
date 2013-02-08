{ ================================================================
  = Project   : Cenw                                             =
  ================================================================
  = Module    : RunOptions                                       =
  =             Routines for the processing of the               =
  =             various run options                              =
  =                                                              =
  ================================================================
  = File      : untRunOptions.PAS                                =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

Unit untRunOptions;

Interface

Uses
  untDeclarations, SysUtils, untFieldValidation, untDivideValidation, untPowerValidation,
  untGenericSave, untGenericDisplay;

Procedure InitialBatch;
Procedure BatchVariableList (Opt: Char; var BatchFileOK: Boolean);

Implementation

Procedure InitialBatch;
Begin
Control.TSoilFound := false;
Control.BatchMode := false;
Control.EquilMode := false;
Control.BatchCalcs := -1;
Control.BatchCount := 0;
Control.EndBatch := false;
End; {of Procedure 'InitialBatch'}

    Procedure BatchVariableList (Opt: Char; var BatchFileOK: Boolean);

       Procedure ReadNextBatchVariable (var BatchVariable: Real48; fScale: Real48);
       var Width, Digits: Integer;
       Begin
       if not eof(Control.BatchName) then
          Begin
          Read (Control.BatchName, BatchVariable);
          If Control.OutputFileOpen then
             Begin
             GetField(BatchVariable, MaxWidth, Width, Digits);
             Write (Control.CenwFileOut, BatchVariable:Width:Digits, ' ');
             End;
          if fScale <> 0 then
             BatchVariable := Divide(Batchvariable, fScale);
          End
       Else
          BatchFileOK := false;
       End; {of Procedure 'ReadNextBatchVariable'}

       Procedure ReadNextBatchString (var BatchVariable: FileNameType);
       var SpacePos, TabPos: Integer;
           BatchString: String;
       Begin
       if not eof(Control.BatchName) then
          Begin
          Readln (Control.BatchName, BatchString);
          SpacePos := 1; 
          While (SpacePos = 1) or (TabPos = 1) do {Remove leading spaces}
                Begin
                SpacePos := Pos(' ', BatchString);
                TabPos := Pos(Chr(9), BatchString);
                If (SpacePos = 1) or (TabPos = 1) then
                   BatchString := Copy(BatchString, 2, Length(BatchString));
                End;
         While ((Copy(BatchString, Length(BatchString), 1) = ' ') or
               (Copy(BatchString, Length(BatchString), 1) = Chr(9))) do {Remove trailing spaces}
               BatchString := Copy(BatchString, 1, Length(BatchString) - 1);
         BatchVariable := BatchString;
         If Control.OutputFileOpen then
             Begin
             Write (Control.CenwFileOut, BatchVariable, ' ');
             End;
          End
       Else
          BatchFileOK := false;
       End; {of Procedure 'ReadNextBatchString'}

       Procedure BatchVariableSwap (BatchVar: BatchOptions; var BatchVariable: Real48; Opt: Char; fScale: Real48);
       Begin
       if Batch.Choose[BatchVar] then
          Begin
          if Opt = 'S' then // save old parameter values
             Batch.OldValue[BatchVar] := BatchVariable
          Else if Opt = 'R' then // re-instate old parameter values
             BatchVariable := Batch.OldValue[BatchVar]
          Else if Opt = 'B' then // read the batch variable
             ReadNextBatchVariable (BatchVariable, fScale);
          End;
       End; {of Procedure 'BatchVariableSwap'}

       Procedure BatchIntegerSwap (BatchVar: BatchOptions; var BatchVariable: Integer; Opt: Char; fScale: Real48);
       var BatchReal: Real48;
       Begin
       if Batch.Choose[BatchVar] then
          Begin
          if Opt = 'S' then // save old parameter values
             Batch.OldValue[BatchVar] := BatchVariable
          Else if Opt = 'R' then // re-instate old parameter values
             BatchVariable := Round(Batch.OldValue[BatchVar])
          Else if Opt = 'B' then // read the batch variable
             Begin
             ReadNextBatchVariable (BatchReal, 1);
             BatchVariable := Round(BatchReal);
             End;
          End;
       End; {of Procedure 'BatchIntegerSwap'}

       Procedure BatchClimateFileSwap (BatchVar: BatchOptions; var BatchVariable: FileNameType; Opt: Char);
       Begin
       if Batch.Choose[BatchVar] then
          Begin
          if Opt = 'S' then // save old parameter values
             Batch.OldFile := BatchVariable
          Else if Opt = 'R' then // re-instate old parameter values
             BatchVariable := Batch.OldFile
          Else if Opt = 'B' then // read the batch variable
             Begin
             ReadNextBatchString (BatchVariable);
             // check whether extension has been added
             End;
          End;
       End; {of Procedure 'BatchClimateFileSwap'}

       Procedure BatchHarvestSwap (BatchVar: BatchOptions; Opt: Char);
       var iHarvest, Comma, Year, Month: Integer;
           Days: LongInt;
           PercHarvested, SizeRatio, Pruned, RemovedWood, RemovedFine: Real48;
           Density: Char;
           BatchVariable: FileNameType;
           TextF: TextFile;
           StrLine, DensityStr: String;
       Begin
       if Batch.Choose[BatchVar] then
          Begin
          if Opt = 'S' then // save old parameter values
             Begin
             Batch.nHarvests := Event.nHarvests;
             for iHarvest := 1 to Batch.nHarvests do
                 Begin
                 Batch.HarvestTimes[iHarvest, 1] := Event.HarvestTimes[iHarvest, 1];
                 Batch.HarvestTimes[iHarvest, 2] := Event.HarvestTimes[iHarvest, 2];
                 Batch.HarvestTimes[iHarvest, 3] := Event.HarvestTimes[iHarvest, 3];
                 Batch.HarvestTimes[iHarvest, 4] := Event.HarvestTimes[iHarvest, 4];
                 Batch.WoodCut[iHarvest] := Event.WoodCut[iHarvest];
                 Batch.RelativeSize[iHarvest] := Event.RelativeSize[iHarvest];
                 Batch.BranchesCut[iHarvest] := Event.BranchesCut[iHarvest];
                 Batch.WoodRemoval[iHarvest] := Event.WoodRemoval[iHarvest];
                 Batch.FineRemoval[iHarvest] := Event.FineRemoval[iHarvest];
                 Batch.AdjustStocking[iHarvest] := Event.AdjustStocking[iHarvest];
                 End;
             End
          Else if Opt = 'R' then // re-instate old parameter values
             Begin
             Event.nHarvests := Batch.nHarvests;
             for iHarvest := 1 to Batch.nHarvests do
                 Begin
                 Event.HarvestTimes[iHarvest, 1] := Batch.HarvestTimes[iHarvest, 1];
                 Event.HarvestTimes[iHarvest, 2] := Batch.HarvestTimes[iHarvest, 2];
                 Event.HarvestTimes[iHarvest, 3] := Batch.HarvestTimes[iHarvest, 3];
                 Event.HarvestTimes[iHarvest, 4] := Batch.HarvestTimes[iHarvest, 4];
                 Event.WoodCut[iHarvest] := Batch.WoodCut[iHarvest];
                 Event.RelativeSize[iHarvest] := Batch.RelativeSize[iHarvest];
                 Event.BranchesCut[iHarvest] := Batch.BranchesCut[iHarvest];
                 Event.WoodRemoval[iHarvest] := Batch.WoodRemoval[iHarvest];
                 Event.FineRemoval[iHarvest] := Batch.FineRemoval[iHarvest];
                 Event.AdjustStocking[iHarvest] := Batch.AdjustStocking[iHarvest];
                 End;
             End
          Else if Opt = 'B' then // read the batch variable
             Begin
             ReadNextBatchString (BatchVariable);
             AssignFile(TextF, BatchVariable);
             Reset(TextF);
             Event.nHarvests := 0;
             while not(eof(TextF)) do
                   begin
                   Readln(TextF, StrLine);
                   if (StrLine <> '') and (StrLine <> ' ') then
                      Begin
                      Event.nHarvests := Event.nHarvests + 1;
                      Comma := Pos(',', StrLine);
                      StrLine := Copy (StrLine, Comma + 1, Length(StrLine)); // remove the leading dummy string
                      Comma := Pos(',', StrLine);
                      Month := StrToInt(Copy(StrLine, 1, Comma - 1));
                      StrLine := Copy (StrLine, Comma + 1, Length(StrLine)); // remove the month info from string
                      Comma := Pos(',', StrLine);
                      Year := StrToInt(Copy(StrLine, 1, Comma - 1));
                      StrLine := Copy (StrLine, Comma + 1, Length(StrLine)); // remove the year info from string
                      Days := Round(365.25 * Year + 30.4375 * Month);         // Calculate days from beginning of the run
                      Event.HarvestTimes[Event.nHarvests, 1] := 1;           // default info for day of the month
                      Event.HarvestTimes[Event.nHarvests, 2] := Month;
                      Event.HarvestTimes[Event.nHarvests, 3] := Year;
                      Event.HarvestTimes[Event.nHarvests, 4] := Days;
                      Comma := Pos(',', StrLine);
                      PercHarvested := StrToFloat(Copy(StrLine, 1, Comma - 1));
                      StrLine := Copy (StrLine, Comma + 1, Length(StrLine)); // remove the %harvested info from string
                      Comma := Pos(',', StrLine);
                      SizeRatio := StrToFloat(Copy(StrLine, 1, Comma - 1));
                      StrLine := Copy (StrLine, Comma + 1, Length(StrLine)); // remove the size ratio info from string
                      Comma := Pos(',', StrLine);
                      DensityStr := Copy(StrLine, 1, Comma - 1);
                      while Pos(' ', DensityStr) = 1 do
                            DensityStr := Copy (DensityStr, 2, Length(DensityStr)); // remove leading spaces
                      Density := DensityStr[1];
                      StrLine := Copy (StrLine, Comma + 1, Length(StrLine)); // remove the density adjustment info from string
                      Comma := Pos(',', StrLine);
                      Pruned := StrToFloat(Copy(StrLine, 1, Comma - 1));
                      StrLine := Copy (StrLine, Comma + 1, Length(StrLine)); // remove the %pruned info from string
                      Comma := Pos(',', StrLine);
                      RemovedWood := StrToFloat(Copy(StrLine, 1, Comma - 1));
                      StrLine := Copy (StrLine, Comma + 1, Length(StrLine)); // remove the %wood removal info from string
                      RemovedFine := StrToFloat(Copy(StrLine, 1, Length(StrLine)));
                      Event.WoodCut[Event.nHarvests] := 0.01 * PercHarvested;
                      Event.RelativeSize[Event.nHarvests] := 0.01 * SizeRatio;
                      Event.BranchesCut[Event.nHarvests] := 0.01 * Pruned;
                      Event.WoodRemoval[Event.nHarvests] := 0.01 * RemovedWood;
                      Event.FineRemoval[Event.nHarvests] := 0.01 * RemovedFine;
                      if UpCase(Density) = 'A' then
                         Event.AdjustStocking[Event.nHarvests] := true
                      Else
                         Event.AdjustStocking[Event.nHarvests] := false;
                      End;
                   End;
             CloseFile(TextF);
             End;
          End;
       End; {of Procedure 'BatchHarvestSwap'}

       Procedure BatchEnvironmentSwap (BatchVar: BatchOptions; Opt: Char);
       var iEnvironment, Space, Comma, Year, Month, Day: Integer;
           Days: LongInt;
           CO2, Temperature, Rainfall, VapourPressure, Radiation: Real48;
           BatchVariable: FileNameType;
           StrLine: String;
           TextF: TextFile;
           Separator: Char;
       Begin
       if Batch.Choose[BatchVar] then
          Begin
          if Opt = 'S' then // save old parameter values
             Begin
             Batch.nEnvironments := Event.nEnvironments;
             For iEnvironment := 1 to Batch.nEnvironments do
                 Begin
                 Batch.EnvironmentTimes[iEnvironment] := Event.EnvironmentTimes[iEnvironment];
                 Batch.CO2[iEnvironment] := Event.CO2[iEnvironment];
                 Batch.Temperature[iEnvironment] := Event.Temperature[iEnvironment];
                 Batch.Rainfall[iEnvironment] := Event.Rainfall[iEnvironment];
                 Event.VP[iEnvironment] := Batch.VP[iEnvironment];
                 Event.Radn[iEnvironment] := Batch.Radn[iEnvironment];
                 End;
             End
          Else if Opt = 'R' then // re-instate old parameter values
             Begin
             Event.nEnvironments := Batch.nEnvironments;
             for iEnvironment := 1 to Batch.nEnvironments do
                 Begin
                 Event.EnvironmentTimes[iEnvironment] := Batch.EnvironmentTimes[iEnvironment];
                 Event.CO2[iEnvironment] := Batch.CO2[iEnvironment];
                 Event.Temperature[iEnvironment] := Batch.Temperature[iEnvironment];
                 Event.Rainfall[iEnvironment] := Batch.Rainfall[iEnvironment];
                 Event.VP[iEnvironment] := Batch.VP[iEnvironment];
                 Event.Radn[iEnvironment] := Batch.Radn[iEnvironment];
                 End;
             End
          Else if Opt = 'B' then // read the batch variable
             Begin
             ReadNextBatchString (BatchVariable);
             AssignFile(TextF, BatchVariable);
             Reset(TextF);
             Event.nEnvironments := 0;
             while not(eof(TextF)) do
                   begin
                   Readln(TextF, StrLine);
                   Comma := Pos(',', StrLine);
                   if Comma = 0 then
                      Separator := ' '
                   Else
                      Separator := ',';
                   if (StrLine <> '') and (StrLine <> ' ') and (Separator = ',') then
                      Begin
                      Event.nEnvironments := Event.nEnvironments + 1;
                      Comma := Pos(',', StrLine);
                      StrLine := Copy (StrLine, Comma + 1, Length(StrLine)); // remove the leading dummy string
                      Comma := Pos(',', StrLine);
                      Month := StrToInt(Copy(StrLine, 1, Comma - 1));
                      StrLine := Copy (StrLine, Comma + 1, Length(StrLine)); // remove the month info from string
                      Comma := Pos(',', StrLine);
                      Year := StrToInt(Copy(StrLine, 1, Comma - 1));
                      StrLine := Copy (StrLine, Comma + 1, Length(StrLine)); // remove the year info from string
                      Days := Round(365.25 * Year + 30.4375 * Month);         // Calculate days from beginning of the run
                      Event.EnvironmentTimes[Event.nEnvironments] := Days;
                      Comma := Pos(',', StrLine);
                      CO2 := StrToFloat(Copy(StrLine, 1, Comma - 1));
                      StrLine := Copy (StrLine, Comma + 1, Length(StrLine)); // remove the %harvested info from string
                      Comma := Pos(',', StrLine);
                      Temperature := StrToFloat(Copy(StrLine, 1, Comma - 1));
                      StrLine := Copy (StrLine, Comma + 1, Length(StrLine)); // remove the size ratio info from string
                      Comma := Pos(',', StrLine);
                      StrLine := Copy (StrLine, Comma + 1, Length(StrLine)); // remove the density adjustment info from string
                      Comma := Pos(',', StrLine);
                      Rainfall := StrToFloat(Copy(StrLine, 1, Comma - 1));
                      StrLine := Copy (StrLine, Comma + 1, Length(StrLine)); // remove the %pruned info from string
                      Comma := Pos(',', StrLine);
                      VapourPressure := StrToFloat(Copy(StrLine, 1, Comma - 1));
                      StrLine := Copy (StrLine, Comma + 1, Length(StrLine)); // remove the %pruned info from string
                      Comma := Pos(',', StrLine);
                      Radiation := StrToFloat(Copy(StrLine, 1, Comma - 1));
                      StrLine := Copy (StrLine, Comma + 1, Length(StrLine)); // remove the %pruned info from string
                      Event.CO2[Event.nEnvironments] := CO2;
                      Event.Temperature[Event.nEnvironments] := Temperature;
                      Event.Rainfall[Event.nEnvironments] := Rainfall;
                      Event.VP[Event.nEnvironments] := VapourPressure;
                      Event.Radn[Event.nEnvironments] := Radiation;
                      End
                   Else if (StrLine <> '') and (StrLine <> ' ') and (Separator = ' ') then
                      Begin
                      Event.nEnvironments := Event.nEnvironments + 1;
                      while Pos(' ', StrLine) = 1 do
                            StrLine := Copy (StrLine, 1, Length(StrLine)); // remove leading spaces
                      Space := Pos(' ', StrLine);
                      Day := StrToInt(Copy(StrLine, 1, Space - 1));
                      StrLine := Copy (StrLine, Space, Length(StrLine)); // remove leading spaces
                      while Pos(' ', StrLine) = 1 do
                            StrLine := Copy (StrLine, 2, Length(StrLine)); // remove leading spaces
                      Space := Pos(' ', StrLine);
                      Month := StrToInt(Copy(StrLine, 1, Space - 1));
                      StrLine := Copy (StrLine, Space, Length(StrLine)); // remove leading spaces
                      while Pos(' ', StrLine) = 1 do
                            StrLine := Copy (StrLine, 2, Length(StrLine)); // remove leading spaces
                      Space := Pos(' ', StrLine);
                      Year := StrToInt(Copy(StrLine, 1, Space - 1));
                      StrLine := Copy (StrLine, Space + 1, Length(StrLine)); // remove the year info from string
                      Days := Round(365.25 * Year + 30.4375 * Month);         // Calculate days from beginning of the run
                      Event.EnvironmentTimes[Event.nEnvironments] := Days;
                      while Pos(' ', StrLine) = 1 do
                            StrLine := Copy (StrLine, 2, Length(StrLine)); // remove leading spaces
                      Space := Pos(' ', StrLine);
                      CO2 := StrToFloat(Copy(StrLine, 1, Space - 1));
                      StrLine := Copy (StrLine, Space + 1, Length(StrLine)); // remove the year info from string
                      while Pos(' ', StrLine) = 1 do
                            StrLine := Copy (StrLine, 2, Length(StrLine)); // remove leading spaces
                      Space := Pos(' ', StrLine);
                      Temperature := StrToFloat(Copy(StrLine, 1, Space - 1));
                      StrLine := Copy (StrLine, Space + 1, Length(StrLine)); // remove the size ratio info from string
                      while Pos(' ', StrLine) = 1 do
                            StrLine := Copy (StrLine, 2, Length(StrLine)); // remove leading spaces
                      Rainfall := StrToFloat(StrLine);
                      StrLine := Copy (StrLine, Space + 1, Length(StrLine)); // remove the size ratio info from string
                      while Pos(' ', StrLine) = 1 do
                            StrLine := Copy (StrLine, 2, Length(StrLine)); // remove leading spaces
                      VapourPressure := StrToFloat(StrLine);
                      StrLine := Copy (StrLine, Space + 1, Length(StrLine)); // remove the size ratio info from string
                      while Pos(' ', StrLine) = 1 do
                            StrLine := Copy (StrLine, 2, Length(StrLine)); // remove leading spaces
                      Radiation := StrToFloat(StrLine);
                      Event.CO2[Event.nEnvironments] := CO2;
                      Event.Temperature[Event.nEnvironments] := Temperature;
                      Event.Rainfall[Event.nEnvironments] := Rainfall;
                      Event.VP[Event.nEnvironments] := VapourPressure;
                      Event.Radn[Event.nEnvironments] := Radiation;
                      End;
                   End;
             CloseFile(TextF);
             End;
          End;
       End; {of Procedure 'BatchEnvironmentSwap'}

       Procedure BatchWHCSwap (BatchVar: BatchOptions; Opt: Char);
       var WHC, OldWHC: Real48;
           iLayer: Integer;
       Begin
       if Batch.Choose[BatchVar] then
          Begin
          if Opt = 'S' then // save old parameter values
             // no action needed as the relevant information on WHC will be re-created from pore size and depth info
          Else if Opt = 'R' then // re-instate old parameter values
             Begin
             for iLayer := 1 to SoilOrganic.nLayers do
                 SoilWat.Layer[iLayer].MaxWater := SoilWat.Layer[iLayer].Depth * SoilWat.Layer[iLayer].Pores / 10;
             End
          Else if Opt = 'B' then // read the batch variable
             Begin
             ReadNextBatchVariable (WHC, 1);
             OldWHC := 0;
             for iLayer := 1 to SoilOrganic.nLayers do
                 OldWHC := OldWHC + SoilWat.Layer[iLayer].Depth * SoilWat.Layer[iLayer].Pores / 10;
             for iLayer := 1 to SoilOrganic.nLayers do
                 SoilWat.Layer[iLayer].MaxWater := SoilWat.Layer[iLayer].Depth * SoilWat.Layer[iLayer].Pores / 10
                                                   * Divide(WHC, OldWHC);
             End;
          End;
       End; {of Procedure 'BatchWHCSwap'}

    Begin
    BatchIntegerSwap (B_RunDay, Control.nDays, Opt, 1);
    BatchIntegerSwap (B_RunMonth, Control.nMonths, Opt, 1);
    BatchIntegerSwap (B_RunYear, Control.nYears, Opt, 1);
    BatchVariableSwap (B_Stocking, Control.Initial.Stocking, Opt, 1);
    BatchVariableSwap (B_Latitude, Parameter.Latitude, Opt, 1);
    BatchVariableSwap (B_ExtraCO2, Event.CO2[1], Opt, 1);
    BatchVariableSwap (B_ExtraTemp, Event.Temperature[1], Opt, 1);
    BatchVariableSwap (B_ExtraRain, Event.Rainfall[1], Opt, 1);
    BatchVariableSwap (B_ExtraVP, Event.VP[1], Opt, 1);
    BatchVariableSwap (B_ExtraRadn, Event.Radn[1], Opt, 1);
    BatchVariableSwap (B_LeafLignin, Parameter.LeafLignin, Opt, 100);
    BatchVariableSwap (B_RootLignin, Parameter.RootLignin, Opt, 100);
    BatchWHCSwap (B_MaxWater, Opt);
    BatchVariableSwap (B_FineSoil, Parameter.FineSoil, Opt, 100);
    BatchVariableSwap (B_FertilityAdjust, Parameter.FertilityAdjust, Opt, 1);
    BatchVariableSwap (B_ConstantLeafN, Parameter.ConstantLeafNValue, Opt, 1000 * Control.NConversion / Control.CConversion);
    if Event.IrrigationType = 'R' then
       BatchVariableSwap (B_Irrigate, Event.IrrigationAmount, Opt, 1)
    Else // if Event.IrrigationType = 'S' then
       BatchVariableSwap (B_Irrigate, Event.IrrigationFraction, Opt, 1);
    BatchVariableSwap (B_FertiliseN, Event.FertiliseAmount[1, N], Opt, 1);
    BatchVariableSwap (B_FertiliseP, Event.FertiliseAmount[1, P], Opt, 1);
    BatchClimateFileSwap (B_ClimateFile, Control.ClimFile, Opt);
    BatchHarvestSwap (B_Harvesting, Opt);
    BatchEnvironmentSwap (B_Environment, Opt);
    if Opt = 'B' then
       Begin
       If (not eof(Control.BatchName)) and
          (not Batch.Choose[B_ClimateFile]) and (not Batch.Choose[B_Harvesting] and (not Batch.Choose[B_Environment])) then
          Readln (Control.BatchName);
       If Control.OutputFileOpen then
          Writeln (Control.CenwFileOut);
       End;
    End; {of Procedure 'BatchVariableList'}

{ --- end of file unRunOptions.PAS ------------------------------------------ }
    
End.


