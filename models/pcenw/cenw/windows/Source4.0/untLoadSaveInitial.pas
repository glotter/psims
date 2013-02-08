{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : GenPools                                         =
  =             SavePools                                        =
  =                                                              =
  =             Routines to read pools file.               =
  =             The save/retrieve algorithms are set up in such  =
  =             a way that new information can easily be added   =
  =             to the list of parameters saved previously       =
  =             so that saved information can remain valid over  =
  =             successvive upgrades of the program.             =
  ================================================================
  = File      : untLoadSaveInitial.PAS                           =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

Unit untLoadSaveInitial;

{$V-}

INTERFACE

Uses
  SysUtils, untDeclarations, untDivideValidation, untFieldValidation;

Procedure GenPools(name : string);
Procedure SavePools(name : string);

IMPLEMENTATION

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

Procedure GenPools(name : string);
var fp : Text;
    Linein, VarStr, CommentStr, LayerInfo: string;
    ErrCode, iLayer, i, RunLayers: integer;

        Procedure RealIn (Var Item: Real48; Comment: String);
        Begin
        If CommentStr = Comment then
           val (VarStr, Item, ErrCode);
        End; {of Procedure 'RealIn'}

        Procedure TSoilIn (Var Item: Real48; Comment: String);
        // New routine includes a safeguard to tell the program whether a soil temperature has been saved previously
        Begin
        If CommentStr = Comment then
           Begin
           val (VarStr, Item, ErrCode);
           Control.TSoilFound := true;
           End;
        End; {of Procedure 'TSoilIn'}

        Procedure IntegerIn (Var Item: Integer; Comment: String);
        Begin
        If CommentStr = Comment then
           val (VarStr, Item, ErrCode);
        End; {of Procedure 'IntegerIn'}

        Procedure LongIn (Var Item: LongInt; Comment: String);
        Begin
        If CommentStr = Comment then
           val (VarStr, Item, ErrCode);
        End; {of Procedure 'IntegerIn'}

        Procedure ElementIn (Var Item: TElements; Comment: String);
           var ElementInfo, CommStr: String;
           Begin
           ElementInfo := Copy(CommentStr,1, 3);
           If (ElementInfo = 'Nit') or (ElementInfo = 'Car')
              or (ElementInfo = 'Pho') or (ElementInfo = 'C13') then
              Begin
              If ElementInfo = 'Nit' then
                 CommStr := Copy(CommentStr, 10, length(CommentStr) - 9)
              Else if ElementInfo = 'Car' then
                 CommStr := Copy(CommentStr, 8, length(CommentStr) - 7)
              Else if ElementInfo = 'C13' then
                 CommStr := Copy(CommentStr, 8, length(CommentStr) - 4)
              Else if ElementInfo = 'Pho' then
                 CommStr := Copy(CommentStr, 12, length(CommentStr) - 11);
              If CommStr = Comment then
                 Begin
                 If ElementInfo = 'Car' then
                    val (VarStr, Item[C], ErrCode)
                 Else if ElementInfo = 'C13' then
                    val (VarStr, Item[C13], ErrCode)
                 Else if ElementInfo = 'Nit' then
                    val (VarStr, Item[N], ErrCode)
                 Else if ElementInfo = 'Pho' then
                    val (VarStr, Item[P], ErrCode);
                 End;
              End;
           End; {of Procedure 'ElementIn'}

        Procedure SoilElementIn (Var Item: SoilElements; Comment: String);
           var ElementInfo, CommStr, NewComment, LayerInfo: String;
               iLayer: Integer;
           Begin
           For iLayer := 0 to SoilOrganic.nLayers do
               Begin
               Str (iLayer:3, LayerInfo);
               NewComment := Comment + ' layer' + LayerInfo;
               ElementInfo := Copy(CommentStr,1, 3);
               If (ElementInfo = 'Nit') or (ElementInfo = 'Car')
                  or (ElementInfo = 'Pho') or (ElementInfo = 'C13') then
                  Begin
                  If ElementInfo = 'Nit' then
                     CommStr := Copy(CommentStr, 10, length(CommentStr) - 9)
                  Else if ElementInfo = 'Car' then
                     CommStr := Copy(CommentStr, 8, length(CommentStr) - 7)
                  Else if ElementInfo = 'C13' then
                     CommStr := Copy(CommentStr, 8, length(CommentStr) - 4)
                  Else if ElementInfo = 'Pho' then
                     CommStr := Copy(CommentStr, 12, length(CommentStr) - 11);
                  If CommStr = NewComment then
                     Begin
                     If ElementInfo = 'Car' then
                        val (VarStr, Item[iLayer, C], ErrCode)
                     Else if ElementInfo = 'C13' then
                        val (VarStr, Item[iLayer, C13], ErrCode)
                     Else if ElementInfo = 'Nit' then
                        val (VarStr, Item[iLayer, N], ErrCode)
                     Else if ElementInfo = 'Pho' then
                       val (VarStr, Item[iLayer, P], ErrCode);
                     End;
                  End;
               End;
           NewComment := Comment;
           ElementInfo := Copy(CommentStr,1, 3);
           If (ElementInfo = 'Nit') or (ElementInfo = 'Car')
               or (ElementInfo = 'Pho') or (ElementInfo = 'C13') then
              Begin
              If ElementInfo = 'Nit' then
                 CommStr := Copy(CommentStr, 10, length(CommentStr) - 9)
              Else if ElementInfo = 'Car' then
                 CommStr := Copy(CommentStr, 8, length(CommentStr) - 7)
              Else if ElementInfo = 'C13' then
                 CommStr := Copy(CommentStr, 8, length(CommentStr) - 4)
              Else if ElementInfo = 'Pho' then
                 CommStr := Copy(CommentStr, 12, length(CommentStr) - 11);
              If CommStr = NewComment then
                 Begin
                 If ElementInfo = 'Car' then
                    val (VarStr, Item[1, C], ErrCode)
                 Else if ElementInfo = 'C13' then
                    val (VarStr, Item[1, C13], ErrCode)
                 Else if ElementInfo = 'Nit' then
                    val (VarStr, Item[1, N], ErrCode)
                 Else if ElementInfo = 'Pho' then
                    val (VarStr, Item[1, P], ErrCode);
                 End;
              End;
           End; {of Procedure 'SoilElementIn'}

        Procedure SoilElementSum (Var Item: SoilElements);
        var iLayer: Integer;
            Sum: Real48;
            E: ElementsUsed;
        Begin
        For E := C13 to P do
            Begin
            Sum := 0;
            For iLayer := 1 to SoilOrganic.nLayers do
                Sum := Sum + Item[iLayer, E];
            Item[1, e] := Sum;
            For iLayer := 2 to SoilOrganic.nLayers do
                Item[iLayer, E] := 0;
            End;
        End; {of Procedure 'SoilElementSum'}

        Procedure OldElementIn (Var Item: SoilElements; Comment: String; Layer: Integer);
        {This procedure is kept to retrieve info from files in old format}
        var ElementInfo, CommStr, NewComment: String;
        Begin
        NewComment := Comment;
        ElementInfo := Copy(CommentStr,1, 5);
        If (ElementInfo = 'Nitro') or (ElementInfo = 'Carbo') or (ElementInfo = 'Phosp') then
            Begin
            If ElementInfo = 'Nitro' then
               CommStr := Copy(CommentStr, 10, length(CommentStr) - 9)
            Else if ElementInfo = 'Carbo' then
               CommStr := Copy(CommentStr, 8, length(CommentStr) - 7)
            Else if ElementInfo = 'Phosp' then
               CommStr := Copy(CommentStr, 12, length(CommentStr) - 11);
            If CommStr = NewComment then
               Begin
               If ElementInfo = 'Carbo' then
                  val (VarStr, Item[Layer, C], ErrCode)
               Else if ElementInfo = 'Nitro' then
                  val (VarStr, Item[Layer, N], ErrCode)
               Else if ElementInfo = 'Phosp' then
                  val (VarStr, Item[Layer, P], ErrCode);
               End;
            End;
        End; {of Procedure 'OldElementIn'}

        Procedure GetCounters (Comment: String);
        var iDay: Integer;
        Begin
        if Comment = 'Previous values for various counters' then
           Begin
           Control.CountersSaved := true;
           for iDay := 1 to 365 do
               Read (fp, Derived.LastYearsWood[iDay]);
           Readln (fp);
           for iDay := 1 to 365 do
               Read (fp, Derived.LastYearsDecomp[iDay]);
           Readln (fp);
           for iDay := 1 to 365 do
               Read (fp, Derived.NPPCount[iDay]);
           Readln (fp);
           for iDay := 1 to 365 do
               Read (fp, Derived.Temperature[iDay]);
           Readln (fp);
           End;
        End;  {of Procedure 'GetCounters'}

Begin
Assign (fp, Name);
Reset (fp);
iLayer := 1;
Control.CountersSaved := false;   // remains false unless it finds the relevant data in 'GetCounters'
while not eof(fp) do
      Begin
      Readln(fp, Linein);
      ProcessLine (Linein, VarStr, CommentStr);
      SoilOrganic.nLayers := SoilWat.nLayers; // In case no value has been saved previously
      IntegerIn (SoilOrganic.nLayers, 'Number of layers for soil organic matter');
      SoilElementIn (soilorganic.Struct, 'in structural litter');
      SoilElementIn (soilorganic.Metab, 'in metabolic litter');
      SoilElementIn (soilorganic.FineWood, 'in fine wood litter');
      SoilElementIn (soilorganic.CoarseWood, 'in coarse wood litter');
      SoilElementIn (soilorganic.Active, 'in active organic matter');
      SoilElementIn (soilorganic.slow, 'in slow organic matter');
      SoilElementIn (soilorganic.Resistant, 'in resistant organic matter');
      SoilElementIn (soilorganic.Inert, 'in inert organic matter');
      SoilElementIn (Soilorganic.Soluble, 'in soluble organic matter');
      SoilElementIn (Soilorganic.RockP, '(in rocks) in');
      SoilElementIn (Soilorganic.OccludedP, '(occluded) in');
      SoilElementIn (Soilorganic.SecondaryInorganicP, '(secondary inorganic) in');
      OldElementIn (soilorganic.Struct, 'in surface structural litter', 0);
      OldElementIn (soilorganic.Metab, 'in surface metabolic litter', 0);
      OldElementIn (soilorganic.Struct, 'in soil structural litter', 1);
      OldElementIn (soilorganic.Metab, 'in soil metabolic litter', 1);
      OldElementIn (soilorganic.FineWood, 'in surface fine wood litter', 0);
      OldElementIn (soilorganic.CoarseWood, 'in surface coarse wood litter', 0);
      OldElementIn (soilorganic.CoarseWood, 'in soil coarse wood litter', 1);
      ElementIn (Litter.CoarseWood, 'in dead coarse wood');
      ElementIn (Litter.FineWood, 'in dead fine wood');
      ElementIn (Litter.CoarseRoot, 'in dead coarse root');
      ElementIn (Litter.FineRoot, 'in dead fine root');
      ElementIn (Litter.Leaves, 'in dead leaves');
      ElementIn (Litter.Other, 'in dead other material');
      ElementIn (Plant.Sapwood, 'in sapwood');
      ElementIn (Plant.Heartwood, 'in heartwood');
      ElementIn (Plant.Coarseroot, 'in coarse roots');
      ElementIn (Plant.Fineroot, 'in fine root');
      ElementIn (Plant.Branches, 'in branches');
      ElementIn (Plant.Bark, 'in bark');
      ElementIn (Plant.Reserves, 'in foliage reserves');
      ElementIn (Plant.Leaves, 'in leaves');
      ElementIn (Plant.Pollen, 'in pollen');
      ElementIn (Plant.Fruit, 'in fruit');
      ElementIn (Plant.Soluble, 'in non-structural form');
      ElementIn (Plant.WeedLeaves, 'in weed leaves');
      ElementIn (Plant.WeedRoots, 'in weed roots');
      RealIn (Plant.Stocking, 'Stocking rate in stems per ha');
      RealIn (SoilOrganic.LitterLig[0], 'Lignin concentration in surface litter');   {Retained to read info from old files}
      RealIn (SoilOrganic.LitterLig[1], 'Lignin concentration in soil litter');      {Retained to read info from old files}
      RealIn (SoilOrganic.BranchLig[0],  'Lignin concentration in branch litter'); {Retained to read info from old files}
      RealIn (SoilOrganic.StemLig[0], 'Lignin concentration in stem litter');      {Retained to read info from old files}
      For iLayer := 0 to SoilOrganic.nLayers do
          Begin
          Str (iLayer:3, LayerInfo);
          RealIn (SoilOrganic.LitterLig[iLayer], 'Litter lignin concentration in layer' + LayerInfo);
          RealIn (SoilOrganic.BranchLig[iLayer],  'Fine-wood lignin concentration in layer' + LayerInfo);
          RealIn (SoilOrganic.StemLig[iLayer], 'Coarse-wood lignin concentration in layer' + LayerInfo);
          End;
      RealIn (SoilWat.Snow, 'Size of snow pack (mm)');
      RealIn (SoilWat.TotalWater,  'Total water held in the soil');
      RealIn (SoilWat.MaxExtract, 'Total relative water extraction term for all layers');
      For iLayer := 0 to SoilWat.nLayers do
          Begin
          Str (iLayer:3, LayerInfo);
          RealIn (SoilWat.Layer[iLayer].WaterContent, 'Amount of water (mm) in layer' + LayerInfo);
          End;
      LongIn (Control.TotalYears, 'Starting Year');
      IntegerIn (Control.ExtraMonths, 'Starting Month');
      IntegerIn (Control.ExtraDays, 'Starting Day');
      LongIn (Plant.Age, 'Plant age');
      RealIn (Plant.Height, 'Average stand height');
      RealIn (Plant.Area, 'Cross-sectional area of sapwood');
      RealIn (Plant.DBH, 'Diameter at breast height');
      RealIn (Plant.CanopyCover, 'Stand canopy cover');
      RealIn (Plant.kex, 'Light extinction coefficient');
      RealIn (Derived.TDamageUnits, 'Temperature damage units');
      RealIn (Derived.RainDamageUnits, 'Damage units by heavy rain');
      RealIn (Derived.WaterLimit, 'Water limitation term');
      RealIn (Derived.DecompLimit, 'Water limitation term for decomposition');
      RealIn (Derived.LeafGrowth, 'Seasonal capacity to convert foliage reserves into new foliage');
      RealIn (Derived.Deciduous, 'Seasonal foliage shedding term');
      RealIn (Derived.ExcessN, 'Nitrogen in excess of the ability of trees to absorb it');
      RealIn (Derived.RespnBase, 'Base rate for respiration rate (for temperature adjustments)');
      RealIn (Derived.HeatSum, 'Heat sum');
      RealIn (Derived.AnnualMeanTemp, 'Annual mean temperature');
      RealIn (Weather.LastMin, 'Previous day minimum temperature');
      RealIn (Weather.LastAbsHum, 'Previous day absolute humidity');
      RealIn (Weather.LastRelHum, 'Previous day relative humidity');
      RealIn (Derived.SpringMaxTemp, 'Average spring-time maximum temperature');
      RealIn (Derived.SpringTempSum, 'Cumulative spring time temperatures');
      IntegerIn (Derived.SpringTempCounter, 'Counter for calculating the next spring-time temperature');
      RealIn (Plant.WeedHeight, 'Height of the weed layer');
      TSoilIn (Weather.TSoil, 'Soil temperature');
      IntegerIn (Control.NextPhenology, 'Sequence counter for phenology settings');
      IntegerIn (Control.PhenologyDayCount, 'Number of days for which the current phenological setting has been operative (if that is the critical element)');
      GetCounters (CommentStr);
      end; {of 'while not eof(fp)' statement}
Close(fp);
iLayer := iLayer - 1;
If SoilWat.nLayers <> iLayer then  {This If-statement is invoked if the number of layers saved
                                     in the parameter file are different from those in the initial pool size file}
   Begin
   SoilWat.TotalWater := 0;
   For i := 1 to iLayer do
       SoilWat.TotalWater := SoilWat.TotalWater + SoilWat.Layer[i].WaterContent;
   For i := 1 to SoilWat.nLayers do
       Begin
       If SoilWat.TotalWater > SoilWat.Layer[i].MaxWater then
          Begin
          SoilWat.Layer[i].WaterContent := SoilWat.Layer[i].MaxWater;
          SoilWat.TotalWater := SoilWat.TotalWater - SoilWat.Layer[i].MaxWater;
          End
       Else
          Begin
          SoilWat.Layer[i].WaterContent := SoilWat.TotalWater;
          SoilWat.TotalWater := 0;
          End;
       End;
   SoilWat.TotalWater := 0;
   For i := 1 to SoilWat.nLayers do
       SoilWat.TotalWater := SoilWat.TotalWater + SoilWat.Layer[i].WaterContent;
   End;                 {of 'If SoilWater.nLayers <> iLayer then' statement}

If Control.AllOneLayer then
   Begin
   RunLayers := 1;
   If SoilOrganic.nLayers <> RunLayers then  {This If-statement is invoked if the number of layers saved
                                     in the parameter file are different from those in the initial pool size file}
      Begin
      SoilElementSum (Soilorganic.Struct);
      SoilElementSum (Soilorganic.Metab);
      SoilElementSum (Soilorganic.FineWood);
      SoilElementSum (Soilorganic.CoarseWood);
      SoilElementSum (Soilorganic.Active);
      SoilElementSum (Soilorganic.Slow);
      SoilElementSum (Soilorganic.Resistant);
      SoilElementSum (Soilorganic.Soluble);
      End;                 {of 'If SoilOrganic.nLayers <> RunLayers then' statement}
   SoilOrganic.nLayers := RunLayers;
   End;
End; {of Procedure 'GenPools'}

Procedure SavePools(name : string);
var fp : Text;
    ElementInfo, LayerInfo: String;
    Elements: ElementsUsed;
    iLayer, iDay: integer;

    Procedure LineOut (Datum: real48; Comment: string);
    var Width, Digits: integer;
    Begin
    GetField(Datum, ParFileMaxWidth, Width, Digits);
    Writeln (fp, Datum: ParFileMaxWidth: Digits, Comment);
    End;  {of Procedure 'LineOut'}

    Procedure IntegerOut (Datum: Integer; Comment: string);
    Begin
    Writeln (fp, Datum:ParFileMaxWidth, Comment);
    End;  {of Procedure 'IntegerOut'}

    Procedure WriteCounters (NextArray: array365; MaxWidth: Integer);
    var iDay, Width, Digits: Integer;
    Begin
    For iDay := Control.Count + 1 to 365 do
        Begin
        GetField(NextArray[iDay], MaxWidth, Width, Digits);
        Write (fp, NextArray[iDay]:Width:Digits, ' ');
        End;
    For iDay := 1 to Control.Count do
        Begin
        GetField(NextArray[iDay], MaxWidth, Width, Digits);
        Write (fp, NextArray[iDay]:Width:Digits, ' ');
        End;
    Writeln (fp);
    End; {of Procedure 'WriteCounters'}

Begin
Assign(fp, name);
ReWrite(fp);
Writeln (fp, Control.Version + ' Pools');
IntegerOut (SoilOrganic.nLayers, ' Number of layers for soil organic matter');
for Elements := C13 to P do
        Begin
        Case Elements of
             C13: ElementInfo := ' C13    ';
             C: ElementInfo := ' Carbon ';
             N: ElementInfo := ' Nitrogen ';
             P: ElementInfo := ' Phosphorus ';
             End;
        For iLayer := 0 to SoilOrganic.nLayers do
            Begin
            Str (iLayer:3, LayerInfo);
            LineOut (soilorganic.Struct[iLayer, Elements], ElementInfo + 'in structural litter layer' + LayerInfo);
            LineOut (soilorganic.Metab[iLayer, Elements], ElementInfo + 'in metabolic litter layer' + LayerInfo);
            LineOut (soilorganic.FineWood[iLayer, Elements], ElementInfo + 'in fine wood litter layer' + LayerInfo);
            LineOut (soilorganic.CoarseWood[iLayer, Elements], ElementInfo + 'in coarse wood litter layer' + LayerInfo);
            LineOut (soilorganic.Active[iLayer, Elements], ElementInfo + 'in active organic matter layer' + LayerInfo);
            LineOut (soilorganic.Slow[iLayer, Elements], ElementInfo + 'in slow organic matter layer' + LayerInfo);
            LineOut (soilorganic.Resistant[iLayer, Elements], ElementInfo + 'in resistant organic matter layer' + LayerInfo);
            LineOut (soilorganic.Inert[iLayer, Elements], ElementInfo + 'in inert organic matter layer' + LayerInfo);
            LineOut (Soilorganic.Soluble[iLayer, Elements], ElementInfo + 'in soluble organic matter layer' + LayerInfo);
            if Elements = P then
               Begin
               LineOut (Soilorganic.RockP[iLayer, Elements], ' Phosphorus (in rocks) in layer' + LayerInfo);
               LineOut (Soilorganic.OccludedP[iLayer, Elements], ' Phosphorus (occluded) in layer' + LayerInfo);
               LineOut (Soilorganic.SecondaryInorganicP[iLayer, Elements], ' Phosphorus (secondary inorganic) in layer' + LayerInfo);
               End;
            End;
        LineOut (Litter.CoarseWood[Elements], ElementInfo + 'in dead coarse wood');
        LineOut (Litter.FineWood[Elements], ElementInfo + 'in dead fine wood');
        LineOut (Litter.CoarseRoot[Elements], ElementInfo + 'in dead coarse root');
        LineOut (Litter.FineRoot[Elements], ElementInfo + 'in dead fine root');
        LineOut (Litter.Leaves[Elements], ElementInfo + 'in dead leaves');
        LineOut (Litter.Other[Elements], ElementInfo + 'in dead other material');
        LineOut (Plant.sapwood[Elements], ElementInfo + 'in sapwood');
        LineOut (Plant.Heartwood[Elements], ElementInfo + 'in heartwood');
        LineOut (Plant.Coarseroot[Elements], ElementInfo + 'in coarse roots');
        LineOut (Plant.Fineroot[Elements], ElementInfo + 'in fine root');
        LineOut (Plant.Branches[Elements], ElementInfo + 'in branches');
        LineOut (Plant.Bark[Elements], ElementInfo + 'in bark');
        LineOut (Plant.Reserves[Elements], ElementInfo + 'in foliage reserves');
        LineOut (Plant.Leaves[Elements], ElementInfo + 'in leaves');
        LineOut (Plant.Pollen[Elements], ElementInfo + 'in pollen');
        LineOut (Plant.Fruit[Elements], ElementInfo + 'in fruit');
        LineOut (Plant.Soluble[Elements], ElementInfo + 'in non-structural form');
        LineOut (Plant.WeedLeaves[Elements], ElementInfo + 'in weed leaves');
        LineOut (Plant.WeedRoots[Elements], ElementInfo + 'in weed roots');
        End;
LineOut (Plant.Stocking, ' Stocking rate in stems per ha');
For iLayer := 0 to SoilOrganic.nLayers do
       Begin
       Str (iLayer:3, LayerInfo);
       LineOut (SoilOrganic.LitterLig[iLayer], ' Litter lignin concentration in layer' + LayerInfo);
       LineOut (SoilOrganic.BranchLig[iLayer],  ' Fine-wood lignin concentration in layer' + LayerInfo);
       LineOut (SoilOrganic.StemLig[iLayer], ' Coarse-wood lignin concentration in layer' + LayerInfo);
       End;
LineOut (SoilWat.Snow,  ' Size of snow pack (mm)');
LineOut (SoilWat.TotalWater,  ' Total water held in the soil');
LineOut (SoilWat.MaxExtract, ' Total relative water extraction term for all layers');
IntegerOut (SoilWat.nLayers, ' Number of layers for soil water');
For iLayer := 0 to SoilWat.nLayers do
       Begin
       Str (iLayer:3, LayerInfo);
       LineOut(SoilWat.Layer[iLayer].WaterContent, ' Amount of water (mm) in layer' + LayerInfo);
       End;
IntegerOut (Control.TotalYears, ' Starting Year');
IntegerOut (Control.ExtraMonths, ' Starting Month');
IntegerOut (Control.ExtraDays, ' Starting Day');
IntegerOut (Plant.Age, ' Plant age');
LineOut (Plant.Height, ' Average stand height');
LineOut (Plant.Area, ' Cross-sectional area of sapwood');
LineOut (Plant.DBH, ' Diameter at breast height');
LineOut (Plant.CanopyCover, ' Stand canopy cover');
LineOut (Plant.kex, ' Light extinction coefficient');
LineOut (Derived.TDamageUnits, ' Temperature damage units');
LineOut (Derived.RainDamageUnits, ' Damage units by heavy rain');
LineOut (Derived.WaterLimit, ' Water limitation term');
LineOut (Derived.DecompLimit, ' Water limitation term for decomposition');
LineOut (Derived.LeafGrowth, ' Seasonal capacity to convert foliage reserves into new foliage');
LineOut (Derived.Deciduous, ' Seasonal foliage shedding term');
LineOut (Derived.ExcessN, ' Nitrogen in excess of the ability of trees to absorb it');
LineOut (Derived.RespnBase, ' Base rate for respiration rate (for temperature adjustments)');
LineOut (Derived.HeatSum, ' Heat sum');
LineOut (Derived.AnnualMeanTemp, ' Annual mean temperature');
LineOut (Weather.LastMin, ' Previous day minimum temperature');
LineOut (Weather.LastAbsHum, ' Previous day absolute humidity');
LineOut (Weather.LastRelHum, ' Previous day relative humidity');
LineOut (Derived.SpringMaxTemp, ' Average spring-time maximum temperature');
LineOut (Derived.SpringTempSum, ' Cumulative spring time temperatures');
IntegerOut (Derived.SpringTempCounter, ' Counter for calculating the next spring-time temperature');
LineOut (Weather.TSoil, ' Soil temperature');
LineOut (Plant.WeedHeight, ' Height of the weed layer');
IntegerOut (Control.NextPhenology, ' Sequence counter for phenology settings');
IntegerOut (Control.PhenologyDayCount, ' Number of days for which the current phenological setting has been operative (if that is the critical element)');
Writeln (fp, '(Dummy) Previous values for various counters');
WriteCounters (Derived.LastYearsWood, 9);
WriteCounters (Derived.LastYearsDecomp, 5);
WriteCounters (Derived.NPPCount, 5);
WriteCounters (Derived.Temperature, 6);
Close(fp);
End; {of Procedure 'SavePools'}

{ --- end of file untLoadSaveInitial.PAS ------------------------------------------ }

End.

