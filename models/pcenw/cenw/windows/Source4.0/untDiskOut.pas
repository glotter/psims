{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : SaveAnyInfo                                      =
  =             SaveAtStart                                      =
  =             SaveRegular                                      =
  =                                                              =
  =             Routines to save data in an ASCII file           =
  =             during normal runs.                              =
  ================================================================
  = File      : untDiskOut.PAS                                   =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

Unit untDiskOut;

INTERFACE

Uses
  untDeclarations, untMiscellaneous, untFieldValidation;

Procedure SaveAnyInfo;
Procedure SaveAtStart (FileName: FileNameType);
Procedure SaveRegular;

IMPLEMENTATION

Procedure SaveAnyInfo;
var SaveItem: SaveVariableOptions;
Label ExitLoop;
Begin
For SaveItem := S_Year to S_Evaporation do
    If SaveVar.Choose[SaveItem] then
       Begin
       Control.DataToBeSaved := true;
       Goto ExitLoop;
       End;
Control.DataToBeSaved := false;
ExitLoop:
End; {of Procedure 'SaveAnyInfo'}

Procedure SaveAtStart (FileName: FileNameType);
var iLine: Integer;

    Procedure HeadingOut (Heading: string);
    Begin
    If Length(Heading) > MaxWidth then
       Heading := Copy(Heading, 1, MaxWidth);
    Write (Control.CenwFileOut, Heading: MaxWidth, ' ');
    End;  {of Procedure 'HeadingOut'}

    Procedure Heading2Out (Heading: string);
    Begin
    HeadingOut (Heading);
    HeadingOut (Heading);
    End;  {of Procedure 'Heading2Out'}

    Procedure HeadingLayerOut (Heading: string);
    var i: Integer;
        LayerHeading: String;
    Begin
    For i := 0 to SoilOrganic.nLayers do
        Begin
        LayerHeading := Heading;
        If Length(LayerHeading) > MaxWidth then
           LayerHeading := Copy(LayerHeading, 1, MaxWidth);
        Write (Control.CenwFileOut, LayerHeading: MaxWidth, ' ');
        End;
    End;  {of Procedure 'HeadingLayerOut'}

    Procedure HeadingLayerWaterOut (Heading: string);
    var i: Integer;
        LayerHeading: String;
    Begin
    For i := 1 to SoilWat.nLayers do
        Begin
        LayerHeading := Heading;
        If Length(LayerHeading) > MaxWidth then
           LayerHeading := Copy(LayerHeading, 1, MaxWidth);
        Write (Control.CenwFileOut, LayerHeading: MaxWidth, ' ');
        End;
    End;  {of Procedure 'HeadingLayerOut'}

    Procedure LayerOut;
    var i: Integer;
        iString, LayerHeading: String;
    Begin
    If Control.OutputByLayer then
       For i := 0 to SoilOrganic.nLayers do
           Begin
           Str(i, iString);
           LayerHeading := 'Layer ' + iString;
           If Length(LayerHeading) > MaxWidth then
              LayerHeading := Copy(LayerHeading, 1, MaxWidth);
           Write (Control.CenwFileOut, LayerHeading: MaxWidth, ' ');
           End
    Else // Only surface and one bulked soil layer
       Begin
       Write (Control.CenwFileOut, 'Surface': MaxWidth, ' ');
       Write (Control.CenwFileOut, 'Soil': MaxWidth, ' ');
       End;
    End;  {of Procedure 'HeadingLayerOut'}

    Procedure LayerWaterOut;
    var i: Integer;
        iString, LayerHeading: String;
    Begin
    If Control.OutputByLayer then
       For i := 1 to SoilWat.nLayers do
           Begin
           Str(i, iString);
           LayerHeading := 'Layer ' + iString;
           If Length(LayerHeading) > MaxWidth then
              LayerHeading := Copy(LayerHeading, 1, MaxWidth);
           Write (Control.CenwFileOut, LayerHeading: MaxWidth, ' ');
           End
    Else // Only bulked soil
       Write (Control.CenwFileOut, 'Soil': MaxWidth, ' ');
    End;  {of Procedure 'HeadingLayerOut'}

    Procedure AllHeadings (IndexNr: Integer; Str1, Str2, Str3, Str4: String);
    Begin
    Case IndexNr of
         1: HeadingOut (Str1);
         2: HeadingOut (Str2);
         3: HeadingOut (Str3);
         4: HeadingOut (Str4);
         End;
    End; {of Procedure 'AllHeadings'}

    Procedure AllLayerHeadings (IndexNr: Integer; Str1, Str2, Str3: String);
    Begin
    If Control.OutputByLayer then
       Case IndexNr of
            1: HeadingLayerOut (Str1);
            2: HeadingLayerOut (Str2);
            3: HeadingLayerOut (Str3);
            4: LayerOut;
            End
    Else
       Case IndexNr of
            1: Heading2Out (Str1);
            2: Heading2Out (Str2);
            3: Heading2Out (Str3);
            4: LayerOut;
            End;
    End; {of Procedure 'AllLayerHeadings'}

Begin
For iLine := 1 to 4 do
    Begin
    If SaveVar.Choose[S_Year] then AllHeadings(iLine, 'Years', '', '', '');
    If SaveVar.Choose[S_Month] then AllHeadings(iLine, 'Month', '', '', '');
    If SaveVar.Choose[S_Day] then AllHeadings(iLine, 'Day', '', '', '');
    If SaveVar.Choose[S_LAI] then AllHeadings(iLine, 'LAI', '', '', '');
    If SaveVar.Choose[S_Height] then AllHeadings(iLine, 'Height', '', 'm', '');
    If SaveVar.Choose[S_DBH] then AllHeadings(iLine, 'DBH', '', 'cm', '');
    If SaveVar.Choose[S_CanopyCover] then AllHeadings(iLine, 'Canopy cover', 'fract', '', '');
    If SaveVar.Choose[S_kex] then AllHeadings(iLine, 'Extinction coeff.', '', '', '');
    If SaveVar.Choose[S_BasalArea] then AllHeadings(iLine, 'Bs area', '', 'm2 ha-1', '');
    If SaveVar.Choose[S_Stocking] then AllHeadings(iLine, 'Stocking', '', 'n ha-1', '');
    If SaveVar.Choose[S_SapWoodC] then AllHeadings(iLine, 'SapWood', 'DW', 't ha-1', '');
    If SaveVar.Choose[S_HeartWoodC] then AllHeadings(iLine, 'Ht-Wood', 'DW', 't ha-1', '');
    If SaveVar.Choose[S_BarkC] then AllHeadings(iLine, 'Bark', 'DW', 't ha-1', '');
    If SaveVar.Choose[S_LeafC] then AllHeadings(iLine, 'Foliage', 'DW', 't ha-1', '');
    If SaveVar.Choose[S_FineRootC] then AllHeadings(iLine, 'FineRt', 'DW', 't ha-1', '');
    If SaveVar.Choose[S_CoarseRootC] then AllHeadings(iLine, 'Crs-Rt', 'DW', 't ha-1', '');
    If SaveVar.Choose[S_BranchesC] then AllHeadings(iLine, 'Branch', 'DW', 't ha-1', '');
    If SaveVar.Choose[S_ReprodC] then AllHeadings(iLine, 'Reprod', 'DW', 't ha-1', '');
    If SaveVar.Choose[S_CH2O] then AllHeadings(iLine, 'CH2O', 'DW', 't ha-1', '');
    If SaveVar.Choose[S_Reserves] then AllHeadings(iLine, 'Reserves', 'DW', 't ha-1', '');
    If SaveVar.Choose[S_SapWoodN] then AllHeadings(iLine, 'SapWd_N', 'Nitro.', 'kg ha-1', '');
    If SaveVar.Choose[S_HeartWoodN] then AllHeadings(iLine, 'Ht-Wd_N', 'Nitro.', 'kg ha-1', '');
    If SaveVar.Choose[S_BarkN] then AllHeadings(iLine, 'Bark_N', 'Nitro.', 'kg ha-1', '');
    If SaveVar.Choose[S_LeafN] then AllHeadings(iLine, 'Leaf_N', 'Nitro.', 'kg ha-1', '');
    If SaveVar.Choose[S_FineRootN] then AllHeadings(iLine, 'FnRt_N', 'Nitro.', 'kg ha-1', '');
    If SaveVar.Choose[S_CoarseRootN] then AllHeadings(iLine, 'Crs-Rt_N', 'Nitro.', 'kg ha-1', '');
    If SaveVar.Choose[S_BranchesN] then AllHeadings(iLine, 'BranchN', 'Nitro.', 'kg ha-1', '');
    If SaveVar.Choose[S_ReprodN] then AllHeadings(iLine, 'Reprod_N', 'Nitro.', 'kg ha-1', '');
    If SaveVar.Choose[S_SolubleN] then AllHeadings(iLine, 'Solub_N', 'Nitro.', 'kg ha-1', '');
    If SaveVar.Choose[S_Nlimit] then AllHeadings(iLine, 'N limit', '', '', '');
    If SaveVar.Choose[S_TotalN] then AllHeadings(iLine, 'Total N', 'Nitro.', 'kg ha-1', '');
    If SaveVar.Choose[S_ReservesN] then AllHeadings(iLine, 'N Reserves', 'Nitro.', 'kg ha-1', '');
    If SaveVar.Choose[S_NConc] then AllHeadings(iLine, '[N]', 'N/DW', 'g kg-1', '');
    If SaveVar.Choose[S_NBiol] then AllHeadings(iLine, 'N biol', 'Nitro.', 'kg/ha/d', '');
    If SaveVar.Choose[S_pi] then AllHeadings(iLine, 'pi', '', 'ubar', '');
    If SaveVar.Choose[S_CAI] then AllHeadings(iLine, 'CAI', 'DW', 't/ha/yr', '');
    If SaveVar.Choose[S_NPP] then AllHeadings(iLine, 'NPP', 'DW', 't/ha/yr', '');
    If SaveVar.Choose[S_NEE] then AllHeadings(iLine, 'NEE', 'DW', 'kg/ha/d', '');
    If SaveVar.Choose[S_CarbonGain] then AllHeadings(iLine, 'C Assim', 'DW', 'kg/ha/d', '');
    If SaveVar.Choose[S_Respn] then AllHeadings(iLine, 'Respn', 'DW', 'kg/ha/d', '');
    If SaveVar.Choose[S_DayCFlux] then AllHeadings(iLine, 'D C Flux', 'DW', 'kg/ha/d', '');
    If SaveVar.Choose[S_NightCFlux] then AllHeadings(iLine, 'N C Flux', 'DW', 'kg/ha/d', '');
    If SaveVar.Choose[S_TDamage] then
       Begin
       AllHeadings(iLine, 'TDamage', '', 'oC', '');
       AllHeadings(iLine, 'TD Units', '', '', '');
       End;
    If SaveVar.Choose[S_Transpiration] then AllHeadings(iLine, 'Transp.', '', 'mm', '');
    If SaveVar.Choose[S_CLeafLitterFall] then AllHeadings(iLine, 'C_Lf lit', 'DW', 'kg/ha/d', '');
    If SaveVar.Choose[S_NLeafLitterFall] then AllHeadings(iLine, 'N_Lf lit', 'Nitro.', 'kg/ha/d', '');
    If SaveVar.Choose[S_CLitterFall] then AllHeadings(iLine, 'C_All lit', 'DW', 'kg/ha/d', '');
    If SaveVar.Choose[S_NLitterFall] then AllHeadings(iLine, 'N_All lit', 'Nitro.', 'kg/ha/d', '');
    If SaveVar.Choose[S_CAgLitterFall] then AllHeadings(iLine, 'C_A-g lit', 'DW', 'kg/ha/d', '');
    If SaveVar.Choose[S_NAgLitterFall] then AllHeadings(iLine, 'N_A-g lit', 'Nitro.', 'kg/ha/d', '');
    If SaveVar.Choose[S_NLeached] then AllHeadings(iLine, 'N_Leached', 'Nitro.', 'kg/ha/d', '');
    If SaveVar.Choose[S_HeteroRespn] then AllHeadings(iLine, 'Het. respn', 'DW', 'kg/ha/d', '');
    If SaveVar.Choose[S_SoilRespn] then AllHeadings(iLine, 'Soil respn', 'DW', 'kg/ha/d', '');
    If SaveVar.Choose[S_Tmax] then AllHeadings(iLine, 'Tmax', '', 'deg', '');
    If SaveVar.Choose[S_Tmin] then AllHeadings(iLine, 'Tmin', '', 'deg', '');
    If SaveVar.Choose[S_Tmean] then AllHeadings(iLine, 'Tmean', '', 'deg', '');
    If SaveVar.Choose[S_Tsoil] then AllHeadings(iLine, 'Tsoil', '', 'deg', '');
    If SaveVar.Choose[S_Tday] then AllHeadings(iLine, 'Tday', '', 'deg', '');
    If SaveVar.Choose[S_Radn] then AllHeadings(iLine, 'Radn', '', 'MJ/m2/d', '');
    If SaveVar.Choose[S_Rain] then AllHeadings(iLine, 'Rain', 'Water', 'mm d-1', '');
    If SaveVar.Choose[S_AbsHum] then AllHeadings(iLine, 'AbsHum', '', 'mbar', '');
    If SaveVar.Choose[S_RelHum] then AllHeadings(iLine, 'RelHum', '', '%', '');
    If SaveVar.Choose[S_WaterLimit] then AllHeadings(iLine, 'WatLim', '', '', '');
    If SaveVar.Choose[S_RelDecomp] then AllHeadings(iLine, 'Decomp', '', 'relat.', '');
    If SaveVar.Choose[S_Snow] then AllHeadings(iLine, 'Snow', 'Water', 'mm', '');
    If SaveVar.Choose[S_Drainage] then AllHeadings(iLine, 'Drainage', 'Water', 'mm d-1', '');
    If SaveVar.Choose[S_HeatSum] then AllHeadings(iLine, 'Heat sum', '', 'oC days', '');
    If SaveVar.Choose[S_Evaporation] then AllHeadings(iLine, 'Evap', 'Water', 'mm d-1', '');
    If SaveVar.Choose[S_NecroC] then AllHeadings(iLine, 'Necro C', 'Carbon', 't ha-1', '');
    If SaveVar.Choose[S_NecroN] then AllHeadings(iLine, 'Necro N', 'Nitro.', 'kg ha-1', '');
    If SaveVar.Choose[S_CMetabolic] then AllLayerHeadings(iLine, 'CMetab', 'Carbon', 't ha-1');
    If SaveVar.Choose[S_NMetabolic] then AllLayerHeadings(iLine, 'NMetab', 'Nitro.', 'kg ha-1');
    If SaveVar.Choose[S_CStructural] then AllLayerHeadings(iLine, 'CStruct', 'Carbon', 't ha-1');
    If SaveVar.Choose[S_NStructural] then AllLayerHeadings(iLine, 'NStruct', 'Nitro.', 'kg ha-1');
    If SaveVar.Choose[S_CWoodyLitter] then AllLayerHeadings(iLine, 'CWdyLt', 'Carbon', 't ha-1');
    If SaveVar.Choose[S_NWoodyLitter] then AllLayerHeadings(iLine, 'NWdyLt', 'Nitro.', 'kg ha-1');
    If SaveVar.Choose[S_CActive] then AllLayerHeadings(iLine, 'C_Actv', 'Carbon', 't ha-1');
    If SaveVar.Choose[S_NActive] then AllLayerHeadings(iLine, 'N_Actv', 'Nitro.', 'kg ha-1');
    If SaveVar.Choose[S_CSlow] then AllLayerHeadings(iLine, 'C_Slow', 'Carbon', 't ha-1');
    If SaveVar.Choose[S_NSlow] then AllLayerHeadings(iLine, 'N_Slow', 'Nitro.', 'kg ha-1');
    If SaveVar.Choose[S_CResistant] then AllLayerHeadings(iLine, 'C_Resist', 'Carbon', 't ha-1');
    If SaveVar.Choose[S_NResistant] then AllLayerHeadings(iLine, 'N_Resist', 'Nitro.', 'kg ha-1');
    If SaveVar.Choose[S_CInert] then AllLayerHeadings(iLine, 'C_Inert', 'Carbon', 't ha-1');
    If SaveVar.Choose[S_NInert] then AllLayerHeadings(iLine, 'N_Inert', 'Nitro.', 'kg ha-1');
    If SaveVar.Choose[S_NMineral] then AllLayerHeadings(iLine, 'N_Minl', 'Nitro.', 'kg ha-1');
    If SaveVar.Choose[S_StoredWater] then AllLayerHeadings(iLine, 'SoilWat', '', 'mm');
    If Control.IncludeP then
       Begin
       If SaveVar.Choose[S_SolubleP] then AllHeadings(iLine, 'Solub P', 'Phosphorus', 'kg ha-1', '');
       If SaveVar.Choose[S_SapWoodP] then AllHeadings(iLine, 'Sapwd P', 'Phosphorus', 'kg ha-1', '');
       If SaveVar.Choose[S_HeartWoodP] then AllHeadings(iLine, 'Hrtwd P', 'Phosphorus', 'kg ha-1', '');
       If SaveVar.Choose[S_LeafP] then AllHeadings(iLine, 'Leaf P', 'Phosphorus', 'kg ha-1', '');
       If SaveVar.Choose[S_FineRootP] then AllHeadings(iLine, 'Fnrt P', 'Phosphorus', 'kg ha-1', '');
       If SaveVar.Choose[S_BarkP] then AllHeadings(iLine, 'Bark P', 'Phosphorus', 'kg ha-1', '');
       If SaveVar.Choose[S_CoarseRootP] then AllHeadings(iLine, 'Csrt P', 'Phosphorus', 'kg ha-1', '');
       If SaveVar.Choose[S_BranchesP] then AllHeadings(iLine, 'Branch P', 'Phosphorus', 'kg ha-1', '');
       If SaveVar.Choose[S_ReprodP] then AllHeadings(iLine, 'Reprod P', 'Phosphorus', 'kg ha-1', '');
       If SaveVar.Choose[S_ReservesP] then AllHeadings(iLine, 'Reserv P', 'Phosphorus', 'kg ha-1', '');
       If SaveVar.Choose[S_TotalOrganicP] then AllHeadings(iLine, 'Organic P', 'Phosphorus', 'kg ha-1', '');
       If SaveVar.Choose[S_Plimit] then AllHeadings(iLine, 'P limit', '', '', '');
       If SaveVar.Choose[S_TotalP] then AllHeadings(iLine, 'Total P', 'Phosphorus', 'kg ha-1', '');
       If SaveVar.Choose[S_NecroP] then AllHeadings(iLine, 'Necro P', 'Phosphorus', 'kg ha-1', '');
       If SaveVar.Choose[S_MetabolicP] then AllLayerHeadings(iLine, 'Metab P', 'Phosphorus', 'kg ha-1');
       If SaveVar.Choose[S_StructuralP] then AllLayerHeadings(iLine, 'Struct P', 'Phosphorus', 'kg ha-1');
       If SaveVar.Choose[S_WoodyLitterP] then AllLayerHeadings(iLine, 'Wd lit P', 'Phosphorus', 'kg ha-1');
       If SaveVar.Choose[S_ActiveP] then AllLayerHeadings(iLine, 'Active P', 'Phosphorus', 'kg ha-1');
       If SaveVar.Choose[S_SlowP] then AllLayerHeadings(iLine, 'Slow P', 'Phosphorus', 'kg ha-1');
       If SaveVar.Choose[S_ResistantP] then AllLayerHeadings(iLine, 'Resist P', 'Phosphorus', 'kg ha-1');
       If SaveVar.Choose[S_InertP] then AllLayerHeadings(iLine, 'Inert P', 'Phosphorus', 'kg ha-1');
       If SaveVar.Choose[S_RockP] then AllLayerHeadings(iLine, 'Rock P', 'Phosphorus', 'kg ha-1');
       If SaveVar.Choose[S_OccludedP] then AllLayerHeadings(iLine, 'Occluded', 'Phosphorus', 'kg ha-1');
       If SaveVar.Choose[S_2ndaryP] then AllLayerHeadings(iLine, '2ndary P', 'Phosphorus', 'kg ha-1');
       If SaveVar.Choose[S_MineralP] then AllLayerHeadings(iLine, 'Min P', 'Phosphorus', 'kg ha-1');
       End;
    If SaveVar.Choose[S_Dummy] then AllHeadings(iLine, 'Dummy', '', '', '');
    Writeln (Control.CenwFileOut);
    End;
End; {of Procedure 'SaveInitial'}

Procedure SaveRegular;
Type MaterialType = array[ElementsUsed] of Real48;
var iLayer: integer;
    Sum: Real48;
    LayerSum: SoilElements;
    SumNecro: MaterialType;

    Procedure DatumOut (Datum, Multiple: real48);
       var Width, Digits: integer;
       Begin
       Datum := Datum * Multiple;
       GetField(Datum, MaxWidth, Width, Digits);
       Write (Control.CenwFileOut, Datum: Width: Digits, ' ');
       End;  {of Procedure 'DatumOut'}

    Procedure IntOut (Datum: Integer);
       Begin
       Write (Control.CenwFileOut, Datum: MaxWidth, ' ');
       End;  {of Procedure 'IntOut'}

    Procedure GetNecroSum(var SumNecro: MaterialType);
    var Elements: ElementsUsed;
        iLayer: Integer;
    Begin
    for Elements := C to P do
        Begin
        SumNecro[Elements] := 0;
        for iLayer := 0 to SoilOrganic.nLayers do
            SumNecro[Elements] := SumNecro[Elements] +
            SoilOrganic.FineWood[iLayer, Elements] +
            SoilOrganic.CoarseWood[iLayer, Elements] +
            SoilOrganic.Struct[iLayer, Elements] +
            SoilOrganic.Metab[iLayer, Elements] +
            SoilOrganic.Active[iLayer, Elements] +
            SoilOrganic.Slow[iLayer, Elements] +
            SoilOrganic.Resistant[iLayer, Elements] +
            SoilOrganic.Inert[iLayer, Elements] +
            SoilOrganic.Soluble[iLayer, Elements];
        End;
    End; {of Procedure 'GetNecroSum'}

    Procedure LayerOut (Data: SoilElements; E: ElementsUsed; WithLayer: Boolean; Multiple: Real48);
    var iLayer: Integer;
        Sum: Real48;
    Begin
    If WithLayer then
       Begin
       For iLayer := 0 to SoilOrganic.nLayers do
           DatumOut (Data[iLayer, E], Multiple);
       End
    Else
       Begin
       DatumOut (Data[0, E], Multiple);
       Sum := 0;
       For iLayer:= 1 to SoilOrganic.nLayers do
           Sum := Sum + Data[iLayer, E];
       DatumOut (Sum, Multiple);
       End;
    End;  {of Procedure 'LayerOut'}

Begin
      If SaveVar.Choose[S_Year] then IntOut(Control.TotalYears);
      If SaveVar.Choose[S_Month] then IntOut(Control.ExtraMonths);
      If SaveVar.Choose[S_Day] then IntOut(Control.ExtraDays);
      If SaveVar.Choose[S_LAI] then DatumOut(Derived.LAI[Over], 1);
      If SaveVar.Choose[S_Height] then DatumOut(Plant.Height, 1);
      If SaveVar.Choose[S_DBH] then DatumOut(Plant.DBH, 1);
      If SaveVar.Choose[S_CanopyCover] then DatumOut(Plant.CanopyCover, 1);
      If SaveVar.Choose[S_kex] then DatumOut(Plant.kex, 1);
      If SaveVar.Choose[S_BasalArea] then DatumOut(Plant.Area, 0.0001);
      If SaveVar.Choose[S_Stocking] then DatumOut(Plant.Stocking, 1);
      If SaveVar.Choose[S_SapWoodC] then DatumOut(Plant.SapWood[C], Control.CConversion * 0.001);
      If SaveVar.Choose[S_HeartWoodC] then DatumOut(Plant.HeartWood[C], Control.CConversion * 0.001);
      If SaveVar.Choose[S_BarkC] then DatumOut(Plant.Bark[C], Control.CConversion * 0.001);
      If SaveVar.Choose[S_LeafC] then DatumOut(Plant.Leaves[C], Control.CConversion * 0.001);
      If SaveVar.Choose[S_FineRootC] then DatumOut(Plant.FineRoot[C], Control.CConversion * 0.001);
      If SaveVar.Choose[S_CoarseRootC] then DatumOut(Plant.CoarseRoot[C], Control.CConversion * 0.001);
      If SaveVar.Choose[S_BranchesC] then DatumOut(Plant.Branches[C], Control.CConversion * 0.001);
      If SaveVar.Choose[S_ReprodC] then DatumOut((Plant.Fruit[C] + Plant.Pollen[C]), Control.CConversion * 0.001);
      If SaveVar.Choose[S_CH2O] then DatumOut(Plant.Soluble[C], Control.CConversion * 0.001);
      If SaveVar.Choose[S_Reserves] then DatumOut(Plant.Reserves[C], Control.CConversion * 0.001);
      If SaveVar.Choose[S_SapWoodN] then DatumOut(Plant.SapWood[N], Control.NConversion);
      If SaveVar.Choose[S_HeartWoodN] then DatumOut(Plant.HeartWood[N], Control.NConversion);
      If SaveVar.Choose[S_BarkN] then DatumOut(Plant.Bark[N], Control.NConversion);
      If SaveVar.Choose[S_LeafN] then DatumOut(Plant.Leaves[N], Control.NConversion);
      If SaveVar.Choose[S_FineRootN] then DatumOut(Plant.FineRoot[N], Control.NConversion);
      If SaveVar.Choose[S_CoarseRootN] then DatumOut(Plant.CoarseRoot[N], Control.NConversion);
      If SaveVar.Choose[S_BranchesN] then DatumOut(Plant.Branches[N], Control.NConversion);
      If SaveVar.Choose[S_ReprodN] then DatumOut((Plant.Fruit[N] + Plant.Pollen[N]), Control.NConversion);
      If SaveVar.Choose[S_SolubleN] then DatumOut(Plant.Soluble[N], Control.NConversion);
      If SaveVar.Choose[S_Nlimit] then DatumOut(Derived.Nlimit, 1);
      If SaveVar.Choose[S_TotalN] then DatumOut(TotalN, Control.NConversion);
      If SaveVar.Choose[S_ReservesN] then DatumOut(Plant.Reserves[N], Control.NConversion);
      If SaveVar.Choose[S_NConc] then DatumOut(1000 * Derived.NConc[Over], Control.NConversion / Control.CConversion);
      If SaveVar.Choose[S_NBiol] then DatumOut(Derived.NBiol, 1);
      If SaveVar.Choose[S_pi] then DatumOut(Derived.p_internal, 1);
      If SaveVar.Choose[S_CAI] then DatumOut(Derived.CAI, Control.CConversion * 0.001);
      If SaveVar.Choose[S_NPP] then DatumOut(Derived.NPP, Control.CConversion * 0.001);
      If SaveVar.Choose[S_NEE] then DatumOut(Derived.NEE, Control.CConversion);
      If SaveVar.Choose[S_CarbonGain] then DatumOut(Derived.CarbonGain, Control.CConversion);
      If SaveVar.Choose[S_Respn] then DatumOut(Derived.Rm + Derived.Rg, Control.CConversion);
      If SaveVar.Choose[S_DayCFlux] then DatumOut(Derived.DayCFlux, Control.CConversion);
      If SaveVar.Choose[S_NightCFlux] then DatumOut(Derived.NightCFlux, Control.CConversion);
      If SaveVar.Choose[S_TDamage] then
                                   Begin
                                   DatumOut(Derived.TDamage, 1);
                                   DatumOut(Derived.TDamageUnits, 1);
                                   End;
      If SaveVar.Choose[S_Transpiration] then DatumOut(Derived.Transpiration, 1);
      If SaveVar.Choose[S_CLeafLitterFall] then DatumOut(Litter.Leaves[C], Control.CConversion);
      If SaveVar.Choose[S_NLeafLitterFall] then DatumOut(Litter.Leaves[N], Control.NConversion);
      If SaveVar.Choose[S_CLitterFall] then DatumOut(Litter.Leaves[C] + Litter.CoarseWood[C] + Litter.FineWood[C]
                                      + Litter.Other[C] + Litter.CoarseRoot[C] + Litter.FineRoot[C], Control.CConversion);
      If SaveVar.Choose[S_NLitterFall] then DatumOut(Litter.Leaves[N] + Litter.CoarseWood[N] + Litter.FineWood[N]
                                      + Litter.Other[N] + Litter.CoarseRoot[N] + Litter.FineRoot[N], Control.NConversion);
      If SaveVar.Choose[S_CAgLitterFall] then DatumOut(Litter.Leaves[C] + Litter.CoarseWood[C] + Litter.FineWood[C]
                                      + Litter.Other[C], Control.CConversion);
      If SaveVar.Choose[S_NAgLitterFall] then DatumOut(Litter.Leaves[N] + Litter.CoarseWood[N] + Litter.FineWood[N]
                                      + Litter.Other[N], Control.NConversion);;
      If SaveVar.Choose[S_NLeached] then DatumOut(Derived.NLeached, 1);
      If SaveVar.Choose[S_HeteroRespn] then DatumOut(Derived.HeteroRespn, Control.CConversion);
      If SaveVar.Choose[S_SoilRespn] then DatumOut(Derived.SoilRespn, Control.CConversion);
      If SaveVar.Choose[S_Tmax] then DatumOut(Weather.Tmax, 1);
      If SaveVar.Choose[S_Tmin] then DatumOut(Weather.Tmin, 1);
      If SaveVar.Choose[S_Tmean] then DatumOut(Weather.Tmean, 1);
      If SaveVar.Choose[S_Tsoil] then DatumOut(Weather.Tsoil, 1);
      If SaveVar.Choose[S_Tday] then DatumOut(Weather.Tday, 1);
      If SaveVar.Choose[S_Radn] then DatumOut(Weather.Radn, 1);
      If SaveVar.Choose[S_Rain] then DatumOut(Weather.Rain, 1);
      If SaveVar.Choose[S_AbsHum] then DatumOut(1000 * Weather.AbsHumidity, 1);
      If SaveVar.Choose[S_RelHum] then DatumOut(100 * Weather.RelHumidity, 1);
      If SaveVar.Choose[S_WaterLimit] then DatumOut(Derived.WaterLimit, 1);
      If SaveVar.Choose[S_RelDecomp] then DatumOut(Derived.LastYearsDecomp[DecompCount], 1);
      If SaveVar.Choose[S_Snow] then DatumOut(SoilWat.Snow, 1);
      If SaveVar.Choose[S_Drainage] then DatumOut(Derived.Drainage[SoilWat.nLayers], 1);
      If SaveVar.Choose[S_HeatSum] then DatumOut(Derived.HeatSum, 1);
      If SaveVar.Choose[S_Evaporation] then DatumOut(Derived.Evaporation, 1);
      If SaveVar.Choose[S_NecroC] then
         Begin
         GetNecroSum(SumNecro);
         DatumOut(SumNecro[C], 0.001);
         End;
      If SaveVar.Choose[S_NecroN] then
         Begin
         GetNecroSum(SumNecro);
         DatumOut(SumNecro[N], 1);
         End;
      If SaveVar.Choose[S_CMetabolic] then LayerOut(SoilOrganic.Metab, C, Control.OutputByLayer, 0.001);
      If SaveVar.Choose[S_NMetabolic] then LayerOut(SoilOrganic.Metab, N, Control.OutputByLayer, 1);
      If SaveVar.Choose[S_CStructural] then LayerOut(SoilOrganic.Struct, C, Control.OutputByLayer, 0.001);
      If SaveVar.Choose[S_NStructural] then LayerOut(SoilOrganic.Struct, N, Control.OutputByLayer, 1);
      If SaveVar.Choose[S_CWoodyLitter] then
         Begin
         For iLayer := 0 to SoilOrganic.nLayers do
             LayerSum[iLayer, C] := SoilOrganic.FineWood[iLayer, C] +
                                 SoilOrganic.CoarseWood[iLayer, C];
         LayerOut(LayerSum, C, Control.OutputByLayer, 0.001);
         End;
      If SaveVar.Choose[S_NWoodyLitter] then
         Begin
         For iLayer := 0 to SoilOrganic.nLayers do
             LayerSum[iLayer, N] := SoilOrganic.FineWood[iLayer, N] +
                                 SoilOrganic.CoarseWood[iLayer, N];
         LayerOut(LayerSum, N, Control.OutputByLayer, 1);
         End;
      If SaveVar.Choose[S_CActive] then LayerOut(SoilOrganic.Active, C, Control.OutputByLayer, 0.001);
      If SaveVar.Choose[S_NActive] then LayerOut(SoilOrganic.Active, N, Control.OutputByLayer, 1);
      If SaveVar.Choose[S_CSlow] then LayerOut(SoilOrganic.Slow, C, Control.OutputByLayer, 0.001);
      If SaveVar.Choose[S_NSlow] then LayerOut(SoilOrganic.Slow, N, Control.OutputByLayer, 1);
      If SaveVar.Choose[S_CResistant] then LayerOut(SoilOrganic.Resistant, C, Control.OutputByLayer, 0.001);
      If SaveVar.Choose[S_NResistant] then LayerOut(SoilOrganic.Resistant, N, Control.OutputByLayer, 1);
      If SaveVar.Choose[S_CInert] then LayerOut(SoilOrganic.Inert, C, Control.OutputByLayer, 0.001);
      If SaveVar.Choose[S_NInert] then LayerOut(SoilOrganic.Inert, N, Control.OutputByLayer, 1);
      If SaveVar.Choose[S_NMineral] then LayerOut(SoilOrganic.Soluble, N, Control.OutputByLayer, 1);
      If SaveVar.Choose[S_StoredWater] then
         If Control.OutputByLayer then
            For iLayer := 0 to SoilWat.nLayers do
                DatumOut(SoilWat.Layer[iLayer].WaterContent, 1)
         Else
            Begin
            DatumOut(SoilWat.Layer[0].WaterContent, 1);
            Sum := 0;
            For iLayer := 1 to SoilWat.nLayers do
                Sum := Sum + SoilWat.Layer[iLayer].WaterContent;
            DatumOut(Sum, 1);
            End;
      If Control.IncludeP then
         Begin
         If SaveVar.Choose[S_SolubleP] then DatumOut(Plant.Soluble[P], 1);
         If SaveVar.Choose[S_SapWoodP] then DatumOut(Plant.SapWood[P], 1);
         If SaveVar.Choose[S_HeartWoodP] then DatumOut(Plant.HeartWood[P], 1);
         If SaveVar.Choose[S_LeafP] then DatumOut(Plant.Leaves[P], 1);
         If SaveVar.Choose[S_FineRootP] then DatumOut(Plant.FineRoot[P], 1);
         If SaveVar.Choose[S_BarkP] then DatumOut(Plant.Bark[P], 1);
         If SaveVar.Choose[S_CoarseRootP] then DatumOut(Plant.CoarseRoot[P], 1);
         If SaveVar.Choose[S_BranchesP] then DatumOut(Plant.Branches[P], 1);
         If SaveVar.Choose[S_ReprodP] then DatumOut(Plant.Fruit[P] + Plant.Pollen[P], 1);
         If SaveVar.Choose[S_ReservesP] then DatumOut(Plant.Reserves[P], 1);
         If SaveVar.Choose[S_TotalOrganicP] then DatumOut(TotalOrganicP, 1);
         If SaveVar.Choose[S_Plimit] then DatumOut(Derived.Plimit, 1);
         If SaveVar.Choose[S_TotalP] then DatumOut(TotalP, 1);
         If SaveVar.Choose[S_NecroP] then
            Begin
            GetNecroSum(SumNecro);
            DatumOut(SumNecro[P], 1);
            End;
         If SaveVar.Choose[S_MetabolicP] then LayerOut(SoilOrganic.Metab, P, Control.OutputByLayer, 1);
         If SaveVar.Choose[S_StructuralP] then LayerOut(SoilOrganic.Struct, P, Control.OutputByLayer, 1);
         If SaveVar.Choose[S_WoodyLitterP] then
            Begin
            For iLayer := 0 to SoilOrganic.nLayers do
                LayerSum[iLayer, P] := SoilOrganic.FineWood[iLayer, P] +
                                 SoilOrganic.CoarseWood[iLayer, P];
            LayerOut(LayerSum, P, Control.OutputByLayer, 1);
            End;
         If SaveVar.Choose[S_ActiveP] then LayerOut(SoilOrganic.Active, P, Control.OutputByLayer, 1);
         If SaveVar.Choose[S_SlowP] then LayerOut(SoilOrganic.Slow, P, Control.OutputByLayer, 1);
         If SaveVar.Choose[S_ResistantP] then LayerOut(SoilOrganic.Resistant, P, Control.OutputByLayer, 1);
         If SaveVar.Choose[S_InertP] then LayerOut(SoilOrganic.Inert, P, Control.OutputByLayer, 1);
         If SaveVar.Choose[S_RockP] then LayerOut(SoilOrganic.RockP, P, Control.OutputByLayer, 1);
         If SaveVar.Choose[S_OccludedP] then LayerOut(SoilOrganic.OccludedP, P, Control.OutputByLayer, 1);
         If SaveVar.Choose[S_2ndaryP] then LayerOut(SoilOrganic.SecondaryInorganicP, P, Control.OutputByLayer, 1);
         If SaveVar.Choose[S_MineralP] then LayerOut(SoilOrganic.Soluble, P, Control.OutputByLayer, 1);
         End;
      If SaveVar.Choose[S_Dummy] then DatumOut(Derived.Dummy, 1);
      Writeln (Control.CenwFileOut);
End; {of Procedure 'SaveRegular'}

End.

{ --- end of file DISKOUT.PAS ------------------------------------------ }

