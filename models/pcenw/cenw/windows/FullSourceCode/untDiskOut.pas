{ ================================================================
  = Project   : CENW                                             =
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
  = Version   : 3.1                                              =
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

    Procedure HeadingOut (Heading: string);
    Begin
    If Length(Heading) > MaxWidth then
       Heading := Copy(Heading, 1, MaxWidth);
    Write (Control.CenwFileOut, Heading: MaxWidth, ' ');
    End;  {of procedure 'HeadingOut'}

    Procedure Heading2Out (Heading: string);
    Begin
    HeadingOut (Heading);
    HeadingOut (Heading);
    End;  {of procedure 'Heading2Out'}

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
    End;  {of procedure 'HeadingLayerOut'}

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
    End;  {of procedure 'HeadingLayerOut'}

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
    End;  {of procedure 'HeadingLayerOut'}

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
    End;  {of procedure 'HeadingLayerOut'}

  Begin
  If SaveVar.Choose[S_Year] then HeadingOut('Years');
  If SaveVar.Choose[S_Month] then HeadingOut('Month');
  If SaveVar.Choose[S_Day] then HeadingOut('Day');
  If SaveVar.Choose[S_LAI] then HeadingOut('LAI');
  If SaveVar.Choose[S_Height] then HeadingOut('Height');
  If SaveVar.Choose[S_DBH] then HeadingOut('DBH');
  If SaveVar.Choose[S_CanopyCover] then HeadingOut('Canopy cover');
  If SaveVar.Choose[S_kex] then HeadingOut('Extinction coeff.');
  If SaveVar.Choose[S_BasalArea] then HeadingOut('Bs area');
  If SaveVar.Choose[S_Stocking] then HeadingOut('Stocking');
  If SaveVar.Choose[S_SapWoodC] then HeadingOut('SapWood');
  If SaveVar.Choose[S_HeartWoodC] then HeadingOut('Ht-Wood');
  If SaveVar.Choose[S_BarkC] then HeadingOut('Bark');
  If SaveVar.Choose[S_LeafC] then HeadingOut('Foliage');
  If SaveVar.Choose[S_FineRootC] then HeadingOut('FineRt');
  If SaveVar.Choose[S_CoarseRootC] then HeadingOut('Crs-Rt');
  If SaveVar.Choose[S_BranchesC] then HeadingOut('Branch');
  If SaveVar.Choose[S_ReprodC] then HeadingOut('Reprod');
  If SaveVar.Choose[S_CH2O] then HeadingOut('CH2O');
  If SaveVar.Choose[S_Reserves] then HeadingOut('Reserves');
  If SaveVar.Choose[S_SapWoodN] then HeadingOut('SapWd_N');
  If SaveVar.Choose[S_HeartWoodN] then HeadingOut('Ht-Wd_N');
  If SaveVar.Choose[S_BarkN] then HeadingOut('Bark_N');
  If SaveVar.Choose[S_LeafN] then HeadingOut('Leaf_N');
  If SaveVar.Choose[S_FineRootN] then HeadingOut('FnRt_N');
  If SaveVar.Choose[S_CoarseRootN] then HeadingOut('Crs-Rt_N');
  If SaveVar.Choose[S_BranchesN] then HeadingOut('BranchN');
  If SaveVar.Choose[S_ReprodN] then HeadingOut('Reprod_N');
  If SaveVar.Choose[S_SolubleN] then HeadingOut('Solub_N');
  If SaveVar.Choose[S_TotalN] then HeadingOut('Total N');
  If SaveVar.Choose[S_ReservesN] then HeadingOut('N Reserves');
  If SaveVar.Choose[S_NConc] then HeadingOut('[N]');
  If SaveVar.Choose[S_pi] then HeadingOut('pi');
  If SaveVar.Choose[S_CAI] then HeadingOut('CAI');
  If SaveVar.Choose[S_NPP] then HeadingOut('NPP');
  If SaveVar.Choose[S_NEE] then HeadingOut('NEE');
  If SaveVar.Choose[S_CarbonGain] then HeadingOut('C Assim');
  If SaveVar.Choose[S_Respn] then HeadingOut('Respn');
  If SaveVar.Choose[S_DayCFlux] then HeadingOut('D C Flux');
  If SaveVar.Choose[S_NightCFlux] then HeadingOut('N C Flux');
  If SaveVar.Choose[S_TDamage] then
                               Begin
                               HeadingOut('TDamage');
                               HeadingOut('TD Units');
                               End;
  If SaveVar.Choose[S_Transpiration] then HeadingOut('Transp.');
  If SaveVar.Choose[S_CLeafLitterFall] then HeadingOut('C_Lf lit');
  If SaveVar.Choose[S_NLeafLitterFall] then HeadingOut('N_Lf lit');
  If SaveVar.Choose[S_CLitterFall] then HeadingOut('C_All lit');
  If SaveVar.Choose[S_NLitterFall] then HeadingOut('N_All lit');
  If SaveVar.Choose[S_CAgLitterFall] then HeadingOut('C_A-g lit');
  If SaveVar.Choose[S_NAgLitterFall] then HeadingOut('N_A-g lit');
  If SaveVar.Choose[S_NLeached] then HeadingOut('N_Leached');
  If SaveVar.Choose[S_HeteroRespn] then HeadingOut('Het. respn');
  If SaveVar.Choose[S_SoilRespn] then HeadingOut('Soil respn');
  If SaveVar.Choose[S_Tmax] then HeadingOut('Tmax');
  If SaveVar.Choose[S_Tmin] then HeadingOut('Tmin');
  If SaveVar.Choose[S_Tmean] then HeadingOut('Tmean');
  If SaveVar.Choose[S_Tsoil] then HeadingOut('Tsoil');
  If SaveVar.Choose[S_Tday] then HeadingOut('Tday');
  If SaveVar.Choose[S_PAR] then HeadingOut('PAR');
  If SaveVar.Choose[S_Rain] then HeadingOut('Rain');
  If SaveVar.Choose[S_AbsHum] then HeadingOut('AbsHum');
  If SaveVar.Choose[S_RelHum] then HeadingOut('RelHum');
  If SaveVar.Choose[S_WaterLimit] then HeadingOut('WatLim');
  If SaveVar.Choose[S_RelDecomp] then HeadingOut('Decomp');
  If SaveVar.Choose[S_Snow] then HeadingOut('Snow');
  If SaveVar.Choose[S_Drainage] then HeadingOut('Drainage');
  If SaveVar.Choose[S_HeatSum] then HeadingOut('Heat sum');
  If SaveVar.Choose[S_Evaporation] then HeadingOut('Evap');
  If Control.OutputByLayer then
     Begin
     If SaveVar.Choose[S_CMetabolic] then HeadingLayerOut('CMetab');
     If SaveVar.Choose[S_NMetabolic] then HeadingLayerOut('NMetab');
     If SaveVar.Choose[S_CStructural] then HeadingLayerOut('CStruct');
     If SaveVar.Choose[S_NStructural] then HeadingLayerOut('NStruct');
     If SaveVar.Choose[S_CWoodyLitter] then HeadingLayerOut('CWdyLt');
     If SaveVar.Choose[S_NWoodyLitter] then HeadingLayerOut('NWdyLt');
     If SaveVar.Choose[S_CActive] then HeadingLayerOut('C_Actv');
     If SaveVar.Choose[S_NActive] then HeadingLayerOut('N_Actv');
     If SaveVar.Choose[S_CSlow] then HeadingLayerOut('C_Slow');
     If SaveVar.Choose[S_NSlow] then HeadingLayerOut('N_Slow');
     If SaveVar.Choose[S_CResistant] then HeadingLayerOut('C_Resist');
     If SaveVar.Choose[S_NResistant] then HeadingLayerOut('N_Resist');
     If SaveVar.Choose[S_CInert] then HeadingLayerOut('C_Inert');
     If SaveVar.Choose[S_NInert] then HeadingLayerOut('N_Inert');
     If SaveVar.Choose[S_NMineral] then HeadingLayerOut('N_Minl');
     If SaveVar.Choose[S_StoredWater] then HeadingLayerOut('SoilWat');
     End
  Else
     Begin
     If SaveVar.Choose[S_CMetabolic] then Heading2Out('CMetab');
     If SaveVar.Choose[S_NMetabolic] then Heading2Out('NMetab');
     If SaveVar.Choose[S_CStructural] then Heading2Out('CStruct');
     If SaveVar.Choose[S_NStructural] then Heading2Out('NStruct');
     If SaveVar.Choose[S_CWoodyLitter] then Heading2Out('CWdyLt');
     If SaveVar.Choose[S_NWoodyLitter] then Heading2Out('NWdyLt');
     If SaveVar.Choose[S_CActive] then Heading2Out('C_Actv');
     If SaveVar.Choose[S_NActive] then Heading2Out('N_Actv');
     If SaveVar.Choose[S_CSlow] then Heading2Out('C_Slow');
     If SaveVar.Choose[S_NSlow] then Heading2Out('N_Slow');
     If SaveVar.Choose[S_CResistant] then Heading2Out('C_Recalc');
     If SaveVar.Choose[S_NResistant] then Heading2Out('N_Recalc');
     If SaveVar.Choose[S_CInert] then Heading2Out('C_Inert');
     If SaveVar.Choose[S_NInert] then Heading2Out('N_Inert');
     If SaveVar.Choose[S_NMineral] then Heading2Out('N_Minl');
     If SaveVar.Choose[S_StoredWater] then Heading2Out('SoilWat');
     End;
  If SaveVar.Choose[S_Dummy] then HeadingOut('Dummy');
  Writeln (Control.CenwFileOut);

  If SaveVar.Choose[S_Year] then HeadingOut('');
  If SaveVar.Choose[S_Month] then HeadingOut('');
  If SaveVar.Choose[S_Day] then HeadingOut('');
  If SaveVar.Choose[S_LAI] then HeadingOut('');
  If SaveVar.Choose[S_Height] then HeadingOut('');
  If SaveVar.Choose[S_DBH] then HeadingOut('');
  If SaveVar.Choose[S_CanopyCover] then HeadingOut('fract');
  If SaveVar.Choose[S_kex] then HeadingOut('');
  If SaveVar.Choose[S_BasalArea] then HeadingOut('');
  If SaveVar.Choose[S_Stocking] then HeadingOut('');
  If SaveVar.Choose[S_SapWoodC] then HeadingOut('DW');
  If SaveVar.Choose[S_HeartWoodC] then HeadingOut('DW');
  If SaveVar.Choose[S_BarkC] then HeadingOut('DW');
  If SaveVar.Choose[S_LeafC] then HeadingOut('DW');
  If SaveVar.Choose[S_FineRootC] then HeadingOut('DW');
  If SaveVar.Choose[S_CoarseRootC] then HeadingOut('DW');
  If SaveVar.Choose[S_BranchesC] then HeadingOut('DW');
  If SaveVar.Choose[S_ReprodC] then HeadingOut('DW');
  If SaveVar.Choose[S_CH2O] then HeadingOut('DW');
  If SaveVar.Choose[S_Reserves] then HeadingOut('DW');
  If SaveVar.Choose[S_SapWoodN] then HeadingOut('Nitr.');
  If SaveVar.Choose[S_HeartWoodN] then HeadingOut('Nitro.');
  If SaveVar.Choose[S_BarkN] then HeadingOut('Nitro.');
  If SaveVar.Choose[S_LeafN] then HeadingOut('Nitro.');
  If SaveVar.Choose[S_FineRootN] then HeadingOut('Nitro.');
  If SaveVar.Choose[S_CoarseRootN] then HeadingOut('Nitro.');
  If SaveVar.Choose[S_BranchesN] then HeadingOut('Nitro.');
  If SaveVar.Choose[S_ReprodN] then HeadingOut('Nitro.');
  If SaveVar.Choose[S_SolubleN] then HeadingOut('Nitro.');
  If SaveVar.Choose[S_TotalN] then HeadingOut('Nitro.');
  If SaveVar.Choose[S_ReservesN] then HeadingOut('Nitro.');
  If SaveVar.Choose[S_NConc] then HeadingOut('N/DW');
  If SaveVar.Choose[S_pi] then HeadingOut('');
  If SaveVar.Choose[S_CAI] then HeadingOut('DW');
  If SaveVar.Choose[S_NPP] then HeadingOut('DW');
  If SaveVar.Choose[S_NEE] then HeadingOut('DW');
  If SaveVar.Choose[S_CarbonGain] then HeadingOut('DW');
  If SaveVar.Choose[S_Respn] then HeadingOut('DW');
  If SaveVar.Choose[S_DayCFlux] then HeadingOut('DW');
  If SaveVar.Choose[S_NightCFlux] then HeadingOut('DW');
  If SaveVar.Choose[S_TDamage] then
                               Begin
                               HeadingOut('');
                               HeadingOut('');
                               End;
  If SaveVar.Choose[S_Transpiration] then HeadingOut('');
  If SaveVar.Choose[S_CLeafLitterFall] then HeadingOut('DW');
  If SaveVar.Choose[S_NLeafLitterFall] then HeadingOut('Nitro.');
  If SaveVar.Choose[S_CLitterFall] then HeadingOut('DW');
  If SaveVar.Choose[S_NLitterFall] then HeadingOut('Nitro.');
  If SaveVar.Choose[S_CAgLitterFall] then HeadingOut('DW');
  If SaveVar.Choose[S_NAgLitterFall] then HeadingOut('Nitro');
  If SaveVar.Choose[S_NLeached] then HeadingOut('Nitro.');
  If SaveVar.Choose[S_HeteroRespn] then HeadingOut('Carbon');
  If SaveVar.Choose[S_SoilRespn] then HeadingOut('Carbon');
  If SaveVar.Choose[S_Tmax] then HeadingOut('');
  If SaveVar.Choose[S_Tmin] then HeadingOut('');
  If SaveVar.Choose[S_Tmean] then HeadingOut('');
  If SaveVar.Choose[S_Tsoil] then HeadingOut('');
  If SaveVar.Choose[S_Tday] then HeadingOut('');
  If SaveVar.Choose[S_PAR] then HeadingOut('');
  If SaveVar.Choose[S_Rain] then HeadingOut('');
  If SaveVar.Choose[S_AbsHum] then HeadingOut('');
  If SaveVar.Choose[S_RelHum] then HeadingOut('');
  If SaveVar.Choose[S_WaterLimit] then HeadingOut('');
  If SaveVar.Choose[S_RelDecomp] then HeadingOut('');
  If SaveVar.Choose[S_Snow] then HeadingOut('water');
  If SaveVar.Choose[S_Drainage] then HeadingOut('water');
  If SaveVar.Choose[S_HeatSum] then HeadingOut('');
  If SaveVar.Choose[S_Evaporation] then HeadingOut('water');
  If Control.OutputByLayer then
     Begin
     If SaveVar.Choose[S_CMetabolic] then HeadingLayerOut('Carbon');
     If SaveVar.Choose[S_NMetabolic] then HeadingLayerOut('Nitro.');
     If SaveVar.Choose[S_CStructural] then HeadingLayerOut('Carbon');
     If SaveVar.Choose[S_NStructural] then HeadingLayerOut('Nitro.');
     If SaveVar.Choose[S_CWoodyLitter] then HeadingLayerOut('Carbon');
     If SaveVar.Choose[S_NWoodyLitter] then HeadingLayerOut('Nitro.');
     If SaveVar.Choose[S_CActive] then HeadingLayerOut('Carbon');
     If SaveVar.Choose[S_NActive] then HeadingLayerOut('Nitro.');
     If SaveVar.Choose[S_CSlow] then HeadingLayerOut('Carbon');
     If SaveVar.Choose[S_NSlow] then HeadingLayerOut('Nitro.');
     If SaveVar.Choose[S_CResistant] then HeadingLayerOut('Carbon');
     If SaveVar.Choose[S_NResistant] then HeadingLayerOut('Nitro.');
     If SaveVar.Choose[S_CInert] then HeadingLayerOut('Carbon');
     If SaveVar.Choose[S_NInert] then HeadingLayerOut('Nitro.');
     If SaveVar.Choose[S_NMineral] then HeadingLayerOut('Nitro.');
     If SaveVar.Choose[S_StoredWater] then HeadingLayerOut('');
     End
  Else
     Begin
     If SaveVar.Choose[S_CMetabolic] then Heading2Out('Carbon');
     If SaveVar.Choose[S_NMetabolic] then Heading2Out('Nitro.');
     If SaveVar.Choose[S_CStructural] then Heading2Out('Carbon');
     If SaveVar.Choose[S_NStructural] then Heading2Out('Nitro.');
     If SaveVar.Choose[S_CWoodyLitter] then Heading2Out('Carbon');
     If SaveVar.Choose[S_NWoodyLitter] then Heading2Out('Nitro.');
     If SaveVar.Choose[S_CActive] then Heading2Out('Carbon');
     If SaveVar.Choose[S_NActive] then Heading2Out('Nitro.');
     If SaveVar.Choose[S_CSlow] then Heading2Out('Carbon');
     If SaveVar.Choose[S_NSlow] then Heading2Out('Nitro.');
     If SaveVar.Choose[S_CResistant] then Heading2Out('Carbon');
     If SaveVar.Choose[S_NResistant] then Heading2Out('Nitro.');
     If SaveVar.Choose[S_CInert] then Heading2Out('Carbon');
     If SaveVar.Choose[S_NInert] then Heading2Out('Nitro.');
     If SaveVar.Choose[S_NMineral] then Heading2Out('Nitro.');
     If SaveVar.Choose[S_StoredWater] then Heading2Out('');
     End;
  If SaveVar.Choose[S_Dummy] then HeadingOut('');
  Writeln (Control.CenwFileOut);

  If SaveVar.Choose[S_Year] then HeadingOut('');
  If SaveVar.Choose[S_Month] then HeadingOut('');
  If SaveVar.Choose[S_Day] then HeadingOut('');
  If SaveVar.Choose[S_LAI] then HeadingOut('');
  If SaveVar.Choose[S_Height] then HeadingOut('m');
  If SaveVar.Choose[S_DBH] then HeadingOut('cm');
  If SaveVar.Choose[S_CanopyCover] then HeadingOut('');
  If SaveVar.Choose[S_kex] then HeadingOut('');
  If SaveVar.Choose[S_BasalArea] then HeadingOut('m2 ha-1');
  If SaveVar.Choose[S_Stocking] then HeadingOut('n ha-1');
  If SaveVar.Choose[S_SapWoodC] then HeadingOut('t ha-1');
  If SaveVar.Choose[S_HeartWoodC] then HeadingOut('t ha-1');
  If SaveVar.Choose[S_BarkC] then HeadingOut('t ha-1');
  If SaveVar.Choose[S_LeafC] then HeadingOut('t ha-1');
  If SaveVar.Choose[S_FineRootC] then HeadingOut('t ha-1');
  If SaveVar.Choose[S_CoarseRootC] then HeadingOut('t ha-1');
  If SaveVar.Choose[S_BranchesC] then HeadingOut('t ha-1');
  If SaveVar.Choose[S_ReprodC] then HeadingOut('t ha-1');
  If SaveVar.Choose[S_CH2O] then HeadingOut('t ha-1');
  If SaveVar.Choose[S_Reserves] then HeadingOut('t ha-1');
  If SaveVar.Choose[S_SapWoodN] then HeadingOut('kg ha-1');
  If SaveVar.Choose[S_HeartWoodN] then HeadingOut('kg ha-1');
  If SaveVar.Choose[S_BarkN] then HeadingOut('kg ha-1');
  If SaveVar.Choose[S_LeafN] then HeadingOut('kg ha-1');
  If SaveVar.Choose[S_FineRootN] then HeadingOut('kg ha-1');
  If SaveVar.Choose[S_CoarseRootN] then HeadingOut('kg ha-1');
  If SaveVar.Choose[S_BranchesN] then HeadingOut('kg ha-1');
  If SaveVar.Choose[S_ReprodN] then HeadingOut('kg ha-1');
  If SaveVar.Choose[S_SolubleN] then HeadingOut('kg ha-1');
  If SaveVar.Choose[S_TotalN] then HeadingOut('kg ha-1');
  If SaveVar.Choose[S_ReservesN] then HeadingOut('kg ha-1');
  If SaveVar.Choose[S_NConc] then HeadingOut('g kg-1');
  If SaveVar.Choose[S_pi] then HeadingOut('ubar');
  If SaveVar.Choose[S_CAI] then HeadingOut('t/ha/yr');
  If SaveVar.Choose[S_NPP] then HeadingOut('t/ha/yr');
  If SaveVar.Choose[S_NEE] then HeadingOut('kg/ha/d-1');
  If SaveVar.Choose[S_CarbonGain] then HeadingOut('kg ha-1');
  If SaveVar.Choose[S_Respn] then HeadingOut('kg/ha/d');
  If SaveVar.Choose[S_DayCFlux] then HeadingOut('kg/ha/d');
  If SaveVar.Choose[S_NightCFlux] then HeadingOut('kg/ha/d');
  If SaveVar.Choose[S_TDamage] then
                               Begin
                               HeadingOut('oC');
                               HeadingOut('');
                               End;
  If SaveVar.Choose[S_Transpiration] then HeadingOut(' mm');
  If SaveVar.Choose[S_CLeafLitterFall] then HeadingOut('kg/ha/d');
  If SaveVar.Choose[S_NLeafLitterFall] then HeadingOut('kg/ha/d');
  If SaveVar.Choose[S_CLitterFall] then HeadingOut('kg/ha/d');
  If SaveVar.Choose[S_NLitterFall] then HeadingOut('kg/ha/d');
  If SaveVar.Choose[S_CAgLitterFall] then HeadingOut('kg/ha/d');
  If SaveVar.Choose[S_NAgLitterFall] then HeadingOut('kg/ha/d');
  If SaveVar.Choose[S_NLeached] then HeadingOut('kg/ha/d');
  If SaveVar.Choose[S_HeteroRespn] then HeadingOut('kg/ha/d');
  If SaveVar.Choose[S_SoilRespn] then HeadingOut('kg/ha/d');
  If SaveVar.Choose[S_Tmax] then HeadingOut('deg');
  If SaveVar.Choose[S_Tmin] then HeadingOut('deg');
  If SaveVar.Choose[S_Tmean] then HeadingOut('deg');
  If SaveVar.Choose[S_Tsoil] then HeadingOut('deg');
  If SaveVar.Choose[S_Tday] then HeadingOut('deg');
  If SaveVar.Choose[S_PAR] then HeadingOut('');
  If SaveVar.Choose[S_Rain] then HeadingOut('mm');
  If SaveVar.Choose[S_AbsHum] then HeadingOut('mbar');
  If SaveVar.Choose[S_RelHum] then HeadingOut('  %');
  If SaveVar.Choose[S_WaterLimit] then HeadingOut('');
  If SaveVar.Choose[S_RelDecomp] then HeadingOut('relat.');
  If SaveVar.Choose[S_Snow] then HeadingOut('mm');
  If SaveVar.Choose[S_Drainage] then HeadingOut('mm');
  If SaveVar.Choose[S_HeatSum] then HeadingOut('oC days');
  If SaveVar.Choose[S_Evaporation] then HeadingOut('mm');
  If Control.OutputByLayer then
     Begin
     If SaveVar.Choose[S_CMetabolic] then HeadingLayerOut('t ha-1');
     If SaveVar.Choose[S_NMetabolic] then HeadingLayerOut('kg ha-1');
     If SaveVar.Choose[S_CStructural] then HeadingLayerOut('t ha-1');
     If SaveVar.Choose[S_NStructural] then HeadingLayerOut('kg ha-1');
     If SaveVar.Choose[S_CWoodyLitter] then HeadingLayerOut('t ha-1');
     If SaveVar.Choose[S_NWoodyLitter] then HeadingLayerOut('kg ha-1');
     If SaveVar.Choose[S_CActive] then HeadingLayerOut('t ha-1');
     If SaveVar.Choose[S_NActive] then HeadingLayerOut('kg ha-1');
     If SaveVar.Choose[S_CSlow] then HeadingLayerOut('t ha-1');
     If SaveVar.Choose[S_NSlow] then HeadingLayerOut('kg ha-1');
     If SaveVar.Choose[S_CResistant] then HeadingLayerOut('t ha-1');
     If SaveVar.Choose[S_NResistant] then HeadingLayerOut('kg ha-1');
     If SaveVar.Choose[S_CInert] then HeadingLayerOut('t ha-1');
     If SaveVar.Choose[S_NInert] then HeadingLayerOut('kg ha-1');
     If SaveVar.Choose[S_NMineral] then HeadingLayerOut('kg ha-1');
     If SaveVar.Choose[S_StoredWater] then HeadingLayerOut('mm');
     End
  Else
     Begin
     If SaveVar.Choose[S_CMetabolic] then Heading2Out('t ha-1');
     If SaveVar.Choose[S_NMetabolic] then Heading2Out('kg ha-1');
     If SaveVar.Choose[S_CStructural] then Heading2Out('t ha-1');
     If SaveVar.Choose[S_NStructural] then Heading2Out('kg ha-1');
     If SaveVar.Choose[S_CWoodyLitter] then Heading2Out('t ha-1');
     If SaveVar.Choose[S_NWoodyLitter] then Heading2Out('kg ha-1');
     If SaveVar.Choose[S_CActive] then Heading2Out('t ha-1');
     If SaveVar.Choose[S_NActive] then Heading2Out('kg ha-1');
     If SaveVar.Choose[S_CSlow] then Heading2Out('t ha-1');
     If SaveVar.Choose[S_NSlow] then Heading2Out('kg ha-1');
     If SaveVar.Choose[S_CResistant] then Heading2Out('t ha-1');
     If SaveVar.Choose[S_NResistant] then Heading2Out('kg ha-1');
     If SaveVar.Choose[S_CInert] then Heading2Out('t ha-1');
     If SaveVar.Choose[S_NInert] then Heading2Out('kg ha-1');
     If SaveVar.Choose[S_NMineral] then Heading2Out('kg ha-1');
     If SaveVar.Choose[S_StoredWater] then Heading2Out('mm');
     End;
  If SaveVar.Choose[S_Dummy] then HeadingOut('');
  Writeln (Control.CenwFileOut);
  If SaveVar.Choose[S_Year] then HeadingOut('');
  If SaveVar.Choose[S_Month] then HeadingOut('');
  If SaveVar.Choose[S_Day] then HeadingOut('');
  If SaveVar.Choose[S_LAI] then HeadingOut('');
  If SaveVar.Choose[S_Height] then HeadingOut('');
  If SaveVar.Choose[S_DBH] then HeadingOut('');
  If SaveVar.Choose[S_CanopyCover] then HeadingOut('');
  If SaveVar.Choose[S_kex] then HeadingOut('');
  If SaveVar.Choose[S_BasalArea] then HeadingOut('');
  If SaveVar.Choose[S_Stocking] then HeadingOut('');
  If SaveVar.Choose[S_SapWoodC] then HeadingOut('');
  If SaveVar.Choose[S_HeartWoodC] then HeadingOut('');
  If SaveVar.Choose[S_BarkC] then HeadingOut('');
  If SaveVar.Choose[S_LeafC] then HeadingOut('');
  If SaveVar.Choose[S_FineRootC] then HeadingOut('');
  If SaveVar.Choose[S_CoarseRootC] then HeadingOut('');
  If SaveVar.Choose[S_BranchesC] then HeadingOut('');
  If SaveVar.Choose[S_ReprodC] then HeadingOut('');
  If SaveVar.Choose[S_CH2O] then HeadingOut('');
  If SaveVar.Choose[S_Reserves] then HeadingOut('');
  If SaveVar.Choose[S_SapWoodN] then HeadingOut('');
  If SaveVar.Choose[S_HeartWoodN] then HeadingOut('');
  If SaveVar.Choose[S_BarkN] then HeadingOut('');
  If SaveVar.Choose[S_LeafN] then HeadingOut('');
  If SaveVar.Choose[S_FineRootN] then HeadingOut('');
  If SaveVar.Choose[S_CoarseRootN] then HeadingOut('');
  If SaveVar.Choose[S_BranchesN] then HeadingOut('');
  If SaveVar.Choose[S_ReprodN] then HeadingOut('');
  If SaveVar.Choose[S_SolubleN] then HeadingOut('');
  If SaveVar.Choose[S_TotalN] then HeadingOut('');
  If SaveVar.Choose[S_ReservesN] then HeadingOut('');
  If SaveVar.Choose[S_NConc] then HeadingOut('');
  If SaveVar.Choose[S_pi] then HeadingOut('');
  If SaveVar.Choose[S_CAI] then HeadingOut('');
  If SaveVar.Choose[S_NPP] then HeadingOut('');
  If SaveVar.Choose[S_NEE] then HeadingOut('');
  If SaveVar.Choose[S_CarbonGain] then HeadingOut('');
  If SaveVar.Choose[S_Respn] then HeadingOut('');
  If SaveVar.Choose[S_DayCFlux] then HeadingOut('');
  If SaveVar.Choose[S_NightCFlux] then HeadingOut('');
  If SaveVar.Choose[S_TDamage] then
                               Begin
                               HeadingOut('');
                               HeadingOut('');
                               End;
  If SaveVar.Choose[S_Transpiration] then HeadingOut('');
  If SaveVar.Choose[S_CLeafLitterFall] then HeadingOut('');
  If SaveVar.Choose[S_NLeafLitterFall] then HeadingOut('');
  If SaveVar.Choose[S_CLitterFall] then HeadingOut('');
  If SaveVar.Choose[S_NLitterFall] then HeadingOut('');
  If SaveVar.Choose[S_CAgLitterFall] then HeadingOut('');
  If SaveVar.Choose[S_NAgLitterFall] then HeadingOut('');
  If SaveVar.Choose[S_NLeached] then HeadingOut('');
  If SaveVar.Choose[S_HeteroRespn] then HeadingOut('');
  If SaveVar.Choose[S_SoilRespn] then HeadingOut('');
  If SaveVar.Choose[S_Tmax] then HeadingOut('');
  If SaveVar.Choose[S_Tmin] then HeadingOut('');
  If SaveVar.Choose[S_Tmean] then HeadingOut('');
  If SaveVar.Choose[S_Tsoil] then HeadingOut('');
  If SaveVar.Choose[S_Tday] then HeadingOut('');
  If SaveVar.Choose[S_PAR] then HeadingOut('');
  If SaveVar.Choose[S_Rain] then HeadingOut('');
  If SaveVar.Choose[S_AbsHum] then HeadingOut('');
  If SaveVar.Choose[S_RelHum] then HeadingOut('');
  If SaveVar.Choose[S_WaterLimit] then HeadingOut('');
  If SaveVar.Choose[S_RelDecomp] then HeadingOut('');
  If SaveVar.Choose[S_Snow] then HeadingOut('');
  If SaveVar.Choose[S_Drainage] then HeadingOut('');
  If SaveVar.Choose[S_HeatSum] then HeadingOut('');
  If SaveVar.Choose[S_Evaporation] then HeadingOut('');
  If SaveVar.Choose[S_CMetabolic] then LayerOut;
  If SaveVar.Choose[S_NMetabolic] then LayerOut;
  If SaveVar.Choose[S_CStructural] then LayerOut;
  If SaveVar.Choose[S_NStructural] then LayerOut;
  If SaveVar.Choose[S_CWoodyLitter] then LayerOut;
  If SaveVar.Choose[S_NWoodyLitter] then LayerOut;
  If SaveVar.Choose[S_CActive] then LayerOut;
  If SaveVar.Choose[S_NActive] then LayerOut;
  If SaveVar.Choose[S_CSlow] then LayerOut;
  If SaveVar.Choose[S_NSlow] then LayerOut;
  If SaveVar.Choose[S_CResistant] then LayerOut;
  If SaveVar.Choose[S_NResistant] then LayerOut;
  If SaveVar.Choose[S_CInert] then LayerOut;
  If SaveVar.Choose[S_NInert] then LayerOut;
  If SaveVar.Choose[S_NMineral] then LayerOut;
  If SaveVar.Choose[S_StoredWater] then LayerOut;
  If SaveVar.Choose[S_Dummy] then HeadingOut('');
  Writeln (Control.CenwFileOut);
End; {of Procedure 'SaveInitial'}

Procedure SaveRegular;
var iLayer: integer;
    Sum: Real48;
    LayerSum: SoilElements;

    Procedure DatumOut (Datum, Multiple: real48);
       var Width, Digits: integer;
       Begin
       Datum := Datum * Multiple;
       GetField(Datum, MaxWidth, Width, Digits);
       Write (Control.CenwFileOut, Datum: Width: Digits, ' ');
       End;  {of procedure 'DatumOut'}

    Procedure IntOut (Datum: Integer);
       Begin
       Write (Control.CenwFileOut, Datum: MaxWidth, ' ');
       End;  {of procedure 'IntOut'}

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
    End;  {of procedure 'LayerOut'}

Begin
      If SaveVar.Choose[S_Year] then IntOut(Control.TotalYears);
      If SaveVar.Choose[S_Month] then IntOut(Control.ExtraMonths);
      If SaveVar.Choose[S_Day] then IntOut(Control.ExtraDays);
      If SaveVar.Choose[S_LAI] then DatumOut(Derived.LAI, 1);
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
      If SaveVar.Choose[S_TotalN] then DatumOut(TotalN, Control.NConversion);
      If SaveVar.Choose[S_ReservesN] then DatumOut(Plant.Reserves[N], Control.NConversion);
      If SaveVar.Choose[S_NConc] then DatumOut(1000 * Derived.NConc, Control.NConversion / Control.CConversion);
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
      If SaveVar.Choose[S_PAR] then DatumOut(Weather.PAR, 1);
      If SaveVar.Choose[S_Rain] then DatumOut(Weather.Rain, 1);
      If SaveVar.Choose[S_AbsHum] then DatumOut(1000 * Weather.AbsHumidity, 1);
      If SaveVar.Choose[S_RelHum] then DatumOut(100 * Weather.RelHumidity, 1);
      If SaveVar.Choose[S_WaterLimit] then DatumOut(Derived.WaterLimit, 1);
      If SaveVar.Choose[S_RelDecomp] then DatumOut(LastYearsDecomp[DecompCount], 1);
      If SaveVar.Choose[S_Snow] then DatumOut(SoilWat.Snow, 1);
      If SaveVar.Choose[S_Drainage] then DatumOut(Derived.Drainage[SoilWat.nLayers], 1);
      If SaveVar.Choose[S_HeatSum] then DatumOut(Derived.HeatSum, 1);
      If SaveVar.Choose[S_Evaporation] then DatumOut(Derived.Evaporation, 1);
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
      If SaveVar.Choose[S_Dummy] then DatumOut(Derived.Dummy, 1);
      Writeln (Control.CenwFileOut);
End; {of Procedure 'SaveRegular'}

End.

{ --- end of file DISKOUT.PAS ------------------------------------------ }

