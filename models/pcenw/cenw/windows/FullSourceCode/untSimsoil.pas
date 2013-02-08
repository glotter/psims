{ ================================================================
  = Project   : CENW                                             =
  ================================================================
  = Module    : NitrogenGain                                     =
  =                                                              =
  =             All routines to run the soil organic matter      =
  =             simulation are packed into this module.          =
  ================================================================
  = File      : untSimSoil.PAS                                   =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

Unit untSimsoil;

INTERFACE

uses untDeclarations, untDivideValidation, untPowerValidation;

procedure NitrogenGain;

IMPLEMENTATION

Procedure NitrogenGain;
Var Active, Slow, Resist, Resid, Struct, Metab, Inert,
    NActive, NSlow, NResist, NMetab, NStruct, NResid, NInert, CO2: OrganicFlowType;
    iLayer, nLayers: Integer;
    MoveOM, SumN, WaterRatio, DecompWatLimit: Real48;
    E: ElementsUsed;

  Function TotalN: real48;
  var iLayer: Integer;
      NSum: Real48;
  Begin
  NSum := Plant.SapWood[N] + Plant.HeartWood[N] + Plant.CoarseRoot[N] + Plant.FineRoot[N] +
            Plant.Branches[N] + Plant.Bark[N] + Plant.Pollen[N] + Plant.Fruit[N] + Plant.Soluble[N] +
            Plant.Leaves[N] + Plant.Reserves[N] +
            Litter.CoarseWood[N] + Litter.FineWood[N] + Litter.CoarseRoot[N] +
            Litter.FineRoot[N] + Litter.Leaves[N] + Litter.Other[N];
  For iLayer := 0 to SoilOrganic.nLayers do
      NSum := NSum +
              SoilOrganic.FineWood[iLayer, N] + SoilOrganic.CoarseWood[iLayer, N] +
              SoilOrganic.Struct[iLayer, N] + SoilOrganic.Metab[iLayer, N] +
              SoilOrganic.Slow[iLayer, N] + SoilOrganic.Active[iLayer, N] +
              SoilOrganic.Resistant[iLayer, N] + SoilOrganic.Inert[iLayer, N] + SoilOrganic.Soluble[iLayer, N];
  TotalN := NSum;
  End; {of Function 'TotalN'}


    Procedure CFluxCalc;
    var FT, FM, LitterNConc, ActRate, StructOut, SlowOut,
        K1act, K2act, K5act, K6act, K7act, K8act, K9act, K10Act: Real48;
    Begin
    If iLayer = 0 then
       FT := 0.85               // no fine soil to stabilise organic matter in surface layer
    Else
       FT := 0.85 - 0.68 * Parameter.FineSoil;
    If iLayer = 0 then
       Begin
       LitterNConc := Divide(Control.NConversion * (Litter.Leaves[N] + Litter.Other[N]),
                        Control.CConversion * (Litter.Leaves[C] + Litter.Other[C]));
       Fm := 0.85 - 0.018 * Divide(Parameter.LeafLignin, LitterNConc);
       End
    Else
       Begin
       LitterNConc := Divide(Control.NConversion * Litter.FineRoot[N], Control.CConversion * Litter.FineRoot[C]);
       Fm := 0.85 - 0.018 * Divide(Parameter.RootLignin, LitterNConc);
       End;
    IF Fm < 0 THEN
       Fm := 0
    Else if Fm > 1 then
       Fm := 1;
    ActRate := exp(3.36 * (Weather.Tsoil - 40) / (Weather.Tsoil + 31.79))
               * DecompWatLimit * Parameter.RateAdjust;
    If iLayer = 0 then
       Begin
       K1Act := ActRate * SOMDecay1 * EXP(-Parameter.LigninInhibition * SoilOrganic.LitterLig[iLayer]);
       K2act := ActRate * SOMDecay2;
       K5Act := ActRate * SOMDecay5;    // No fine-soil effect for surface layer
       End
    Else
       Begin
       K1Act := ActRate * SOMDecay3 * EXP(-Parameter.LigninInhibition * SoilOrganic.LitterLig[iLayer]);
       K2act := ActRate * SomDecay4;
       K5Act := ActRate * SOMDecay5 * (1 - 0.75 * Parameter.FineSoil);
       End;
    K6act := ActRate * SOMDecay6;
    K7act := ActRate * SOMDecay7;
    K8act := ActRate * Parameter.Decay8 * EXP(-Parameter.LigninInhibition * SoilOrganic.BranchLig[0]);
    K9act := ActRate * Parameter.Decay9 * EXP(-Parameter.LigninInhibition * SoilOrganic.StemLig[iLayer]);
    K10act := ActRate * Parameter.Decay10;
    If iLayer = 0 then
       Begin
       Resid[1] := (Litter.Leaves[C] + Litter.Other[C]) * (1 - FM); {Fresh litter that becomes structural surface litter}
       Resid[2] := (Litter.Leaves[C] + Litter.Other[C])* FM;        {Fresh litter that becomes metabolic surface litter}
       Resid[3] := Litter.FineWood[C];
       Resid[4] := Litter.CoarseWood[C];
       End
    Else
       Begin
       Resid[1] := Litter.FineRoot[C] * (1 - FM) * SoilOrganic.FineRootLitterIn[iLayer]; {Fresh litter that becomes structural soil litter}
       Resid[2] := Litter.FineRoot[C] * FM * SoilOrganic.FineRootLitterIn[iLayer];       {Fresh litter that becomes metabolic soil litter}
       Resid[4] := Litter.CoarseRoot[C] * SoilOrganic.CoarseRootLitterIn[iLayer];
       Resid[3] := 0;
       End;
    StructOut := SoilOrganic.Struct[iLayer, C] * K1Act;      {Carbon flux out of structural pool}
    Struct[10] := StructOut * SoilOrganic.LitterLig[iLayer]; {Carbon flux to slow pool}
    Struct[1] := Struct[10] * 0.7;                           {Carbon gain by slow pool}
    Struct[2] := StructOut * (1 - SoilOrganic.LitterLig[iLayer]); {Carbon flux to active pool}
    Struct[3] := Struct[2] * 0.45;                           {Carbon gain by active pool}
    CO2[1] := Struct[10] * 0.3 + Struct[2] * 0.55;
    If iLayer = 0 then
       StructOut := SoilOrganic.FineWood[0, C] * K8act          {Carbon flux out of fine wood pool - only for surface layer}
    Else
       StructOut := 0;                                          {Set to 0 for anything other than surface layer}
    Struct[11] := StructOut * SoilOrganic.BranchLig[0];         {Carbon flux to slow pool}
    Struct[4] := Struct[11] * 0.7;                              {Carbon gain by slow pool}
    Struct[5] := StructOut * (1 - SoilOrganic.BranchLig[0]);    {Carbon flux to active pool}
    Struct[6] := Struct[5] * 0.45;                              {Carbon gain by active pool}
    CO2[6] := Struct[11] * 0.3 + Struct[5] * 0.55;
    StructOut := SoilOrganic.CoarseWood[iLayer, C] * K9act;     {Carbon flux out of coarse wood pool}
    Struct[12] := StructOut * SoilOrganic.StemLig[iLayer];      {Carbon flux to slow pool}
    Struct[7] := Struct[12] * 0.7;                              {Carbon gain by slow pool}
    Struct[8] := StructOut * (1 - SoilOrganic.StemLig[iLayer]); {Carbon flux to active pool}
    Struct[9] := Struct[8] * 0.45;                              {Carbon gain by active pool}
    CO2[7] := Struct[12] * 0.3 + Struct[8] * 0.55;
    Metab[1] := SoilOrganic.Metab[iLayer, C] * K2act;           {Carbon loss from metabolic pool}
    Metab[2] := Metab[1] * 0.45;                                {Carbon gain by active from metabolic pool}
    CO2[2] := Metab[1] * 0.55;
    Active[1] := SoilOrganic.Active[iLayer, C] * K5Act;         {Carbon loss from active pool}
    Active[2] := Active[1] * 0.004;                             {Carbon gain by resistant pool}
    Active[3] := Active[1] * (1 - FT - 0.004);                  {Carbon gain by slow pool}
    CO2[3] := Active[1] * FT;
    SlowOut := SoilOrganic.Slow[iLayer, C] * K6act;
    Slow[1] := SlowOut * 0.97;                                  {Carbon loss from slow to active pool (before respiration loss)}
    Slow[2] := SlowOut * 0.03;                                  {Carbon flux from slow to resistant pool (no respiration loss)}
    CO2[4] := Slow[1] * 0.55;
    Slow[3] := Slow[1] * 0.45;                                  {Carbon gain by active from slow pool (after respiration loss)}
    Resist[1] := SoilOrganic.Resistant[iLayer, C] * K7act;      {Carbon flux from resistant pool}
    Resist[2] := SoilOrganic.Resistant[iLayer, C] * K7act * 0.45; {Carbon gain by active from resistant pool}
    CO2[5] := SoilOrganic.Resistant[iLayer, C] * K7act * 0.55;
    Inert[1] := SoilOrganic.Inert[iLayer, C] * K10act;          {Carbon from inert pool}
    Inert[2] := SoilOrganic.Inert[iLayer, C] * K10act * 0.45;   {Carbon gain by active from inert pool}
    CO2[8] := SoilOrganic.Inert[iLayer, C] * K10act * 0.55;
    Derived.SoilRespn := Derived.SoilRespn + CO2[1] + CO2[2] + CO2[3] +
                         CO2[4] + CO2[5] + CO2[6] + CO2[7] + CO2[8];
    End; {of Procedure 'CFluxCalc'}

    Procedure NFluxCalc;
    var LitterIn: Real48;
    Begin
    If iLayer = 0 then
       LitterIn := Litter.Leaves[N] + Litter.Other[N]
    Else
       LitterIn := Litter.FineRoot[N] * SoilOrganic.FineRootLitterIn[iLayer];
    IF Resid[2] > 0 THEN
       NResid[2] := LitterIn / (1 + Divide(Resid[1], (Parameter.RelativeCN * Resid[2])))
    ELSE
       NResid[2] := 0;
    NResid[1] := LitterIn - NResid[2];
    If iLayer = 0 then
       Begin
       NResid[3] := Litter.FineWood[N];
       NResid[4] := Litter.CoarseWood[N]
       End
    Else
       Begin
       NResid[3] := 0;
       NResid[4] := Litter.CoarseRoot[N] * SoilOrganic.CoarseRootLitterIn[iLayer];
       End;
    IF SoilOrganic.Struct[iLayer, C] > 0 THEN
       Begin
       NStruct[10] := Struct[10] * SoilOrganic.Struct[iLayer, N] / SoilOrganic.Struct[iLayer, C];
       NStruct[2] := Struct[2] * SoilOrganic.Struct[iLayer, N] / SoilOrganic.Struct[iLayer, C];
       End
    Else
       Begin
       NStruct[10] := 0;
       NStruct[2] := 0;
       End;
    IF (iLayer = 0) and (SoilOrganic.FineWood[0, C] > 0) THEN
       BEGIN
       NStruct[11] := Struct[11] * SoilOrganic.FineWood[0, N] / SoilOrganic.FineWood[0, C];
       NStruct[5] := Struct[5] * SoilOrganic.FineWood[0, N] / SoilOrganic.FineWood[0, C];
       END
    Else
       BEGIN
       NStruct[11] := 0;
       NStruct[5] := 0;
       END;
    IF SoilOrganic.CoarseWood[iLayer, C] > 0 THEN
       BEGIN
       NStruct[8] := Struct[8] * SoilOrganic.CoarseWood[iLayer, N] / SoilOrganic.CoarseWood[iLayer, C];
       NStruct[12] := Struct[12] * SoilOrganic.CoarseWood[iLayer, N] / SoilOrganic.CoarseWood[iLayer, C];
       END
    Else
       BEGIN
       NStruct[8] := 0;
       NStruct[12] := 0;
       END;
    IF SoilOrganic.Metab[iLayer, C] > 0 THEN
       NMetab[1] := Metab[1] * SoilOrganic.Metab[iLayer, N] / SoilOrganic.Metab[iLayer, C]
    ELSE
       NMetab[1] := 0;
    IF SoilOrganic.Active[iLayer, C] > 0 THEN
       Begin
       NActive[2] := Active[2] * SoilOrganic.Active[iLayer, N] / SoilOrganic.Active[iLayer, C];
       NActive[3] := Active[3] * SoilOrganic.Active[iLayer, N] / SoilOrganic.Active[iLayer, C];
       End
    Else
       Begin
       NActive[2] := 0;
       NActive[3] := 0;
       End;
    If SoilOrganic.Slow[iLayer, C] > 0 then
       Begin
       NSlow[1] := Slow[1] * SoilOrganic.Slow[iLayer, N] / SoilOrganic.Slow[iLayer, C];
       NSlow[2] := Slow[2] * SoilOrganic.Slow[iLayer, N] / SoilOrganic.Slow[iLayer, C];
       End
    Else
       Begin
       NSlow[1] := 0;
       NSlow[2] := 0;
       End;
    IF SoilOrganic.Resistant[iLayer, C] > 0 THEN
       NResist[1] := Resist[1] * SoilOrganic.Resistant[iLayer, N] / SoilOrganic.Resistant[iLayer, C]
    ELSE
       NResist[1] := 0;
    IF SoilOrganic.Inert[iLayer, C] > 0 THEN
       NInert[1] := Inert[1] * SoilOrganic.Inert[iLayer, N] / SoilOrganic.Inert[iLayer, C]
    ELSE
       NInert[1] := 0;
    End; {of Procedure 'NFluxCalc'}

    Procedure CPoolsCalc;
    var LigTemp: Real48;
    Begin
    IF Resid[4] > 0 THEN
       SoilOrganic.StemLig[iLayer] := (SoilOrganic.StemLig[iLayer] * (SoilOrganic.CoarseWood[0, C] - Struct[8] - Struct[12])
       + Parameter.WoodLignin * Resid[4]) / (Resid[4] + SoilOrganic.CoarseWood[0, C] - Struct[8] - Struct[12]);
    IF iLayer = 0 then
       Begin
       If Resid[1] > 0 THEN
          BEGIN
          Ligtemp := Parameter.LeafLignin * (Resid[1] + Resid[2]) / Resid[1];
          If Ligtemp > 1 then
             Ligtemp := 1;
          SoilOrganic.LitterLig[0] := (SoilOrganic.LitterLig[0] * (SoilOrganic.Struct[0, C] - Struct[2] - Struct[10])
             + Ligtemp * Resid[1]) / (Resid[1] + SoilOrganic.Struct[0, C] - Struct[2] - Struct[10]);
          END;
       IF Resid[3] > 0 THEN
          SoilOrganic.BranchLig[0] := (SoilOrganic.BranchLig[0] * (SoilOrganic.FineWood[0, C] - Struct[5] - Struct[11])
               + Parameter.WoodLignin * Resid[3]) / (Resid[3] + SoilOrganic.FineWood[0, C] - Struct[5] - Struct[11]);
       SoilOrganic.FineWood[0, C] := SoilOrganic.FineWood[0, C] + Resid[3] - Struct[5] - Struct[11];
       End
    Else if Resid[1] > 0 THEN
       BEGIN
       Ligtemp := Parameter.RootLignin * (Resid[1] + Resid[2]) / Resid[1];
       If Ligtemp > 1 then
          Ligtemp := 1;
       SoilOrganic.LitterLig[iLayer] := (SoilOrganic.LitterLig[iLayer] * (SoilOrganic.Struct[iLayer, C] - Struct[2] - Struct[10])
               + Ligtemp * Resid[1]) / (Resid[1] + SoilOrganic.Struct[iLayer, C] - Struct[2] - Struct[10]);
       END;
    SoilOrganic.Metab[iLayer, C] := SoilOrganic.Metab[iLayer, C] + Resid[2] - Metab[1];
    SoilOrganic.CoarseWood[iLayer, C] := SoilOrganic.CoarseWood[iLayer, C] + Resid[4] - Struct[8] - Struct[12];
    SoilOrganic.Struct[iLayer, C] := SoilOrganic.Struct[iLayer, C] + Resid[1] - Struct[2] - Struct[10];
    SoilOrganic.Active[iLayer, C] := SoilOrganic.Active[iLayer, C] + Struct[3] + Metab[2]
                    + Struct[6] + Struct[9] + Resist[2] + Slow[3] + Inert[2] - Active[1];
    SoilOrganic.Slow[iLayer, C] := SoilOrganic.Slow[iLayer, C] + Struct[1] + Active[3]
              + Struct[4] + Struct[7] - Slow[1] - Slow[2];
    SoilOrganic.Resistant[iLayer, C] := SoilOrganic.Resistant[iLayer, C] + Active[2] + Slow[2] - Resist[1];
    SoilOrganic.Inert[iLayer, C] := SoilOrganic.Inert[iLayer, C] - Inert[1];
    If iLayer <> nLayers then
       Begin
       SoilOrganic.Struct[iLayer + 1, C] := SoilOrganic.Struct[iLayer + 1, C] +
                                MoveOM * SoilOrganic.Struct[iLayer, C];
       SoilOrganic.Struct[iLayer, C] := SoilOrganic.Struct[iLayer, C] * (1 - MoveOM);
       SoilOrganic.Metab[iLayer + 1, C] := SoilOrganic.Metab[iLayer + 1, C] +
                                MoveOM * SoilOrganic.Metab[iLayer, C];
       SoilOrganic.Metab[iLayer, C] := SoilOrganic.Metab[iLayer, C] * (1 - MoveOM);
       SoilOrganic.Active[iLayer + 1, C] := SoilOrganic.Active[iLayer + 1, C] +
                                MoveOM * SoilOrganic.Active[iLayer, C];
       SoilOrganic.Active[iLayer, C] := SoilOrganic.Active[iLayer, C] * (1 - MoveOM);
       SoilOrganic.Slow[iLayer + 1, C] := SoilOrganic.Slow[iLayer + 1, C] +
                                MoveOM * SoilOrganic.Slow[iLayer, C];
       SoilOrganic.Slow[iLayer, C] := SoilOrganic.Slow[iLayer, C] * (1 - MoveOM);
       SoilOrganic.Resistant[iLayer + 1, C] := SoilOrganic.Resistant[iLayer + 1, C] +
                                MoveOM * SoilOrganic.Resistant[iLayer, C];
       SoilOrganic.Resistant[iLayer, C] := SoilOrganic.Resistant[iLayer, C] * (1 - MoveOM);
       SoilOrganic.Inert[iLayer + 1, C] := SoilOrganic.Inert[iLayer + 1, C] +
                                MoveOM * SoilOrganic.Inert[iLayer, C];
       SoilOrganic.Inert[iLayer, C] := SoilOrganic.Inert[iLayer, C] * (1 - MoveOM);
       End
    Else
       Begin
       SoilOrganic.Struct[iLayer, C] := SoilOrganic.Struct[iLayer, C] * (1 - MoveOM);
       SoilOrganic.Metab[iLayer, C] := SoilOrganic.Metab[iLayer, C] * (1 - MoveOM);
       SoilOrganic.Active[iLayer, C] := SoilOrganic.Active[iLayer, C] * (1 - MoveOM);
       SoilOrganic.Slow[iLayer, C] := SoilOrganic.Slow[iLayer, C] * (1 - MoveOM);
       SoilOrganic.Resistant[iLayer, C] := SoilOrganic.Resistant[iLayer, C] * (1 - MoveOM);
       SoilOrganic.Inert[iLayer, C] := SoilOrganic.Inert[iLayer, C] * (1 - MoveOM);
       End;
    End; {of Procedure 'CPoolsCalc'}

    Procedure NPoolsCalc;
    Begin
    If iLayer = 0 then
       Begin
       SoilOrganic.FineWood[0, N] := SoilOrganic.FineWood[0, N] + NResid[3] - NStruct[5] - NStruct[11];
       SoilOrganic.Active[iLayer, N] := SoilOrganic.Active[iLayer, N] + NStruct[5];
       SoilOrganic.Slow[iLayer, N] := SoilOrganic.Slow[iLayer, N] + NStruct[11];
       End;
    SoilOrganic.CoarseWood[iLayer, N] := SoilOrganic.CoarseWood[iLayer, N] + NResid[4] - NStruct[8] - NStruct[12];
    SoilOrganic.Struct[iLayer, N] := SoilOrganic.Struct[iLayer, N] + NResid[1] - NStruct[10] - NStruct[2];
    SoilOrganic.Metab[iLayer, N] := SoilOrganic.Metab[iLayer, N] + NResid[2] - NMetab[1];
    SoilOrganic.Active[iLayer, N] := SoilOrganic.Active[iLayer, N]
               + NStruct[2] + NMetab[1] + NStruct[8] + NSlow[1] + NResist[1] + NInert[1] - NActive[2] - NActive[3];
    SoilOrganic.Slow[iLayer, N] := SoilOrganic.Slow[iLayer, N] + NStruct[10] + NActive[3]
                                 + NStruct[12] - NSlow[1] - NSlow[2];
    SoilOrganic.Resistant[iLayer, N] := SoilOrganic.Resistant[iLayer, N]
                                    + NActive[2] + NSlow[2] - NResist[1];
    SoilOrganic.Inert[iLayer, N] := SoilOrganic.Inert[iLayer, N] - NInert[1];
    If iLayer <> nLayers then
       Begin
       SoilOrganic.Struct[iLayer + 1, N] := SoilOrganic.Struct[iLayer + 1, N] +
                                MoveOM * SoilOrganic.Struct[iLayer, N];
       SoilOrganic.Struct[iLayer, N] := SoilOrganic.Struct[iLayer, N] * (1 - MoveOM);
       SoilOrganic.Metab[iLayer + 1, N] := SoilOrganic.Metab[iLayer + 1, N] +
                                MoveOM * SoilOrganic.Metab[iLayer, N];
       SoilOrganic.Metab[iLayer, N] := SoilOrganic.Metab[iLayer, N] * (1 - MoveOM);
       SoilOrganic.Active[iLayer + 1, N] := SoilOrganic.Active[iLayer + 1, N] +
                                MoveOM * SoilOrganic.Active[iLayer, N];
       SoilOrganic.Active[iLayer, N] := SoilOrganic.Active[iLayer, N] * (1 - MoveOM);
       SoilOrganic.Slow[iLayer + 1, N] := SoilOrganic.Slow[iLayer + 1, N] +
                                MoveOM * SoilOrganic.Slow[iLayer, N];
       SoilOrganic.Slow[iLayer, N] := SoilOrganic.Slow[iLayer, N] * (1 - MoveOM);
       SoilOrganic.Resistant[iLayer + 1, N] := SoilOrganic.Resistant[iLayer + 1, N] +
                                MoveOM * SoilOrganic.Resistant[iLayer, N];
       SoilOrganic.Resistant[iLayer, N] := SoilOrganic.Resistant[iLayer, N] * (1 - MoveOM);
       SoilOrganic.Inert[iLayer + 1, N] := SoilOrganic.Inert[iLayer + 1, N] +
                                MoveOM * SoilOrganic.Inert[iLayer, N];
       SoilOrganic.Inert[iLayer, N] := SoilOrganic.Inert[iLayer, N] * (1 - MoveOM);
       End
    Else
       Begin
       SoilOrganic.Struct[iLayer, N] := SoilOrganic.Struct[iLayer, N] * (1 - MoveOM);
       SoilOrganic.Metab[iLayer, N] := SoilOrganic.Metab[iLayer, N] * (1 - MoveOM);
       SoilOrganic.Active[iLayer, N] := SoilOrganic.Active[iLayer, N] * (1 - MoveOM);
       SoilOrganic.Slow[iLayer, N] := SoilOrganic.Slow[iLayer, N] * (1 - MoveOM);
       SoilOrganic.Resistant[iLayer, N] := SoilOrganic.Resistant[iLayer, N] * (1 - MoveOM);
       SoilOrganic.Inert[iLayer, N] := SoilOrganic.Inert[iLayer, N] * (1 - MoveOM);
       End;
    End; {of Procedure 'NPoolsCalc'}

    Procedure C13Calc;
    var LitterC13: Real48;

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

    Begin
    If iLayer = 0 then
       Begin
       LitterC13 := Divide(Litter.Leaves[C] * Litter.Leaves[C13] + Litter.Other[C] * Litter.Other[C13],
                    (Litter.Leaves[C] + Litter.Other[C]));
       Dilute (SoilOrganic.Metab[iLayer, C13], SoilOrganic.Metab[iLayer, C], LitterC13, Resid[2]);
       Dilute (SoilOrganic.Struct[iLayer, C13], SoilOrganic.Struct[iLayer, C], LitterC13, Resid[4]);
       Dilute (SoilOrganic.CoarseWood[iLayer, C13], SoilOrganic.CoarseWood[iLayer, C], Litter.CoarseWood[C13], Resid[4]);
       Dilute (SoilOrganic.Active[iLayer, C13], SoilOrganic.Active[iLayer, C], SoilOrganic.FineWood[0, C13], Struct[6]);
       Dilute (SoilOrganic.Slow[iLayer, C13], SoilOrganic.Slow[iLayer, C], SoilOrganic.FineWood[iLayer, C13], Struct[4]);
       End
    Else
       Begin
       Dilute (SoilOrganic.Metab[iLayer, C13], SoilOrganic.Metab[iLayer, C], Litter.FineRoot[C13], Resid[2]);
       Dilute (SoilOrganic.Struct[iLayer, C13], SoilOrganic.Struct[iLayer, C], Litter.FineRoot[C13], Resid[1]);
       Dilute (SoilOrganic.CoarseWood[iLayer, C13], SoilOrganic.CoarseWood[iLayer, C], Litter.CoarseRoot[C13], Resid[4]);
       End;
    Dilute (SoilOrganic.Active[iLayer, C13], SoilOrganic.Active[iLayer, C], SoilOrganic.Struct[iLayer, C13], Struct[3]);
    Dilute (SoilOrganic.Active[iLayer, C13], SoilOrganic.Active[iLayer, C], SoilOrganic.Metab[iLayer, C13], Metab[2]);
    Dilute (SoilOrganic.Active[iLayer, C13], SoilOrganic.Active[iLayer, C], SoilOrganic.CoarseWood[iLayer, C13], Struct[9]);
    Dilute (SoilOrganic.Active[iLayer, C13], SoilOrganic.Active[iLayer, C], SoilOrganic.Resistant[iLayer, C13], Resist[2]);
    Dilute (SoilOrganic.Active[iLayer, C13], SoilOrganic.Active[iLayer, C], SoilOrganic.Slow[iLayer, C13], Slow[3]);
    Dilute (SoilOrganic.Active[iLayer, C13], SoilOrganic.Active[iLayer, C], SoilOrganic.Inert[iLayer, C13], Inert[1]);
    Dilute (SoilOrganic.Slow[iLayer, C13], SoilOrganic.Slow[iLayer, C], SoilOrganic.Struct[iLayer, C13], Struct[1]);
    Dilute (SoilOrganic.Slow[iLayer, C13], SoilOrganic.Slow[iLayer, C], SoilOrganic.Active[iLayer, C13], Active[3]);
    Dilute (SoilOrganic.Slow[iLayer, C13], SoilOrganic.Slow[iLayer, C], SoilOrganic.CoarseWood[iLayer, C13], Struct[7]);
    Dilute (SoilOrganic.Resistant[iLayer, C13], SoilOrganic.Resistant[iLayer, C], SoilOrganic.Active[iLayer, C13], Active[2]);
    Dilute (SoilOrganic.Resistant[iLayer, C13], SoilOrganic.Resistant[iLayer, C], SoilOrganic.Slow[iLayer, C13], Slow[2]);
    If iLayer <> nLayers then
       Begin
       Dilute (SoilOrganic.Struct[iLayer + 1, C13], SoilOrganic.Struct[iLayer + 1, C],
               SoilOrganic.Struct[iLayer, C13], MoveOM * SoilOrganic.Struct[iLayer, C]);
       Dilute (SoilOrganic.Metab[iLayer + 1, C13], SoilOrganic.Metab[iLayer + 1, C],
               SoilOrganic.Metab[iLayer, C13], MoveOM * SoilOrganic.Metab[iLayer, C]);
       Dilute (SoilOrganic.Active[iLayer + 1, C13], SoilOrganic.Active[iLayer + 1, C],
               SoilOrganic.Active[iLayer, C13], MoveOM * SoilOrganic.Active[iLayer, C]);
       Dilute (SoilOrganic.Slow[iLayer + 1, C13], SoilOrganic.Slow[iLayer + 1, C],
               SoilOrganic.Slow[iLayer, C13], MoveOM * SoilOrganic.Slow[iLayer, C]);
       Dilute (SoilOrganic.Resistant[iLayer + 1, C13], SoilOrganic.Resistant[iLayer + 1, C],
               SoilOrganic.Resistant[iLayer, C13], MoveOM * SoilOrganic.Resistant[iLayer, C]);
       Dilute (SoilOrganic.Inert[iLayer + 1, C13], SoilOrganic.Inert[iLayer + 1, C],
               SoilOrganic.Inert[iLayer, C13], MoveOM * SoilOrganic.Inert[iLayer, C]);
       End;
    End; {of Procedure 'C13Calc'}

    Procedure MineralNCalc;
    var NAtmos, FertiliserAvail, CriticalNActive: real;
        jLayer: Integer;
    BEGIN
    FertiliserAvail := 0;
    If iLayer = 0 then
       NAtmos := Parameter.Atmos_N
    Else
       Begin
       NAtmos := 0;
       If iLayer = 1 then
          If SoilWat.Layer[1].WaterContent > (0.5 * SoilWat.Layer[1].MaxWater) then
             Begin
             FertiliserAvail :=  Event.FertiliserAdded * Parameter.FertiliserRelease;
             Event.FertiliserAdded := Event.FertiliserAdded - FertiliserAvail;
             End
       End;
    If Parameter.VariableNFixation then
       Derived.NBiol := Parameter.BiolFix * Plant.NewGrowth[C] * (1 - Derived.NLimit)
    Else
       Derived.NBiol := Parameter.BiolFix * Plant.NewGrowth[C];
    IF Derived.NBiol < 0 THEN
       Derived.NBiol := 0
    Else
       Derived.NBiol := Derived.NBiol / (nLayers + 1);
    IF not Control.DecayOnly THEN
       SoilOrganic.Active[iLayer, N] := SoilOrganic.Active[iLayer, N] + NAtmos + FertiliserAvail;
    CriticalNActive := Divide(SoilOrganic.Active[iLayer, C], Parameter.CriticalCN);
    IF SoilOrganic.Active[iLayer, N] > CriticalNActive THEN // Excess N to mineralise
       BEGIN
       SoilOrganic.Soluble[iLayer, N] := SoilOrganic.Soluble[iLayer, N] +
                       (1 - Parameter.Nloss) * (SoilOrganic.Active[iLayer, N] - CriticalNActive);
       SoilOrganic.Active[iLayer, N] := CriticalNActive;
       END
    ELSE                                                    // Active pool is N deficient
       Begin
       IF SoilOrganic.Soluble[iLayer, N] > (CriticalNActive - SoilOrganic.Active[iLayer, N]) THEN
          BEGIN                                     // Enough N to satisfy needs of active pool
          SoilOrganic.Soluble[iLayer, N] := SoilOrganic.Soluble[iLayer, N]
                                  + SoilOrganic.Active[iLayer, N] - CriticalNActive;
          SoilOrganic.Active[iLayer, N] := CriticalNActive;
          END
       ELSE
          BEGIN                        // Not enough for the needs of the active pool; so grab all there is
          SoilOrganic.Active[iLayer, N] := SoilOrganic.Active[iLayer, N] + SoilOrganic.Soluble[iLayer, N];
          SoilOrganic.Soluble[iLayer, N] := 0;
          END;
       End;
    IF NOT Control.DecayOnly then
       BEGIN
       SoilOrganic.Mycorrhizal[iLayer, N] := SoilOrganic.Active[iLayer, N] * Parameter.MicroFract *
                 exp(3.36 * (Weather.Tsoil - 40) / (Weather.Tsoil + 31.79));
       SoilOrganic.Active[iLayer, N] := SoilOrganic.Active[iLayer, N] - SoilOrganic.Mycorrhizal[iLayer, N];
       END;
    SoilOrganic.Resistant[iLayer, N] := SoilOrganic.Resistant[iLayer, N] + SoilOrganic.Soluble[iLayer, N] * Parameter.Immobilise * (1 - Parameter.ImmobiliseInSlow);
    SoilOrganic.Slow[iLayer, N] := SoilOrganic.Slow[iLayer, N] + SoilOrganic.Soluble[iLayer, N] * Parameter.Immobilise * Parameter.ImmobiliseInSlow;
    SoilOrganic.Soluble[iLayer, N] := SoilOrganic.Soluble[iLayer, N] * (1 - Parameter.Immobilise);
    If (iLayer < nLayers) and (Derived.Drainage[iLayer] > 0) and (Derived.Drainage[nLayers] = 0) then // only re-distribution within layers but no drainage out of bottom; drainage is dealt with below
       Begin
       SoilOrganic.Soluble[iLayer + 1, N] := SoilOrganic.Soluble[iLayer + 1, N]
                                           + SoilOrganic.Soluble[iLayer, N] * Parameter.Leaching;
       SoilOrganic.Soluble[iLayer, N] := SoilOrganic.Soluble[iLayer, N] * (1 - Parameter.Leaching);
       End;
    If Not Control.DecayOnly then
       Begin
       Derived.NMineralised := Derived.NMineralised + SoilOrganic.Soluble[iLayer, N];
       Derived.NUptake := Derived.NUptake + SoilOrganic.Soluble[iLayer, N] + Derived.NBiol + SoilOrganic.Mycorrhizal[iLayer, N];
       End;
    If (iLayer = nLayers) and (Derived.Drainage[nLayers] > 0) then  // There is drainage all the way through the profile
       Begin
       Derived.nLeached := 0;
       For jLayer := 0 to nLayers do
           Begin
           Derived.NLeached := Derived.nLeached + SoilOrganic.Soluble[jLayer, N] * Parameter.Leaching;
           SoilOrganic.Soluble[jLayer, N] := SoilOrganic.Soluble[jLayer, N] * (1 - Parameter.Leaching);
           End;
       Derived.NMineralised := Derived.NMineralised * (1 - Parameter.Leaching);
       Derived.NUptake := Derived.NUptake * (1 - Parameter.Leaching);
       End
    Else if iLayer = nLayers then
       Derived.NLeached := 0;
    End;

Begin
If Control.AllOneLayer then
   nLayers := 1
Else
   nLayers := SoilOrganic.nLayers;
SumN := 0;
For iLayer := 0 to nLayers do
    SumN := SumN + SoilOrganic.Soluble[iLayer, N];
For iLayer := 0 to nLayers do  // Add Excess N taken up by plants back to soil soluble pools in proportion to amounts in each layer
    Begin
    If SumN > 0 then
        SoilOrganic.Soluble[iLayer, N] := SoilOrganic.Soluble[iLayer, N] * Derived.ExcessN / SumN
    Else
        SoilOrganic.Soluble[iLayer, N] := 0;
    End;
Derived.SoilRespn := 0;
Derived.NUptake := 0;
Derived.NMineralised := 0;
For iLayer := 0 to nLayers do
    Begin
    If iLayer = 0 then
       MoveOM := 1 - Power((1 - Parameter.OMIncorporate), 1 / 365)
    Else if nLayers <> 1 then
       MoveOM := 1 - Power((1 - Parameter.OMTransfer / SoilWat.Layer[iLayer].Depth), 1 / 365)
    Else
       MoveOM := 0;
    If Control.DecayOnly then
       DecompWatLimit := 1
    Else
       DecompWatLimit := Derived.DecompWatLimit[iLayer];
    CFluxCalc;
    NFluxCalc;
    CPoolsCalc;
    If Control.IncludeIsotopes then C13Calc;
    NPoolsCalc;
    MineralNCalc;
    End;
Derived.nBiol := Derived.nBiol * (nLayers + 1);   Derived.Dummy:= Derived.NBiol;
Derived.HeteroRespn := Derived.SoilRespn;
Derived.DayCFlux := Derived.DayCFlux - Derived.SoilRespn * Control.nSeconds / 86400;
Derived.NightCFlux := Derived.NightCFlux - Derived.SoilRespn * (86400 - Control.nSeconds) / 86400;
If Control.DecayOnly then
   For E := C to P do
       Begin
       Litter.CoarseWood[E] := 0;
       Litter.FineWood[E] := 0;
       Litter.CoarseRoot[E] := 0;
       Litter.FineRoot[E] := 0;
       Litter.Leaves[E] := 0;
       Litter.Other[E] := 0;
       End
Else {if Not Control.DecayOnly then}
   Begin
   Plant.Newgrowth[N] := Derived.NUptake;
   Plant.Soluble[N] := Plant.Soluble[N] + Plant.NewGrowth[N];
   End;
End; {of Procedure 'NitrogenGain'}

End. { --- end of file SIMSOIL.PAS ------------------------------------------ }
