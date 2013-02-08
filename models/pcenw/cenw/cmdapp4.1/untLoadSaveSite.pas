{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : GenSite                                          =
  =             SaveSite                                         =
  =                                                              =
  =             Routines to read and save site parameters file.  =
  =             The save/retrieve algorithms are set up in such  =
  =             a way that new information can easily be added   =
  =             to the list of parameters saved previously       =
  =             so that saved information can remain valid over  =
  =             successvive upgrades of the program.             =
  ================================================================
  = File      : untLoadSaveSite.PAS                              =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

Unit untLoadSaveSite;

{$V-}

INTERFACE

Uses
  SysUtils, untDeclarations, untDivideValidation, untFieldValidation;

Procedure RealSiteVariables(ReadWrite: Char; CommentStr, VarStr: String; var defp: Text);
Procedure IntegerSiteVariables(ReadWrite: Char; CommentStr, VarStr: String; var defp: Text);
Procedure GenSite(name : string);
Procedure SaveSite(name : string);

IMPLEMENTATION

Procedure ReadInteger (Var Item: Integer; Comment, CommentStr, VarStr: String);
var ErrCode: Integer;
Begin
If CommentStr = Comment then
   val (VarStr, Item, ErrCode);
End; {of Procedure 'ReadInteger'}

Procedure IntegerOut (Datum: Integer; Comment: string; var Defp: Text);
Begin
Writeln (Defp, Datum: ParFileMaxWidth, ' ', Comment);
End;  {of Procedure 'IntegerOut'}

Procedure ReadReal (Var Item: double; Comment, CommentStr, VarStr: String);
var ErrCode: Integer;
Begin
If CommentStr = Comment then
   val (VarStr, Item, ErrCode);
End; {of Procedure 'ReadReal'}

Procedure RealOut (Datum: double; Comment: string; var Defp: Text);
var Width, Digits: integer;
Begin
GetField(Datum, ParFileMaxWidth, Width, Digits);
Writeln (Defp, Datum: ParFileMaxWidth: Digits, ' ', Comment);
End;  {of Procedure 'RealOut'}

Procedure IntegerSiteVariables(ReadWrite: Char; CommentStr, VarStr: String; var defp: Text);

    Procedure ProcessNext(var NextVar: Integer; ProcessString: String; DefaultValue: Integer;
                          CommentStr, VarStr: String; ReadWrite: Char; var Defp: Text);
    Begin
    if ReadWrite = 'R' then
       ReadInteger (NextVar, ProcessString, CommentStr, VarStr)
    Else if ReadWrite = 'W' then
       IntegerOut (NextVar, ProcessString, Defp)
    Else {if ReadWrite = 'D' then}
       NextVar := DefaultValue;
    End;

Begin
ProcessNext (Soil.nLayers, 'Number of layers for soil water', 7, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.WarmestDay, 'Warmest day of the year (for simulated climate runs)', 23, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.MostRadn, 'Day with highest solar radiation (for simulated climate runs)', 357, CommentStr, VarStr, ReadWrite, Defp);
End; {of Procedure 'IntegerSiteVariables'}

Procedure RealSiteVariables(ReadWrite: Char; CommentStr, VarStr: String; var defp: Text);

    Procedure ProcessNext(var NextVar: double; ProcessString: String; DefaultValue: double;
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
ProcessNext (Parameter.SoilEvap, 'Scaling factor for soil evaporation', 1.0, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Atmos_N, 'Annual atmosperic input of nitrogen', 1 / 365, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Atmos_P, 'Annual atmosperic input of phosphorus', 0.01 / 365, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Leaching, 'Fraction of N not taken up by plants that is leached', 0.1, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.FineSoil, 'Fine soil fraction', 0.25, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.MeanSoilTemp, 'Mean soil temperature', 15, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.CO2conc, 'CO2 concentration', 380, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.AtmosPressure, 'Atmospheric pressure', 1000, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.AnnualRain, 'Annual rainfall', 1000, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.RainProb, 'Daily rainfall probability', 0.25, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.MeanTmax, 'Annual mean maximum temperature', 25, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.MeanTmin, 'Annual mean minimum temperature', 10, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.MeanRadn, 'Annual mean radiation', 15, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.MeanAbsHum, 'Annual mean absolute humidity', -1, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Temp_Amplitude, 'Annual temperature amplitude', 10, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Radn_Amplitude, 'Annual radiation amplitude', 10, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Daily_Amplitude, 'Daily temperature amplitude', 10, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Humid_Amplitude, 'Annual amplitude in absolute humidity', 0.003, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.RelWaterSens, 'Sensitivity of decomposition to water stress relative to that of plant processes', 0.5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.MinDecomp, 'Residual decomposition rate under extremely dry conditions', 0.1, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.RelativeCN, 'RATIO OF C/N RATIOS IN METABOLIC AND STRUCTURAL POOLS', 5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.CriticalCN, 'Critical C/N RATIO OF ACTIVE POOL', 10, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.RelativeCP, 'Ratio of C/P ratios in metabolic and structural pools', 5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.CriticalCPmin, 'Minimum critical C/P ratio of active pool', 30, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.CriticalCPmax, 'Maximum critical C/P ratio of active pool', 80, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.OMIncorporate, 'Annual incorporation of organic matter from the surface to first soil layer', 0.1, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.DecayBranch_StructRatio, 'Decay constant fine woody litter (branches) relative to structural litter', 2, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.DecayWood_StructRatio, 'Decay constant coarse woody litter (logs) relative to structural litter', 5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Inert_Resistant_Ratio, 'Ratio of decay constants of inert and resistant pools', 0.1, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Latitude, 'Latitude of the site', -40, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.RateAdjust, 'Adjustment to soil organic matter decomposition rates', 1, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Immobilise, 'Fraction of inorganic nitrogen that is immobilised at each time step', 0.01, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.ImmobiliseInSlow, 'Fraction of immobilised N that is immobilised in the slow pool', 0, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.SnowMelt, 'mm of snow that melt per degree above zero', 1.0, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.RadnMelt, 'mm of snow that melt per MJ m-2', 0.5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.SoilTResist, 'Soil resistance to temperature changes', 5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.SnowInsulate, 'Extra insulation due to snow layer: extra resistance per mm in snow pack', 10, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.DefaultSoilDelta, 'Default carbon isotope ratio in soil organic carbon pools', 20, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.MaxTBoost, 'The proportional increase of soil temperature aboveaverage air temperature', 0.2, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.TLAISensitivity, 'Dependence of increase in soil temperature on LAI', 0.5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.LigninInhibition, 'Lignin-inhibition parameter of organic matter decomposition', 0, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Weathering, 'Rate of weathering of rock phosporus', 1e-6, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.OccludedP_rate, 'Rate of occluded P formation from secondary inorganic P', 1e-5, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Labile_to_Sec, 'Rate of conversion from labile to secondary inorganic P', 0.1, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Sec_to_Labile, 'Rate of conversion from secondary inorganic to labile P', 5e-4, CommentStr, VarStr, ReadWrite, Defp);
ProcessNext (Parameter.Phosphatase, 'Phosphatase activity', 1e-8, CommentStr, VarStr, ReadWrite, Defp);
End;

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

Procedure GenSite(name : string);
Var Defp: text;
    Linein, VarStr, CommentStr: string;
    ErrCode, iLayer: integer;
    DecayDummy: double;

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

        Procedure RealIn (Var Item: double; Comment: String);
           Begin
           If CommentStr = Comment then
              val (VarStr, Item, ErrCode);
           End; {of Procedure 'RealIn'}

        Procedure IntegerIn (Var Item: Integer; Comment: String);
           Begin
           If CommentStr = Comment then
              val (VarStr, Item, ErrCode);
           End; {of Procedure 'IntegerIn'}

        Procedure OldOMTransferIn;
        var iLayer: Integer;
        Begin
        RealIn(Soil.OMTransfer[1], 'Annual transfer of organic matter from one layer to the next lower layer');
        If CommentStr = 'Annual transfer of organic matter from one layer to the next lower layer' then
           For iLayer := 2 to Soil.nLayers do
               Soil.OMTransfer[iLayer] := Soil.OMTransfer[1];
        End; {of Procedure 'OldOMTransferIn}

Begin
Parameter.DecayWood_StructRatio := 1;     // Temporarily included until new parameters properly used
Parameter.DecayBranch_StructRatio := 1;
Parameter.Inert_Resistant_Ratio := 0.1;
DecayDummy := 0;
Assign (Defp, Name);
Reset (Defp);
iLayer := 1;
while not eof(defp) do
      Begin
      Readln(defp, Linein);
      ProcessLine (Linein, VarStr, CommentStr);
      IntegerSiteVariables('R', CommentStr, VarStr, defp);
      RealSiteVariables('R', CommentStr, VarStr, defp);
      BoolIn (Soil.SeparateSensitivity, 'Give a separate water stress sensitivity for different layers in the soil');
      RealIn (Soil.WaterLayer[iLayer].Depth, 'Depth of ith layer');
      RealIn (Soil.WaterLayer[iLayer].Pores, 'Percentage pores of ith layer');
      RealIn (Soil.WaterLayer[iLayer].MaxWater, 'Maximum water (mm) in ith layer');
      RealIn (Soil.FineRootLitterIn[iLayer], 'Relative decomposition activity');
      RealIn (Soil.CoarseRootLitterIn[iLayer], 'Relative coarse-root litter addition');
      RealIn (Soil.OMTransfer[iLayer], 'Organic matter transfer rate in the soil');
      RealIn (Soil.WaterLayer[iLayer].RelEvap, 'Relative evaporation rate');
      RealIn (Soil.WaterLayer[iLayer].StressSensitivity, 'Relative water stress sensitivity');
      RealIn (Soil.WaterLayer[iLayer].ExtractEffort, 'Effort for water extraction from ith layer');
      If CommentStr = 'Effort for water extraction from ith layer' then
         If iLayer < Soil.nLayers then
            iLayer := iLayer + 1;
      RealIn (Soil.MaxWater, 'Maximum amount of water that can be held in the soil');
      OldOMTransferIn;{(Soil.OMTransfer[1], 'Annual transfer of organic matter from one layer to the next lower layer');} // For reading data from an old file
      RealIn (DecayDummy, 'Decay constant (d-1) for structural surface litter');
      end; {of 'while not eof(defp)' statement}
Close(Defp);
Parameter.Decay8 := Divide(SOMDecay1, Parameter.DecayBranch_StructRatio);
Parameter.Decay9 := Divide(SOMDecay1, Parameter.DecayWood_StructRatio);
Parameter.Decay10 := SOMDecay7 * Parameter.Inert_Resistant_Ratio;
If DecayDummy <> 0 then // must be reading an old data file that still has decay constants listed
   Parameter.RateAdjust := DecayDummy / SOMDecay1; // so adjust the rate constant to make sure actual rate
                                                   // constants are consistent with the old data
End; {of Procedure 'GenSite'}

Procedure SaveSite(name : string);
    Var Defp: text;
        i: integer;

    Procedure LineOut (Datum: double; Comment: string);
       var Width, Digits: integer;
       Begin
       GetField(Datum, ParFileMaxWidth, Width, Digits);
       Writeln (Defp, Datum: ParFileMaxWidth: Digits, Comment);
       End;  {of Procedure 'LineOut'}

    Begin
    assign (defp, Name);   rewrite (defp);
    Writeln(defp, Control.Version + ' Site parameters');
    IntegerSiteVariables('W', 'Dummy', 'Dummy', defp);
    RealSiteVariables('W', 'Dummy', 'Dummy', defp);
    Writeln (defp, Soil.SeparateSensitivity, ' Give a separate water stress sensitivity for different layers in the soil');
    For i := 1 to Soil.nLayers do
        Begin
        LineOut(Soil.WaterLayer[i].Depth, ' Depth of ith layer');
        LineOut(Soil.WaterLayer[i].Pores, ' Percentage pores of ith layer');
        LineOut(Soil.WaterLayer[i].MaxWater, ' Maximum water (mm) in ith layer');
        LineOut(Soil.FineRootLitterIn[i], ' Relative decomposition activity');
        LineOut(Soil.CoarseRootLitterIn[i], ' Relative coarse-root litter addition');
        LineOut(Soil.OMTransfer[i], ' Organic matter transfer rate in the soil');
        LineOut(Soil.WaterLayer[i].RelEvap, ' Relative evaporation rate');
        If Soil.SeparateSensitivity then
           LineOut(Soil.WaterLayer[i].StressSensitivity, ' Relative water stress sensitivity')
        Else
           LineOut(Divide(Soil.WaterLayer[i].MaxWater, Soil.MaxWater), ' Relative water stress sensitivity');
        LineOut(Soil.WaterLayer[i].ExtractEffort, ' Effort for water extraction from ith layer');
        End;
    LineOut (Soil.MaxWater, ' Maximum amount of water that can be held in the soil');
    close (defp);
End; {of Procedure 'SaveSite'}

{ --- end of file untLoadSaveSite.PAS ------------------------------------------ }

End.

