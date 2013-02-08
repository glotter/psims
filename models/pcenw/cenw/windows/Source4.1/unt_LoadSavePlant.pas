{ ================================================================
  = Project   : CENW                                             =
  ================================================================
  = Modules   : GenPlant                                         =
  =             GenProject                                       =
  =             GenSite                                          =
  =             GenPools                                         =
  =             BatchParameters                                  =
  =                                                              =
  =             Routines to read parameters files.               =
  =             The save/retrieve algorithms are set up in such  =
  =             a way that new information can easily be added   =
  =             to the list of parameters saved previously       =
  =             so that saved information can remain valid over  =
  =             successvive upgrades of the program.             =
  ================================================================
  = File      : untFileIO3.PAS                                   =
  =                                                              =
  = Version   : 3.2                                              =
  ================================================================ }

Unit untLoadSavePlant;

{$V-}

INTERFACE

Uses
  SysUtils, untDeclarations, untDivideValidation;

Procedure GenPlant(name : string);
Procedure GenProject(name : string);
Procedure GenSite(name : string);
Procedure GenPools(name : string);
Procedure BatchParameters(Name : string);

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

Procedure GenProject(name : string);
    Var iHarvest, iFertiliser, iEnviron, iPest, iFire, iPlough, ErrCode: integer;
        FileFound: Boolean;
        Defp: text;
        SetSensVar: SensitivityType;
        SaveInFileVar: SaveVariableOptions;
        Linein, VarStr, CommentStr, Extension: string;
        ScreenVar: ScreenOptions;

        Procedure ReadBool (Var Bool_in: Boolean);
           Var Ch: char;
           Begin
           Ch := VarStr[1];
           IF Ch = 'T' then
              Bool_in := true
           Else
              Bool_in := false;
           End; {of Procedure 'ReadBool'}

        Procedure RealIn (Var Item: Real48; Comment: String);
           Begin
           If CommentStr = Comment then
              val (VarStr, Item, ErrCode);
           End; {of Procedure 'RealIn'}

        Procedure IntegerIn (Var Item: Integer; Comment: String);
           Begin
           If CommentStr = Comment then
              val (VarStr, Item, ErrCode);
           End; {of Procedure 'IntegerIn'}

        Procedure LongIn (Var Item: LongInt; Comment: String);
           Begin
           If CommentStr = Comment then
              val (VarStr, Item, ErrCode);
           End; {of Procedure 'LongIn'}

        Procedure BoolIn (Var Item: Boolean; Comment: String);
           Begin
           If CommentStr = Comment then
              ReadBool (Item);
           End; {of Procedure 'BoolIn'}

        Procedure CharIn (Var Item: Char; Comment: String);
           Begin
           If CommentStr = Comment then
              Item := VarStr[1];
           End; {of Procedure 'CharIn'}

        Procedure FileIn;
           var i, CharPos: Integer;
           Begin
           CharPos := Pos('!', VarStr);
           If CharPos > 3 then
              Begin
              FileFound := true;
              Extension := Copy(VarStr, CharPos - 2, 3);
              CharPos := Pos('*', VarStr);
              While CharPos <> 0 do {Turn '*' back into spaces}
                 Begin
                 VarStr := Copy(VarStr, 1, CharPos -1) + ' ' + Copy(VarStr, CharPos + 1, Length(VarStr));
                 CharPos := Pos('*', VarStr);
                 End;
              for i := 1 to Length(Extension) do
                 Extension[i] := UpCase(Extension[i]);
              End
           Else
              FileFound := false;
           End; {of Procedure 'FileIn'}

        Procedure EquilIn (Var Target: EquilOptions; Comment: String);
           Begin
           If CommentStr = Comment then
              Begin
              If VarStr = 'SOM' then
                 Target := SOM
              Else if VarStr = 'Foliage_[N]' then
                 Target := LeafNConc
              Else if VarStr = 'Foliage_nitrogen' then
                 Target := LeafNitrogen
              Else if VarStr = 'Foliage_mass' then
                 Target := Leafmass
              Else {if VarStr = 'Wood_mass'} 
                 Target := WoodMass;
              End;
           End; {of Procedure 'EquilIn'}

        Procedure EquilParameters (Var EquilPar: EquilParameterOptions; Comment: String);
           Begin
           If CommentStr = Comment then
              Begin
              If VarStr = 'Biological_N_fixation' then
                 EquilPar := BiolNFix
              Else if VarStr = 'N_fractional_loss' then
                 EquilPar := NFraction;
              End;
           End; {of Procedure 'EquilIn'}

        Procedure MultipleRunIn (Comment: String);
           var i: Integer;
           Begin
           If CommentStr = Comment then
              Begin
              IntegerIn (Control.nProjects, Comment);
              If Control.nProjects > MaxProjects then
                 Control.nProjects := MaxProjects;
              Readln(defp, Linein);
              ProcessLine (Linein, VarStr, CommentStr);
              FileIn;
              Control.MultipleRunPoolFile := VarStr;
              For i := 1 to Control.nProjects do
                  Begin
                  Readln(defp, Linein);
                  ProcessLine (Linein, VarStr, CommentStr);
                  FileIn;
                  Control.MultipleRuns[i] := VarStr;
                  End;
              For i := Control.nProjects + 1 to MaxProjects do
                  Control.MultipleRuns[i] := '';
              End;
           End; {of Procedure 'MultipleRunIn'}

Begin
  Assign (Defp, Name);
  Reset (Defp);
  Event.nFertilisations := 0;
  Event.nPloughing := 0;
  Event.nHarvests := 0;
  SaveInFileVar := S_Year;
  SetSensVar := Dummy;
  iFertiliser := 0;
  iHarvest := 0;
  iEnviron := 0;
  iPest := 0;
  iFire := 0;
  iPlough := 0;
  for ScreenVar := D_CarbonGain to D_Snow do
  begin
    // random series colours
    ScreenRec.Color[ScreenVar] := Random(16777216);
  end;

  // go through the project file
  while not eof(defp) do
      Begin
      Readln(defp, Linein);
      ProcessLine (Linein, VarStr, CommentStr);
      If CommentStr = '(Project name)' then
         StrPCopy(Control.ProjectName, VarStr);

      // check for the colour of the screen variables
      for ScreenVar := D_CarbonGain to D_Snow do
          LongIn (ScreenRec.Color[ScreenVar], 'Colour for display of ' + ScreenVariableNames[ScreenVar]);
      BoolIn (ScreenRec.Choose[D_CarbonGain], 'Boolean variable for display of total carbon uptake');
      RealIn (ScreenRec.UpRange[D_CarbonGain], 'Upper range for display of total carbon uptake');
      RealIn (ScreenRec.LowRange[D_CarbonGain], 'Lower range for display of total carbon uptake');
      BoolIn (ScreenRec.Choose[D_CAI], 'Boolean variable for display of current annual increment');
      RealIn (ScreenRec.UpRange[D_CAI], 'Upper range for display of current annual increment');
      RealIn (ScreenRec.LowRange[D_CAI], 'Lower range for display of current annual increment');
      BoolIn (ScreenRec.Choose[D_NPP], 'Boolean variable for display of net primary production');
      RealIn (ScreenRec.UpRange[D_NPP], 'Upper range for display of net primary production');
      RealIn (ScreenRec.LowRange[D_NPP], 'Lower range for display of net primary production');
      BoolIn (ScreenRec.Choose[D_NEE], 'Boolean variable for display of net ecosystem exchange');
      RealIn (ScreenRec.UpRange[D_NEE], 'Upper range for display of net ecosystem exchange');
      RealIn (ScreenRec.LowRange[D_NEE], 'Lower range for display of net ecosystem exchange');
      BoolIn (ScreenRec.Choose[D_Respn], 'Boolean variable for display of respiration rate');
      RealIn (ScreenRec.UpRange[D_Respn], 'Upper range for display of respiration rate');
      RealIn (ScreenRec.LowRange[D_Respn], 'Lower range for display of respiration rate');
      BoolIn (ScreenRec.Choose[D_DayCFlux], 'Boolean variable for display of daytime carbon flux');
      RealIn (ScreenRec.UpRange[D_DayCFlux], 'Upper range for display of daytime carbon flux');
      RealIn (ScreenRec.LowRange[D_DayCFlux], 'Lower range for display of daytime carbon flux');
      BoolIn (ScreenRec.Choose[D_NightCFlux], 'Boolean variable for display of nighttime carbon flux');
      RealIn (ScreenRec.UpRange[D_NightCFlux], 'Upper range for display of nighttime carbon flux');
      RealIn (ScreenRec.LowRange[D_NightCFlux], 'Lower range for display of nighttime carbon flux');
      BoolIn (ScreenRec.Choose[D_SolubleCH2O], 'Boolean variable for display of soluble carbohydrate');
      RealIn (ScreenRec.UpRange[D_SolubleCH2O], 'Upper range for display of soluble carbohydrate');
      RealIn (ScreenRec.LowRange[D_SolubleCH2O], 'Lower range for display of soluble carbohydrate');
      BoolIn (ScreenRec.Choose[D_LAI], 'Boolean variable for display of LAI');
      RealIn (ScreenRec.UpRange[D_LAI], 'Upper range for display of LAI');
      RealIn (ScreenRec.LowRange[D_LAI], 'Lower range for display of LAI');
      BoolIn (ScreenRec.Choose[D_pi], 'Boolean variable for display of pi');
      RealIn (ScreenRec.UpRange[D_pi], 'Upper range for display of pi');
      RealIn (ScreenRec.LowRange[D_pi], 'Lower range for display of pi');
      BoolIn (ScreenRec.Choose[D_Wood], 'Boolean variable for display of wood');
      RealIn (ScreenRec.UpRange[D_Wood], 'Upper range for display of wood');
      RealIn (ScreenRec.LowRange[D_Wood], 'Lower range for display of wood');
      BoolIn (ScreenRec.Choose[D_Bark], 'Boolean variable for display of bark');
      RealIn (ScreenRec.UpRange[D_Bark], 'Upper range for display of bark');
      RealIn (ScreenRec.LowRange[D_Bark], 'Lower range for display of bark');
      BoolIn (ScreenRec.Choose[D_SapW], 'Boolean variable for display of sapwood');
      RealIn (ScreenRec.UpRange[D_SapW], 'Upper range for display of sapwood');
      RealIn (ScreenRec.LowRange[D_SapW], 'Lower range for display of sapwood');
      BoolIn (ScreenRec.Choose[D_HeartW], 'Boolean variable for display of heartwood');
      RealIn (ScreenRec.UpRange[D_HeartW], 'Upper range for display of heartwood');
      RealIn (ScreenRec.LowRange[D_HeartW], 'Lower range for display of heartwood');
      BoolIn (ScreenRec.Choose[D_Reserves], 'Boolean variable for display of foliage reserves');
      RealIn (ScreenRec.UpRange[D_Reserves], 'Upper range for display of foliage reserves');
      RealIn (ScreenRec.LowRange[D_Reserves], 'Lower range for display of foliage reserves');
      BoolIn (ScreenRec.Choose[D_Leaf], 'Boolean variable for display of leaves');
      RealIn (ScreenRec.UpRange[D_Leaf], 'Upper range for display of leaves');
      RealIn (ScreenRec.LowRange[D_Leaf], 'Lower range for display of leaves');
      BoolIn (ScreenRec.Choose[D_FineRoot], 'Boolean variable for display of fine roots');
      RealIn (ScreenRec.UpRange[D_FineRoot], 'Upper range for display of fine roots');
      RealIn (ScreenRec.LowRange[D_FineRoot], 'Lower range for display of fine roots');
      BoolIn (ScreenRec.Choose[D_CoarseRoot], 'Boolean variable for display of coarse roots');
      RealIn (ScreenRec.UpRange[D_CoarseRoot], 'Upper range for display of coarse roots');
      RealIn (ScreenRec.LowRange[D_CoarseRoot], 'Lower range for display of coarse roots');
      BoolIn (ScreenRec.Choose[D_Branches], 'Boolean variable for display of branches');
      RealIn (ScreenRec.UpRange[D_Branches], 'Upper range for display of branches');
      RealIn (ScreenRec.LowRange[D_Branches], 'Lower range for display of branches');
      BoolIn (ScreenRec.Choose[D_Reprod], 'Boolean variable for display of reproductive carbon');
      RealIn (ScreenRec.UpRange[D_Reprod], 'Upper range for display of reproductive carbon');
      RealIn (ScreenRec.LowRange[D_Reprod], 'Lower range for display of reproductive carbon');
      BoolIn (ScreenRec.Choose[D_Height], 'Boolean variable for display of stand height');
      RealIn (ScreenRec.UpRange[D_Height], 'Upper range for display of stand height');
      RealIn (ScreenRec.LowRange[D_Height], 'Lower range for display of stand height');
      BoolIn (ScreenRec.Choose[D_DBH], 'Boolean variable for display of DBH');
      RealIn (ScreenRec.UpRange[D_DBH], 'Upper range for display of DBH');
      RealIn (ScreenRec.LowRange[D_DBH], 'Lower range for display of DBH');
      BoolIn (ScreenRec.Choose[D_CanopyCover], 'Boolean variable for display of Canopy cover');
      RealIn (ScreenRec.UpRange[D_CanopyCover], 'Upper range for display of Canopy cover');
      RealIn (ScreenRec.LowRange[D_CanopyCover], 'Lower range for display of Canopy cover');
      BoolIn (ScreenRec.Choose[D_kex], 'Boolean variable for display of Light extinction coefficient');
      RealIn (ScreenRec.UpRange[D_kex], 'Upper range for display of Light extinction coefficient');
      RealIn (ScreenRec.LowRange[D_kex], 'Lower range for display of Light extinction coefficient');
      BoolIn (ScreenRec.Choose[D_BasalArea], 'Boolean variable for display of basal area');
      RealIn (ScreenRec.UpRange[D_BasalArea], 'Upper range for display of basal area');
      RealIn (ScreenRec.LowRange[D_BasalArea], 'Lower range for display of basal area');
      BoolIn (ScreenRec.Choose[D_Stocking], 'Boolean variable for display of stocking');
      RealIn (ScreenRec.UpRange[D_Stocking], 'Upper range for display of stocking');
      RealIn (ScreenRec.LowRange[D_Stocking], 'Lower range for display of stocking');
      BoolIn (ScreenRec.Choose[D_TDamage], 'Boolean variable for display of temperature damage units');
      RealIn (ScreenRec.UpRange[D_TDamage], 'Upper range for display of temperature damage units');
      RealIn (ScreenRec.LowRange[D_TDamage], 'Lower range for display of temperature damage units');
      BoolIn (ScreenRec.Choose[D_NConc], 'Boolean variable for display of foliar [N]');
      RealIn (ScreenRec.UpRange[D_NConc], 'Upper range for display of foliar [N]');
      RealIn (ScreenRec.LowRange[D_NConc], 'Lower range for display of foliar [N]');
      BoolIn (ScreenRec.Choose[D_NConc1], 'Boolean variable for display of [N] in top canopy level');
      RealIn (ScreenRec.UpRange[D_NConc1], 'Upper range for display of [N] in top canopy level');
      RealIn (ScreenRec.LowRange[D_NConc1], 'Lower range for display of [N] in top canopy level');
      BoolIn (ScreenRec.Choose[D_SolubleN], 'Boolean variable for display of soluble nitrogen in plants');
      RealIn (ScreenRec.UpRange[D_SolubleN], 'Upper range for display of soluble nitrogen in plants');
      RealIn (ScreenRec.LowRange[D_SolubleN], 'Lower range for display of soluble nitrogen in plants');
      BoolIn (ScreenRec.Choose[D_WoodN], 'Boolean variable for display of wood nitrogen');
      RealIn (ScreenRec.UpRange[D_WoodN], 'Upper range for display of wood nitrogen');
      RealIn (ScreenRec.LowRange[D_WoodN], 'Lower range for display of wood nitrogen');
      BoolIn (ScreenRec.Choose[D_SapWN], 'Boolean variable for display of sapwood nitrogen');
      RealIn (ScreenRec.UpRange[D_SapWN], 'Upper range for display of sapwood nitrogen');
      RealIn (ScreenRec.LowRange[D_SapWN], 'Lower range for display of sapwood nitrogen');
      BoolIn (ScreenRec.Choose[D_HeartWN], 'Boolean variable for display of heartwood nitrogen');
      RealIn (ScreenRec.UpRange[D_HeartWN], 'Upper range for display of heartwood nitrogen');
      RealIn (ScreenRec.LowRange[D_HeartWN], 'Lower range for display of heartwood nitrogen');
      BoolIn (ScreenRec.Choose[D_BarkN], 'Boolean variable for display of bark nitrogen');
      RealIn (ScreenRec.UpRange[D_BarkN], 'Upper range for display of bark nitrogen');
      RealIn (ScreenRec.LowRange[D_BarkN], 'Lower range for display of bark nitrogen');
      BoolIn (ScreenRec.Choose[D_ReservesN], 'Boolean variable for display of foliage reserves nitrogen');
      RealIn (ScreenRec.UpRange[D_ReservesN], 'Upper range for display of foliage reserves nitrogen');
      RealIn (ScreenRec.LowRange[D_ReservesN], 'Lower range for display of foliage reserves nitrogen');
      BoolIn (ScreenRec.Choose[D_LeafN], 'Boolean variable for display of leaf nitrogen');
      RealIn (ScreenRec.UpRange[D_LeafN], 'Upper range for display of leaf nitrogen');
      RealIn (ScreenRec.LowRange[D_LeafN], 'Lower range for display of leaf nitrogen');
      BoolIn (ScreenRec.Choose[D_FineRootN], 'Boolean variable for display of fine root nitrogen');
      RealIn (ScreenRec.UpRange[D_FineRootN], 'Upper range for display of fine root nitrogen');
      RealIn (ScreenRec.LowRange[D_FineRootN], 'Lower range for display of fine root nitrogen');
      BoolIn (ScreenRec.Choose[D_CoarseRootN], 'Boolean variable for display of coarse root nitrogen');
      RealIn (ScreenRec.UpRange[D_CoarseRootN], 'Upper range for display of coarse root nitrogen');
      RealIn (ScreenRec.LowRange[D_CoarseRootN],'Lower range for display of coarse root nitrogen');
      BoolIn (ScreenRec.Choose[D_BranchN], 'Boolean variable for display of branch nitrogen');
      RealIn (ScreenRec.UpRange[D_BranchN], 'Upper range for display of branch nitrogen');
      RealIn (ScreenRec.LowRange[D_BranchN], 'Lower range for display of branch nitrogen');
      BoolIn (ScreenRec.Choose[D_ReprodN], 'Boolean variable for display of reproductive nitrogen');
      RealIn (ScreenRec.UpRange[D_ReprodN], 'Upper range for display of reproductive nitrogen');
      RealIn (ScreenRec.LowRange[D_ReprodN], 'Lower range for display of reproductive nitrogen');
      BoolIn (ScreenRec.Choose[D_NSum], 'Boolean variable for display of total nitrogen in system');
      RealIn (ScreenRec.UpRange[D_NSum], 'Upper range for display of total nitrogen in system');
      RealIn (ScreenRec.LowRange[D_NSum], 'Lower range for display of total nitrogen in system');
      BoolIn (ScreenRec.Choose[D_CLeafLitter], 'Boolean variable for display of fresh foliage litter');
      RealIn (ScreenRec.UpRange[D_CLeafLitter], 'Upper range for display of fresh foliage litter');
      RealIn (ScreenRec.LowRange[D_CLeafLitter], 'Lower range for display of fresh foliage litter');
      BoolIn (ScreenRec.Choose[D_CAll_Litter], 'Boolean variable for display of all fresh litter');
      RealIn (ScreenRec.UpRange[D_CAll_Litter], 'Upper range for display of all fresh litter');
      RealIn (ScreenRec.LowRange[D_CAll_Litter], 'Lower range for display of all fresh litter');
      BoolIn (ScreenRec.Choose[D_CAg_Litter], 'Boolean variable for display of above-ground fresh litter');
      RealIn (ScreenRec.UpRange[D_CAg_Litter], 'Upper range for display of above-ground fresh litter');
      RealIn (ScreenRec.LowRange[D_CAg_Litter], 'Lower range for display of above-ground fresh litter');
      BoolIn (ScreenRec.Choose[D_CMetabSurf], 'Boolean variable for display of metabolic surface litter');
      RealIn (ScreenRec.UpRange[D_CMetabSurf], 'Upper range for display of metabolic surface litter');
      RealIn (ScreenRec.LowRange[D_CMetabSurf], 'Lower range for display of metabolic surface litter');
      BoolIn (ScreenRec.Choose[D_CMetabSoil], 'Boolean variable for display of metabolic soil litter');
      RealIn (ScreenRec.UpRange[D_CMetabSoil], 'Upper range for display of metabolic soil litter');
      RealIn (ScreenRec.LowRange[D_CMetabSoil], 'Lower range for display of metabolic soil litter');
      BoolIn (ScreenRec.Choose[D_CStructSurf], 'Boolean variable for display of structural surface litter');
      RealIn (ScreenRec.UpRange[D_CStructSurf], 'Upper range for display of structural surface litter');
      RealIn (ScreenRec.LowRange[D_CStructSurf], 'Lower range for display of structural surface litter');
      BoolIn (ScreenRec.Choose[D_CStructSoil], 'Boolean variable for display of structural soil litter');
      RealIn (ScreenRec.UpRange[D_CStructSoil], 'Upper range for display of structural soil litter');
      RealIn (ScreenRec.LowRange[D_CStructSoil], 'Lower range for display of structural soil litter');
      BoolIn (ScreenRec.Choose[D_CWoodyLitter], 'Boolean variable for display of the woody litter pool');
      RealIn (ScreenRec.UpRange[D_CWoodyLitter], 'Upper range for display of the woody litter pool');
      RealIn (ScreenRec.LowRange[D_CWoodyLitter], 'Lower range for display of the woody litter pool');
      BoolIn (ScreenRec.Choose[D_CActive], 'Boolean variable for display of the active pool');
      RealIn (ScreenRec.UpRange[D_CActive], 'Upper range for display of the active pool');
      RealIn (ScreenRec.LowRange[D_CActive], 'Lower range for display of the active pool');
      BoolIn (ScreenRec.Choose[D_CSlow], 'Boolean variable for display of the slow pool');
      RealIn (ScreenRec.UpRange[D_CSlow], 'Upper range for display of the slow pool');
      RealIn (ScreenRec.LowRange[D_CSlow], 'Lower range for display of the slow pool');
      BoolIn (ScreenRec.Choose[D_CResistant], 'Boolean variable for display of the resistant pool');
      RealIn (ScreenRec.UpRange[D_CResistant], 'Upper range for display of the resistant pool');
      RealIn (ScreenRec.LowRange[D_CResistant], 'Lower range for display of the resistant pool');
      BoolIn (ScreenRec.Choose[D_SoilRespn], 'Boolean variable for display of soil respiration');
      RealIn (ScreenRec.UpRange[D_SoilRespn], 'Upper range for display of soil respiration');
      RealIn (ScreenRec.LowRange[D_SoilRespn], 'Lower range for display of soil respiration');
      BoolIn (ScreenRec.Choose[D_NLeafLitter], 'Boolean variable for display of fresh foliage litter nitrogen');
      RealIn (ScreenRec.UpRange[D_NLeafLitter], 'Upper range for display of fresh foliage litter nitrogen');
      RealIn (ScreenRec.LowRange[D_NLeafLitter], 'Lower range for display of fresh foliage litter nitrogen');
      BoolIn (ScreenRec.Choose[D_NAll_Litter], 'Boolean variable for display of all fresh litter nitrogen');
      RealIn (ScreenRec.UpRange[D_NAll_Litter], 'Upper range for display of all fresh litter nitrogen');
      RealIn (ScreenRec.LowRange[D_NAll_Litter], 'Lower range for display of all fresh litter nitrogen');
      BoolIn (ScreenRec.Choose[D_NAg_Litter], 'Boolean variable for display of above-ground fresh litter nitrogen');
      RealIn (ScreenRec.UpRange[D_NAg_Litter], 'Upper range for display of above-ground fresh litter nitrogen');
      RealIn (ScreenRec.LowRange[D_NAg_Litter], 'Lower range for display of above-ground fresh litter nitrogen');
      BoolIn (ScreenRec.Choose[D_NMetabSurf], 'Boolean variable for display of N in the metabolic surface litter');
      RealIn (ScreenRec.UpRange[D_NMetabSurf], 'Upper range for display of N in the metabolic surface litter');
      RealIn (ScreenRec.LowRange[D_NMetabSurf], 'Lower range for display of N in the metabolic surface litter');
      BoolIn (ScreenRec.Choose[D_NMetabSoil], 'Boolean variable for display of N in the metabolic soil litter');
      RealIn (ScreenRec.UpRange[D_NMetabSoil], 'Upper range for display of N in the metabolic soil litter');
      RealIn (ScreenRec.LowRange[D_NMetabSoil], 'Lower range for display of N in the metabolic soil litter');
      BoolIn (ScreenRec.Choose[D_NStructSurf], 'Boolean variable for display of N in the structural surface litter');
      RealIn (ScreenRec.UpRange[D_NStructSurf], 'Upper range for display of N in the structural surface litter');
      RealIn (ScreenRec.LowRange[D_NStructSurf], 'Lower range for display of N in the structural surface litter');
      BoolIn (ScreenRec.Choose[D_NStructSoil], 'Boolean variable for display of N in the structural soil litter');
      RealIn (ScreenRec.UpRange[D_NStructSoil], 'Upper range for display of N in the structural soil litter');
      RealIn (ScreenRec.LowRange[D_NStructSoil], 'Lower range for display of N in the structural soil litter');
      BoolIn (ScreenRec.Choose[D_NWoodyLitter], 'Boolean variable for display of N in the woody litter pool');
      RealIn (ScreenRec.UpRange[D_NWoodyLitter], 'Upper range for display of N in the woody litter pool');
      RealIn (ScreenRec.LowRange[D_NWoodyLitter], 'Lower range for display of N in the woody litter pool');
      BoolIn (ScreenRec.Choose[D_NActive], 'Boolean variable for display of N in the active pool');
      RealIn (ScreenRec.UpRange[D_NActive], 'Upper range for display of N in the active pool');
      RealIn (ScreenRec.LowRange[D_NActive], 'Lower range for display of N in the active pool');
      BoolIn (ScreenRec.Choose[D_NSlow], 'Boolean variable for display of N in the slow pool');
      RealIn (ScreenRec.UpRange[D_NSlow], 'Upper range for display of N in the slow pool');
      RealIn (ScreenRec.LowRange[D_NSlow], 'Lower range for display of N in the slow pool');
      BoolIn (ScreenRec.Choose[D_NResistant], 'Boolean variable for display of N in the resistant pool');
      RealIn (ScreenRec.UpRange[D_NResistant], 'Upper range for display of N in the resistant pool');
      RealIn (ScreenRec.LowRange[D_NResistant], 'Lower range for display of N in the resistant pool');
      BoolIn (ScreenRec.Choose[D_NMineral], 'Boolean variable for display of N in the mineral pool');
      RealIn (ScreenRec.UpRange[D_NMineral], 'Upper range for display of N in the mineral pool');
      RealIn (ScreenRec.LowRange[D_NMineral], 'Lower range for display of N in the mineral pool');
      BoolIn (ScreenRec.Choose[D_NLeached], 'Boolean variable for display of leached N');
      RealIn (ScreenRec.UpRange[D_NLeached], 'Upper range for display of leached N');
      RealIn (ScreenRec.LowRange[D_NLeached], 'Lower range for display of leached N');
      BoolIn (ScreenRec.Choose[D_Tmax], 'Boolean variable for display of Tmax');
      RealIn (ScreenRec.UpRange[D_Tmax], 'Upper range for display of Tmax');
      RealIn (ScreenRec.LowRange[D_Tmax], 'Lower range for display of Tmax');
      BoolIn (ScreenRec.Choose[D_Tmin], 'Boolean variable for display of Tmin');
      RealIn (ScreenRec.UpRange[D_Tmin], 'Upper range for display of Tmin');
      RealIn (ScreenRec.LowRange[D_Tmin], 'Lower range for display of Tmin');
      BoolIn (ScreenRec.Choose[D_Tmean], 'Boolean variable for display of Tmean');
      RealIn (ScreenRec.UpRange[D_Tmean], 'Upper range for display of Tmean');
      RealIn (ScreenRec.LowRange[D_Tmean], 'Lower range for display of Tmean');
      BoolIn (ScreenRec.Choose[D_Tsoil], 'Boolean variable for display of Tsoil');
      RealIn (ScreenRec.UpRange[D_Tsoil], 'Upper range for display of Tsoil');
      RealIn (ScreenRec.LowRange[D_Tsoil], 'Lower range for display of Tsoil');
      BoolIn (ScreenRec.Choose[D_Tday], 'Boolean variable for display of Tday');
      RealIn (ScreenRec.UpRange[D_Tday], 'Upper range for display of Tday');
      RealIn (ScreenRec.LowRange[D_Tday], 'Lower range for display of Tday');
      BoolIn (ScreenRec.Choose[D_PAR], 'Boolean variable for display of PAR');
      RealIn (ScreenRec.UpRange[D_PAR], 'Upper range for display of PAR');
      RealIn (ScreenRec.LowRange[D_PAR], 'Lower range for display of PAR');
      BoolIn (ScreenRec.Choose[D_CO2], 'Boolean variable for display of ambient CO2');
      RealIn (ScreenRec.UpRange[D_CO2], 'Upper range for display of ambient CO2');
      RealIn (ScreenRec.LowRange[D_CO2], 'Lower range for display of ambient CO2');
      BoolIn (ScreenRec.Choose[D_Rain], 'Boolean variable for display of Rain');
      RealIn (ScreenRec.UpRange[D_Rain], 'Upper range for display of Rain');
      RealIn (ScreenRec.LowRange[D_Rain], 'Lower range for display of Rain');
      BoolIn (ScreenRec.Choose[D_StoredWater], 'Boolean variable for display of stored water');
      RealIn (ScreenRec.UpRange[D_StoredWater], 'Upper range for display of stored water');
      RealIn (ScreenRec.LowRange[D_StoredWater], 'Lower range for display of stored water');
      BoolIn (ScreenRec.Choose[D_AbsHum], 'Boolean variable for display of absolute humidity');
      RealIn (ScreenRec.UpRange[D_AbsHum], 'Upper range for display of absolute humidity');
      RealIn (ScreenRec.LowRange[D_AbsHum], 'Lower range for display of absolute humidity');
      BoolIn (ScreenRec.Choose[D_RelHum], 'Boolean variable for display of relative humidity');
      RealIn (ScreenRec.UpRange[D_RelHum], 'Upper range for display of relative humidity');
      RealIn (ScreenRec.LowRange[D_RelHum], 'Lower range for display of relative humidity');
      BoolIn (ScreenRec.Choose[D_WaterLimit], 'Boolean variable for display of water limitation');
      RealIn (ScreenRec.UpRange[D_WaterLimit], 'Upper range for display of water limitation');
      RealIn (ScreenRec.LowRange[D_WaterLimit], 'Lower range for display of water limitation');
      BoolIn (ScreenRec.Choose[D_HeatSum], 'Boolean variable for display of heat sums');
      RealIn (ScreenRec.UpRange[D_HeatSum], 'Upper range for display of heat sums');
      RealIn (ScreenRec.LowRange[D_HeatSum], 'Lower range for display of heat sums');
      BoolIn (ScreenRec.Choose[D_Transpiration], 'Boolean variable for display of transpiration rate');
      RealIn (ScreenRec.UpRange[D_Transpiration], 'Upper range for display of transpiration rate');
      RealIn (ScreenRec.LowRange[D_Transpiration], 'Lower range for display of transpiration rate');
      BoolIn (ScreenRec.Choose[D_Evaporation], 'Boolean variable for display of evaporation rate');
      RealIn (ScreenRec.UpRange[D_Evaporation], 'Upper range for display of evaporation rate');
      RealIn (ScreenRec.LowRange[D_Evaporation], 'Lower range for display of evaporation rate');
      BoolIn (ScreenRec.Choose[D_Snow], 'Boolean variable for display of snow');
      RealIn (ScreenRec.UpRange[D_Snow], 'Upper range for display of snow');
      RealIn (ScreenRec.LowRange[D_Snow], 'Lower range for display of snow');
      BoolIn (ScreenRec.Choose[D_Drainage], 'Boolean variable for display of deep drainage');
      RealIn (ScreenRec.UpRange[D_Drainage], 'Upper range for display of deep drainage');
      RealIn (ScreenRec.LowRange[D_Drainage], 'Lower range for display of deep drainage');
      BoolIn (ScreenRec.Choose[D_Dummy], 'Boolean variable for display of dummy variable');
      RealIn (ScreenRec.UpRange[D_Dummy], 'Upper range for display of dummy variable');
      RealIn (ScreenRec.LowRange[D_Dummy], 'Lower range for display of dummy variable');
      If CommentStr = 'Boolean variable for filesave of particular variables' then
         Begin
         ReadBool(SaveVar.Choose[SaveInFileVar]);
         If SaveInFileVar <> S_Dummy then
            SaveInFileVar := succ(SaveInFileVar);
         End;
      If CommentStr = 'Boolean variable for sensitivity testing of particular parameters' then
         Begin
         ReadBool(TestSens.Choose[SetSensVar]);
         If SetSensVar <> Dummy then
            SetSensVar := succ(SetSensVar);
         End;
      BoolIn (WeatherFile[W_Tmax], 'Boolean variable to read Tmax');
      BoolIn (WeatherFile[W_Tmin], 'Boolean variable to read Tmin');
      BoolIn (WeatherFile[W_Tmean], 'Boolean variable to read Tmean');
      BoolIn (WeatherFile[W_Tsoil], 'Boolean variable to read Tsoil');
      BoolIn (WeatherFile[W_PAR], 'Boolean variable to read PAR');
      BoolIn (WeatherFile[W_Rain], 'Boolean variable to read Rainfall');
      BoolIn (WeatherFile[W_AbsHum], 'Boolean variable to read absolute humidity');
      BoolIn (WeatherFile[W_RelHum], 'Boolean variable to read relative humidity');
      BoolIn (WeatherFile[W_CO2], 'Boolean variable to read CO2 concentration');
      IntegerIn (MaxWidth, 'Variable to define the width for each variable in the output file');
      RealIn (Parameter.StemDeath, 'Fraction of trees dying annually');
      RealIn (Parameter.FertiliserRelease, 'Daily rate of fertiliser release');
      CharIn (Control.Fertilise_DateType, 'Interpretation of fertiliser dates');
      IntegerIn (Event.nFertilisations, 'Number of times that fertiliser is to be applied');
      If CommentStr = 'Day for fertiliser application' then
         If iFertiliser < MaxFertiliseEvents then
            iFertiliser := iFertiliser + 1;
      If iFertiliser > 0 then
         Begin
         LongIn (Event.FertiliseTimes[iFertiliser, 1], 'Day for fertiliser application');
         LongIn (Event.FertiliseTimes[iFertiliser, 2], 'Month for fertiliser application');
         LongIn (Event.FertiliseTimes[iFertiliser, 3], 'Year for fertiliser application');
         RealIn (Event.FertiliseAmount[iFertiliser], 'Application rate of fertiliser');
         End;
      CharIn (Event.HarvestUnits, 'Interpretation of harvest units');
      CharIn (Control.Harvest_DateType, 'Interpretation of harvest dates');
      IntegerIn (Event.nHarvests, 'Number of harvests or thinnings');
      If CommentStr = 'Day for harvest/ thinning' then
         If iHarvest < MaxHarvestEvents then
            iHarvest := iHarvest + 1;
      If iHarvest > 0 then
         Begin
         LongIn (Event.HarvestTimes[iHarvest, 1], 'Day for harvest/ thinning');
         LongIn (Event.HarvestTimes[iHarvest, 2], 'Month for harvest/ thinning');
         LongIn (Event.HarvestTimes[iHarvest, 3], 'Year for harvest/ thinning');
         LongIn (Event.HarvestTimes[iHarvest, 4], 'Day number of harvest/ thinning');
         RealIn (Event.WoodCut[iHarvest], 'Fraction of stems cut');
         RealIn (Event.RelativeSize[iHarvest], 'Size of harvested trees relative to that of average trees');
         BoolIn (Event.AdjustStocking[iHarvest], 'Indicates whether stocking rate is to be adjusted during the harvest');
         RealIn (Event.BranchesCut[iHarvest], 'Fraction of branches/ leaves cut');
         RealIn (Event.WoodRemoval[iHarvest], 'Fraction of stems removed from the site');
         RealIn (Event.FineRemoval[iHarvest], 'Fraction of leaves, branches, etc removed from the site');
         End;
      CharIn (Control.Pest_DateType, 'Interpretation of pest dates');
      CharIn (Event.PestDamageUnits, 'Units in which pest damage is expressed (% or W)');
      IntegerIn (Event.nPests, 'Number of pest outbreaks');
      If CommentStr = 'Day for pest outbreak' then
         If iPest < MaxPestEvents then
            iPest := iPest + 1;
      If iPest > 0 then
         Begin
         LongIn (Event.PestTimes[iPest, 1], 'Day for pest outbreak');
         LongIn (Event.PestTimes[iPest, 2], 'Month for pest outbreak');
         LongIn (Event.PestTimes[iPest, 3], 'Year for pest outbreak');
         LongIn (Event.PestTimes[iPest, 4], 'Day number of pest outbreak');
         LongIn (Event.PestTimes[iPest, 5], 'Duration (days) of of pest outbreak');
         RealIn (Event.LeafDamage[iPest], 'Daily foliage damage by insects');
         RealIn (Event.SolubleDamage[iPest], 'Daily loss by sap-sucking insect pests');
         RealIn (Event.SenescenceDamage[iPest], 'Daily induced foliage senesce by pest damage');
         RealIn (Event.PhotosynthesisFraction[iPest], 'Fractional reduction in photosynthesis for the duration of the pest outbreak');
         RealIn (Event.PestMortality[iPest], 'Death rate due to pest damage');
         RealIn (Event.PestDeathRatio[iPest], 'Size ratio of trees killed by pests to living trees');
         End;
      CharIn (Control.Fire_DateType, 'Interpretation of fire dates');
      IntegerIn (Event.nFires, 'Number of fire outbreaks');
      If CommentStr = 'Day for fire outbreak' then
         If iFire < MaxFireEvents then
            iFire := iFire + 1;
      If iFire > 0 then
         Begin
         LongIn (Event.FireTimes[iFire, 1], 'Day for fire outbreak');
         LongIn (Event.FireTimes[iFire, 2], 'Month for fire outbreak');
         LongIn (Event.FireTimes[iFire, 3], 'Year for fire outbreak');
         LongIn (Event.FireTimes[iFire, 4], 'Day number of fire outbreak');
         RealIn (Event.LeafBurn[iFire], 'Fraction of foliage burnt during a fire');
         RealIn (Event.WoodBurn[iFire], 'Fraction of stem wood burnt during a fire');
         RealIn (Event.WoodToChar[iFire], 'Fraction of wood converted to charcoal during a fire');
         RealIn (Event.FineToChar[iFire], 'Fraction of fine material (foliage and litter) converted to charcoal during a fire');
         RealIn (Event.LitterBurn[iFire], 'Fraction of forest litter burnt during a fire');
         RealIn (Event.LeafBurnSenesc[iFire], 'Fraction of foliage killed (but not burnt) during a fire');
         RealIn (Event.WoodBurnSenesc[iFire], 'Fraction of trees killed (but not burnt) during a fire');
         RealIn (Event.Burn_N_CRatio[iFire], 'Ratio of nitrogen to carbon losses during combustion');
         End;
      CharIn (Control.Plough_DateType, 'Interpretation of plough dates');
      IntegerIn (Event.nPloughing, 'Number of ploughing events');
      If CommentStr = 'Day for ploughing' then
         If iPlough < MaxPloughEvents then
            iPlough := iPlough + 1;
      If iPlough > 0 then
         Begin
         LongIn (Event.PloughTimes[iPlough, 1], 'Day for ploughing');
         LongIn (Event.PloughTimes[iPlough, 2], 'Month for ploughing');
         LongIn (Event.PloughTimes[iPlough, 3], 'Year for ploughing');
         LongIn (Event.PloughTimes[iPlough, 4], 'Day number of ploughing');
         IntegerIn (Event.PloughDepth[iPlough], 'Depth of Ploughing (layers)');
         End;
      IntegerIn (Event.nEnvironments, 'Number of times environmental variables are changed');
      If CommentStr = 'Day number of change in environmental variable' then
         If iEnviron < MaxEnvironmentEvents then
            iEnviron := iEnviron + 1;
      If iEnviron > 0 then
         Begin
         LongIn (Event.EnvironmentTimes[iEnviron], 'Day number of change in environmental variable');
         RealIn (Event.CO2[iEnviron], 'Change CO2 concentration');
         RealIn (Event.Temperature[iEnviron], 'Change Temperature');
         RealIn (Event.Rainfall[iEnviron], 'Change Rainfall');
         End;
      BoolIn (Event.Irrigate, 'Boolean variable to indicate whether irrigation is applied');
      CharIn (Event.IrrigationType, 'Type of irrigation');
      RealIn (Event.IrrigationAmount, 'Amount of irrigation applied');
      RealIn (Event.IrrigationFraction, 'Fraction of field capacity to which irrigation water is applied');
      IntegerIn (Event.IrrigationInterval, 'Interval between applications of irrigation');
      IntegerIn (Event.IrrigStartDay, 'Starting day for irrigation');
      IntegerIn (Event.IrrigStartMonth, 'Starting month for irrigation');
      IntegerIn (Event.IrrigStartYear, 'Starting year for irrigation');
      IntegerIn (Event.IrrigEndDay, 'Last day for irrigation');
      IntegerIn (Event.IrrigEndMonth, 'Last month for irrigation');
      IntegerIn (Event.IrrigEndYear, 'Last year for irrigation');
      IntegerIn (Control.nYears, 'years for simulation run');
      IntegerIn (Control.nMonths, 'months for simulation run');
      IntegerIn (Control.nDays, 'number of days for simulation run');
      RealIn(Control.Initial.SapWood[C], 'Initial setting for sapwood carbon');
      RealIn(Control.Initial.HeartWood[C], 'Initial setting for heartwood carbon');
      RealIn(Control.Initial.CoarseRoot[C], 'Initial setting for coarseroot carbon');
      RealIn(Control.Initial.FineRoot[C], 'Initial setting for fineroot carbon');
      RealIn(Control.Initial.Branches[C], 'Initial setting for branch carbon');
      RealIn(Control.Initial.Bark[C], 'Initial setting for bark carbon');
      RealIn(Control.Initial.Leaves[C], 'Initial setting for foliage carbon');
      RealIn(Control.Initial.Pollen[C], 'Initial setting for pollen carbon');
      RealIn(Control.Initial.Fruit[C], 'Initial setting for fruit carbon');
      RealIn(Control.Initial.Soluble[C], 'Initial setting for soluble carbon');
      RealIn(Control.Initial.Reserves[C], 'Initial setting for leaf primordia carbon');
      RealIn(Control.Initial.SapWood[N], 'Initial setting for sapwood nitrogen');
      RealIn(Control.Initial.HeartWood[N], 'Initial setting for heartwood nitrogen');
      RealIn(Control.Initial.CoarseRoot[N], 'Initial setting for coarseroot nitrogen');
      RealIn(Control.Initial.FineRoot[N], 'Initial setting for fineroot nitrogen');
      RealIn(Control.Initial.Branches[N], 'Initial setting for branch nitrogen');
      RealIn(Control.Initial.Bark[N], 'Initial setting for bark nitrogen');
      RealIn(Control.Initial.Leaves[N], 'Initial setting for foliage nitrogen');
      RealIn(Control.Initial.Pollen[N], 'Initial setting for pollen nitrogen');
      RealIn(Control.Initial.Fruit[N], 'Initial setting for fruit nitrogen');
      RealIn(Control.Initial.Soluble[N], 'Initial setting for soluble nitrogen');
      RealIn(Control.Initial.Reserves[N], 'Initial setting for leaf primordia nitrogen');
      IntegerIn(Control.Initial.Age, 'Initial setting for stand age');
      RealIn(Control.Initial.Stocking, 'Initial setting for stocking');
      RealIn(Control.Initial.Height, 'Initial setting for height');
      RealIn(Control.Initial.DBH, 'Initial setting for diameter');
      IntegerIn(Control.Initial.Days, 'Initial setting starting day');
      IntegerIn(Control.Initial.Months, 'Initial setting starting month');
      IntegerIn(Control.Initial.Years, 'Initial setting starting year');
      LongIn (Control.nDisplays, 'number of times output is given');
      LongIn (Control.nDiskOut, 'number of times output is written to disk');
      CharIn (Control.CalcType, 'Calculation type');
      BoolIn (Control.ResetPlantPools, 'Boolean variable to indicate whether plant pools should be reset at the beginning of a run');
      BoolIn (Control.DecayOnly, 'Boolean variable for calculations of organic matter decomposition only');
      BoolIn (Control.AllOneLayer, 'Boolean variable for organic matter to be treated as single layer');
      BoolIn (Control.OutputByLayer, 'Boolean variable whether output is to be given by soil layer');
      BoolIn (Control.IncludeIsotopes, 'Boolean variable whether to include information about carbon isotopes');
      RealIn (Parameter.FertilityAdjust, 'Adjustment to soil fertility level at start-up');
      CharIn (Control.ClimType, 'Type of climate input');
      MultipleRunIn ('Number of projects to load for multiple project runs');
      FileIn;
      IntegerIn(Control.nCohorts, 'Number of cohorts to simulate');

      If FileFound then
         Begin
         If Extension = 'CL!' then
            Control.ClimFile := VarStr
         Else if Extension = 'DT!' then
            Control.FileOut := VarStr
         Else if Extension = 'IL!' then
            Control.PoolFile := VarStr
         Else if Extension = 'SL!' then
            Begin
            Control.SavePoolFile := VarStr;
            Control.SavePoolFile[Length(Control.SavePoolFile) - 2] := 'i';
            End
         Else if Extension = 'ST!' then
            Control.SiteFile := VarStr
         Else if Extension = 'PL!' then
            Control.PlantFile := VarStr
         Else if Extension = 'BT!' then
            Control.BatchFile := VarStr;
         End;
      RealIn (Control.CConversion, 'Coefficient to convert between carbon and biomass');
      RealIn (Control.NConversion, 'Coefficient to convert between nitrogen units in model and output');
      RealIn (Parameter.gamma, 'Conversion factor from MJ to umol');
      If CommentStr = 'Type of function to calculate mortality' then
         If VarStr = 'Fraction:' then
            Parameter.MortalityType := Fraction
         Else if VarStr = 'Both:' then
            Parameter.MortalityType := Both
         Else if VarStr = 'Density:' then
            Parameter.MortalityType := Density;
      RealIn (Parameter.DeathRatio, 'Ratio of tree sizes of dieing and average trees');
      IntegerIn (Control.BatchCalcs, 'For batch run: number of calculations');
      Equilin (Control.Equil.EquilTarget, 'Type of target value for equilibrium runs');
      EquilParameters (Control.Equil.EquilParameter, 'Type of parameter to change for equilibrium runs');
      Longin (Control.Equil.MaxIterations, 'Maximum number of iterations for equlibrium runs');
      Longin (Control.Equil.MaxGoodCount, 'Required number of good fits to satisfy user of equlibrium conditions');
      RealIn (Control.Equil.Criterion1, 'Value for equilibrium criterium 1 in equilibrium runs');
      RealIn (Control.Equil.Criterion2, 'Value for equilibrium criterium 2 in equilibrium runs');
      RealIn (Control.Equil.Criterion3, 'Value for equilibrium criterium 3 in equilibrium runs');
      RealIn (Control.Equil.TargetValue, 'Target value for equilibrium runs');
      RealIn (Control.Equil.DeltaMin, 'Minimum delta value for equilibrium runs');
      RealIn (Control.Equil.DeltaMax, 'Maximum delta value for equilibrium runs');
      RealIn (Control.Equil.DeltaAdjust, 'Percentage adjustment of delta value for equilibrium runs');
      RealIn (Control.Equil.MaxChangeRatio, 'Maximum change ratio in equilibrium runs');
      RealIn (Control.Equil.BoostResistant, 'Boosting of changes in resistant organic matter relative to other changes');
      BoolIn (Control.Equil.SamePlantPools, 'Boolean variable to indicate whether to keep the same initial plant pools in searching for equilibrium');
      end; {of 'while not eof(defp)' statement}
  Close (Defp);
End; {of Procedure 'GenProject'}

Procedure GenPlant(name : string);
Var Defp: text;
    Linein, VarStr, CommentStr: string;
    ErrCode: integer;
    Height, BasalDiam: Real48;

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

        Procedure RealIn (Var Item: Real48; Comment: String);
           Begin
           If CommentStr = Comment then
              val (VarStr, Item, ErrCode);
           End; {of Procedure 'RealIn'}

        Procedure IntegerIn (Var Item: Integer; Comment: String);
           Begin
           If CommentStr = Comment then
              val (VarStr, Item, ErrCode);
           End; {of Procedure 'IntegerIn'}

        Procedure CharIn (Var Item: Char; Comment: String);
           Begin
           If CommentStr = Comment then
              Item := VarStr[1];
           End; {of Procedure 'CharIn'}

        Procedure PhenologyIn (Comment: String);
           var i: Integer;
           Begin
           If CommentStr = Comment then
              Begin
              IntegerIn (Parameter.Phenology.nChanges, Comment);
              Readln(defp, Parameter.Phenology.Threshold);
              For i := 1 to Parameter.Phenology.nChanges do
                  Begin
                  Readln(defp, Linein);
                  ProcessLine (Linein, VarStr, CommentStr);
                  If VarStr = 'Heatsum' then
                     Parameter.Phenology.Units[i] := HeatSum
                  Else if VarStr = 'Julianday' then
                     Parameter.Phenology.Units[i] := JulianDay
                  Else if VarStr = 'Daylength' then
                     Parameter.Phenology.Units[i] := DayLength
                  Else // If VarStr = 'nDays' then
                     Parameter.Phenology.Units[i] := nDays;
                  Readln (defp, Parameter.Phenology.JulianDay[i]);
                  Readln (defp, Parameter.Phenology.nDays[i]);
                  Readln (defp, Parameter.Phenology.HeatSum[i]);
                  Readln (defp, Parameter.Phenology.Daylength[i]);
                  Readln (defp, Parameter.Phenology.Senescence[i]);
                  Readln (defp, Parameter.Phenology.LeafGrowth[i]);
                  End;
              End;
           End;

Begin
Assign (Defp, Name);
Reset (Defp);
while not eof(defp) do
      Begin
      Readln(defp, Linein);
      ProcessLine (Linein, VarStr, CommentStr);
      RealIn (Parameter.kexmax, 'Max. light extinction coefficient');
      RealIn (Parameter.KlowRange, 'Max. decrease of max. light extinction coeff');
      RealIn (Parameter.Albedo, 'Leaf albedo');
      RealIn (Parameter.SLA, 'Specific leaf area');
      RealIn (Parameter.NMVOC, 'Loss rate of non-methyl volatile organic carbon');
      RealIn (Parameter.BallBerry1, 'Ball-Berry parameter that gives the slope between gs and Ah/c in unstressed plants');
      RealIn (Parameter.BallBerry2, 'Ball-Berry parameter that gives the slope between gs and Ah/c in stressed plants');
      RealIn (Parameter.MicroFract, 'Fraction of "Active SON" taken up daily through micorrhizal associations');
      BoolIn (Parameter.VariableNFixation, 'Calculate biological N fixation as a function of N status?');
      RealIn (Parameter.BiolFix, 'Amount of nitrogen fixed biologically per unit carbon fixed');
      RealIn (Parameter.RespFromN, 'Daily respiration rate per unit N');
      RealIn (Parameter.GrowthRespn, 'Fraction of carbon used up in growth respiration');
      RealIn (Parameter.RespnAdjust, 'Time constant for adjustments in temperature response of respiration rate');
      RealIn (Parameter.SenescLeafRatio, 'Ratio of N in senescing and living leaves');
      RealIn (Parameter.InternalNRatio, 'Ratio of N between average foliage and top foliage layers');
      RealIn (Parameter.DrySenesc, 'Maximum fraction of leaves and branches dying daily during drought');
      RealIn (Parameter.StressLimit, 'Relative soil water content for the commencement of plant stress');
      CharIn (Parameter.DirectEvapType, 'Flag whether direct rain evap is calculated as const or f(LAI)');
      RealIn (Parameter.DirectEvapSlope, 'Slope of relationship relating intercepted rain and LAI');
      RealIn (Parameter.DirectEvapFract, 'Fraction of rain lost to direct evaporation');
      RealIn (Parameter.BarkSenesc, 'Fraction of bark senescing daily');
      RealIn (Parameter.LeafSenesc, 'Fraction of foliage senescing daily');
      RealIn (Parameter.BranchSenesc, 'Fraction of branches senescing daily');
      RealIn (Parameter.RootSenesc, 'Fraction of roots senescing daily');
      RealIn (Parameter.FruitSenesc, 'Fraction of fruit senescing daily');
      RealIn (Parameter.PollenSenesc, 'Fraction of pollen senescing daily (can be greater than 1)');
      RealIn (Parameter.SenescLowLight, 'Low-light level at which foliage senesces');
      RealIn (Parameter.MaxSenescLowLight, 'Maximum daily foliage senescence rate due to low light');
      RealIn (Parameter.RootLeafRatio1, 'Ratio of fine roots to foliage in unstressed plants');
      RealIn (Parameter.RootLeafRatio2, 'Ratio of fine roots to foliage in nutritionally stressed plants');
      RealIn (Parameter.LeafBranchRatio, 'Ratio of foliage to branches');
      RealIn (Parameter.WoodBranchRatio, 'Ratio of wood to branches');
      RealIn (Parameter.CoarseRootWoodRatio, 'Ratio of coarse roots to stem wood');
      RealIn (Parameter.BarkWoodRatio, 'Ratio of bark to wood');
      RealIn (Parameter.MinWoodAlloc, 'Minimum amount of carbon allocated to stem-wood production in young trees');
      RealIn (Parameter.C_FruitAlloc, 'Carbon allocation to fruiting bodies');
      RealIn (Parameter.C_PollenAlloc, 'Carbon allocation to pollen');
      IntegerIn (Parameter.SexAge, 'Minimum age for sexual reproduction');
      IntegerIn (Parameter.SapWoodYears, 'Longevity of sapwood before turning into heartwood');
      RealIn (Parameter.Ncrit, 'Nitrogen concentration that is saturating');
      RealIn (Parameter.Nmax, 'Maximum nitrogen concentration to which nitrogen can be taken up by foliage');
      RealIn (Parameter.N0, 'Nitrogen concentration ''compensation point''');
      RealIn (Parameter.KmGrowth[C], 'Km term for the soluble carbohydrate dependence of growth');
      RealIn (Parameter.KmGrowth[N], 'Km term for the soluble inorganic nitrogen dependence of growth');
      RealIn (Parameter.Nloss, 'Fraction of N volatilised during mineralisation processes');
      RealIn (Parameter.WoodRetrans, 'ratio of nitrogen concentrations in heartwood and sapwood');
      RealIn (Parameter.bRoots, 'ratio of nitrogen concentrations in roots and foliage');
      RealIn (Parameter.bWood, 'ratio of nitrogen concentrations in wood and foliage');
      RealIn (Parameter.bBark, 'ratio of nitrogen concentrations in bark and foliage');
      RealIn (Parameter.bBranch, 'ratio of nitrogen concentrations in branches and foliage');
      RealIn (Parameter.bFruit, 'ratio of nitrogen concentrations in fruit and foliage');
      RealIn (Parameter.bPollen, 'ratio of nitrogen concentrations in pollen and foliage');
      RealIn (Parameter.WoodLignin, 'Lignin concentration in wood');
      RealIn (Parameter.LeafLignin, 'Lignin concentration in leaves');
      RealIn (Parameter.RootLignin, 'Lignin concentration in roots');
      RealIn (Parameter.LigninInhibition, 'Term that describes the extent by which structural-litter decomposition is inhibited by lignin');
      RealIn (Parameter.Amax, 'Maximum nitrogen use efficiency (Amax / [N])');
      RealIn (Parameter.alpha, 'Photosynthetic quantum yield');
      RealIn (Parameter.Theta, 'Theta - curvature term in light dependence of daily carbon gain');
      RealIn (Parameter.RelkPEP, 'PEP carboxylase activity relative to maximum assimilation rate (for C4 phs)');
      RealIn (Parameter.Beta, 'Curvature term in the transition from CO2 limited to CO2 saturated phs (in C4 phs)');
      RealIn (Parameter.TMinLim, 'Lower mean daytime temperature limit for plant growth');
      RealIn (Parameter.TOpt1, 'Lower mean daytime temperature limit for optimum plant growth');
      RealIn (Parameter.TOpt2, 'Upper mean daytime temperature limit for optimum plant growth');
      RealIn (Parameter.TMaxLim, 'Upper mean daytime temperature limit for plant growth');
      RealIn (Parameter.TFrost, 'Threshold nighttime temperature for leaf damage');
      RealIn (Parameter.TScorch, 'Threshold daytime temperature for leaf damage');
      RealIn (Parameter.TSensitivity, 'Growth sensitivity to accumulated temperature damage');
      RealIn (Parameter.TRepair, 'Rate of repair from accumulated temperature damage');
      RealIn (Parameter.HDInter, 'Intercept in the allometric relationship of height versus DBH');
      RealIn (Parameter.HDSlope, 'Slope in the allometric relationship of height versus DBH');
      RealIn (Parameter.WDSlope, 'Slope in the allometric relationship of stem weight versus DBH');
      RealIn (Parameter.WHSlope, 'Slope in the allometric relationship of stem weight versus height');
      RealIn (Parameter.Mindbh, 'Minimum dbh at which to apply the allometric relationship');
      RealIn (Parameter.WoodDensity, 'Wood density');
      RealIn (Parameter.Transmit, 'Leaf transmissivity');
      RealIn (Parameter.gamma, 'Coversion factor from MJ to umol');
      RealIn (Parameter.TMaxRepairTime, 'Maximum number of days for temperature damage to be fully repaired');
      RealIn (Parameter.ExcessNUptake, 'Ratio of total nitrogen uptake into foliage and that utilised in new growth');
      RealIn (Parameter.LitterWHC, 'Water holding capacity of surface litter');
      RealIn (Parameter.Mulching, 'Mulching effect of surface litter');
      RealIn (Parameter.AeroResist, 'Canopy aerodynamic resistance (s m-1)');
      If CommentStr = 'Photosynthetic pathway' then
         If VarStr = 'C3:' then
            Parameter.Phs := C3
         Else
            Parameter.Phs := C4;
      If CommentStr = 'Type of respiration function' then
         If VarStr = 'Ratio:' then
            Parameter.RespnType := Ratio
         Else if VarStr = 'Basics:' then
            Parameter.RespnType := Basics;
      RealIn (Parameter.Respnalpha, 'Parameter alpha in the T-respn relationship');
      RealIn (Parameter.Respnbeta, 'Parameter beta in the T-respn relationship');
      RealIn (Parameter.RespnOpt, 'Temperature for maximum respiration rate');
      BoolIn (Parameter.RespnTAcclimation, 'Boolean variable to indicate whether to include temperature acclimation for plant respiration');
      RealIn (Parameter.Three_Two_Power_Law, 'Parameter in the 3/2 power law relationship');
      RealIn (Parameter.RespnRatio, 'Ratio of respiration rate to daily photosynthetic carbon gain');
      BoolIn (Parameter.AgeDecline, 'Boolean variable to indicate whether to include age-related NPP decline');
      IntegerIn (Parameter.MatureAge, 'Age of maturity (at which NPP is only 50% of young-tree NPP');
      BoolIn (Parameter.SizeDecline, 'Boolean variable to indicate whether to include size-related NPP decline');
      IntegerIn (Parameter.MatureSize, 'Size of maturity (at which NPP is only 50% of young-tree NPP');
      BoolIn (Parameter.FoliageClumping, 'Boolean variable to indicate whether to include foliage clumping for plant light interception');
      BoolIn (Parameter.ConstantLeafN, 'Boolean variable to indicate whether to run with constant leaf nitrogen');
      RealIn (Parameter.ConstantLeafNValue, 'Constant leaf [N] (if that is to be kept constant)');
      RealIn (Parameter.AgePower, 'Power term in age related decline in NPP');
      RealIn (Parameter.SizePower, 'Power term in size related decline in NPP');
      RealIn (Parameter.DefaultPlantDelta, 'Default plant carbon isotope ratio');
      If Commentstr = 'Type of delta setting for new carbon' then
         If VarStr = 'SetValue:' then
            Parameter.SetDeltaType := SetValue
         Else if VarStr = 'CalculateValue:' then
            Parameter.SetDeltaType := CalculateValue;
      RealIn (Parameter.NewDelta, 'Set value for carbon isotope discrimination of newly fixed carbon');
      RealIn (Parameter.phi, 'CO2 leakage from bundle-sheet cells (only relevant for C4 plants)');
      PhenologyIn ('number of changes in plant phenology');
      end; {of 'while not eof(defp)' statement}
Close (defp);
End; {of Procedure 'GenPlant'}

Procedure GenSite(name : string);
Var Defp: text;
    Linein, VarStr, CommentStr: string;
    ErrCode, iLayer: integer;
    DecayDummy: Real48;

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

        Procedure RealIn (Var Item: Real48; Comment: String);
           Begin
           If CommentStr = Comment then
              val (VarStr, Item, ErrCode);
           End; {of Procedure 'RealIn'}

        Procedure IntegerIn (Var Item: Integer; Comment: String);
           Begin
           If CommentStr = Comment then
              val (VarStr, Item, ErrCode);
           End; {of Procedure 'IntegerIn'}

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
      RealIn (Parameter.SoilEvap, 'Scaling factor for soil evaporation');
      RealIn (Parameter.Atmos_N, 'Annual atmosperic input of nitrogen');
      RealIn (Parameter.Leaching, 'Fraction of N not taken up by plants that is leached');
      RealIn  (Parameter.FineSoil, 'Fine soil fraction');
      IntegerIn (SoilWat.nLayers, 'Number of layers for soil water');
      BoolIn (SoilWat.SeparateSensitivity, 'Give a separate water stress sensitivity for different layers in the soil');
      RealIn (SoilWat.Layer[iLayer].Depth, 'Depth of ith layer');
      RealIn (SoilWat.Layer[iLayer].Pores, 'Percentage pores of ith layer');
      RealIn (SoilWat.Layer[iLayer].MaxWater, 'Maximum water (mm) in ith layer');
      RealIn (SoilOrganic.FineRootLitterIn[iLayer], 'Relative decomposition activity');
      RealIn (SoilOrganic.CoarseRootLitterIn[iLayer], 'Relative coarse-root litter addition');
      RealIn (SoilWat.Layer[iLayer].RelEvap, 'Relative evaporation rate');
      RealIn (SoilWat.Layer[iLayer].StressSensitivity, 'Relative water stress sensitivity');
      RealIn (SoilWat.Layer[iLayer].ExtractEffort, 'Effort for water extraction from ith layer');
      If CommentStr = 'Effort for water extraction from ith layer' then
         If iLayer < SoilWat.nLayers then
            iLayer := iLayer + 1;
      RealIn (SoilWat.MaxWater, 'Maximum amount of water that can be held in the soil');
      RealIn (Parameter.MeanSoilTemp, 'Mean soil temperature');
      RealIn (Parameter.CO2conc, 'CO2 concentration');
      RealIn (Parameter.AtmosPressure, 'Atmospheric pressure');
      RealIn (Parameter.AnnualRain, 'Annual rainfall');
      RealIn (Parameter.RainProb, 'Daily rainfall probability');
      RealIn (Parameter.MeanTmax, 'Annual mean maximum temperature');
      RealIn (Parameter.MeanTmin, 'Annual mean minimum temperature');
      RealIn (Parameter.MeanRadn, 'Annual mean radiation');
      RealIn (Parameter.MeanAbsHum, 'Annual mean absolute humidity');
      RealIn (Parameter.Temp_Amplitude, 'Annual temperature amplitude');
      RealIn (Parameter.Radn_Amplitude, 'Annual radiation amplitude');
      RealIn (Parameter.Daily_Amplitude, 'Daily temperature amplitude');
      RealIn (Parameter.Humid_Amplitude, 'Annual amplitude in absolute humidity');
      RealIn (Parameter.RelWaterSens, 'Sensitivity of decomposition to water stress relative to that of plant processes');
      RealIn (Parameter.MinDecomp, 'Residual decomposition rate under extremely dry conditions');
      RealIn (Parameter.RelativeCN, 'RATIO OF C/N RATIOS IN METABOLIC AND STRUCTURAL POOLS');
      RealIn (Parameter.CriticalCN, 'Critical C/N RATIO OF ACTIVE POOL');
      RealIn (Parameter.OMTransfer, 'Annual transfer of organic matter from one layer to the next lower layer');
      RealIn (Parameter.OMIncorporate, 'Annual incorporation of organic matter from the surface to first soil layer');
      RealIn (DecayDummy, 'Decay constant (d-1) for structural surface litter');
      RealIn (Parameter.DecayBranch_StructRatio, 'Decay constant fine woody litter (branches) relative to structural litter');
      RealIn (Parameter.DecayWood_StructRatio, 'Decay constant coarse woody litter (logs) relative to structural litter');
      RealIn (Parameter.Inert_Resistant_Ratio, 'Ratio of decay constants of inert and resistant pools');
      IntegerIn (Parameter.WarmestDay, 'Warmest day of the year (for simulated climate runs)');
      IntegerIn (Parameter.MostPAR, 'Day with highest solar radiation (for simulated climate runs)');
      RealIn (Parameter.Latitude, 'Latitude of the site');
      RealIn (Parameter.RateAdjust, 'Adjustment to soil organic matter decomposition rates');
      RealIn (Parameter.Immobilise, 'Fraction of inorganic nitrogen that is immobilised at each time step');
      RealIn (Parameter.ImmobiliseInSlow, 'Fraction of immobilised N that is immobilised in the slow pool');
      RealIn (Parameter.SnowMelt, 'mm of snow that melt per degree above zero');
      RealIn (Parameter.RadnMelt, 'mm of snow that melt per MJ m-2');
      RealIn (Parameter.SoilTResist, 'Soil resistance to temperature changes');
      RealIn (Parameter.SnowInsulate, 'Extra insulation due to snow layer: extra resistance per mm in snow pack');
      RealIn (Parameter.DefaultSoilDelta, 'Default carbon isotope ratio in soil organic carbon pools');
      RealIn (Parameter.MaxTBoost, 'The proportional increase of soil temperature aboveaverage air temperature');
      RealIn (Parameter.TLAISensitivity, 'Dependence of increase in soil temperature on LAI');
      RealIn (Parameter.LigninInhibition, 'Lignin-inhibition parameter of organic matter decomposition');
      end; {of 'while not eof(defp)' statement}
Close(Defp);
Parameter.Decay8 := Divide(SOMDecay1, Parameter.DecayBranch_StructRatio);
Parameter.Decay9 := Divide(SOMDecay1, Parameter.DecayWood_StructRatio);
Parameter.Decay10 := SOMDecay7 * Parameter.Inert_Resistant_Ratio;
If DecayDummy <> 0 then // must be reading an old data file that still has decay constants listed
   Parameter.RateAdjust := DecayDummy / SOMDecay1; // so adjust the rate constant to make sure actual rate
                                                   // constants are consistent with the old data
End; {of Procedure 'GenSite'}

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

Begin
Assign (fp, Name);
Reset (fp);
iLayer := 1;
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
      IntegerIn (Control.TotalYears, 'Starting Year');
      IntegerIn (Control.ExtraMonths, 'Starting Month');
      IntegerIn (Control.ExtraDays, 'Starting Day');
      IntegerIn (Plant.Age, 'Plant age');
      RealIn (Plant.Height, 'Average stand height');
      RealIn (Plant.Area, 'Cross-sectional area of sapwood');
      RealIn (Plant.DBH, 'Diameter at breast height');
      RealIn (Plant.CanopyCover, 'Stand canopy cover');
      RealIn (Plant.kex, 'Light extinction coefficient');
      RealIn (Derived.TDamageUnits, 'Temperature damage units');
      RealIn (Derived.WaterLimit, 'Water limitation term');
      RealIn (Derived.DecompLimit, 'Water limitation term for decomposition');
      RealIn (Derived.LeafGrowth, 'Seasonal capacity to convert foliage reserves into new foliage');
      RealIn (Derived.Deciduous, 'Seasonal foliage shedding term');
      RealIn (Derived.ExcessN, 'Nitrogen in excess of the ability of trees to absorb it');
      RealIn (Derived.RespnBase, 'Base rate for respiration rate (for temperature adjustments)');
      RealIn (Derived.HeatSum, 'Heat sum');
      RealIn (Weather.LastMin, 'Previous day minimum temperature');
      RealIn (Weather.LastAbsHum, 'Previous day absolute humidity');
      RealIn (Weather.LastRelHum, 'Previous day relative humidity');
      TSoilIn (Weather.TSoil, 'Soil temperature');
      IntegerIn (Control.NextPhenology, 'Sequence counter for phenology settings');
      IntegerIn (Control.PhenologyDayCount, 'Number of days for which the current phenological setting has been operative (if that is the critical element)');
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

Procedure BatchParameters(Name : string);
Begin
Assign (Control.BatchName, Name);
Reset (Control.BatchName);
End; {of Procedure 'BatchParameters'}

{ --- end of file FILE_IO3.PAS ------------------------------------------ }

End.

