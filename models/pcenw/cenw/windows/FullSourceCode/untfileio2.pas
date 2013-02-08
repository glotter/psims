{ ================================================================
  = Project   : CENW                                             =
  ================================================================
  = Modules   : ReadCenW                                         =
  =             SaveCenW                                         =
  =             SavePlant                                        =
  =             SaveProject                                      =
  =             SaveSite                                         =
  =             SavePools                                        =
  =                                                              =
  =             Routines to save files with model parameters.    =
  =             It also includes routines to read and save a     =
  =             default file that tells the program where the    =
  =             latest default project is to be found.           =
  ================================================================
  = File      : unFileIO2.PAS                                    =
  =                                                              =
  = Version   : 3.1                                            =
  ================================================================ }

Unit untFileIO2;

{$V-}

INTERFACE

Uses
  SysUtils, Messages, Dialogs, untFieldValidation, untDivideValidation, untFileIO, untDefaults, untDeclarations;

Procedure ReadCenw (DefaultName: string; var FileName: FileNameType);
Procedure SaveCenw (DefaultName: string; FileName: FileNameType);
procedure SavePlant(name : string);
procedure SaveProject(name : string);
procedure SaveSite(name : string);
procedure SavePools(name : string);

IMPLEMENTATION

Procedure ReadCenW (DefaultName: string; var FileName: FileNameType);
    var FileStrng: text;
        Cha: Char;
        MessageToUser: String;

    Begin
    If FileExists(DefaultName) then
       Begin
       Assign (FileStrng, DefaultName);
       Reset (FileStrng);
       FileName := '                                   ';
       Readln (FileStrng, FileName);
       Readln (FileStrng, Control.Version);
       Readln (FileStrng, Cha);
       If Cha = 'T' then
          Control.AgreeOK := true
       Else
          Control.AgreeOK := false;
       Close (FileStrng);
       Control.OldDirectory := GetCurrentDir;
       Control.ProjectDirectory := ExtractFilePath(FileName);
       SetCurrentDir(ExtractFilePath(FileName));
       End
    Else
       Begin
       MessageToUser := 'File not found!' + chr(10) +
           'The project file may be in the wrong subdirectory' + chr(10) +
           'or you may not have created or copied one' + chr(10) +
           'or deleted one that had been created before.' + chr(10) + chr(10) +
           'The program will try to run with default parameters';
       // show the error message
       ShowMessage(MessageToUser);
       Control.Version := '3.1';
       ProjectDefaults;
       SiteDefaults;
       PlantDefaults;
       PoolDefaults;
       Control.ProjectHasChanged := true;
       Control.PlantHasChanged := true;
       Control.SiteHasChanged := true;
       SavePools(Control.PoolFile);
       Control.RunWithDefaults := true;
       Control.OldDirectory := GetCurrentDir;
       Control.ProjectDirectory := Control.OldDirectory;
       End;
    End; {of procedure 'ReadCenw'}

Procedure SaveCenW (DefaultName: string; FileName: FileNameType);
    var FileStrng: text;
        ExclamationPos, ErrorNo: Integer;
        NameString, st: String;
        s : array[0..255] of char;
    Begin
    SetCurrentDir(Control.OldDirectory);
    {$I-}
    Assign (FileStrng, DefaultName);
    Rewrite (FileStrng);
    {$I+}
    ErrorNo := IOResult;
    If ErrorNo <> 0 then
        Control.ErrorSaving := true
    Else
        Begin
        Control.ErrorSaving := false;
        ExclamationPos := Pos('!', FileName);
        NameString := Copy(FileName, 1, ExclamationPos);
        Writeln (FileStrng, NameString);
        {Extra safeguard introduced to prevent some rubbish characters being appended to the end
        of the project file name. This solution is awkward, but should work.}
        Writeln (FileStrng, Control.Version);
        Writeln (FileStrng, Control.AgreeOK);
        Close (FileStrng);
        End;
    End; {of procedure 'SaveCenw'}

Procedure SaveProject(name : string);
    Var Defp: text;
        TempFile: FileNameType;
        i: integer;
        SetSensVar: SensitivityType;
        SaveInFileVar: SaveVariableOptions;
        ScreenVar: ScreenOptions;

    Procedure LineOut (Datum: real48; Comment: string);
       var Width, Digits: integer;
       Begin
       GetField(Datum, ParFileMaxWidth, Width, Digits);
       Writeln (Defp, Datum: ParFileMaxWidth: Digits, Comment);
       End;  {of procedure 'LineOut'}

    Procedure IntegerOut (Datum: Integer; Comment: string);
       Var StrngVar: String;
       Begin
       Str(Datum, StrngVar);
       Writeln (Defp, StrngVar,' ', Comment);
       End;  {of procedure 'IntegerOut'}

    Procedure LongOut (Datum: LongInt; Comment: string);
       Var StrngVar: String;
       Begin
       Str(Datum, StrngVar);
       Writeln (Defp, StrngVar,' ', Comment);
       End;  {of procedure 'LongOut'}

    Procedure FileOut (FileName: string);
       Var CharPos: Integer;
       Begin
       CharPos := Pos(' ', FileName);
       While CharPos <> 0 do {Turn spaces into '*'}
          Begin
          FileName := Copy(FileName, 1, CharPos -1) + '*' + Copy(FileName, CharPos + 1, Length(FileName));
          CharPos := Pos(' ', FileName);
          End;
       Writeln (Defp, FileName);
       End;  {of procedure 'FileOut'}

    Begin
    assign (defp, Name);   rewrite (defp);
    Writeln(defp, Control.Version + ' Project');
    Writeln(defp, Control.ProjectName, ' (Project name)');

    // save the colours of the screen variables
    for ScreenVar := D_CarbonGain to D_Snow do
      LongOut(ScreenRec.Color[ScreenVar], 'Colour for display of ' + ScreenVariableNames[ScreenVar]);

    Writeln(defp, ScreenRec.Choose[D_CarbonGain]:1,'    Boolean variable for display of total carbon uptake');
    LineOut(ScreenRec.UpRange[D_CarbonGain],' Upper range for display of total carbon uptake');
    LineOut(ScreenRec.LowRange[D_CarbonGain],' Lower range for display of total carbon uptake');
    Writeln(defp, ScreenRec.Choose[D_CAI]:1,'    Boolean variable for display of current annual increment');
    LineOut(ScreenRec.UpRange[D_CAI],' Upper range for display of current annual increment');
    LineOut(ScreenRec.LowRange[D_CAI],' Lower range for display of current annual increment');
    Writeln(defp, ScreenRec.Choose[D_NPP]:1,'    Boolean variable for display of net primary production');
    LineOut(ScreenRec.UpRange[D_NPP],' Upper range for display of net primary production');
    LineOut(ScreenRec.LowRange[D_NPP],' Lower range for display of net primary production');
    Writeln(defp, ScreenRec.Choose[D_NEE]:1,'    Boolean variable for display of net ecosystem exchange');
    LineOut(ScreenRec.UpRange[D_NEE],' Upper range for display of net ecosystem exchange');
    LineOut(ScreenRec.LowRange[D_NEE],' Lower range for display of net ecosystem exchange');
    Writeln(defp, ScreenRec.Choose[D_Respn]:1,'    Boolean variable for display of respiration rate');
    LineOut(ScreenRec.UpRange[D_Respn],' Upper range for display of respiration rate');
    LineOut(ScreenRec.LowRange[D_Respn],' Lower range for display of respiration rate');
    Writeln(defp, ScreenRec.Choose[D_DayCFlux]:1,'    Boolean variable for display of daytime carbon flux');
    LineOut(ScreenRec.UpRange[D_DayCFlux],' Upper range for display of daytime carbon flux');
    LineOut(ScreenRec.LowRange[D_DayCFlux],' Lower range for display of daytime carbon flux');
    Writeln(defp, ScreenRec.Choose[D_NightCFlux]:1,'    Boolean variable for display of nighttime carbon flux');
    LineOut(ScreenRec.UpRange[D_NightCFlux],' Upper range for display of nighttime carbon flux');
    LineOut(ScreenRec.LowRange[D_NightCFlux],' Lower range for display of nighttime carbon flux');
    Writeln(defp, ScreenRec.Choose[D_SolubleCH2O]:1,'    Boolean variable for display of soluble carbohydrate');
    LineOut(ScreenRec.UpRange[D_SolubleCH2O],' Upper range for display of soluble carbohydrate');
    LineOut(ScreenRec.LowRange[D_SolubleCH2O],' Lower range for display of soluble carbohydrate');
    Writeln (defp, ScreenRec.Choose[D_LAI]:1,'    Boolean variable for display of LAI');
    LineOut(ScreenRec.UpRange[D_LAI],' Upper range for display of LAI');
    LineOut(ScreenRec.LowRange[D_LAI],' Lower range for display of LAI');
    Writeln (defp, ScreenRec.Choose[D_pi]:1,'    Boolean variable for display of pi');
    LineOut(ScreenRec.UpRange[D_pi],' Upper range for display of pi');
    LineOut(ScreenRec.LowRange[D_pi],' Lower range for display of pi');
    Writeln (defp, ScreenRec.Choose[D_Wood]:1,'    Boolean variable for display of wood');
    LineOut(ScreenRec.UpRange[D_Wood],' Upper range for display of wood');
    LineOut(ScreenRec.LowRange[D_Wood],' Lower range for display of wood');
    Writeln (defp, ScreenRec.Choose[D_SapW]:1,'    Boolean variable for display of sapwood');
    LineOut(ScreenRec.UpRange[D_SapW],' Upper range for display of sapwood');
    LineOut(ScreenRec.LowRange[D_SapW],' Lower range for display of sapwood');
    Writeln (defp, ScreenRec.Choose[D_HeartW]:1,'    Boolean variable for display of heartwood');
    LineOut(ScreenRec.UpRange[D_HeartW],' Upper range for display of heartwood');
    LineOut(ScreenRec.LowRange[D_HeartW],' Lower range for display of heartwood');
    Writeln (defp, ScreenRec.Choose[D_Bark]:1,'    Boolean variable for display of bark');
    LineOut(ScreenRec.UpRange[D_Bark],' Upper range for display of bark');
    LineOut(ScreenRec.LowRange[D_Bark],' Lower range for display of bark');
    Writeln (defp, ScreenRec.Choose[D_Reserves]:1,'    Boolean variable for display of foliage reserves');
    LineOut(ScreenRec.UpRange[D_Reserves],' Upper range for display of foliage reserves');
    LineOut(ScreenRec.LowRange[D_Reserves],' Lower range for display of foliage reserves');
    Writeln (defp, ScreenRec.Choose[D_Leaf]:1,'    Boolean variable for display of leaves');
    LineOut(ScreenRec.UpRange[D_Leaf],' Upper range for display of leaves');
    LineOut(ScreenRec.LowRange[D_Leaf],' Lower range for display of leaves');
    Writeln (defp, ScreenRec.Choose[D_FineRoot]:1,'    Boolean variable for display of fine roots');
    LineOut(ScreenRec.UpRange[D_FineRoot],' Upper range for display of fine roots');
    LineOut(ScreenRec.LowRange[D_FineRoot],' Lower range for display of fine roots');
    Writeln (defp, ScreenRec.Choose[D_CoarseRoot]:1,'    Boolean variable for display of coarse roots');
    LineOut(ScreenRec.UpRange[D_CoarseRoot],' Upper range for display of coarse roots');
    LineOut(ScreenRec.LowRange[D_CoarseRoot],' Lower range for display of coarse roots');
    Writeln (defp, ScreenRec.Choose[D_Branches]:1,'    Boolean variable for display of branches');
    LineOut(ScreenRec.UpRange[D_Branches],' Upper range for display of branches');
    LineOut(ScreenRec.LowRange[D_Branches],' Lower range for display of branches');
    Writeln (defp, ScreenRec.Choose[D_Reprod]:1,'    Boolean variable for display of reproductive carbon');
    LineOut(ScreenRec.UpRange[D_Reprod],' Upper range for display of reproductive carbon');
    LineOut(ScreenRec.LowRange[D_Reprod],' Lower range for display of reproductive carbon');
    Writeln (defp, ScreenRec.Choose[D_Height]:1,'    Boolean variable for display of stand height');
    LineOut(ScreenRec.UpRange[D_Height],' Upper range for display of stand height');
    LineOut(ScreenRec.LowRange[D_Height],' Lower range for display of stand height');
    Writeln (defp, ScreenRec.Choose[D_DBH]:1,'    Boolean variable for display of DBH');
    LineOut(ScreenRec.UpRange[D_DBH],' Upper range for display of DBH');
    LineOut(ScreenRec.LowRange[D_DBH],' Lower range for display of DBH');
    Writeln (defp, ScreenRec.Choose[D_CanopyCover]:1,'    Boolean variable for display of Canopy cover');
    LineOut(ScreenRec.UpRange[D_CanopyCover],' Upper range for display of Canopy cover');
    LineOut(ScreenRec.LowRange[D_CanopyCover],' Lower range for display of Canopy cover');
    Writeln (defp, ScreenRec.Choose[D_kex]:1,'    Boolean variable for display of light extinction coeff.');
    LineOut(ScreenRec.UpRange[D_kex],' Upper range for display of light extinction coeff.');
    LineOut(ScreenRec.LowRange[D_kex],' Lower range for display of light extinction coeff.');
    Writeln (defp, ScreenRec.Choose[D_BasalArea]:1,'    Boolean variable for display of basal area');
    LineOut(ScreenRec.UpRange[D_BasalArea],' Upper range for display of basal area');
    LineOut(ScreenRec.LowRange[D_BasalArea],' Lower range for display of basal area');
    Writeln (defp, ScreenRec.Choose[D_Stocking]:1,'    Boolean variable for display of stocking');
    LineOut(ScreenRec.UpRange[D_Stocking],' Upper range for display of stocking');
    LineOut(ScreenRec.LowRange[D_Stocking],' Lower range for display of stocking');
    Writeln (defp, ScreenRec.Choose[D_TDamage]:1,'    Boolean variable for display of temperature damage units');
    LineOut(ScreenRec.UpRange[D_TDamage],' Upper range for display of temperature damage units');
    LineOut(ScreenRec.LowRange[D_TDamage],' Lower range for display of temperature damage units');
    Writeln (defp, ScreenRec.Choose[D_NConc]:1,'    Boolean variable for display of foliar [N]');
    LineOut(ScreenRec.UpRange[D_NConc],' Upper range for display of foliar [N]');
    LineOut(ScreenRec.LowRange[D_NConc],' Lower range for display of foliar [N]');
    Writeln (defp, ScreenRec.Choose[D_NConc1]:1,'    Boolean variable for display of [N] in top canopy level');
    LineOut(ScreenRec.UpRange[D_NConc1],' Upper range for display of [N] in top canopy level');
    LineOut(ScreenRec.LowRange[D_NConc1],' Lower range for display of [N] in top canopy level');
    Writeln(defp, ScreenRec.Choose[D_SolubleN]:1,'    Boolean variable for display of soluble nitrogen in plants');
    LineOut(ScreenRec.UpRange[D_SolubleN],' Upper range for display of soluble nitrogen in plants');
    LineOut(ScreenRec.LowRange[D_SolubleN],' Lower range for display of soluble nitrogen in plants');
    Writeln (defp, ScreenRec.Choose[D_WoodN]:1,'    Boolean variable for display of wood nitrogen');
    LineOut(ScreenRec.UpRange[D_WoodN],' Upper range for display of wood nitrogen');
    LineOut(ScreenRec.LowRange[D_WoodN],' Lower range for display of wood nitrogen');
    Writeln (defp, ScreenRec.Choose[D_SapWN]:1,'    Boolean variable for display of sapwood nitrogen');
    LineOut(ScreenRec.UpRange[D_SapWN],' Upper range for display of sapwood nitrogen');
    LineOut(ScreenRec.LowRange[D_SapWN],' Lower range for display of sapwood nitrogen');
    Writeln (defp, ScreenRec.Choose[D_HeartWN]:1,'    Boolean variable for display of heartwood nitrogen');
    LineOut(ScreenRec.UpRange[D_HeartWN],' Upper range for display of heartwood nitrogen');
    LineOut(ScreenRec.LowRange[D_HeartWN],' Lower range for display of heartwood nitrogen');
    Writeln (defp, ScreenRec.Choose[D_BarkN]:1,'    Boolean variable for display of bark nitrogen');
    LineOut(ScreenRec.UpRange[D_BarkN],' Upper range for display of bark nitrogen');
    LineOut(ScreenRec.LowRange[D_BarkN],' Lower range for display of bark nitrogen');
    Writeln (defp, ScreenRec.Choose[D_ReservesN]:1,'    Boolean variable for display of foliage reserves nitrogen');
    LineOut(ScreenRec.UpRange[D_ReservesN],' Upper range for display of foliage reserves nitrogen');
    LineOut(ScreenRec.LowRange[D_ReservesN],' Lower range for display of foliage reserves nitrogen');
    Writeln (defp, ScreenRec.Choose[D_LeafN]:1,'    Boolean variable for display of leaf nitrogen');
    LineOut(ScreenRec.UpRange[D_LeafN],' Upper range for display of leaf nitrogen');
    LineOut(ScreenRec.LowRange[D_LeafN],' Lower range for display of leaf nitrogen');
    Writeln (defp, ScreenRec.Choose[D_FineRootN]:1,'    Boolean variable for display of fine root nitrogen');
    LineOut(ScreenRec.UpRange[D_FineRootN],' Upper range for display of fine root nitrogen');
    LineOut(ScreenRec.LowRange[D_FineRootN],' Lower range for display of fine root nitrogen');
    Writeln (defp, ScreenRec.Choose[D_CoarseRootN]:1,'    Boolean variable for display of coarse root nitrogen');
    LineOut(ScreenRec.UpRange[D_CoarseRootN],' Upper range for display of coarse root nitrogen');
    LineOut(ScreenRec.LowRange[D_CoarseRootN],' Lower range for display of coarse root nitrogen');
    Writeln (defp, ScreenRec.Choose[D_BranchN]:1,'    Boolean variable for display of branch nitrogen');
    LineOut(ScreenRec.UpRange[D_BranchN],' Upper range for display of branch nitrogen');
    LineOut(ScreenRec.LowRange[D_BranchN],' Lower range for display of branch nitrogen');
    Writeln (defp, ScreenRec.Choose[D_ReprodN]:1,'    Boolean variable for display of reproductive nitrogen');
    LineOut(ScreenRec.UpRange[D_ReprodN],' Upper range for display of reproductive nitrogen');
    LineOut(ScreenRec.LowRange[D_ReprodN],' Lower range for display of reproductive nitrogen');
    Writeln (defp, ScreenRec.Choose[D_NSum]:1,'    Boolean variable for display of total nitrogen in system');
    LineOut(ScreenRec.UpRange[D_NSum],' Upper range for display of total nitrogen in system');
    LineOut(ScreenRec.LowRange[D_NSum],' Lower range for display of total nitrogen in system');
    Writeln (defp, ScreenRec.Choose[D_PConc]:1,'    Boolean variable for display of [P] ');
    LineOut(ScreenRec.UpRange[D_PConc],' Upper range for display of [P]');
    LineOut(ScreenRec.LowRange[D_PConc],' Lower range for display of [P]');
    Writeln (defp, ScreenRec.Choose[D_PUptake]:1,'    Boolean variable for display of P uptake');
    LineOut(ScreenRec.UpRange[D_PUptake],' Upper range for display of P uptake');
    LineOut(ScreenRec.LowRange[D_PUptake],' Lower range for display of P uptake');
    Writeln (defp, ScreenRec.Choose[D_PSum]:1,'    Boolean variable for display of total P in system');
    LineOut(ScreenRec.UpRange[D_PSum],' Upper range for display of total P in system');
    LineOut(ScreenRec.LowRange[D_PSum],' Lower range for display of total P in system');
    Writeln (defp, ScreenRec.Choose[D_CLeafLitter]:1,'    Boolean variable for display of fresh foliage litter');
    LineOut(ScreenRec.UpRange[D_CLeafLitter],' Upper range for display of fresh foliage litter');
    LineOut(ScreenRec.LowRange[D_CLeafLitter],' Lower range for display of fresh foliage litter');
    Writeln (defp, ScreenRec.Choose[D_CAll_Litter]:1,'    Boolean variable for display of all fresh litter');
    LineOut(ScreenRec.UpRange[D_CAll_Litter],' Upper range for display of all fresh litter');
    LineOut(ScreenRec.LowRange[D_CAll_Litter],' Lower range for display of all fresh litter');
    Writeln (defp, ScreenRec.Choose[D_CAg_Litter]:1,'    Boolean variable for display of above-ground fresh litter');
    LineOut(ScreenRec.UpRange[D_CAg_Litter],' Upper range for display of above-ground fresh litter');
    LineOut(ScreenRec.LowRange[D_CAg_Litter],' Lower range for display of above-ground fresh litter');
    Writeln (defp, ScreenRec.Choose[D_CMetabSurf]:1,'    Boolean variable for display of metabolic surface litter');
    LineOut(ScreenRec.UpRange[D_CMetabSurf],' Upper range for display of metabolic surface litter');
    LineOut(ScreenRec.LowRange[D_CMetabSurf],' Lower range for display of metabolic surface litter');
    Writeln (defp, ScreenRec.Choose[D_CMetabSoil]:1,'    Boolean variable for display of metabolic soil litter');
    LineOut(ScreenRec.UpRange[D_CMetabSoil],' Upper range for display of metabolic soil litter');
    LineOut(ScreenRec.LowRange[D_CMetabSoil],' Lower range for display of metabolic soil litter');
    Writeln (defp, ScreenRec.Choose[D_CStructSurf]:1,'    Boolean variable for display of structural surface litter');
    LineOut(ScreenRec.UpRange[D_CStructSurf],' Upper range for display of structural surface litter');
    LineOut(ScreenRec.LowRange[D_CStructSurf],' Lower range for display of structural surface litter');
    Writeln (defp, ScreenRec.Choose[D_CStructSoil]:1,'    Boolean variable for display of structural soil litter');
    LineOut(ScreenRec.UpRange[D_CStructSoil],' Upper range for display of structural soil litter');
    LineOut(ScreenRec.LowRange[D_CStructSoil],' Lower range for display of structural soil litter');
    Writeln (defp, ScreenRec.Choose[D_CWoodyLitter]:1,'    Boolean variable for display of the woody litter pool');
    LineOut(ScreenRec.UpRange[D_CWoodyLitter],' Upper range for display of the woody litter pool');
    LineOut(ScreenRec.LowRange[D_CWoodyLitter],' Lower range for display of the woody litter pool');
    Writeln (defp, ScreenRec.Choose[D_CActive]:1,'    Boolean variable for display of the active pool');
    LineOut(ScreenRec.UpRange[D_CActive],' Upper range for display of the active pool');
    LineOut(ScreenRec.LowRange[D_CActive],' Lower range for display of the active pool');
    Writeln (defp, ScreenRec.Choose[D_CSlow]:1,'    Boolean variable for display of the slow pool');
    LineOut(ScreenRec.UpRange[D_CSlow],' Upper range for display of the slow pool');
    LineOut(ScreenRec.LowRange[D_CSlow],' Lower range for display of the slow pool');
    Writeln (defp, ScreenRec.Choose[D_CResistant]:1,'    Boolean variable for display of the resistant pool');
    LineOut(ScreenRec.UpRange[D_CResistant],' Upper range for display of the resistant pool');
    LineOut(ScreenRec.LowRange[D_CResistant],' Lower range for display of the resistant pool');
    Writeln (defp, ScreenRec.Choose[D_SoilRespn]:1,'    Boolean variable for display of soil respiration');
    LineOut(ScreenRec.UpRange[D_SoilRespn],' Upper range for display of soil respiration');
    LineOut(ScreenRec.LowRange[D_SoilRespn],' Lower range for display of soil respiration');
    Writeln (defp, ScreenRec.Choose[D_NLeafLitter]:1,'    Boolean variable for display of fresh foliage litter nitrogen');
    LineOut(ScreenRec.UpRange[D_NLeafLitter],' Upper range for display of fresh foliage litter nitrogen');
    LineOut(ScreenRec.LowRange[D_NLeafLitter],' Lower range for display of fresh foliage litter nitrogen');
    Writeln (defp, ScreenRec.Choose[D_NAll_Litter]:1,'    Boolean variable for display of all fresh litter nitrogen');
    LineOut(ScreenRec.UpRange[D_NAll_Litter],' Upper range for display of all fresh litter nitrogen');
    LineOut(ScreenRec.LowRange[D_NAll_Litter],' Lower range for display of all fresh litter nitrogen');
    Writeln (defp, ScreenRec.Choose[D_NAg_Litter]:1,'    Boolean variable for display of above-ground fresh litter nitrogen');
    LineOut(ScreenRec.UpRange[D_NAg_Litter],' Upper range for display of above-ground fresh litter nitrogen');
    LineOut(ScreenRec.LowRange[D_NAg_Litter],' Lower range for display of above-ground fresh litter nitrogen');
    Writeln (defp, ScreenRec.Choose[D_NMetabSurf]:1,'    Boolean variable for display of N in the metabolic surface litter');
    LineOut(ScreenRec.UpRange[D_NMetabSurf],' Upper range for display of N in the metabolic surface litter');
    LineOut(ScreenRec.LowRange[D_NMetabSurf],' Lower range for display of N in the metabolic surface litter');
    Writeln (defp, ScreenRec.Choose[D_NMetabSoil]:1,'    Boolean variable for display of N in the metabolic soil litter');
    LineOut(ScreenRec.UpRange[D_NMetabSoil],' Upper range for display of N in the metabolic soil litter');
    LineOut(ScreenRec.LowRange[D_NMetabSoil],' Lower range for display of N in the metabolic soil litter');
    Writeln (defp, ScreenRec.Choose[D_NStructSurf]:1,'    Boolean variable for display of N in the structural surface litter');
    LineOut(ScreenRec.UpRange[D_NStructSurf],' Upper range for display of N in the structural surface litter');
    LineOut(ScreenRec.LowRange[D_NStructSurf],' Lower range for display of N in the structural surface litter');
    Writeln (defp, ScreenRec.Choose[D_NStructSoil]:1,'    Boolean variable for display of N in the structural soil litter');
    LineOut(ScreenRec.UpRange[D_NStructSoil],' Upper range for display of N in the structural soil litter');
    LineOut(ScreenRec.LowRange[D_NStructSoil],' Lower range for display of N in the structural soil litter');
    Writeln (defp, ScreenRec.Choose[D_NWoodyLitter]:1,'    Boolean variable for display of N in the woody litter pool');
    LineOut(ScreenRec.UpRange[D_NWoodyLitter],' Upper range for display of N in the woody litter pool');
    LineOut(ScreenRec.LowRange[D_NWoodyLitter],' Lower range for display of N in the woody litter pool');
    Writeln (defp, ScreenRec.Choose[D_NActive]:1,'    Boolean variable for display of N in the active pool');
    LineOut(ScreenRec.UpRange[D_NActive],' Upper range for display of N in the active pool');
    LineOut(ScreenRec.LowRange[D_NActive],' Lower range for display of N in the active pool');
    Writeln (defp, ScreenRec.Choose[D_NSlow]:1,'    Boolean variable for display of N in the slow pool');
    LineOut(ScreenRec.UpRange[D_NSlow],' Upper range for display of N in the slow pool');
    LineOut(ScreenRec.LowRange[D_NSlow],' Lower range for display of N in the slow pool');
    Writeln (defp, ScreenRec.Choose[D_NResistant]:1,'    Boolean variable for display of N in the resistant pool');
    LineOut(ScreenRec.UpRange[D_NResistant],' Upper range for display of N in the resistant pool');
    LineOut(ScreenRec.LowRange[D_NResistant],' Lower range for display of N in the resistant pool');
    Writeln (defp, ScreenRec.Choose[D_NMineral]:1,'    Boolean variable for display of N in the mineral pool');
    LineOut(ScreenRec.UpRange[D_NMineral],' Upper range for display of N in the mineral pool');
    LineOut(ScreenRec.LowRange[D_NMineral],' Lower range for display of N in the mineral pool');
    Writeln (defp, ScreenRec.Choose[D_NLeached]:1,'    Boolean variable for display of leached N');
    LineOut(ScreenRec.UpRange[D_NLeached],' Upper range for display of leached N');
    LineOut(ScreenRec.LowRange[D_NLeached],' Lower range for display of leached N');
    Writeln (defp, ScreenRec.Choose[D_NSoilRespn]:1,'    Dummy variable');
    LineOut(ScreenRec.UpRange[D_NSoilRespn],' Dummy variable');
    LineOut(ScreenRec.LowRange[D_NSoilRespn],' Dummy variable');
    Writeln (defp, ScreenRec.Choose[D_PLeafLitter]:1,'    Boolean variable for display of fresh foliage litter phosphorus');
    LineOut(ScreenRec.UpRange[D_PLeafLitter],' Upper range for display of fresh foliage litter phosphorus');
    LineOut(ScreenRec.LowRange[D_PLeafLitter],' Lower range for display of fresh foliage litter phosphorus');
    Writeln (defp, ScreenRec.Choose[D_PAll_Litter]:1,'    Boolean variable for display of all fresh litter phosphorus');
    LineOut(ScreenRec.UpRange[D_PAll_Litter],' Upper range for display of all fresh litter phosphorus');
    LineOut(ScreenRec.LowRange[D_PAll_Litter],' Lower range for display of all fresh litter phosphorus');
    Writeln (defp, ScreenRec.Choose[D_PMetabSurf]:1,'    Boolean variable for display of P in metabolic surface litter');
    LineOut(ScreenRec.UpRange[D_PMetabSurf],' Upper range for display of P in metabolic surface litter');
    LineOut(ScreenRec.LowRange[D_PMetabSurf],' Lower range for display of P in metabolic surface litter');
    Writeln (defp, ScreenRec.Choose[D_PMetabSoil]:1,'    Boolean variable for display of P in metabolic soil litter');
    LineOut(ScreenRec.UpRange[D_PMetabSoil],' Upper range for display of P in metabolic soil litter');
    LineOut(ScreenRec.LowRange[D_PMetabSoil],' Lower range for display of P in metabolic soil litter');
    Writeln (defp, ScreenRec.Choose[D_PStructSurf]:1,'    Boolean variable for display of P in structural surface litter');
    LineOut(ScreenRec.UpRange[D_PStructSurf],' Upper range for display of P in structural surface litter');
    LineOut(ScreenRec.LowRange[D_PStructSurf],' Lower range for display of P in structural surface litter');
    Writeln (defp, ScreenRec.Choose[D_PStructSoil]:1,'    Boolean variable for display of P in structural soil litter');
    LineOut(ScreenRec.UpRange[D_PStructSoil],' Upper range for display of P in structural soil litter');
    LineOut(ScreenRec.LowRange[D_PStructSoil],' Lower range for display of P in structural soil litter');
    Writeln (defp, ScreenRec.Choose[D_PWoodyLitter]:1,'    Boolean variable for display of P in the woody litter pool');
    LineOut(ScreenRec.UpRange[D_PWoodyLitter],' Upper range for display of P in the woody litter pool');
    LineOut(ScreenRec.LowRange[D_PWoodyLitter],' Lower range for display of P in the woody litter pool');
    Writeln (defp, ScreenRec.Choose[D_PActive]:1,'    Boolean variable for display of P in the active pool');
    LineOut(ScreenRec.UpRange[D_PActive],' Upper range for display of P in the active pool');
    LineOut(ScreenRec.LowRange[D_PActive],' Lower range for display of P in the active pool');
    Writeln (defp, ScreenRec.Choose[D_PSlow]:1,'    Boolean variable for display of P in the slow pool');
    LineOut(ScreenRec.UpRange[D_PSlow],' Upper range for display of P in the slow pool');
    LineOut(ScreenRec.LowRange[D_PSlow],' Lower range for display of P in the slow pool');
    Writeln (defp, ScreenRec.Choose[D_PResistant]:1,'    Boolean variable for display of P in the resistant pool');
    LineOut(ScreenRec.UpRange[D_PResistant],' Upper range for display of P in the resistant pool');
    LineOut(ScreenRec.LowRange[D_PResistant],' Lower range for display of P in the resistant pool');
    Writeln (defp, ScreenRec.Choose[D_PMineral]:1,'    Boolean variable for display of P in the mineral pool');
    LineOut(ScreenRec.UpRange[D_PMineral],' Upper range for display of P in the mineral pool');
    LineOut(ScreenRec.LowRange[D_PMineral],' Lower range for display of P in the mineral pool');
    Writeln (defp, ScreenRec.Choose[D_PSoilRespn]:1,'    Dummy variable');
    LineOut(ScreenRec.UpRange[D_PSoilRespn],' Dummy variable');
    LineOut(ScreenRec.LowRange[D_PSoilRespn],' Dummy variable');
    Writeln (defp, ScreenRec.Choose[D_Tmax]:1,'    Boolean variable for display of Tmax');
    LineOut(ScreenRec.UpRange[D_Tmax],' Upper range for display of Tmax');
    LineOut(ScreenRec.LowRange[D_Tmax],' Lower range for display of Tmax');
    Writeln (defp, ScreenRec.Choose[D_Tmin]:1,'    Boolean variable for display of Tmin');
    LineOut(ScreenRec.UpRange[D_Tmin],' Upper range for display of Tmin');
    LineOut(ScreenRec.LowRange[D_Tmin],' Lower range for display of Tmin');
    Writeln (defp, ScreenRec.Choose[D_Tmean]:1,'    Boolean variable for display of Tmean');
    LineOut(ScreenRec.UpRange[D_Tmean],' Upper range for display of Tmean');
    LineOut(ScreenRec.LowRange[D_Tmean],' Lower range for display of Tmean');
    Writeln (defp, ScreenRec.Choose[D_Tsoil]:1,'    Boolean variable for display of Tsoil');
    LineOut(ScreenRec.UpRange[D_Tsoil],' Upper range for display of Tsoil');
    LineOut(ScreenRec.LowRange[D_Tsoil],' Lower range for display of Tsoil');
    Writeln (defp, ScreenRec.Choose[D_Tday]:1,'    Boolean variable for display of Tday');
    LineOut(ScreenRec.UpRange[D_Tday],' Upper range for display of Tday');
    LineOut(ScreenRec.LowRange[D_Tday],' Lower range for display of Tday');
    Writeln (defp, ScreenRec.Choose[D_PAR]:1,'    Boolean variable for display of PAR');
    LineOut(ScreenRec.UpRange[D_PAR],' Upper range for display of PAR');
    LineOut(ScreenRec.LowRange[D_PAR],' Lower range for display of PAR');
    Writeln (defp, ScreenRec.Choose[D_CO2]:1,'    Boolean variable for display of ambient CO2');
    LineOut(ScreenRec.UpRange[D_CO2],' Upper range for display of ambient CO2');
    LineOut(ScreenRec.LowRange[D_CO2],' Lower range for display of ambient CO2');
    Writeln (defp, ScreenRec.Choose[D_Rain]:1,'    Boolean variable for display of Rain');
    LineOut(ScreenRec.UpRange[D_Rain],' Upper range for display of Rain');
    LineOut(ScreenRec.LowRange[D_Rain],' Lower range for display of Rain');
    Writeln (defp, ScreenRec.Choose[D_StoredWater]:1,'    Boolean variable for display of stored water');
    LineOut(ScreenRec.UpRange[D_StoredWater],' Upper range for display of stored water');
    LineOut(ScreenRec.LowRange[D_StoredWater],' Lower range for display of stored water');
    Writeln (defp, ScreenRec.Choose[D_AbsHum]:1,'    Boolean variable for display of absolute humidity');
    LineOut(ScreenRec.UpRange[D_AbsHum],' Upper range for display of absolute humidity');
    LineOut(ScreenRec.LowRange[D_AbsHum],' Lower range for display of absolute humidity');
    Writeln (defp, ScreenRec.Choose[D_RelHum]:1,'    Boolean variable for display of relative humidity');
    LineOut(ScreenRec.UpRange[D_RelHum],' Upper range for display of relative humidity');
    LineOut(ScreenRec.LowRange[D_RelHum],' Lower range for display of relative humidity');
    Writeln (defp, ScreenRec.Choose[D_WaterLimit]:1,'    Boolean variable for display of water limitation');
    LineOut(ScreenRec.UpRange[D_WaterLimit],' Upper range for display of water limitation');
    LineOut(ScreenRec.LowRange[D_WaterLimit],' Lower range for display of water limitation');
    Writeln (defp, ScreenRec.Choose[D_HeatSum]: 1, '    Boolean variable for display of heat sums');
    LineOut (ScreenRec.UpRange[D_HeatSum], ' Upper range for display of heat sums');
    LineOut (ScreenRec.LowRange[D_HeatSum], ' Lower range for display of heat sums');
    Writeln (defp, ScreenRec.Choose[D_Transpiration]:1,'    Boolean variable for display of transpiration rate');
    LineOut(ScreenRec.UpRange[D_Transpiration],' Upper range for display of transpiration rate');
    LineOut(ScreenRec.LowRange[D_Transpiration],' Lower range for display of transpiration rate');
    Writeln (defp, ScreenRec.Choose[D_Evaporation]:1,'    Boolean variable for display of evaporation rate');
    LineOut(ScreenRec.UpRange[D_Evaporation],' Upper range for display of evaporation rate');
    LineOut(ScreenRec.LowRange[D_Evaporation],' Lower range for display of evaporation rate');
    Writeln (defp, ScreenRec.Choose[D_Snow]:1,'    Boolean variable for display of snow');
    LineOut(ScreenRec.UpRange[D_Snow],' Upper range for display of snow');
    LineOut(ScreenRec.LowRange[D_Snow],' Lower range for display of snow');
    Writeln (defp, ScreenRec.Choose[D_Drainage]:1,'    Boolean variable for display of deep drainage');
    LineOut(ScreenRec.UpRange[D_Drainage],' Upper range for display of deep drainage');
    LineOut(ScreenRec.LowRange[D_Drainage],' Lower range for display of deep drainage');
    Writeln (defp, ScreenRec.Choose[D_Dummy]:1,'    Boolean variable for display of dummy variable');
    LineOut(ScreenRec.UpRange[D_Dummy],' Upper range for display of deep dummy variable');
    LineOut(ScreenRec.LowRange[D_Dummy],' Lower range for display of deep dummy variable');
    For SaveInFileVar := S_Year to S_Dummy do
        Writeln (defp, SaveVar.Choose[SaveInFileVar]:1,'    Boolean variable for filesave of particular variables');
    For SetSensVar := Dummy to EndDummy do
        Writeln (defp, TestSens.Choose[SetSensVar]:1,'    Boolean variable for sensitivity testing of particular parameters');
    Writeln (defp, WeatherFile[W_Tmax]:1,'    Boolean variable to read Tmax ');
    Writeln (defp, WeatherFile[W_Tmin]:1,'    Boolean variable to read Tmin');
    Writeln (defp, WeatherFile[W_Tmean]:1,'    Boolean variable to read Tmean');
    Writeln (defp, WeatherFile[W_Tsoil]:1,'    Boolean variable to read Tsoil');
    Writeln (defp, WeatherFile[W_PAR]:1,'    Boolean variable to read PAR');
    Writeln (defp, WeatherFile[W_Rain]:1,'    Boolean variable to read Rainfall');
    Writeln (defp, WeatherFile[W_AbsHum]:1,'    Boolean variable to read absolute humidity');
    Writeln (defp, WeatherFile[W_RelHum]:1,'    Boolean variable to read relative humidity');
    Writeln (defp, WeatherFile[W_CO2]:1,'    Boolean variable to read CO2 concentration');
    Writeln (defp, MaxWidth:ParFileMaxWidth,' Variable to define the width for each variable in the output file');
    LineOut(Parameter.StemDeath, ' Fraction of trees dying annually');
    LineOut (Parameter.FertiliserRelease, ' Daily rate of fertiliser release');
    Writeln (defp, Control.Fertilise_DateType, '       Interpretation of fertiliser dates');
    Writeln (defp, Event.nFertilisations:ParFileMaxWidth, ' Number of times that fertiliser is to be applied');
    For i := 1 to Event.nFertilisations do
        Begin
        Writeln (defp, Event.FertiliseTimes[i, 1], ' Day for fertiliser application');
        Writeln (defp, Event.FertiliseTimes[i, 2], ' Month for fertiliser application');
        Writeln (defp, Event.FertiliseTimes[i, 3], ' Year for fertiliser application');
        LineOut (Event.FertiliseAmount[i], ' Application rate of fertiliser');
        End;
    Writeln (defp, Event.HarvestUnits, '       Interpretation of harvest units');
    Writeln (defp, Control.Harvest_DateType, '       Interpretation of harvest dates');
    Writeln (defp, Event.nHarvests:ParFileMaxWidth, ' Number of harvests or thinnings');
    For i := 1 to Event.nHarvests do
        Begin
        LongOut (Event.HarvestTimes[i, 1], ' Day for harvest/ thinning');
        LongOut (Event.HarvestTimes[i, 2], ' Month for harvest/ thinning');
        LongOut (Event.HarvestTimes[i, 3], ' Year for harvest/ thinning');
        LongOut (Event.HarvestTimes[i, 4], ' Day number of harvest/ thinning');
        LineOut (Event.WoodCut[i], ' Fraction of stems cut');
        LineOut (Event.RelativeSize[i], ' Size of harvested trees relative to that of average trees');
        Writeln (defp, Event.AdjustStocking[i], ' Indicates whether stocking rate is to be adjusted during the harvest');
        LineOut (Event.BranchesCut[i], ' Fraction of branches/ leaves cut');
        LineOut (Event.WoodRemoval[i], ' Fraction of stems removed from the site');
        LineOut (Event.FineRemoval[i], ' Fraction of leaves, branches, etc removed from the site');
        End;
    Writeln (defp, Control.Pest_DateType, '       Interpretation of pest dates');
    Writeln (defp, Event.PestDamageUnits, '       Units in which pest damage is expressed (% or W)');
    Writeln (defp, Event.nPests:ParFileMaxWidth, ' Number of pest outbreaks');
    For i := 1 to Event.nPests do
        Begin
        LongOut (Event.PestTimes[i, 1], ' Day for pest outbreak');
        LongOut (Event.PestTimes[i, 2], ' Month for pest outbreak');
        LongOut (Event.PestTimes[i, 3], ' Year for pest outbreak');
        LongOut (Event.PestTimes[i, 4], ' Day number of pest outbreak');
        LongOut (Event.PestTimes[i, 5], ' Duration (days) of of pest outbreak');
        LineOut (Event.LeafDamage[i], ' Daily foliage damage by insects');
        LineOut (Event.SolubleDamage[i], ' Daily loss by sap-sucking insect pests');
        LineOut (Event.SenescenceDamage[i], ' Daily induced foliage senesce by pest damage');
        LineOut (Event.PhotosynthesisFraction[i], ' Fractional reduction in photosynthesis for the duration of the pest outbreak');
        LineOut (Event.PestMortality[i], ' Death rate due to pest damage');
        LineOut (Event.PestDeathRatio[i], ' Size ratio of trees killed by pests to living trees');
        End;
    Writeln (defp, Control.Fire_DateType, '       Interpretation of fire dates');
    Writeln (defp, Event.nFires:ParFileMaxWidth, ' Number of fire outbreaks');
    For i := 1 to Event.nFires do
        Begin
        LongOut (Event.FireTimes[i, 1], ' Day for fire outbreak');
        LongOut (Event.FireTimes[i, 2], ' Month for fire outbreak');
        LongOut (Event.FireTimes[i, 3], ' Year for fire outbreak');
        LongOut (Event.FireTimes[i, 4], ' Day number of fire outbreak');
        LineOut (Event.LeafBurn[i], ' Fraction of foliage burnt during a fire');
        LineOut (Event.WoodBurn[i], ' Fraction of stem wood burnt during a fire');
        LineOut (Event.LitterBurn[i], ' Fraction of forest litter burnt during a fire');
        LineOut (Event.WoodToChar[i], ' Fraction of wood converted to charcoal during a fire');
        LineOut (Event.FineToChar[i], ' Fraction of fine material (foliage and litter) converted to charcoal during a fire');
        LineOut (Event.LeafBurnSenesc[i], ' Fraction of foliage killed (but not burnt) during a fire');
        LineOut (Event.WoodBurnSenesc[i], ' Fraction of trees killed (but not burnt) during a fire');
        LineOut (Event.Burn_N_CRatio[i], ' Ratio of nitrogen to carbon losses during combustion');
        End;
    Writeln (defp, Control.Plough_DateType, '       Interpretation of plough dates');
    Writeln (defp, Event.nPloughing:ParFileMaxWidth, ' Number of ploughing events');
    For i := 1 to Event.nPloughing do
        Begin
        LongOut (Event.PloughTimes[i, 1], ' Day for ploughing');
        LongOut (Event.PloughTimes[i, 2], ' Month for ploughing');
        LongOut (Event.PloughTimes[i, 3], ' Year for ploughing');
        LongOut (Event.PloughTimes[i, 4], ' Day number of ploughing');
        Writeln (defp, Event.PloughDepth[i]:3, ' Depth of Ploughing (layers)');
        End;
    Writeln (defp, Event.nEnvironments:ParFileMaxWidth, ' Number of times environmental variables are changed');
    For i := 1 to Event.nEnvironments do
        Begin
        LongOut (Event.EnvironmentTimes[i], ' Day number of change in environmental variable');
        LineOut (Event.CO2[i], ' Change CO2 concentration');
        LineOut (Event.Temperature[i], ' Change Temperature');
        LineOut (Event.Rainfall[i], ' Change Rainfall');
        End;
    Writeln (defp, Event.Irrigate:1, '    Boolean variable to indicate whether irrigation is applied');
    Writeln (defp, Event.IrrigationType, '       Type of irrigation');
    LineOut (Event.IrrigationAmount, ' Amount of irrigation applied');
    LineOut (Event.IrrigationFraction, ' Fraction of field capacity to which irrigation water is applied');
    Writeln (defp, Event.IrrigationInterval:ParFileMaxWidth, ' Interval between applications of irrigation');
    Writeln (defp, Event.IrrigStartDay:ParFileMaxWidth, ' Starting day for irrigation');
    Writeln (defp, Event.IrrigStartMonth:ParFileMaxWidth, ' Starting month for irrigation');
    Writeln (defp, Event.IrrigStartYear:ParFileMaxWidth, ' Starting year for irrigation');
    Writeln (defp, Event.IrrigEndDay:ParFileMaxWidth, ' Last day for irrigation');
    Writeln (defp, Event.IrrigEndMonth:ParFileMaxWidth, ' Last month for irrigation');
    Writeln (defp, Event.IrrigEndYear:ParFileMaxWidth, ' Last year for irrigation');
    Writeln (defp, Control.nYears:ParFileMaxWidth, ' years for simulation run');
    Writeln (defp, Control.nMonths:ParFileMaxWidth, ' months for simulation run');
    Writeln (defp, Control.nDays:ParFileMaxWidth, ' number of days for simulation run');
    LineOut(Control.Initial.SapWood[C], ' Initial setting for sapwood carbon');
    LineOut(Control.Initial.HeartWood[C], ' Initial setting for heartwood carbon');
    LineOut(Control.Initial.CoarseRoot[C], ' Initial setting for coarseroot carbon');
    LineOut(Control.Initial.FineRoot[C], ' Initial setting for fineroot carbon');
    LineOut(Control.Initial.Branches[C], ' Initial setting for branch carbon');
    LineOut(Control.Initial.Bark[C], ' Initial setting for bark carbon');
    LineOut(Control.Initial.Leaves[C], ' Initial setting for foliage carbon');
    LineOut(Control.Initial.Pollen[C], ' Initial setting for pollen carbon');
    LineOut(Control.Initial.Fruit[C], ' Initial setting for fruit carbon');
    LineOut(Control.Initial.Soluble[C], ' Initial setting for soluble carbon');
    LineOut(Control.Initial.Reserves[C], ' Initial setting for leaf primordia carbon');
    LineOut(Control.Initial.SapWood[N], ' Initial setting for sapwood nitrogen');
    LineOut(Control.Initial.HeartWood[N], ' Initial setting for heartwood nitrogen');
    LineOut(Control.Initial.CoarseRoot[N], ' Initial setting for coarseroot nitrogen');
    LineOut(Control.Initial.FineRoot[N], ' Initial setting for fineroot nitrogen');
    LineOut(Control.Initial.Branches[N], ' Initial setting for branch nitrogen');
    LineOut(Control.Initial.Bark[N], ' Initial setting for bark nitrogen');
    LineOut(Control.Initial.Leaves[N], ' Initial setting for foliage nitrogen');
    LineOut(Control.Initial.Pollen[N], ' Initial setting for pollen nitrogen');
    LineOut(Control.Initial.Fruit[N], ' Initial setting for fruit nitrogen');
    LineOut(Control.Initial.Soluble[N], ' Initial setting for soluble nitrogen');
    LineOut(Control.Initial.Reserves[N], ' Initial setting for leaf primordia nitrogen');
    IntegerOut(Control.Initial.Age, ' Initial setting for stand age');
    LineOut(Control.Initial.Stocking, ' Initial setting for stocking');
    LineOut(Control.Initial.DBH, ' Initial setting for diameter');
    LineOut(Control.Initial.Height, ' Initial setting for height');
    IntegerOut(Control.Initial.Days, ' Initial setting starting day');
    IntegerOut(Control.Initial.Months, ' Initial setting starting month');
    IntegerOut(Control.Initial.Years, ' Initial setting starting year');
    Writeln (defp, Control.nDisplays:ParFileMaxWidth, ' number of times output is given');
    Writeln (defp, Control.nDiskOut:ParFileMaxWidth, ' number of times output is written to disk');
    Writeln (defp, Control.CalcType, '       Calculation type');
    Writeln (defp, Control.ResetPlantPools:1, '    Boolean variable to indicate whether plant pools should be reset at the beginning of a run');
    Writeln (defp, Control.DecayOnly:1, '    Boolean variable for calculations of organic matter decomposition only');
    Writeln (defp, Control.AllOneLayer:1, '    Boolean variable for organic matter to be treated as single layer');
    Writeln (defp, Control.OutputByLayer:1, '    Boolean variable whether output is to be given by soil layer');
    Writeln (defp, Control.IncludeIsotopes:1, '    Boolean variable whether to include information about carbon isotopes');
    LineOut (Parameter.FertilityAdjust, ' Adjustment to soil fertility level at start-up');
    Writeln (defp, Control.ClimType, '       Type of climate input');
    Writeln (defp, Control.nProjects:1, '   Number of projects to load for multiple project runs');
    If Control.nProjects > 0 then
        FileOut (Control.MultipleRunPoolFile);
    For i := 1 to Control.nProjects do
        FileOut (Control.MultipleRuns[i]);
    FileOut (Control.ClimFile);
    FileOut (Control.FileOut);
    FileOut (Control.PoolFile);
    If Control.SavePoolFile <> '' then
       TempFile := Control.SavePoolFile
    Else
       TempFile := Control.PoolFile;
    TempFile[Length(TempFile) - 2] := 'S';
    FileOut (TempFile);
    FileOut (Control.SiteFile);
    Writeln (defp, Control.nCohorts:1,  '   Number of cohorts to simulate');
    FileOut (Control.PlantFile);
    LineOut (Control.CConversion, ' Coefficient to convert between carbon and biomass');
    LineOut (Control.NConversion, ' Coefficient to convert between nitrogen units in model and output');
    LineOut (Parameter.gamma, ' Conversion factor from MJ to umol');
    FileOut (Control.BatchFile);
    LongOut (Control.BatchCalcs, ' For batch run: number of calculations');
    If Control.Equil.EquilTarget = SOM then
       Writeln (defp, 'SOM       Type of target value for equilibrium runs')
    Else if Control.Equil.EquilTarget = LeafNConc then
       Writeln (defp, 'Foliage_[N]   Type of target value for equilibrium runs')
    Else if Control.Equil.EquilTarget = LeafNitrogen then
       Writeln (defp, 'Foliage_nitrogen   Type of target value for equilibrium runs')
    Else if Control.Equil.EquilTarget = Leafmass then
       Writeln (defp, 'Foliage_mass       Type of target value for equilibrium runs')
    Else {if Control.Equil.EquilTarget = WoodMass then }
       Writeln (defp, 'Wood_mass       Type of target value for equilibrium runs');
    If Control.Equil.EquilParameter = BiolNFix then
       Writeln (defp, 'Biological_N_fixation  Type of parameter to change for equilibrium runs')
    Else if Control.Equil.EquilParameter = NFraction then
       Writeln (defp, 'N_fractional_loss  Type of parameter to change for equilibrium runs');
    Longout (Control.Equil.MaxIterations, ' Maximum number of iterations for equlibrium runs');
    Longout (Control.Equil.MaxGoodCount, ' Required number of good fits to satisfy user of equlibrium conditions');
    LineOut (Control.Equil.Criterion1, ' Value for equilibrium criterium 1 in equilibrium runs');
    LineOut (Control.Equil.Criterion2, ' Value for equilibrium criterium 2 in equilibrium runs');
    LineOut (Control.Equil.Criterion3, ' Value for equilibrium criterium 3 in equilibrium runs');
    LineOut (Control.Equil.TargetValue, ' Target value for equilibrium runs');
    LineOut (Control.Equil.DeltaMin, ' Minimum delta value for equilibrium runs');
    LineOut (Control.Equil.DeltaMax, ' Maximum delta value for equilibrium runs');
    LineOut (Control.Equil.DeltaAdjust, ' Percentage adjustment of delta value for equilibrium runs');
    LineOut (Control.Equil.MaxChangeRatio, ' Maximum change ratio in equilibrium runs');
    LineOut (Control.Equil.BoostResistant, ' Boosting of changes in resistant organic matter relative to other changes');
    Writeln (defp, Control.Equil.SamePlantPools:1,'    Boolean variable to indicate whether to keep the same initial plant pools in searching for equilibrium');
    If Parameter.MortalityType = Fraction then
       Writeln (defp, 'Fraction:        Type of function to calculate mortality')
    Else if Parameter.MortalityType = Density then
       Writeln (defp, 'Density:        Type of function to calculate mortality')
    Else
       Writeln (defp, 'Both:       Type of function to calculate mortality');
    LineOut(Parameter.DeathRatio, ' Ratio of tree sizes of dieing and average trees');
    close (defp);
End; {of Procedure 'SaveProject'}

Procedure SavePlant(name : string);
    Var Defp: text;
        i: integer;

    Procedure IntegerOut (Datum: Integer; Comment: string);
       Var StrngVar: String;
       Begin
       Str(Datum, StrngVar);
       Writeln (Defp, StrngVar,' ', Comment);
       End;  {of procedure 'IntegerOut'}

    Procedure LineOut (Datum: real48; Comment: string);
       var Width, Digits: integer;
       Begin
       GetField(Datum, ParFileMaxWidth, Width, Digits);
       Writeln (Defp, Datum: ParFileMaxWidth: Digits, Comment);
       End;  {of procedure 'LineOut'}

    Begin
    assign (defp, Name);   rewrite (defp);
    Writeln(defp, Control.Version + ' Plant parameters');
    LineOut(Parameter.kexmax, ' Max. light extinction coefficient');
    LineOut(Parameter.KlowRange, ' Max. decrease of max. light extinction coeff');
    LineOut(Parameter.Albedo, ' Leaf albedo');
    LineOut(Parameter.SLA, ' Specific leaf area ');
    LineOut(Parameter.NMVOC, ' Loss rate of non-methyl volatile organic carbon');
    LineOut(Parameter.BallBerry1, ' Ball-Berry parameter that gives the slope between gs and Ah/c in unstressed plants');
    LineOut(Parameter.BallBerry2, ' Ball-Berry parameter that gives the slope between gs and Ah/c in stressed plants');
    LineOut(Parameter.MicroFract, ' Fraction of "Active SON" taken up daily through micorrhizal associations');
    Writeln (defp, Parameter.VariableNFixation:1, ' Calculate biological N fixation as a function of N status?');
    LineOut(Parameter.BiolFix, ' Amount of nitrogen fixed biologically per unit carbon fixed');
    LineOut(Parameter.RespFromN, ' Daily respiration rate per unit N');
    LineOut(Parameter.RespnAdjust, ' Time constant for adjustments in temperature response of respiration rate');
    LineOut(Parameter.GrowthRespn, ' Fraction of carbon used up in growth respiration');
    LineOut(Parameter.SenescLeafRatio, ' Ratio of N in senescing and living leaves');
    LineOut(Parameter.InternalNRatio, ' Ratio of N between average foliage and top foliage layers');
    LineOut(Parameter.DrySenesc, ' Maximum fraction of leaves and branches dying daily during drought');
    LineOut(Parameter.StressLimit, ' Relative soil water content for the commencement of plant stress');
    Writeln (defp, Parameter.DirectEvapType, '       Flag whether direct rain evap is calculated as const or f(LAI)');
    LineOut(Parameter.DirectEvapSlope, ' Slope of relationship relating intercepted rain and LAI');
    LineOut(Parameter.DirectEvapFract, ' Fraction of rain lost to direct evaporation');
    LineOut(Parameter.BarkSenesc, ' Fraction of bark senescing daily');
    LineOut(Parameter.LeafSenesc, ' Fraction of foliage senescing daily');
    LineOut(Parameter.BranchSenesc, ' Fraction of branches senescing daily');
    LineOut(Parameter.RootSenesc, ' Fraction of roots senescing daily');
    LineOut(Parameter.FruitSenesc, ' Fraction of fruit senescing daily');
    LineOut(Parameter.PollenSenesc, ' Fraction of pollen senescing daily (can be greater than 1)');
    LineOut(Parameter.SenescLowLight, ' Low-light level at which foliage senesces');
    LineOut(Parameter.MaxSenescLowLight, ' Maximum daily foliage senescence rate due to low light');
    LineOut(Parameter.RootLeafRatio1, ' Ratio of fine roots to foliage in unstressed plants');
    LineOut(Parameter.RootLeafRatio2, ' Ratio of fine roots to foliage in nutritionally stressed plants');
    LineOut(Parameter.LeafBranchRatio, ' Ratio of foliage to branches');
    LineOut(Parameter.WoodBranchRatio, ' Ratio of wood to branches');
    LineOut(Parameter.CoarseRootWoodRatio, ' Ratio of coarse roots to stem wood');
    LineOut(Parameter.BarkWoodRatio, ' Ratio of bark to wood');
    LineOut(Parameter.MinWoodAlloc, ' Minimum amount of carbon allocated to stem-wood production in young trees');
    LineOut(Parameter.C_FruitAlloc, ' Carbon allocation to fruiting bodies');
    LineOut(Parameter.C_PollenAlloc, ' Carbon allocation to pollen');
    Writeln(defp, Parameter.SexAge:ParFileMaxWidth, ' Minimum age for sexual reproduction');
    Writeln(defp, Parameter.SapWoodYears:ParFileMaxWidth, ' Longevity of sapwood before turning into heartwood');
    LineOut(Parameter.Ncrit, ' Nitrogen concentration that is saturating');
    LineOut(Parameter.Nmax, ' Maximum nitrogen concentration to which nitrogen can be taken up by foliage');
    LineOut(Parameter.N0, ' Nitrogen concentration ''compensation point''');
    LineOut(Parameter.KmGrowth[C], ' Km term for the soluble carbohydrate dependence of growth');
    LineOut(Parameter.KmGrowth[N], ' Km term for the soluble inorganic nitrogen dependence of growth');
    LineOut(Parameter.Nloss, ' Fraction of N volatilised during mineralisation processes');
    LineOut(Parameter.WoodRetrans, ' ratio of nitrogen concentrations in heartwood and sapwood');
    LineOut(Parameter.bRoots, ' ratio of nitrogen concentrations in roots and foliage');
    LineOut(Parameter.bWood, ' ratio of nitrogen concentrations in wood and foliage');
    LineOut(Parameter.bBark, ' ratio of nitrogen concentrations in bark and foliage');
    LineOut(Parameter.bBranch, ' ratio of nitrogen concentrations in branches and foliage');
    LineOut(Parameter.bFruit, ' ratio of nitrogen concentrations in fruit and foliage');
    LineOut(Parameter.bPollen, ' ratio of nitrogen concentrations in pollen and foliage');
    LineOut (Parameter.WoodLignin, ' Lignin concentration in wood');
    LineOut (Parameter.LeafLignin, ' Lignin concentration in leaves');
    LineOut (Parameter.RootLignin, ' Lignin concentration in roots');
    LineOut (Parameter.LigninInhibition, ' Term that describes the extent by which structural-litter decomposition is inhibited by lignin');
    LineOut (Parameter.Amax, ' Maximum nitrogen use efficiency (Amax / [N])');
    LineOut (Parameter.alpha, ' Photosynthetic quantum yield');
    LineOut (Parameter.Theta, ' Theta - curvature term in light dependence of daily carbon gain');
    LineOut (Parameter.RelkPEP, ' PEP carboxylase activity relative to maximum assimilation rate (for C4 phs)');
    LineOut (Parameter.Beta, ' Curvature term in the transition from CO2 limited to CO2 saturated phs (in C4 phs)');
    LineOut (Parameter.TMinLim, ' Lower mean daytime temperature limit for plant growth');
    LineOut (Parameter.TOpt1, ' Lower mean daytime temperature limit for optimum plant growth');
    LineOut (Parameter.TOpt2, ' Upper mean daytime temperature limit for optimum plant growth');
    LineOut (Parameter.TMaxLim, ' Upper mean daytime temperature limit for plant growth');
    LineOut (Parameter.TFrost, ' Threshold nighttime temperature for leaf damage');
    LineOut (Parameter.TScorch, ' Threshold daytime temperature for leaf damage');
    LineOut (Parameter.TSensitivity, ' Growth sensitivity to accumulated temperature damage');
    LineOut (Parameter.TRepair, ' Rate of repair from accumulated temperature damage');
    LineOut (Parameter.HDInter, ' Intercept in the allometric relationship of height versus DBH');
    LineOut (Parameter.HDSlope, ' Slope in the allometric relationship of height versus DBH');
    LineOut (Parameter.WDSlope, ' Slope in the allometric relationship of stem weight versus DBH');
    LineOut (Parameter.WHSlope, ' Slope in the allometric relationship of stem weight versus height');
    LineOut (Parameter.Mindbh, ' Minimum dbh at which to apply the allometric relationship');
    LineOut (Parameter.WoodDensity, ' Wood density');
    LineOut (Parameter.Transmit, ' Leaf transmissivity');
    LineOut (Parameter.gamma, ' Coversion factor from MJ to umol');
    LineOut (Parameter.TMaxRepairTime, ' Maximum number of days for temperature damage to be fully repaired');
    LineOut (Parameter.ExcessNUptake, ' Ratio of total nitrogen uptake into foliage and that utilised in new growth');
    LineOut (Parameter.LitterWHC, ' Water holding capacity of surface litter');
    LineOut (Parameter.Mulching, ' Mulching effect of surface litter');
    LineOut (Parameter.AeroResist, ' Canopy aerodynamic resistance (s m-1)');
    If Parameter.RespnType = Ratio then
       Writeln (defp, 'Ratio:        Type of respiration function')
    Else
       Writeln (defp, 'Basics:       Type of respiration function');
    LineOut (Parameter.Respnalpha, ' Parameter alpha in the T-respn relationship');
    LineOut (Parameter.Respnbeta, ' Parameter beta in the T-respn relationship');
    LineOut (Parameter.RespnOpt, ' Temperature for maximum respiration rate');
    LineOut (Parameter.RespnRatio, ' Ratio of respiration rate to daily photosynthetic carbon gain');
    Writeln (defp, Parameter.RespnTAcclimation:1, '    Boolean variable to indicate whether to include temperature acclimation for plant respiration');
    LineOut (Parameter.Three_Two_Power_Law, ' Parameter in the 3/2 power law relationship');
    Writeln (defp, Parameter.AgeDecline:1, '    Boolean variable to indicate whether to include age-related NPP decline');
    IntegerOut (Parameter.MatureAge, ' Age of maturity (at which NPP is only 50% of young-tree NPP');
    Writeln (defp, Parameter.SizeDecline:1, '    Boolean variable to indicate whether to include size-related NPP decline');
    IntegerOut (Parameter.MatureSize, ' Size of maturity (at which NPP is only 50% of young-tree NPP');
    Writeln (defp, Parameter.FoliageClumping:1, ' Boolean variable to indicate whether to include foliage clumping for plant light interception');
    Writeln (defp, Parameter.ConstantLeafN:1, ' Boolean variable to indicate whether to run with constant leaf nitrogen');
    LineOut (Parameter.ConstantLeafNValue, ' Constant leaf [N] (if that is to be kept constant)');
    LineOut (Parameter.AgePower, ' Power term in age related decline in NPP');
    LineOut (Parameter.SizePower, ' Power term in size related decline in NPP');
    LineOut (Parameter.DefaultPlantDelta, ' Default plant carbon isotope ratio');
    If Parameter.Phs = C3 then
       Writeln (defp, 'C3:     Photosynthetic pathway')
    Else
       Writeln (defp, 'C4:     Photosynthetic pathway');
    If Parameter.SetDeltaType = SetValue then
       Writeln (defp, 'SetValue:     Type of delta setting for new carbon')
    Else {If VarStr = 'CalculateValue' then}
       Writeln (defp, 'CalculateValue:     Type of delta setting for new carbon');
    LineOut (Parameter.NewDelta, ' Set value for carbon isotope discrimination of newly fixed carbon');
    LineOut (Parameter.phi, ' CO2 leakage from bundle-sheet cells (only relevant for C4 plants)');
    IntegerOut (Parameter.Phenology.nChanges, ' number of changes in plant phenology');
    LineOut (Parameter.Phenology.Threshold, ' Threshold temperature for counting heat sums');
    For i := 1 to Parameter.Phenology.nChanges do
        Begin
        If Parameter.Phenology.Units[i] = HeatSum then
           Writeln (defp, 'Heatsum   units used for controlling phenological events')
        Else if Parameter.Phenology.Units[i] = JulianDay then
           Writeln (defp, 'Julianday   units used for controlling phenological events')
        Else if Parameter.Phenology.Units[i] = DayLength then
           Writeln (defp, 'Daylength   units used for controlling phenological events')
        Else
           Writeln (defp, 'nDays   units used for controlling phenological events');
           IntegerOut (Parameter.Phenology.JulianDay[i], ' Julian day to change phenology');
           IntegerOut (Parameter.Phenology.nDays[i], ' number of days before changing phenology');
           LineOut (Parameter.Phenology.HeatSum[i], ' heat sum needed before changing phenology');
           LineOut (Parameter.Phenology.Daylength[i], ' daylength needed before changing phenology');
           LineOut (Parameter.Phenology.Senescence[i], ' foliage loss rate');
           LineOut (Parameter.Phenology.LeafGrowth[i], ' foliage growth rate (from buds)');
        End;
    close (defp);
End; {of Procedure 'SavePlant'}

Procedure SaveSite(name : string);
    Var Defp: text;
        i: integer;

    Procedure LineOut (Datum: real48; Comment: string);
       var Width, Digits: integer;
       Begin
       GetField(Datum, ParFileMaxWidth, Width, Digits);
       Writeln (Defp, Datum: ParFileMaxWidth: Digits, Comment);
       End;  {of procedure 'LineOut'}

    Begin
    assign (defp, Name);   rewrite (defp);
    Writeln(defp, Control.Version + ' Site parameters');
    LineOut(Parameter.SoilEvap, ' Scaling factor for soil evaporation');
    LineOut(Parameter.Atmos_N, ' Annual atmosperic input of nitrogen');
    LineOut(Parameter.Leaching, ' Fraction of N not taken up by plants that is leached');
    LineOut (Parameter.FineSoil, ' Fine soil fraction');
    Writeln (defp, SoilWat.SeparateSensitivity, ' Give a separate water stress sensitivity for different layers in the soil');
    Writeln (defp, SoilWat.nLayers:ParFileMaxWidth, ' Number of layers for soil water');
    For i := 1 to SoilWat.nLayers do
        Begin
        LineOut(SoilWat.Layer[i].Depth, ' Depth of ith layer');
        LineOut(SoilWat.Layer[i].Pores, ' Percentage pores of ith layer');
        LineOut(SoilWat.Layer[i].MaxWater, ' Maximum water (mm) in ith layer');
        LineOut(SoilOrganic.FineRootLitterIn[i], ' Relative decomposition activity');
        LineOut(SoilOrganic.CoarseRootLitterIn[i], ' Relative coarse-root litter addition');
        LineOut(SoilWat.Layer[i].RelEvap, ' Relative evaporation rate');
        If SoilWat.SeparateSensitivity then
           LineOut(SoilWat.Layer[i].StressSensitivity, ' Relative water stress sensitivity')
        Else
           LineOut(Divide(SoilWat.Layer[i].MaxWater, SoilWat.MaxWater), ' Relative water stress sensitivity');
        LineOut(SoilWat.Layer[i].ExtractEffort, ' Effort for water extraction from ith layer');
        End;
    LineOut (SoilWat.MaxWater, ' Maximum amount of water that can be held in the soil');
    LineOut (Parameter.MeanSoilTemp, ' Mean soil temperature');
    LineOut (Parameter.CO2conc, ' CO2 concentration');
    LineOut (Parameter.AtmosPressure, ' Atmospheric pressure');
    LineOut (Parameter.AnnualRain, ' Annual rainfall');
    LineOut (Parameter.RainProb, ' Daily rainfall probability');
    LineOut (Parameter.MeanTmax, ' Annual mean maximum temperature');
    LineOut (Parameter.MeanTmin, ' Annual mean minimum temperature');
    LineOut (Parameter.MeanRadn, ' Annual mean radiation');
    LineOut (Parameter.MeanAbsHum, ' Annual mean absolute humidity');
    LineOut (Parameter.Temp_Amplitude, ' Annual temperature amplitude');
    LineOut (Parameter.Radn_Amplitude, ' Annual radiation amplitude');
    LineOut (Parameter.Daily_Amplitude, ' Daily temperature amplitude');
    LineOut (Parameter.Humid_Amplitude, ' Annual amplitude in absolute humidity');
    LineOut (Parameter.RelWaterSens, ' Sensitivity of decomposition to water stress relative to that of plant processes');
    LineOut (Parameter.MinDecomp, ' Residual decomposition rate under extremely dry conditions');
    LineOut (Parameter.RELATIVECN, ' RATIO OF C/N RATIOS IN METABOLIC AND STRUCTURAL POOLS');
    LineOut (Parameter.CriticalCN, ' Critical C/N RATIO OF ACTIVE POOL');
    LineOut (Parameter.OMTransfer, ' Annual transfer of organic matter from one layer to the next lower layer');
    LineOut (Parameter.OMIncorporate, ' Annual incorporation of organic matter from the surface to first soil layer');
    LineOut (Parameter.DecayBranch_StructRatio, ' Decay constant fine woody litter (branches) relative to structural litter');
    LineOut (Parameter.DecayWood_StructRatio, ' Decay constant coarse woody litter (logs) relative to structural litter');
    LineOut (Parameter.Inert_Resistant_Ratio, ' Ratio of decay constants of inert and resistant pools');
    Writeln (defp, Parameter.WarmestDay:ParFileMaxWidth, ' Warmest day of the year (for simulated climate runs)');
    Writeln (defp, Parameter.MostPAR:ParFileMaxWidth, ' Day with highest solar radiation (for simulated climate runs)');
    LineOut (Parameter.Latitude, ' Latitude of the site');
    LineOut (Parameter.RateAdjust, ' Adjustment to soil organic matter decomposition rates');
    LineOut (Parameter.Immobilise, ' Fraction of inorganic nitrogen that is immobilised at each time step');
    LineOut (Parameter.ImmobiliseInSlow, ' Fraction of immobilised N that is immobilised in the slow pool');
    LineOut (Parameter.SnowMelt, ' mm of snow that melt per degree above zero');
    LineOut (Parameter.RadnMelt, ' mm of snow that melt per MJ m-2');
    LineOut (Parameter.SoilTResist, ' Soil resistance to temperature changes');
    LineOut (Parameter.SnowInsulate, ' Extra insulation due to snow layer: extra resistance per mm in snow pack');
    LineOut (Parameter.MaxTBoost, ' The proportional increase of soil temperature aboveaverage air temperature');
    LineOut (Parameter.TLAISensitivity, ' Dependence of increase in soil temperature on LAI');
    LineOut (Parameter.LigninInhibition, ' Lignin-inhibition parameter of organic matter decomposition');
    close (defp);
End; {of Procedure 'SaveSite'}

Procedure SavePools(name : string);
var fp : Text;
    ElementInfo, LayerInfo: String;
    Elements: ElementsUsed;
    iLayer: integer;

    Procedure LineOut (Datum: real48; Comment: string);
       var Width, Digits: integer;
       Begin
       GetField(Datum, ParFileMaxWidth, Width, Digits);
       Writeln (fp, Datum: ParFileMaxWidth: Digits, Comment);
       End;  {of procedure 'LineOut'}

    Procedure IntegerOut (Datum: Integer; Comment: string);
       Begin
       Writeln (fp, Datum:ParFileMaxWidth, Comment);
       End;  {of procedure 'IntegerOut'}

   Begin
   Assign(fp, name);
   ReWrite(fp);
   Writeln (fp, Control.Version + ' Pools');
   IntegerOut (SoilOrganic.nLayers, ' Number of layers for soil organic matter');
   for Elements := C13 to N do
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
   LineOut (Derived.WaterLimit, ' Water limitation term');
   LineOut (Derived.DecompLimit, ' Water limitation term for decomposition');
   LineOut (Derived.LeafGrowth, ' Seasonal capacity to convert foliage reserves into new foliage');
   LineOut (Derived.Deciduous, ' Seasonal foliage shedding term');
   LineOut (Derived.ExcessN, ' Nitrogen in excess of the ability of trees to absorb it');
   LineOut (Derived.RespnBase, ' Base rate for respiration rate (for temperature adjustments)');
   LineOut (Derived.HeatSum, ' Heat sum');
   LineOut (Weather.LastMin, ' Previous day minimum temperature');
   LineOut (Weather.LastAbsHum, ' Previous day absolute humidity');
   LineOut (Weather.LastRelHum, ' Previous day relative humidity');
   LineOut (Weather.TSoil, ' Soil temperature');
   IntegerOut (Control.NextPhenology, ' Sequence counter for phenology settings');
   IntegerOut (Control.PhenologyDayCount, ' Number of days for which the current phenological setting has been operative (if that is the critical element)');
   Close(fp);
End; {of Procedure 'SavePools'}

End.

{ --- end of file FILE_IO2.PAS ------------------------------------------ }
