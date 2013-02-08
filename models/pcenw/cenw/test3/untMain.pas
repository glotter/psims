{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmMain                                         =
  =                                                              =
  =             This handles the main menu and sets up           =
  =             most procedures and menu options                 =
  =             other than those that have their own units       =
  ================================================================
  = File      : untMain.PAS                                      =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untMain;

interface

Procedure SetInitialProjectDefaults;
Procedure SetInitialPlantDefaults;
Procedure SetInitialSiteDefaults;
Procedure AppStart;

implementation

{ uses
  untSiteParameters, untStandParameters, untPhotoSyntheticParameters,
  untAllocationParameters, untLoadSaveCenW, UntLoadSaveProject,
  untLoadSavePlant, untLoadSaveSite, untLoadSaveInitial, untWeedParameters,
  untDecompositionParameters, untWeatherParameters, untControlParameters,
  untPools, untDivideValidation, untSoilOrganicMatterDisplay, untPowerValidation,
  untSavePlantInfo, untSaveSoilInfo, untGenericSave, untSensSelect,
  untSaveWeatherInfo, untAbout, untIrrigationManagement, Chart,
  untHelpConsts, untDeclarations, untSimulate, untSimSoil, untFileIO,
  untDiskOut, untMiscellaneous, untRun, untFileReadWeather, untGraph,
  untAgreement, untBatchParameters, untEquilParameters, untEquilFinished,
  untProgress, untMultipleRunParameters, untEquilProgress, untInitialPools, untC13Pools,
  untGenericListDialogue, untSavePhosphorusInfo, untGenericDisplay,
  untSpatialParameters, untSpatialProgress, untRunOptions;
}

uses SysUtils, untDeclarations, untFileIO, untDivideValidation, untMiscellaneous, untRun, untLoadSaveCenW, untDefaults;

Procedure SetInitialProjectDefaults;
    {This routine sets newly introduced parameters to some default values.
    This is over-ridden once users select their own values.}
    Begin
    Event.nPloughing := 0;
    Event.nGrazings := 0;
    Control.IncludeP := false;
    Control.NoticeFlags := true;
    Control.IncludeWeeds := false;
    Control.Equil.OscillationCount := 3;
    Event.AdjustVP := true;
    Parameter.UseAllometrics := true;
    Derived.AnnualMeanTemp := 15;      // Annual mean temperature - will be calculated after one year and a previous value might be read in from the initial values file
    Derived.SpringMaxTemp := Derived.AnnualMeanTemp - 5; // Spring time mean max temperature - will be calculated after one year and a previous value might be read in from the initial values file
    Derived.SpringTempCounter := 0;    // Counter for calculating new spring time neam max temp
    End; {of Procedure 'SetInitialProjectDefaults'}

Procedure SetInitialPlantDefaults;
    {This routine sets newly introduced parameters to some default values.
    This is over-ridden once users select their own values.}
    Begin
    Parameter.Respnalpha := -3.18;
    Parameter.Respnbeta := 0.001696;
    Parameter.RespnOpt := 50;
    Parameter.RespnTAcclimation := true;
    Parameter.RespnAdjust := 10;
    Parameter.phi := 0.2;  // Bundle sheet leakage
    Parameter.NMVOC := 0;
    Parameter.alpha := 0.08; // quantum yield
    Parameter.Pcrit := 0.004;
    Parameter.Pmax := 0.02;
    Parameter.P0 := 0;
    Parameter.WoodDensity := 0;
    Parameter.WoodDensity0 := 0;
    Parameter.WoodDensity25 := 0;
    Parameter.WoodDensTemp := 0;
    Parameter.WoodDensFertility := 0;
    Parameter.WoodDensStocking := 0;
    Parameter.VariableHD := false;
    Parameter.HD_Const := 0.9;
    Parameter.HD_Temp := 0.025;
    Parameter.HD_Stocking := 0.00025;
    Parameter.HD_Fertil := -0.29;
    Parameter.HD_InitialSlope := 0.52;
    Parameter.HD_InitialInter := 0.6;
    Parameter.CanopyWidthInter := 0.7544;
    Parameter.CanopyWidthSlope := 0.2073;
    Parameter.WaterLogLimit := 1;
    Parameter.WaterLogSensitivity := 1;
    Parameter.RainDamageLimit := 100;
    Parameter.RainDamageSensitivity := 0;
    End; {of Procedure 'SetInitialPlantDefaults'}

Procedure SetInitialSiteDefaults;
    var iLayer: Integer;
    {This routine sets newly introduced parameters to some default values.
    This is over-ridden once users select their own values.}
    Begin
    Parameter.MaxTBoost := 0.0;
    Parameter.LigninInhibition := 3;
    Parameter.ImmobiliseInSlow := 0;
    Parameter.RelativeCP := 5;
    Parameter.CriticalCPmin := 30;
    Parameter.CriticalCPmax := 80;
    Parameter.Weathering := 0.0002 / 365;
    Parameter.OccludedP_rate := 0.002 / 365;
    Parameter.Labile_to_Sec := 0.1;
    Parameter.Sec_to_Labile := 5e-4;
    Parameter.Phosphatase := 0.005/ 365;
    Parameter.Atmos_P := 0;
    SoilWat.SeparateSensitivity := false;
    For iLayer := 1 to MaxSoilLayers do
        SoilWat.Layer[iLayer].StressSensitivity := 0;
    End; {of Procedure 'SetInitialSiteDefaults'}

Procedure AppInit0;
begin
    PlantDefaults;
    ProjectDefaults;
    PoolDefaults;
    SiteDefaults;
end;


Procedure AppInit1;
begin
   writeln('In AppInit1 - Control.ProjectFile');
   Control.ProjectHasChanged := FALSE;
   Control.PlantHasChanged := FALSE;
   Control.SiteHasChanged := FALSE;
   Control.InitHasChanged := False;
   Control.InitGenerated := False;
   Control.ScreenHasChanged := false;
   Control.AgreeChecked := false;
   Control.SensitivityTestOn := false;
   Control.OutputFileOpen := false;
   Control.BatchMode := false;
   Control.Run_On := False;
   Control.Legendon := false;
   Control.Range := false;
   Control.CountersSaved := false;
   ReadCenW ('CenWDef.df!', Control.ProjectFile);
end; { AppIinit1 }

Procedure AppInit2;
begin
If not Control.RunWithDefaults then
   Begin
   SetInitialProjectDefaults; // Set all newly introduced project parameters to some starting values
   SetInitialPlantDefaults; // Set all newly introduced plant parameters to some starting values
   SetInitialSiteDefaults; // Set all newly introduced site parameters to some starting values
   // does the project file exist?
   while not(FileExists(Control.ProjectFile)) do
      begin
      // no, so ask the user to find the real project file
         writeln('The project file "' + Control.ProjectFile +
                  '" does not exist.  Please select a valid project to load. HALTING IN CMD APP');
         Halt;
      End;
   // load the project file
   If not Control.RunWithDefaults then
       Begin
       GetParameterFile(Control.ProjectFile, EXT_PROJECT);
//       Control.ProjectDirectory := ExtractFilePath(Control.ProjectFile);
//       SetCurrentDir(ExtractFilePath(Control.ProjectFile));
       GetParameterFile(Control.PlantFile, EXT_PLANT);
       GetParameterFile(Control.SiteFile, EXT_SITE);
	  writeln('AppInit2: Control.PoolFile='+Control.PoolFile);
       GetParameterFile(Control.PoolFile, EXT_INITIAL);
       End;
   If Control.ResetPlantPools then
       ResetPlantPools;
   End;
{
IntroImage.Visible := true;
actIncludePDisplayExecute(Sender);
actIncludePSaveExecute(Sender);
actnIncludePhosphorusExecute(Sender);
actIncludeDeltaExecute(Sender);
actIncludeWeedParametersExecute(Sender);
}
End;

Procedure AppStart;
begin

AppInit0;
AppInit1;
AppInit2;

Control.TSoilFound := false;
Control.EquilMode := false;
Control.BatchMode := false;
  // start a normal cmd line app run
   Derived.ExcessN := 0;
   GetParameterFile(Control.PoolFile, EXT_INITIAL);
   If Control.ResetPlantPools then
      ResetPlantPools;
   If (Parameter.FertilityAdjust <> 1) then
   begin
      writeln('Caution: Sizes of the initial soil organic matter pools ' +
	      'are being adjusted at the start of this run.' + chr(10) +
	      'If you want to change that, ' +
	      'you can adjust the fertility parameter ' +
	      'in the "PARAMETERS".."DECOMPOSITION" menu.');
   end;
   { Start simulation }
   If (Plant.Stocking = 0) then
   begin
      writeln('Caution: Stand stocking is set to ZERO.' + chr(10) +
	      'You cannot run meaningful simulations ' +
	      'when there are no plants on the site.' + chr(10) +
	      'You can adjust the initial stocking rate ' +
	      'in the "INITIAL".."MISCELLANEOUS" menu.');
   end;
   AdjustFertility;
   Control.InitGenerated := true;
   ControlRun;
end; { AppStart }


{ START OF COMMENTED OUT PROCS

Procedure appLoadprojectClick;
Var FileIsThere, Dummy: Boolean;
begin
  appDlgOpenProject.FileName := Control.ProjectFile;
  if (appDlgOpenProject.Execute) then
    begin
    if Control.ProjectHasChanged then
       appSaveprojectClick;
    if Control.PlantHasChanged then
       appSavePlantFileClick;
    if Control.SiteHasChanged then
       appSaveSiteFileClick;
    Control.ProjectFile := AppDlgOpenProject.FileName;
    SetInitialProjectDefaults;
    FileIO.GetParameterFile(Control.ProjectFile, EXT_PROJECT);
    Control.ProjectHasChanged := false;
    Control.ProjectDirectory := ExtractFilePath(Control.ProjectFile);
    SetCurrentDir(Control.ProjectDirectory);
    SetInitialPlantDefaults;
    FileIO.GetParameterFile(Control.PlantFile, EXT_PLANT);
    Control.PlantHasChanged := false;
    SetInitialSiteDefaults;
    FileIO.GetParameterFile(Control.SiteFile, EXT_SITE);
    Control.SiteHasChanged := false;
    FileIO.IsFileThere(Control.ClimFile, FileIsThere);
    If not FileIsThere and (Control.ClimType = 'O') then
       Begin
       frmFileIO.SolveProblem(Control.ClimFile, Ext_Climate, Dummy, false);
       Control.ProjectHasChanged := true;
       End;
    end;
end;

Procedure appSaveprojectClick;
begin
appDlgSaveProject.FileName := Control.ProjectFile;
if (appDlgSaveProject.Execute) then
    begin
    Control.ProjectFile := appDlgSaveProject.FileName;
    SaveProject(Control.ProjectFile);
    Control.ProjectHasChanged := false;
    If Control.PlantHasChanged then
       Begin
       SavePlant(Control.PlantFile);
       Control.PlantHasChanged := false;
       End;
    If Control.SiteHasChanged then
       Begin
       SaveSite(Control.SiteFile);
       Control.SiteHasChanged := false;
       End;
    end;
end;

Procedure appLoadPlantfileClick;
Begin
dlgOpenPlant.FileName := Control.PlantFile;
if (dlgOpenPlant.Execute) then
    begin
    if Control.PlantHasChanged then
       frmMain.mnuSavePlantFileClick(Sender);
    Control.PlantFile := dlgOpenPlant.FileName;
    SetCurrentDir(Control.ProjectDirectory);
    SetInitialPlantDefaults;
    frmFileIO.GetParameterFile(Control.PlantFile, EXT_PLANT);
    Control.PlantHasChanged := false;
    Control.ProjectHasChanged := true;
    end;
end;

Procedure TfrmMain.mnuSavePlantfileClick(Sender: TObject);
begin
dlgSavePlant.FileName := Control.PlantFile;
if (dlgSavePlant.Execute) then
   Begin
   Control.PlantFile := dlgSavePlant.FileName;
   SavePlant(Control.PlantFile);
   Control.PlantHasChanged := false;
   End;
end;

Procedure TfrmMain.mnuLoadSitefileClick(Sender: TObject);
begin
dlgOpenSite.FileName := Control.SiteFile;
if (dlgOpenSite.Execute) then
    begin
    if Control.SiteHasChanged then
       frmMain.mnuSaveSiteFileClick(Sender);
    Control.SiteFile := dlgOpenSite.FileName;
    SetCurrentDir(Control.ProjectDirectory);
    SetInitialSiteDefaults;
    frmFileIO.GetParameterFile(Control.SiteFile, EXT_SITE);
    Control.SiteHasChanged := false;
    Control.ProjectHasChanged := true;
    end;
end;

Procedure TfrmMain.mnuSaveSitefileClick(Sender: TObject);
begin
dlgSaveSite.FileName := Control.SiteFile;
if (dlgSaveSite.Execute) then
   Begin
   Control.SiteFile := dlgSaveSite.FileName;
   SaveSite(Control.SiteFile);
   Control.SiteHasChanged := false;
   End;
end;

Procedure TfrmMain.mnuLoadInitialvaluesClick(Sender: TObject);
Begin
dlgOpenInitialValues.FileName := Control.PoolFile;
If (dlgOpenInitialValues.Execute) then
    Begin
    Control.PoolFile := dlgOpenInitialValues.FileName;
    SetCurrentDir(Control.ProjectDirectory);
    frmFileIO.GetParameterFile(Control.PoolFile, EXT_INITIAL);
    Control.InitHasChanged := false;
    Control.InitGenerated := true;
    End;
End;

Procedure TfrmMain.mnuSaveinitialvaluesClick(Sender: TObject);
var OldFile: FileNameType;
begin
if (Control.InitGenerated or
   (MessageDlg('No initial values have yet been loaded or generated.' + chr(10) +
                 'Do you want to save null values anyway?',
                 mtConfirmation, [mbYes, mbNo], 0) = mrYes)) then
     begin
     OldFile := Control.SavePoolFile;
     dlgSaveInitialValues.FileName := Control.SavePoolFile;
     if (dlgSaveInitialValues.Execute) then
       Begin
       Control.SavePoolFile := dlgSaveInitialValues.FileName;
       SavePools(Control.SavePoolFile);
       Control.InitHasChanged := false;
       If Control.SavePoolFile <> OldFile then
          Control.ProjectHasChanged := true;
       end;
  end;
end;

Procedure TfrmMain.mnuReadWeatherdataClick(Sender: TObject);
begin
frmFileReadWeather.ShowModal;
end;

Procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
// close the program
Close;
end;

Procedure TfrmMain.mnuSiteClick(Sender: TObject);
begin
frmSiteParameters.ShowModal;
end;

Procedure TfrmMain.mnuStandClick(Sender: TObject);
begin
frmStandParameters.ShowModal;
end;

Procedure TfrmMain.mnuPhotosynthetictermsClick(Sender: TObject);
begin
frmPhotoSyntheticParameters.ShowModal;
end;

Procedure TfrmMain.mnuPhenologytermsClick(Sender: TObject);
begin
Repeat
   SetUpPhenologyDialogue;
   frmGenericListDialogue.ShowModal;
   If List.HasChanged then
      GetPhenologyDialogueInfo;
   If List.ShowMessage then
      Begin
      ShowMessage(List.Message);
      frmMain.mnuPhenologyTermsClick(Sender);
      End;
Until not List.Redraw;
end;

Procedure TfrmMain.mnuSoilwaterClick(Sender: TObject);
begin
Repeat
   SetUpSoilWaterDialogue;
   frmGenericListDialogue.ShowModal;
   If List.HasChanged then
      GetSoilWaterDialogueInfo;
   If List.ShowMessage then
      Begin
      ShowMessage(List.Message);
      frmMain.mnuSoilWaterClick(Sender);
      End;
Until not List.Redraw;
end;

Procedure TfrmMain.mnuSoilLitterClick(Sender: TObject);
begin
Repeat
   SetUpLitterDialogue;
   frmGenericListDialogue.ShowModal;
   If List.HasChanged then
      GetLitterDialogueInfo;
Until not List.Redraw;
end;

Procedure TfrmMain.mnuAllocationClick(Sender: TObject);
begin
frmAllocationParameters.ShowModal;
end;

procedure TfrmMain.mnuWeedParametersClick(Sender: TObject);
begin
frmWeedParameters.ShowModal;
end;

Procedure TfrmMain.mnuDecompositionClick(Sender: TObject);
begin
frmDecompositionParameters.ShowModal;
end;

Procedure TfrmMain.mnuWeatherClick(Sender: TObject);
begin
frmWeatherParameters.ShowModal;
end;

Procedure TfrmMain.mnuControlClick(Sender: TObject);
begin
frmControlParameters.ShowModal;
actIncludePDisplayExecute(Sender);
actIncludePSaveExecute(Sender);
actnIncludePhosphorusExecute(Sender);
actIncludeDeltaExecute(Sender);
actIncludeWeedParametersExecute(Sender);
end;

Procedure TfrmMain.mnuCarbonPoolsClick(Sender: TObject);
begin
if (MessageDlg('Before editing start-up values,' + chr(10) +
               'do you want to load new initial values?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
   mnuLoadInitialvaluesClick(Nil);
frmPools.pgcPages.ActivePage := frmPools.tbsCarbon;
if (frmPools.ShowModal = mrOK) then
   begin
   if (MessageDlg('Initial values have been changed.' + chr(10) +
                  'Do you want to save them?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
       mnuSaveinitialvaluesClick(Nil);
   end;
end;

Procedure TfrmMain.NitrogenpoolsClick(Sender: TObject);
begin
if (MessageDlg('Before editing start-up values,' + chr(10) +
               'do you want to load new initial values?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
   mnuLoadInitialvaluesClick(Nil);
frmPools.pgcPages.ActivePage := frmPools.tbsNitrogen;
if (frmPools.ShowModal = mrOK) then
   if (MessageDlg('Initial values have been changed.' + chr(10) +
                  'Do you want to save them?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      mnuSaveinitialvaluesClick(Nil);
end;

Procedure TfrmMain.mnuPhosphorusPoolsClick(Sender: TObject);
begin
if (MessageDlg('Before editing start-up values,' + chr(10) +
               'do you want to load new initial values?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
   mnuLoadInitialvaluesClick(Nil);
frmPools.pgcPages.ActivePage := frmPools.tbsPhosphorus;
if (frmPools.ShowModal = mrOK) then
   if (MessageDlg('Initial values have been changed.' + chr(10) +
                  'Do you want to save them?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      mnuSaveinitialvaluesClick(Nil);
end;

Procedure TfrmMain.mnuMiscellaneousClick(Sender: TObject);
begin
if (MessageDlg('Before editing start-up values,' + chr(10) +
               'do you want to load new initial values?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    mnuLoadInitialvaluesClick(Nil);
frmPools.pgcPages.ActivePage := frmPools.tbsMisc;
if (frmPools.ShowModal = mrOK) then
    if (MessageDlg('Initial values have been changed.' + chr(10) +
                   'Do you want to save them?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
       mnuSaveinitialvaluesClick(Nil);
end;

Procedure TfrmMain.mnuC13PoolsClick(Sender: TObject);
begin
if (MessageDlg('Before editing start-up values,' + chr(10) +
               'do you want to load new initial values?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
   mnuLoadInitialvaluesClick(Nil);
if (frmC13Pools.ShowModal = mrOK) then
   if (MessageDlg('Initial values have been changed.' + chr(10) +
                  'Do you want to save them?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
       mnuSaveinitialvaluesClick(Nil);
end;

Procedure TfrmMain.mnuInitialPoolsClick(Sender: TObject);
begin
frmInitialPools.ShowModal;
end;

Procedure TfrmMain.mnuAddfertiliserClick(Sender: TObject);
begin
Repeat
    SetUpFertiliserDialogue;
    frmGenericListDialogue.ShowModal;
    If List.HasChanged then
       GetFertiliserDialogueInfo;
Until not List.Redraw;
end;

Procedure TfrmMain.mnuIrrigationClick(Sender: TObject);
begin
frmIrrigationManagement.ShowModal;
end;

Procedure TfrmMain.mnuEnvironmentClick(Sender: TObject);
begin
Repeat
   SetUpEnvironmentDialogue;
   frmGenericListDialogue.ShowModal;
   If List.HasChanged then
      GetEnvironmentDialogueInfo;
Until not List.Redraw;
end;

Procedure TfrmMain.mnuHarvestandthinningClick(Sender: TObject);
Begin
Repeat
   SetUpHarvestDialogue;
   frmGenericListDialogue.ShowModal;
   If List.HasChanged then
      GetHarvestDialogueInfo;
Until not List.Redraw;
End;

Procedure TfrmMain.mnuPestsClick(Sender: TObject);
begin
Repeat
   SetUpPestDialogue;
   frmGenericListDialogue.ShowModal;
   If List.HasChanged then
      GetPestDialogueInfo;
Until not List.Redraw;
end;

Procedure TfrmMain.mnuFireClick(Sender: TObject);
begin
SetUpFireDialogue;
frmGenericListDialogue.ShowModal;
If List.HasChanged then
   GetFireDialogueInfo;
end;

procedure TfrmMain.PloughingClick(Sender: TObject);
begin
SetUpPloughDialogue;
frmGenericListDialogue.ShowModal;
If List.HasChanged then
   GetPloughDialogueInfo;
end;

procedure TfrmMain.OMAdditionsClick(Sender: TObject);
begin
SetUpOMAdditionsDialogue;
frmGenericListDialogue.ShowModal;
If List.HasChanged then
   GetOMAdditionsDialogueInfo;
end;

Procedure TfrmMain.mnuGrazingClick(Sender: TObject);
begin
Repeat
    SetUpGrazingDialogue;
    frmGenericListDialogue.ShowModal;
    If List.HasChanged then
       GetGrazingDialogueInfo;
Until not List.Redraw;
end;

Procedure TfrmMain.mnuPlantcarbonClick(Sender: TObject);
begin
SetUpGenericDisplay(D_CarbonGain, D_TDamage, 'Display options for plant carbon');
end;

Procedure TfrmMain.mnuPlantNitrogenClick(Sender: TObject);
begin
SetUpGenericDisplay(D_NConc, D_NSum, 'Display options for plant nitrogen');
end;

Procedure TfrmMain.mnuPhosphorusClick(Sender: TObject);
begin
SetUpGenericDisplay(D_PConc, D_2ndaryP, 'Display options for phosphorus pools and fluxes');
end;

Procedure TfrmMain.mnuSoilorganicmatterClick(Sender: TObject);
begin
SetUpGenericDisplay(D_CLeafLitter, D_NLeached, 'Display options for soil organic matter pools and fluxes');
end;

Procedure TfrmMain.mnuWeatherdataClick(Sender: TObject);
begin
SetUpGenericDisplay(D_Tmax, D_Dummy, 'Display options for weather-related variables');
end;

Procedure TfrmMain.mnuSavePlantInfoClick(Sender: TObject);
begin
SetUpGenericSave(S_CH2O, S_NConc, false,'Select plant-related variables to save');
end;

Procedure TfrmMain.mnuSavesoilInfoClick(Sender: TObject);
begin
SetUpGenericSave(S_CMetabolic, S_SoilRespn, true,'Select soil-related variables to save');
end;

Procedure TfrmMain.mnuSavephosphorusInfoClick(Sender: TObject);
begin
SetUpGenericSave(S_SolubleP, S_TotalP, false,'Select phosphorus-related variables to save');
end;

Procedure TfrmMain.mnuSaveWeatherinfoClick(Sender: TObject);
begin
SetUpGenericSave(S_Tmax, S_Dummy, false,'Select weather-related variables to save');
end; { TfrmMain }

Procedure TfrmMain.mnuStartClick(Sender: TObject);
begin
Control.TSoilFound := false;
Control.EquilMode := false;
Control.BatchMode := false;
  // start a normal run
  Screen.Cursor := crHourglass;
  try
    If (Control.AgreeOK) then
      Begin
      Derived.ExcessN := 0;
      frmFileIO.GetParameterFile(Control.PoolFile, EXT_INITIAL);
      If Control.ResetPlantPools then
         ResetPlantPools;
      If (Parameter.FertilityAdjust <> 1) then
        begin
        MessageDlg('Caution: Sizes of the initial soil organic matter pools ' +
                   'are being adjusted at the start of this run.' + chr(10) +
                   'If you want to change that, ' +
                   'you can adjust the fertility parameter ' +
                   'in the "PARAMETERS".."DECOMPOSITION" menu.',
                   mtInformation, [mbOK], 0);
        end;
      { Start simulation }
      If (Plant.Stocking = 0) then
        begin
        MessageDlg('Caution: Stand stocking is set to ZERO.' + chr(10) +
                   'You cannot run meaningful simulations ' +
                   'when there are no plants on the site.' + chr(10) +
                   'You can adjust the initial stocking rate ' +
                   'in the "INITIAL".."MISCELLANEOUS" menu.',
                   mtInformation, [mbOK], 0);
        end;
      AdjustFertility;
      Control.InitGenerated := true;
      ControlRun;
    End Else {if not Control.AgreeOK}
    begin
      MessageDlg('CANNOT RUN SIMULATION' + chr(10) +
                 'You can only run simulations ' +
                 'after indicating your acceptance ' +
                 'of the licence agreement.' + chr(10) +
                 'To enable you to run the simulation routine, go to the ' +
                 'VIEW menu, choose LICENCE AGREEMENT and indicate ' +
                 'your acceptance of the licence agreement.',
                 mtError, [mbOK], 0);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

END OF COMMENTED OUT PROCS }


{ MORE COMMENTED OUT PROCS

Procedure TfrmMain.mnuMultipleRunsClick(Sender: TObject);
Var iRun, nRuns: Integer;
    FileIsThere, Dummy: Boolean;
    ProjectsUsed: MultipleRunsType;
    MultipleRunPoolFile: FileNameType;
Begin
Control.TSoilFound := false;
Control.EquilMode := false;
Control.BatchMode := false;
// start one of multiple runs
Screen.Cursor := crHourglass;
try
    If (Control.AgreeOK) then
      Begin
      frmMultipleRunParameters.ShowModal;
      If not Control.AbortRun then
        Begin
        ProjectsUsed := Control.MultipleRuns;
        ProjectsUsed[0] := Control.ProjectFile;
        MultipleRunPoolFile := Control.MultipleRunPoolFile;
        nRuns := Control.nProjects;
        If Control.SaveBeforeRun then
           Begin
           SaveProject(Control.ProjectFile);
           If Control.PlantHasChanged then
              SavePlant(Control.PlantFile);
           If Control.SiteHasChanged then
              SaveSite(Control.SiteFile);
           End;
        Control.ProjectHasChanged := false;
        Control.PlantHasChanged := false;
        Control.SiteHasChanged := false;
        Derived.ExcessN := 0;
        For iRun := 1 to nRuns do
           Begin
           frmFileIO.GetParameterFile(ProjectsUsed[iRun], EXT_PROJECT);
           Control.ProjectDirectory := ExtractFilePath(ProjectsUsed[iRun]);
           SetCurrentDir(Control.ProjectDirectory);
           frmFileIO.GetParameterFile(Control.PlantFile, EXT_PLANT);
           frmFileIO.GetParameterFile(Control.SiteFile, EXT_SITE);
           frmFileIO.IsFileThere(Control.ClimFile, FileIsThere);
           If not FileIsThere then
              frmFileIO.SolveProblem(Control.ClimFile, Ext_Climate, Dummy, false);
           If iRun = 1 then
              Begin
              frmFileIO.GetParameterFile(Control.PoolFile, EXT_INITIAL);
              AdjustFertility;
              If (Parameter.FertilityAdjust <> 1) then
                 MessageDlg('Caution: Sizes of the initial soil organic matter pools ' +
                   'are being adjusted at the start of this run.' + chr(10) +
                   'If you want to change that, ' +
                   'you can adjust the fertility parameter ' +
                   'in the "PARAMETERS".."DECOMPOSITION" menu.',
                   mtInformation, [mbOK], 0);
              End;
           If Control.ResetPlantPools then
              ResetPlantPools;
           { Start simulation }
           If (Plant.Stocking = 0) then
              MessageDlg('Caution: Stand stocking is set to ZERO.' + chr(10) +
                   'You cannot run meaningful simulations ' +
                   'when there are no plants on the site.' + chr(10) +
                   'You can adjust the initial stocking rate ' +
                   'in the "INITIAL".."MISCELLANEOUS" menu.',
                    mtInformation, [mbOK], 0);
           ControlRun;
           If iRun = (nRuns - 1) then
              SavePools(MultipleRunPoolFile);
           End;
        Control.ProjectFile := ProjectsUsed[0];
        frmFileIO.GetParameterFile(ProjectsUsed[0], EXT_PROJECT);
        Control.ProjectDirectory := ExtractFilePath(ProjectsUsed[0]);
        SetCurrentDir(Control.ProjectDirectory);
        frmFileIO.GetParameterFile(Control.PlantFile, EXT_PLANT);
        frmFileIO.GetParameterFile(Control.SiteFile, EXT_SITE);
        frmFileIO.IsFileThere(Control.ClimFile, FileIsThere);
        If not FileIsThere then
           frmFileIO.SolveProblem(Control.ClimFile, Ext_Climate, Dummy, false);
        End;
      End
    Else {if not Control.AgreeOK}
        MessageDlg('CANNOT RUN SIMULATION' + chr(10) +
                 'You can only run simulations ' +
                 'after indicating your acceptance ' +
                 'of the licence agreement.' + chr(10) +
                 'To enable you to run the simulation routine, go to the ' +
                 'VIEW menu, choose LICENCE AGREEMENT and indicate ' +
                 'your acceptance of the licence agreement.',
                 mtError, [mbOK], 0);
    finally
      Screen.Cursor := crDefault;
    end;
end;

Procedure TfrmMain.mnuBatchmodeClick(Sender: TObject);
var BatchFileOK: Boolean;
Begin
InitialBatch;
frmBatchParameters.ShowModal;
If Control.AgreeOK and Control.BatchMode then
      Begin
      frmFileIO.GetParameterFile(Control.BatchFile, EXT_BATCH);
      frmFileIO.GetParameterFile(Control.PoolFile, EXT_INITIAL);
      If Control.ResetPlantPools then
         ResetPlantPools;
      AdjustFertility;
      ControlRun;
      BatchVariableList ('S', BatchFileOK);
      BatchFileOK := true;
      While ((Control.BatchCount <= Control.BatchCalcs) or (Control.BatchCalcs < 0))
            and (not Control.EndBatch) and BatchFileOK and Control.BatchFileOpen do
          Begin
          BatchVariableList ('B', BatchFileOK);
          frmFileIO.GetParameterFile(Control.PoolFile, EXT_INITIAL);
          If Control.ResetPlantPools then
             ResetPlantPools;
          AdjustFertility;
          ControlRun;
          if (eof(Control.BatchName) or (Control.BatchCount = Control.BatchCalcs)) then
             Control.EndBatch := true;
          Control.BatchCount := Control.BatchCount + 1;
          if eof(Control.BatchName) or (Control.BatchCount = Control.BatchCalcs) then
             Control.EndBatch := true;
          End;
      Control.BatchMode := false;
      EndRun;
      BatchVariableList ('R', BatchFileOK);
      End
   Else if not Control.AgreeOK then
      MessageDlg('CANNOT RUN SIMULATION' + chr(10) +
                 'You can only run simulations ' +
                 'after indicating your acceptance ' +
                 'of the licence agreement.' + chr(10) +
                 'To enable you to run the simulation routine, go to the ' +
                 'VIEW menu, choose LICENCE AGREEMENT and indicate ' +
                 'your acceptance of the licence agreement.',
                 mtError, [mbOK], 0);
end;

procedure TfrmMain.mnuEquilibriumClick(Sender: TObject);
Const PRestrictInitial = 0.1;
      PRestrictIncrement = 1.02;
      PRestrictMaximum = 1;
      MaxOscillations = 100;

Type InitialDataType = Record
                       Year, Month, Day, Age: Integer;
                       Stocking, Height, dbh: Real48;
                       Sapwood, HeartWood, CoarseRoot, FineRoot, Leaves, Reserves,
                       Branches, Bark, Pollen, Fruit, Soluble: TElements;
                       Soilwater: SoilWaterType;
                       Inert, RockP: SoilElements;
                       End;

      OscillationType = Record
                        PSum, NSum, Slow, Resistant: array[0..MaxOscillations, 1..2] of Real48;
                        Counter: Integer;
                        End;

var PAdjust, SomPoolAdjustment, LastConverg1, SOMSum, SearchRatio, PSearchRatio, OldBiolNFix, OldNLoss,
    AllN, AllP, OldAllN, OldAllP, OldPInput: Real48;
    iLayer, nLayers: Integer;
    GetOut, Abort: Boolean;
    E, LastE: ElementsUsed;
    Direction: Char;
    LastStruct, LastFineWOod, LastCoarseWood, LastActive, LastSlow, LastResistant,
    StartStruct, StartFineWOod, StartCoarseWood, StartActive, StartSlow, StartResistant,
    SignStruct, SignFineWOod, SignCoarseWood, SignActive, SignSlow, SignResistant: SoilElements;
    FlipStruct, FlipFineWOod, FlipCoarseWood, FlipActive, FlipSlow, FlipResistant: Array[0..MaxSoilLAyers, C..P] of Boolean;
    InitialData: InitialDataType;
    Oscillate: OscillationType;

    Procedure AdjustSOMPools (var OldEndPool, NewEndPool: Real48; var NewStartPool: Real48; Flip: Boolean; Speed: Real48);
    Begin
    If (OldEndPool > 0) and (NewEndPool > 0) then
       Begin
       If Flip <> true then
          Begin
          NewStartPool := NewStartPool * Divide(NewEndPool, OldEndPool) * Power(SearchRatio, Derived.Equil.Delta * Speed);
//          OldEndPool := OldEndPool * Divide(NewEndPool, OldEndPool) * Power(SearchRatio, Derived.Equil.Delta * Speed);
          NewEndPool := NewEndPool * Divide(NewEndPool, OldEndPool) * Power(SearchRatio, Derived.Equil.Delta * Speed);
          End
       Else
          Begin
          NewStartPool := Divide (NewStartPool, Power((NewEndPool / OldEndPool), 0.5));
//          OldEndPool := Divide (OldEndPool, Power((NewEndPool / OldEndPool), 0.5));
          NewEndPool := Divide (NewEndPool, Power((NewEndPool / OldEndPool), 0.5));
          End;
       End;
    End; {of Procedure 'AdjustSOMPools'}

    Procedure AdjustSOMbyN (OldN, NewN: Real48; var NewStartPool: Real48; Flip: Boolean; Speed: Real48);
    Const ChangeLimit = 0.1;
    var NewPool, OldPool, ChangePool: Real48;
    Begin
    OldPool := OldN; NewPool := NewN; ChangePool := NewStartPool;
    if abs(NewPool / OldPool - 1) > ChangeLimit then
       Begin
       if NewPool > OldPool then
          NewPool := OldPool * (1 + ChangeLimit)
       Else // if NewN < OldN then
          NewPool := OldPool * (1 - ChangeLimit);
       End;
    AdjustSOMPools (OldPool, NewPool, ChangePool, false, Speed);
    NewStartPool := ChangePool;
    End; {of Procedure 'AdjustSOMbyN'}

   Procedure AdjustPPools (var LastPool, NewPool: Real48; Delta: Real48; Direction: Char);
    Begin
    NewPool := NewPool * Delta;
    LastPool := LastPool * Delta;
    End; {of Procedure 'AdjustPPools'}

    Procedure SignFlip(NewPool, OldPool: Real48; var Flip: Boolean; var Sign: Real48);
    Begin
    If ((NewPool > OldPool) and (Sign = 1) or (NewPool < OldPool) and (Sign = -1)) then
       Flip := false
    Else
       Flip := true;
    If NewPool > OldPool then
       Sign := 1
    Else
       Sign := -1;
    End; {of Procedure 'SignFlip'}

    Procedure CheckLimits(var Pool: Real48; LowerLimit, UpperLimit: Real48);
    Begin
    If Pool < LowerLimit then
       Pool := LowerLimit
    Else if Pool > UpperLimit then
       Pool := UpperLimit;
    End; {of Procedure 'CheckLimit'}

    Procedure SetInertToZero (nLayer: Integer);
    var iLayer: Integer;
        E: ElementsUsed;
    Begin
    For E := C to P do
        For iLayer := 0 to nLayers do
            SoilOrganic.Inert[iLayer, E] := 0;
    if Control.IncludeP then
        For iLayer := 0 to nLayers do
            SoilOrganic.RockP[iLayer, P] := 0;
    End; {of Procedure 'SetInertToZero'}

    Procedure OscillationCheck;
        Procedure DataAccumulation;
        var Sum: Real48;
            iLayer: Integer;
        Begin
        if Oscillate.Counter = 0 then // No previous data
           Begin
           Oscillate.NSum[Oscillate.Counter, 1] := TotalN;
           Sum := 0;
           for iLayer := 0 to SoilOrganic.nLayers do
               Sum := Sum + SoilOrganic.Slow[iLayer, C];
           Oscillate.Slow[Oscillate.Counter, 1] := Sum;
           Sum := 0;
           for iLayer := 0 to SoilOrganic.nLayers do
               Sum := Sum + SoilOrganic.Resistant[iLayer, C];
           Oscillate.Resistant[Oscillate.Counter, 1] := Sum;
           if Control.IncludeP then
              Oscillate.PSum[Oscillate.Counter, 1] := TotalP;
           End
        Else
           Begin
           Oscillate.NSum[Oscillate.Counter, 2] := Oscillate.NSum[Oscillate.Counter - 1, 1] - TotalN;
           Oscillate.NSum[Oscillate.Counter, 1] := TotalN;
           Sum := 0;
           for iLayer := 0 to SoilOrganic.nLayers do
               Sum := Sum + SoilOrganic.Slow[iLayer, C];
           Oscillate.Slow[Oscillate.Counter, 2] := Oscillate.Slow[Oscillate.Counter - 1, 1] - Sum;
           Oscillate.Slow[Oscillate.Counter, 1] := Sum;
           Sum := 0;
           for iLayer := 0 to SoilOrganic.nLayers do
               Sum := Sum + SoilOrganic.Resistant[iLayer, C];
           Oscillate.Resistant[Oscillate.Counter, 2] := Oscillate.Resistant[Oscillate.Counter - 1, 1] - Sum;
           Oscillate.Resistant[Oscillate.Counter, 1] := Sum;
           if Control.IncludeP then
              Begin
              Oscillate.PSum[Oscillate.Counter, 2] := Oscillate.PSum[Oscillate.Counter - 1, 1] - TotalP;
              Oscillate.PSum[Oscillate.Counter, 1] := TotalP;
              End;
           End;
        End; {of Procedure 'DataAccumulation'}

        Function OscillationDetected: Boolean;
        var OscillateSlow, OscillateResistant, OscillateNSum, OscillatePSum: Boolean;
            iData: Integer;
        Begin
        OscillateNSum := true;
        OscillateSlow := true;
        OscillateResistant := true;
        OscillatePSum := true;
        for iData := 2 to Control.Equil.OscillationCount do
            Begin
            if (abs(Oscillate.NSum[iData, 2]) < abs(Oscillate.NSum[iData - 1, 2])) or
               ((Oscillate.NSum[iData, 2] * Oscillate.NSum[iData - 1, 2]) > 0) then
               OscillateNSum := false;
            if (abs(Oscillate.Slow[iData, 2]) < abs(Oscillate.Slow[iData - 1, 2])) or
               ((Oscillate.Slow[iData, 2] * Oscillate.Slow[iData - 1, 2]) > 0) then
               OscillateSlow := false;
            if (abs(Oscillate.Resistant[iData, 2]) < abs(Oscillate.Resistant[iData - 1, 2])) or
               ((Oscillate.Resistant[iData, 2] * Oscillate.Resistant[iData - 1, 2]) > 0) then
               OscillateResistant := false;
            if (abs(Oscillate.PSum[iData, 2]) < abs(Oscillate.PSum[iData - 1, 2])) or
               ((Oscillate.PSum[iData, 2] * Oscillate.PSum[iData - 1, 2]) > 0) then
               OscillatePSum := false;
            End;
        if Control.IncludeP then
           OscillationDetected := OscillateNSum or OscillateSlow or OscillateResistant or OscillatePSum
        Else
           OscillationDetected := OscillateNSum or OscillateSlow or OscillateResistant;
        End; {of Function 'OscillationDetected'}

        Procedure SetConservative;
        var E: ElementsUsed;
            iLayer: Integer;
        Begin
        Derived.Equil.Delta := Control.Equil.DeltaMin;
        Derived.Equil.PDelta := Control.Equil.DeltaMin;
        Derived.Equil.GoodCount := 0;
        Oscillate.Counter := 0;
        Derived.Equil.PRestrict := PRestrictInitial;
        For E := C to LastE do
            For iLayer := 0 to SoilOrganic.nLayers do
                Begin  // while this is not necessarily correct it is a conservative setting
                FlipCoarseWood[iLayer, E] := true;
                FlipFineWood[iLayer, E] := true;
                FlipStruct[iLayer, E] := true;
                FlipActive[iLayer, E] := true;
                FlipSlow[iLayer, E] := true;
                FlipResistant[iLayer, E] := true;
                End;
        End; {of Procedure 'SetConservative'}

        Procedure ShuffleData;
        var iData, iItem: Integer;
        Begin
        for iData := 1 to Control.Equil.OscillationCount - 1 do
            for iItem := 1 to 2 do
                Begin
                Oscillate.NSum[iData, iItem] := Oscillate.NSum[iData + 1, iItem];
                Oscillate.PSum[iData, iItem] := Oscillate.PSum[iData + 1, iItem];
                Oscillate.Slow[iData, iItem] := Oscillate.Slow[iData + 1, iItem];
                Oscillate.Resistant[iData, iItem] := Oscillate.Resistant[iData + 1, iItem];
                End;
        Oscillate.Counter := Control.Equil.OscillationCount;
        End; {of Procedure 'ShuffleData'}

    Begin
    Oscillate.Counter := Oscillate.Counter + 1;
    If Oscillate.Counter <= Control.Equil.OscillationCount then // still in the data accumulation phase
       DataAccumulation
    Else // enough observations - see what it looks like
       Begin
       if OscillationDetected then
          Begin
          Derived.Equil.Oscillating := true;
          SetConservative;
          End
       Else
          Derived.Equil.Oscillating := false;
       ShuffleData;
       DataAccumulation;
       End;

    End; {of Procedure 'OscillationCheck'}

Begin
Control.TSoilFound := false;
Control.BatchMode := false;
Control.EquilMode := true;
Control.EndEquil := false;
OldScreen[D_Equil].First := true;
If Control.AgreeOK and Control.EquilMode then
   Begin
   If frmEquilParameters.ShowModal = mrOK then
      Abort := false
   Else
      Abort := true;
   GetOut := false;
   frmFileIO.GetParameterFile(Control.PoolFile, EXT_INITIAL); // start sites with initial pools file
   If Control.ResetPlantPools then
      ResetPlantPools;
   InitialData.Year := Control.TotalYears;
   InitialData.Month := Control.ExtraMonths;
   InitialData.Day := Control.ExtraDays;
   InitialData.Stocking := Plant.Stocking;
   InitialData.Height := Plant.Height;
   InitialData.dbh := Plant.dbh;
   InitialData.Age := Plant.Age;
   InitialData.Sapwood := Plant.Sapwood;
   InitialData.HeartWood := Plant.Heartwood;
   InitialData.CoarseRoot := Plant.CoarseRoot;
   InitialData.FineRoot := Plant.FineRoot;
   InitialData.Leaves := Plant.Leaves;
   InitialData.Reserves := Plant.Reserves;
   InitialData.Branches := Plant.Branches;
   InitialData.Bark := Plant.Bark;
   InitialData.Pollen := Plant.Pollen;
   InitialData.Fruit := Plant.Fruit;
   InitialData.Soluble := Plant.Soluble;
   InitialData.SoilWater := SoilWat;
   InitialData.Inert := SoilOrganic.Inert;
   InitialData.RockP := SoilOrganic.RockP;
   OldBiolNFix := Parameter.BiolFix;
   OldPInput := Parameter.Atmos_P;
   OldNLoss := Parameter.NLoss;
   Derived.Equil.GoodCount := 0;
   Oscillate.Counter := -1;
   If Control.AllOneLayer then
      nLayers := 1
   Else
      nLayers := SoilOrganic.nLayers;
   SetInertToZero (nLayers); // For the purpose of finding 'equilibrium' SOM pools,
                             // set Inert C to zero as thsoe pools are not part of the dynamic equilibrium system
                             // If Pinclude, set Rock P to zero as well
   If Control.Equil.DeltaMin < 0.01 then
      Control.Equil.DeltaMin := 0.01;
   If Control.Equil.DeltaMax < Control.Equil.DeltaMin then
      Control.Equil.DeltaMax := Control.Equil.DeltaMin
   Else if Control.Equil.DeltaMax > 10 then
      Control.Equil.DeltaMax := 10;
   If Control.Equil.MaxChangeRatio < 1.01 then
      Control.Equil.MaxChangeRatio := 1.01
   Else if Control.Equil.MaxChangeRatio > 5 then
      Control.Equil.MaxChangeRatio := 5;
   Derived.Equil.ManualAdjust := false;
   Derived.Equil.Delta := Control.Equil.DeltaMin;
   Derived.Equil.PDelta := Control.Equil.DeltaMin;
   Derived.Equil.PRestrict := PRestrictInitial;
   Derived.Equil.Iterations := 0;
   LastConverg1 := 0;
   CheckLimits(Control.Equil.BoostResistant, 0.1, 100);
   AllN := TotalN;
   OldAllN := AllN;
   if Control.IncludeP then
      Begin
      Derived.Equil.PRestrict := PRestrictInitial;
      LastE := P;
      SOMPoolAdjustment := 0.25;
      AllP := TotalP;
      OldAllP := AllP;
      Control.Equil.HoldPInput := false;
      End
   Else
      Begin
      LastE := N;
      SOMPoolAdjustment := 1;
      End;
   For E := C to LastE do
     For iLayer := 0 to nLayers do
       Begin
       CheckLimits(SoilOrganic.Slow[iLayer, E], 0 , 1e6);
       CheckLimits(SoilOrganic.Resistant[iLayer, E], 0 , 1e6);
       CheckLimits(SoilOrganic.Active[iLayer, E], 0 , 1e6);
       CheckLimits(SoilOrganic.Struct[iLayer, E], 0 , 1e6);
       CheckLimits(SoilOrganic.Metab[iLayer, E], 0 , 1e6);
       CheckLimits(SoilOrganic.FineWood[iLayer, E], 0 , 1e6);
       CheckLimits(SoilOrganic.CoarseWood[iLayer, E], 0 , 1e6);
       end;
   StartStruct := SoilOrganic.Struct;
   StartFineWood := SoilOrganic.FineWood;
   StartCoarseWood := SoilOrganic.CoarseWood;
   StartActive := SoilOrganic.Active;
   StartSlow := SoilOrganic.Slow;
   StartResistant := Soilorganic.Resistant;
   // Start of Sarch Loop
   If (Control.Equil.MaxIterations > 0) and (not Abort) then
      Repeat
        Control.TotalYears := InitialData.Year;
        Control.ExtraMonths := InitialData.Month;
        Control.ExtraDays := InitialData.Day;
        Plant.Stocking := InitialData.Stocking;
        Plant.Height := InitialData.Height;
        Plant.dbh := InitialData.dbh;
        Plant.Age := InitialData.Age;
        If Control.Equil.SamePlantPools then
           Begin
           Plant.Sapwood := InitialData.Sapwood;
           Plant.Heartwood := InitialData.HeartWood;
           Plant.CoarseRoot := InitialData.CoarseRoot;
           Plant.FineRoot := InitialData.FineRoot;
           Plant.Leaves := InitialData.Leaves;
           Plant.Reserves := InitialData.Reserves;
           Plant.Branches := InitialData.Branches;
           Plant.Bark := InitialData.Bark;
           Plant.Pollen := InitialData.Pollen;
           Plant.Fruit := InitialData.Fruit;
           Plant.Soluble := InitialData.Soluble;
           SoilWat := InitialData.SoilWater;
           End;
        LastStruct := SoilOrganic.Struct;
        LastFineWood := SoilOrganic.FineWood;
        LastCoarseWood := SoilOrganic.CoarseWood;
        LastActive := SoilOrganic.Active;
        LastSlow := SoilOrganic.Slow;
        LastResistant := Soilorganic.Resistant;
{        SoilOrganic.Struct := StartStruct;
        SoilOrganic.FineWood := StartFineWood;
        SoilOrganic.CoarseWood := StartCoarseWood;
        SoilOrganic.Active := StartActive;
        SoilOrganic.Slow := StartSlow;
        Soilorganic.Resistant := StartResistant;}
        OldAllN := TotalN;
        ControlRun;
        IF (Control.ClimType = 'S') or
           ((Control.ClimType = 'O') and not WeatherFile[W_Rain]) THEN
           RandSeed := 0;
        If Control.Equil.EquilTarget = SOM then
           Begin
           SOMSum := 0;
           For iLayer := 0 to nLayers do
               SOMSum := SOMSum + SoilOrganic.Slow[iLayer, C] + SoilOrganic.Resistant[iLayer, C] +
                         SoilOrganic.Active[iLayer, C];
           Derived.Equil.SearchValue := 0.001 * SOMSum;
           End
        Else if Control.Equil.EquilTarget = LeafNConc then
           Derived.Equil.SearchValue := 1000 * Divide (Plant.Leaves[N], Plant.Leaves[C] * Control.CConversion)
        Else if Control.Equil.EquilTarget = LeafNitrogen then
           Derived.Equil.SearchValue := Plant.Leaves[N]
        Else if Control.Equil.EquilTarget = Leafmass then
           Derived.Equil.SearchValue := 0.001 * Plant.Leaves[C] * Control.CConversion
        Else { if Control.Equil.EquilTarget = WoodMass then}
           Derived.Equil.SearchValue := 0.001 * (Plant.SapWood[C] + Plant.HeartWood[C]) * Control.CConversion;
        SearchRatio := Divide (Control.Equil.TargetValue, Derived.Equil.SearchValue);
        If SearchRatio > 10 then SearchRatio := 10; // safeguard if we are way off the target
        Derived.Equil.Converg1 := Divide((Derived.Equil.SearchValue - Control.Equil.TargetValue), Control.Equil.TargetValue);
        Derived.Equil.Converg2 := 0;
        Derived.Equil.Converg3 := 0;
        For iLayer := 0 to nLayers do
            Begin
            Derived.Equil.Converg2 := Derived.Equil.Converg2 + abs((SoilOrganic.Slow[iLayer, N] - LastSlow[iLayer, N]) / SoilOrganic.Slow[iLayer, N]);
            Derived.Equil.Converg3 := Derived.Equil.Converg3 + abs((SoilOrganic.Resistant[iLayer, N] - LastResistant[iLayer, N]) / SoilOrganic.Resistant[iLayer, N]);
            End;
        Derived.Equil.Converg2 := Derived.Equil.Converg2 / (nLayers + 1);
        Derived.Equil.Converg3 := Derived.Equil.Converg3 / (nLayers + 1);
        If (Derived.Equil.Converg1 * LastConverg1) > 0 then //We keep on having to change in the same direction
           Begin
           Derived.Equil.Delta := Derived.Equil.Delta * (1 + Control.Equil.DeltaAdjust / 100);
           If Derived.Equil.Delta > Control.Equil.DeltaMax then
              Derived.Equil.Delta := Control.Equil.DeltaMax;
           End
        Else // We have just passed the target value
           Derived.Equil.Delta := Control.Equil.DeltaMin;
        Derived.Equil.ConvergP := Divide((Derived.PLimit - Control.Equil.TargetPLimit), Control.Equil.TargetValue);
        OscillationCheck;
        if Control.IncludeP then
           Begin
           if Derived.PLimit > Control.Equil.TargetPlimit then
              Begin
              If Direction = 'D' then {Above target and last iteration above target, too}
                 Derived.Equil.PDelta := Derived.Equil.PDelta * (1 + Control.Equil.DeltaAdjust / 100)
              Else {if Direction = 'U' then // Above target but last iteration it was below}
                 Begin
                 Derived.Equil.PDelta := Control.Equil.DeltaMin;
                 Derived.Equil.PRestrict := PRestrictInitial;
                 End;
              Direction := 'D';
              End
           Else {if Derived.PLimit > Control.Equil.TargetPlimit then}
              Begin
              if Direction = 'U' then {Below target as last iteration was below as well}
                 Derived.Equil.PDelta := Derived.Equil.PDelta * (1 + Control.Equil.DeltaAdjust / 100)
              Else {if Direction = 'D' then // Below target but last was above}
                 Begin
                 Derived.Equil.PDelta := Control.Equil.DeltaMin;
                 Derived.Equil.PRestrict := PRestrictInitial;
                 End;
              Direction := 'U';
              End;
           If Derived.Equil.PDelta > Control.Equil.DeltaMax then
              Begin
              Derived.Equil.PDelta := Control.Equil.DeltaMax;
              Derived.Equil.PRestrict := Derived.Equil.PRestrict * PRestrictIncrement;
              if Derived.Equil.PRestrict > PRestrictMaximum then
                 Derived.Equil.PRestrict := PRestrictMaximum;
              End;
           PSearchRatio := Divide (Control.Equil.TargetPlimit, Derived.Plimit);
           PAdjust := power(PSearchRatio, Derived.Equil.PRestrict * Derived.Equil.PDelta);
           if Control.Equil.AdjustPInput then
              Begin
              if not Control.Equil.HoldPINput then
                 Parameter.Atmos_P := Parameter.Atmos_P * PAdjust;
              End
           Else
              for iLayer := 0 to nLayers do
                  Begin
                  AdjustPPools (LastCoarseWood[iLayer, P], SoilOrganic.CoarseWood[iLayer, P], PAdjust, Direction);
                  AdjustPPools (LastFineWood[iLayer, P], SoilOrganic.FineWood[iLayer, P], PAdjust, Direction);
                  AdjustPPools (LastStruct[iLayer, P], SoilOrganic.Struct[iLayer, P], PAdjust, Direction);
                  AdjustPPools (LastActive[iLayer, P], SoilOrganic.Active[iLayer, P], PAdjust, Direction);
                  AdjustPPools (LastSlow[iLayer, P], SoilOrganic.Slow[iLayer, P], PAdjust, Direction);
                  AdjustPPools (LastResistant[iLayer, P], SoilOrganic.Resistant[iLayer, P], PAdjust, Direction);
                  End;
           End;
        if (abs(Derived.Equil.Converg1) <= Control.Equil.Criterion1) and
           (abs(Derived.Equil.converg2) <= Control.Equil.Criterion2) and
           (abs(Derived.Equil.converg3) <= Control.Equil.Criterion3) and
           (Not Control.IncludeP or (abs(Derived.Equil.ConvergP) <= Control.Equil.Criterion1)) and
           (Derived.Equil.Iterations > 0) then
           Begin
           Derived.Equil.GoodCount := Derived.Equil.GoodCount + 1;
           If Derived.Equil.GoodCount >= Control.Equil.MaxGoodCount then
              GetOut:=true; {if difference is acceptable then loop ends}
           End
        Else
          Derived.Equil.GoodCount := 0;
        If Control.Equil.EquilParameter = BiolNFix then
           Parameter.BiolFix := Parameter.BiolFix * power(SearchRatio, Derived.Equil.Delta)
        Else //if Control.Equil.EquilParameter = NFraction then
           parameter.NLoss := Parameter.NLoss / power(SearchRatio, Derived.Equil.Delta);
        AllN := TotalN;
        If Derived.Equil.ManualAdjust then
           Begin
           For E := C to LastE do
             For iLayer := 0 to nLayers do
                Begin
                LastCoarseWood[iLayer, E] := SoilOrganic.CoarseWood[iLayer, E];
                LastFineWood[iLayer, E] := SoilOrganic.FineWood[iLayer, E];
                LastStruct[iLayer, E] := SoilOrganic.Struct[iLayer, E];
                LastActive[iLayer, E] := SoilOrganic.Active[iLayer, E];
                LastSlow[iLayer, E] := SoilOrganic.Slow[iLayer, E];
                LastResistant[iLayer, E] := SoilOrganic.Resistant[iLayer, E];
                End;
           Derived.Equil.ManualAdjust := false;
           End
        Else
          For E := C to LastE do
             For iLayer := 0 to nLayers do
                Begin
                SignFlip(SoilOrganic.CoarseWood[iLayer, E], LastCoarseWood[iLayer, E], FlipCoarseWood[iLayer, E], SignCoarseWood[iLayer, E]);
                SignFlip(SoilOrganic.FineWood[iLayer, E], LastFineWood[iLayer, E], FlipFineWood[iLayer, E], SignFineWood[iLayer, E]);
                SignFlip(SoilOrganic.Struct[iLayer, E], LastStruct[iLayer, E], FlipStruct[iLayer, E], SignStruct[iLayer, E]);
                SignFlip(SoilOrganic.Active[iLayer, E], LastActive[iLayer, E], FlipActive[iLayer, E], SignActive[iLayer, E]);
                SignFlip(SoilOrganic.Slow[iLayer, E], LastSlow[iLayer, E], FlipSlow[iLayer, E], SignSlow[iLayer, E]);
                SignFlip(SoilOrganic.Resistant[iLayer, E], LastResistant[iLayer, E], FlipResistant[iLayer, E], SignResistant[iLayer, E]);
                if Derived.Equil.Iterations <> 0 then
                   Begin
                   AdjustSOMPools (LastCoarseWood[iLayer, E], SoilOrganic.CoarseWood[iLayer, E], StartCoarseWood[iLayer, E] {StartSOM was an attempt to separately record starting SOM}, FlipCoarseWood[iLayer, E], SOMPoolAdjustment);
                   AdjustSOMPools (LastFineWood[iLayer, E], SoilOrganic.FineWood[iLayer, E], StartFineWood[iLayer, E], FlipFineWood[iLayer, E], SOMPoolAdjustment);
                   AdjustSOMPools (LastStruct[iLayer, E], SoilOrganic.Struct[iLayer, E], StartStruct[iLayer, E], FlipStruct[iLayer, E], SOMPoolAdjustment);
                   AdjustSOMPools (LastActive[iLayer, E], SoilOrganic.Active[iLayer, E], StartActive[iLayer, E], FlipActive[iLayer, E], SOMPoolAdjustment);
                   AdjustSOMPools (LastSlow[iLayer, E], SoilOrganic.Slow[iLayer, E], StartSlow[iLayer, E], FlipSlow[iLayer, E], SOMPoolAdjustment);
                   AdjustSOMPools (LastResistant[iLayer, E], SoilOrganic.Resistant[iLayer, E], StartResistant[iLayer, E], FlipResistant[iLayer, E], SOMPoolAdjustment * Control.Equil.BoostResistant);
{An attempt to have a more sophisticated search routine where the SOM pools at start and end of the run are
 judged separately. But wasn't working right. So, I have gone back to the previous version
                   AdjustSOMbyN (OldAllN, AllN, StartCoarseWood[iLayer, E], FlipCoarseWood[iLayer, E], SOMPoolAdjustment);
                   AdjustSOMbyN (OldAllN, AllN, StartFineWood[iLayer, E], FlipFineWood[iLayer, E], SOMPoolAdjustment);
                   AdjustSOMbyN (OldAllN, AllN, StartStruct[iLayer, E], FlipStruct[iLayer, E], SOMPoolAdjustment);
                   AdjustSOMbyN (OldAllN, AllN, StartActive[iLayer, E], FlipActive[iLayer, E], SOMPoolAdjustment);
                   AdjustSOMbyN (OldAllN, AllN, StartSlow[iLayer, E], FlipSlow[iLayer, E], SOMPoolAdjustment);
                   AdjustSOMbyN (OldAllN, AllN, StartResistant[iLayer, E], FlipResistant[iLayer, E], SOMPoolAdjustment * Control.Equil.BoostResistant);}
                   End;
                End;
        If Control.Equil.EquilParameter = BiolNFix then
           Begin
           If Parameter.BiolFix < 1e-8 then
              Parameter.BiolFix := 1e-8
           Else if Parameter.BiolFix > 0.1 then
              Parameter.BiolFix := 0.1;
           End
        Else //if Control.Equil.EquilParameter = NFraction then
           Begin
           If Parameter.NLoss < 1e-5 then
              Parameter.NLoss := 1e-5
           Else if Parameter.NLoss > 0.5 then
              Parameter.NLoss := 0.5;
           End;
        For E := C to LastE do
            For iLayer := 0 to nLayers do
                Begin
                CheckLimits(SoilOrganic.Slow[iLayer, E], 0 , 1e6);
                CheckLimits(SoilOrganic.Resistant[iLayer, E], 0 , 1e6);
                CheckLimits(SoilOrganic.Active[iLayer, E], 0 , 1e6);
                CheckLimits(SoilOrganic.Struct[iLayer, E], 0 , 1e6);
                CheckLimits(SoilOrganic.Metab[iLayer, E], 0 , 1e6);
                CheckLimits(SoilOrganic.FineWood[iLayer, E], 0 , 1e6);
                CheckLimits(SoilOrganic.CoarseWood[iLayer, E], 0 , 1e6);
                End;
        LastConverg1 := Derived.Equil.Converg1;
        Derived.Equil.Iterations := Derived.Equil.Iterations + 1;
        frmEquilProgress.UpdateInfo(Sender);
      until GetOut or (Derived.Equil.Iterations = Control.Equil.MaxIterations) or Control.EndEquil;
   frmEquilProgress.Stop;
   If GetOut then
      Derived.Equil.SolutionFound := true
   Else
      Derived.Equil.SolutionFound := false;
   {save resulting pool file}
{   SoilOrganic.Struct := StartStruct;
   SoilOrganic.FineWood := StartFineWood;
   SoilOrganic.CoarseWood := StartCoarseWood;
   SoilOrganic.Active := StartActive;
   SoilOrganic.Slow := StartSlow;
   Soilorganic.Resistant := StartResistant;}
   Control.TotalYears := InitialData.Year;
   Control.ExtraMonths := InitialData.Month;
   Control.ExtraDays := InitialData.Day;
   If Control.Equil.SamePlantPools then
      Begin
      Plant.Sapwood := InitialData.Sapwood;
      Plant.Heartwood := InitialData.HeartWood;
      Plant.CoarseRoot := InitialData.CoarseRoot;
      Plant.FineRoot := InitialData.FineRoot;
      Plant.Leaves := InitialData.Leaves;
      Plant.Reserves := InitialData.Reserves;
      Plant.Branches := InitialData.Branches;
      Plant.Bark := InitialData.Bark;
      Plant.Pollen := InitialData.Pollen;
      Plant.Fruit := InitialData.Fruit;
      Plant.Soluble := InitialData.Soluble;
      SoilWat := InitialData.SoilWater;
      End;
   Plant.Stocking := InitialData.Stocking;
   Plant.Height := InitialData.Height;
   Plant.dbh := InitialData.dbh;
   Plant.Age := InitialData.Age;
   SoilOrganic.Inert := InitialData.Inert;
   SoilOrganic.RockP := InitialData.RockP;
   If not Abort then
      If frmEquilFinished.ShowModal = mrOK then
         Begin
         Control.InitGenerated := true;
         mnuSaveinitialvaluesClick(Sender);
         End
      Else
         Begin
         Parameter.BiolFix := OldBiolNFix;
         Parameter.NLoss := OldNLoss;
         Parameter.Atmos_P := OldPInput;
         End;
   Control.EndEquil := true;
   Control.EquilMode := false;
   EndRun;
   Control.ProjectHasChanged := true;
   If Control.Equil.EquilParameter = BiolNFix then
      Control.PlantHasChanged := true
   Else
      Control.SiteHasChanged := true;
   End
Else if not Control.AgreeOK then
   MessageDlg('CANNOT RUN SIMULATION' + chr(10) +
                 'You can only run simulations ' +
                 'after indicating your acceptance ' +
                 'of the licence agreement.' + chr(10) +
                 'To enable you to run the simulation routine, go to the ' +
                 'VIEW menu, choose LICENCE AGREEMENT and indicate ' +
                 'your acceptance of the licence agreement.',
                 mtError, [mbOK], 0);
end;

Procedure TfrmMain.mnuSensititivityClick(Sender: TObject);
var i: Integer;
Begin
If Control.AgreeOK then
   Begin
   If (Parameter.FertilityAdjust <> 1) then
      MessageDlg('Caution: Sizes of the initial soil organic matter pools ' +
                 'are being adjusted at the start of this run.' + chr(10) +
                 'If you want to change that, ' +
                 'you can adjust the fertility parameter ' +
                 'in the "PARAMETERS".."DECOMPOSITION" menu.',
                 mtInformation, [mbOK], 0);
   If (Plant.Stocking = 0) then
      MessageDlg('Caution: Stand stocking is set to ZERO.' + chr(10) +
                 'You cannot run meaningful simulations ' +
                 'when there are no plants on the site.' + chr(10) +
                 'You can adjust the initial stocking rate ' +
                 'in the "INITIAL".."MISCELLANEOUS" menu.',
                 mtInformation, [mbOK], 0);
   TestSens.Index := 0;
   TestSens.Abort := false;
   while ((TestSens.Index < 3) and (not TestSens.Abort)) do
       Begin
       TestSens.Index := TestSens.Index + 1;
       frmSensSelect.ShowModal;
       End;
   if not TestSens.Abort then
      Begin
      Control.SensParameter := Dummy;
      Control.SensFlag := 1;
      Control.SensFileOpen := false;
      Control.InitGenerated := true;
      Control.SensitivityTestOn := true;
      InitialiseSensitivity;
      frmFileIO.GetParameterFile(Control.PoolFile, EXT_INITIAL);
      If Control.ResetPlantPools then
         ResetPlantPools;
      AdjustFertility;
      ControlRun;
      WriteOutSens;
      While Control.SensParameter < EndInputs do
            Begin
            Repeat
               Control.SensParameter := Succ(Control.SensParameter);
            Until TestSens.Choose[Control.SensParameter] or (Control.SensParameter = EndInputs);
            if Control.SensParameter <> EndInputs then
               Begin
               for i := 0 to 1 do
                   Begin
                   frmFileIO.GetParameterFile(Control.PoolFile, EXT_INITIAL);
                   If Control.ResetPlantPools then
                      ResetPlantPools;
                   AdjustFertility;
                   If i = 0 then
                      SetupSens(-1)
                   Else
                      SetupSens(1);
                   ControlRun;
                   WriteOutSens;
                   End;
               SetUpSens (0);  //In order to reset the last parameter
               End;
            End;
      EndSensRoutine;
      End
   End
Else if not Control.AgreeOK then
      MessageDlg('CANNOT RUN SIMULATION' + chr(10) +
                 'You can only run simulations ' +
                 'after indicating your acceptance ' +
                 'of the licence agreement.' + chr(10) +
                 'To enable you to run the simulation routine, go to the ' +
                 'VIEW menu, choose LICENCE AGREEMENT and indicate ' +
                 'your acceptance of the licence agreement.',
                 mtError, [mbOK], 0);

end; { TfrmMain }


Procedure TfrmMain.mnuSpatialClick(Sender: TObject);
var NextChar: Char;
    i, j, iLayer, TargetHit: integer;
    TargetN, RainSum, TempSum, MinTemp, MaxTemp, OldWaterHold, OldBio, OldSlow, Deltax, DeltaY,
    SlowC, SlowN, ActiveC, ActiveN, ResistantC, ResistantN: real48;
    CardinalTemps: array [1..4] of real48;
    OutOfBounds, FileFound: Boolean;
Begin
for iLayer := 1 to SoilWat.nLayers do
    OldWaterHold := OldwaterHold + SoilWat.Layer[iLayer].WaterContent;
CardinalTemps[1] := Parameter.TMinLim;
CardinalTemps[2] := Parameter.TOpt1;
CardinalTemps[3] := Parameter.TOpt2;
CardinalTemps[4] := Parameter.TMaxLim;
OldBio := Parameter.BiolFix;
SlowC := 0;
Control.EndSpatial := false;
Control.SpatialMode := true;
frmSpatialParameters.ShowModal;
If Control.AgreeOK and Control.SpatialMode then
      Begin
      frmFileIO.GetParameterFile(Control.SpatialFile, EXT_SPATIAL);
      Control.Spatial.xInter := trunc(-0.001 + Divide((ClientRect.Right - ClientRect.Left),
              Divide((Control.Spatial.LongMax - Control.Spatial.LongMin), Control.Spatial.LongInterval)));
      Control.Spatial.yInter := trunc(-0.001 + Divide((ClientRect.Bottom - ClientRect.Top),
              Divide((Control.Spatial.LatMax - Control.Spatial.LatMin), Control.Spatial.LatInterval)));
      frmFileIO.GetParameterFile(Control.PoolFile, 'IL!');
      If Control.Spatial.Initial > 0 then
         Control.Spatial.Count := Control.Spatial.Initial
      Else
         Control.Spatial.Count := 0;
      For j := 1 to Control.Spatial.Initial do  // skip over initial sites
            Begin
            if not eof(Control.SpatialText) then
               Repeat
                 Readln (Control.SpatialText, NextChar);
               Until (NextChar = '*') or eof(Control.SpatialText);
            if not eof(Control.Spatialtext) then
               Repeat
                  Readln (Control.Spatialtext, NextChar);
               Until (NextChar <> '*') or eof(Control.Spatialtext);
            If not eof(Control.SpatialText) then
               Readln (Control.SpatialText, Parameter.Longitude, Parameter.Latitude,
                       Control.Spatial.WaterHold, Control.Spatial.Fertility);
            For i := 1 to 12 do
                begin
                if not eof(Control.SpatialText) then
                   Readln (Control.SpatialText, Control.Spatial.Tmin[i],
                                              Control.Spatial.Tmax[i],
                                              Control.Spatial.Radn[i],
                                              Control.Spatial.Rain[i])
                else
                   Control.EndSpatial := true;
                end;
            End;
      While (not Control.EndSpatial) and
            ((Control.Spatial.Calcs < 0) or (Control.Spatial.Count < Control.Spatial.Calcs)) do
            Begin
            if not eof(Control.SpatialText) then
               Repeat
                 Readln (Control.SpatialText, NextChar);
               Until (NextChar = '*') or eof(Control.SpatialText);
            if not eof(Control.SpatialText) then
               Repeat
                  Readln (Control.SpatialText, NextChar);
               Until (NextChar <> '*') or eof(Control.SpatialText);
            RainSum := 0; TempSum := 0;
            If not eof(Control.SpatialText) then
               Readln (Control.SpatialText, Parameter.Longitude, Parameter.Latitude,
                       Control.Spatial.WaterHold, Control.Spatial.Fertility,
                       Control.Spatial.ActiveC, Control.Spatial.ActiveN,
                       Control.Spatial.SlowC, Control.Spatial.SlowN,
                       Control.Spatial.ResistC, Control.Spatial.ResistN);
            SoilWat.Layer[1].MaxWater := 10;
            SoilWat.Layer[2].MaxWater := Control.Spatial.WaterHold - SoilWat.Layer[1].MaxWater;
            SoilWat.Layer[1].WaterContent := 0.5 * SoilWat.Layer[1].MaxWater;
            SoilWat.Layer[2].WaterContent := 0.5 * SoilWat.Layer[2].MaxWater;
            Parameter.FertilityAdjust := Control.Spatial.Fertility;
            MaxTemp := -100; MinTemp := 100;
            For i := 1 to 12 do
                begin
                if not eof(Control.SpatialText) then
                   Readln (Control.SpatialText, Control.Spatial.Tmin[i],
                                                Control.Spatial.Tmax[i],
                                                Control.Spatial.Radn[i],
                                                Control.Spatial.Rain[i])
                else
                   Control.EndSpatial := true;
                If (0.5 * (Control.Spatial.Tmax[i]+ Control.Spatial.Tmin[i])) > MaxTemp then
                   MaxTemp := 0.5 * (Control.Spatial.Tmax[i]+ Control.Spatial.Tmin[i]);
                If (0.5 * (Control.Spatial.Tmax[i]+ Control.Spatial.Tmin[i])) < MinTemp then
                   MinTemp := 0.5 * (Control.Spatial.Tmax[i]+ Control.Spatial.Tmin[i]);
                RainSum := RainSum + Control.Spatial.Rain[i];
                TempSum := TempSum + Control.Spatial.Tmax[i]+ Control.Spatial.Tmin[i];
                end;
            If not Control.EndSpatial then
               Begin
               Control.Spatial.Count := Control.Spatial.Count + 1;
               If (Parameter.Latitude > Control.Spatial.LatMin) and (Parameter.Latitude < Control.Spatial.LatMax) and
                  (Parameter.Longitude > Control.Spatial.LongMin) and (Parameter.Longitude < Control.Spatial.LongMax) then
                  OutOfBounds := false
               Else
                  OutOfBounds := true;
               RainSum := RainSum * 30.5;
               TempSum := TempSum / 24;
               If (Control.Spatial.Count = 1) or
                  ((RainSum > Control.Spatial.RainMin) and (RainSum < Control.Spatial.RainMax) and
                  (TempSum > Control.Spatial.TempMin) and (TempSum < Control.Spatial.TempMax) and
                  (not OutOfBounds)) then
                  Begin
                  If Control.Spatial.SoilType = Equil then
                     Begin
                     If Parameter.FertilityAdjust <= 1 then
                        TargetN := Parameter.N0 + (Parameter.Ncrit - Parameter.N0) * Parameter.FertilityAdjust
                     Else
                        TargetN := 0.5 * (Parameter.Ncrit + Parameter.NMax);
                     Parameter.BiolFix := Parameter.BiolFix * TargetN / Parameter.Ncrit;
                     If SlowC <> 0 then
                        Begin
                        SoilOrganic.Active[1, C] := Control.Spatial.ActiveC;
                        SoilOrganic.Active[1, N] := Control.Spatial.ActiveN;
                        SoilOrganic.Slow[1, C] := Control.Spatial.SlowC;
                        SoilOrganic.Slow[1, N] := Control.Spatial.SlowN;
                        SoilOrganic.Resistant[1, C] := Control.Spatial.ResistC;
                        SoilOrganic.Resistant[1, N] := Control.Spatial.ResistN;
                        SoilOrganic.Struct[0, C] := SoilOrganic.Struct[0, C] * TargetN / Parameter.Ncrit;
                        SoilOrganic.Struct[0, N] := SoilOrganic.Struct[0, N] * sqr(TargetN / Parameter.Ncrit);
                        SoilOrganic.Struct[1, C] := SoilOrganic.Struct[1, C] * TargetN / Parameter.Ncrit;
                        SoilOrganic.Struct[1, N] := SoilOrganic.Struct[1, N] * sqr(TargetN / Parameter.Ncrit);
                        SoilOrganic.Metab[0, N] := SoilOrganic.Metab[0, C] * TargetN / Parameter.Ncrit;
                        SoilOrganic.Metab[0, N] := SoilOrganic.Metab[0, N] * sqr(TargetN / Parameter.Ncrit);
                        SoilOrganic.Metab[1, C] := SoilOrganic.Metab[1, C] * TargetN / Parameter.Ncrit;
                        SoilOrganic.Metab[1, N] := SoilOrganic.Metab[1, N] * sqr(TargetN / Parameter.Ncrit);
                        Plant.Stocking := 1000;
                        Plant.Height := 1;
                        Plant.DBH := 1;
                        Plant.SapWood[C] := 0;
                        Plant.SapWood[N] := 0;
                        Plant.HeartWood[C] := 0;
                        Plant.HeartWood[N] := 0;
                        Plant.Branches[C] := Plant.Branches[C] * TargetN / Parameter.Ncrit;
                        Plant.Branches[N] := Plant.Branches[N] * sqr(TargetN / Parameter.Ncrit);
                        Plant.CoarseRoot[C] := Plant.CoarseRoot[C] * TargetN / Parameter.Ncrit;
                        Plant.CoarseRoot[N] := Plant.CoarseRoot[N] * sqr(TargetN / Parameter.Ncrit);
                        Plant.Leaves[N] := Plant.Leaves[N] * sqr(TargetN / Parameter.Ncrit);
                        Plant.Leaves[C] := Plant.Leaves[C] * TargetN / Parameter.Ncrit;
                        If Plant.Leaves[C] < 10 then Plant.Leaves[C] := 10;
                        If Plant.Leaves[N] < (0.5 * Plant.Leaves[C] * (Parameter.N0 + Parameter.Ncrit)) then
                           Plant.Leaves[N] := 0.5 * Plant.Leaves[C] * (Parameter.N0 + Parameter.Ncrit);
                        Control.TotalYears := 1983;
                        Control.ExtraMonths := 6;
                        Control.ExtraDays := 30;
                        Plant.Age := 0;
                        End
                     Else
                        frmFileIO.GetParameterFile(Control.PoolFile, 'IL!');
                     OldSlow := SoilOrganic.Slow[1, N] + SoilOrganic.Resistant[1, N] + SoilOrganic.Active[1, N];
                     Deltax := 1;
                     DeltaY := 0.1;
                     TargetHit := 0;
                     End
                  Else
                     Begin
                     frmFileIO.GetParameterFile(Control.PoolFile, 'IL!');
                     SoilOrganic.Active[1, C] := Control.Spatial.ActiveC;
                     SoilOrganic.Active[1, N] := Control.Spatial.ActiveN;
                     SoilOrganic.Slow[1, C] := Control.Spatial.SlowC;
                     SoilOrganic.Slow[1, N] := Control.Spatial.SlowN;
                     SoilOrganic.Resistant[1, C] := Control.Spatial.ResistC;
                     SoilOrganic.Resistant[1, N] := Control.Spatial.ResistN;
                     End;
                  Initialise;
                  If Control.Spatial.PlantType = Optimal then
                     Begin
                     Parameter.TOpt1 := TempSum;
                     If Parameter.TOpt1 < 10 then
                        Parameter.TOpt1 := 10
                     Else if Parameter.TOpt1 > 30 then
                        Parameter.TOpt1 := 30;
                     Parameter.TOpt2 := Parameter.TOpt1;
                     Parameter.TMinLim := 2 * MinTemp - TempSum;
                     If Parameter.TMinLim < 0 then
                        Parameter.TMinLim := 0;
                     Parameter.TMaxLim := 2 * MaxTemp - TempSum;
                     If Parameter.TMaxLim > 35 then
                        Parameter.TMaxLim := 35;
                     End;
                  ControlRun;
                  If Control.Spatial.SoilType = Equil then
                     Begin
                     SlowC := SoilOrganic.Slow[1, C] * Parameter.Ncrit / TargetN;
                     SlowN := SoilOrganic.Slow[1, N] * Parameter.Ncrit / TargetN;
                     ActiveC:= SoilOrganic.Active[1, C] * Parameter.Ncrit / TargetN;
                     ActiveN:= SoilOrganic.Active[1, N] * Parameter.Ncrit / TargetN;
                     ResistantC := SoilOrganic.Resistant[1, C] * Parameter.Ncrit / TargetN;
                     ResistantN := SoilOrganic.Resistant[1, N] * Parameter.Ncrit / TargetN;
                     SoilOrganic.Struct[0, C] := SoilOrganic.Struct[0, C] * Parameter.Ncrit / TargetN;
                     SoilOrganic.Struct[0, N] := SoilOrganic.Struct[0, N] * sqr(Parameter.Ncrit / TargetN);
                     SoilOrganic.Struct[1, C] := SoilOrganic.Struct[1, C] * Parameter.Ncrit / TargetN;
                     SoilOrganic.Struct[1, N] := SoilOrganic.Struct[1, N] * sqr(Parameter.Ncrit / TargetN);
                     SoilOrganic.Metab[0, N] := SoilOrganic.Metab[0, C] * Parameter.Ncrit / TargetN;
                     SoilOrganic.Metab[0, N] := SoilOrganic.Metab[0, N] * sqr(Parameter.Ncrit / TargetN);
                     SoilOrganic.Metab[1, C] := SoilOrganic.Metab[1, C] * Parameter.Ncrit / TargetN;
                     SoilOrganic.Metab[1, N] := SoilOrganic.Metab[1, N] * sqr(Parameter.Ncrit / TargetN);
                     Plant.Branches[C] := Plant.Branches[C] * Parameter.Ncrit / TargetN;
                     Plant.Branches[N] := Plant.Branches[N] * sqr(Parameter.Ncrit / TargetN);
                     Plant.CoarseRoot[C] := Plant.CoarseRoot[C] * Parameter.Ncrit / TargetN;
                     Plant.CoarseRoot[N] := Plant.CoarseRoot[N] * sqr(Parameter.Ncrit / TargetN);
                     Plant.Leaves[N] := Plant.Leaves[N] * sqr(Parameter.Ncrit / TargetN);
                     Plant.Leaves[C] := Plant.Leaves[C] * Parameter.Ncrit / TargetN;
                     Parameter.BiolFix := Parameter.BiolFix * Parameter.Ncrit / TargetN;
                     End;
                  End
               Else if not OutOfBounds or Control.EndSpatial then
                  Begin
                  If not Control.OutputFileOpen then
                     Initialise;
                  Control.DataToBeSaved := true;
                  Control.Run_On := false;
                  Derived.CAI := 0; Derived.NPP := 0;
//                  PrintOut(msg);
                  End;
               frmSpatialProgress.formShow(Sender);
               If Control.Spatial.ShowNumeric and not OutOfBounds then
                  Begin
                  If (Control.Spatial.SoilType = Equil) or (Control.Spatial.Count mod 20 = 1) then
                     Writeln (Control.CenWFileOut, 'Count':5, 'Rain':5, 'Mean':6, 'Long':6, 'Lat':5,
                              'Hold':5, 'Fert':5, 'Slow':7, 'Wood':7, 'Leaf':6, 'LeafN':6,
                              'CAI':5, 'NPP':8);
                  Writeln (Control.CenWFileOut, Control.Spatial.Count:5, RainSum:5:0, TempSum:6:1,
                          Parameter.Longitude:6:1, Parameter.Latitude:6:1,
                       Control.Spatial.WaterHold:4:0, Control.Spatial.Fertility:5:2,
                       Soilorganic.Slow[1, C]:7:0, (Plant.SapWood[C] + Plant.HeartWood[C]):7:0,
                       Plant.Leaves[C]:6:0, Plant.Leaves[N]:6:1,
                       Derived.CAI/450:8:4, Derived.NPP/450:8:1);
                  End;
               End;
            End;
      EndRun;
      frmEquilProgress.Stop;
      Control.SpatialMode := false;
      Parameter.FertilityAdjust := 1;
      SoilWat.Layer[1].MaxWater := 20;
      SoilWat.Layer[2].MaxWater := OldWaterHold - SoilWat.Layer[1].MaxWater;
      Parameter.TMinLim := CardinalTemps[1];
      Parameter.TOpt1 := CardinalTemps[2];
      Parameter.TOpt2 := CardinalTemps[3];
      Parameter.TMaxLim := CardinalTemps[4];
      Parameter.BiolFix := OldBio;
      End
   Else if not Control.AgreeOK then
      MessageDlg('CANNOT RUN SIMULATION' + chr(10) +
                 'You can only run simulations ' +
                 'after indicating your acceptance ' +
                 'of the licence agreement.' + chr(10) +
                 'To enable you to run the simulation routine, go to the ' +
                 'VIEW menu, choose LICENCE AGREEMENT and indicate ' +
                 'your acceptance of the licence agreement.',
                 mtError, [mbOK], 0);
End; {of Procedure 'mnuSpatialClick'}

Procedure TfrmMain.mnuLegendClick(Sender: TObject);
begin
// toggle the legend
if mnuLegend.Checked then
   // turn off the legend
   TfrmGraph(MDIChildren[0]).chtGraph.Legend.Visible := false
Else
   // turn on the legend
   TfrmGraph(MDIChildren[0]).chtGraph.Legend.Visible := true;
end;

Procedure TfrmMain.actnShowLegendUpdate(Sender: TObject);
begin
// update the legend status
if (MDIChildCount > 0) then
   begin
   // the checked flag comes from the visibile flag of the legend on
   // the foreground graph window
   actnShowLegend.Checked := TfrmGraph(MDIChildren[0]).chtGraph.Legend.Visible;
   actnShowLegend.Enabled := true;
   End
Else
   Begin
   // no graphs visible, so disable the menu item
   actnShowLegend.Enabled := false;
   actnShowLegend.Checked := false;
   End;
end;

Procedure TfrmMain.mnuLegendwithrangeClick(Sender: TObject);
var iCount: integer;
    sText: string;
    cht: TChart;
    ScreenVar: ScreenOptions;
begin
  // view the legend with range
  if (mnuLegendwithrange.Checked) then
    begin
    // turn off the range
    mnuLegendwithrange.Checked := false;
    cht := TfrmGraph(MDIChildren[0]).chtGraph;
    for iCount:=1 to cht.SeriesList.Count do
      begin
      sText := cht.SeriesList.Items[iCount - 1].Title;
      // does this title have a range?
      if (Pos(' : ', sText) > 0) then
        begin
        // yes, so remove it
        sText := Copy(sText, 1, Pos(' : ', sText) - 1);
        cht.SeriesList.Items[iCount - 1].Title := sText;
        end;
      end;
    end
  else
    begin
    // turn on the range
    mnuLegendwithrange.Checked := true;
    cht := TfrmGraph(MDIChildren[0]).chtGraph;
    for iCount:=1 to cht.SeriesList.Count do
      begin
      sText := cht.SeriesList.Items[iCount - 1].Title;
      // does this title have a range?
      if (Pos(' : ', sText) = 0) then
        begin
        // no, so add it
        // first, find the ScreenVar it belongs to
        for ScreenVar:=D_CarbonGain to D_Snow do
          begin
          // find the matching ScreenVariableName, since that is what the
          // data series title was made from in the first place
          if (sText = ScreenVariableNames[ScreenVar]) then
            begin
            sText := sText + ' : ' + Format('%.2f', [ScreenRec.LowRange[ScreenVar]]);
            sText := sText + '..' + Format('%.2f', [ScreenRec.UpRange[ScreenVar]]);
            cht.SeriesList.Items[iCount - 1].Title := sText;
            break;
            end;
          end;
        end;
      end;
    end;
  end;

Procedure TfrmMain.actnShowRangesUpdate(Sender: TObject);
var
  sText: string;
begin
// update the legend range status
// are there any data series in the current graph?
If (MDIChildCount > 0) and (mnuLegend.Checked) and
   (TfrmGraph(MDIChildren[0]).chtGraph.SeriesList.Count > 0) then
    begin
    // see if the current graph legend has ranges shown in it
    // ranges are shown after the data series title
    // eg. Rainfall : 0-50mm
    // The ' : ' string is the important token, which we use to keep
    // track of whether there is a range in the legend or not.
    // Therefore it is important that this string does not appear
    // in any title!
    sText := TfrmGraph(MDIChildren[0]).chtGraph.SeriesList.Items[0].Title;
    actnShowRanges.Checked := (Pos(' : ', sText) <> 0);
    actnShowRanges.Enabled := true;
    end
else
    // no graphs visible, so disable the menu item
    actnShowRanges.Enabled := false;
end;

Procedure TfrmMain.mnuLicenceagreementClick(Sender: TObject);
begin
frmAgreement.ShowModal;
end;

Procedure TfrmMain.mnuTileClick(Sender: TObject);
begin
Tile;
end;

Procedure TfrmMain.mnuCascadeClick(Sender: TObject);
begin
Cascade;
end;

Procedure TfrmMain.mnuArrangeAllClick(Sender: TObject);
begin
ArrangeIcons;
end;

Procedure TfrmMain.mnuContentsClick(Sender: TObject);
begin
WinHelp(Handle, PChar(Application.HelpFile), Help_Index, 0);
end;

Procedure TfrmMain.mnuHowtoUseHelpClick(Sender: TObject);
begin
WinHelp(Handle, PChar(Application.HelpFile), HELP_HELPONHELP, 0);
end;

Procedure TfrmMain.mnuAboutClick(Sender: TObject);
begin
frmAbout.ShowModal;
end;

Procedure TfrmMain.FormCreate(Sender: TObject);

begin
Application.HelpFile := ExtractFilePath(ParamStr(0)) + 'CenW.hlp';
Control.ProjectHasChanged := FALSE;
Control.PlantHasChanged := FALSE;
Control.SiteHasChanged := FALSE;
Control.InitHasChanged := False;
Control.InitGenerated := False;
Control.ScreenHasChanged := false;
Control.AgreeChecked := false;
Control.SensitivityTestOn := false;
Control.OutputFileOpen := false;
Control.BatchMode := false;
Control.Run_On := False;
Control.Legendon := false;
Control.Range := false;
Control.CountersSaved := false;
ReadCenw ('CenwDef.df!', Control.ProjectFile);
end; { TfrmMain }


function TfrmMain.CheckForSave: boolean;
Var Answer: Integer;
    st: String;
    s : array[0..255] of char;
Begin
// check if we need to save any changed datasets
result := true;
If (Control.ProjectHasChanged or Control.PlantHasChanged or Control.SiteHasChanged) then
   Begin
   Answer := MessageDlg('Dataset has changed' + chr(10) + 'Do you want to save it?',
                        mtConfirmation, [mbYes, mbNo, mbCancel], 0);
   If (Answer = mrYes) then
      Begin
      result := true;
      SetCurrentDir(Control.ProjectDirectory);
      If Control.ProjectHasChanged then SaveProject(Control.ProjectFile);
      If Control.PlantHasChanged then SavePlant(Control.PlantFile);
      If Control.SiteHasChanged then SaveSite(Control.SiteFile);
      End
   Else if (Answer = mrNo) then
      result := true
   Else {if Answer = id_Cancel then}
      result := false;
   End;
SaveCenw ('CenWDef.df!', Control.ProjectFile);
If Control.ErrorSaving then
   Begin
   st := '                   Program will end' + chr(10) +
         '                 without saving data.' + chr(10);
   strpcopy(s,st);
   Application.MessageBox(s, 'CANNOT CREATE OUTPUT FILE!', MB_OK);
   End;
end;

Procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
CanClose := CheckForSave;
end;

Procedure TfrmMain.FormShow(Sender: TObject);
begin
If not Control.RunWithDefaults then
   Begin
   SetInitialProjectDefaults; // Set all newly introduced project parameters to some starting values
   SetInitialPlantDefaults; // Set all newly introduced plant parameters to some starting values
   SetInitialSiteDefaults; // Set all newly introduced site parameters to some starting values
   // does the project file exist?
   while not(FileExists(Control.ProjectFile)) do
      begin
      // no, so ask the user to find the real project file
      ShowMessage('The project file "' + Control.ProjectFile +
                  '" does not exist.  Please select a valid project to load.');
      if (dlgOpenProject.Execute) then
         Control.ProjectFile := dlgOpenProject.FileName
      Else
         // if they cancel here, we'll just have to exit
         Halt;
      End;
   // load the project file
   If not Control.RunWithDefaults then
       Begin
       frmFileIO.GetParameterFile(Control.ProjectFile, EXT_PROJECT);
       Control.ProjectDirectory := ExtractFilePath(Control.ProjectFile);
       SetCurrentDir(ExtractFilePath(Control.ProjectFile));
       frmFileIO.GetParameterFile(Control.PlantFile, EXT_PLANT);
       frmFileIO.GetParameterFile(Control.SiteFile, EXT_SITE);
       frmFileIO.GetParameterFile(Control.PoolFile, EXT_INITIAL);
       End;
   If Control.ResetPlantPools then
       ResetPlantPools;
   End;
If (not Control.AgreeOK) and (not Control.AgreeChecked) then
   Begin
   mnuLicenceagreementClick(Sender);
   FormShow(Sender);
   End;
IntroImage.Visible := true;
actIncludePDisplayExecute(Sender);
actIncludePSaveExecute(Sender);
actnIncludePhosphorusExecute(Sender);
actIncludeDeltaExecute(Sender);
actIncludeWeedParametersExecute(Sender);
End;

Procedure TfrmMain.actIncludeWeedParametersExecute(Sender: TObject);
Begin
If Control.IncludeWeeds then
   Begin
   frmMain.mnuWeedParameters.Enabled := true;
   frmMain.mnuWeedParameters.Visible := true;
   End
Else
   Begin
   frmMain.mnuWeedParameters.Enabled := false;
   frmMain.mnuWeedParameters.Visible := false;
   End;
End;

Procedure TfrmMain.actIncludePDisplayExecute(Sender: TObject);
Begin
If Control.IncludeP then
   Begin
   frmMain.mnuPhosphorus.Enabled := true;
   frmMain.mnuPhosphorus.Visible := true;
   End
Else
   Begin
   frmMain.mnuPhosphorus.Enabled := false;
   frmMain.mnuPhosphorus.Visible := false;
   End;
End;

Procedure TfrmMain.actIncludePSaveExecute(Sender: TObject);
Begin
If Control.IncludeP then
   Begin
   frmMain.mnuSavePhosphorusInfo.Enabled := true;
   frmMain.mnuSavePhosphorusInfo.Visible := true;
   End
Else
   Begin
   frmMain.mnuSavePhosphorusInfo.Enabled := false;
   frmMain.mnuSavePhosphorusInfo.Visible := false;
   End;
End;


Procedure TfrmMain.actnIncludePhosphorusExecute(Sender: TObject);
Begin
If Control.IncludeP then
   Begin
   frmMain.mnuPhosphorusPools.Enabled := true;
   frmMain.mnuPhosphorusPools.Visible := true;
   End
Else
   Begin
   frmMain.mnuPhosphorusPools.Enabled := false;
   frmMain.mnuPhosphorusPools.Visible := false;
   End;
End;

Procedure TfrmMain.actIncludeDeltaExecute(Sender: TObject);
begin
If Control.IncludeIsotopes then
   Begin
   frmMain.mnuC13Pools.Enabled := true;
   frmMain.mnuC13Pools.Visible := true;
   End
Else
   Begin
   frmMain.mnuC13Pools.Enabled := false;
   frmMain.mnuC13Pools.Visible := false;
   End;
end;

Procedure TfrmMain.FillEdit(Sender: TObject; edtBox: TEdit; fValue, fScale: Real48);
var Width, Digits: Integer;
    s: String;
Begin
If fScale = 0 then  // Code that the number passed is an integer
   Begin
   Width := 0;
   Digits := 0;
   End
Else
   Begin
   fValue := fValue * fScale;
   GetField(fValue, MaxedtFieldWidth, Width, Digits);
   End;
Str (fValue:Width:Digits, s);
edtBox.Text := s;
edtBox.MaxLength := Width;
End;

Procedure TfrmMain.GetEdit(Sender: TObject; edtBox: TEdit; var fValue: Real48; fScale: Real48);
var s: String;
    Num: Real48;
    Code: Integer;
Begin
s := edtBox.Text;
Val (s, Num, Code);
If Code <> 0 then
   MessageDlg('Caution: Invalid numeric format.' + chr(10) +
              'The previous value is not modified.' +
              'Re-enter the dialogue box if you' + chr(10) +
              'want to enter a valid number.',
               mtInformation, [mbOK], 0)
Else
   fValue := Num / fScale;
End;

Procedure TfrmMain.GetInteger(Sender: TObject; edtBox: TEdit; var iValue: Integer);
Const Threshold = 0.0001;
var fValue: Real48;

Begin
fValue := iValue;
frmMain.GetEdit(Sender, edtBox, fValue, 1);
If fValue <> 0 then
   Begin
   If (fValue - Round(fValue)) > Threshold then
      MessageDlg('Caution: The input number is not an integer!' + chr(10) +
              'The number will be rounded' + chr(10) +
              'to the nearest integer',
               mtInformation, [mbOK], 0);
   End;
iValue := Round(fValue);
End;

Procedure TfrmMain.BackUpFilesClick(Sender: TObject);
var ProjectBkp, PlantBkp, SiteBkp: FileNameType;
    DateTime: String;
    i: Integer;

begin
DateTime := ' - ' + DateToStr(Date) + ' ' + TimeToStr(Time);
For i := 1 to length(DateTime) do
    Begin
    If DateTime[i] = '/' then
       DateTime[i] := '-'
    Else if DateTime[i] = ':' then
       DateTime[i] := 'h'    End;
ProjectBkp := Copy(Control.ProjectFile, 1, length(Control.ProjectFile) - 4) + DateTime + '.PJ!';
PlantBkp := Copy(Control.PlantFile, 1, length(Control.PlantFile) - 4) + DateTime + '.PL!';
SiteBkp := copy(Control.SiteFile, 1, length(Control.SiteFile) - 4) + DateTime + '.ST!';
SaveProject(ProjectBkp);
SavePlant(PlantBkp);
SaveSite(SiteBkp);
end;

Procedure TfrmMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
IntroImage.Visible := false;
end;

END OF MORE COMMENTED OUT PROCS }

end.
