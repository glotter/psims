{ ================================================================
  = Project   : CENW                                             =
  ================================================================
  = Modules   : TfrmMain                                         =
  =                                                              =
  =             This handles the main menu and sets up           =
  =             most procedures and menu options                 =
  =             other than those that have their own units       =
  ================================================================
  = File      : untMain.PAS                                      =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

unit untMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Menus, CheckLst, ActnList, TeEngine, Series, untFieldValidation,
  ExtCtrls, WinHelpViewer;

type
  TfrmMain = class(TForm)
    mnuMain: TMainMenu;
    mnuFiles: TMenuItem;
    mnuParameters: TMenuItem;
    mnuPools: TMenuItem;
    mnuEvents: TMenuItem;
    mnuScreen: TMenuItem;
    mnuDisk: TMenuItem;
    mnuRun: TMenuItem;
    mnuView: TMenuItem;
    mnuLoadproject: TMenuItem;
    mnuSaveproject: TMenuItem;
    mnuN1: TMenuItem;
    mnuLoadPlantfile: TMenuItem;
    mnuSavePlantfile: TMenuItem;
    mnuN2: TMenuItem;
    mnuLoadSitefile: TMenuItem;
    mnuSaveSitefile: TMenuItem;
    mnuN3: TMenuItem;
    mnuLoadInitialvalues: TMenuItem;
    mnuSaveinitialvalues: TMenuItem;
    mnuWindow: TMenuItem;
    mnuArrangeAll: TMenuItem;
    mnuCascade: TMenuItem;
    mnuTile: TMenuItem;
    mnuHelp: TMenuItem;
    mnuAbout: TMenuItem;
    mnuHowtoUseHelp: TMenuItem;
    mnuSearchforHelpOn: TMenuItem;
    mnuContents: TMenuItem;
    mnuN5: TMenuItem;
    mnuReadWeatherdata: TMenuItem;
    mnuN6: TMenuItem;
    mnuExit: TMenuItem;
    dlgOpenProject: TOpenDialog;
    mnuSite: TMenuItem;
    mnuStand: TMenuItem;
    mnuPhotosyntheticterms: TMenuItem;
    mnuPhenologyterms: TMenuItem;
    mnuSoilwater: TMenuItem;
    mnuAllocation: TMenuItem;
    mnuN7: TMenuItem;
    mnuDecomposition: TMenuItem;
    mnuWeather: TMenuItem;
    mnuN8: TMenuItem;
    mnuControl: TMenuItem;
    mnuNitrogenpools: TMenuItem;
    mnuMiscellaneous: TMenuItem;
    mnuAddfertiliser: TMenuItem;
    mnuIrrigation: TMenuItem;
    mnuEnvironment: TMenuItem;
    mnuHarvestandthinning: TMenuItem;
    mnuPlantcarbon: TMenuItem;
    mnuPlantnitrogen: TMenuItem;
    mnuSoilorganicmatter: TMenuItem;
    mnuWeatherdata: TMenuItem;
    mnuSaveplantinfo: TMenuItem;
    mnuSavesoilinfo: TMenuItem;
    mnuSaveweatherinfo: TMenuItem;
    mnuStart: TMenuItem;
    mnuBatchmode: TMenuItem;
    mnuLegend: TMenuItem;
    mnuLegendwithrange: TMenuItem;
    mnuLicenceagreement: TMenuItem;
    mnuEquilibrium: TMenuItem;
    mnuN4: TMenuItem;
    dlgOpenPlant: TOpenDialog;
    dlgOpenSite: TOpenDialog;
    dlgOpenInitialValues: TOpenDialog;
    dlgSaveProject: TSaveDialog;
    dlgSaveInitialValues: TSaveDialog;
    dlgSaveSite: TSaveDialog;
    dlgSavePlant: TSaveDialog;
    ActionList1: TActionList;
    actnShowLegend: TAction;
    actnShowRanges: TAction;
    mnuFire: TMenuItem;
    mnuPests: TMenuItem;
    mnuCarbonpools: TMenuItem;
    mnuInitialPools: TMenuItem;
    mnuC13Pools: TMenuItem;
    actIncludeDelta: TAction;
    B1: TMenuItem;
    mnuPloughing: TMenuItem;
    IntroImage: TImage;
    procedure mnuExitClick(Sender: TObject);
    procedure mnuLoadprojectClick(Sender: TObject);
    procedure mnuSaveprojectClick(Sender: TObject);
    procedure mnuLoadPlantfileClick(Sender: TObject);
    procedure mnuSavePlantfileClick(Sender: TObject);
    procedure mnuLoadSitefileClick(Sender: TObject);
    procedure mnuSaveSitefileClick(Sender: TObject);
    procedure mnuLoadInitialvaluesClick(Sender: TObject);
    procedure mnuSaveinitialvaluesClick(Sender: TObject);
    procedure mnuReadWeatherdataClick(Sender: TObject);
    procedure mnuSiteClick(Sender: TObject);
    procedure mnuStandClick(Sender: TObject);
    procedure mnuPhotosynthetictermsClick(Sender: TObject);
    procedure mnuPhenologytermsClick(Sender: TObject);
    procedure mnuSoilwaterClick(Sender: TObject);
    procedure mnuSoilLitterClick(Sender: TObject);
    procedure mnuAllocationClick(Sender: TObject);
    procedure mnuDecompositionClick(Sender: TObject);
    procedure mnuWeatherClick(Sender: TObject);
    procedure mnuControlClick(Sender: TObject);
    procedure mnuCarbonPoolsClick(Sender: TObject);
    procedure mnuNitrogenpoolsClick(Sender: TObject);
    procedure mnuMiscellaneousClick(Sender: TObject);
    procedure mnuAddfertiliserClick(Sender: TObject);
    procedure mnuIrrigationClick(Sender: TObject);
    procedure mnuEnvironmentClick(Sender: TObject);
    procedure mnuHarvestandthinningClick(Sender: TObject);
    procedure mnuPestsClick(Sender: TObject);
    procedure mnuFireClick(Sender: TObject);
    procedure mnuPloughingClick(Sender: TObject);
    procedure mnuPlantcarbonClick(Sender: TObject);
    procedure mnuPlantnitrogenClick(Sender: TObject);
    procedure mnuSoilorganicmatterClick(Sender: TObject);
    procedure mnuWeatherdataClick(Sender: TObject);
    procedure mnuSaveplantinfoClick(Sender: TObject);
    procedure mnuSaveweatherinfoClick(Sender: TObject);
    procedure mnuSavesoilinfoClick(Sender: TObject);
    procedure mnuStartClick(Sender: TObject);
    procedure mnuMultipleRunsClick(Sender: TObject);
    procedure mnuBatchmodeClick(Sender: TObject);
    procedure mnuEquilibriumClick(Sender: TObject);
    procedure mnuLegendClick(Sender: TObject);
    procedure mnuLegendwithrangeClick(Sender: TObject);
    procedure mnuLicenceagreementClick(Sender: TObject);
    procedure mnuTileClick(Sender: TObject);
    procedure mnuCascadeClick(Sender: TObject);
    procedure mnuArrangeAllClick(Sender: TObject);
    procedure mnuContentsClick(Sender: TObject);
    procedure mnuHowtoUseHelpClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actnShowLegendUpdate(Sender: TObject);
    procedure actnShowRangesUpdate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuInitialPoolsClick(Sender: TObject);
    procedure mnuC13PoolsClick(Sender: TObject);
    procedure actIncludeDeltaExecute(Sender: TObject);
    procedure BackUpFilesClick(Sender: TObject);
    Procedure FillEdit(Sender: TObject; edtBox: TEdit; fValue, fScale: Real48);
    Procedure GetEdit(Sender: TObject; edtBox: TEdit; var fValue: Real48; fScale: Real48);
    Procedure GetInteger(Sender: TObject; edtBox: TEdit; var iValue: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

  private
    { Private declarations }
    function CheckForSave: boolean;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  untSiteParameters, untStandParameters, untPhotoSyntheticParameters,
  untAllocationParameters,
  untDecompositionParameters, untWeatherParameters, untControlParameters,
  untPools, untPlantCarbonDisplay, untDivideValidation,
  untPlantNitrogenDisplay, untSoilOrganicMatterDisplay, untPowerValidation,
  untWeatherDataDisplay, untSavePlantInfo, untSaveSoilInfo,
  untSaveWeatherInfo, untAbout, untIrrigationManagement, Chart,
  untHelpConsts, untDeclarations, untSimulate, untSimSoil, untFileIO, untFileIO2,
  untDiskOut, untMiscellaneous, untRun, untFileReadWeather, untGraph,
  untAgreement, untBatchParameters, untEquilParameters, untEquilFinished,
  untProgress, untMultipleRunParameters, untEquilProgress, untInitialPools, untC13Pools,
  untGenericListDialogue;

{$R *.DFM}

Procedure SetInitialProjectDefaults;
    {This small routine sets newly introduced parameters to some default values.
    This is over-ridden once users select their own values.}
    Begin
    Event.nPloughing := 0;
    End; {of procedure 'SetInitialProjectDefaults'}

Procedure SetInitialPlantDefaults;
    {This small routine sets newly introduced parameters to some default values.
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
    End; {of procedure 'SetInitialPlantDefaults'}

Procedure SetInitialSiteDefaults;
    var iLayer: Integer;
    {This small routine sets newly introduced parameters to some default values.
    This is over-ridden once users select their own values.}
    Begin
    Parameter.MaxTBoost := 0.0;
    Parameter.LigninInhibition := 3;
    Parameter.ImmobiliseInSlow := 0;
    SoilWat.SeparateSensitivity := false;
    For iLayer := 1 to MaxSoilLayers do
        SoilWat.Layer[iLayer].StressSensitivity := 0;
    End; {of procedure 'SetInitialSiteDefaults'}

procedure TfrmMain.mnuLoadprojectClick(Sender: TObject);
Var FileIsThere, Dummy: Boolean;
begin
  dlgOpenProject.FileName := Control.ProjectFile;
  if (dlgOpenProject.Execute) then
    begin
    if Control.ProjectHasChanged then
       frmMain.mnuSaveprojectClick(Sender);
    if Control.PlantHasChanged then
       frmMain.mnuSavePlantFileClick(Sender);
    if Control.SiteHasChanged then
       frmMain.mnuSaveSiteFileClick(Sender);
    Control.ProjectFile := dlgOpenProject.FileName;
    SetInitialProjectDefaults;
    frmFileIO.GetParameterFile(Control.ProjectFile, EXT_PROJECT);
    Control.ProjectHasChanged := false;
    Control.ProjectDirectory := ExtractFilePath(Control.ProjectFile);
    SetCurrentDir(Control.ProjectDirectory);
    SetInitialPlantDefaults;
    frmFileIO.GetParameterFile(Control.PlantFile, EXT_PLANT);
    Control.PlantHasChanged := false;
    SetInitialSiteDefaults;
    frmFileIO.GetParameterFile(Control.SiteFile, EXT_SITE);
    Control.SiteHasChanged := false;
    frmFileIO.IsFileThere(Control.ClimFile, FileIsThere);
    If not FileIsThere and (Control.ClimType = 'O') then
       frmFileIO.SolveProblem(Control.ClimFile, Ext_Climate, Dummy, false);
    end;
end;

procedure TfrmMain.mnuSaveprojectClick(Sender: TObject);
begin
dlgSaveProject.FileName := Control.ProjectFile;
if (dlgSaveProject.Execute) then
    begin
    Control.ProjectFile := dlgSaveProject.FileName;
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

Procedure TfrmMain.mnuLoadPlantfileClick(Sender: TObject);
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

procedure TfrmMain.mnuSavePlantfileClick(Sender: TObject);
begin
dlgSavePlant.FileName := Control.PlantFile;
if (dlgSavePlant.Execute) then
   Begin
   Control.PlantFile := dlgSavePlant.FileName;
   SavePlant(Control.PlantFile);
   Control.PlantHasChanged := false;
   End;
end;

procedure TfrmMain.mnuLoadSitefileClick(Sender: TObject);
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

procedure TfrmMain.mnuSaveSitefileClick(Sender: TObject);
begin
dlgSaveSite.FileName := Control.SiteFile;
if (dlgSaveSite.Execute) then
   Begin
   Control.SiteFile := dlgSaveSite.FileName;
   SaveSite(Control.SiteFile);
   Control.SiteHasChanged := false;
   End;
end;

procedure TfrmMain.mnuLoadInitialvaluesClick(Sender: TObject);
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

procedure TfrmMain.mnuSaveinitialvaluesClick(Sender: TObject);
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

procedure TfrmMain.mnuReadWeatherdataClick(Sender: TObject);
begin
frmFileReadWeather.ShowModal;
end;

procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
// close the program
Close;
end;

procedure TfrmMain.mnuSiteClick(Sender: TObject);
begin
frmSiteParameters.ShowModal;
end;

procedure TfrmMain.mnuStandClick(Sender: TObject);
begin
frmStandParameters.ShowModal;
end;

procedure TfrmMain.mnuPhotosynthetictermsClick(Sender: TObject);
begin
frmPhotoSyntheticParameters.ShowModal;
end;

procedure TfrmMain.mnuPhenologytermsClick(Sender: TObject);
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

procedure TfrmMain.mnuSoilwaterClick(Sender: TObject);
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

procedure TfrmMain.mnuSoilLitterClick(Sender: TObject);
begin
Repeat
   SetUpLitterDialogue;
   frmGenericListDialogue.ShowModal;
   If List.HasChanged then
      GetLitterDialogueInfo;
Until not List.Redraw;
end;

procedure TfrmMain.mnuAllocationClick(Sender: TObject);
begin
frmAllocationParameters.ShowModal;
end;

procedure TfrmMain.mnuDecompositionClick(Sender: TObject);
begin
frmDecompositionParameters.ShowModal;
end;

procedure TfrmMain.mnuWeatherClick(Sender: TObject);
begin
frmWeatherParameters.ShowModal;
end;

procedure TfrmMain.mnuControlClick(Sender: TObject);
begin
frmControlParameters.ShowModal;
actIncludeDeltaExecute(Sender);
end;

procedure TfrmMain.mnuCarbonPoolsClick(Sender: TObject);
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

procedure TfrmMain.mnuNitrogenpoolsClick(Sender: TObject);
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

procedure TfrmMain.mnuMiscellaneousClick(Sender: TObject);
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

procedure TfrmMain.mnuC13PoolsClick(Sender: TObject);
begin
if (MessageDlg('Before editing start-up values,' + chr(10) +
               'do you want to load new initial values?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
   mnuLoadInitialvaluesClick(Nil);
if (frmC13Pools.ShowModal = mrOK) then
   if (MessageDlg('Initial values have been changed.' + chr(10) +
                  'Do you want to save them?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
       mnuSaveinitialvaluesClick(Nil);
end;

procedure TfrmMain.mnuInitialPoolsClick(Sender: TObject);
begin
frmInitialPools.ShowModal;
end;

procedure TfrmMain.mnuAddfertiliserClick(Sender: TObject);
begin
SetUpFertiliserDialogue;
frmGenericListDialogue.ShowModal;
If List.HasChanged then
   GetFertiliserDialogueInfo;
end;

procedure TfrmMain.mnuIrrigationClick(Sender: TObject);
begin
frmIrrigationManagement.ShowModal;
end;

procedure TfrmMain.mnuEnvironmentClick(Sender: TObject);
begin
SetUpEnvironmentDialogue;
frmGenericListDialogue.ShowModal;
If List.HasChanged then
   GetEnvironmentDialogueInfo;
end;

procedure TfrmMain.mnuHarvestandthinningClick(Sender: TObject);
Begin
Repeat
   SetUpHarvestDialogue;
   frmGenericListDialogue.ShowModal;
   If List.HasChanged then
      GetHarvestDialogueInfo;
Until not List.Redraw;
End;

procedure TfrmMain.mnuPestsClick(Sender: TObject);
begin
Repeat
   SetUpPestDialogue;
   frmGenericListDialogue.ShowModal;
   If List.HasChanged then
      GetPestDialogueInfo;
Until not List.Redraw;
end;

procedure TfrmMain.mnuFireClick(Sender: TObject);
begin
SetUpFireDialogue;
frmGenericListDialogue.ShowModal;
If List.HasChanged then
   GetFireDialogueInfo;
end;

procedure TfrmMain.mnuPloughingClick(Sender: TObject);
begin
SetUpPloughDialogue;
frmGenericListDialogue.ShowModal;
If List.HasChanged then
   GetPloughDialogueInfo;
end;

procedure TfrmMain.mnuPlantcarbonClick(Sender: TObject);
begin
frmPlantCarbonDisplay.ShowModal;
end;

procedure TfrmMain.mnuPlantnitrogenClick(Sender: TObject);
begin
frmPlantNitrogenDisplay.ShowModal;
end;

procedure TfrmMain.mnuSoilorganicmatterClick(Sender: TObject);
begin
frmSoilOrganicMatterDisplay.ShowModal;
end;

procedure TfrmMain.mnuWeatherdataClick(Sender: TObject);
begin
frmWeatherDataDisplay.ShowModal;
end;

procedure TfrmMain.mnuSaveplantinfoClick(Sender: TObject);
begin
frmSavePlantInfo.ShowModal;
end;

procedure TfrmMain.mnuSavesoilInfoClick(Sender: TObject);
begin
frmSaveSoilInfo.ShowModal;
end;

procedure TfrmMain.mnuSaveweatherinfoClick(Sender: TObject);
begin
frmSaveWeatherInfo.ShowModal;
end;

procedure TfrmMain.mnuStartClick(Sender: TObject);
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

procedure TfrmMain.mnuMultipleRunsClick(Sender: TObject);
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

procedure TfrmMain.mnuBatchmodeClick(Sender: TObject);
var iFertilise, OldnEnviron, OldEnvrionTimes: integer;
    FertiliseAmount, OldIrrig, OldCO2, OldTemp, OldRain, OldFertility, OldLeafN: Real48;
    OldIrrigOn: Boolean;
    OldFertiliseAmount: array[0..MaxFertiliseEvents] of real48;

Begin
Control.TSoilFound := false;
Control.BatchMode := false;
Control.EquilMode := false;
Control.BatchCalcs := -1;
frmBatchParameters.ShowModal;
If Control.AgreeOK and Control.BatchMode then
      Begin
      frmFileIO.GetParameterFile(Control.BatchFile, EXT_BATCH);
      { Start batch of simulations }
      Control.InitGenerated := true;
      Control.BatchCount := 1;
      Control.EndBatch := false;
      frmFileIO.GetParameterFile(Control.PoolFile, EXT_INITIAL);
      If Control.ResetPlantPools then
         ResetPlantPools;
      AdjustFertility;
      ControlRun;
      OldFertility := Parameter.FertilityAdjust;
      OldLeafN := Parameter.ConstantLeafNValue;
      OldIrrigOn := Event.Irrigate; Event.Irrigate := true;
      OldIrrig := Event.IrrigationAmount;
      For iFertilise := 1 to Event.nFertilisations do
          OldFertiliseAmount[iFertilise] := Event.FertiliseAmount[iFertilise];
      OldCO2 := Event.CO2[1];
      OldTemp := Event.Temperature[1];
      OldRain := Event.Rainfall[1];
      OldnEnviron := Event.nEnvironments;
      OldEnvrionTimes := Event.EnvironmentTimes[1];
      Event.nEnvironments := 1;
      Event.EnvironmentTimes[1] := 1;
      While ((Control.BatchCount <= Control.BatchCalcs) or (Control.BatchCalcs < 0))
            and (not Control.EndBatch) do
          Begin
          frmFileIO.GetParameterFile(Control.PoolFile, EXT_INITIAL);
          If Control.ResetPlantPools then
             ResetPlantPools;
          if not eof(Control.BatchName) then Read (Control.BatchName, Event.IrrigationAmount);
          if not eof(Control.BatchName) then Read (Control.BatchName, FertiliseAmount);
          For iFertilise := 1 to Event.nFertilisations do
              Event.FertiliseAmount[iFertilise] := FertiliseAmount;
          if not eof(Control.BatchName) then Read (Control.BatchName, Event.CO2[1]);
          if not eof(Control.BatchName) then Read (Control.BatchName, Event.Temperature[1]);
          if not eof(Control.BatchName) then Read (Control.BatchName, Event.Rainfall[1]);
          if not eof(Control.BatchName) then Read (Control.BatchName, Parameter.FertilityAdjust);
          if not eof(Control.BatchName) then Readln (Control.BatchName, Parameter.ConstantLeafNValue);
          If Control.OutputFileOpen then
             writeln (Control.CenwFileOut, Event.IrrigationAmount:8:3, FertiliseAmount:9:1,
                      Event.CO2[1]:9:1, Event.Temperature[1]:9:2, Event.Rainfall[1]:9:2,
                      Parameter.FertilityAdjust:9:2, Parameter.ConstantLeafNValue:9:3);
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
      Event.Irrigate := OldIrrigOn;
      Event.IrrigationAmount := OldIrrig;
      For iFertilise := 1 to Event.nFertilisations do
          Event.FertiliseAmount[iFertilise] := OldFertiliseAmount[iFertilise];
      Event.CO2[1] := OldCO2;
      Event.Temperature[1] := OldTemp;
      Event.Rainfall[1] := OldRain;
      Event.nEnvironments := OldnEnviron;
      Event.EnvironmentTimes[1] := OldEnvrionTimes;
      Parameter.FertilityAdjust := OldFertility;
      Parameter.ConstantLeafNValue := OldLeafN;
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

Type InitialDataType = Record
                       Year, Month, Day, Age: Integer;
                       Stocking, Height, dbh: Real48;
                       Sapwood, HeartWood, CoarseRoot, FineRoot, Leaves, Reserves,
                       Branches, Bark, Pollen, Fruit, Soluble: TElements;
                       Soilwater: SoilWaterType;
                       Inert: SoilElements;
                       End;

var Delta, LastConverg1, SOMSum, SearchRatio, OldBiolNFix, OldNLoss: Real48;
    iLayer, nLayers: Integer;
    GetOut, Abort: Boolean;
    E: ElementsUsed;
    LastStruct, LastFineWOod, LastCoarseWood, LastActive, LastSlow, LastResistant,
    SignStruct, SignFineWOod, SignCoarseWood, SignActive, SignSlow, SignResistant: SoilElements;
    FlipStruct, FlipFineWOod, FlipCoarseWood, FlipActive, FlipSlow, FlipResistant: Array[0..MaxSoilLAyers, C..N] of Boolean;
    InitialData: InitialDataType;

    Procedure AdjustSOMPools (OldPool: Real48; var NewPool: Real48; Flip: Boolean; Speed: Real48);
    Begin
       If (OldPool > 0) and (NewPool > 0) then
          Begin
          If Flip <> true then
             NewPool := NewPool * (NewPool / OldPool) * Power(SearchRatio, Delta * Speed)
          Else
             NewPool := NewPool / Power((NewPool / OldPool), 0.5);
          End;
    End; {of Procedure 'AdjustSOMPools'}

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
    For E := C to N do
        For iLayer := 0 to nLayers do
            SoilOrganic.Inert[iLayer, E] := 0;
    End; {of Procedure 'SetInertToZero'}


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
   OldBiolNFix := Parameter.BiolFix;
   OldNLoss := Parameter.NLoss;
   Derived.Equil.GoodCount := 0;
   If Control.AllOneLayer then
      nLayers := 1
   Else
      nLayers := SoilOrganic.nLayers;
   SetInertToZero (nLayers); // For the purpose of finding 'equilibrium' SOM pools,
                             // set Inert C to zero as thsoe pools are not part of the dynamic equilibrium system
   If Control.Equil.DeltaMin < 0.01 then
      Control.Equil.DeltaMin := 0.01;
   If Control.Equil.DeltaMax < Control.Equil.DeltaMin then
      Control.Equil.DeltaMax := Control.Equil.DeltaMin
   Else if Control.Equil.DeltaMax > 10 then
      Control.Equil.DeltaMax := 10;
   If Control.Equil.MaxChangeRatio < 1.01 then
      Control.Equil.MaxChangeRatio := 1.01
   Else if Control.Equil.MaxChangeRatio > 2 then
      Control.Equil.MaxChangeRatio := 2;
   Derived.Equil.ManualAdjust := false;
   Delta := Control.Equil.DeltaMin;
   Derived.Equil.Iterations := 0;
   LastConverg1 := 0;
   CheckLimits(Control.Equil.BoostResistant, 0.1, 100);
   for E := C to N do
     For iLayer := 0 to nLayers do
       Begin
       CheckLimits(SoilOrganic.Slow[iLayer, E], 0 , 1e6);
       LastSlow[iLayer, E] := SoilOrganic.Slow[iLayer, E];
       CheckLimits(SoilOrganic.Resistant[iLayer, E], 0 , 1e6);
       LastResistant[iLayer, E] := SoilOrganic.Resistant[iLayer, E];
       CheckLimits(SoilOrganic.Active[iLayer, E], 0 , 1e6);
       CheckLimits(SoilOrganic.Struct[iLayer, E], 0 , 1e6);
       CheckLimits(SoilOrganic.Metab[iLayer, E], 0 , 1e6);
       CheckLimits(SoilOrganic.FineWood[iLayer, E], 0 , 1e6);
       CheckLimits(SoilOrganic.CoarseWood[iLayer, E], 0 , 1e6);
       end;
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
           Delta := Delta * (1 + Control.Equil.DeltaAdjust / 100);
           If Delta > Control.Equil.DeltaMax then Delta := Control.Equil.DeltaMax;
           End
        Else // We have just passed the target value
           Delta := Control.Equil.DeltaMin;
        if (abs(Derived.Equil.Converg1) <= Control.Equil.Criterion1) and
           (abs(Derived.Equil.converg2) <= Control.Equil.Criterion2) and
           (abs(Derived.Equil.converg3) <= Control.Equil.Criterion3) and
           (Derived.Equil.Iterations > 0) then
           Begin
           Derived.Equil.GoodCount := Derived.Equil.GoodCount + 1;
           If Derived.Equil.GoodCount >= Control.Equil.MaxGoodCount then
              GetOut:=true; {if difference is acceptable then loop ends}
           End
        Else
          Derived.Equil.GoodCount := 0;
        If Control.Equil.EquilParameter = BiolNFix then
           Parameter.BiolFix := Parameter.BiolFix * power(SearchRatio, Delta)
        Else //if Control.Equil.EquilParameter = NFraction then
           parameter.NLoss := Parameter.NLoss / power(SearchRatio, Delta);
        If Derived.Equil.ManualAdjust then
           Begin
           For E := C to N do
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
          For E := C to N do
             For iLayer := 0 to nLayers do
                Begin
                SignFlip(SoilOrganic.CoarseWood[iLayer, E], LastCoarseWood[iLayer, E], FlipCoarseWood[iLayer, E], SignCoarseWood[iLayer, E]);
                SignFlip(SoilOrganic.FineWood[iLayer, E], LastFineWood[iLayer, E], FlipFineWood[iLayer, E], SignFineWood[iLayer, E]);
                SignFlip(SoilOrganic.Struct[iLayer, E], LastStruct[iLayer, E], FlipStruct[iLayer, E], SignStruct[iLayer, E]);
                SignFlip(SoilOrganic.Active[iLayer, E], LastActive[iLayer, E], FlipActive[iLayer, E], SignActive[iLayer, E]);
                SignFlip(SoilOrganic.Slow[iLayer, E], LastSlow[iLayer, E], FlipSlow[iLayer, E], SignSlow[iLayer, E]);
                SignFlip(SoilOrganic.Resistant[iLayer, E], LastResistant[iLayer, E], FlipResistant[iLayer, E], SignResistant[iLayer, E]);
                AdjustSOMPools (LastCoarseWood[iLayer, E], SoilOrganic.CoarseWood[iLayer, E], FlipCoarseWood[iLayer, E], 1);
                AdjustSOMPools (LastFineWood[iLayer, E], SoilOrganic.FineWood[iLayer, E], FlipFineWood[iLayer, E], 1);
                AdjustSOMPools (LastStruct[iLayer, E], SoilOrganic.Struct[iLayer, E], FlipStruct[iLayer, E], 1);
                AdjustSOMPools (LastActive[iLayer, E], SoilOrganic.Active[iLayer, E], FlipActive[iLayer, E], 1);
                AdjustSOMPools (LastSlow[iLayer, E], SoilOrganic.Slow[iLayer, E], FlipSlow[iLayer, E], 1);
                AdjustSOMPools (LastResistant[iLayer, E], SoilOrganic.Resistant[iLayer, E], FlipResistant[iLayer, E], Control.Equil.BoostResistant);
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
        For E := C to N do
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

procedure TfrmMain.mnuLegendClick(Sender: TObject);
begin
// toggle the legend
if mnuLegend.Checked then
   // turn off the legend
   TfrmGraph(MDIChildren[0]).chtGraph.Legend.Visible := false
Else
   // turn on the legend
   TfrmGraph(MDIChildren[0]).chtGraph.Legend.Visible := true;
end;

procedure TfrmMain.actnShowLegendUpdate(Sender: TObject);
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

procedure TfrmMain.mnuLegendwithrangeClick(Sender: TObject);
var
  iCount: integer;
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

procedure TfrmMain.actnShowRangesUpdate(Sender: TObject);
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

procedure TfrmMain.mnuLicenceagreementClick(Sender: TObject);
begin
  // view licence agreement
  frmAgreement.ShowModal;
end;

procedure TfrmMain.mnuTileClick(Sender: TObject);
begin
  // tile the open windows
  Tile;
end;

procedure TfrmMain.mnuCascadeClick(Sender: TObject);
begin
  // cascade the open windows
  Cascade;
end;

procedure TfrmMain.mnuArrangeAllClick(Sender: TObject);
begin
  // arrange all open windows
  ArrangeIcons;
end;

procedure TfrmMain.mnuContentsClick(Sender: TObject);
begin
  // show the help contents
  WinHelp(Handle, PChar(Application.HelpFile), Help_Index, 0);
end;

procedure TfrmMain.mnuHowtoUseHelpClick(Sender: TObject);
begin
  // show how to use help
  WinHelp(Handle, PChar(Application.HelpFile), HELP_HELPONHELP, 0);
end;

procedure TfrmMain.mnuAboutClick(Sender: TObject);
begin
  // show the About CenW box
  frmAbout.ShowModal;
end;

procedure TfrmMain.FormCreate(Sender: TObject);

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
  ReadCenw ('CenwDef.df!', Control.ProjectFile);
end;

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

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckForSave;
end;

procedure TfrmMain.FormShow(Sender: TObject);
var DateTime : TDateTime;
    st: String;
    SecondsWaited: Real48;
Const WaitSeconds = 1;

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
actIncludeDeltaExecute(Sender);
End;

procedure TfrmMain.actIncludeDeltaExecute(Sender: TObject);
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

procedure TfrmMain.BackUpFilesClick(Sender: TObject);
var ProjectBkp, PlantBkp, SiteBkp: FileNameType;
    DateTime: String;
    CharPos, i: Integer;

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

procedure TfrmMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
IntroImage.Visible := false;
end;

end.
