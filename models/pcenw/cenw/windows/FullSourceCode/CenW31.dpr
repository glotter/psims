{ ================================================================
  = Project   : CENW                                             =
  ================================================================
  = Modules   : Project modeule                                  =
  =                                                              =
  =             Routines to set up and initialise all units      =
  ================================================================
  = File      : CenW.PAS                                         =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

program CenW31;

uses
  Forms,
  untDeclarations in 'untDeclarations.pas',
  untMain in 'untMain.pas' {frmMain},
  untAbout in 'untAbout.pas' {frmAbout},
  untPools in 'untPools.pas' {frmPools},
  untC13Pools in 'untC13Pools.pas' {frmC13Pools},
  untInitialPools in 'untInitialPools.pas' {frmInitialPools},
  untControlParameters in 'untControlParameters.pas' {frmControlParameters},
  untDecompositionParameters in 'untDecompositionParameters.pas' {frmDecompositionParameters},
  untGenericListDialogue in 'untGenericListDialogue.pas' {frmGenericListDialogue},
  untAllocationParameters in 'untAllocationParameters.pas' {frmAllocationParameters},
  untWeatherParameters in 'untWeatherParameters.pas' {frmWeatherParameters},
  untPhotoSyntheticParameters in 'untPhotoSyntheticParameters.pas' {frmPhotoSyntheticParameters},
  untPlantCarbonDisplay in 'untPlantCarbonDisplay.pas' {frmPlantCarbonDisplay},
  untPlantNitrogenDisplay in 'untPlantNitrogenDisplay.pas' {frmPlantNitrogenDisplay},
  untFileReadWeather in 'untFileReadWeather.pas' {frmFileReadWeather},
  untSaveSoilInfo in 'untSaveSoilInfo.pas' {frmSaveSoilInfo},
  untSaveWeatherInfo in 'untSaveWeatherInfo.pas' {frmSaveWeatherInfo},
  untSiteParameters in 'untSiteParameters.pas' {frmSiteParameters},
  untSoilOrganicMatterDisplay in 'untSoilOrganicMatterDisplay.pas' {frmSoilOrganicMatterDisplay},
  untStandParameters in 'untStandParameters.pas' {frmStandParameters},
  untWeatherDataDisplay in 'untWeatherDataDisplay.pas' {frmWeatherDataDisplay},
  untHelpConsts in 'untHelpConsts.pas',
  untIrrigationManagement in 'untIrrigationManagement.pas' {frmIrrigationManagement},
  untMultipleRunParameters in 'untMultipleRunParameters.pas' {frmMultipleRunParameters},
  untRun in 'untRun.pas',
  untDiskOut in 'untDiskOut.pas',
  untFileIO3 in 'untFileIO3.PAS',
  untDefaults in 'untDefaults.PAS',
  untSimulate in 'untSimulate.pas',
  untSimsoil in 'untSimsoil.pas',
  untMiscellaneous in 'untMiscellaneous.PAS',
  untTrigValidation in 'untTrigValidation.pas',
  untFieldValidation in 'untFieldValidation.pas',
  untPowerValidation in 'untPowerValidation.pas',
  untSathumValidation in 'untSathumValidation.pas',
  untDivideValidation in 'untDivideValidation.pas',
  untGraph in 'untGraph.pas' {frmGraph},
  untSavePlantInfo in 'untSavePlantInfo.pas' {frmSavePlantInfo},
  untProgress in 'untProgress.pas' {frmProgress},
  untEquilProgress in 'untEquilProgress.pas' {frmEquilProgress},
  untEquilAdjust in 'untEquilAdjust.pas' {frmEquilAdjust},
  untAgreement in 'untAgreement.pas' {frmAgreement},
  untFileIO in 'untFileIO.pas' {frmFileIO},
  SysUtils,
  untNotices in 'untNotices.pas' {frmNotices},
  untBatchParameters in 'untBatchParameters.pas' {frmBatchParameters},
  untEquilParameters in 'untEquilParameters.pas' {frmEquilParameters},
  untEquilFinished in 'untEquilFinished.pas' {frmEquilFinished};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'CenW 3.1';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.CreateForm(TfrmPools, frmPools);
  Application.CreateForm(TfrmC13Pools, frmC13Pools);
  Application.CreateForm(TfrmInitialPools, frmInitialPools);
  Application.CreateForm(TfrmMultipleRunParameters, frmMultipleRunParameters);
  Application.CreateForm(TfrmControlParameters, frmControlParameters);
  Application.CreateForm(TfrmDecompositionParameters, frmDecompositionParameters);
  Application.CreateForm(TfrmGenericListDialogue, frmGenericListDialogue);
  Application.CreateForm(TfrmAllocationParameters, frmAllocationParameters);
  Application.CreateForm(TfrmWeatherParameters, frmWeatherParameters);
  Application.CreateForm(TfrmPhotoSyntheticParameters, frmPhotoSyntheticParameters);
  Application.CreateForm(TfrmPlantCarbonDisplay, frmPlantCarbonDisplay);
  Application.CreateForm(TfrmPlantNitrogenDisplay, frmPlantNitrogenDisplay);
  Application.CreateForm(TfrmFileReadWeather, frmFileReadWeather);
  Application.CreateForm(TfrmSaveSoilInfo, frmSaveSoilInfo);
  Application.CreateForm(TfrmSaveWeatherInfo, frmSaveWeatherInfo);
  Application.CreateForm(TfrmSiteParameters, frmSiteParameters);
  Application.CreateForm(TfrmSoilOrganicMatterDisplay, frmSoilOrganicMatterDisplay);
  Application.CreateForm(TfrmStandParameters, frmStandParameters);
  Application.CreateForm(TfrmWeatherDataDisplay, frmWeatherDataDisplay);
  Application.CreateForm(TfrmIrrigationManagement, frmIrrigationManagement);
  Application.CreateForm(TfrmSavePlantInfo, frmSavePlantInfo);
  Application.CreateForm(TfrmProgress, frmProgress);
  Application.CreateForm(TfrmEquilProgress, frmEquilProgress);
  Application.CreateForm(TfrmAgreement, frmAgreement);
  Application.CreateForm(TfrmFileIO, frmFileIO);
  Application.CreateForm(TfrmBatchParameters, frmBatchParameters);
  Application.CreateForm(TfrmEquilParameters, frmEquilParameters);
  Application.CreateForm(TfrmEquilFinished, frmEquilFinished);
  Application.Run;
end.
