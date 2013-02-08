{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : Project modeule                                  =
  =                                                              =
  =             Routines to set up and initialise all units      =
  ================================================================
  = File      : CenW.PAS                                         =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

program CenW40;

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
  untFileReadWeather in 'untFileReadWeather.pas' {frmFileReadWeather},
  untSiteParameters in 'untSiteParameters.pas' {frmSiteParameters},
  untStandParameters in 'untStandParameters.pas' {frmStandParameters},
  untHelpConsts in 'untHelpConsts.pas',
  untIrrigationManagement in 'untIrrigationManagement.pas' {frmIrrigationManagement},
  untMultipleRunParameters in 'untMultipleRunParameters.pas' {frmMultipleRunParameters},
  untRun in 'untRun.pas',
  untDiskOut in 'untDiskOut.pas',
  untLoadSaveProject in 'untLoadSaveProject.PAS',
  untLoadSavePlant in 'untLoadSavePlant.pas',
  untLoadSaveSite in 'untLoadSaveSite.pas',
  untLoadSaveInitial in 'untLoadSaveInitial.pas',
  untLoadSaveCenW in 'untLoadSaveCenW.pas',
  untDefaults in 'untDefaults.PAS',
  untSimulate in 'untSimulate.pas',
  untSimsoil in 'untSimsoil.pas',
  untRunOptions in 'untRunOptions.PAS',
  untTrigValidation in 'untTrigValidation.pas',
  untFieldValidation in 'untFieldValidation.pas',
  untPowerValidation in 'untPowerValidation.pas',
  untSathumValidation in 'untSathumValidation.pas',
  untDivideValidation in 'untDivideValidation.pas',
  untGraph in 'untGraph.pas' {frmGraph},
  untProgress in 'untProgress.pas' {frmProgress},
  untEquilProgress in 'untEquilProgress.pas' {frmEquilProgress},
  untEquilAdjust in 'untEquilAdjust.pas' {frmEquilAdjust},
  untAgreement in 'untAgreement.pas' {frmAgreement},
  untFileIO in 'untFileIO.pas' {frmFileIO},
  SysUtils,
  untNotices in 'untNotices.pas' {frmNotices},
  untBatchParameters in 'untBatchParameters.pas' {frmBatchParameters},
  untEquilParameters in 'untEquilParameters.pas' {frmEquilParameters},
  untEquilFinished in 'untEquilFinished.pas' {frmEquilFinished},
  untGenericDisplay in 'untGenericDisplay.pas' {frmGenericDisplay},
  untBatchList in 'untBatchList.pas' {frmBatchList},
  untSpatialParameters in 'untSpatialParameters.pas' {frmSpatialParameters},
  untSpatialProgress in 'untSpatialProgress.pas' {frmSpatialProgress},
  untGenericListColumns in 'untGenericListColumns.pas' {frmGenericListColumns},
  untGenericSave in 'untGenericSave.pas' {frmGenericSave},
  untMiscellaneous in 'untMiscellaneous.PAS',
  untWeedParameters in 'untWeedParameters.pas' {frmWeedParameters},
  untSensSelect in 'untSensSelect.pas' {frmSensSelect};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'CenW 4.0';
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
  Application.CreateForm(TfrmGenericDisplay, frmGenericDisplay);
  Application.CreateForm(TfrmFileReadWeather, frmFileReadWeather);
  Application.CreateForm(TfrmSiteParameters, frmSiteParameters);
  Application.CreateForm(TfrmStandParameters, frmStandParameters);
  Application.CreateForm(TfrmIrrigationManagement, frmIrrigationManagement);
  Application.CreateForm(TfrmProgress, frmProgress);
  Application.CreateForm(TfrmEquilProgress, frmEquilProgress);
  Application.CreateForm(TfrmAgreement, frmAgreement);
  Application.CreateForm(TfrmFileIO, frmFileIO);
  Application.CreateForm(TfrmBatchParameters, frmBatchParameters);
  Application.CreateForm(TfrmEquilParameters, frmEquilParameters);
  Application.CreateForm(TfrmEquilFinished, frmEquilFinished);
  Application.CreateForm(TfrmGenericDisplay, frmGenericDisplay);
  Application.CreateForm(TfrmGenericSave, frmGenericSave);
  Application.CreateForm(TfrmSpatialParameters, frmSpatialParameters);
  Application.CreateForm(TfrmSpatialProgress, frmSpatialProgress);
  Application.CreateForm(TfrmGenericListColumns, frmGenericListColumns);
  Application.CreateForm(TfrmBatchList, frmBatchList);
  Application.CreateForm(TfrmWeedParameters, frmWeedParameters);
  Application.CreateForm(TfrmSensSelect, frmSensSelect);
  Application.Run;
end.
