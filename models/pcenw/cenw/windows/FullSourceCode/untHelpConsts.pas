{ ================================================================
  = Project   : Cenw                                             =
  ================================================================
  = Module    : Help constant declaration                        =
  ================================================================
  = File      : untHelpConsts.PAS                                =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

Unit untHelpConsts;

Interface

Const
   { HelpID's }
   idm_File       = 1000;
   idm_Simulate   = 2000;
   idm_Parameters = 3000;
   idm_Help       = 4000;
   idm_View       = 5000;
   idm_Disk       = 6000;
   idm_Screen     = 7000;
   idm_Management = 8000;
   idm_Initial    = 9000;

   idm_Quit        = 1100;
   idm_Load        = 1200;
   idm_Save        = 1300;
   idm_LoadPlFile  = 1210;
   idm_SavePlFile  = 1310;
   idm_LoadSiteFile= 1220;
   idm_SaveSiteFile= 1320;
   idm_LoadInitial = 1400;
   idm_SaveInitial = 1500;
   idm_WeatherFile = 1600;

   idm_Start        = 2100;
   idm_Sensitivity  = 2200;
   idm_Sensitivity2 = 2400;
   idm_Sensitivity3 = 2500;
   idm_Geography    = 2600;
   idm_Batch        = 2700;
   idm_Multiple     = 2300;
   idm_Equilibrium  = 2800;

   idm_ControlPars    = 3400;
   idm_SitePars       = 3500;
   idm_StandPars      = 3200;
   idm_PhotoPars      = 3800;
   idm_PhenolPars     = 3900;
   idm_SoilWater      = 3700;
   idm_SoilLitter     = 3750;
   idm_SoilPara       = 3100;
   idm_AllocPars      = 3300;
   idm_WeatherPars    = 3600;

   idm_CarbonPools     = 9100;
   idm_NitrogenPools   = 9200;
   idm_PhosphorusPools = 9300;
   idm_Miscellaneous   = 9400;
   idm_ResetPools      = 9500;
   idm_DeltaPools      = 9600;

   idm_Fertilisation  = 8100;
   idm_Irrigation     = 8200;
   idm_Environment    = 8300;
   idm_Harvest        = 8400;
   idm_Pest           = 8500;
   idm_Fire           = 8600;

   idm_SavePlant       = 6100;
   idm_SaveSOM         = 6200;
   idm_SaveWeather     = 6300;

   idm_ScreenCarbon     = 7100;
   idm_ScreenNitrogen   = 7200;
   idm_ScreenPhosphorus = 7300;
   idm_ScreenSOM        = 7400;
   idm_ScreenWeather    = 7500;

   idm_About       = 4100;
   idm_HelpIndex   = 4001;
   idm_HelpOnHelp  = 4002;

   idm_Legend1     = 5100;
   idm_Legend2     = 5200;
   idm_Agree       = 5300;

Implementation
{Nothing to do}

End.
