{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : SatHumidity                                      =
  =                                                              =
  =             Routines to calculate saturated vapour pressure  =
  =             at a given temperature (in degrees C).           =
  ================================================================
  = File      : untSatHumValidation.PAS                          =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untSathumValidation;

interface
     {uses Wincrt,
          V_IPower;}
     Function SatHumidity (T: double): double;
     Function InverseSatHumidity (VP: double): double;

implementation

uses Math;

Function SatHumidity (T: double): double;
Begin
SatHumidity := 6.1078 * exp(17.269 * T / (T + 237.3));
End; {of Function 'SatHumidity'}

Function InverseSatHumidity (VP: double): double;
Const ln61078 = 1.809566643;     // ln(6.1078)
Begin
InverseSatHumidity := 237.3 * (ln(VP) - ln61078) / (17.269 - (ln(VP) - ln61078));
End; {of Function 'InverseSatHumidity'}

end.

