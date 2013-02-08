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
     Function SatHumidity (T: real48): real48;
     Function InverseSatHumidity (VP: real48): real48;

implementation

uses Math;

Function SatHumidity (T: real48): real48;
Begin
SatHumidity := 6.1078 * exp(17.269 * T / (T + 237.3));
End; {of Function 'SatHumidity'}

Function InverseSatHumidity (VP: real48): real48;
Const ln61078 = 1.809566643;     // ln(6.1078)
Begin
if VP > 0 then
   InverseSatHumidity := 237.3 * (ln(VP) - ln61078) / (17.269 - (ln(VP) - ln61078))
Else
   InverseSatHumidity := -50;            // safeguard if asking vapour pressure is 0 or negative
End; {of Function 'InverseSatHumidity'}

end.

