{ ================================================================
  = Project   : CENW                                             =
  ================================================================
  = Modules   : SatHumidity                                      =
  =                                                              =
  =             Routines to calculate saturated vapour pressure  =
  =             at a given temperature (in degrees C).           =
  ================================================================
  = File      : untSatHumValidation.PAS                          =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

unit untSathumValidation;

interface
     {uses Wincrt,
          V_IPower;}
     Function SatHumidity (T: real48): real48;

implementation

uses
  Math;

     Function SatHumidity (T: real48): real48;
        begin
        SatHumidity := 6.1078 * exp(17.269 * T / (T + 237.3));
     end; {of Function 'SatHumidity'}
end.

