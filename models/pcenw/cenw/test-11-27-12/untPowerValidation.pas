{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : Power                                            =
  =                                                              =
  =            A simple function that returns Power = arg^expon. =
  =            It can handle the case where the exponent         =
  =            is positive. It returns '0' if the exponent       =
  =            is very small or negative.                        =
  =            The routine only handles standard cases,          =
  =            and does not handle non-standard situations well. =
  =            It provides only minimal error checking.
  ================================================================
  = File      : untPowerValidation.PAS                           =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

unit untPowerValidation;

interface
{     uses WinCrt; }
Function Power (arg, expon: double): double;
Function Square (arg: double): double;

implementation

Function Power (arg, expon: double): double;
Begin
If arg > 1e-12 then
   Power := exp(expon * ln(arg))
else
   Power := 0;
end; {of Function 'Power'}

Function Square (arg: double): double;
Begin
Square := arg * arg;
End;

end.


