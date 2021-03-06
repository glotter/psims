{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : InvSin                                           =
  =             InvCos                                           =
  =             Tan                                              =
  =             ArcSin                                           =
  =                                                              =
  =            Contains a number of trignonmetric functions that =
  =            are not available as Delphi in-built units        =
  ================================================================
  = File      : untTrigValidation.PAS                            =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

  unit untTrigValidation;


interface

Function InvSin (Rad: double): double;
Function InvCos (Rad: double): double;
Function Tan (Rad: double): double;
Function Arcsin(Rad: double): double;

implementation

Function InvSin (Rad: double): double;   {inverse sin function}
         Begin
         InvSin := ArcTan(Rad / Sqrt(1 - sqr(Rad)));
         End; {of Function 'InvSin'}

Function InvCos (Rad: double): double;   {inverse cos function}
         Begin
         InvCos := 0.5 * pi - ArcTan(Rad / sqrt(1 - sqr(Rad)));
         End; {of Function 'InvCos'}

Function Tan (Rad: double): double;
         Begin
         tan := sin(Rad) / cos(Rad);
         End; {of Function 'Tan'}

Function Arcsin(Rad: double): double;
         Begin
         Arcsin := arctan(Rad / sqrt(1 - sqr(Rad)));
         End; {of Function 'Arcsin'}

end.

