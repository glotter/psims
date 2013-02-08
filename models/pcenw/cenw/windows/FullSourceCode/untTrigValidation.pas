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

Function InvSin (Rad: real48): Real48;
Function InvCos (Rad: real48): Real48;
Function Tan (Rad: real48): Real48;
Function Arcsin(Rad: real48): real48;

implementation

Function InvSin (Rad: real48): Real48;   {inverse sin function}
         Begin
         InvSin := ArcTan(Rad / Sqrt(1 - sqr(Rad)));
         End; {of Function 'InvSin'}

Function InvCos (Rad: real48): Real48;   {inverse cos function}
         Begin
         InvCos := 0.5 * pi - ArcTan(Rad / sqrt(1 - sqr(Rad)));
         End; {of Function 'InvCos'}

Function Tan (Rad: real48): Real48;
         Begin
         tan := sin(Rad) / cos(Rad);
         End; {of Function 'Tan'}

Function Arcsin(Rad: real48): real48;
         Begin
         Arcsin := arctan(Rad / sqrt(1 - sqr(Rad)));
         End; {of Function 'Arcsin'}

end.

