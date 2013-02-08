{ ================================================================
  = Project   : CENW                                             =
  ================================================================
  = Module    : Divide                                           =
  =                                                              =
  =             A simple routines to prevent program crashes     =
  =             when illegal divisions are performed.            =
  ================================================================
  = File      : untDivideValidation.PAS                          =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

unit untDivideValidation;

{ = Author    : Miko Kirschbaum                                    =
  =             CSIRO Division of Forestry                         =
  =             PO Box 4008                                        =
  =             Queen Victoria Terrace                             =
  =             Canberra  ACT 2600  Australia                      =
  =             Version 1.0.0                                      =}

interface

Function Divide (a, b: real48): real48;  {The purpose of this procedure is to
                                          act as a safeguard in divisions to ensure
                                          that program runs are not aborted if invalid
                                          divisions are to be performed.
                                          It divides a by b, and returns the result as 'Divide'.
                                          This routine should not crash even if b = 0,
                                          or if the result of a/b would be out of range for
                                          normal calculations with real numbers.}
implementation

     Function Divide (a, b: real48): real48;
     Const LogRatio = 2.302585; {ratio of ln(a) /log10(a)}
     var   d: real48;
        begin
        if b = 0 then
           Begin
           if a = 0 then
              Divide := 1
           Else if a > 0 then
              Divide := 1E12
           Else {if a < 0}
              Divide := -1E12
           End {if b = 0}
        Else if a = 0 then
           Divide := 0
        Else
           Begin
           if abs(ln(abs(a)) - ln(abs(b))) > (LogRatio * 38) then  {because real nos are 2.9e-39..1.7e38}
              Begin
              if abs(a) > abs(b) then
                 d := 1e38
              else
                 d := 1e-38;
              if (a * b) < 0 then
                 d := -d;
              Divide := d;
              End
           else
              Divide := a / b;
           end; {of else statement}
        end; {of Function 'Divide'}
end.