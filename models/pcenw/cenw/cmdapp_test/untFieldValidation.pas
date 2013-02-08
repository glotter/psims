{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : GetField                                         =
  =                                                              =
  =             Routine to work out the optimum number of        =
  =             digits to display to make best use of available  =
  =             space for output.                                =
  ================================================================
  = File      : untFiledValidation.PAS                           =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

unit untFieldValidation;

interface

Procedure GetField(Param: double; Maxwidth: integer; var Width, Digits: integer);

Implementation

Procedure GetField(Param: double; Maxwidth: integer; var Width, Digits: integer);
Const LogRatio = 2.302585; {ratio of ln(a) /log10(a)}
Var Extraspace: 0..1;
Begin
If Param < 0 then
   Begin
   Param := -Param;
   ExtraSpace := 1;
   End
Else
   ExtraSpace := 0;
If Param = 0 then
   Begin
   Width := MaxWidth;
   Digits := MaxWidth - 2;
   End
Else if (ln(Param) + ExtraSpace) > (LogRatio * MaxWidth) then
   Begin
   Width := trunc(ExtraSpace + ln(Param) / LogRatio);
   Digits := 0;
   End
Else if Param > 1 then
   Begin
   Width := MaxWidth;
   Digits := trunc(MaxWidth - ln(Param) / LogRatio - ExtraSpace - 1);
   End
Else
   Begin
   Width := MaxWidth;
   Digits := Width - 2 - ExtraSpace;
   End;
END; {OF PROCEDURE 'GetField'}

{The purpose of this routine is to calculate field widths of a number to be used in outputting
 that number. For example, if 6 characters are to be output,
 '1000.0232' should be output as '1000.0', whereas '0.0232' should be output as '0.0232'.
 This routine looks at the magnitude of the number, 'Param' to be investigated, the maximum
 field width available, 'MaxWidth', and then returns 'Width', the total width that should be used
 for the output field and 'Digits' the number of digits that are to be printed. In our example,
 Param = 1000.0232, MaxWidth = 6, would be returned with Width = 6, Digits = 1, and
 Param = 0.0232, MaxWidth = 6, would be returned with Width = 6, Digits = 4.
 A width greater than MaxWidth may be returned if Param cannot be displayed within the size set
 by MaxWidth. For example,
 Param = 1000000, MaxWidth = 6, would be returned with Width = 7, Digits = 0.}
 
end.


