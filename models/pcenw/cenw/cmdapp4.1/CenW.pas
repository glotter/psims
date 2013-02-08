program CenW;

// {$APPTYPE CONSOLE}

// {$R *.res}

uses
  SysUtils,
  untMain,
  untDeclarations in 'untDeclarations.pas',
  untDiskOut in 'untDiskOut.pas',
  untDivideValidation in 'untDivideValidation.pas',
  untFieldValidation in 'untFieldValidation.pas',
  untMiscellaneous in 'untMiscellaneous.PAS',
  untPowerValidation in 'untPowerValidation.pas',
  untRun in 'untRun.pas',
  untSathumValidation in 'untSathumValidation.pas',
  untSimsoil in 'untSimsoil.pas',
  untSimulate in 'untSimulate.pas',
  untTrigValidation in 'untTrigValidation.pas';

var
  test : string;

begin

   Initialise;
   WriteLn('ret from Initialise');
   writeln('Calling AppStart');
   AppStart;
   writeln('Returned from AppStart');
   
   {
   WriteLn('Press enter to start');
   ReadLn(test);
   Initialise;
   WriteLn('ret from Initialise');
   ControlRun();
   WriteLn('ret from ControlRun');
   EndRun();
   WriteLn('ret from EndRun');
   WriteLn('Press enter to exit');
   ReadLn(test);
   }
end.
