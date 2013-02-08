{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : ReadCenW                                         =
  =             SaveCenW                                         =
  =             BatchParameters                                  =
  =             SpatialParameters                                =
  =                                                              =
  =             Routines to save files with model parameters.    =
  =             It also includes routines to read and save a     =
  =             default file that tells the program where the    =
  =             latest default project is to be found.           =
  ================================================================
  = File      : untLoadSaveCenW.PAS                              =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

Unit untLoadSaveCenW;

{$V-}

INTERFACE

Uses
  SysUtils, Messages, Dialogs, untFieldValidation, untDeclarations, untDefaults,
  untLoadSaveInitial;

Procedure ReadCenw (DefaultName: string; var FileName: FileNameType);
Procedure SaveCenw (DefaultName: string; FileName: FileNameType);
Procedure BatchParameters(Name : string);
Procedure SpatialParameters(Name : string);

IMPLEMENTATION

Procedure ReadCenW (DefaultName: string; var FileName: FileNameType);
var FileStrng: text;
    Cha: Char;
    MessageToUser: String;

Begin
If FileExists(DefaultName) then
   Begin
   Assign (FileStrng, DefaultName);
   Reset (FileStrng);
   FileName := '                                   ';
   Readln (FileStrng, FileName);
   Readln (FileStrng, Control.Version);
   Readln (FileStrng, Cha);
   If Cha = 'T' then
      Control.AgreeOK := true
   Else
      Control.AgreeOK := false;
   Close (FileStrng);
   Control.OldDirectory := GetCurrentDir;
   Control.ProjectDirectory := ExtractFilePath(FileName);
   SetCurrentDir(ExtractFilePath(FileName));
   End
Else
   Begin
   MessageToUser := 'File not found!' + chr(10) +
           'The project file may be in the wrong subdirectory' + chr(10) +
           'or you may not have created or copied one' + chr(10) +
           'or deleted one that had been created before.' + chr(10) + chr(10) +
           'The program will try to run with default parameters';
   // show the error message
   ShowMessage(MessageToUser);
   Control.Version := '4.0';
   ProjectDefaults;
   SiteDefaults;
   PlantDefaults;
   PoolDefaults;
   Control.ProjectHasChanged := true;
   Control.PlantHasChanged := true;
   Control.SiteHasChanged := true;
   SavePools(Control.PoolFile);
   Control.RunWithDefaults := true;
   Control.OldDirectory := GetCurrentDir;
   Control.ProjectDirectory := Control.OldDirectory;
   End;
End; {of Procedure 'ReadCenw'}

Procedure SaveCenW (DefaultName: string; FileName: FileNameType);
var FileStrng: text;
    ExclamationPos, ErrorNo: Integer;
    NameString: String;
Begin
SetCurrentDir(Control.OldDirectory);
{$I-}
Assign (FileStrng, DefaultName);
Rewrite (FileStrng);
{$I+}
ErrorNo := IOResult;
If ErrorNo <> 0 then
   Control.ErrorSaving := true
Else
   Begin
   Control.ErrorSaving := false;
   ExclamationPos := Pos('!', FileName);
   NameString := Copy(FileName, 1, ExclamationPos);
   Writeln (FileStrng, NameString);
   {Extra safeguard introduced to prevent some rubbish characters being appended to the end
    of the project file name. This solution is awkward, but seems to work.}
   Writeln (FileStrng, Control.Version);
   Writeln (FileStrng, Control.AgreeOK);
   Close (FileStrng);
   End;
End; {of Procedure 'SaveCenw'}

Procedure BatchParameters(Name : string);
Begin
Assign (Control.BatchName, Name);
Reset (Control.BatchName);
Control.BatchFileOpen := true;
End; {of Procedure 'BatchParameters'}

Procedure SpatialParameters(Name : string);
Begin
Assign (Control.SpatialText, Name);
Reset (Control.SpatialText);
End; {of Procedure 'SpatialParameters'}

End.

{ --- end of file untLoadSaveCenW.PAS ------------------------------------------ }
