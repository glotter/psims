{ ================================================================
  = Project   : CENW                                             =
  ================================================================
  = Modules   : GetDefaults                                      =
  =             GetParameterFile                                 =
  =             CorrectFile                                      =
  =             IsFileThere                                      =
  =             SolveProblem                                     =
  =                                                              =
  =             Routine to do some checking of I/O and to        =
  =             control excecution if problems are encountered   =
  ================================================================
  = File      : untFileIO.PAS                                    =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

Unit untFileIO;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Buttons, untDeclarations;

const
  EXT_SITE = '.ST!';
  EXT_PLANT = '.PL!';
  EXT_PROJECT = '.PJ!';
  EXT_INITIAL = '.IL!';
  EXT_CLIMATE = '.CL!';
  EXT_BATCH = '.BT!';

type
  TfrmFileIO = class(TForm)
    btnSearch: TButton;
    LblInfo: TLabel;
    btnDefaults: TButton;
    btnAbort: TButton;
    btnExtract: TButton;
    btnHelp: TButton;
    dlgOpenFile: TOpenDialog;
    procedure btnDefaultsClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
    procedure btnExtractClick(Sender: TObject);
    Procedure GetDefaults (Extension: String);
    Procedure GetParameterFile(var Name: FileNameType; Extension: String);
    Procedure CorrectFile(Name: FileNameType; var CorrectVersion: Boolean);
    Procedure IsFileThere(Name: FileNameType; var FileFound: Boolean);
    Procedure SolveProblem (var Name: FileNameType; Extension: String; var DefaultsLoaded: Boolean; FileFound: Boolean);
  private
    { Private declarations }
    FExtension: string;
    FName: FileNameType;
    FFileType: string;
  public
    { Public declarations }
  end;

var
  frmFileIO: TfrmFileIO;

implementation

{$R *.DFM}

uses
  untDefaults, untFileIO3;

const
  fsPathName  = 79;
  fsDirectory = 67;
  fsFileName  = 8;
  fsExtension = 4;
  // modal result values returned when frmFileIO dialog is closed
  mrDefaultsLoaded = 100;
  mrExtracted = 101;
  mrSearched = 102;

Procedure TfrmFileIO.GetDefaults (Extension: String);
Begin
  If (Extension = EXT_INITIAL) then
    Begin
      PoolDefaults;
      Control.InitHasChanged := true;
      Control.InitGenerated := true;
    End
  Else if (Extension = EXT_SITE) then
    Begin
      SiteDefaults;
      Control.SiteHasChanged := true;
    End
  Else if (Extension = EXT_PLANT) then
    Begin
      PlantDefaults;
      Control.PlantHasChanged := true;
    End
  Else if (Extension = EXT_PROJECT) then
    Begin
      ProjectDefaults;
      Control.ProjectHasChanged := true;
    End;
End; {of Procedure 'GetDefaults'}

Procedure TfrmFileIO.IsFileThere(Name: FileNameType; var FileFound: Boolean);
Begin
  FileFound := FileExists(Name);
End; {of Procedure 'IsFileThere'}

Procedure TfrmFileIO.CorrectFile(Name: FileNameType; var CorrectVersion: Boolean);
Var
  Defp: TextFile;
  Extension: string;
  FileStrng, Version, WhatFile: string;
Begin
  Extension := UpperCase(ExtractFileExt(Name));
  FileStrng := Name;
  AssignFile(defp, FileStrng);
  Reset(defp);
  Readln(defp, Version);
  CloseFile(Defp);
  If Extension = EXT_PROJECT then
     WhatFile := 'Project'
  Else if Extension = EXT_PLANT then
     WhatFile := 'Plant parameters'
  Else if Extension = EXT_SITE then
     WhatFile := 'Site parameters'
  Else if Extension = EXT_INITIAL then
     WhatFile := 'Pools';
  // compare the expected version with the real version, without case sensitivity
  If Extension <> EXT_BATCH then
     CorrectVersion := (CompareText(Control.Version + ' ' + WhatFile, Version) = 0)
  Else
     CorrectVersion := true;
End; {of Procedure 'CorrectFile'}

Procedure TfrmFileIO.SolveProblem (var Name: FileNameType; Extension: String; var DefaultsLoaded: Boolean; FileFound: Boolean);
var
  MessageToUser: String;
  iResult: integer;
Begin
  FExtension := Extension;
  FName := Name;
  If Extension = EXT_PROJECT then
    FFileType := 'PROJECT'
  Else if Extension = EXT_PLANT then
    FFileType := 'PLANT'
  Else if Extension = EXT_SITE then
    FFileType := 'SITE'
  Else if Extension = EXT_BATCH then
    FFileType := 'BATCH'
  Else if Extension = EXT_INITIAL then
    FFileType := 'INITIAL POOLS'
  Else if Extension = EXT_CLIMATE then
    FFileType := 'CLIMATE'
  Else
    FFileType := 'UNIDENTIFIED';
  If FileFound then
    Begin
      MessageToUser := 'Wrong file type!' + chr(10) +
           'The ' + FFileType + ' file may be a wrong type of file' + chr(10) +
           'or it may have been created by an earlier version of CenW';
    End
  Else
    Begin
      MessageToUser := 'File not found!' + chr(10) +
           'The ' + FFileType + ' file may be in the wrong subdirectory' + chr(10) +
           'or you may not have created or copied one' + chr(10) +
           'or deleted one that had been created before';
    End;
  // show the error message
  ShowMessage(MessageToUser);
  // are we already showing the dialog?
  if not(frmFileIO.Visible) then
  begin
    // show the dialog so the user can solve the problem
    iResult := frmFileIO.ShowModal;
    // check the result
    DefaultsLoaded := (iResult = mrDefaultsLoaded);
    if (iResult = mrDefaultsLoaded) or
       (iResult = mrExtracted) or
       (iResult = mrSearched) then
    begin
      Name := FName;
    end else
    begin
      // user didn't solve the problem, so exit
      Application.Terminate;
    end;
  end;
End;

Procedure TfrmFileIO.GetParameterFile(var Name: FileNameType; Extension: String);
var
  FileFound: Boolean;
  CorrectVersion, DefaultsLoaded: Boolean;
Begin
  // check if the file exists
  DefaultsLoaded := false;
  If (Extension = EXT_CLIMATE) and (Control.ClimType <> 'O') then
     FileFound := true  // We don't need the climate file
  Else
     IsFileThere(Name, FileFound);
  // if it does exist, then check for correct version
  If FileFound then
     Begin
     If Extension <> EXT_CLIMATE then
        Begin
        CorrectFile(Name, CorrectVersion);
        // if not correct version, then get user to solve the problem
        If not CorrectVersion then
          SolveProblem (Name, Extension, DefaultsLoaded, FileFound);
        End
     End
  Else {If File not found}
     SolveProblem (Name, Extension, DefaultsLoaded, FileFound);
  // did we load a real file?
  If not DefaultsLoaded then
     Begin
       If Extension = EXT_PROJECT then
         GenProject(Name)
       Else if Extension = EXT_PLANT then
         GenPlant(Name)
       Else if Extension = EXT_SITE then
         GenSite(Name)
       Else if Extension = EXT_BATCH then
         BatchParameters(Name)
       Else if Extension = EXT_INITIAL then
         GenPools(Name);
     End;
End; {of Procedure 'GetParameterFile'}

procedure TfrmFileIO.btnDefaultsClick(Sender: TObject);
begin
  // get the defaults
  If FExtension = EXT_CLIMATE then
     Control.ClimType := 'S'
  Else
     GetDefaults(FExtension);
  // close the dialog with the mrDefaultsLoaded result
  ModalResult := mrDefaultsLoaded;
end;

procedure TfrmFileIO.btnSearchClick(Sender: TObject);
var
  CorrectVersion, FileFound, DefaultsLoaded: Boolean;
begin
  // select a new file
  dlgOpenFile.DefaultExt := '*' + FExtension;
  dlgOpenFile.FileName := '*' + FExtension;
  dlgOpenFile.Filter := FFileType + ' files|*' + FExtension;
  if (dlgOpenFile.Execute) then
    begin
    FName := dlgOpenFile.FileName;
    if (CompareText(ExtractFilePath(FName), ExtractFilePath(Control.ProjectFile)) = 0) then
       FName := ExtractFileName(FName);
    // check for validity of this file
    IsFileThere(FName, FileFound);
    If FileFound and (FExtension <> EXT_CLIMATE) then
         Begin
         CorrectFile(FName, CorrectVersion);
         If not CorrectVersion then
           SolveProblem (FName, FExtension, DefaultsLoaded, FileFound)
         Else
             Begin
             // project has changed since parameter file is different
             Control.ProjectHasChanged := true;
             ModalResult := mrSearched; // the file is valid, so return mrSearched
             End;
         End
    Else If not Filefound then
       SolveProblem (FName, FExtension, DefaultsLoaded, FileFound)
    Else
       ModalResult := mrSearched; // the climate file is valid, so return mrSearched
  end;
end;

procedure TfrmFileIO.btnAbortClick(Sender: TObject);
begin
  // just terminate the application here
  Application.Terminate;
  Close;
  Halt;
end;

procedure TfrmFileIO.btnExtractClick(Sender: TObject);
begin
  GetDefaults(FExtension);
  If FExtension = EXT_PROJECT then
     Begin
     GenProject(FName);
     Control.ProjectHasChanged := true;
     End
  Else if FExtension = EXT_PLANT then
     Begin
     GenPlant(FName);
     Control.PlantHasChanged := true;
     End
  Else if FExtension = EXT_SITE then
     Begin
     GenSite(FName);
     Control.SiteHasChanged := true;
     End
  Else if FExtension = EXT_BATCH then
     BatchParameters(FName)
  Else if FExtension = EXT_CLIMATE then
     Control.ClimType := 'S'
  Else if FExtension = EXT_INITIAL then
     Begin
     GenPools(FName);
     Control.InitHasChanged := true;
     End;           
  // close the dialog with the mrExtracted result
  ModalResult := mrExtracted;
end;

end.

{ --- end of file untFileIO.PAS ------------------------------------------ }

