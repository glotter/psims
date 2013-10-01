{ ================================================================
  = Project   : Cenw                                             =
  ================================================================
  = Module    : TfrmControlParameters                            =
  =                                                              =
  =             Interface routine for users to control some key  =
  =             parameters of program execution.                 =
  ================================================================
  = File      : untControlParameters.PAS                         =
  =                                                              =
  = Version   : 4.1                                              =
  ================================================================ }

unit untControlParameters;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TfrmControlParameters = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    grpLength: TGroupBox;
    lblYears: TLabel;
    lblMonths: TLabel;
    lblDays: TLabel;
    grpDisplayNum: TGroupBox;
    grpClimate: TGroupBox;
    edtClimate: TEdit;
    btnClimate: TButton;
    grpOutput: TGroupBox;
    edtOutput: TEdit;
    btnOutput: TButton;
    grpPlant: TGroupBox;
    edtPlant: TEdit;
    btnPlant: TButton;
    grpSite: TGroupBox;
    edtSite: TEdit;
    btnSite: TButton;
    grpInitial: TGroupBox;
    edtInitial: TEdit;
    btnInitial: TButton;
    grpTitle: TGroupBox;
    edtTitle: TEdit;
    rgCalculation: TRadioGroup;
    rgClimate: TRadioGroup;
    dlgOpenSite: TOpenDialog;
    dlgOpenInitial: TOpenDialog;
    dlgOpenOutput: TOpenDialog;
    dlgOpenClimate: TOpenDialog;
    grpProjectName: TGroupBox;
    edtProjectFile: TEdit;
    edtResetPlantPools: TCheckBox;
    chkIsotopes: TCheckBox;
    dlgOpenPlant: TOpenDialog;
    edtYears: TEdit;
    edtMonths: TEdit;
    edtDays: TEdit;
    edtDisplayNum: TEdit;
    edtDiskNum: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    chkIncludeP: TCheckBox;
    chkFlags: TCheckBox;
    chkIncludeWeeds: TCheckBox;
    rgDateSystem: TRadioGroup;
    Label3: TLabel;
    edtDateSeperator: TEdit;
    chkStartingDate: TCheckBox;
    lblStartingDay: TLabel;
    edtStartingDay: TEdit;
    lblStartingMonth: TLabel;
    edtStartingMonth: TEdit;
    lblStartingYear: TLabel;
    edtStartingYear: TEdit;
    Procedure FormShow(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
    Procedure btnClimateClick(Sender: TObject);
    Procedure btnOutputClick(Sender: TObject);
    Procedure btnPlantClick(Sender: TObject);
    Procedure btnSiteClick(Sender: TObject);
    Procedure btnInitialClick(Sender: TObject);
    Procedure edtnCohortsChange(Sender: TObject);
    procedure chkStartingDateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var frmControlParameters: TfrmControlParameters;

implementation

{$R *.DFM}

uses untDeclarations, untFileIO, untMain;

Procedure TfrmControlParameters.FormShow(Sender: TObject);
Begin
// fill form with parameters
frmMain.FillEdit(Sender, edtYears, Control.nYears, 0);
frmMain.FillEdit(Sender, edtMonths, Control.nMonths, 0);
frmMain.FillEdit(Sender, edtDays, Control.nDays, 0);
frmMain.FillEdit(Sender, edtDisplayNum, Control.nDisplays, 0);
frmMain.FillEdit(Sender, edtDiskNum, Control.nDiskOut, 0);
frmMain.FillEdit(Sender, edtStartingDay, Control.StartingDay, 0);
frmMain.FillEdit(Sender, edtStartingMonth, Control.StartingMonth, 0);
frmMain.FillEdit(Sender, edtStartingYear, Control.StartingYear, 0);
if (CompareText(ExtractFilePath(Control.ClimFile), ExtractFilePath(Control.ProjectFile)) = 0) then
   Control.ClimFile := ExtractFileName(Control.ClimFile);
if (CompareText(ExtractFilePath(Control.FileOut), ExtractFilePath(Control.ProjectFile)) = 0) then
   Control.FileOut := ExtractFileName(Control.FileOut);
if (CompareText(ExtractFilePath(Control.PlantFile), ExtractFilePath(Control.ProjectFile)) = 0) then
   Control.PlantFile := ExtractFileName(Control.PlantFile);
if (CompareText(ExtractFilePath(Control.SiteFile), ExtractFilePath(Control.ProjectFile)) = 0) then
   Control.SiteFile := ExtractFileName(Control.SiteFile);
if (CompareText(ExtractFilePath(Control.PoolFile), ExtractFilePath(Control.ProjectFile)) = 0) then
   Control.PoolFile := ExtractFileName(Control.PoolFile);
edtClimate.Text := Control.ClimFile;
edtOutput.Text := Control.FileOut;
edtPlant.Text := Control.PlantFile;
edtSite.Text := Control.SiteFile;
edtInitial.Text := Control.PoolFile;
edtTitle.Text := Control.ProjectName;
edtProjectFile.Text := Control.ProjectFile;
edtResetPlantPools.Checked := Control.ResetPlantPools;
chkIsotopes.Checked := Control.IncludeIsotopes;
chkIncludeP.Checked := Control.IncludeP;
chkIncludeWeeds.Checked := Control.IncludeWeeds;
chkFlags.Checked := Control.NoticeFlags;
chkStartingDate.Checked := Control.UseStartingDate;
frmControlParameters.chkStartingDateClick(Sender);
if Control.DecayOnly then
    rgCalculation.ItemIndex := 0
else
    rgCalculation.ItemIndex := 1;
edtDateSeperator.Text := Control.DateSeperator;
If Control.DateSystem = 'YYYYMMDD' then
   rgDateSystem.ItemIndex := 1
Else if Control.DateSystem = 'MMDDYYYY' then
   rgDateSystem.ItemIndex := 2
Else if Control.DateSystem = 'YYYYDDMM' then
   rgDateSystem.ItemIndex := 3
Else  //If Control.DateSystem = 'DDMMYYYY' then - also use as default
   rgDateSystem.ItemIndex := 0;
case Control.ClimType of
    'O': rgClimate.ItemIndex := 0;
    'U': rgClimate.ItemIndex := 1;
    'S': rgClimate.ItemIndex := 2;
    end;
end;

Procedure TfrmControlParameters.btnOKClick(Sender: TObject);

  Procedure Get_InfoString(var Datum: InfoTransferRecord; s: string);
  var
    iChar: integer;
  begin
    for iChar := 1 to Length(s) do
      if s[iChar] = ' ' then
         s[iChar] := '_';
    StrPCopy(Datum, s);
  end; {of Procedure 'Get_InfoString'}

begin
// change the paths of the files if it is the same as for the project file
If (CompareText(ExtractFilePath(edtClimate.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
   edtClimate.Text := ExtractFileName(edtClimate.Text);
If (CompareText(ExtractFilePath(edtOutput.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
   edtOutput.Text := ExtractFileName(edtOutput.Text);
If (CompareText(ExtractFilePath(edtPlant.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
   edtPlant.Text := ExtractFileName(edtPlant.Text);
If (CompareText(ExtractFilePath(edtSite.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
   edtSite.Text := ExtractFileName(edtSite.Text);
If (CompareText(ExtractFilePath(edtInitial.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
   edtInitial.Text := ExtractFileName(edtInitial.Text);
frmMain.GetInteger(Sender, edtYears, Control.nYears);
frmMain.GetInteger(Sender, edtMonths, Control.nMonths);
frmMain.GetInteger(Sender, edtDays, Control.nDays);
frmMain.GetInteger(Sender, edtDisplayNum, Control.nDisplays);
frmMain.GetInteger(Sender, edtDiskNum, Control.nDiskOut);
frmMain.GetInteger(Sender, edtStartingDay, Control.StartingDay);
frmMain.GetInteger(Sender, edtStartingMonth, Control.StartingMonth);
frmMain.GetInteger(Sender, edtStartingYear, Control.StartingYear);
If UpperCase(ExtractFileExt(edtClimate.Text)) <> '.CL!' then
   Control.ClimFile := edtClimate.Text + '.CL!'
Else
   Control.ClimFile := edtClimate.Text;
If UpperCase(ExtractFileExt(edtOutput.Text)) <> '.DT!' then
   Control.FileOut := edtOutput.Text + '.DT!'
Else
   Control.FileOut := edtOutput.Text;
If Control.PlantFile <> edtPlant.Text then
   Begin
   If UpperCase(ExtractFileExt(edtPlant.Text)) <> '.PL!' then
      Control.PlantFile := edtPlant.Text + '.PL!'
   Else
      Control.PlantFile := edtPlant.Text;
   If MessageDlg('You have selected a new plant file.' + chr(10) +
                 'Do you want to load new parameters from that file?' + chr(10) +
                 'Otherwise the currently used plant parameters will simply be saved' + chr(10) +
                 'under this newly selected file name upon exiting the program',
                 mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      frmMain.mnuLoadPlantfileClick(Sender)
   Else
      Control.PlantHasChanged := true;
   End;
If Control.SiteFile <> edtSite.Text then
   Begin
   If UpperCase(ExtractFileExt(edtSite.Text)) <> '.ST!' then
      Control.SiteFile := edtSite.Text + '.ST!'
   Else
      Control.SiteFile := edtSite.Text;
   If MessageDlg('You have selected a new site file.' + chr(10) +
                 'Do you want to load new parameters from that file?' + chr(10) +
                 'Otherwise the currently used site parameters will simply be saved' + chr(10) +
                 'under this newly selected file name upon exiting the program',
                 mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      frmMain.mnuLoadSitefileClick(Sender)
   Else
      Control.SiteHasChanged := true;
   End;
If UpperCase(ExtractFileExt(edtInitial.Text)) <> '.IL!' then
   Control.PoolFile := edtInitial.Text + '.IL!'
Else
   Control.PoolFile := edtInitial.Text;
Get_InfoString(Control.ProjectName, edtTitle.Text);
Control.DecayOnly := (rgCalculation.ItemIndex = 0);
Control.ResetPlantPools := edtResetPlantPools.Checked;
Control.IncludeIsotopes := chkIsotopes.Checked;
Control.IncludeP := chkIncludeP.Checked;
Control.IncludeWeeds := chkIncludeWeeds.Checked;
Control.NoticeFlags := chkFlags.Checked;
Control.UseStartingDate := chkStartingDate.Checked;
if Control.UseStartingDate and (not WeatherFile[W_Date]) then
   Begin
   If MessageDlg('You have selected to start the simulation based on dates read in.' + chr(10) +
                 'This is onyl possible if the read weather file option includes reading the date' + chr(10) +
                 'This option has now been switched on!' + chr(10) +
                 'Please, ensure that your weather file contains suitable date information' + chr(10) +
                 'or turn off the option of starting runs at a specified date' + chr(10) +
                 'Do you want to switch on the read-date option?',
                  mtConfirmation, [mbYes, mbNo], 0) =mrYes then
                    WeatherFile[W_Date] := true
                  else
                     Control.UseStartingDate := false;
   End;
Control.DateSeperator := edtDateSeperator.Text;
case rgDateSystem.ItemIndex of
     0: Control.DateSystem := 'DDMMYYYY';
     1: Control.DateSystem := 'YYYYMMDD';
     2: Control.DateSystem := 'MMDDYYYY';
     3: Control.DateSystem := 'YYYYDDMM';
     End;
case rgClimate.ItemIndex of
     0: Control.ClimType := 'O';
     1: Control.ClimType := 'U';
     2: Control.ClimType := 'S';
     End;
Control.ProjectHasChanged := TRUE;
ModalResult := mrOK;
end;

Procedure TfrmControlParameters.btnClimateClick(Sender: TObject);
begin
// select a new climate file
dlgOpenClimate.FileName := edtClimate.Text;
if (dlgOpenClimate.Execute) then
   begin
   edtClimate.Text := dlgOpenClimate.FileName;
   if (CompareText(ExtractFilePath(edtClimate.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtClimate.Text := ExtractFileName(edtClimate.Text);
   SetCurrentDir(Control.ProjectDirectory);
   end;
end;

Procedure TfrmControlParameters.btnOutputClick(Sender: TObject);
begin
// select a new output file
dlgOpenOutput.FileName := edtOutput.Text;
if (dlgOpenOutput.Execute) then
    begin
    edtOutput.Text := dlgOpenOutput.FileName;
    if (CompareText(ExtractFilePath(edtOutput.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
       edtOutput.Text := ExtractFileName(edtOutput.Text);
    SetCurrentDir(Control.ProjectDirectory);
    end;
end;

Procedure TfrmControlParameters.btnPlantClick(Sender: TObject);
begin
// select a new plant file
dlgOpenPlant.FileName := edtPlant.Text;
if (dlgOpenPlant.Execute) then
    begin
    edtPlant.Text := dlgOpenPlant.FileName;
    if (CompareText(ExtractFilePath(edtPlant.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
       edtPlant.Text := ExtractFileName(edtPlant.Text);
    SetCurrentDir(Control.ProjectDirectory);
    end;
end;

Procedure TfrmControlParameters.btnSiteClick(Sender: TObject);
begin
// select a new site file
dlgOpenSite.FileName := edtSite.Text;
if (dlgOpenSite.Execute) then
    begin
    edtSite.Text := dlgOpenSite.FileName;
    if (CompareText(ExtractFilePath(edtSite.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
       edtSite.Text := ExtractFileName(edtSite.Text);
    SetCurrentDir(Control.ProjectDirectory);
    end;
end;

procedure TfrmControlParameters.chkStartingDateClick(Sender: TObject);
begin
if chkStartingDate.Checked then
   Begin
   frmControlParameters.ClientWidth := 720;
   lblStartingDay.Visible := true;
   lblStartingMonth.Visible := true;
   lblStartingYear.Visible := true;
   edtStartingDay.Visible := true;
   edtStartingMonth.Visible := true;
   edtStartingYear.Visible := true;
   End
Else
   Begin
   frmControlParameters.ClientWidth := 580;
   lblStartingDay.Visible := false;
   lblStartingMonth.Visible := false;
   lblStartingYear.Visible := false;
   edtStartingDay.Visible := false;
   edtStartingMonth.Visible := false;
   edtStartingYear.Visible := false;
   End;
end;

Procedure TfrmControlParameters.btnInitialClick(Sender: TObject);
begin
// select a new initial values file
dlgOpenInitial.FileName := edtInitial.Text;
if (dlgOpenInitial.Execute) then
    begin
    edtInitial.Text := dlgOpenInitial.FileName;
    if (CompareText(ExtractFilePath(edtInitial.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
       edtInitial.Text := ExtractFileName(edtInitial.Text);
    end;
SetCurrentDir(Control.ProjectDirectory);
end;

Procedure TfrmControlParameters.edtnCohortsChange(Sender: TObject);
begin
frmControlParameters.btnOKClick(Sender);
ModalResult := mrNone;
frmControlParameters.FormShow(Sender);
end;

end.