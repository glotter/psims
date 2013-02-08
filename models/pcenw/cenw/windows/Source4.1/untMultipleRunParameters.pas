{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmMultipleRunParameters                        =
  =                                                              =
  =             This is where the variables are set for doing    =
  =             runs of multiple projects.                       =
  ================================================================
  = File      : untMultipleRunParameters.PAS                     =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

unit untMultipleRunParameters;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TfrmMultipleRunParameters = class(TForm)
    btnRun: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    grpPools: TGroupBox;
    edtPools: TEdit;
    btnInitial: TButton;
    dlgOpenPools: TOpenDialog;
    grpFile1: TGroupBox;
    edtProjectFile1: TEdit;
    btnProjectFile1: TButton;
    dlgOpenProjectFile1: TOpenDialog;
    grpFile2: TGroupBox;
    edtProjectFile2: TEdit;
    dlgOpenProjectFile2: TOpenDialog;
    grpFile3: TGroupBox;
    edtProjectFile3: TEdit;
    btnProjectFile3: TButton;
    dlgOpenProjectFile3: TOpenDialog;
    grpFile4: TGroupBox;
    edtProjectFile4: TEdit;
    btnProjectFile4: TButton;
    dlgOpenProjectFile4: TOpenDialog;
    grpFile5: TGroupBox;
    edtProjectFile5: TEdit;
    btnProjectFile5: TButton;
    dlgOpenProjectFile5: TOpenDialog;
    grpFile6: TGroupBox;
    edtProjectFile6: TEdit;
    btnProjectFile6: TButton;
    dlgOpenProjectFile6: TOpenDialog;
    grpFile7: TGroupBox;
    edtProjectFile7: TEdit;
    btnProjectFile7: TButton;
    dlgOpenProjectFile7: TOpenDialog;
    grpFile8: TGroupBox;
    edtProjectFile8: TEdit;
    btnProjectFile8: TButton;
    dlgOpenProjectFile8: TOpenDialog;
    grpFile9: TGroupBox;
    edtProjectFile9: TEdit;
    btnProjectFile9: TButton;
    dlgOpenProjectFile9: TOpenDialog;
    grpFile10: TGroupBox;
    edtProjectFile10: TEdit;
    btnProjectFile10: TButton;
    dlgOpenProjectFile10: TOpenDialog;
    btnSave_Run: TBitBtn;
    Procedure FormShow(Sender: TObject);
    Procedure btnProjectFile1Click(Sender: TObject);
    Procedure btnPoolsClick(Sender: TObject);
    Procedure btnProjectFile2Click(Sender: TObject);
    Procedure btnProjectFile3Click(Sender: TObject);
    Procedure btnProjectFile4Click(Sender: TObject);
    Procedure btnProjectFile5Click(Sender: TObject);
    Procedure btnProjectFile6Click(Sender: TObject);
    Procedure btnProjectFile7Click(Sender: TObject);
    Procedure btnProjectFile8Click(Sender: TObject);
    Procedure btnProjectFile9Click(Sender: TObject);
    Procedure btnProjectFile10Click(Sender: TObject);
    Procedure btnRunClick(Sender: TObject);
    Procedure btnSaveRunClick(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
    Procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMultipleRunParameters: TfrmMultipleRunParameters;

implementation

{$R *.DFM}

uses
  untDeclarations, untFileIO;

Procedure TfrmMultipleRunParameters.FormShow(Sender: TObject);
begin
  // fill form with parameters
  edtProjectFile1.Text := Control.MultipleRuns[1];
  edtProjectFile2.Text := Control.MultipleRuns[2];
  edtProjectFile3.Text := Control.MultipleRuns[3];
  edtProjectFile4.Text := Control.MultipleRuns[4];
  edtProjectFile5.Text := Control.MultipleRuns[5];
  edtProjectFile6.Text := Control.MultipleRuns[6];
  edtProjectFile7.Text := Control.MultipleRuns[7];
  edtProjectFile8.Text := Control.MultipleRuns[8];
  edtProjectFile9.Text := Control.MultipleRuns[9];
  edtProjectFile10.Text := Control.MultipleRuns[10];
  edtPools.Text := Control.MultipleRunPoolFile;
end;

Procedure TfrmMultipleRunParameters.btnOKClick(Sender: TObject);
var ext: string;
    iProjects, jProjects, LastProject: Integer;

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
    if (CompareText(ExtractFilePath(edtProjectFile1.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtProjectFile1.Text := ExtractFileName(edtProjectFile1.Text);
    if (CompareText(ExtractFilePath(edtProjectFile2.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtProjectFile2.Text := ExtractFileName(edtProjectFile2.Text);
    if (CompareText(ExtractFilePath(edtProjectFile3.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtProjectFile3.Text := ExtractFileName(edtProjectFile3.Text);
    if (CompareText(ExtractFilePath(edtProjectFile4.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtProjectFile4.Text := ExtractFileName(edtProjectFile4.Text);
    if (CompareText(ExtractFilePath(edtProjectFile5.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtProjectFile5.Text := ExtractFileName(edtProjectFile5.Text);
    if (CompareText(ExtractFilePath(edtProjectFile6.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtProjectFile6.Text := ExtractFileName(edtProjectFile6.Text);
    if (CompareText(ExtractFilePath(edtProjectFile7.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtProjectFile7.Text := ExtractFileName(edtProjectFile7.Text);
    if (CompareText(ExtractFilePath(edtProjectFile8.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtProjectFile8.Text := ExtractFileName(edtProjectFile8.Text);
    if (CompareText(ExtractFilePath(edtProjectFile9.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtProjectFile9.Text := ExtractFileName(edtProjectFile9.Text);
    if (CompareText(ExtractFilePath(edtProjectFile10.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtProjectFile10.Text := ExtractFileName(edtProjectFile10.Text);
    if (CompareText(ExtractFilePath(edtPools.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtPools.Text := ExtractFileName(edtPools.Text);
    If (UpperCase(ExtractFileExt(edtProjectFile1.Text)) <> '.PJ!') and
       (edtProjectFile1.Text <> '') then
       Control.MultipleRuns[1] := edtProjectFile1.Text + '.PJ!'
    Else
       Control.MultipleRuns[1] := edtProjectFile1.Text;
    If (UpperCase(ExtractFileExt(edtProjectFile2.Text)) <> '.PJ!') and
       (edtProjectFile2.Text <> '')  then
       Control.MultipleRuns[2] := edtProjectFile2.Text + '.PJ!'
    Else
       Control.MultipleRuns[2] := edtProjectFile2.Text;
    If (UpperCase(ExtractFileExt(edtProjectFile3.Text)) <> '.PJ!') and
       (edtProjectFile3.Text <> '')  then
       Control.MultipleRuns[3] := edtProjectFile3.Text + '.PJ!'
    Else
       Control.MultipleRuns[3] := edtProjectFile3.Text;
    If (UpperCase(ExtractFileExt(edtProjectFile4.Text)) <> '.PJ!') and
       (edtProjectFile4.Text <> '')  then
       Control.MultipleRuns[4] := edtProjectFile4.Text + '.PJ!'
    Else
       Control.MultipleRuns[4] := edtProjectFile4.Text;
    If (UpperCase(ExtractFileExt(edtProjectFile5.Text)) <> '.PJ!') and
       (edtProjectFile5.Text <> '')  then
       Control.MultipleRuns[5] := edtProjectFile5.Text + '.PJ!'
    Else
       Control.MultipleRuns[5] := edtProjectFile5.Text;
    If (UpperCase(ExtractFileExt(edtProjectFile6.Text)) <> '.PJ!') and
       (edtProjectFile6.Text <> '')  then
       Control.MultipleRuns[6] := edtProjectFile6.Text + '.PJ!'
    Else
       Control.MultipleRuns[6] := edtProjectFile6.Text;
    If (UpperCase(ExtractFileExt(edtProjectFile7.Text)) <> '.PJ!') and
       (edtProjectFile7.Text <> '')  then
       Control.MultipleRuns[7] := edtProjectFile7.Text + '.PJ!'
    Else
       Control.MultipleRuns[7] := edtProjectFile7.Text;
    If (UpperCase(ExtractFileExt(edtProjectFile8.Text)) <> '.PJ!') and
       (edtProjectFile8.Text <> '')  then
       Control.MultipleRuns[8] := edtProjectFile8.Text + '.PJ!'
    Else
       Control.MultipleRuns[8] := edtProjectFile8.Text;
    If (UpperCase(ExtractFileExt(edtProjectFile9.Text)) <> '.PJ!') and
       (edtProjectFile9.Text <> '')  then
       Control.MultipleRuns[9] := edtProjectFile9.Text + '.PJ!'
    Else
       Control.MultipleRuns[9] := edtProjectFile9.Text;
    If (UpperCase(ExtractFileExt(edtProjectFile10.Text)) <> '.PJ!') and
       (edtProjectFile10.Text <> '')  then
       Control.MultipleRuns[10] := edtProjectFile10.Text + '.PJ!'
    Else
       Control.MultipleRuns[10] := edtProjectFile10.Text;
    Control.nProjects := 0;
    LastProject := 9;
    For iProjects := 1 to 10 do
    If Control.MultipleRuns[iProjects] <> '' then
       Control.nProjects := Control.nProjects + 1
    Else
       Repeat
          For jProjects := iProjects to LastProject do
              Control.MultipleRuns[jProjects] := Control.MultipleRuns[jProjects + 1];
          LastProject := LastProject - 1;
       Until (Control.MultipleRuns[iProjects] <> '') or (LastProject <= iProjects);
    If edtPools.Text = '' then
       edtPools.Text := 'Default.IL!';
    If UpperCase(ExtractFileExt(edtPools.Text)) <> '.IL!' then
       Control.MultipleRunPoolFile := edtPools.Text + '.IL!'
    Else
       Control.MultipleRunPoolFile := edtPools.Text;
    Control.ProjectHasChanged := TRUE;
    ModalResult := mrOK;
end;

Procedure TfrmMultipleRunParameters.btnProjectFile1Click(Sender: TObject);
begin
  // select a new climate file
  dlgOpenProjectFile1.FileName := edtProjectFile1.Text;
  if (dlgOpenProjectFile1.Execute) then
  begin
    edtProjectFile1.Text := dlgOpenProjectFile1.FileName;
    if (CompareText(ExtractFilePath(edtProjectFile1.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtProjectFile1.Text := ExtractFileName(edtProjectFile1.Text);
  SetCurrentDir(Control.ProjectDirectory);
  end;
end;

Procedure TfrmMultipleRunParameters.btnProjectFile2Click(Sender: TObject);
begin
  dlgOpenProjectFile2.FileName := edtProjectFile2.Text;
  if (dlgOpenProjectFile2.Execute) then
  begin
    edtProjectFile2.Text := dlgOpenProjectFile2.FileName;
    if (CompareText(ExtractFilePath(edtProjectFile2.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtProjectFile2.Text := ExtractFileName(edtProjectFile2.Text);
  SetCurrentDir(Control.ProjectDirectory);
  end;
end;

Procedure TfrmMultipleRunParameters.btnProjectFile3Click(Sender: TObject);
begin
  dlgOpenProjectFile3.FileName := edtProjectFile3.Text;
  if (dlgOpenProjectFile3.Execute) then
  begin
    edtProjectFile3.Text := dlgOpenProjectFile3.FileName;
    if (CompareText(ExtractFilePath(edtProjectFile3.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtProjectFile3.Text := ExtractFileName(edtProjectFile3.Text);
  SetCurrentDir(Control.ProjectDirectory);
  end;
end;

Procedure TfrmMultipleRunParameters.btnProjectFile4Click(Sender: TObject);
begin
  dlgOpenProjectFile4.FileName := edtProjectFile4.Text;
  if (dlgOpenProjectFile4.Execute) then
  begin
    edtProjectFile4.Text := dlgOpenProjectFile4.FileName;
    if (CompareText(ExtractFilePath(edtProjectFile4.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtProjectFile4.Text := ExtractFileName(edtProjectFile4.Text);
  SetCurrentDir(Control.ProjectDirectory);
  end;
end;

Procedure TfrmMultipleRunParameters.btnProjectFile5Click(Sender: TObject);
begin
  dlgOpenProjectFile5.FileName := edtProjectFile5.Text;
  if (dlgOpenProjectFile5.Execute) then
  begin
    edtProjectFile5.Text := dlgOpenProjectFile5.FileName;
    if (CompareText(ExtractFilePath(edtProjectFile5.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtProjectFile5.Text := ExtractFileName(edtProjectFile5.Text);
  SetCurrentDir(Control.ProjectDirectory);
  end;
end;

Procedure TfrmMultipleRunParameters.btnProjectFile6Click(Sender: TObject);
begin
  dlgOpenProjectFile6.FileName := edtProjectFile6.Text;
  if (dlgOpenProjectFile6.Execute) then
  begin
    edtProjectFile6.Text := dlgOpenProjectFile6.FileName;
    if (CompareText(ExtractFilePath(edtProjectFile6.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtProjectFile6.Text := ExtractFileName(edtProjectFile6.Text);
  SetCurrentDir(Control.ProjectDirectory);
  end;
end;

Procedure TfrmMultipleRunParameters.btnProjectFile7Click(Sender: TObject);
begin
  dlgOpenProjectFile7.FileName := edtProjectFile7.Text;
  if (dlgOpenProjectFile7.Execute) then
  begin
    edtProjectFile7.Text := dlgOpenProjectFile7.FileName;
    if (CompareText(ExtractFilePath(edtProjectFile7.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtProjectFile7.Text := ExtractFileName(edtProjectFile7.Text);
  SetCurrentDir(Control.ProjectDirectory);
  end;
end;

Procedure TfrmMultipleRunParameters.btnProjectFile8Click(Sender: TObject);
begin
  dlgOpenProjectFile8.FileName := edtProjectFile8.Text;
  if (dlgOpenProjectFile8.Execute) then
  begin
    edtProjectFile8.Text := dlgOpenProjectFile8.FileName;
    if (CompareText(ExtractFilePath(edtProjectFile8.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtProjectFile8.Text := ExtractFileName(edtProjectFile8.Text);
  SetCurrentDir(Control.ProjectDirectory);
  end;
end;

Procedure TfrmMultipleRunParameters.btnProjectFile9Click(Sender: TObject);
begin
  dlgOpenProjectFile9.FileName := edtProjectFile9.Text;
  if (dlgOpenProjectFile9.Execute) then
  begin
    edtProjectFile9.Text := dlgOpenProjectFile9.FileName;
    if (CompareText(ExtractFilePath(edtProjectFile9.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtProjectFile9.Text := ExtractFileName(edtProjectFile9.Text);
  SetCurrentDir(Control.ProjectDirectory);
  end;
end;

Procedure TfrmMultipleRunParameters.btnProjectFile10Click(Sender: TObject);
begin
  dlgOpenProjectFile10.FileName := edtProjectFile10.Text;
  if (dlgOpenProjectFile10.Execute) then
  begin
    edtProjectFile10.Text := dlgOpenProjectFile10.FileName;
    if (CompareText(ExtractFilePath(edtProjectFile10.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtProjectFile10.Text := ExtractFileName(edtProjectFile10.Text);
  SetCurrentDir(Control.ProjectDirectory);
  end;
end;

Procedure TfrmMultipleRunParameters.btnPoolsClick(Sender: TObject);
begin
  // select a new pool file
  dlgOpenPools.FileName := edtPools.Text;
  if (dlgOpenPools.Execute) then
    begin
    edtPools.Text := dlgOpenPools.FileName;
    if (CompareText(ExtractFilePath(edtPools.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtPools.Text := ExtractFileName(edtPools.Text);
    end;
 SetCurrentDir(Control.ProjectDirectory);
end;

Procedure TfrmMultipleRunParameters.btnRunClick(Sender: TObject);
begin
Control.SaveBeforeRun := false;
Control.AbortRun := false;
frmMultipleRunParameters.btnOKClick(Sender);
end;

Procedure TfrmMultipleRunParameters.btnSaveRunClick(Sender: TObject);
begin
Control.SaveBeforeRun := true;
Control.AbortRun := false;
frmMultipleRunParameters.btnOKClick(Sender);
end;

Procedure TfrmMultipleRunParameters.btnCancelClick(Sender: TObject);
begin
Control.AbortRun := true;
end;

end.
