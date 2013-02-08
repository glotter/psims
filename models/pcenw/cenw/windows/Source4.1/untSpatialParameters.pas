{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmSpatialParameters                            =
  =                                                              =
  =             Edit window to change parameters for running     =
  =             in spatial mode					     =	
  ================================================================
  = File      : untSpatialParameters.PAS                         =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untSpatialParameters;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, untFieldValidation;

type
  TfrmSpatialParameters = class(TForm)
    grpCoordinates: TGroupBox;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    Label7: TLabel;
    edtTempMax: TEdit;
    Label8: TLabel;
    edtTempMin: TEdit;
    Label9: TLabel;
    edtRainMax: TEdit;
    Label10: TLabel;
    edtRainMin: TEdit;
    Label11: TLabel;
    edtCalcs: TEdit;
    Label12: TLabel;
    edtInitial: TEdit;
    lblReleaseFertiliser: TLabel;
    edtLatMin: TEdit;
    edtLatMax: TEdit;
    lblLeaching: TLabel;
    edtLongMin: TEdit;
    Label15: TLabel;
    Label16: TLabel;
    edtLongMax: TEdit;
    edtLongInterval: TEdit;
    Label4: TLabel;
    Label3: TLabel;
    edtLatInterval: TEdit;
    rgPlantType: TRadioGroup;
    rgSoilType: TRadioGroup;
    grpSpatialFile: TGroupBox;
    edtSpatialFile: TEdit;
    btnSpatialFile: TButton;
    dlgOpenSpatialFile: TOpenDialog;
    chkNumeric: TCheckBox;
    Procedure FormShow(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
    procedure btnSpatialFileClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSpatialParameters: TfrmSpatialParameters;

implementation

{$R *.DFM}

uses untDeclarations, untMiscellaneous, untMain;

Procedure TfrmSpatialParameters.FormShow(Sender: TObject);

begin
frmMain.FillEdit(Sender, edtInitial, Control.Spatial.Initial, 0);
frmMain.FillEdit(Sender, edtCalcs, Control.Spatial.Calcs, 0);
frmMain.FillEdit(Sender, edtRainMin, Control.Spatial.RainMin, 1);
frmMain.FillEdit(Sender, edtRainMax, Control.Spatial.RainMax, 1);
frmMain.FillEdit(Sender, edtTempMin, Control.Spatial.TempMin, 1);
frmMain.FillEdit(Sender, edtTempMax, Control.Spatial.TempMax, 1);
frmMain.FillEdit(Sender, edtLongMin, Control.Spatial.LongMin, 1);
frmMain.FillEdit(Sender, edtLongMax, Control.Spatial.LongMax, 1);
frmMain.FillEdit(Sender, edtLongInterval, Control.Spatial.LongInterval, 1);
frmMain.FillEdit(Sender, edtLatMin, Control.Spatial.LatMin, 1);
frmMain.FillEdit(Sender, edtLatMax, Control.Spatial.LatMax, 1);
frmMain.FillEdit(Sender, edtLatInterval, Control.Spatial.LatInterval, 1);
If Control.Spatial.PlantType = Optimal then
   rgPlantType.ItemIndex := 0 else
   rgPlantType.ItemIndex := 1;
If Control.Spatial.SoilType = Equil then
   rgSoilType.ItemIndex := 0 else
   rgSoilType.ItemIndex := 1;
if (CompareText(ExtractFilePath(Control.SpatialFile), ExtractFilePath(Control.SpatialFile)) = 0) then
   Control.SpatialFile := ExtractFileName(Control.SpatialFile);
edtSpatialFile.Text := Control.SpatialFile;
chkNumeric.Checked := Control.Spatial.ShowNumeric;
end;
{ If Control.Geography.ShowNumeric then
}
Procedure TfrmSpatialParameters.btnOKClick(Sender: TObject);
var ScreenCount: Integer;
    ScreenItem: ScreenOptions;
begin
frmMain.GetInteger(Sender, edtInitial, Control.Spatial.Initial);
frmMain.GetInteger(Sender, edtCalcs, Control.Spatial.Calcs);
frmMain.GetEdit(Sender, edtRainMin, Control.Spatial.RainMin, 1);
frmMain.GetEdit(Sender, edtRainMax, Control.Spatial.RainMax, 1);
frmMain.GetEdit(Sender, edtTempMin, Control.Spatial.TempMin, 1);
frmMain.GetEdit(Sender, edtTempMax, Control.Spatial.TempMax, 1);
frmMain.GetEdit(Sender, edtLongMin, Control.Spatial.LongMin, 1);
frmMain.GetEdit(Sender, edtLongMax, Control.Spatial.LongMax, 1);
frmMain.GetEdit(Sender, edtLongInterval, Control.Spatial.LongInterval, 1);
frmMain.GetEdit(Sender, edtLatMin, Control.Spatial.LatMin, 1);
frmMain.GetEdit(Sender, edtLatMax, Control.Spatial.LatMax, 1);
frmMain.GetEdit(Sender, edtLatInterval, Control.Spatial.LatInterval, 1);
if rgPlantType.ItemIndex = 0 then
   Control.Spatial.PlantType := Optimal
else
   Control.Spatial.PlantType := Exotic;
if rgSoilType.ItemIndex = 0 then
   Control.Spatial.SoilType := Equil
else
   Control.Spatial.SoilType := SoilSet;
If (CompareText(ExtractFilePath(edtSpatialFile.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
   edtSpatialFile.Text := ExtractFileName(edtSpatialFile.Text);
If UpperCase(ExtractFileExt(edtSpatialFile.Text)) <> '.SP!' then
   Control.SpatialFile := edtSpatialFile.Text + '.SP!'
Else
   Control.SpatialFile := edtSpatialFile.Text;
Control.Spatial.ShowNumeric := chkNumeric.Checked;
ScreenCount := 0;
For ScreenItem := D_CarbonGain to D_Dummy do
   If ScreenRec.Choose[ScreenItem] then
      Begin
      if (ScreenItem < D_PConc) or (ScreenItem > D_2ndaryP) or Control.IncludeP then
          Begin
          ScreenCount := ScreenCount + 1;
          Control.Spatial.Draw := ScreenItem;
          End;
      End;
If ScreenCount <> 1 then
   Begin
   MessageDlg('CANNOT RUN SPATIAL SIMULATION' + chr(10) +
              'You can must specify exactly one variable' + chr(10) +
              'for screen output to run the model in spatial mode.' + chr(10) +
              'Go to the screen options and select one,' + chr(10) +
              'and one only screen display option' + chr(10) +
              'to enable spatial runs.',
               mtError, [mbOK], 0);
   Control.SpatialMode := false;
   End;
end;

procedure TfrmSpatialParameters.btnSpatialFileClick(Sender: TObject);
begin
dlgOpenSpatialFile.FileName := edtSpatialFile.Text;
if (dlgOpenSpatialFile.Execute) then
   begin
   edtSpatialFile.Text := dlgOpenSpatialFile.FileName;
   if (CompareText(ExtractFilePath(edtSpatialFile.Text), ExtractFilePath(Control.ProjectFile)) = 0) then
      edtSpatialFile.Text := ExtractFileName(edtSpatialFile.Text);
   SetCurrentDir(Control.ProjectDirectory);
   end;
end;

end.
