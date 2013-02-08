{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmSaveSoilInfo                                 =
  =                                                              =
  =             Interface routine to nominate soil-related       =
  =             variables that are to be saved in an ASCII file  =
  =             during program execution.                        =
  ================================================================
  = File      : untSaveSoilInfo.PAS                              =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

unit untSaveSoilInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TfrmSaveSoilInfo = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    rgOutputLayers: TRadioGroup;
    rgSaveGeneric: TRadioGroup;
    Procedure FormShow(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
    Procedure rgSaveGenericClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSaveSoilInfo: TfrmSaveSoilInfo;

implementation

{$R *.DFM}

uses
  untDeclarations, untMiscellaneous;

Procedure TfrmSaveSoilInfo.FormShow(Sender: TObject);
var
  cbEnabled: TCheckBox;
  iCount, column, iEntries, HalfEntries: integer;
  SaveInFileVar: SaveVariableOptions;
begin
  while (Self.ComponentCount > 5) do
    Self.Components[5].Free;
  If Control.OutputByLayer then
     rgOutputLayers.ItemIndex := 1
  Else
     rgOutputLayers.ItemIndex := 0;
  iCount := 20;
  Column := 1;
  For SaveInFileVar := S_Year to S_Day do
  Begin
    cbEnabled := TCheckBox.Create(Self);
    cbEnabled.Parent := Self;
    cbEnabled.Left := 15;
    cbEnabled.Width := 120;
    cbEnabled.Top := iCount;
    cbEnabled.Caption := SaveVariableNames[SaveInFileVar];
    If SaveVar.Choose[SaveInFileVar] then
      cbEnabled.Checked := true;
    iCount := iCount + 18;
  End;
  iEntries := 0;
  For SaveInFileVar := S_CMetabolic to S_SoilRespn do
      iEntries := iEntries + 1;
  frmSaveSoilInfo.ClientWidth := 380;
  HalfEntries := (iEntries + 1) div 2;
  frmSaveSoilInfo.Height := 120 + (HalfEntries + 4) * 20;
  btnOK.Top := frmSaveSoilInfo.Height - 65;
  btnCancel.Top := btnOK.Top;
  btnHelp.Top := btnOK.Top;
  rgSaveGeneric.Top := btnOK.Top - 45;
  iEntries := 0;
  For SaveInFileVar := S_CMetabolic to S_SoilRespn do
    Begin
    iCount := iCount + 18;
    iEntries := iEntries + 1;
    cbEnabled := TCheckBox.Create(Self);
    cbEnabled.Parent := Self;
    If iEntries > HalfEntries then
       Begin
       iEntries := 0;
       Column := 2;
       iCount := 92;
       End;
    If Column = 1 then
       cbEnabled.Left := 15
    Else
       cbEnabled.Left := 200;
    cbEnabled.Width := 120;
    cbEnabled.Top := iCount;
    cbEnabled.Caption := SaveVariableNames[SaveInFileVar];
    If SaveVar.Choose[SaveInFileVar] then
      cbEnabled.Checked := true;
  End;
end;

Procedure TfrmSaveSoilInfo.btnOKClick(Sender: TObject);
var
  iCount: integer;
  SaveInFileVar: SaveVariableOptions;
begin
  iCount := 0;
  If rgOutputLayers.ItemIndex = 1 then
     Begin
     Control.OutputByLayer := true;
     If Control.AllOneLayer then
        Soil.nLayers := 1;
     End
  Else
     Control.OutputByLayer := false;
  For SaveInFileVar := S_Year to S_Day do
  begin
    SaveVar.Choose[SaveInFileVar] := TCheckBox(Self.Components[iCount + 5]).Checked;
    iCount := iCount + 1;
  End;
  For SaveInFileVar := S_CMetabolic to S_SoilRespn do
  begin
    SaveVar.Choose[SaveInFileVar] := TCheckBox(Self.Components[iCount + 5]).Checked;
    iCount := iCount + 1;
  End;
  Control.ProjectHasChanged := TRUE;
end;

Procedure TfrmSaveSoilInfo.rgSaveGenericClick(Sender: TObject);
var SaveInFileVar: SaveVariableOptions;
begin
frmSaveSoilInfo.btnOKClick(Sender);
If rgSaveGeneric.ItemIndex = 0 then
   Begin
   For SaveInFileVar := S_Year to S_Day do
       SaveVar.Choose[SaveInFileVar] := true;
   For SaveInFileVar := S_CMetabolic to S_SoilRespn do
       SaveVar.Choose[SaveInFileVar] := true;
   Control.ProjectHasChanged := TRUE;
   End
Else if rgSaveGeneric.ItemIndex = 1 then
   Begin
   For SaveInFileVar := S_Year to S_Day do
       SaveVar.Choose[SaveInFileVar] := false;
   For SaveInFileVar := S_CMetabolic to S_SoilRespn do
       SaveVar.Choose[SaveInFileVar] := false;
   Control.ProjectHasChanged := TRUE;
   End;
frmSaveSoilInfo.FormShow(Sender);
end;

end.
