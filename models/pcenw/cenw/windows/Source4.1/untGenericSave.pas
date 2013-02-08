{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmGenericSave                                  =
  =                                                              =
  =             Generic interface routine to nominate            =
  =             variables that are to be saved in an ASCII file  =
  =             during program execution.                        =
  ================================================================
  = File      : untGenericSave.PAS                               =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untGenericSave;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     StdCtrls, ExtCtrls, Buttons;
Const BaseComponents = 5;

type
  TfrmGenericSave = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    rgSaveGeneric: TRadioGroup;
    rgOutputLayers: TRadioGroup;
    Procedure FormShow(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
    Procedure rgSaveGenericClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var frmGenericSave: TfrmGenericSave;

implementation

{$R *.DFM}

uses untDeclarations, untMiscellaneous;

Procedure TfrmGenericSave.FormShow(Sender: TObject);
var cbEnabled: TCheckBox;
    iCount, column, iEntries, HalfEntries: integer;
    SaveInFileVar: SaveVariableOptions;
Begin
while (Self.ComponentCount > BaseComponents) do
      Self.Components[BaseComponents].Free;
If GenericOutput.IncludeSoilLayerInfo then
   Begin
   frmGenericSave.rgOutputLayers.Visible := true;
   iCount := 25;
   If Control.OutputByLayer then
      rgOutputLayers.ItemIndex := 1
   Else
      rgOutputLayers.ItemIndex := 0;
   End
Else
   Begin
   frmGenericSave.rgOutputLayers.Visible := false;
   iCount := 16;
   End;
Column := 1;
frmGenericSave.Text := GenericOutput.Title;
For SaveInFileVar := S_Year to S_Day do
    Begin
    cbEnabled := TCheckBox.Create(Self);
    cbEnabled.Parent := Self;
    cbEnabled.Left := 8;
    cbEnabled.Width := 50;
    cbEnabled.Top := iCount;
    cbEnabled.Caption := SaveVariableNames[SaveInFileVar];
    If SaveVar.Choose[SaveInFileVar] then
       cbEnabled.Checked := true;
    iCount := iCount + 16;
    End;
iEntries := 0;
For SaveInFileVar := GenericOutput.FirstSave to GenericOutput.LastSave do
    iEntries := iEntries + 1;
If iEntries <= 20 then
    Begin
    frmGenericSave.ClientWidth := 280;
    End
Else
    Begin
    frmGenericSave.ClientWidth := 400;
    End;
If iEntries > 20 then
   HalfEntries := (iEntries + 1) div 2
Else
   HalfEntries := iEntries;
frmGenericSave.Height := 110 + (HalfEntries + 4) * 16;
btnOK.Top := frmGenericSave.Height - 60;
btnCancel.Top := btnOK.Top;
btnHelp.Top := btnOK.Top;
rgSaveGeneric.ItemIndex := -1;
iEntries := 0;
For SaveInFileVar := GenericOutput.FirstSave to GenericOutput.LastSave do
    Begin
    iCount := iCount + 16;
    iEntries := iEntries + 1;
    cbEnabled := TCheckBox.Create(Self);
    cbEnabled.Parent := Self;
    If iEntries > HalfEntries then
       Begin
       iEntries := 0;
       Column := 2;
       If GenericOutput.IncludeSoilLayerInfo then
          iCount := 89
       Else
          iCount := 80;
       End;
    If Column = 1 then
       cbEnabled.Left := 8
    Else
       cbEnabled.Left := 200;
    cbEnabled.Top := iCount;
    cbEnabled.Width := 200;
    cbEnabled.Caption := SaveVariableNames[SaveInFileVar];
    If SaveVar.Choose[SaveInFileVar] then
      cbEnabled.Checked := true;
    End;
End;

Procedure TfrmGenericSave.btnOKClick(Sender: TObject);
var iCount: integer;
    SaveInFileVar: SaveVariableOptions;
Begin
iCount := 0;
For SaveInFileVar := S_Year to S_Day do
    Begin
    SaveVar.Choose[SaveInFileVar] := TCheckBox(Self.Components[iCount + BaseComponents]).Checked;
    iCount := iCount + 1;
    End;
For SaveInFileVar := GenericOutput.FirstSave to GenericOutput.LastSave do
    Begin
    SaveVar.Choose[SaveInFileVar] := TCheckBox(Self.Components[iCount + BaseComponents]).Checked;
    iCount := iCount + 1;
    End;
If rgOutputLayers.ItemIndex = 1 then
   Begin
   Control.OutputByLayer := true;
   If Control.AllOneLayer then
      Soil.nLayers := 1;
   End
Else
   Control.OutputByLayer := false;
Control.ProjectHasChanged := true;
End;

Procedure TfrmGenericSave.rgSaveGenericClick(Sender: TObject);
var SaveInFileVar: SaveVariableOptions;
    iCount: Integer;
    AllOn: Boolean;
Begin
iCount := 0;
If rgSaveGeneric.ItemIndex = 0 then
   AllOn := true
Else
   AllOn := false;
For SaveInFileVar := S_Year to S_Day do
    Begin
    TCheckBox(Self.Components[iCount + BaseComponents]).Checked := AllOn;
    iCount := iCount + 1;
    End;
For SaveInFileVar := GenericOutput.FirstSave to GenericOutput.LastSave do
    Begin
    TCheckBox(Self.Components[iCount + BaseComponents]).Checked := AllOn;
    iCount := iCount + 1;
    End;

End;

End.
