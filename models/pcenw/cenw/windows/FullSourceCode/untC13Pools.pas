{ ================================================================
  = Project   : CENW                                             =
  ================================================================
  = Modules   : TfrmC13Pools                                     =
  =                                                              =
  =             Interface routine to show all pools              =
  =             and allow the user to change them.               =
  ================================================================
  = File      : untC13Pools.PAS                                  =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

unit untC13Pools;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, untDeclarations;

type

  TfrmC13Pools = class(TForm)
    pgcPages: TPageControl;
    tbsCarbon: TTabSheet;
    grpPlantPools: TGroupBox;
    pnlButtons: TPanel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    lbCLayer: TLabel;
    lbStructuralC: TLabel;
    lbMetabolicC: TLabel;
    lbActiveC: TLabel;
    lbSlowC: TLabel;
    lbResistantC: TLabel;
    lbFineWoodC: TLabel;
    lbCoarseWoodC: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    rdSetAllPlantDeltas: TRadioButton;
    rdSetAllSoilDeltas: TRadioButton;
    Label1: TLabel;
    edtSapWoodC: TEdit;
    lblLine1: TLabel;
    Label2: TLabel;
    edtHeartWoodC: TEdit;
    edtCoarseRootC: TEdit;
    Label5: TLabel;
    edtBarkC: TEdit;
    Label6: TLabel;
    edtBranchesC: TEdit;
    Label7: TLabel;
    edtReserves: TEdit;
    Label8: TLabel;
    edtSolubleC: TEdit;
    Label9: TLabel;
    edtPollenC: TEdit;
    Label10: TLabel;
    edtFruitC: TEdit;
    Label11: TLabel;
    edtFineRootC: TEdit;
    Label12: TLabel;
    edtLeavesC: TEdit;
    Label13: TLabel;
    edtChangePlantDelta: TEdit;
    Label14: TLabel;
    edtChangeSoilDelta: TEdit;
    Label15: TLabel;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure rdSetAllPlantDeltasClick(Sender: TObject);
    procedure rdSetAllSoilDeltasClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmC13Pools: TfrmC13Pools;
  DisplayLayers: Integer;

implementation

{$R *.DFM}

uses untMiscellaneous, untHelpConsts, untMain;

procedure TfrmC13Pools.FormShow(Sender: TObject);
var iCount, iLayer: integer;
    LayerInfo: String;

  Procedure FillEditBoxDisplay(TabSheet: TTabSheet; Width, Left, Top: Integer;
            ReadOnlyFlag: Boolean; fValue: Real48);
  var NextBox: TEdit;
      FieldWidth, Digits: Integer;
    Begin
    NextBox := TEdit.Create(TabSheet);
    NextBox.Parent := TabSheet;
    NextBox.Width := Width;
    NextBox.Left := Left;
    NextBox.Top := Top;
    NextBox.ReadOnly := ReadOnlyFlag;
    frmMain.FillEdit(Sender, NextBox, fValue, 1);
    End;

  Procedure LabelDisplay(TabSheet: TTabSheet; Caption: String; Width, Left, Top: Integer);
    var NextLabel: TLabel;
    Begin
    NextLabel := TLabel.Create(TabSheet);
    NextLabel.Parent := TabSheet;
    NextLabel.Caption := Caption;
    NextLabel.Width := Width;
    NextLabel.Left := Left;
    NextLabel.Top := Top;
    End;

Begin
frmMain.FillEdit(Sender, edtChangeSoilDelta, Parameter.DefaultSoilDelta, 1);
frmMain.FillEdit(Sender, edtChangePlantDelta, Parameter.DefaultPlantDelta, 1);
If Control.AllOneLayer then
   DisplayLayers := 1
Else
   DisplayLayers := SoilOrganic.nLayers;
If DisplayLayers < 5 then
   Begin
   frmC13Pools.Height := 410;
   frmC13Pools.Top := 165;
   End
Else
   Begin
   frmC13Pools.Height := 300 + 22 * DisplayLayers;
   frmC13Pools.Top := 232 - 11 * DisplayLayers;
   End;
frmMain.FillEdit(Sender, edtSapWoodC, Plant.SapWood[C13], 1);
frmMain.FillEdit(Sender, edtHeartWoodC, Plant.HeartWood[C13], 1);
frmMain.FillEdit(Sender, edtCoarseRootC, Plant.CoarseRoot[C13], 1);
frmMain.FillEdit(Sender, edtFineRootC, Plant.FineRoot[C13], 1);
frmMain.FillEdit(Sender, edtBranchesC, Plant.Branches[C13], 1);
frmMain.FillEdit(Sender, edtBarkC, Plant.Bark[C13], 1);
frmMain.FillEdit(Sender, edtLeavesC, Plant.Leaves[C13], 1);
frmMain.FillEdit(Sender, edtPollenC, Plant.Pollen[C13], 1);
frmMain.FillEdit(Sender, edtFruitC, Plant.Fruit[C13], 1);
frmMain.FillEdit(Sender, edtSolubleC, Plant.Soluble[C13], 1);
frmMain.FillEdit(Sender, edtReserves, Plant.Reserves[C13], 1);
while (frmC13Pools.tbsCarbon.ComponentCount > 0) do
      frmC13Pools.tbsCarbon.Components[0].Free;
// fill form with parameters
  iCount := 100;
  For iLayer := 0 to DisplayLayers do
     Begin
     iCount := iCount + 22;
     If iLayer = 0 then
        LabelDisplay (frmC13Pools.tbsCarbon, 'Surface', 40, 35, iCount + 4)
     Else
        Begin
        Str(iLayer:3, LayerInfo);
        LabelDisplay (frmC13Pools.tbsCarbon, LayerInfo + ':', 25, 53, iCount + 4);
        End;
     FillEditBoxDisplay (frmC13Pools.tbsCarbon, 50, 85, iCount, false, SoilOrganic.Struct[iLayer, C13]);
     FillEditBoxDisplay (frmC13Pools.tbsCarbon, 50, 170, iCount, false, SoilOrganic.Metab[iLayer, C13]);
     If iLayer = 0 then
        FillEditBoxDisplay (frmC13Pools.tbsCarbon, 50, 255, iCount, false, SoilOrganic.FineWood[0, C13]);
     FillEditBoxDisplay (frmC13Pools.tbsCarbon, 50, 340, iCount, false, SoilOrganic.CoarseWood[iLayer, C13]);
     FillEditBoxDisplay (frmC13Pools.tbsCarbon, 50, 425, iCount, false, SoilOrganic.Active[iLayer, C13]);
     FillEditBoxDisplay (frmC13Pools.tbsCarbon, 50, 510, iCount, false, SoilOrganic.Slow[iLayer, C13]);
     FillEditBoxDisplay (frmC13Pools.tbsCarbon, 50, 595, iCount, false, SoilOrganic.Resistant[iLayer, C13]);
     FillEditBoxDisplay (frmC13Pools.tbsCarbon, 50, 680, iCount, false, SoilOrganic.Inert[iLayer, C13]);
     End;

end;

procedure TfrmC13Pools.btnOKClick(Sender: TObject);
var
  DisplayLayers, iLayer: Integer;

begin
If Control.AllOneLayer then
   DisplayLayers := 1
Else
   DisplayLayers := SoilOrganic.nLayers;
frmMain.GetEdit(Sender, edtSapWoodC, Plant.SapWood[C13], 1);
frmMain.GetEdit(Sender, edtHeartWoodC, Plant.HeartWood[C13], 1);
frmMain.GetEdit(Sender, edtCoarseRootC, Plant.CoarseRoot[C13], 1);
frmMain.GetEdit(Sender, edtFineRootC, Plant.FineRoot[C13], 1);
frmMain.GetEdit(Sender, edtBranchesC, Plant.Branches[C13], 1);
frmMain.GetEdit(Sender, edtBarkC, Plant.Bark[C13], 1);
frmMain.GetEdit(Sender, edtLeavesC, Plant.Leaves[C13], 1);
frmMain.GetEdit(Sender, edtPollenC, Plant.Pollen[C13], 1);
frmMain.GetEdit(Sender, edtFruitC, Plant.Fruit[C13], 1);
frmMain.GetEdit(Sender, edtSolubleC, Plant.Soluble[C13], 1);
frmMain.GetEdit(Sender, edtReserves, Plant.Reserves[C13], 1);
frmMain.GetEdit(Sender, TEdit(frmC13Pools.tbsCarbon.Components[1]), SoilOrganic.Struct[0, C13], 1);
frmMain.GetEdit(Sender, TEdit(frmC13Pools.tbsCarbon.Components[2]), SoilOrganic.Metab[0, C13], 1);
frmMain.GetEdit(Sender, TEdit(frmC13Pools.tbsCarbon.Components[3]), SoilOrganic.FineWood[0, C13], 1);
frmMain.GetEdit(Sender, TEdit(frmC13Pools.tbsCarbon.Components[4]), SoilOrganic.CoarseWood[0, C13], 1);
frmMain.GetEdit(Sender, TEdit(frmC13Pools.tbsCarbon.Components[5]), SoilOrganic.Active[0, C13], 1);
frmMain.GetEdit(Sender, TEdit(frmC13Pools.tbsCarbon.Components[6]), SoilOrganic.Slow[0, C13], 1);
frmMain.GetEdit(Sender, TEdit(frmC13Pools.tbsCarbon.Components[7]), SoilOrganic.Resistant[0, C13], 1);
frmMain.GetEdit(Sender, TEdit(frmC13Pools.tbsCarbon.Components[8]), SoilOrganic.Inert[0, C13], 1);
For iLayer := 1 to DisplayLayers do
    Begin
    frmMain.GetEdit(Sender, TEdit(frmC13Pools.tbsCarbon.Components[iLayer * 8 + 2]), SoilOrganic.Struct[iLayer, C13], 1);
    frmMain.GetEdit(Sender, TEdit(frmC13Pools.tbsCarbon.Components[iLayer * 8 + 3]), SoilOrganic.Metab[iLayer, C13], 1);
    frmMain.GetEdit(Sender, TEdit(frmC13Pools.tbsCarbon.Components[iLayer * 8 + 4]), SoilOrganic.CoarseWood[iLayer, C13], 1);
    frmMain.GetEdit(Sender, TEdit(frmC13Pools.tbsCarbon.Components[iLayer * 8 + 5]), SoilOrganic.Active[iLayer, C13], 1);
    frmMain.GetEdit(Sender, TEdit(frmC13Pools.tbsCarbon.Components[iLayer * 8 + 6]), SoilOrganic.Slow[iLayer, C13], 1);
    frmMain.GetEdit(Sender, TEdit(frmC13Pools.tbsCarbon.Components[iLayer * 8 + 7]), SoilOrganic.Resistant[iLayer, C13], 1);
    frmMain.GetEdit(Sender, TEdit(frmC13Pools.tbsCarbon.Components[iLayer * 8 + 8]), SoilOrganic.Inert[iLayer, C13], 1);
    End;
Control.InitHasChanged := TRUE;
end;

procedure TfrmC13Pools.btnHelpClick(Sender: TObject);
begin
  // show the appropriate help page
  Application.HelpContext(idm_DeltaPools);
end;

procedure TfrmC13Pools.rdSetAllPlantDeltasClick(Sender: TObject);

begin
frmMain.GetEdit(Sender, edtChangePlantDelta, Parameter.DefaultPlantDelta, 1);
frmMain.FillEdit(Sender, edtSapWoodC, Parameter.DefaultPlantDelta, 1);
frmMain.FillEdit(Sender, edtHeartWoodC, Parameter.DefaultPlantDelta, 1);
frmMain.FillEdit(Sender, edtCoarseRootC, Parameter.DefaultPlantDelta, 1);
frmMain.FillEdit(Sender, edtFineRootC, Parameter.DefaultPlantDelta, 1);
frmMain.FillEdit(Sender, edtBranchesC, Parameter.DefaultPlantDelta, 1);
frmMain.FillEdit(Sender, edtBarkC, Parameter.DefaultPlantDelta, 1);
frmMain.FillEdit(Sender, edtLeavesC, Parameter.DefaultPlantDelta, 1);
frmMain.FillEdit(Sender, edtPollenC, Parameter.DefaultPlantDelta, 1);
frmMain.FillEdit(Sender, edtFruitC, Parameter.DefaultPlantDelta, 1);
frmMain.FillEdit(Sender, edtSolubleC, Parameter.DefaultPlantDelta, 1);
frmMain.FillEdit(Sender, edtReserves, Parameter.DefaultPlantDelta, 1);
rdSetAllPlantDeltas.Checked := false;
Control.PlantHasChanged := true;
end;

procedure TfrmC13Pools.rdSetAllSoilDeltasClick(Sender: TObject);
var iLayer, iCol: Integer;

begin
frmMain.GetEdit(Sender, edtChangeSoilDelta, Parameter.DefaultSoilDelta, 1);
For iCol := 1 to 8 do
    frmMain.FillEdit(Sender, TEdit(frmC13Pools.tbsCarbon.Components[iCol]), Parameter.DefaultSoilDelta, 1);
For iLayer := 1 to DisplayLayers do
    For iCol := 1 to 7 do
        frmMain.FillEdit(Sender, TEdit(frmC13Pools.tbsCarbon.Components[iLayer * 8 + 1 + iCol]), Parameter.DefaultSoilDelta, 1);
rdSetAllSoilDeltas.Checked := false;
Control.SiteHasChanged := true;
end;

end.
