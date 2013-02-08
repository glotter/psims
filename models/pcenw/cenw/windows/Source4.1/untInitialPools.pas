{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmInitialPools                                 =
  =                                                              =
  =             Interface routine to allow users to              =
  =             change plant pools at the start of a run.        =
  ================================================================
  = File      : untInitialPools.PAS                              =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untInitialPools;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, untDeclarations;

type

  TfrmInitialPools = class(TForm)
    grpPools: TGroupBox;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    lblDay: TLabel;
    lblMonth: TLabel;
    lblYear: TLabel;
    edtSapWoodC: TEdit;
    Label3: TLabel;
    edtSapwoodN: TEdit;
    Label4: TLabel;
    edtHeartWoodC: TEdit;
    Label5: TLabel;
    edtHeartwoodN: TEdit;
    Label6: TLabel;
    edtLeavesC: TEdit;
    Label7: TLabel;
    edtFoliageN: TEdit;
    Label8: TLabel;
    edtFineRootC: TEdit;
    Label9: TLabel;
    edtFineRootN: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    edtCoarseRootC: TEdit;
    edtCoarseRootN: TEdit;
    Label12: TLabel;
    edtBranchesC: TEdit;
    Label13: TLabel;
    edtBranchN: TEdit;
    Label14: TLabel;
    edtBarkC: TEdit;
    Label15: TLabel;
    edtBarkN: TEdit;
    Label16: TLabel;
    edtFruitC: TEdit;
    Label17: TLabel;
    edtFruitN: TEdit;
    Label18: TLabel;
    edtPollenC: TEdit;
    Label19: TLabel;
    edtPollenN: TEdit;
    Label20: TLabel;
    edtSolubleC: TEdit;
    Label21: TLabel;
    edtSolubleN: TEdit;
    Label22: TLabel;
    edtReservesC: TEdit;
    Label23: TLabel;
    Label24: TLabel;
    edtReservesN: TEdit;
    edtPlantAge: TEdit;
    lblAge: TLabel;
    edtStocking: TEdit;
    lblStocking: TLabel;
    edtStartDay: TEdit;
    lblStartingDate: TLabel;
    lblDash1: TLabel;
    edtStartMonth: TEdit;
    lblDash2: TLabel;
    edtStartYear: TEdit;
    lblHeight: TLabel;
    lblDBH: TLabel;
    edtHeight: TEdit;
    edtDBH: TEdit;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    edtSapwoodP: TEdit;
    edtHeartwoodP: TEdit;
    edtFoliageP: TEdit;
    edtFinerootP: TEdit;
    edtCoarserootP: TEdit;
    edtBranchP: TEdit;
    edtBarkP: TEdit;
    edtFruitP: TEdit;
    edtPollenP: TEdit;
    edtSolubleP: TEdit;
    edtReservesP: TEdit;
    grpWeeds: TGroupBox;
    lblWeedLeavesC: TLabel;
    lblWeedRootsC: TLabel;
    lblWeedLeavesN: TLabel;
    lblWeedRootsN: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    edtWeedLeavesC: TEdit;
    edtWeedRootsC: TEdit;
    edtWeedLeavesN: TEdit;
    edtWeedRootsN: TEdit;
    edtWeedLeavesP: TEdit;
    edtWeedRootsP: TEdit;
    Procedure FormShow(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
    Procedure btnHelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var frmInitialPools: TfrmInitialPools;

implementation

{$R *.DFM}

uses untMiscellaneous, untHelpConsts, untMain;

Procedure TfrmInitialPools.FormShow(Sender: TObject);
var ExtraVertical: Integer;
Begin
frmMain.FillEdit(Sender, edtSapWoodC, Control.Initial.SapWood[C], Control.CConversion);
frmMain.FillEdit(Sender, edtHeartWoodC, Control.Initial.HeartWood[C], Control.CConversion);
frmMain.FillEdit(Sender, edtCoarseRootC, Control.Initial.CoarseRoot[C], Control.CConversion);
frmMain.FillEdit(Sender, edtFineRootC, Control.Initial.FineRoot[C], Control.CConversion);
frmMain.FillEdit(Sender, edtBranchesC, Control.Initial.Branches[C], Control.CConversion);
frmMain.FillEdit(Sender, edtBarkC, Control.Initial.Bark[C], Control.CConversion);
frmMain.FillEdit(Sender, edtLeavesC, Control.Initial.Leaves[C], Control.CConversion);
frmMain.FillEdit(Sender, edtPollenC, Control.Initial.Pollen[C], Control.CConversion);
frmMain.FillEdit(Sender, edtFruitC, Control.Initial.Fruit[C], Control.CConversion);
frmMain.FillEdit(Sender, edtSolubleC, Control.Initial.Soluble[C], Control.CConversion);
frmMain.FillEdit(Sender, edtReservesC, Control.Initial.Reserves[C], Control.CConversion);
frmMain.FillEdit(Sender, edtWeedLeavesC, Control.Initial.WeedLeaves[C], Control.CConversion);
frmMain.FillEdit(Sender, edtWeedRootsC, Control.Initial.WeedRoots[C], Control.CConversion );
frmMain.FillEdit(Sender, edtSapWoodN, Control.Initial.SapWood[N], 1);
frmMain.FillEdit(Sender, edtHeartWoodN, Control.Initial.HeartWood[N], 1);
frmMain.FillEdit(Sender, edtCoarseRootN, Control.Initial.CoarseRoot[N], 1);
frmMain.FillEdit(Sender, edtFineRootN, Control.Initial.FineRoot[N], 1);
frmMain.FillEdit(Sender, edtBranchN, Control.Initial.Branches[N], 1);
frmMain.FillEdit(Sender, edtBarkN, Control.Initial.Bark[N], 1);
frmMain.FillEdit(Sender, edtFoliageN, Control.Initial.Leaves[N], 1);
frmMain.FillEdit(Sender, edtPollenN, Control.Initial.Pollen[N], 1);
frmMain.FillEdit(Sender, edtFruitN, Control.Initial.Fruit[N], 1);
frmMain.FillEdit(Sender, edtSolubleN, Control.Initial.Soluble[N], 1);
frmMain.FillEdit(Sender, edtReservesN, Control.Initial.Reserves[N], 1);
frmMain.FillEdit(Sender, edtWeedLeavesN, Control.Initial.WeedLeaves[N], 1);
frmMain.FillEdit(Sender, edtWeedRootsN, Control.Initial.WeedRoots[N], 1);
frmMain.FillEdit(Sender, edtSapWoodP, Control.Initial.SapWood[P], 1);
frmMain.FillEdit(Sender, edtHeartWoodP, Control.Initial.HeartWood[P], 1);
frmMain.FillEdit(Sender, edtCoarseRootP, Control.Initial.CoarseRoot[P], 1);
frmMain.FillEdit(Sender, edtFineRootP, Control.Initial.FineRoot[P], 1);
frmMain.FillEdit(Sender, edtBranchP, Control.Initial.Branches[P], 1);
frmMain.FillEdit(Sender, edtBarkP, Control.Initial.Bark[P], 1);
frmMain.FillEdit(Sender, edtFoliageP, Control.Initial.Leaves[P], 1);
frmMain.FillEdit(Sender, edtPollenP, Control.Initial.Pollen[P], 1);
frmMain.FillEdit(Sender, edtFruitP, Control.Initial.Fruit[P], 1);
frmMain.FillEdit(Sender, edtSolubleP, Control.Initial.Soluble[P], 1);
frmMain.FillEdit(Sender, edtWeedLeavesP, Control.Initial.WeedLeaves[P], 1);
frmMain.FillEdit(Sender, edtWeedRootsP, Control.Initial.WeedRoots[P], 1);
frmMain.FillEdit(Sender, edtReservesP, Control.Initial.Reserves[P], 1);
frmMain.FillEdit(Sender, edtPlantAge, Control.Initial.Age, 0);
frmMain.FillEdit(Sender, edtStocking, Control.Initial.Stocking, 0);
frmMain.FillEdit(Sender, edtHeight, Control.Initial.Height, 1);
frmMain.FillEdit(Sender, edtDBH, Control.Initial.DBH, 1);
frmMain.FillEdit(Sender, edtStartDay, Control.Initial.Days, 0);
frmMain.FillEdit(Sender, edtStartMonth, Control.Initial.Months, 0);
frmMain.FillEdit(Sender, edtStartYear, Control.Initial.Years, 0);
if Control.IncludeP then
   Begin
   frmInitialPools.Width := 540;
   grpPools.Width := 465;
   grpWeeds.Width := 465;
   edtSapWoodP.Visible := true;
   edtHeartWoodP.Visible := true;
   edtCoarseRootP.Visible := true;
   edtFineRootP.Visible := true;
   edtBranchP.Visible := true;
   edtBarkP.Visible := true;
   edtFoliageP.Visible := true;
   edtPollenP.Visible := true;
   edtFruitP.Visible := true;
   edtSolubleP.Visible := true;
   edtReservesP.Visible := true;
   End
Else {if not ControlP then}
   Begin
   frmInitialPools.Width := 390;
   grpPools.Width := 310;
   grpWeeds.Width := 310;
   edtSapWoodP.Visible := false;
   edtHeartWoodP.Visible := false;
   edtCoarseRootP.Visible := false;
   edtFineRootP.Visible := false;
   edtBranchP.Visible := false;
   edtBarkP.Visible := false;
   edtFoliageP.Visible := false;
   edtPollenP.Visible := false;
   edtFruitP.Visible := false;
   edtSolubleP.Visible := false;
   edtReservesP.Visible := false;
   End;
if Control.IncludeWeeds then
   Begin
   ExtraVertical := 70;
   grpWeeds.Visible := true;
   lblWeedLeavesC.Visible := true;
   lblWeedLeavesN.Visible := true;
   lblWeedRootsC.Visible := true;
   lblWeedRootsN.Visible := true;
   edtWeedLeavesC.Visible := true;
   edtWeedLeavesN.Visible := true;
   edtWeedRootsC.Visible := true;
   edtWeedRootsN.Visible := true;
   End
Else
   Begin
   ExtraVertical := 0;
   grpWeeds.Visible := false;
   lblWeedLeavesC.Visible := false;
   lblWeedLeavesN.Visible := false;
   lblWeedRootsC.Visible := false;
   lblWeedRootsN.Visible := false;
   edtWeedLeavesC.Visible := false;
   edtWeedLeavesN.Visible := false;
   edtWeedRootsC.Visible := false;
   edtWeedRootsN.Visible := false;
   End;
edtPlantAge.Top := 430 + ExtraVertical;
lblAge.Top := 434 + ExtraVertical;
edtStocking.Top := 430 + ExtraVertical;
lblStocking.Top := 434 + ExtraVertical;
edtHeight.Top := 462 + ExtraVertical;
lblHeight.Top := 466 + ExtraVertical;
edtDBH.Top := 462 + ExtraVertical;
lblDBH.Top := 466 + ExtraVertical;
lblDay.Top := 498 + ExtraVertical;
lblMonth.Top := 498 + ExtraVertical;
lblYear.Top := 498 + ExtraVertical;
edtStartDay.Top := 510 + ExtraVertical;
edtStartMonth. Top := 510 + ExtraVertical;
edtStartYear.Top := 510 + ExtraVertical;
lblStartingDate.Top := 514 + ExtraVertical;
lblDash1.Top := 514 + ExtraVertical;
lblDash2.Top := 514 + ExtraVertical;
btnOK.Top := 558 + ExtraVertical;
btnCancel.Top := btnOK.Top;
btnHelp.Top := btnOK.Top;
end;

Procedure TfrmInitialPools.btnOKClick(Sender: TObject);
Begin
frmMain.GetEdit(Sender, edtSapWoodC, Control.Initial.SapWood[C], Control.CConversion);
frmMain.GetEdit(Sender, edtHeartWoodC, Control.Initial.HeartWood[C], Control.CConversion);
frmMain.GetEdit(Sender, edtCoarseRootC, Control.Initial.CoarseRoot[C], Control.CConversion);
frmMain.GetEdit(Sender, edtFineRootC, Control.Initial.FineRoot[C], Control.CConversion);
frmMain.GetEdit(Sender, edtBranchesC, Control.Initial.Branches[C], Control.CConversion);
frmMain.GetEdit(Sender, edtBarkC, Control.Initial.Bark[C], Control.CConversion);
frmMain.GetEdit(Sender, edtLeavesC, Control.Initial.Leaves[C], Control.CConversion);
frmMain.GetEdit(Sender, edtPollenC, Control.Initial.Pollen[C], Control.CConversion);
frmMain.GetEdit(Sender, edtFruitC, Control.Initial.Fruit[C], Control.CConversion);
frmMain.GetEdit(Sender, edtSolubleC, Control.Initial.Soluble[C], Control.CConversion);
frmMain.GetEdit(Sender, edtReservesC, Control.Initial.Reserves[C], Control.CConversion);
frmMain.GetEdit(Sender, edtWeedLeavesC, Control.Initial.WeedLeaves[C], Control.CConversion);
frmMain.GetEdit(Sender, edtWeedRootsC, Control.Initial.WeedRoots[C], Control.CConversion);
frmMain.GetEdit(Sender, edtSapWoodN, Control.Initial.SapWood[N], 1);
frmMain.GetEdit(Sender, edtHeartWoodN, Control.Initial.HeartWood[N], 1);
frmMain.GetEdit(Sender, edtCoarseRootN, Control.Initial.CoarseRoot[N], 1);
frmMain.GetEdit(Sender, edtFineRootN, Control.Initial.FineRoot[N], 1);
frmMain.GetEdit(Sender, edtBranchN, Control.Initial.Branches[N], 1);
frmMain.GetEdit(Sender, edtBarkN, Control.Initial.Bark[N], 1);
frmMain.GetEdit(Sender, edtFoliageN, Control.Initial.Leaves[N], 1);
frmMain.GetEdit(Sender, edtPollenN, Control.Initial.Pollen[N], 1);
frmMain.GetEdit(Sender, edtFruitN, Control.Initial.Fruit[N], 1);
frmMain.GetEdit(Sender, edtSolubleN, Control.Initial.Soluble[N], 1);
frmMain.GetEdit(Sender, edtReservesN, Control.Initial.Reserves[N], 1);
frmMain.GetEdit(Sender, edtWeedLeavesN, Control.Initial.WeedLeaves[N], 1);
frmMain.GetEdit(Sender, edtWeedRootsN, Control.Initial.WeedRoots[N], 1);
frmMain.GetEdit(Sender, edtSapWoodP, Control.Initial.SapWood[P], 1);
frmMain.GetEdit(Sender, edtHeartWoodP, Control.Initial.HeartWood[P], 1);
frmMain.GetEdit(Sender, edtCoarseRootP, Control.Initial.CoarseRoot[P], 1);
frmMain.GetEdit(Sender, edtFineRootP, Control.Initial.FineRoot[P], 1);
frmMain.GetEdit(Sender, edtBranchP, Control.Initial.Branches[P], 1);
frmMain.GetEdit(Sender, edtBarkP, Control.Initial.Bark[P], 1);
frmMain.GetEdit(Sender, edtFoliageP, Control.Initial.Leaves[P], 1);
frmMain.GetEdit(Sender, edtPollenP, Control.Initial.Pollen[P], 1);
frmMain.GetEdit(Sender, edtFruitP, Control.Initial.Fruit[P], 1);
frmMain.GetEdit(Sender, edtSolubleP, Control.Initial.Soluble[P], 1);
frmMain.GetEdit(Sender, edtReservesP, Control.Initial.Reserves[P], 1);
frmMain.GetEdit(Sender, edtWeedLeavesP, Control.Initial.WeedLeaves[P], 1);
frmMain.GetEdit(Sender, edtWeedRootsP, Control.Initial.WeedRoots[P], 1);
frmMain.GetInteger(Sender, edtPlantAge, Control.Initial.Age);
frmMain.GetEdit(Sender, edtStocking, Control.Initial.Stocking, 1);
frmMain.GetEdit(Sender, edtHeight, Control.Initial.Height, 1);
frmMain.GetEdit(Sender, edtDBH, Control.Initial.DBH, 1);
frmMain.GetInteger(Sender, edtStartDay, Control.Initial.Days);
frmMain.GetInteger(Sender, edtStartMonth, Control.Initial.Months);
frmMain.GetInteger(Sender, edtStartYear, Control.Initial.Years);
Control.ProjectHasChanged := TRUE;
End;

Procedure TfrmInitialPools.btnHelpClick(Sender: TObject);
begin
  // show the appropriate help page
    Application.HelpContext(idm_ResetPools);
end;

end.
