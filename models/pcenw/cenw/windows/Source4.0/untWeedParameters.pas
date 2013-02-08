{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmWeedParameters                               =
  =                                                              =
  =             This dialogue unit handles the change of         =
  =             Weed-related parameters (if that is used)        =
  ================================================================
  = File      : untWeedParameters.PAS                            =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untWeedParameters;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, untFieldValidation;

type
  TfrmWeedParameters = class(TForm)
    btnOK: TbitBtn;
    btnCancel: TbitBtn;
    btnHelp: TBitBtn;
    edtSenescence: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    edtMaxheight: TEdit;
    Label11: TLabel;
    edtLUE: TEdit;
    Label12: TLabel;
    edtKMheight: TEdit;
    edtKMRootPlantNUptake: TEdit;
    edtKMRootPlantPUptake: TEdit;
    Label2: TLabel;
    Label1: TLabel;
    Procedure FormShow(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var frmWeedParameters: TfrmWeedParameters;

implementation

{$R *.DFM}
uses untDeclarations, untMiscellaneous, untMain;

Procedure TfrmWeedParameters.FormShow(Sender: TObject);
Begin
frmMain.FillEdit(Sender, edtMaxHeight, Parameter.Weed.MaxHeight, 1);
frmMain.FillEdit(Sender, edtKMHeight, Parameter.Weed.KMHeight, Control.CConversion * 0.001);
frmMain.FillEdit(Sender, edtSenescence, Parameter.Weed.Senescence, 365);
frmMain.FillEdit(Sender, edtLUE, Parameter.Weed.AllocLeaves, 1);
frmMain.FillEdit(Sender, edtKMRootPlantNUptake, Parameter.Weed.KMRootPlantNUptake, Control.CConversion * 0.001);
frmMain.FillEdit(Sender, edtKMRootPlantPUptake, Parameter.Weed.KMRootPlantPUptake, Control.CConversion * 0.001);
end;

Procedure TfrmWeedParameters.btnOKClick(Sender: TObject);
Begin
frmMain.GetEdit(Sender, edtMaxHeight, Parameter.Weed.MaxHeight, 1);
frmMain.GetEdit(Sender, edtKMHeight, Parameter.Weed.KMHeight, Control.CConversion * 0.001);
frmMain.GetEdit(Sender, edtSenescence, Parameter.Weed.Senescence, 365);
frmMain.GetEdit(Sender, edtLUE, Parameter.Weed.AllocLeaves, 1);
frmMain.GetEdit(Sender, edtKMRootPlantNUptake, Parameter.Weed.KMRootPlantNUptake, Control.CConversion * 0.001);
frmMain.GetEdit(Sender, edtKMRootPlantPUptake, Parameter.Weed.KMRootPlantPUptake, Control.CConversion * 0.001);
Control.PlantHasChanged := TRUE;
end;

end.
