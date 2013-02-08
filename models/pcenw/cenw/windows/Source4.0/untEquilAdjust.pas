{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmEquilAdjust                                  =
  =                                                              =
  =             Routines to allow a manual update of some        =
  =             variables to help the program search for         =
  =             equilibrium conditions                           =
  ================================================================
  = File      : untEquilAdjust.PAS                               =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

unit untEquilAdjust;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Buttons, untDeclarations;

type
  TfrmEquilAdjust = class(TForm)
    btnOK: TBitBtn;
    lblTop: TLabel;
    lblPerc1: TLabel;
    lblPerc3: TLabel;
    lblPerc2: TLabel;
    edtAdjustVariable: TEdit;
    lblLine1: TLabel;
    edtAdjustSlow: TEdit;
    Label1: TLabel;
    edtAdjustResistant: TEdit;
    Label2: TLabel;
    Procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

  end;

var
  frmEquilAdjust: TfrmEquilAdjust;

implementation

uses untMain;

{$R *.DFM}

Procedure TfrmEquilAdjust.btnOKClick(Sender: TObject);
var Adjust: real48;
    iLayer: Integer;
    E: ElementsUsed;
Begin
frmMain.GetEdit(Sender, edtAdjustVariable, Adjust, 100);
If Control.Equil.EquilParameter = BiolNFix then
   parameter.BiolFix := Parameter.BiolFix * Adjust
Else //if Control.Equil.EquilParameter = NFraction then
   parameter.NLoss := Parameter.NLoss * Adjust;
frmMain.GetEdit(Sender, edtAdjustSlow, Adjust, 100);
For E := C to N do
    For iLayer := 0 to SoilOrganic.nLayers do
        SoilOrganic.Slow[iLayer, E] := SoilOrganic.Slow[iLayer, E] * Adjust;
frmMain.GetEdit(Sender, edtAdjustResistant, Adjust, 100);
For E := C to N do
    For iLayer := 0 to SoilOrganic.nLayers do
        SoilOrganic.Resistant[iLayer, E] := SoilOrganic.Resistant[iLayer, E] * Adjust;
Derived.Equil.ManualAdjust := true;
end;

end.
