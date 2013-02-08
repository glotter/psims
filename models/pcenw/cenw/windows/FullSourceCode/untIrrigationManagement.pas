{ ================================================================
  = Project   : CENW                                             =
  ================================================================
  = Module    : TfrmIrrigationManagement                         =
  =                                                              =
  =             Interface routine to control                     =
  =             irrigation application.                          =
  ================================================================
  = File      : untIrrigationManagement.PAS                      =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

unit untIrrigationManagement;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TfrmIrrigationManagement = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    chkIrrigation: TCheckBox;
    rgIrrigationType: TRadioGroup;
    grpIrrigationInterval: TGroupBox;
    lblLine1: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    edtIrrigationFraction: TEdit;
    lblIrrigationFraction: TLabel;
    edtIrrigationAmount: TEdit;
    lblIrrigationAmount: TLabel;
    edtDays: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    edtMonths: TEdit;
    edtYears: TEdit;
    Label7: TLabel;
    edtStartDay: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    edtStartMonth: TEdit;
    Label10: TLabel;
    edtStartYear: TEdit;
    edtEndDay: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    edtEndMonth: TEdit;
    Label13: TLabel;
    edtEndYear: TEdit;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure rgIrrigationTypeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmIrrigationManagement: TfrmIrrigationManagement;

implementation

{$R *.DFM}

uses
  untDeclarations, untMiscellaneous, untMain;

procedure TfrmIrrigationManagement.FormShow(Sender: TObject);
var Days, Months, Years: Integer;

  Procedure Calc_Interval (n: integer; Var Days, Months, Years: integer);
  Begin
  Years := n div 365;
  n := n - Years * 365;
  Months := n div 30;
  n := n - Months * 30;
  Days := n;
  End; {of Procedure 'Calc_Interval'}

Begin
ChkIrrigation.Checked := Event.Irrigate;
frmMain.FillEdit(Sender, edtIrrigationFraction, Event.IrrigationFraction, 100);
frmMain.FillEdit(Sender, edtIrrigationAmount, Event.IrrigationAmount, 1);
If Event.IrrigationType = 'R' then
   Begin
   rgIrrigationType.ItemIndex := 1;
   edtIrrigationFraction.enabled := false;
   lblIrrigationFraction.enabled := false;
   edtIrrigationAmount.enabled := true;
   lblIrrigationAmount.enabled := true;
   End
Else
   Begin
   rgIrrigationType.ItemIndex := 0;
   edtIrrigationFraction.enabled := true;
   lblIrrigationFraction.enabled := true;
   edtIrrigationAmount.enabled := false;
   lblIrrigationAmount.enabled := false;
   End;
Calc_Interval (Event.IrrigationInterval, Days, Months, Years);
frmMain.FillEdit(Sender, edtDays, Days, 0);
frmMain.FillEdit(Sender, edtMonths, Months, 0);
frmMain.FillEdit(Sender, edtYears, Years, 0);
frmMain.FillEdit(Sender, edtStartDay, Event.IrrigStartDay, 0);
frmMain.FillEdit(Sender, edtStartMonth, Event.IrrigStartMonth, 0);
frmMain.FillEdit(Sender, edtStartYear, Event.IrrigStartYear, 0);
frmMain.FillEdit(Sender, edtEndDay, Event.IrrigEndDay, 0);
frmMain.FillEdit(Sender, edtEndMonth, Event.IrrigEndMonth, 0);
frmMain.FillEdit(Sender, edtEndYear, Event.IrrigEndYear, 0);
End;

Procedure TfrmIrrigationManagement.btnOKClick(Sender: TObject);
var Days, Months, Years: Integer;
Begin
Event.Irrigate := ChkIrrigation.Checked;
frmMain.GetEdit(Sender, edtIrrigationFraction, Event.IrrigationFraction, 100);
frmMain.GetEdit(Sender, edtIrrigationAmount, Event.IrrigationAmount, 1);
frmMain.GetInteger(Sender, edtDays, Days);
frmMain.GetInteger(Sender, edtMonths, Months);
frmMain.GetInteger(Sender, edtYears, Years);
Event.IrrigationInterval := Days + 30 * Months + 365 * Years;
frmMain.GetInteger(Sender, edtStartDay, Event.IrrigStartDay);
frmMain.GetInteger(Sender, edtStartMonth, Event.IrrigStartMonth);
frmMain.GetInteger(Sender, edtStartYear, Event.IrrigStartYear);
frmMain.GetInteger(Sender, edtEndDay, Event.IrrigEndDay);
frmMain.GetInteger(Sender, edtEndMonth, Event.IrrigEndMonth);
frmMain.GetInteger(Sender, edtEndYear, Event.IrrigEndYear);
If Event.IrrigStartYear < 0 then
   Begin
   Event.IrrigStartYear := -1;
   Event.IrrigStartDay := 0;
   Event.IrrigStartMonth := 0;
   End;
If Event.IrrigEndYear < 0 then
   Begin
   Event.IrrigEndYear := -1;
   Event.IrrigEndDay := 0;
   Event.IrrigEndMonth := 0;
   End;
Control.ProjectHasChanged := TRUE;
End;

Procedure TfrmIrrigationManagement.rgIrrigationTypeClick(Sender: TObject);
Begin
If rgIrrigationType.ItemIndex = 1 then
   Begin
   Event.IrrigationType := 'R';
   edtIrrigationFraction.enabled := false;
   lblIrrigationFraction.enabled := false;
   edtIrrigationAmount.enabled := true;
   lblIrrigationAmount.enabled := true;
   End
Else if rgIrrigationType.ItemIndex = 0 then
   Begin
   Event.IrrigationType := 'S';
   edtIrrigationFraction.enabled := true;
   lblIrrigationFraction.enabled := true;
   edtIrrigationAmount.enabled := false;
   lblIrrigationAmount.enabled := false;
   End;
End;

end.
