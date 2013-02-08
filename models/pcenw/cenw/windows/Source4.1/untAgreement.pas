{ ================================================================
  = Project   : Cenw                                             =
  ================================================================
  = Module    : TfrmAgreement                                    =
  =                                                              =
  =             Simple routine to handle the licence agreement   =
  =             that user have to agree to before being able     =
  =             to run the program.                              =
  ================================================================
  = File      : untAgreement.PAS                                 =
  =                                                              =
  = Version   : 4.1                                              =
  ================================================================ }

unit untAgreement;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfrmAgreement = class(TForm)
    lblLicence: TLabel;
    edtLicence: TMemo;
    btnAgree: TButton;
    btnNotAgree: TButton;
    Procedure btnAgreeClick(Sender: TObject);
    Procedure btnNotAgreeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAgreement: TfrmAgreement;

implementation

{$R *.DFM}

uses
  untDeclarations;

Procedure TfrmAgreement.btnAgreeClick(Sender: TObject);
begin
  Control.AgreeChecked := true;
  Control.AgreeOK := true;
  Close;
end;

Procedure TfrmAgreement.btnNotAgreeClick(Sender: TObject);
begin
  Control.AgreeChecked := true;
  Control.AgreeOK := false;
  Close;
end;

end.
