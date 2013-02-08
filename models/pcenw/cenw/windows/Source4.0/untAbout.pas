{ ================================================================
  = Project   : Cenw                                             =
  ================================================================
  = Module    : TfrmAbout                                        =
  =                                                              =
  =             Routines to give basic information               =
  =             about CenW.                                      =
  ================================================================
  = File      : untAbout.PAS                                     =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untAbout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TfrmAbout = class(TForm)
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Panel2: TPanel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Panel3: TPanel;
    Label16: TLabel;
    Label17: TLabel;
    Panel4: TPanel;
    Image1: TImage;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.DFM}

end.
