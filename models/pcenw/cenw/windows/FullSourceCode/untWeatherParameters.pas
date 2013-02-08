{ ================================================================
  = Project   : CENW                                             =
  ================================================================
  = Modules   : TfrmMain                                         =
  =                                                              =
  =             This dialogue unit handles the change of         =
  =             weather-related parameters (mainly for use       =
  =             if no actual weather data are available.         =
  ================================================================
  = File      : untWeatherParameters.PAS                         =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

unit untWeatherParameters;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, untFieldValidation;

type
  TfrmWeatherParameters = class(TForm)
    btnOK: TbitBtn;
    btnCancel: TbitBtn;
    btnHelp: TBitBtn;
    grpConstantandSimulated: TGroupBox;
    grp_SimulatedClimate: TGroupBox;
    edtRadn_Amplitude: TEdit;
    Label0: TLabel;
    Label1: TLabel;
    edtHumid_Amplitude: TEdit;
    Label2: TLabel;
    edtTemp_Amplitude: TEdit;
    edtAnnualRain: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    edtMeanRadn: TEdit;
    Label5: TLabel;
    edtMeanAbsHum: TEdit;
    Label6: TLabel;
    edtRainProb: TEdit;
    Label7: TLabel;
    edtMeanTmax: TEdit;
    edtMeanTmin: TEdit;
    Label8: TLabel;
    edtMeanSoilTemp: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    edtCO2Conc: TEdit;
    Label11: TLabel;
    edtLatitude: TEdit;
    Label12: TLabel;
    edtAtmosPressure: TEdit;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmWeatherParameters: TfrmWeatherParameters;

implementation

{$R *.DFM}
uses
  untDeclarations, untMiscellaneous, untMain;

procedure TfrmWeatherParameters.FormShow(Sender: TObject);
   Begin
   frmMain.FillEdit(Sender, edtCO2Conc, Parameter.CO2Conc, 1);
   frmMain.FillEdit(Sender, edtAtmosPressure, Parameter.AtmosPressure, 1);
   frmMain.FillEdit(Sender, edtMeanSoilTemp, Parameter.MeanSoilTemp, 1);
   frmMain.FillEdit(Sender, edtLatitude, Parameter.Latitude, 1);
   frmMain.FillEdit(Sender, edtAnnualRain, Parameter.AnnualRain, 1);
   frmMain.FillEdit(Sender, edtRainProb, Parameter.RainProb, 1);
   frmMain.FillEdit(Sender, edtMeanRadn, Parameter.MeanRadn, 1);
   frmMain.FillEdit(Sender, edtMeanAbsHum, Parameter.MeanAbsHum, 1000);
   frmMain.FillEdit(Sender, edtMeanTmin, Parameter.MeanTmin, 1);
   frmMain.FillEdit(Sender, edtMeanTmax, Parameter.MeanTmax, 1);
   frmMain.FillEdit(Sender, edtTemp_Amplitude, Parameter.Temp_Amplitude, 1);
   frmMain.FillEdit(Sender, edtHumid_Amplitude, Parameter.Humid_Amplitude, 1000);
   frmMain.FillEdit(Sender, edtRadn_Amplitude, Parameter.Radn_Amplitude, 1);
   end;

procedure TfrmWeatherParameters.btnOKClick(Sender: TObject);
 Begin
 frmMain.GetEdit(Sender, edtCO2Conc, Parameter.CO2Conc, 1);
 frmMain.GetEdit(Sender, edtAtmosPressure, Parameter.AtmosPressure, 1);
 frmMain.GetEdit(Sender, edtMeanSoilTemp, Parameter.MeanSoilTemp, 1);
 frmMain.GetEdit(Sender, edtLatitude, Parameter.Latitude, 1);
 If Parameter.Latitude > 0 then // Northern hemisphere
      Begin
      Parameter.WarmestDay := 201;
      Parameter.MostPAR := 173;
      End
   Else                           // Southern hemisphere
      Begin
      Parameter.WarmestDay := 19;
      Parameter.MostPAR := 356;
      End;
 frmMain.GetEdit(Sender, edtAnnualRain, Parameter.AnnualRain, 1);
 frmMain.GetEdit(Sender, edtRainProb, Parameter.RainProb, 1);
 frmMain.GetEdit(Sender, edtMeanTmax, Parameter.MeanTmax, 1);
 frmMain.GetEdit(Sender, edtMeanTmin, Parameter.MeanTmin, 1);
 frmMain.GetEdit(Sender, edtMeanRadn, Parameter.MeanRadn, 1);
 frmMain.GetEdit(Sender, edtMeanAbsHum, Parameter.MeanAbsHum, 1000);
 frmMain.GetEdit(Sender, edtTemp_Amplitude, Parameter.Temp_Amplitude, 1);
 frmMain.GetEdit(Sender, edtHumid_Amplitude, Parameter.Humid_Amplitude, 1000);
 frmMain.GetEdit(Sender, edtRadn_Amplitude, Parameter.Radn_Amplitude, 1);
 Control.SiteHasChanged := TRUE;
 end;

end.
