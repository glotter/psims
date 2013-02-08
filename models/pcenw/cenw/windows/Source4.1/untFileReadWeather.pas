{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmFileReadWeather                              =
  =                                                              =
  =             Interface routine to control which weather       =
  =             parameters to read from an external file         =
  ================================================================
  = File      : untFileReadWeather.PAS                           =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untFileReadWeather;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TfrmFileReadWeather = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    grpDaily: TGroupBox;
    chkMax: TCheckBox;
    chkMin: TCheckBox;
    chkMean: TCheckBox;
    chkSoil: TCheckBox;
    chkPAR: TCheckBox;
    chkRainfall: TCheckBox;
    grpHumid: TGroupBox;
    chkAbs: TCheckBox;
    chkRel: TCheckBox;
    chkCO2: TCheckBox;
    rgClimate: TRadioGroup;
    chkDate: TCheckBox;
    Procedure FormShow(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmFileReadWeather: TfrmFileReadWeather;

implementation

{$R *.DFM}

uses
  untDeclarations, untMiscellaneous;

Procedure TfrmFileReadWeather.FormShow(Sender: TObject);
begin
  chkMax.Checked := WeatherFile[W_Tmax];
  chkMin.Checked := WeatherFile[W_Tmin];
  chkMean.Checked := WeatherFile[W_Tmean];
  chkSoil.Checked := WeatherFile[W_Tsoil];
  chkPAR.Checked := WeatherFile[W_Radn];
  chkRainfall.Checked := WeatherFile[W_Rain];
  chkAbs.Checked := WeatherFile[W_AbsHum];
  chkRel.Checked := WeatherFile[W_RelHum];
  chkCO2.Checked := WeatherFile[W_CO2];
  chkDate.Checked := WeatherFile[W_Date];
  case Control.ClimType of
    'O': rgClimate.ItemIndex := 0;
    'U': rgClimate.ItemIndex := 1;
    'S': rgClimate.ItemIndex := 2;
  end;
end;

Procedure TfrmFileReadWeather.btnOKClick(Sender: TObject);
begin
  WeatherFile[W_Tmax] := chkMax.Checked;
  WeatherFile[W_Tmin] := chkMin.Checked;
  WeatherFile[W_Tmean] := chkMean.Checked;
  WeatherFile[W_Tsoil] := chkSoil.Checked;
  WeatherFile[W_Radn] := chkPAR.Checked;
  WeatherFile[W_Rain] := chkRainfall.Checked;
  WeatherFile[W_AbsHum] := chkAbs.Checked;
  WeatherFile[W_RelHum] := chkRel.Checked;
  WeatherFile[W_CO2] := chkCO2.Checked;
  WeatherFile[W_Date] := chkDate.Checked;
  case rgClimate.ItemIndex of
    0: Control.ClimType := 'O';
    1: Control.ClimType := 'U';
    2: Control.ClimType := 'S';
  end;
  Control.ProjectHasChanged := TRUE;
end;

end.
