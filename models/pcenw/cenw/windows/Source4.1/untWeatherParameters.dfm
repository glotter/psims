object frmWeatherParameters: TfrmWeatherParameters
  Left = 380
  Top = 230
  BorderStyle = bsDialog
  Caption = 'Weather Parameters'
  ClientHeight = 409
  ClientWidth = 416
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label9: TLabel
    Left = 24
    Top = 48
    Width = 105
    Height = 25
    AutoSize = False
    Caption = 'Soil temperature for constant-climate runs'
    WordWrap = True
  end
  object Label10: TLabel
    Left = 24
    Top = 24
    Width = 89
    Height = 13
    AutoSize = False
    Caption = 'CO2 concentration'
    WordWrap = True
  end
  object Label11: TLabel
    Left = 232
    Top = 59
    Width = 41
    Height = 13
    AutoSize = False
    Caption = 'Latitude'
    WordWrap = True
  end
  object Label12: TLabel
    Left = 232
    Top = 19
    Width = 65
    Height = 30
    AutoSize = False
    Caption = 'Atmospheric pressure'
    WordWrap = True
  end
  object btnOK: TBitBtn
    Left = 32
    Top = 344
    Width = 75
    Height = 25
    TabOrder = 6
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 136
    Top = 344
    Width = 75
    Height = 25
    TabOrder = 7
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 240
    Top = 344
    Width = 75
    Height = 25
    HelpContext = 3600
    TabOrder = 8
    Kind = bkHelp
  end
  object grpConstantandSimulated: TGroupBox
    Left = 20
    Top = 88
    Width = 373
    Height = 113
    Caption = 'For constant and simulated climate'
    TabOrder = 4
    object Label3: TLabel
      Left = 24
      Top = 24
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Annual rainfall'
      WordWrap = True
    end
    object Label4: TLabel
      Left = 24
      Top = 53
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Mean radiation'
      WordWrap = True
    end
    object Label5: TLabel
      Left = 24
      Top = 78
      Width = 73
      Height = 25
      AutoSize = False
      Caption = 'Mean Absolute humidity'
      WordWrap = True
    end
    object Label6: TLabel
      Left = 208
      Top = 19
      Width = 73
      Height = 30
      AutoSize = False
      Caption = 'Daily rainfall probability'
      WordWrap = True
    end
    object Label7: TLabel
      Left = 208
      Top = 53
      Width = 89
      Height = 13
      AutoSize = False
      Caption = 'Mean daily max T'
      WordWrap = True
    end
    object Label8: TLabel
      Left = 208
      Top = 83
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Mean daily min T'
      WordWrap = True
    end
    object edtAnnualRain: TEdit
      Left = 129
      Top = 20
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Rainfall'
    end
    object edtMeanRadn: TEdit
      Left = 129
      Top = 49
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'Radiation'
    end
    object edtMeanAbsHum: TEdit
      Left = 129
      Top = 80
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'Absolute humidity'
    end
    object edtRainProb: TEdit
      Left = 297
      Top = 20
      Width = 52
      Height = 21
      TabOrder = 3
      Text = 'Rain probability'
    end
    object edtMeanTmax: TEdit
      Left = 297
      Top = 49
      Width = 52
      Height = 21
      TabOrder = 4
      Text = 'Mean T max'
    end
    object edtMeanTmin: TEdit
      Left = 297
      Top = 80
      Width = 52
      Height = 21
      TabOrder = 5
      Text = 'Mean T min'
    end
  end
  object grp_SimulatedClimate: TGroupBox
    Left = 20
    Top = 216
    Width = 237
    Height = 105
    Caption = 'Only for simulated climate'
    TabOrder = 5
    object Label0: TLabel
      Left = 16
      Top = 72
      Width = 145
      Height = 13
      AutoSize = False
      Caption = 'Annual radiation amplitude'
      WordWrap = True
    end
    object Label1: TLabel
      Left = 16
      Top = 48
      Width = 145
      Height = 13
      AutoSize = False
      Caption = 'Annual humidity amplitude'
      WordWrap = True
    end
    object Label2: TLabel
      Left = 16
      Top = 24
      Width = 145
      Height = 13
      AutoSize = False
      Caption = 'Annual temperature amplitude'
      WordWrap = True
    end
    object edtRadn_Amplitude: TEdit
      Left = 169
      Top = 68
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'Radn amplitude'
    end
    object edtHumid_Amplitude: TEdit
      Left = 169
      Top = 44
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'Humidity amplitude'
    end
    object edtTemp_Amplitude: TEdit
      Left = 169
      Top = 20
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Temperature amplitude'
    end
  end
  object edtMeanSoilTemp: TEdit
    Left = 149
    Top = 52
    Width = 52
    Height = 21
    TabOrder = 1
    Text = 'Mean soil temp'
  end
  object edtCO2Conc: TEdit
    Left = 149
    Top = 20
    Width = 52
    Height = 21
    TabOrder = 0
    Text = 'CO2 concentration'
  end
  object edtLatitude: TEdit
    Left = 317
    Top = 52
    Width = 52
    Height = 21
    TabOrder = 3
    Text = 'Latitude'
  end
  object edtAtmosPressure: TEdit
    Left = 317
    Top = 20
    Width = 52
    Height = 21
    TabOrder = 2
    Text = 'Atmospheric Pressure'
  end
end
