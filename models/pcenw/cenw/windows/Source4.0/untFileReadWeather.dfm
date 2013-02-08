object frmFileReadWeather: TfrmFileReadWeather
  Left = 277
  Top = 194
  BorderStyle = bsDialog
  Caption = 'Read Weather Variables from File'
  ClientHeight = 293
  ClientWidth = 251
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
  object btnOK: TBitBtn
    Left = 8
    Top = 256
    Width = 75
    Height = 25
    TabOrder = 6
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 88
    Top = 256
    Width = 75
    Height = 25
    TabOrder = 7
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 168
    Top = 256
    Width = 75
    Height = 25
    HelpContext = 1600
    TabOrder = 8
    Kind = bkHelp
  end
  object grpDaily: TGroupBox
    Left = 8
    Top = 8
    Width = 233
    Height = 65
    Caption = 'Daily Temperatures'
    TabOrder = 0
    object chkMax: TCheckBox
      Left = 8
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Maximum'
      TabOrder = 0
    end
    object chkMin: TCheckBox
      Left = 8
      Top = 40
      Width = 97
      Height = 17
      Caption = 'Minimum'
      TabOrder = 1
    end
    object chkMean: TCheckBox
      Left = 120
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Mean'
      TabOrder = 2
    end
    object chkSoil: TCheckBox
      Left = 120
      Top = 40
      Width = 97
      Height = 17
      Caption = 'Soil'
      TabOrder = 3
    end
  end
  object chkPAR: TCheckBox
    Left = 16
    Top = 88
    Width = 97
    Height = 17
    Caption = 'Solar radiation'
    TabOrder = 1
  end
  object chkRainfall: TCheckBox
    Left = 128
    Top = 88
    Width = 97
    Height = 17
    Caption = 'Rainfall'
    TabOrder = 2
  end
  object grpHumid: TGroupBox
    Left = 8
    Top = 112
    Width = 233
    Height = 41
    Caption = 'Humidities'
    TabOrder = 3
    object chkAbs: TCheckBox
      Left = 8
      Top = 16
      Width = 81
      Height = 17
      Caption = 'Absolute'
      TabOrder = 0
    end
    object chkRel: TCheckBox
      Left = 120
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Relative'
      TabOrder = 1
    end
  end
  object chkCO2: TCheckBox
    Left = 16
    Top = 168
    Width = 89
    Height = 17
    Caption = 'CO2 Conc'
    TabOrder = 4
  end
  object rgClimate: TRadioGroup
    Left = 120
    Top = 160
    Width = 121
    Height = 81
    Caption = 'Climate Data'
    Items.Strings = (
      'Observed'
      'Constant'
      'Simulated')
    TabOrder = 5
  end
end
