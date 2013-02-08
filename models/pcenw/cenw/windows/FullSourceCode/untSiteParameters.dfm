object frmSiteParameters: TfrmSiteParameters
  Left = 260
  Top = 194
  BorderStyle = bsDialog
  Caption = 'Site Parameters'
  ClientHeight = 400
  ClientWidth = 578
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
  object lblLine1: TLabel
    Left = 16
    Top = 227
    Width = 193
    Height = 13
    AutoSize = False
    Caption = 'Canopy aerodynamic resistance (s m-1)'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 16
    Top = 200
    Width = 193
    Height = 13
    AutoSize = False
    Caption = 'Mulching effect on water loss (% tDW-1)'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 16
    Top = 174
    Width = 193
    Height = 13
    AutoSize = False
    Caption = 'Litter water-holding capacity (g gDW-1)'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 16
    Top = 147
    Width = 193
    Height = 13
    AutoSize = False
    Caption = 'Max. rate of soil evap.'
    WordWrap = True
  end
  object Label7: TLabel
    Left = 304
    Top = 147
    Width = 193
    Height = 14
    AutoSize = False
    Caption = 'Soil insulation effect of LAI'
    WordWrap = True
  end
  object Label8: TLabel
    Left = 304
    Top = 123
    Width = 193
    Height = 13
    AutoSize = False
    Caption = 'Maximum fractional increase in soil T'
    WordWrap = True
  end
  object Label9: TLabel
    Left = 304
    Top = 99
    Width = 193
    Height = 13
    AutoSize = False
    Caption = 'Snow insulation (r mm-1)'
    WordWrap = True
  end
  object Label10: TLabel
    Left = 304
    Top = 75
    Width = 193
    Height = 13
    AutoSize = False
    Caption = 'Resist. to soil T change'
    WordWrap = True
  end
  object Label11: TLabel
    Left = 304
    Top = 51
    Width = 193
    Height = 13
    AutoSize = False
    Caption = 'Radn melt (mm (MJ m-2)-1)'
    WordWrap = True
  end
  object Label12: TLabel
    Left = 304
    Top = 27
    Width = 193
    Height = 13
    AutoSize = False
    Caption = 'Warm melt (mm def-1 d-1)'
    WordWrap = True
  end
  object grpSystemNutrientDynamics: TGroupBox
    Left = 10
    Top = 8
    Width = 273
    Height = 129
    Caption = 'System nutrient dynamics'
    TabOrder = 0
    object Label13: TLabel
      Left = 8
      Top = 99
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'Daily release rate of applied fertiliser'
      WordWrap = True
    end
    object Label14: TLabel
      Left = 8
      Top = 75
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'Leaching fraction Nitrogen'
      WordWrap = True
    end
    object Label15: TLabel
      Left = 8
      Top = 51
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'Atmos. input (kg ha-1 yr-1) Nitrogen'
      WordWrap = True
    end
    object Label16: TLabel
      Left = 8
      Top = 27
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'Volatilisation fractions Nitrogen'
      WordWrap = True
    end
    object edtDailyReleaseRate: TEdit
      Left = 205
      Top = 95
      Width = 52
      Height = 21
      TabOrder = 3
      Text = 'Daily release rate'
    end
    object edtLeachingFraction: TEdit
      Left = 205
      Top = 71
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'Leaching fraction'
    end
    object edtAtmos_N: TEdit
      Left = 205
      Top = 47
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'Atmospheric N input'
    end
    object edtVolatilisation: TEdit
      Left = 205
      Top = 23
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'N volatilisation'
    end
  end
  object btnOK: TBitBtn
    Left = 160
    Top = 368
    Width = 75
    Height = 25
    TabOrder = 12
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 280
    Top = 368
    Width = 75
    Height = 25
    TabOrder = 13
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 400
    Top = 368
    Width = 75
    Height = 25
    HelpContext = 3500
    TabOrder = 14
    Kind = bkHelp
  end
  object grpTypeOfEvap: TGroupBox
    Left = 8
    Top = 264
    Width = 545
    Height = 89
    Caption = 'Type of direct evap. calculation'
    TabOrder = 11
    object lblSlope: TLabel
      Left = 232
      Top = 19
      Width = 193
      Height = 30
      AutoSize = False
      Caption = 
        'Slope of relationship relating waterinterception of foliage to L' +
        'AI'
      WordWrap = True
    end
    object lblFraction: TLabel
      Left = 232
      Top = 56
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'Fraction of rain lost to direct evaporation'
      WordWrap = True
    end
    object rgCalc: TRadioGroup
      Left = 8
      Top = 16
      Width = 185
      Height = 57
      ItemIndex = 0
      Items.Strings = (
        'Amount intercepted = f(LAI)'
        'Constant fraction of rain')
      TabOrder = 0
      OnClick = rgCalcClick
    end
    object edtSlopeIntercept: TEdit
      Left = 431
      Top = 20
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'SlopeIntercept'
    end
    object edtFractionLost: TEdit
      Left = 431
      Top = 52
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'Fraction lost'
    end
  end
  object edtCanopyResist: TEdit
    Left = 215
    Top = 224
    Width = 52
    Height = 21
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    TabOrder = 4
    Text = 'CanopyResist'
  end
  object edtMulching: TEdit
    Left = 215
    Top = 197
    Width = 52
    Height = 21
    TabOrder = 3
    Text = 'Mulching'
  end
  object edtLitterWHC: TEdit
    Left = 215
    Top = 170
    Width = 52
    Height = 21
    TabOrder = 2
    Text = 'LWC'
  end
  object edtMaxSoilEvap: TEdit
    Left = 215
    Top = 143
    Width = 52
    Height = 21
    TabOrder = 1
    Text = 'MaxSoilEvap'
  end
  object edtTLAISensitivity: TEdit
    Left = 503
    Top = 143
    Width = 52
    Height = 21
    TabOrder = 10
    Text = 'TLAISensitivity'
  end
  object edtMaxTBoost: TEdit
    Left = 503
    Top = 119
    Width = 52
    Height = 21
    TabOrder = 9
    Text = 'Max T Boost'
  end
  object edtSnowInsulation: TEdit
    Left = 503
    Top = 95
    Width = 52
    Height = 21
    TabOrder = 8
    Text = 'Snow insulation'
  end
  object edtResistSoil: TEdit
    Left = 503
    Top = 71
    Width = 52
    Height = 21
    TabOrder = 7
    Text = 'Resist soil'
  end
  object edtRadnMelt: TEdit
    Left = 503
    Top = 47
    Width = 52
    Height = 21
    TabOrder = 6
    Text = 'Radn melt'
  end
  object edtWarmMelt: TEdit
    Left = 503
    Top = 23
    Width = 52
    Height = 21
    TabOrder = 5
    Text = 'Warm melt'
  end
end
