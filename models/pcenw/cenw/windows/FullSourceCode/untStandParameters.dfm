object frmStandParameters: TfrmStandParameters
  Left = 209
  Top = 126
  BorderStyle = bsDialog
  Caption = 'Stand Parameters'
  ClientHeight = 556
  ClientWidth = 588
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
  object LblDeathRatio: TLabel
    Left = 96
    Top = 19
    Width = 73
    Height = 30
    AutoSize = False
    Caption = 'Ratio dieing to average trees'
    WordWrap = True
  end
  object lblStemDeath: TLabel
    Left = 96
    Top = 59
    Width = 89
    Height = 13
    AutoSize = False
    Caption = 'Mortality fraction'
    WordWrap = True
  end
  object lbl32PowerLaw: TLabel
    Left = 96
    Top = 83
    Width = 73
    Height = 30
    AutoSize = False
    Caption = 'Parameter in 3/2 power law'
    WordWrap = True
  end
  object Label12: TLabel
    Left = 256
    Top = 147
    Width = 89
    Height = 46
    AutoSize = False
    Caption = 'Ratio of average [N] in foliage and [N] in top layer'
    WordWrap = True
  end
  object Label13: TLabel
    Left = 432
    Top = 147
    Width = 81
    Height = 46
    AutoSize = False
    Caption = 'Ratio of [N[ in senescing and living foliage'
    WordWrap = True
  end
  object Label14: TLabel
    Left = 432
    Top = 227
    Width = 73
    Height = 30
    AutoSize = False
    Caption = 'Wood density (kg m-3)'
    WordWrap = True
  end
  object Label15: TLabel
    Left = 16
    Top = 283
    Width = 145
    Height = 30
    AutoSize = False
    Caption = 'Fraction of soil microbial N taken up directly (g d-1 kg-1)'
    WordWrap = True
  end
  object Label16: TLabel
    Left = 16
    Top = 321
    Width = 153
    Height = 30
    AutoSize = False
    Caption = 'Maximum daily death rate of foliage during drought (% d-1)'
    WordWrap = True
  end
  object Label17: TLabel
    Left = 264
    Top = 283
    Width = 153
    Height = 30
    AutoSize = False
    Caption = 'Amount of N biologically fixed per unit C fixed (gN kgC-1)'
    WordWrap = True
  end
  object Label18: TLabel
    Left = 264
    Top = 323
    Width = 145
    Height = 30
    AutoSize = False
    Caption = 'Relative soil water content that starts plant water stress'
    WordWrap = True
  end
  object btnOK: TBitBtn
    Left = 208
    Top = 520
    Width = 75
    Height = 25
    TabOrder = 16
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 288
    Top = 520
    Width = 75
    Height = 25
    TabOrder = 17
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 368
    Top = 520
    Width = 75
    Height = 25
    HelpContext = 3200
    TabOrder = 18
    Kind = bkHelp
  end
  object grpTypeOfEvap: TGroupBox
    Left = 8
    Top = 360
    Width = 497
    Height = 145
    Caption = 'Respiration'
    TabOrder = 15
    object lblRespnRatio: TLabel
      Left = 32
      Top = 91
      Width = 97
      Height = 13
      AutoSize = False
      Caption = 'Respiration ratio'
      WordWrap = True
    end
    object lblRespFromN: TLabel
      Left = 232
      Top = 19
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'Respiration rate per unit N'
      WordWrap = True
    end
    object lblbeta: TLabel
      Left = 232
      Top = 43
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'beta parameter in T response (x1000)'
      WordWrap = True
    end
    object lblRespnOpt: TLabel
      Left = 232
      Top = 67
      Width = 193
      Height = 14
      AutoSize = False
      Caption = 'Temperature for max respiration rate'
      WordWrap = True
    end
    object lblGrowthRespn: TLabel
      Left = 232
      Top = 91
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'Growth respiration'
      WordWrap = True
    end
    object lblRespnAdjust: TLabel
      Left = 232
      Top = 115
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'Time constant for temp adjustment'
      WordWrap = True
    end
    object rgCalc: TRadioGroup
      Left = 8
      Top = 16
      Width = 185
      Height = 57
      ItemIndex = 0
      Items.Strings = (
        'Calc. from/ basics'
        'Calc. as ratio')
      TabOrder = 0
      OnClick = rgCalcClick
    end
    object chkAcclimation: TCheckBox
      Left = 8
      Top = 118
      Width = 177
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Include temperature acclimation'
      TabOrder = 2
      OnClick = chkAcclimationClick
    end
    object edtRespnRatio: TEdit
      Left = 135
      Top = 87
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'Respiration ratio'
    end
    object edtRespFromN: TEdit
      Left = 431
      Top = 15
      Width = 52
      Height = 21
      TabOrder = 3
      Text = 'Respiration rate per unit N'
    end
    object edtbeta: TEdit
      Left = 431
      Top = 39
      Width = 52
      Height = 21
      TabOrder = 4
      Text = 'Beta parameter in T response'
    end
    object edtRespnOpt: TEdit
      Left = 431
      Top = 63
      Width = 52
      Height = 21
      TabOrder = 5
      Text = 'T for maximum respiration'
    end
    object edtGrowthRespn: TEdit
      Left = 431
      Top = 87
      Width = 52
      Height = 21
      TabOrder = 6
      Text = 'Growth respiration'
    end
    object edtRespnAdjust: TEdit
      Left = 431
      Top = 111
      Width = 52
      Height = 21
      TabOrder = 7
      Text = 'Time constant for acclimation'
    end
  end
  object rgMortalityType: TRadioGroup
    Left = 8
    Top = 8
    Width = 81
    Height = 97
    Caption = 'Mortality'
    Items.Strings = (
      'Fraction'
      'Density'
      'Both')
    TabOrder = 0
    OnClick = rgMortalityTypeClick
  end
  object grpFoliageSenesc: TGroupBox
    Left = 8
    Top = 128
    Width = 225
    Height = 73
    Caption = 'Foliage senescence in dense canopy'
    TabOrder = 5
    object Label10: TLabel
      Left = 16
      Top = 19
      Width = 137
      Height = 13
      AutoSize = False
      Caption = 'Low light limit (MJ m-2 d-1)'
      WordWrap = True
    end
    object Label11: TLabel
      Left = 16
      Top = 43
      Width = 137
      Height = 13
      AutoSize = False
      Caption = 'Max daily senescence'
      WordWrap = True
    end
    object edtSenescLowLight: TEdit
      Left = 159
      Top = 16
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Senescence limit in low light'
    end
    object edtMaxDailySenesc: TEdit
      Left = 159
      Top = 40
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'Daily senescence limit in low light'
    end
  end
  object grpAnnualSenesc: TGroupBox
    Left = 248
    Top = 8
    Width = 321
    Height = 121
    Caption = 'Annual senescence'
    TabOrder = 4
    object Label3: TLabel
      Left = 16
      Top = 19
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Foliage minimum'
      WordWrap = True
    end
    object Label4: TLabel
      Left = 16
      Top = 43
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Branches'
      WordWrap = True
    end
    object Label5: TLabel
      Left = 16
      Top = 67
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Fine roots'
      WordWrap = True
    end
    object Label6: TLabel
      Left = 192
      Top = 19
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Bark'
      WordWrap = True
    end
    object Label7: TLabel
      Left = 192
      Top = 43
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Fruit'
      WordWrap = True
    end
    object Label8: TLabel
      Left = 192
      Top = 67
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Pollen'
      WordWrap = True
    end
    object Label9: TLabel
      Left = 120
      Top = 91
      Width = 113
      Height = 13
      AutoSize = False
      Caption = 'Longevity of sapwood'
      WordWrap = True
    end
    object edtLeafSenesc: TEdit
      Left = 103
      Top = 15
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Foliage senescence'
    end
    object edtBranchSenesc: TEdit
      Left = 103
      Top = 39
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'Branch senescence'
    end
    object edtRootSenesc: TEdit
      Left = 103
      Top = 63
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'Root senescence'
    end
    object edtBarkSenesc: TEdit
      Left = 263
      Top = 15
      Width = 52
      Height = 21
      TabOrder = 3
      Text = 'Bark senescence'
    end
    object edtFruitSenesc: TEdit
      Left = 263
      Top = 39
      Width = 52
      Height = 21
      TabOrder = 4
      Text = 'Fruit senescence'
    end
    object edtPollenSenesc: TEdit
      Left = 263
      Top = 63
      Width = 52
      Height = 21
      TabOrder = 5
      Text = 'Pollen senescence'
    end
    object edtSapwoodYears: TEdit
      Left = 263
      Top = 87
      Width = 52
      Height = 21
      TabOrder = 6
      Text = 'Sapwood duration'
    end
  end
  object grpUseCarbo: TGroupBox
    Left = 8
    Top = 208
    Width = 409
    Height = 65
    Caption = 'Use of carbohydrate and soluble nitrogen'
    TabOrder = 8
    object Label25: TLabel
      Left = 40
      Top = 22
      Width = 105
      Height = 30
      AutoSize = False
      Caption = 'Carbohydrate Km as % of live tissue'
      WordWrap = True
    end
    object Label26: TLabel
      Left = 240
      Top = 22
      Width = 97
      Height = 30
      AutoSize = False
      Caption = 'Soluble N Km as % of live tissue'
      WordWrap = True
    end
    object edtKmGrowthC: TEdit
      Left = 159
      Top = 25
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Km (CH2O)'
    end
    object edtKmGrowthN: TEdit
      Left = 343
      Top = 25
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'Km (soluble N)'
    end
  end
  object chkVariableNFix: TCheckBox
    Left = 488
    Top = 288
    Width = 97
    Height = 17
    Caption = 'Variable N fix'#39'n'
    TabOrder = 13
  end
  object edtDeathRatio: TEdit
    Left = 183
    Top = 23
    Width = 52
    Height = 21
    TabOrder = 1
    Text = 'Death ratio'
  end
  object edtStemDeath: TEdit
    Left = 183
    Top = 55
    Width = 52
    Height = 21
    TabOrder = 2
    Text = 'Mortality fraction'
  end
  object edt32PowerLaw: TEdit
    Left = 183
    Top = 87
    Width = 52
    Height = 21
    TabOrder = 3
    Text = 'Self-thinning law'
  end
  object edtInternalNRatio: TEdit
    Left = 351
    Top = 157
    Width = 52
    Height = 21
    TabOrder = 6
    Text = 'Internal N ratio'
  end
  object edtSenescLeafRatio: TEdit
    Left = 511
    Top = 157
    Width = 52
    Height = 21
    TabOrder = 7
    Text = 'Senescence N ratio'
  end
  object edtWoodDensity: TEdit
    Left = 511
    Top = 229
    Width = 52
    Height = 21
    TabOrder = 9
    Text = 'Wood density'
  end
  object edtMicroFract: TEdit
    Left = 175
    Top = 285
    Width = 52
    Height = 21
    TabOrder = 10
    Text = 'Mycorrhizal fraction'
  end
  object edtDrySenesc: TEdit
    Left = 175
    Top = 325
    Width = 52
    Height = 21
    TabOrder = 11
    Text = 'Drought senescence'
  end
  object edtBiolFix: TEdit
    Left = 415
    Top = 285
    Width = 52
    Height = 21
    TabOrder = 12
    Text = 'Biological N fixation'
  end
  object edtStressLimit: TEdit
    Left = 415
    Top = 325
    Width = 52
    Height = 21
    TabOrder = 14
    Text = 'Water-stress limit'
  end
end
