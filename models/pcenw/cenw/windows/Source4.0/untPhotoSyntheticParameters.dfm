object frmPhotoSyntheticParameters: TfrmPhotoSyntheticParameters
  Left = 249
  Top = 121
  BiDiMode = bdRightToLeft
  BorderStyle = bsDialog
  Caption = 'Photosynthetic Parameters'
  ClientHeight = 557
  ClientWidth = 930
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ParentBiDiMode = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblLine1: TLabel
    Left = 16
    Top = 19
    Width = 89
    Height = 13
    AutoSize = False
    Caption = 'Specific Leaf Area'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 184
    Top = 19
    Width = 81
    Height = 13
    AutoSize = False
    Caption = 'Foliage albedo'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 344
    Top = 19
    Width = 73
    Height = 13
    AutoSize = False
    Caption = 'Transmissivity'
    WordWrap = True
  end
  object Label23: TLabel
    Left = 496
    Top = 19
    Width = 89
    Height = 13
    AutoSize = False
    Caption = 'Loss as NMVOC'
    WordWrap = True
  end
  object Label16: TLabel
    Left = 521
    Top = 310
    Width = 169
    Height = 13
    AutoSize = False
    Caption = 'Max length for complete repair'
    WordWrap = True
  end
  object btnOK: TBitBtn
    Left = 303
    Top = 464
    Width = 75
    Height = 25
    TabOrder = 14
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 399
    Top = 464
    Width = 75
    Height = 25
    TabOrder = 15
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 494
    Top = 464
    Width = 75
    Height = 25
    HelpContext = 3800
    TabOrder = 16
    Kind = bkHelp
  end
  object grpLeafPhotosyntheticParameters: TGroupBox
    Left = 16
    Top = 48
    Width = 473
    Height = 107
    Caption = 'Leaf photosynthetic parameters (top of canopy)'
    TabOrder = 4
    object Label5: TLabel
      Left = 12
      Top = 57
      Width = 57
      Height = 13
      AutoSize = False
      Caption = 'No (g kg-1)'
      WordWrap = True
    end
    object Label6: TLabel
      Left = 160
      Top = 57
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Saturating [N]'
      WordWrap = True
    end
    object Label7: TLabel
      Left = 320
      Top = 57
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Max [N] (g kg-1)'
      WordWrap = True
    end
    object Label8: TLabel
      Left = 328
      Top = 15
      Width = 73
      Height = 29
      AutoSize = False
      BiDiMode = bdLeftToRight
      Caption = 'Curvature in light response'
      ParentBiDiMode = False
      WordWrap = True
    end
    object Label9: TLabel
      Left = 8
      Top = 15
      Width = 81
      Height = 33
      AutoSize = False
      BiDiMode = bdLeftToRight
      Caption = 'Max A with CO2 non-limiting'
      ParentBiDiMode = False
      WordWrap = True
    end
    object Label25: TLabel
      Left = 176
      Top = 15
      Width = 65
      Height = 33
      AutoSize = False
      BiDiMode = bdLeftToRight
      Caption = 'Maximum quantum yield'
      ParentBiDiMode = False
      WordWrap = True
    end
    object lblP0: TLabel
      Left = 12
      Top = 83
      Width = 57
      Height = 13
      AutoSize = False
      Caption = 'Po (g kg-1)'
      WordWrap = True
    end
    object lblPcrit: TLabel
      Left = 160
      Top = 83
      Width = 81
      Height = 14
      AutoSize = False
      Caption = 'Saturating [P]'
      WordWrap = True
    end
    object lblPmax: TLabel
      Left = 320
      Top = 83
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Max [P] (g kg-1)'
      WordWrap = True
    end
    object edtN0: TEdit
      Left = 95
      Top = 54
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'N0'
    end
    object edtNcrit: TEdit
      Left = 247
      Top = 54
      Width = 52
      Height = 21
      TabOrder = 4
      Text = 'Ncrit'
    end
    object edtNmax: TEdit
      Left = 407
      Top = 54
      Width = 52
      Height = 21
      TabOrder = 7
      Text = 'Nmax'
    end
    object edtTheta: TEdit
      Left = 407
      Top = 19
      Width = 52
      Height = 21
      TabOrder = 6
      Text = 'Theta'
    end
    object edtAmax: TEdit
      Left = 95
      Top = 19
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Amax'
    end
    object edtAlpha: TEdit
      Left = 247
      Top = 19
      Width = 52
      Height = 21
      TabOrder = 3
      Text = 'alpha'
    end
    object edtP0: TEdit
      Left = 95
      Top = 81
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'P0'
    end
    object edtPcrit: TEdit
      Left = 247
      Top = 81
      Width = 52
      Height = 21
      TabOrder = 5
      Text = 'Pcrit'
    end
    object edtPmax: TEdit
      Left = 407
      Top = 81
      Width = 52
      Height = 21
      TabOrder = 8
      Text = 'Pmax'
    end
  end
  object Stomatal_Conductance_Parameters: TGroupBox
    Left = 16
    Top = 161
    Width = 265
    Height = 49
    Caption = 'Stomatal conductance parameters'
    TabOrder = 6
    object Label10: TLabel
      Left = 8
      Top = 19
      Width = 57
      Height = 13
      AutoSize = False
      Caption = 'Unstressed'
      WordWrap = True
    end
    object Label11: TLabel
      Left = 136
      Top = 19
      Width = 49
      Height = 13
      AutoSize = False
      Caption = 'Stressed'
      WordWrap = True
    end
    object edtBallBerry1: TEdit
      Left = 71
      Top = 15
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'BallBerry 1'
    end
    object edtBallBerry2: TEdit
      Left = 191
      Top = 15
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'BallBerry 2'
    end
  end
  object Temperature_Damage: TGroupBox
    Left = 505
    Top = 217
    Width = 265
    Height = 79
    Caption = 'Temperature damage parameters'
    TabOrder = 7
    object Label12: TLabel
      Left = 16
      Top = 19
      Width = 33
      Height = 13
      AutoSize = False
      Caption = 'TFrost'
      WordWrap = True
    end
    object Label13: TLabel
      Left = 136
      Top = 19
      Width = 57
      Height = 13
      AutoSize = False
      Caption = 'TScorch'
      WordWrap = True
    end
    object Label14: TLabel
      Left = 8
      Top = 51
      Width = 57
      Height = 13
      AutoSize = False
      Caption = 'Sensitivity'
      WordWrap = True
    end
    object Label15: TLabel
      Left = 136
      Top = 51
      Width = 57
      Height = 13
      AutoSize = False
      Caption = 'Repair rate'
      WordWrap = True
    end
    object edtTFrost: TEdit
      Left = 63
      Top = 15
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Frost'
    end
    object edtTScorch: TEdit
      Left = 199
      Top = 15
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'Scorch'
    end
    object edtTSensitivity: TEdit
      Left = 63
      Top = 47
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'Sensitivity'
    end
    object edtTRepair: TEdit
      Left = 199
      Top = 47
      Width = 52
      Height = 21
      TabOrder = 3
      Text = 'Repair'
    end
  end
  object grpTemperature_Response: TGroupBox
    Left = 304
    Top = 336
    Width = 274
    Height = 105
    Caption = 'Temperature response'
    TabOrder = 12
    object Label4: TLabel
      Left = 8
      Top = 30
      Width = 33
      Height = 13
      AutoSize = False
      Caption = 'Tmin'
      WordWrap = True
    end
    object Label17: TLabel
      Left = 136
      Top = 30
      Width = 33
      Height = 13
      AutoSize = False
      Caption = 'Tmax'
      WordWrap = True
    end
    object Label18: TLabel
      Left = 8
      Top = 70
      Width = 57
      Height = 13
      AutoSize = False
      Caption = 'Topt (lower)'
      WordWrap = True
    end
    object Label19: TLabel
      Left = 136
      Top = 70
      Width = 65
      Height = 13
      AutoSize = False
      BiDiMode = bdLeftToRight
      Caption = 'Topt (upper)'
      ParentBiDiMode = False
      WordWrap = True
    end
    object edtTmin: TEdit
      Left = 73
      Top = 26
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Tmin'
    end
    object edtTmax: TEdit
      Left = 201
      Top = 26
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'Tmax'
    end
    object edtTopt1: TEdit
      Left = 73
      Top = 66
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'Topt(lower)'
    end
    object edtTopt2: TEdit
      Left = 201
      Top = 66
      Width = 52
      Height = 21
      TabOrder = 3
      Text = 'Topt(upper)'
    end
  end
  object Age_Decline: TGroupBox
    Left = 16
    Top = 216
    Width = 467
    Height = 114
    BiDiMode = bdLeftToRight
    Caption = 'Age or size-related photosynthetic decline'
    ParentBiDiMode = False
    TabOrder = 11
    object lblMatureAge: TLabel
      Left = 48
      Top = 67
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Stand maturity'
      WordWrap = True
    end
    object lblMatureSize: TLabel
      Left = 280
      Top = 67
      Width = 49
      Height = 13
      AutoSize = False
      Caption = 'tDM ha-1'
      WordWrap = True
    end
    object lblAgePower: TLabel
      Left = 56
      Top = 94
      Width = 65
      Height = 13
      AutoSize = False
      Caption = 'Power term'
      WordWrap = True
    end
    object rgIncludeAge: TRadioGroup
      Left = 8
      Top = 16
      Width = 425
      Height = 41
      BiDiMode = bdRightToLeft
      Caption = 'Include age-based decline'
      Columns = 4
      Items.Strings = (
        'Don'#39't include'
        'Age-based'
        'Size-based'
        'Include both')
      ParentBiDiMode = False
      TabOrder = 0
      OnClick = rgIncludeAgeClick
    end
    object edtMatureAge: TEdit
      Left = 127
      Top = 63
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'Mature age'
    end
    object edtMatureSize: TEdit
      Left = 222
      Top = 63
      Width = 52
      Height = 21
      TabOrder = 3
      Text = 'Mature size'
    end
    object edtAgePower: TEdit
      Left = 127
      Top = 90
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'Age power term'
    end
    object edtSizePower: TEdit
      Left = 222
      Top = 90
      Width = 52
      Height = 21
      TabOrder = 4
      Text = 'Size power term'
    end
  end
  object Foliage_clumping: TGroupBox
    Left = 504
    Top = 48
    Width = 266
    Height = 154
    Caption = 'Light interception'
    TabOrder = 5
    object Label3: TLabel
      Left = 8
      Top = 19
      Width = 161
      Height = 13
      AutoSize = False
      Caption = 'Max. light extinction coefficient'
      WordWrap = True
    end
    object lblCanopyWidth: TLabel
      Left = 13
      Top = 116
      Width = 87
      Height = 26
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Canopy width depend. on dbh'
      WordWrap = True
    end
    object lblKlowRange: TLabel
      Left = 128
      Top = 45
      Width = 129
      Height = 13
      AutoSize = False
      Caption = 'Ext. coeff. decrease range'
      WordWrap = True
    end
    object lblIntercept: TLabel
      Left = 106
      Top = 101
      Width = 44
      Height = 15
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Intercept'
      WordWrap = True
    end
    object lblSlope: TLabel
      Left = 183
      Top = 101
      Width = 34
      Height = 12
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Slope'
      WordWrap = True
    end
    object edtFoliageClumping: TCheckBox
      Left = 8
      Top = 64
      Width = 137
      Height = 17
      Alignment = taLeftJustify
      BiDiMode = bdRightToLeft
      Caption = 'Include foliage clumping'
      ParentBiDiMode = False
      TabOrder = 1
      OnClick = edtFoliageClumpingClick
    end
    object edtkexmax: TEdit
      Left = 173
      Top = 15
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'kexMax'
    end
    object edtKlowRange: TEdit
      Left = 173
      Top = 63
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'kLowRange'
    end
    object edtCanopyWidthInter: TEdit
      Left = 101
      Top = 119
      Width = 52
      Height = 21
      TabOrder = 3
      Text = 'kLowRange'
    end
    object edtCanopyWidthSlope: TEdit
      Left = 173
      Top = 119
      Width = 52
      Height = 21
      TabOrder = 4
      Text = 'kLowRange'
    end
  end
  object gpConstantLeafN: TGroupBox
    Left = 592
    Top = 336
    Width = 178
    Height = 105
    Caption = 'Option for constant N'
    TabOrder = 13
    object lblConstantLeafNValue: TLabel
      Left = 32
      Top = 51
      Width = 113
      Height = 13
      AutoSize = False
      Caption = 'constant leaf N (g kg-1)'
      WordWrap = True
    end
    object edtConstantLeafN: TCheckBox
      Left = 40
      Top = 24
      Width = 89
      Height = 17
      Alignment = taLeftJustify
      BiDiMode = bdRightToLeft
      Caption = 'constant leaf N'
      ParentBiDiMode = False
      TabOrder = 0
      OnClick = edtConstantLeafNClick
    end
    object edtConstantLeafNValue: TEdit
      Left = 65
      Top = 71
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'Constant leaf N'
    end
  end
  object grpIsotopes: TGroupBox
    Left = 16
    Top = 336
    Width = 265
    Height = 89
    Caption = 'Carbon isotopes'
    TabOrder = 8
    object lblNewDelta: TLabel
      Left = 112
      Top = 35
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Delta of new C'
      WordWrap = True
    end
    object lblExtraDelta: TLabel
      Left = 107
      Top = 57
      Width = 97
      Height = 13
      AutoSize = False
      Caption = 'Extra discrimination'
      WordWrap = True
    end
    object rgCalcNewDelta: TRadioGroup
      Left = 8
      Top = 16
      Width = 97
      Height = 65
      ItemIndex = 0
      Items.Strings = (
        'Set as fixed '
        'Calc. from pi')
      TabOrder = 0
      OnClick = rgCalcNewDeltaClick
    end
    object edtNewDelta: TEdit
      Left = 207
      Top = 30
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'New delta'
    end
    object edtExtraDelta: TEdit
      Left = 207
      Top = 54
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'Extra delta'
    end
  end
  object grpPhsPathway: TGroupBox
    Left = 16
    Top = 432
    Width = 81
    Height = 89
    Caption = 'C3 or C4 phs?'
    TabOrder = 9
    object rdC3_C4: TRadioGroup
      Left = 12
      Top = 16
      Width = 57
      Height = 65
      ItemIndex = 0
      Items.Strings = (
        'C3'
        'C4')
      TabOrder = 0
      OnClick = rdC3_C4Click
    end
  end
  object grpC4parameters: TGroupBox
    Left = 104
    Top = 432
    Width = 177
    Height = 121
    Caption = 'Extra C4 parameters'
    TabOrder = 10
    object Label20: TLabel
      Left = 3
      Top = 25
      Width = 113
      Height = 13
      AutoSize = False
      Caption = 'PEPC activ rel to Amax'
      WordWrap = True
    end
    object Label22: TLabel
      Left = 0
      Top = 58
      Width = 113
      Height = 13
      AutoSize = False
      Caption = 'beta curv in CO2 resp'
      WordWrap = True
    end
    object lblphi: TLabel
      Left = 0
      Top = 81
      Width = 113
      Height = 32
      AutoSize = False
      Caption = 'CO2 leakage from bundle sheeth cells '
      WordWrap = True
    end
    object edtRelkPEP: TEdit
      Left = 119
      Top = 22
      Width = 50
      Height = 21
      TabOrder = 0
      Text = 'PEPC activity'
    end
    object edtbeta: TEdit
      Left = 119
      Top = 54
      Width = 50
      Height = 21
      TabOrder = 1
      Text = 'beta'
    end
    object edtphi: TEdit
      Left = 119
      Top = 86
      Width = 50
      Height = 21
      TabOrder = 2
      Text = 'phi'
    end
  end
  object edtSLA: TEdit
    Left = 111
    Top = 15
    Width = 52
    Height = 21
    TabOrder = 0
    Text = 'SLA'
  end
  object edtAlbedo: TEdit
    Left = 263
    Top = 15
    Width = 52
    Height = 21
    TabOrder = 1
    Text = 'Albedo'
  end
  object edtTransmit: TEdit
    Left = 423
    Top = 15
    Width = 52
    Height = 21
    TabOrder = 2
    Text = 'Transmissivity'
  end
  object edtNMVOC: TEdit
    Left = 583
    Top = 15
    Width = 52
    Height = 21
    TabOrder = 3
    Text = 'NMVOC'
  end
  object edtTMaxRepairTime: TEdit
    Left = 718
    Top = 307
    Width = 52
    Height = 21
    TabOrder = 17
    Text = 'Max repair length'
  end
  object GroupBox1: TGroupBox
    Left = 784
    Top = 216
    Width = 137
    Height = 113
    Caption = 'Heavy rain damage'
    TabOrder = 18
    object Label21: TLabel
      Left = 16
      Top = 25
      Width = 33
      Height = 13
      AutoSize = False
      Caption = 'Limit'
      WordWrap = True
    end
    object Label26: TLabel
      Left = 3
      Top = 51
      Width = 57
      Height = 13
      AutoSize = False
      Caption = 'Sensitivity'
      WordWrap = True
    end
    object Label27: TLabel
      Left = 6
      Top = 75
      Width = 57
      Height = 13
      AutoSize = False
      Caption = 'Repair rate'
      WordWrap = True
    end
    object edtRainDamageLimit: TEdit
      Left = 71
      Top = 20
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Limit rain damage'
    end
    object edtRainDamageSensitivity: TEdit
      Left = 71
      Top = 47
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'Sensitivity rain damage'
    end
    object edtRainDamageRepair: TEdit
      Left = 71
      Top = 74
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'Repair rain damage'
    end
  end
end
