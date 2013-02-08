object DLGCenW: TDLGCenW
  Left = 253
  Top = 168
  BorderStyle = bsDialog
  Caption = 'Edit CenW parameters'
  ClientHeight = 540
  ClientWidth = 856
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lstBxCenW: TListBox
    Left = 735
    Top = 20
    Width = 111
    Height = 390
    ItemHeight = 13
    TabOrder = 0
  end
  object PC: TPageControl
    Left = 0
    Top = -2
    Width = 729
    Height = 541
    ActivePage = TabSheet3
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Site'
      object Label6: TLabel
        Left = 9
        Top = 150
        Width = 104
        Height = 13
        Caption = 'Max. rate of soil evap.'
      end
      object Label7: TLabel
        Left = 7
        Top = 176
        Width = 184
        Height = 13
        Caption = 'Litter water-holding capacity (g gDW-1)'
      end
      object Label8: TLabel
        Left = 297
        Top = 20
        Width = 120
        Height = 13
        Caption = 'Warm melt (mm def-1 d-1)'
      end
      object Label9: TLabel
        Left = 297
        Top = 46
        Width = 125
        Height = 13
        Caption = 'Radn melt (mm (MJ m-2)-1)'
      end
      object Label10: TLabel
        Left = 297
        Top = 72
        Width = 111
        Height = 13
        Caption = 'Resist. to soil T change'
      end
      object Label11: TLabel
        Left = 297
        Top = 98
        Width = 114
        Height = 13
        Caption = 'Snow insulation (r mm-1)'
      end
      object Label12: TLabel
        Left = 297
        Top = 148
        Width = 184
        Height = 21
        Caption = 'Canopy aerodynamic resistance (s m-1)'
        WordWrap = True
      end
      object Label13: TLabel
        Left = 297
        Top = 187
        Width = 189
        Height = 13
        Caption = 'Mulching effect on water loss (% tDW-1)'
      end
      object grpSystemNutrientDynamics: TGroupBox
        Left = 2
        Top = 8
        Width = 273
        Height = 121
        Caption = 'System nutrient dynamics'
        TabOrder = 0
        object Label2: TLabel
          Left = 13
          Top = 20
          Width = 144
          Height = 13
          Caption = 'Volatilisation fractions Nitrogen'
        end
        object Label3: TLabel
          Left = 13
          Top = 43
          Width = 166
          Height = 13
          Caption = 'Atmos. input (kg ha-1 yr-1) Nitrogen'
        end
        object Label4: TLabel
          Left = 13
          Top = 67
          Width = 125
          Height = 13
          Caption = 'Leaching fraction Nitrogen'
        end
        object Label5: TLabel
          Left = 13
          Top = 91
          Width = 168
          Height = 13
          Caption = 'Daily release rate of applied fertiliser'
        end
        object Edit5: TEdit
          Left = 208
          Top = 89
          Width = 53
          Height = 24
          TabOrder = 0
          Text = 'Edit1'
        end
        object Edit6: TEdit
          Left = 208
          Top = 63
          Width = 53
          Height = 24
          TabOrder = 1
          Text = 'Edit1'
        end
        object Edit7: TEdit
          Left = 208
          Top = 37
          Width = 53
          Height = 24
          TabOrder = 2
          Text = 'Edit1'
        end
        object Edit8: TEdit
          Left = 208
          Top = 11
          Width = 53
          Height = 24
          TabOrder = 3
          Text = 'Edit1'
        end
      end
      object GroupBox2: TGroupBox
        Left = 8
        Top = 208
        Width = 545
        Height = 89
        Caption = 'Type of direct evap. calculation'
        TabOrder = 1
        object Label14: TLabel
          Left = 215
          Top = 21
          Width = 194
          Height = 28
          Caption = 
            'Slope of relationship relating water interception of foliage to ' +
            'LAI'
          WordWrap = True
        end
        object Label15: TLabel
          Left = 215
          Top = 57
          Width = 189
          Height = 13
          Caption = 'Fraction of rain lost to direct evaporation'
        end
        object RadioGroup1: TRadioGroup
          Left = 8
          Top = 16
          Width = 185
          Height = 57
          ItemIndex = 0
          Items.Strings = (
            'Amount intercepted = f(LAI)'
            'Constant fraction of rain')
          TabOrder = 0
        end
        object Edit13: TEdit
          Left = 432
          Top = 24
          Width = 56
          Height = 24
          TabOrder = 1
          Text = 'Edit1'
        end
        object Edit14: TEdit
          Left = 431
          Top = 54
          Width = 57
          Height = 24
          TabOrder = 2
          Text = 'Edit1'
        end
      end
      object Edit1: TEdit
        Left = 501
        Top = 20
        Width = 52
        Height = 24
        TabOrder = 2
        Text = 'Edit1'
      end
      object Edit2: TEdit
        Left = 501
        Top = 46
        Width = 52
        Height = 24
        TabOrder = 3
        Text = 'Edit1'
      end
      object Edit3: TEdit
        Left = 501
        Top = 72
        Width = 52
        Height = 24
        TabOrder = 4
        Text = 'Edit1'
      end
      object Edit4: TEdit
        Left = 501
        Top = 98
        Width = 52
        Height = 24
        TabOrder = 5
        Text = 'Edit1'
      end
      object Edit9: TEdit
        Left = 207
        Top = 176
        Width = 57
        Height = 24
        TabOrder = 6
        Text = 'Edit1'
      end
      object Edit10: TEdit
        Left = 207
        Top = 150
        Width = 57
        Height = 24
        TabOrder = 7
        Text = 'Edit1'
      end
      object Edit11: TEdit
        Left = 501
        Top = 182
        Width = 52
        Height = 24
        TabOrder = 8
        Text = 'Edit1'
      end
      object Edit12: TEdit
        Left = 501
        Top = 150
        Width = 52
        Height = 24
        TabOrder = 9
        Text = 'Edit1'
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Stand'
      ImageIndex = 1
      object Label16: TLabel
        Left = 98
        Top = 13
        Width = 42
        Height = 52
        Caption = 'Ratio dieing: average trees'
        WordWrap = True
      end
      object Label17: TLabel
        Left = 91
        Top = 52
        Width = 77
        Height = 13
        Caption = 'Mortality fraction'
      end
      object Label18: TLabel
        Left = 91
        Top = 78
        Width = 62
        Height = 39
        Caption = 'Parameter in 3/2 power law'
        WordWrap = True
      end
      object Label21: TLabel
        Left = 239
        Top = 141
        Width = 83
        Height = 39
        Caption = 'Ratio of average [N] in foliage and [N] in top layer'
        WordWrap = True
      end
      object Label22: TLabel
        Left = 397
        Top = 142
        Width = 57
        Height = 65
        Caption = 'Ratio of [N] in senescing and living foliage'
        WordWrap = True
      end
      object Label25: TLabel
        Left = 13
        Top = 275
        Width = 100
        Height = 39
        Caption = 'Fraction of soil microbial N taken up directly (g d-1 kg-1)'
        WordWrap = True
      end
      object Label26: TLabel
        Left = 13
        Top = 321
        Width = 134
        Height = 26
        Caption = 'Maximum daily death rate of foliage during drought (%)'
        WordWrap = True
      end
      object Label27: TLabel
        Left = 247
        Top = 280
        Width = 113
        Height = 39
        Caption = 'Amount of N biologically fixed per unit C fixed (g kg-1)'
        WordWrap = True
      end
      object Label28: TLabel
        Left = 247
        Top = 325
        Width = 131
        Height = 26
        Caption = 'Relative soil water content that starts plant water stress'
        WordWrap = True
      end
      object Label35: TLabel
        Left = 403
        Top = 219
        Width = 65
        Height = 26
        Caption = 'Wood density (g cm-3)'
        WordWrap = True
      end
      object Label45: TLabel
        Left = 572
        Top = 280
        Width = 38
        Height = 13
        Caption = 'Label45'
      end
      object grpTypeOfEvap: TGroupBox
        Left = 8
        Top = 360
        Width = 505
        Height = 145
        Caption = 'Respiration'
        TabOrder = 0
        object Label29: TLabel
          Left = 216
          Top = 23
          Width = 123
          Height = 13
          Caption = 'Respiration rate per unit N'
        end
        object Label30: TLabel
          Left = 216
          Top = 46
          Width = 178
          Height = 13
          Caption = 'beta parameter in T response (x1000)'#39
        end
        object Label31: TLabel
          Left = 216
          Top = 71
          Width = 169
          Height = 13
          Caption = 'Temperature for max respiration rate'
        end
        object Label32: TLabel
          Left = 216
          Top = 95
          Width = 85
          Height = 13
          Caption = 'Growth respiration'
        end
        object Label33: TLabel
          Left = 216
          Top = 119
          Width = 162
          Height = 13
          Caption = 'Time constant for temp adjustment'
        end
        object Label34: TLabel
          Left = 33
          Top = 85
          Width = 76
          Height = 13
          Caption = 'Respiration ratio'
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
        end
        object chkAcclimation: TCheckBox
          Left = 16
          Top = 118
          Width = 177
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Include temperature acclimation'
          TabOrder = 1
        end
        object Edit36: TEdit
          Left = 130
          Top = 85
          Width = 58
          Height = 24
          TabOrder = 2
          Text = 'Edit36'
        end
        object Edit37: TEdit
          Left = 410
          Top = 20
          Width = 57
          Height = 24
          TabOrder = 3
          Text = 'Edit37'
        end
        object Edit38: TEdit
          Left = 410
          Top = 44
          Width = 57
          Height = 24
          TabOrder = 4
          Text = 'Edit38'
        end
        object Edit39: TEdit
          Left = 410
          Top = 68
          Width = 57
          Height = 24
          TabOrder = 5
          Text = 'Edit39'
        end
        object Edit40: TEdit
          Left = 410
          Top = 93
          Width = 57
          Height = 24
          TabOrder = 6
          Text = 'Edit40'
        end
        object Edit41: TEdit
          Left = 410
          Top = 117
          Width = 57
          Height = 24
          TabOrder = 7
          Text = 'Edit41'
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
        TabOrder = 1
      end
      object grpFoliageSenesc: TGroupBox
        Left = 8
        Top = 128
        Width = 225
        Height = 73
        Caption = 'Foliage senescence in dense canopy'
        TabOrder = 2
        object Label19: TLabel
          Left = 10
          Top = 22
          Width = 123
          Height = 13
          Caption = 'Low light limit (MJ m-2 d-1)'
        end
        object Label20: TLabel
          Left = 10
          Top = 41
          Width = 105
          Height = 13
          Caption = 'Max daily senescence'
        end
        object Edit25: TEdit
          Left = 143
          Top = 15
          Width = 60
          Height = 24
          TabOrder = 0
          Text = 'Edit25'
        end
        object Edit26: TEdit
          Left = 143
          Top = 41
          Width = 60
          Height = 24
          TabOrder = 1
          Text = 'Edit26'
        end
      end
      object grpAnnualSenesc: TGroupBox
        Left = 240
        Top = 8
        Width = 345
        Height = 121
        Caption = 'Annual senescence'
        TabOrder = 3
        object Label36: TLabel
          Left = 12
          Top = 22
          Width = 56
          Height = 13
          Caption = 'Foliage min.'
        end
        object Label37: TLabel
          Left = 12
          Top = 45
          Width = 45
          Height = 13
          Caption = 'Branches'
        end
        object Label38: TLabel
          Left = 12
          Top = 67
          Width = 46
          Height = 13
          Caption = 'Fine roots'
        end
        object Label39: TLabel
          Left = 160
          Top = 21
          Width = 22
          Height = 13
          Caption = 'Bark'
        end
        object Label40: TLabel
          Left = 160
          Top = 44
          Width = 20
          Height = 13
          Caption = 'Fruit'
        end
        object Label41: TLabel
          Left = 160
          Top = 67
          Width = 25
          Height = 13
          Caption = 'Pollin'
        end
        object Label42: TLabel
          Left = 160
          Top = 91
          Width = 104
          Height = 13
          Caption = 'Longevity of sapwood'
        end
        object Edit15: TEdit
          Left = 275
          Top = 20
          Width = 60
          Height = 24
          TabOrder = 0
          Text = 'Edit15'
        end
        object Edit16: TEdit
          Left = 275
          Top = 41
          Width = 60
          Height = 24
          TabOrder = 1
          Text = 'Edit15'
        end
        object Edit17: TEdit
          Left = 275
          Top = 63
          Width = 60
          Height = 24
          TabOrder = 2
          Text = 'Edit15'
        end
        object Edit18: TEdit
          Left = 275
          Top = 85
          Width = 60
          Height = 24
          TabOrder = 3
          Text = 'Edit18'
        end
        object Edit19: TEdit
          Left = 76
          Top = 20
          Width = 59
          Height = 24
          TabOrder = 4
          Text = 'Edit19'
        end
        object Edit20: TEdit
          Left = 76
          Top = 42
          Width = 59
          Height = 24
          TabOrder = 5
          Text = 'Edit20'
        end
        object Edit21: TEdit
          Left = 76
          Top = 65
          Width = 59
          Height = 24
          TabOrder = 6
          Text = 'Edit21'
        end
      end
      object grpUseCarbo: TGroupBox
        Left = 8
        Top = 208
        Width = 385
        Height = 65
        Caption = 'Use of carbohydrate and soluble nitrogen'
        TabOrder = 4
        object Label23: TLabel
          Left = 8
          Top = 22
          Width = 95
          Height = 26
          Caption = 'Carbohydrate Km as % of live tissue'
          WordWrap = True
        end
        object Label24: TLabel
          Left = 188
          Top = 26
          Width = 95
          Height = 26
          Caption = 'Nitrogen Km as % of live tissue'
          WordWrap = True
        end
        object Edit30: TEdit
          Left = 293
          Top = 26
          Width = 56
          Height = 24
          TabOrder = 0
          Text = 'Edit30'
        end
        object Edit31: TEdit
          Left = 111
          Top = 26
          Width = 56
          Height = 24
          TabOrder = 1
          Text = 'Edit31'
        end
      end
      object Edit22: TEdit
        Left = 176
        Top = 16
        Width = 52
        Height = 24
        TabOrder = 5
        Text = 'Edit22'
      end
      object Edit23: TEdit
        Left = 176
        Top = 50
        Width = 52
        Height = 24
        TabOrder = 6
        Text = 'Edit23'
      end
      object Edit24: TEdit
        Left = 176
        Top = 85
        Width = 52
        Height = 24
        TabOrder = 7
        Text = 'Edit24'
      end
      object Edit27: TEdit
        Left = 325
        Top = 156
        Width = 53
        Height = 24
        TabOrder = 8
        Text = 'Edit27'
      end
      object Edit28: TEdit
        Left = 481
        Top = 163
        Width = 53
        Height = 24
        TabOrder = 9
        Text = 'Edit28'
      end
      object Edit29: TEdit
        Left = 475
        Top = 234
        Width = 52
        Height = 24
        TabOrder = 10
        Text = 'Edit29'
      end
      object Edit32: TEdit
        Left = 156
        Top = 286
        Width = 53
        Height = 24
        TabOrder = 11
        Text = 'Edit32'
      end
      object Edit33: TEdit
        Left = 156
        Top = 332
        Width = 53
        Height = 24
        TabOrder = 12
        Text = 'Edit33'
      end
      object Edit34: TEdit
        Left = 390
        Top = 293
        Width = 53
        Height = 24
        TabOrder = 13
        Text = 'Edit34'
      end
      object Edit35: TEdit
        Left = 403
        Top = 332
        Width = 53
        Height = 24
        TabOrder = 14
        Text = 'Edit35'
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Photosynthetic terms'
      ImageIndex = 2
      object Label43: TLabel
        Left = 6
        Top = 9
        Width = 87
        Height = 13
        Caption = 'Specific Leaf Area'
      end
      object Label44: TLabel
        Left = 176
        Top = 9
        Width = 69
        Height = 13
        Caption = 'Foliage albedo'
      end
      object Label70: TLabel
        Left = 315
        Top = 9
        Width = 65
        Height = 13
        Caption = 'Transmissivity'
      end
      object grpLeafPhotosyntheticParameters: TGroupBox
        Left = 16
        Top = 48
        Width = 473
        Height = 81
        Caption = 'Leaf photosynthetic parameters (top of canopy)'
        TabOrder = 0
        object Label47: TLabel
          Left = 7
          Top = 19
          Width = 53
          Height = 13
          Caption = 'No (g kg-1)'
        end
        object Label48: TLabel
          Left = 133
          Top = 19
          Width = 104
          Height = 13
          Caption = 'Saturating [N] (g kg-1)'
        end
        object Label49: TLabel
          Left = 327
          Top = 19
          Width = 76
          Height = 13
          Caption = 'Max [N] (g kg-1)'
        end
        object Label50: TLabel
          Left = 7
          Top = 54
          Width = 121
          Height = 13
          Caption = 'Max A at non-limiting CO2'
        end
        object Label51: TLabel
          Left = 226
          Top = 54
          Width = 161
          Height = 13
          Caption = 'Curvature in ligfht response (theta)'
        end
        object Edit47: TEdit
          Left = 408
          Top = 19
          Width = 55
          Height = 21
          TabOrder = 0
          Text = 'Edit47'
        end
        object Edit48: TEdit
          Left = 410
          Top = 50
          Width = 52
          Height = 21
          TabOrder = 1
          Text = 'Edit48'
        end
        object Edit49: TEdit
          Left = 239
          Top = 19
          Width = 55
          Height = 21
          TabOrder = 2
          Text = 'Edit49'
        end
        object Edit50: TEdit
          Left = 63
          Top = 19
          Width = 56
          Height = 21
          TabOrder = 3
          Text = 'Edit50'
        end
        object Edit51: TEdit
          Left = 132
          Top = 50
          Width = 52
          Height = 21
          TabOrder = 4
          Text = 'Edit51'
        end
      end
      object Stomatal_Conductance_Parameters: TGroupBox
        Left = 16
        Top = 144
        Width = 265
        Height = 49
        Caption = 'Stomatal conductance parameters'
        TabOrder = 1
        object Label52: TLabel
          Left = 6
          Top = 22
          Width = 53
          Height = 13
          Caption = 'Unstressed'
        end
        object Label53: TLabel
          Left = 136
          Top = 22
          Width = 41
          Height = 13
          Caption = 'Stressed'
        end
        object Edit52: TEdit
          Left = 65
          Top = 20
          Width = 52
          Height = 21
          TabOrder = 0
          Text = 'Edit52'
        end
        object Edit53: TEdit
          Left = 189
          Top = 20
          Width = 52
          Height = 21
          TabOrder = 1
          Text = 'Edit53'
        end
      end
      object Temperature_Damage: TGroupBox
        Left = 16
        Top = 200
        Width = 265
        Height = 121
        Caption = 'Temperature damage parameters'
        TabOrder = 2
        object Label54: TLabel
          Left = 13
          Top = 26
          Width = 30
          Height = 13
          Caption = 'TFrost'
        end
        object Label55: TLabel
          Left = 13
          Top = 59
          Width = 47
          Height = 13
          Caption = 'Sensitivity'
        end
        object Label56: TLabel
          Left = 33
          Top = 91
          Width = 142
          Height = 13
          Caption = 'Max length for complete repair'
        end
        object Label57: TLabel
          Left = 140
          Top = 25
          Width = 41
          Height = 13
          Caption = 'TScorch'
        end
        object Label58: TLabel
          Left = 137
          Top = 57
          Width = 52
          Height = 13
          Caption = 'Repair rate'
        end
        object Edit54: TEdit
          Left = 67
          Top = 21
          Width = 54
          Height = 21
          TabOrder = 0
          Text = 'Edit54'
        end
        object Edit55: TEdit
          Left = 67
          Top = 54
          Width = 54
          Height = 21
          TabOrder = 1
          Text = 'Edit55'
        end
        object Edit56: TEdit
          Left = 192
          Top = 24
          Width = 54
          Height = 21
          TabOrder = 2
          Text = 'Edit56'
        end
        object Edit57: TEdit
          Left = 192
          Top = 55
          Width = 54
          Height = 21
          TabOrder = 3
          Text = 'Edit57'
        end
        object Edit58: TEdit
          Left = 192
          Top = 91
          Width = 54
          Height = 21
          TabOrder = 4
          Text = 'Edit58'
        end
      end
      object Temperature_Response: TGroupBox
        Left = 304
        Top = 280
        Width = 265
        Height = 81
        Caption = 'Temperature response'
        TabOrder = 3
        object Label64: TLabel
          Left = 13
          Top = 26
          Width = 23
          Height = 13
          Caption = 'Tmin'
        end
        object Label65: TLabel
          Left = 13
          Top = 52
          Width = 56
          Height = 13
          Caption = 'Topt (lower)'
        end
        object Label66: TLabel
          Left = 143
          Top = 52
          Width = 58
          Height = 13
          Caption = 'Topt (upper)'
        end
        object Label46: TLabel
          Left = 143
          Top = 26
          Width = 26
          Height = 13
          Caption = 'Tmax'
        end
        object Edit61: TEdit
          Left = 78
          Top = 24
          Width = 51
          Height = 21
          TabOrder = 0
          Text = 'Edit61'
        end
        object Edit62: TEdit
          Left = 78
          Top = 50
          Width = 51
          Height = 21
          TabOrder = 1
          Text = 'Edit62'
        end
        object Edit63: TEdit
          Left = 207
          Top = 24
          Width = 51
          Height = 21
          TabOrder = 2
          Text = 'Edit63'
        end
        object Edit64: TEdit
          Left = 207
          Top = 50
          Width = 51
          Height = 21
          TabOrder = 3
          Text = 'Edit64'
        end
      end
      object Age_Decline: TGroupBox
        Left = 304
        Top = 144
        Width = 413
        Height = 121
        Caption = 'Age or size-related photosynthetic decline'
        TabOrder = 4
        object Label59: TLabel
          Left = 52
          Top = 72
          Width = 67
          Height = 13
          Caption = 'Stand maturity'
        end
        object Label60: TLabel
          Left = 64
          Top = 96
          Width = 53
          Height = 13
          Caption = 'Power term'
        end
        object Label61: TLabel
          Left = 286
          Top = 72
          Width = 44
          Height = 13
          Caption = 'tDM ha-1'
        end
        object rgIncludeAge: TRadioGroup
          Left = 8
          Top = 24
          Width = 373
          Height = 41
          Caption = 'Include age-based decline'
          Columns = 4
          Items.Strings = (
            'Don'#39't include'
            'Age-based'
            'Size-based'
            'Include both')
          TabOrder = 0
        end
        object Edit66: TEdit
          Left = 129
          Top = 68
          Width = 51
          Height = 21
          TabOrder = 1
          Text = 'Edit66'
        end
        object Edit67: TEdit
          Left = 129
          Top = 94
          Width = 51
          Height = 21
          TabOrder = 2
          Text = 'Edit67'
        end
        object Edit68: TEdit
          Left = 223
          Top = 70
          Width = 51
          Height = 21
          TabOrder = 3
          Text = 'Edit68'
        end
        object Edit69: TEdit
          Left = 223
          Top = 96
          Width = 51
          Height = 21
          TabOrder = 4
          Text = 'Edit69'
        end
      end
      object Foliage_clumping: TGroupBox
        Left = 504
        Top = 16
        Width = 212
        Height = 113
        Caption = 'Light interception'
        TabOrder = 5
        object Label62: TLabel
          Left = 33
          Top = 26
          Width = 95
          Height = 13
          Caption = 'Max. light ext. coeff.'
        end
        object Label63: TLabel
          Left = 4
          Top = 89
          Width = 125
          Height = 13
          Caption = 'Ext. coeff. decrease range'
        end
        object edtFoliageClumping: TCheckBox
          Left = 8
          Top = 56
          Width = 161
          Height = 17
          Alignment = taLeftJustify
          BiDiMode = bdRightToLeft
          Caption = 'Include foliage clumping'
          ParentBiDiMode = False
          TabOrder = 0
        end
        object Edit45: TEdit
          Left = 135
          Top = 21
          Width = 54
          Height = 21
          TabOrder = 1
          Text = 'Edit45'
        end
        object Edit46: TEdit
          Left = 135
          Top = 86
          Width = 54
          Height = 21
          TabOrder = 2
          Text = 'Edit46'
        end
      end
      object gpConstantLeafN: TGroupBox
        Left = 579
        Top = 280
        Width = 139
        Height = 129
        Caption = 'Option for constant N'
        TabOrder = 6
        object Label67: TLabel
          Left = 14
          Top = 70
          Width = 111
          Height = 13
          Caption = 'constant leaf N (g kg-1)'
        end
        object edtConstantLeafN: TCheckBox
          Left = 14
          Top = 24
          Width = 89
          Height = 17
          Alignment = taLeftJustify
          BiDiMode = bdRightToLeft
          Caption = 'constant leaf N'
          ParentBiDiMode = False
          TabOrder = 0
        end
        object Edit65: TEdit
          Left = 42
          Top = 92
          Width = 54
          Height = 21
          TabOrder = 1
          Text = 'Edit65'
        end
      end
      object grpIsotopes: TGroupBox
        Left = 16
        Top = 328
        Width = 265
        Height = 89
        Caption = 'Carbon isotopes'
        TabOrder = 7
        object Label68: TLabel
          Left = 111
          Top = 33
          Width = 70
          Height = 13
          Caption = 'Delta of new C'
        end
        object Label69: TLabel
          Left = 111
          Top = 59
          Width = 90
          Height = 13
          Caption = 'Extra discrimination'
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
        end
        object Edit59: TEdit
          Left = 208
          Top = 28
          Width = 49
          Height = 21
          TabOrder = 1
          Text = 'Edit59'
        end
        object Edit60: TEdit
          Left = 208
          Top = 54
          Width = 49
          Height = 21
          TabOrder = 2
          Text = 'Edit60'
        end
      end
      object Edit42: TEdit
        Left = 106
        Top = 7
        Width = 56
        Height = 21
        TabOrder = 8
        Text = 'Edit42'
      end
      object Edit43: TEdit
        Left = 254
        Top = 7
        Width = 52
        Height = 21
        TabOrder = 9
        Text = 'Edit43'
      end
      object Edit44: TEdit
        Left = 392
        Top = 7
        Width = 57
        Height = 21
        TabOrder = 10
        Text = 'Edit44'
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Phenology terms'
      ImageIndex = 3
      object Label71: TLabel
        Left = 488
        Top = 20
        Width = 167
        Height = 13
        Caption = 'Threshold temperature for heat sum'
      end
      object GroupBox1: TGroupBox
        Left = 24
        Top = 8
        Width = 433
        Height = 377
        Caption = 'Leaf growth and senescence parameters'
        TabOrder = 0
        object Label72: TLabel
          Left = 20
          Top = 26
          Width = 30
          Height = 26
          Alignment = taCenter
          Caption = 'Julian Day'
          Color = clBtnFace
          ParentColor = False
          WordWrap = True
        end
        object Label73: TLabel
          Left = 62
          Top = 26
          Width = 31
          Height = 13
          Alignment = taCenter
          Caption = 'n days'
          Color = clBtnFace
          ParentColor = False
        end
        object Label74: TLabel
          Left = 96
          Top = 26
          Width = 45
          Height = 13
          Caption = 'Heat sum'
          Color = clBtnFace
          ParentColor = False
        end
        object Label75: TLabel
          Left = 144
          Top = 26
          Width = 29
          Height = 26
          Alignment = taCenter
          Caption = 'Day length'
          Color = clBtnFace
          ParentColor = False
          WordWrap = True
        end
        object Label76: TLabel
          Left = 183
          Top = 26
          Width = 40
          Height = 26
          Alignment = taCenter
          Caption = 'Leaf fall (% per'
          Color = clBtnFace
          ParentColor = False
          WordWrap = True
        end
        object Label77: TLabel
          Left = 235
          Top = 26
          Width = 32
          Height = 26
          Alignment = taCenter
          Caption = 'Leaf growth'
          Color = clBtnFace
          ParentColor = False
          WordWrap = True
        end
        object sgPhen: TStringGrid
          Left = 18
          Top = 54
          Width = 310
          Height = 317
          ColCount = 6
          FixedCols = 0
          FixedRows = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor, goThumbTracking]
          TabOrder = 0
          ColWidths = (
            49
            42
            56
            56
            64
            50)
        end
      end
      object BitBtn1: TBitBtn
        Left = 480
        Top = 104
        Width = 163
        Height = 25
        Caption = 'Insert'
        TabOrder = 1
      end
      object BitBtn2: TBitBtn
        Left = 480
        Top = 136
        Width = 163
        Height = 25
        Caption = 'Duplicate'
        TabOrder = 2
      end
      object BitBtn3: TBitBtn
        Left = 480
        Top = 168
        Width = 163
        Height = 25
        Caption = 'Delete'
        TabOrder = 3
      end
      object BitBtn4: TBitBtn
        Left = 480
        Top = 200
        Width = 163
        Height = 25
        Caption = 'Delete All'
        TabOrder = 4
      end
      object BitBtn5: TBitBtn
        Left = 480
        Top = 232
        Width = 163
        Height = 25
        Caption = 'Load Sequence'
        TabOrder = 5
      end
      object BitBtn6: TBitBtn
        Left = 480
        Top = 264
        Width = 163
        Height = 25
        Caption = 'Save Sequence'
        TabOrder = 6
      end
      object Edit115: TEdit
        Left = 527
        Top = 39
        Width = 65
        Height = 24
        TabOrder = 7
        Text = 'Edit115'
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Soil water'
      ImageIndex = 4
      object grpTable: TGroupBox
        Left = 8
        Top = 0
        Width = 449
        Height = 497
        Caption = 'Set soil water properties'
        TabOrder = 0
        object Label78: TLabel
          Left = 20
          Top = 26
          Width = 52
          Height = 13
          Alignment = taCenter
          Caption = 'Depth (cm)'
          Color = clBtnFace
          ParentColor = False
        end
        object Label79: TLabel
          Left = 72
          Top = 26
          Width = 69
          Height = 26
          Alignment = taCenter
          Caption = 'Water holding cpaacity (%)'
          Color = clBtnFace
          ParentColor = False
          WordWrap = True
        end
        object Label80: TLabel
          Left = 163
          Top = 26
          Width = 52
          Height = 26
          Alignment = taCenter
          Caption = 'Max water held (mm)'
          Color = clBtnFace
          ParentColor = False
          WordWrap = True
        end
        object Label81: TLabel
          Left = 221
          Top = 26
          Width = 46
          Height = 39
          Alignment = taCenter
          Caption = 'Relative water extraction'
          Color = clBtnFace
          ParentColor = False
          WordWrap = True
        end
        object Label82: TLabel
          Left = 299
          Top = 26
          Width = 42
          Height = 26
          Alignment = taCenter
          Caption = 'Relative soil evap'
          Color = clBtnFace
          ParentColor = False
          WordWrap = True
        end
        object sgSO: TStringGrid
          Left = 18
          Top = 54
          Width = 373
          Height = 317
          FixedCols = 0
          FixedRows = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor, goThumbTracking]
          TabOrder = 0
          ColWidths = (
            70
            98
            78
            90
            80)
        end
      end
      object btnInsert: TBitBtn
        Left = 512
        Top = 16
        Width = 163
        Height = 25
        Caption = 'Insert'
        TabOrder = 1
      end
      object btnDuplicate: TBitBtn
        Left = 512
        Top = 48
        Width = 163
        Height = 25
        Caption = 'Duplicate'
        TabOrder = 2
      end
      object btnDelete: TBitBtn
        Left = 512
        Top = 80
        Width = 163
        Height = 25
        Caption = 'Delete'
        TabOrder = 3
      end
      object btnDeleteAll: TBitBtn
        Left = 512
        Top = 112
        Width = 163
        Height = 25
        Caption = 'Delete All'
        TabOrder = 4
      end
      object btnLoad: TBitBtn
        Left = 512
        Top = 144
        Width = 163
        Height = 25
        Caption = 'Load Sequence'
        TabOrder = 5
      end
      object btnSave: TBitBtn
        Left = 512
        Top = 176
        Width = 163
        Height = 25
        Caption = 'Save Sequence'
        TabOrder = 6
      end
      object btnSoilLitter: TButton
        Left = 512
        Top = 232
        Width = 161
        Height = 25
        Caption = 'Edit Litter input'
        TabOrder = 7
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Allocation'
      ImageIndex = 5
      object Label110: TLabel
        Left = 384
        Top = 390
        Width = 57
        Height = 65
        Caption = 'Ratio of [N] in heartwood and sapwood'
        WordWrap = True
      end
      object Label1: TLabel
        Left = 20
        Top = 345
        Width = 155
        Height = 13
        Caption = 'Minimum allocation to wood stem'
      end
      object grpAllocation_Terms: TGroupBox
        Left = 16
        Top = 8
        Width = 473
        Height = 81
        Caption = 
          'For information only: at current plant size and foliar [N], the ' +
          'following ratios result'
        TabOrder = 0
        object Label83: TLabel
          Left = 10
          Top = 20
          Width = 34
          Height = 13
          Caption = 'Foliage'
        end
        object Label84: TLabel
          Left = 11
          Top = 52
          Width = 22
          Height = 13
          Caption = 'Bark'
        end
        object Label85: TLabel
          Left = 129
          Top = 20
          Width = 45
          Height = 13
          Caption = 'Branches'
        end
        object Label86: TLabel
          Left = 133
          Top = 52
          Width = 46
          Height = 13
          Caption = 'Fine roots'
        end
        object Label87: TLabel
          Left = 297
          Top = 20
          Width = 45
          Height = 13
          Caption = 'Sapwood'
        end
        object Label88: TLabel
          Left = 297
          Top = 52
          Width = 59
          Height = 13
          Caption = 'Coarse roots'
        end
        object Edit70: TEdit
          Left = 51
          Top = 20
          Width = 51
          Height = 24
          Enabled = False
          TabOrder = 0
          Text = 'Edit70'
        end
        object Edit71: TEdit
          Left = 52
          Top = 51
          Width = 50
          Height = 24
          Enabled = False
          TabOrder = 1
          Text = 'Edit71'
        end
        object Edit72: TEdit
          Left = 193
          Top = 19
          Width = 50
          Height = 24
          Enabled = False
          TabOrder = 2
          Text = 'Edit72'
        end
        object Edit73: TEdit
          Left = 193
          Top = 51
          Width = 51
          Height = 24
          Enabled = False
          TabOrder = 3
          Text = 'Edit73'
        end
        object Edit74: TEdit
          Left = 364
          Top = 19
          Width = 50
          Height = 24
          Enabled = False
          TabOrder = 4
          Text = 'Edit74'
        end
        object Edit75: TEdit
          Left = 364
          Top = 51
          Width = 50
          Height = 24
          Enabled = False
          TabOrder = 5
          Text = 'Edit75'
        end
      end
      object grpAllocation_Ratios: TGroupBox
        Left = 16
        Top = 96
        Width = 473
        Height = 177
        Caption = 'Carbon allocation ratios'
        TabOrder = 1
        object Label89: TLabel
          Left = 13
          Top = 26
          Width = 135
          Height = 13
          Caption = 'Fine:root:foliage (unstressed)'
        end
        object Label90: TLabel
          Left = 13
          Top = 57
          Width = 119
          Height = 13
          Caption = 'foliage:branch (@h=10m)'
        end
        object Label91: TLabel
          Left = 13
          Top = 88
          Width = 119
          Height = 13
          Caption = 'Coarse roots : stem wood'
        end
        object Label92: TLabel
          Left = 13
          Top = 119
          Width = 83
          Height = 13
          Caption = 'Allocation to fruits'
        end
        object Label93: TLabel
          Left = 13
          Top = 150
          Width = 141
          Height = 13
          Caption = #39'Minimum age for reproduction'
        end
        object Label94: TLabel
          Left = 254
          Top = 26
          Width = 123
          Height = 13
          Caption = 'Fine:root:foliage (stressed)'
        end
        object Label95: TLabel
          Left = 254
          Top = 57
          Width = 66
          Height = 13
          Caption = 'Stem : branch'
        end
        object Label96: TLabel
          Left = 254
          Top = 88
          Width = 79
          Height = 13
          Caption = 'Bark : stemwood'
        end
        object Label97: TLabel
          Left = 254
          Top = 119
          Width = 89
          Height = 13
          Caption = 'Allocation to pollen'
        end
        object Label98: TLabel
          Left = 254
          Top = 150
          Width = 104
          Height = 13
          Caption = 'Excess N uptake ratio'
        end
        object Edit76: TEdit
          Left = 163
          Top = 26
          Width = 57
          Height = 24
          TabOrder = 0
          Text = 'Edit76'
        end
        object Edit77: TEdit
          Left = 163
          Top = 55
          Width = 57
          Height = 24
          TabOrder = 1
          Text = 'Edit77'
        end
        object Edit78: TEdit
          Left = 163
          Top = 85
          Width = 57
          Height = 24
          TabOrder = 2
          Text = 'Edit78'
        end
        object Edit79: TEdit
          Left = 163
          Top = 115
          Width = 57
          Height = 24
          TabOrder = 3
          Text = 'Edit79'
        end
        object Edit80: TEdit
          Left = 163
          Top = 145
          Width = 57
          Height = 24
          TabOrder = 4
          Text = 'Edit80'
        end
        object Edit81: TEdit
          Left = 392
          Top = 22
          Width = 57
          Height = 24
          TabOrder = 5
          Text = 'Edit81'
        end
        object Edit82: TEdit
          Left = 392
          Top = 53
          Width = 57
          Height = 24
          TabOrder = 6
          Text = 'Edit82'
        end
        object Edit83: TEdit
          Left = 392
          Top = 85
          Width = 57
          Height = 24
          TabOrder = 7
          Text = 'Edit83'
        end
        object Edit84: TEdit
          Left = 392
          Top = 116
          Width = 57
          Height = 24
          TabOrder = 8
          Text = 'Edit84'
        end
        object Edit85: TEdit
          Left = 392
          Top = 148
          Width = 57
          Height = 24
          TabOrder = 9
          Text = 'Edit85'
        end
      end
      object grpStem_Allom_Paras: TGroupBox
        Left = 16
        Top = 280
        Width = 233
        Height = 49
        Caption = 'Stem allometric relationships [Wt = f(D, H)]'
        TabOrder = 2
        object Label99: TLabel
          Left = 20
          Top = 24
          Width = 23
          Height = 13
          Caption = 'DBH'
        end
        object Label100: TLabel
          Left = 117
          Top = 24
          Width = 31
          Height = 13
          Caption = 'Height'
        end
        object Edit86: TEdit
          Left = 52
          Top = 22
          Width = 57
          Height = 24
          TabOrder = 0
          Text = 'Edit86'
        end
        object Edit87: TEdit
          Left = 163
          Top = 22
          Width = 56
          Height = 24
          TabOrder = 1
          Text = 'Edit87'
        end
      end
      object grpAllometric_H_vs_dbh: TGroupBox
        Left = 256
        Top = 280
        Width = 265
        Height = 89
        Caption = 'Allometric relationship height vs DBH'
        TabOrder = 3
        object Label101: TLabel
          Left = 11
          Top = 26
          Width = 42
          Height = 13
          Caption = 'Intercept'
        end
        object Label102: TLabel
          Left = 10
          Top = 52
          Width = 42
          Height = 39
          Caption = 'Min dbh for allom eqn.'
          WordWrap = True
        end
        object Label103: TLabel
          Left = 143
          Top = 26
          Width = 27
          Height = 13
          Caption = 'Slope'
        end
        object Edit88: TEdit
          Left = 72
          Top = 23
          Width = 52
          Height = 24
          TabOrder = 0
          Text = 'Edit88'
        end
        object Edit89: TEdit
          Left = 72
          Top = 65
          Width = 51
          Height = 24
          TabOrder = 1
          Text = 'Edit89'
        end
        object Edit90: TEdit
          Left = 184
          Top = 23
          Width = 52
          Height = 24
          TabOrder = 2
          Text = 'Edit90'
        end
      end
      object grpNRatios: TGroupBox
        Left = 16
        Top = 376
        Width = 353
        Height = 81
        Caption = 'Ratios of [N] in plant component to [N] in foliage'
        TabOrder = 4
        object Label104: TLabel
          Left = 7
          Top = 24
          Width = 29
          Height = 13
          Caption = 'Wood'
        end
        object Label105: TLabel
          Left = 7
          Top = 54
          Width = 22
          Height = 13
          Caption = 'Bark'
        end
        object Label106: TLabel
          Left = 111
          Top = 24
          Width = 46
          Height = 13
          Caption = 'Fine roots'
        end
        object Label107: TLabel
          Left = 111
          Top = 54
          Width = 45
          Height = 13
          Caption = 'Branches'
        end
        object Label108: TLabel
          Left = 235
          Top = 24
          Width = 25
          Height = 13
          Caption = 'Fruits'
        end
        object Label109: TLabel
          Left = 236
          Top = 54
          Width = 29
          Height = 13
          Caption = 'Pollen'
        end
        object Edit91: TEdit
          Left = 48
          Top = 23
          Width = 45
          Height = 24
          TabOrder = 0
          Text = 'Edit91'
        end
        object Edit92: TEdit
          Left = 48
          Top = 50
          Width = 45
          Height = 24
          TabOrder = 1
          Text = 'Edit92'
        end
        object Edit93: TEdit
          Left = 165
          Top = 23
          Width = 45
          Height = 24
          TabOrder = 2
          Text = 'Edit93'
        end
        object Edit94: TEdit
          Left = 166
          Top = 51
          Width = 44
          Height = 24
          TabOrder = 3
          Text = 'Edit94'
        end
        object Edit95: TEdit
          Left = 275
          Top = 23
          Width = 45
          Height = 24
          TabOrder = 4
          Text = 'Edit95'
        end
        object Edit96: TEdit
          Left = 275
          Top = 51
          Width = 45
          Height = 24
          TabOrder = 5
          Text = 'Edit96'
        end
      end
      object Edit97: TEdit
        Left = 462
        Top = 403
        Width = 58
        Height = 24
        TabOrder = 5
        Text = 'Edit97'
      end
      object Edit116: TEdit
        Left = 193
        Top = 341
        Width = 50
        Height = 24
        TabOrder = 6
        Text = 'Edit116'
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'Decomposition'
      ImageIndex = 6
      object Label113: TLabel
        Left = 0
        Top = 7
        Width = 82
        Height = 26
        Caption = 'Critcal C:N ratio of the active pool'
        WordWrap = True
      end
      object Label114: TLabel
        Left = -2
        Top = 61
        Width = 89
        Height = 52
        Caption = 'Ratio of C:N ratios in structural and metabolic litter pools'
        WordWrap = True
      end
      object Label115: TLabel
        Left = 163
        Top = 20
        Width = 73
        Height = 26
        Caption = 'Decomposition rate adjustment'
        WordWrap = True
      end
      object Label116: TLabel
        Left = 176
        Top = 74
        Width = 55
        Height = 26
        Caption = 'Percent silt and clay'
        WordWrap = True
      end
      object Label117: TLabel
        Left = 332
        Top = 13
        Width = 60
        Height = 39
        Caption = 'Fertility adjustmentat at start-up'
        WordWrap = True
      end
      object Label118: TLabel
        Left = 332
        Top = 65
        Width = 68
        Height = 52
        Caption = 'Mineral N fraction immobilised (g kg-1)'
        WordWrap = True
      end
      object Label119: TLabel
        Left = 520
        Top = 9
        Width = 93
        Height = 52
        Caption = 'Decomposibility of inert organic matter relative to resistant OM'
        WordWrap = True
      end
      object Label125: TLabel
        Left = 345
        Top = 189
        Width = 75
        Height = 65
        Caption = 
          'Water stress sensitivity of decomposition relative to plant proc' +
          'esses'
        WordWrap = True
      end
      object Label126: TLabel
        Left = 514
        Top = 197
        Width = 104
        Height = 52
        Caption = 'Residual decomposition activity under extremely dry conditions'
        WordWrap = True
      end
      object grpLignin: TGroupBox
        Left = 8
        Top = 120
        Width = 505
        Height = 57
        Caption = 'Lignin concentrations'
        TabOrder = 0
        object Label120: TLabel
          Left = 7
          Top = 26
          Width = 60
          Height = 13
          Caption = 'Dead foliage'
        end
        object Label121: TLabel
          Left = 182
          Top = 26
          Width = 52
          Height = 13
          Caption = 'Dead roots'
        end
        object Label122: TLabel
          Left = 377
          Top = 26
          Width = 29
          Height = 13
          Caption = 'Wood'
        end
        object Edit105: TEdit
          Left = 85
          Top = 24
          Width = 52
          Height = 21
          TabOrder = 0
          Text = 'Edit105'
        end
        object Edit106: TEdit
          Left = 245
          Top = 24
          Width = 52
          Height = 21
          TabOrder = 1
          Text = 'Edit106'
        end
        object Edit107: TEdit
          Left = 428
          Top = 24
          Width = 53
          Height = 21
          TabOrder = 2
          Text = 'Edit107'
        end
      end
      object grpCoarseLitter: TGroupBox
        Left = 8
        Top = 192
        Width = 321
        Height = 57
        Caption = 'Decomposability relative to structural litter'
        TabOrder = 1
        object Label123: TLabel
          Left = 17
          Top = 23
          Width = 45
          Height = 13
          Caption = 'Branches'
        end
        object Label124: TLabel
          Left = 172
          Top = 23
          Width = 56
          Height = 13
          Caption = 'Dead stems'
        end
        object Edit108: TEdit
          Left = 84
          Top = 20
          Width = 54
          Height = 21
          TabOrder = 0
          Text = 'Edit108'
        end
        object Edit109: TEdit
          Left = 244
          Top = 20
          Width = 54
          Height = 21
          TabOrder = 1
          Text = 'Edit109'
        end
      end
      object grpOMLayers: TGroupBox
        Left = 8
        Top = 264
        Width = 609
        Height = 81
        Caption = 'Organic-matter dynamics'
        TabOrder = 2
        object Label111: TLabel
          Left = 208
          Top = 13
          Width = 114
          Height = 52
          Caption = 
            'Annual percentage of organic matter transferred to the next-lowe' +
            'r layer (per cm)'
          WordWrap = True
        end
        object Label112: TLabel
          Left = 413
          Top = 20
          Width = 113
          Height = 39
          Caption = 
            'Annual percent organic matter transfer from surface to 1st soil ' +
            'layer'
          WordWrap = True
        end
        inline rgLayerOption: TRadioGroup
          Left = 8
          Top = 16
          Width = 193
          Height = 55
          Items.Strings = (
            'Simulated as a single bulked layer'
            'Simulate each layer separately')
          TabOrder = 0
        end
        object Edit112: TEdit
          Left = 342
          Top = 33
          Width = 54
          Height = 21
          TabOrder = 1
          Text = 'Edit112'
        end
        object Edit113: TEdit
          Left = 550
          Top = 33
          Width = 54
          Height = 21
          TabOrder = 2
          Text = 'Edit113'
        end
      end
      object btnLitterInput: TButton
        Left = 40
        Top = 366
        Width = 75
        Height = 26
        Caption = 'Litter Input'
        TabOrder = 3
      end
      object Edit98: TEdit
        Left = 98
        Top = 20
        Width = 50
        Height = 21
        TabOrder = 4
        Text = 'Edit98'
      end
      object Edit99: TEdit
        Left = 97
        Top = 78
        Width = 50
        Height = 21
        TabOrder = 5
        Text = 'Edit99'
      end
      object Edit100: TEdit
        Left = 254
        Top = 20
        Width = 50
        Height = 21
        TabOrder = 6
        Text = 'Edit100'
      end
      object Edit101: TEdit
        Left = 255
        Top = 78
        Width = 51
        Height = 21
        TabOrder = 7
        Text = 'Edit101'
      end
      object Edit102: TEdit
        Left = 436
        Top = 20
        Width = 50
        Height = 21
        TabOrder = 8
        Text = 'Edit102'
      end
      object Edit103: TEdit
        Left = 436
        Top = 78
        Width = 51
        Height = 21
        TabOrder = 9
        Text = 'Edit103'
      end
      object Edit104: TEdit
        Left = 637
        Top = 20
        Width = 50
        Height = 21
        TabOrder = 10
        Text = 'Edit104'
      end
      object Edit110: TEdit
        Left = 442
        Top = 215
        Width = 50
        Height = 21
        TabOrder = 11
        Text = 'Edit110'
      end
      object Edit111: TEdit
        Left = 637
        Top = 208
        Width = 50
        Height = 21
        TabOrder = 12
        Text = 'Edit111'
      end
    end
    object TabSheet8: TTabSheet
      Caption = 'Weather'
      ImageIndex = 7
      object Label127: TLabel
        Left = 13
        Top = 7
        Width = 101
        Height = 13
        Caption = 'Atmospheric pressure'
      end
      object Edit114: TEdit
        Left = 124
        Top = 7
        Width = 47
        Height = 21
        TabOrder = 0
        Text = 'Edit114'
      end
    end
    object TabSheet9: TTabSheet
      Caption = 'Control'
      ImageIndex = 8
      object rgCalculation: TRadioGroup
        Left = 38
        Top = 16
        Width = 153
        Height = 65
        Caption = 'Calculation scope'
        Items.Strings = (
          'Decomposition only'
          'Full growth simulation')
        TabOrder = 0
      end
      object chkIsotopes: TCheckBox
        Left = 46
        Top = 91
        Width = 143
        Height = 24
        Alignment = taLeftJustify
        BiDiMode = bdRightToLeft
        Caption = 'Include C isotopes'
        ParentBiDiMode = False
        TabOrder = 1
      end
      object edtResetPlantPools: TCheckBox
        Left = 20
        Top = 122
        Width = 169
        Height = 17
        Alignment = taLeftJustify
        BiDiMode = bdRightToLeft
        Caption = ' Reset plant pools at start of run'
        ParentBiDiMode = False
        TabOrder = 2
      end
    end
  end
  object OKBtn: TButton
    Left = 736
    Top = 424
    Width = 71
    Height = 25
    Caption = '&Ok'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object Button1: TButton
    Left = 737
    Top = 457
    Width = 71
    Height = 24
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object HelpBtn: TButton
    Left = 736
    Top = 496
    Width = 71
    Height = 24
    Caption = '&Help'
    TabOrder = 4
  end
end
