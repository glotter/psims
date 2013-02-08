object frmPools: TfrmPools
  Left = 226
  Top = 231
  BorderStyle = bsDialog
  Caption = 'Pools'
  ClientHeight = 339
  ClientWidth = 874
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
  object Label3: TLabel
    Left = 80
    Top = 104
    Width = 67
    Height = 13
    Caption = 'Structural litter'
  end
  object Label4: TLabel
    Left = 416
    Top = 120
    Width = 55
    Height = 13
    Caption = 'Active SOC'
  end
  object Label5: TLabel
    Left = 672
    Top = 104
    Width = 76
    Height = 13
    Caption = 'Inert (char) SOC'
  end
  object pgcPages: TPageControl
    Left = 0
    Top = 0
    Width = 874
    Height = 295
    ActivePage = tbsMisc
    Align = alClient
    TabOrder = 0
    OnChange = pgcPagesChange
    object tbsCarbon: TTabSheet
      Caption = 'Carbon Pools'
      object lbCLayer: TLabel
        Left = 40
        Top = 104
        Width = 26
        Height = 13
        Caption = 'Layer'
      end
      object lbStructuralC: TLabel
        Left = 72
        Top = 104
        Width = 67
        Height = 13
        Caption = 'Structural litter'
      end
      object lbMetabolicC: TLabel
        Left = 160
        Top = 104
        Width = 68
        Height = 13
        Caption = 'Metabolic litter'
      end
      object lbActiveC: TLabel
        Left = 424
        Top = 104
        Width = 55
        Height = 13
        Caption = 'Active SOC'
      end
      object lbSlowC: TLabel
        Left = 512
        Top = 104
        Width = 48
        Height = 13
        Caption = 'Slow SOC'
      end
      object lbResistantC: TLabel
        Left = 584
        Top = 104
        Width = 69
        Height = 13
        Caption = 'Resistant SOC'
      end
      object lbFineWoodC: TLabel
        Left = 248
        Top = 104
        Width = 56
        Height = 13
        Caption = 'Branch litter'
      end
      object lbCoarseWoodC: TLabel
        Left = 320
        Top = 104
        Width = 89
        Height = 13
        Caption = 'Coarse woody litter'
      end
      object Label7: TLabel
        Left = 672
        Top = 104
        Width = 76
        Height = 13
        Caption = 'Inert (char) SOC'
      end
      object lblPercCurrent: TLabel
        Left = 728
        Top = 131
        Width = 65
        Height = 13
        AutoSize = False
        Caption = '% of current'
        WordWrap = True
      end
      object grpPlantPools: TGroupBox
        Left = 18
        Top = 8
        Width = 839
        Height = 89
        Caption = 'Plant pools (kg ha-1)'
        TabOrder = 0
        object Label32: TLabel
          Left = 8
          Top = 27
          Width = 73
          Height = 13
          AutoSize = False
          Caption = 'Sapwood DW'
          WordWrap = True
        end
        object Label33: TLabel
          Left = 152
          Top = 27
          Width = 81
          Height = 13
          AutoSize = False
          Caption = 'Heartwood DW'
          WordWrap = True
        end
        object Label34: TLabel
          Left = 296
          Top = 27
          Width = 73
          Height = 13
          AutoSize = False
          Caption = 'Coarseroot DW'
          WordWrap = True
        end
        object Label35: TLabel
          Left = 440
          Top = 27
          Width = 49
          Height = 13
          AutoSize = False
          Caption = 'Bark DW'
          WordWrap = True
        end
        object Label36: TLabel
          Left = 576
          Top = 27
          Width = 57
          Height = 13
          AutoSize = False
          Caption = 'Branch DW'
          WordWrap = True
        end
        object Label37: TLabel
          Left = 8
          Top = 59
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Foliage DW'
          WordWrap = True
        end
        object Label38: TLabel
          Left = 152
          Top = 59
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Fineroot DW'
          WordWrap = True
        end
        object Label39: TLabel
          Left = 296
          Top = 59
          Width = 57
          Height = 13
          AutoSize = False
          Caption = 'Fruit DW'
          WordWrap = True
        end
        object Label40: TLabel
          Left = 440
          Top = 59
          Width = 57
          Height = 13
          AutoSize = False
          Caption = 'Pollen DW'
          WordWrap = True
        end
        object Label41: TLabel
          Left = 576
          Top = 59
          Width = 73
          Height = 13
          AutoSize = False
          Caption = 'Carbohydrate'
          WordWrap = True
        end
        object Label42: TLabel
          Left = 728
          Top = 52
          Width = 57
          Height = 25
          AutoSize = False
          Caption = 'Foliage primordia'
          WordWrap = True
        end
        object edtSapWoodC: TEdit
          Left = 87
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 0
          Text = 'Sapwood'
        end
        object edtHeartWoodC: TEdit
          Left = 231
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 2
          Text = 'Heartwood'
        end
        object edtCoarseRootC: TEdit
          Left = 375
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 4
          Text = 'Coarse root'
        end
        object edtBarkC: TEdit
          Left = 503
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 6
          Text = 'Bark'
        end
        object edtBranchesC: TEdit
          Left = 647
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 8
          Text = 'Branches'
        end
        object edtLeavesC: TEdit
          Left = 87
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 1
          Text = 'Foliage'
        end
        object edtFineRootC: TEdit
          Left = 231
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 3
          Text = 'Fine roots'
        end
        object edtFruitC: TEdit
          Left = 375
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 5
          Text = 'Fruit'
        end
        object edtPollenC: TEdit
          Left = 503
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 7
          Text = 'Pollen'
        end
        object edtSolubleC: TEdit
          Left = 647
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 9
          Text = 'CH2O'
        end
        object edtReservesC: TEdit
          Left = 783
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 10
          Text = 'Primordia'
        end
      end
      object ChkResetNPools: TCheckBox
        Left = 56
        Top = 128
        Width = 97
        Height = 17
        Caption = 'Reset N pools'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object rdRescaleSOM: TRadioButton
        Left = 544
        Top = 128
        Width = 129
        Height = 17
        Caption = 'Rescale SOM pools to'
        TabOrder = 2
        OnClick = rdRescaleSOMClick
      end
      object edtChangePerc: TEdit
        Left = 671
        Top = 127
        Width = 52
        Height = 21
        TabOrder = 3
        Text = 'Change percentage'
      end
    end
    object tbsNitrogen: TTabSheet
      Caption = 'Nitrogen Pools'
      ImageIndex = 1
      object lbLayerN: TLabel
        Left = 16
        Top = 104
        Width = 26
        Height = 13
        Caption = 'Layer'
      end
      object lbStructN: TLabel
        Left = 56
        Top = 104
        Width = 67
        Height = 13
        Caption = 'Structural litter'
      end
      object lbMetabolicN: TLabel
        Left = 136
        Top = 104
        Width = 68
        Height = 13
        Caption = 'Metabolic litter'
      end
      object lbFineWoodN: TLabel
        Left = 224
        Top = 104
        Width = 56
        Height = 13
        Caption = 'Branch litter'
      end
      object lbCoarseWoodN: TLabel
        Left = 296
        Top = 104
        Width = 89
        Height = 13
        Caption = 'Coarse woody litter'
      end
      object lbActiveN: TLabel
        Left = 400
        Top = 104
        Width = 56
        Height = 13
        Caption = 'Active SON'
      end
      object lbSlowN: TLabel
        Left = 480
        Top = 104
        Width = 49
        Height = 13
        Caption = 'Slow SON'
      end
      object lbResistantN: TLabel
        Left = 552
        Top = 104
        Width = 70
        Height = 13
        Caption = 'Resistant SON'
      end
      object lbSolubleN: TLabel
        Left = 736
        Top = 104
        Width = 42
        Height = 13
        Caption = 'MineralN'
      end
      object Label6: TLabel
        Left = 640
        Top = 104
        Width = 77
        Height = 13
        Caption = 'Inert (char) SON'
      end
      object GroupBox3: TGroupBox
        Left = 16
        Top = 6
        Width = 849
        Height = 91
        Caption = 'Plant pools (kg ha-1)'
        TabOrder = 0
        object Label20: TLabel
          Left = 8
          Top = 27
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Sapwood N'
          WordWrap = True
        end
        object Label21: TLabel
          Left = 136
          Top = 27
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Heartwood N'
          WordWrap = True
        end
        object Label22: TLabel
          Left = 272
          Top = 27
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Coarseroot N'
          WordWrap = True
        end
        object Label23: TLabel
          Left = 416
          Top = 27
          Width = 57
          Height = 13
          AutoSize = False
          Caption = 'Bark N'
          WordWrap = True
        end
        object Label24: TLabel
          Left = 552
          Top = 27
          Width = 57
          Height = 13
          AutoSize = False
          Caption = 'Branch N'
          WordWrap = True
        end
        object Label25: TLabel
          Left = 8
          Top = 59
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Foliage N'
          WordWrap = True
        end
        object Label26: TLabel
          Left = 136
          Top = 59
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Fine root N'
          WordWrap = True
        end
        object Label27: TLabel
          Left = 272
          Top = 59
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Fruit N'
          WordWrap = True
        end
        object Label28: TLabel
          Left = 416
          Top = 59
          Width = 49
          Height = 13
          AutoSize = False
          Caption = 'Pollen N'
          WordWrap = True
        end
        object Label29: TLabel
          Left = 552
          Top = 59
          Width = 81
          Height = 13
          AutoSize = False
          Caption = 'Soluble nitrogen'
          WordWrap = True
        end
        object Label30: TLabel
          Left = 704
          Top = 51
          Width = 81
          Height = 30
          AutoSize = False
          Caption = 'Foliage primordia nitrogen'
          WordWrap = True
        end
        object edtSapwoodN: TEdit
          Left = 71
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 0
          Text = 'Sapwood N'
        end
        object edtHeartwoodN: TEdit
          Left = 207
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 2
          Text = 'Heartwood N'
        end
        object edtCoarseRootN: TEdit
          Left = 343
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 4
          Text = 'Coarse root N'
        end
        object edtBarkN: TEdit
          Left = 471
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 6
          Text = 'Bark N'
        end
        object edtBranchN: TEdit
          Left = 639
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 8
          Text = 'Branch N'
        end
        object edtFoliageN: TEdit
          Left = 71
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 1
          Text = 'Foliage N'
        end
        object edtFineRootN: TEdit
          Left = 207
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 3
          Text = 'Fine root N'
        end
        object edtFruitN: TEdit
          Left = 343
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 5
          Text = 'Fruit N'
        end
        object edtPollenN: TEdit
          Left = 471
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 7
          Text = 'Pollen N'
        end
        object edtSolubleN: TEdit
          Left = 639
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 9
          Text = 'Soluble N'
        end
        object edtReservesN: TEdit
          Left = 791
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 10
          Text = 'Primordia N'
        end
      end
    end
    object tbsMisc: TTabSheet
      Caption = 'Miscellaneous'
      ImageIndex = 2
      object lblLine1: TLabel
        Left = 472
        Top = 107
        Width = 17
        Height = 13
        AutoSize = False
        Caption = 'DD'
        WordWrap = True
      end
      object Label1: TLabel
        Left = 520
        Top = 107
        Width = 33
        Height = 13
        AutoSize = False
        Caption = 'YYYY'
        WordWrap = True
      end
      object Label2: TLabel
        Left = 496
        Top = 107
        Width = 17
        Height = 13
        AutoSize = False
        Caption = 'MM'
        WordWrap = True
      end
      object lbStruct1: TLabel
        Left = 96
        Top = 98
        Width = 45
        Height = 13
        Caption = 'Structural'
      end
      object lbFineWood1: TLabel
        Left = 168
        Top = 98
        Width = 34
        Height = 13
        Caption = 'Branch'
      end
      object lbCoarseWood1: TLabel
        Left = 224
        Top = 98
        Width = 62
        Height = 13
        Caption = 'Coarse wood'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lbLignin: TLabel
        Left = 80
        Top = 64
        Width = 185
        Height = 20
        Caption = 'Litter lignin concentrations'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lbLayer: TLabel
        Left = 56
        Top = 98
        Width = 26
        Height = 13
        Caption = 'Layer'
      end
      object Label8: TLabel
        Left = 40
        Top = 19
        Width = 65
        Height = 14
        AutoSize = False
        Caption = 'Plant age'
        WordWrap = True
      end
      object Label9: TLabel
        Left = 184
        Top = 19
        Width = 57
        Height = 14
        AutoSize = False
        Caption = 'Stocking'
        WordWrap = True
      end
      object Label10: TLabel
        Left = 320
        Top = 19
        Width = 57
        Height = 14
        AutoSize = False
        Caption = 'Soil water'
        WordWrap = True
      end
      object Label11: TLabel
        Left = 448
        Top = 19
        Width = 57
        Height = 14
        AutoSize = False
        Caption = 'Snow'
        WordWrap = True
      end
      object Label12: TLabel
        Left = 424
        Top = 51
        Width = 73
        Height = 14
        AutoSize = False
        Caption = 'Canopy Cover'
        WordWrap = True
      end
      object Label13: TLabel
        Left = 424
        Top = 83
        Width = 73
        Height = 14
        AutoSize = False
        Caption = 'Light ext. coeff.'
        WordWrap = True
      end
      object Label14: TLabel
        Left = 392
        Top = 123
        Width = 65
        Height = 13
        AutoSize = False
        Caption = 'Starting date:'
        WordWrap = True
      end
      object Label15: TLabel
        Left = 485
        Top = 123
        Width = 9
        Height = 13
        AutoSize = False
        Caption = '/'
        WordWrap = True
      end
      object Label16: TLabel
        Left = 512
        Top = 123
        Width = 9
        Height = 13
        AutoSize = False
        Caption = '/'
        WordWrap = True
      end
      object Label17: TLabel
        Left = 456
        Top = 155
        Width = 41
        Height = 14
        AutoSize = False
        Caption = 'Height'
        WordWrap = True
      end
      object Label18: TLabel
        Left = 456
        Top = 187
        Width = 41
        Height = 14
        AutoSize = False
        Caption = 'DBH'
        WordWrap = True
      end
      object grpInfo: TGroupBox
        Left = 440
        Top = 216
        Width = 129
        Height = 49
        Caption = 'For information only'
        TabOrder = 11
        object Label19: TLabel
          Left = 0
          Top = 20
          Width = 57
          Height = 13
          AutoSize = False
          Caption = 'Basal area'
          WordWrap = True
        end
        object edtArea: TEdit
          Left = 61
          Top = 17
          Width = 52
          Height = 21
          TabOrder = 0
          Text = 'Area'
        end
      end
      object edtPlantAge: TEdit
        Left = 103
        Top = 15
        Width = 52
        Height = 21
        TabOrder = 0
        Text = 'Plant age'
      end
      object edtStocking: TEdit
        Left = 239
        Top = 15
        Width = 52
        Height = 21
        TabOrder = 1
        Text = 'Stocking'
      end
      object edtSoilWater: TEdit
        Left = 375
        Top = 15
        Width = 52
        Height = 21
        TabOrder = 2
        Text = 'Soil water'
      end
      object edtSnow: TEdit
        Left = 503
        Top = 15
        Width = 52
        Height = 21
        TabOrder = 3
        Text = 'Snow'
      end
      object edtCanopyCover: TEdit
        Left = 503
        Top = 47
        Width = 52
        Height = 21
        TabOrder = 4
        Text = 'Canopy cover'
      end
      object edtkex: TEdit
        Left = 503
        Top = 79
        Width = 52
        Height = 21
        TabOrder = 5
        Text = 'Extinction coeffciient'
      end
      object edtStartDay: TEdit
        Left = 464
        Top = 119
        Width = 19
        Height = 21
        TabOrder = 6
        Text = 'Start day'
      end
      object edtStartMonth: TEdit
        Left = 493
        Top = 119
        Width = 19
        Height = 21
        TabOrder = 7
        Text = 'Start month'
      end
      object edtStartYear: TEdit
        Left = 517
        Top = 119
        Width = 36
        Height = 21
        TabOrder = 8
        Text = 'Start year'
      end
      object edtHeight: TEdit
        Left = 503
        Top = 151
        Width = 52
        Height = 21
        TabOrder = 9
        Text = 'Height'
      end
      object edtDBH: TEdit
        Left = 503
        Top = 183
        Width = 52
        Height = 21
        TabOrder = 10
        Text = 'DBH'
      end
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 295
    Width = 874
    Height = 44
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnOK: TBitBtn
      Left = 64
      Top = 11
      Width = 75
      Height = 25
      TabOrder = 0
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 176
      Top = 11
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
    object btnHelp: TBitBtn
      Left = 288
      Top = 11
      Width = 75
      Height = 25
      TabOrder = 2
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnEditC13: TButton
      Left = 648
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Edit C13 data'
      TabOrder = 3
      OnClick = btnEditC13Click
    end
  end
end
