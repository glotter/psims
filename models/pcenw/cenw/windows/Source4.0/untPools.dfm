object frmPools: TfrmPools
  Left = 226
  Top = 231
  BorderStyle = bsDialog
  Caption = 'Pools'
  ClientHeight = 339
  ClientWidth = 1038
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
    Left = 701
    Top = 103
    Width = 44
    Height = 13
    Caption = 'Mineral P'
  end
  object pgcPages: TPageControl
    Left = 0
    Top = 0
    Width = 1038
    Height = 295
    ActivePage = tbsMisc
    Align = alClient
    TabOrder = 0
    OnChange = pgcPagesChange
    object tbsCarbon: TTabSheet
      Caption = 'Carbon Pools'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
      object lblInert: TLabel
        Left = 672
        Top = 104
        Width = 76
        Height = 13
        Caption = 'Inert (char) SOC'
      end
      object lblPercCurrent: TLabel
        Left = 737
        Top = 129
        Width = 65
        Height = 13
        AutoSize = False
        Caption = '% of current'
        WordWrap = True
      end
      object grpPlantPools: TGroupBox
        Left = 27
        Top = 9
        Width = 959
        Height = 89
        Caption = 'Plant pools (kg ha-1)'
        TabOrder = 0
        object lblSapwood: TLabel
          Left = 8
          Top = 27
          Width = 73
          Height = 13
          AutoSize = False
          Caption = 'Sapwood DW'
          WordWrap = True
        end
        object lblHeartwood: TLabel
          Left = 152
          Top = 27
          Width = 81
          Height = 13
          AutoSize = False
          Caption = 'Heartwood DW'
          WordWrap = True
        end
        object lblCoarseroot: TLabel
          Left = 296
          Top = 27
          Width = 73
          Height = 13
          AutoSize = False
          Caption = 'Coarseroot DW'
          WordWrap = True
        end
        object lblBark: TLabel
          Left = 440
          Top = 27
          Width = 49
          Height = 13
          AutoSize = False
          Caption = 'Bark DW'
          WordWrap = True
        end
        object lblBranch: TLabel
          Left = 576
          Top = 27
          Width = 57
          Height = 13
          AutoSize = False
          Caption = 'Branch DW'
          WordWrap = True
        end
        object lblFoliage: TLabel
          Left = 8
          Top = 59
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Foliage DW'
          WordWrap = True
        end
        object lblFineroot: TLabel
          Left = 152
          Top = 59
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Fineroot DW'
          WordWrap = True
        end
        object lblFruit: TLabel
          Left = 296
          Top = 59
          Width = 57
          Height = 13
          AutoSize = False
          Caption = 'Fruit DW'
          WordWrap = True
        end
        object lblPollen: TLabel
          Left = 440
          Top = 59
          Width = 57
          Height = 13
          AutoSize = False
          Caption = 'Pollen DW'
          WordWrap = True
        end
        object lblCH2O: TLabel
          Left = 576
          Top = 59
          Width = 73
          Height = 13
          AutoSize = False
          Caption = 'Carbohydrate'
          WordWrap = True
        end
        object lblReserves: TLabel
          Left = 736
          Top = 24
          Width = 57
          Height = 25
          AutoSize = False
          Caption = 'Foliage primordia'
          WordWrap = True
        end
        object lblWeedLeavesC: TLabel
          Left = 808
          Top = 27
          Width = 89
          Height = 13
          AutoSize = False
          Caption = 'Weed leaves DW'
          WordWrap = True
        end
        object lblWeedRootsC: TLabel
          Left = 808
          Top = 55
          Width = 89
          Height = 13
          AutoSize = False
          Caption = 'Weed roots DW'
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
          Left = 733
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 10
          Text = 'Primordia'
        end
        object edtWeedLeavesC: TEdit
          Left = 895
          Top = 21
          Width = 52
          Height = 21
          TabOrder = 11
          Text = 'Weed leaves C'
        end
        object edtWeedRootsC: TEdit
          Left = 895
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 12
          Text = 'Weed root DW'
        end
      end
      object ChkResetNPPools: TCheckBox
        Left = 56
        Top = 128
        Width = 146
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
        Left = 674
        Top = 123
        Width = 52
        Height = 21
        TabOrder = 3
        Text = 'Change percentage'
      end
    end
    object tbsNitrogen: TTabSheet
      Caption = 'Nitrogen Pools'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
        Left = 223
        Top = 103
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
        Left = 555
        Top = 103
        Width = 70
        Height = 13
        Caption = 'Resistant SON'
      end
      object lblSolubleSoilN: TLabel
        Left = 736
        Top = 104
        Width = 42
        Height = 13
        Caption = 'MineralN'
      end
      object lblInertN: TLabel
        Left = 642
        Top = 103
        Width = 77
        Height = 13
        Caption = 'Inert (char) SON'
      end
      object lblPercN: TLabel
        Left = 793
        Top = 129
        Width = 65
        Height = 13
        AutoSize = False
        Caption = '% of current'
        WordWrap = True
      end
      object grpPlantPoolsC: TGroupBox
        Left = 16
        Top = 7
        Width = 970
        Height = 91
        Caption = 'Plant pools (kg ha-1)'
        TabOrder = 0
        object lblSapwoodN: TLabel
          Left = 8
          Top = 27
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Sapwood N'
          WordWrap = True
        end
        object lblHeartwoodN: TLabel
          Left = 136
          Top = 27
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Heartwood N'
          WordWrap = True
        end
        object lblCoarserootN: TLabel
          Left = 272
          Top = 27
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Coarseroot N'
          WordWrap = True
        end
        object lblBarkN: TLabel
          Left = 416
          Top = 26
          Width = 57
          Height = 13
          AutoSize = False
          Caption = 'Bark N'
          WordWrap = True
        end
        object lblBranchN: TLabel
          Left = 552
          Top = 27
          Width = 57
          Height = 13
          AutoSize = False
          Caption = 'Branch N'
          WordWrap = True
        end
        object lblFoliageN: TLabel
          Left = 8
          Top = 59
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Foliage N'
          WordWrap = True
        end
        object lblFinerootN: TLabel
          Left = 136
          Top = 59
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Fine root N'
          WordWrap = True
        end
        object lblFruitN: TLabel
          Left = 272
          Top = 59
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Fruit N'
          WordWrap = True
        end
        object lblPollenN: TLabel
          Left = 416
          Top = 59
          Width = 49
          Height = 13
          AutoSize = False
          Caption = 'Pollen N'
          WordWrap = True
        end
        object lblSolublePlantN: TLabel
          Left = 552
          Top = 59
          Width = 81
          Height = 13
          AutoSize = False
          Caption = 'Soluble nitrogen'
          WordWrap = True
        end
        object lblReservesN: TLabel
          Left = 728
          Top = 19
          Width = 81
          Height = 30
          AutoSize = False
          Caption = 'Foliage primordia nitrogen'
          WordWrap = True
        end
        object lblWeedLeavesN: TLabel
          Left = 824
          Top = 26
          Width = 81
          Height = 13
          AutoSize = False
          Caption = 'Weed leaves N'
          WordWrap = True
        end
        object lblWeedRootsN: TLabel
          Left = 824
          Top = 59
          Width = 81
          Height = 13
          AutoSize = False
          Caption = 'Weed roots N'
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
          Left = 731
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 10
          Text = 'Primordia N'
        end
        object edtWeedLeavesN: TEdit
          Left = 903
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 11
          Text = 'Weed leaves N'
        end
        object edtWeedRootsN: TEdit
          Left = 903
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 12
          Text = 'Weed roots N'
        end
      end
      object rdRescaleSON: TRadioButton
        Left = 600
        Top = 128
        Width = 129
        Height = 17
        Caption = 'Rescale SON pools to'
        TabOrder = 1
        OnClick = rdRescaleSONClick
      end
      object edtChangePercN: TEdit
        Left = 735
        Top = 126
        Width = 52
        Height = 21
        TabOrder = 2
        Text = 'Change percentage'
      end
    end
    object tbsPhosphorus: TTabSheet
      Caption = 'Phosphorus Pools'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblLayerP: TLabel
        Left = 16
        Top = 104
        Width = 26
        Height = 13
        Caption = 'Layer'
      end
      object lblStructP: TLabel
        Left = 56
        Top = 104
        Width = 50
        Height = 13
        Caption = 'Struct litter'
      end
      object lblMetabolicP: TLabel
        Left = 126
        Top = 104
        Width = 52
        Height = 13
        Caption = 'Metab litter'
      end
      object lblFinewoodP: TLabel
        Left = 203
        Top = 103
        Width = 56
        Height = 13
        Caption = 'Branch litter'
      end
      object lblCoarseWoodP: TLabel
        Left = 281
        Top = 104
        Width = 56
        Height = 13
        Caption = 'Cs wdy litter'
      end
      object lblActiveP: TLabel
        Left = 354
        Top = 104
        Width = 55
        Height = 13
        Caption = 'Active SOP'
      end
      object lblSlowP: TLabel
        Left = 431
        Top = 104
        Width = 48
        Height = 13
        Caption = 'Slow SOP'
      end
      object lblResistantP: TLabel
        Left = 489
        Top = 104
        Width = 69
        Height = 13
        Caption = 'Resistant SOP'
      end
      object lblSolubleSoilP: TLabel
        Left = 659
        Top = 103
        Width = 44
        Height = 13
        Caption = 'Mineral P'
      end
      object lblInertP: TLabel
        Left = 566
        Top = 103
        Width = 76
        Height = 13
        Caption = 'Inert (char) SOP'
      end
      object lblPercP: TLabel
        Left = 794
        Top = 125
        Width = 65
        Height = 13
        AutoSize = False
        Caption = '% of current'
        WordWrap = True
      end
      object Label1: TLabel
        Left = 725
        Top = 103
        Width = 50
        Height = 13
        Caption = '2nd min. P'
      end
      object Label2: TLabel
        Left = 807
        Top = 103
        Width = 36
        Height = 13
        Caption = 'Rock P'
      end
      object Label4: TLabel
        Left = 873
        Top = 103
        Width = 56
        Height = 13
        Caption = 'Occluded P'
      end
      object grpPlantP: TGroupBox
        Left = 18
        Top = 6
        Width = 968
        Height = 91
        Caption = 'Plant pools (kg ha-1)'
        TabOrder = 0
        object lblSapwoodP: TLabel
          Left = 8
          Top = 27
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Sapwood P'
          WordWrap = True
        end
        object lblHeartwoodP: TLabel
          Left = 136
          Top = 27
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Heartwood P'
          WordWrap = True
        end
        object lblCoarserootP: TLabel
          Left = 272
          Top = 27
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Coarseroot P'
          WordWrap = True
        end
        object lblBarkP: TLabel
          Left = 416
          Top = 27
          Width = 57
          Height = 13
          AutoSize = False
          Caption = 'Bark P'
          WordWrap = True
        end
        object lblBranchP: TLabel
          Left = 552
          Top = 27
          Width = 57
          Height = 13
          AutoSize = False
          Caption = 'Branch P'
          WordWrap = True
        end
        object lblFoliageP: TLabel
          Left = 8
          Top = 59
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Foliage P'
          WordWrap = True
        end
        object lblFinerootP: TLabel
          Left = 136
          Top = 59
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Fine root P'
          WordWrap = True
        end
        object lblFruitP: TLabel
          Left = 272
          Top = 59
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Fruit P'
          WordWrap = True
        end
        object lblPollenP: TLabel
          Left = 416
          Top = 59
          Width = 49
          Height = 13
          AutoSize = False
          Caption = 'Pollen P'
          WordWrap = True
        end
        object lblSolublePlantP: TLabel
          Left = 552
          Top = 59
          Width = 81
          Height = 13
          AutoSize = False
          Caption = 'Soluble P'
          WordWrap = True
        end
        object lblReservesP: TLabel
          Left = 712
          Top = 19
          Width = 81
          Height = 30
          AutoSize = False
          Caption = 'Foliage primordia phosphorus'
          WordWrap = True
        end
        object lblWeedLeavesP: TLabel
          Left = 799
          Top = 26
          Width = 74
          Height = 18
          AutoSize = False
          Caption = 'Weed leaves P'
          WordWrap = True
        end
        object lblWeedRootsP: TLabel
          Left = 799
          Top = 59
          Width = 74
          Height = 18
          AutoSize = False
          Caption = 'Weed roots P'
          WordWrap = True
        end
        object edtSapwoodP: TEdit
          Left = 71
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 0
          Text = 'Sapwood P'
        end
        object edtHeartwoodP: TEdit
          Left = 207
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 2
          Text = 'Heartwood P'
        end
        object edtCoarseRootP: TEdit
          Left = 343
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 4
          Text = 'Coarse root P'
        end
        object edtBarkP: TEdit
          Left = 471
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 6
          Text = 'Bark P'
        end
        object edtBranchP: TEdit
          Left = 639
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 8
          Text = 'Branch P'
        end
        object edtFoliageP: TEdit
          Left = 71
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 1
          Text = 'Foliage P'
        end
        object edtFineRootP: TEdit
          Left = 207
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 3
          Text = 'Fine root P'
        end
        object edtFruitP: TEdit
          Left = 343
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 5
          Text = 'Fruit P'
        end
        object edtPollenP: TEdit
          Left = 471
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 7
          Text = 'Pollen P'
        end
        object edtSolubleP: TEdit
          Left = 639
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 9
          Text = 'Soluble P'
        end
        object edtReservesP: TEdit
          Left = 712
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 10
          Text = 'Primordia P'
        end
        object edtWeedLeavesP: TEdit
          Left = 879
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 11
          Text = 'Weed leaves P'
        end
        object edtWeedRootsP: TEdit
          Left = 879
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 12
          Text = 'Weed roots P'
        end
      end
      object rdRescaleP: TRadioButton
        Left = 600
        Top = 124
        Width = 129
        Height = 17
        Caption = 'Rescale SOP pools to'
        TabOrder = 1
        OnClick = rdRescalePClick
      end
      object edtChangePercP: TEdit
        Left = 730
        Top = 122
        Width = 52
        Height = 21
        TabOrder = 2
        Text = 'Change percentage'
      end
    end
    object tbsMisc: TTabSheet
      Caption = 'Miscellaneous'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblDay: TLabel
        Left = 467
        Top = 104
        Width = 17
        Height = 13
        AutoSize = False
        Caption = 'DD'
        WordWrap = True
      end
      object lblYear: TLabel
        Left = 520
        Top = 104
        Width = 33
        Height = 13
        AutoSize = False
        Caption = 'YYYY'
        WordWrap = True
      end
      object lblMonth: TLabel
        Left = 495
        Top = 104
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
      object lblAge: TLabel
        Left = 40
        Top = 19
        Width = 65
        Height = 14
        AutoSize = False
        Caption = 'Plant age'
        WordWrap = True
      end
      object lblStocking: TLabel
        Left = 184
        Top = 19
        Width = 57
        Height = 14
        AutoSize = False
        Caption = 'Stocking'
        WordWrap = True
      end
      object lblSoilwater: TLabel
        Left = 320
        Top = 19
        Width = 57
        Height = 14
        AutoSize = False
        Caption = 'Soil water'
        WordWrap = True
      end
      object lblSnow: TLabel
        Left = 448
        Top = 19
        Width = 57
        Height = 14
        AutoSize = False
        Caption = 'Snow'
        WordWrap = True
      end
      object lblCover: TLabel
        Left = 424
        Top = 51
        Width = 73
        Height = 14
        AutoSize = False
        Caption = 'Canopy Cover'
        WordWrap = True
      end
      object lblExtinct: TLabel
        Left = 424
        Top = 83
        Width = 73
        Height = 14
        AutoSize = False
        Caption = 'Light ext. coeff.'
        WordWrap = True
      end
      object lblStart: TLabel
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
      object lblHeight: TLabel
        Left = 456
        Top = 155
        Width = 41
        Height = 14
        AutoSize = False
        Caption = 'Height'
        WordWrap = True
      end
      object lblDBH: TLabel
        Left = 456
        Top = 187
        Width = 41
        Height = 14
        AutoSize = False
        Caption = 'DBH'
        WordWrap = True
      end
      object lblWeedHeight: TLabel
        Left = 568
        Top = 123
        Width = 73
        Height = 14
        AutoSize = False
        Caption = 'Weed height'
        WordWrap = True
      end
      object grpInfo: TGroupBox
        Left = 440
        Top = 216
        Width = 129
        Height = 49
        Caption = 'For information only'
        TabOrder = 11
        object lblBasalArea: TLabel
          Left = 3
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
      object edtWeedHeight: TEdit
        Left = 573
        Top = 151
        Width = 52
        Height = 21
        TabOrder = 12
        Text = 'Weed height'
      end
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 295
    Width = 1038
    Height = 44
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnOK: TBitBtn
      Left = 60
      Top = 10
      Width = 75
      Height = 25
      TabOrder = 0
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnHelp: TBitBtn
      Left = 288
      Top = 10
      Width = 75
      Height = 25
      TabOrder = 1
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnEditC13: TButton
      Left = 648
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Edit C13 data'
      TabOrder = 2
      OnClick = btnEditC13Click
    end
    object btnCancel: TBitBtn
      Left = 188
      Top = 10
      Width = 75
      Height = 25
      TabOrder = 3
      OnClick = btnOKClick
      Kind = bkCancel
    end
  end
end
