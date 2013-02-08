object frmC13Pools: TfrmC13Pools
  Left = 161
  Top = 115
  BorderStyle = bsDialog
  Caption = 'Isotope dialogue'
  ClientHeight = 339
  ClientWidth = 876
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
  object pgcPages: TPageControl
    Left = 0
    Top = 0
    Width = 876
    Height = 288
    ActivePage = tbsCarbon
    Align = alClient
    TabOrder = 0
    object tbsCarbon: TTabSheet
      Caption = 'Carbon 13 Ratios'
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
      object Label1: TLabel
        Left = 672
        Top = 104
        Width = 76
        Height = 13
        Caption = 'Inert (char) SOC'
      end
      object grpPlantPools: TGroupBox
        Left = 18
        Top = 8
        Width = 847
        Height = 89
        Caption = 'Plant pools (kg ha-1)'
        TabOrder = 0
        object lblLine1: TLabel
          Left = 32
          Top = 27
          Width = 49
          Height = 13
          AutoSize = False
          Caption = 'Sapwood'
          WordWrap = True
        end
        object Label2: TLabel
          Left = 168
          Top = 27
          Width = 57
          Height = 13
          AutoSize = False
          Caption = 'Heartwood'
          WordWrap = True
        end
        object Label5: TLabel
          Left = 320
          Top = 27
          Width = 57
          Height = 13
          AutoSize = False
          Caption = 'Coarseroot'
          WordWrap = True
        end
        object Label6: TLabel
          Left = 472
          Top = 27
          Width = 25
          Height = 13
          AutoSize = False
          Caption = 'Bark'
          WordWrap = True
        end
        object Label7: TLabel
          Left = 600
          Top = 27
          Width = 41
          Height = 13
          AutoSize = False
          Caption = 'Branch'
          WordWrap = True
        end
        object Label8: TLabel
          Left = 728
          Top = 53
          Width = 49
          Height = 30
          AutoSize = False
          Caption = 'Foliage primordia'
          WordWrap = True
        end
        object Label9: TLabel
          Left = 576
          Top = 59
          Width = 73
          Height = 14
          AutoSize = False
          Caption = 'Carbohydrate'
          WordWrap = True
        end
        object Label10: TLabel
          Left = 464
          Top = 59
          Width = 33
          Height = 13
          AutoSize = False
          Caption = 'Pollen'
          WordWrap = True
        end
        object Label11: TLabel
          Left = 344
          Top = 59
          Width = 25
          Height = 13
          AutoSize = False
          Caption = 'Fruit'
          WordWrap = True
        end
        object Label12: TLabel
          Left = 184
          Top = 59
          Width = 41
          Height = 13
          AutoSize = False
          Caption = 'Fineroot'
          WordWrap = True
        end
        object Label13: TLabel
          Left = 40
          Top = 59
          Width = 41
          Height = 13
          AutoSize = False
          Caption = 'Foliage'
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
          TabOrder = 1
          Text = 'Heartwood'
        end
        object edtCoarseRootC: TEdit
          Left = 375
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 2
          Text = 'Coarse root'
        end
        object edtBarkC: TEdit
          Left = 503
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 3
          Text = 'Bark'
        end
        object edtBranchesC: TEdit
          Left = 647
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 4
          Text = 'Branches'
        end
        object edtReserves: TEdit
          Left = 783
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 5
          Text = 'Foliage primordia'
        end
        object edtSolubleC: TEdit
          Left = 647
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 6
          Text = 'CH2O'
        end
        object edtPollenC: TEdit
          Left = 503
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 7
          Text = 'Pollen'
        end
        object edtFruitC: TEdit
          Left = 375
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 8
          Text = 'Fruit'
        end
        object edtFineRootC: TEdit
          Left = 231
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 9
          Text = 'Fine root'
        end
        object edtLeavesC: TEdit
          Left = 87
          Top = 55
          Width = 52
          Height = 21
          TabOrder = 10
          Text = 'Foliage'
        end
      end
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 288
    Width = 876
    Height = 51
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Label14: TLabel
      Left = 456
      Top = 19
      Width = 25
      Height = 13
      AutoSize = False
      Caption = 'o/oo'
      WordWrap = True
    end
    object Label15: TLabel
      Left = 680
      Top = 19
      Width = 25
      Height = 13
      AutoSize = False
      Caption = 'o/oo'
      WordWrap = True
    end
    object btnOK: TBitBtn
      Left = 8
      Top = 11
      Width = 75
      Height = 25
      TabOrder = 0
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 96
      Top = 11
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
    object btnHelp: TBitBtn
      Left = 184
      Top = 11
      Width = 75
      Height = 25
      TabOrder = 2
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object rdSetAllPlantDeltas: TRadioButton
      Left = 280
      Top = 18
      Width = 121
      Height = 17
      Caption = 'Set all plant ratios to:'
      TabOrder = 3
      OnClick = rdSetAllPlantDeltasClick
    end
    object rdSetAllSoilDeltas: TRadioButton
      Left = 512
      Top = 18
      Width = 121
      Height = 17
      Caption = 'Set all soil ratios to:'
      TabOrder = 5
      OnClick = rdSetAllSoilDeltasClick
    end
    object edtChangePlantDelta: TEdit
      Left = 399
      Top = 16
      Width = 52
      Height = 21
      TabOrder = 4
      Text = 'Plant delta'
    end
    object edtChangeSoilDelta: TEdit
      Left = 623
      Top = 16
      Width = 52
      Height = 21
      TabOrder = 6
      Text = 'Soil delta'
    end
  end
end
