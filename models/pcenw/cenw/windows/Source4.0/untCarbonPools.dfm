object frmCarbonPools: TfrmCarbonPools
  Left = 418
  Top = 237
  BorderStyle = bsDialog
  Caption = 'Carbon Pools'
  ClientHeight = 419
  ClientWidth = 579
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
    Left = 320
    Top = 384
    Width = 75
    Height = 25
    TabOrder = 0
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 400
    Top = 384
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 480
    Top = 384
    Width = 75
    Height = 25
    HelpContext = 9100
    TabOrder = 2
    Kind = bkHelp
  end
  object grpLitter: TGroupBox
    Left = 8
    Top = 16
    Width = 561
    Height = 121
    Caption = 'Organic litter pools (kg ha-1)'
    TabOrder = 3
    inline edtStructSurf: Tframe1Line
      Left = 24
      Top = 24
      Width = 153
      Height = 21
      TabOrder = 0
      inherited lblLine1: TLabel
        Width = 97
        Caption = 'Structural surface C'
      end
      inherited edtNumber: TEnhancedEdit
        Left = 103
      end
    end
    inline edtMetabSurf: Tframe1Line
      Left = 200
      Top = 24
      Width = 153
      Height = 21
      TabOrder = 1
      inherited lblLine1: TLabel
        Width = 97
        Caption = 'Metabolic surface C'
      end
      inherited edtNumber: TEnhancedEdit
        Left = 103
      end
    end
    inline edtStructSoil: Tframe1Line
      Left = 393
      Top = 24
      Width = 160
      Height = 21
      TabOrder = 2
      inherited lblLine1: TLabel
        Width = 73
        Caption = 'Structural soil C'
      end
      inherited edtNumber: TEnhancedEdit
        Left = 103
      end
    end
    inline edtMetabSoil: Tframe1Line
      Left = 24
      Top = 56
      Width = 153
      Height = 21
      TabOrder = 3
      inherited lblLine1: TLabel
        Width = 89
        Caption = 'Metabolic soil C'
      end
      inherited edtNumber: TEnhancedEdit
        Left = 103
      end
    end
    inline edtFineWoodSurf: Tframe1Line
      Left = 200
      Top = 56
      Width = 153
      Height = 21
      TabOrder = 4
      inherited lblLine1: TLabel
        Width = 97
        Height = 14
        Caption = 'Fine wood surface C'
      end
      inherited edtNumber: TEnhancedEdit
        Left = 103
      end
    end
    inline edtCoarseWoodSurf: Tframe1Line
      Left = 376
      Top = 56
      Width = 169
      Height = 21
      TabOrder = 5
      inherited lblLine1: TLabel
        Width = 113
        Caption = 'Coarse wood surface C'
      end
      inherited edtNumber: TEnhancedEdit
        Left = 119
      end
    end
    inline edtCoarseWoodSoil: Tframe1Line
      Left = 200
      Top = 88
      Width = 169
      Height = 21
      TabOrder = 6
      inherited lblLine1: TLabel
        Width = 105
        Caption = 'Coarse wood soil C'
      end
      inherited edtNumber: TEnhancedEdit
        Left = 103
      end
    end
  end
  object grpSOC: TGroupBox
    Left = 10
    Top = 144
    Width = 561
    Height = 57
    Caption = 'Soil organic carbon pools (kg ha-1)'
    TabOrder = 4
    inline edtActive: Tframe1Line
      Left = 24
      Top = 24
      Width = 153
      Height = 21
      TabOrder = 0
      inherited lblLine1: TLabel
        Left = 40
        Width = 57
        Caption = 'Active SOC'
      end
      inherited edtNumber: TEnhancedEdit
        Left = 103
      end
    end
    inline edtSlow: Tframe1Line
      Left = 200
      Top = 24
      Width = 153
      Height = 21
      TabOrder = 1
      inherited lblLine1: TLabel
        Left = 48
        Width = 49
        Caption = 'Slow SOC'
      end
      inherited edtNumber: TEnhancedEdit
        Left = 103
      end
    end
    inline edtResistant: Tframe1Line
      Left = 393
      Top = 24
      Width = 160
      Height = 21
      TabOrder = 2
      inherited lblLine1: TLabel
        Left = 16
        Width = 73
        Caption = 'Resistant SOC'
      end
      inherited edtNumber: TEnhancedEdit
        Left = 103
      end
    end
  end
  object grpPlantPools: TGroupBox
    Left = 10
    Top = 216
    Width = 561
    Height = 153
    Caption = 'Plant pools (kg ha-1)'
    TabOrder = 5
    inline edtSapWood: Tframe1Line
      Left = 24
      Top = 24
      Width = 153
      Height = 21
      TabOrder = 0
      inherited lblLine1: TLabel
        Left = 24
        Width = 73
        Caption = 'Sapwood DW'
      end
      inherited edtNumber: TEnhancedEdit
        Left = 103
      end
    end
    inline edtHeartWood: Tframe1Line
      Left = 200
      Top = 24
      Width = 153
      Height = 21
      TabOrder = 1
      inherited lblLine1: TLabel
        Left = 16
        Width = 81
        Caption = 'Heartwood DW'
      end
      inherited edtNumber: TEnhancedEdit
        Left = 103
      end
    end
    inline edtCoarseRoot: Tframe1Line
      Left = 393
      Top = 24
      Width = 160
      Height = 21
      TabOrder = 2
      inherited lblLine1: TLabel
        Left = 16
        Width = 73
        Caption = 'Coarseroot DW'
      end
      inherited edtNumber: TEnhancedEdit
        Left = 103
      end
    end
    inline edtBark: Tframe1Line
      Left = 24
      Top = 56
      Width = 153
      Height = 21
      TabOrder = 3
      inherited lblLine1: TLabel
        Left = 32
        Width = 65
        Caption = 'Bark DW'
      end
      inherited edtNumber: TEnhancedEdit
        Left = 103
      end
    end
    inline edtFruit: Tframe1Line
      Left = 24
      Top = 88
      Width = 153
      Height = 21
      TabOrder = 4
      inherited lblLine1: TLabel
        Left = 40
        Width = 57
        Caption = 'Fruit DW'
      end
      inherited edtNumber: TEnhancedEdit
        Left = 103
      end
    end
    inline edtSoluble: Tframe1Line
      Left = 24
      Top = 120
      Width = 177
      Height = 21
      TabOrder = 5
      inherited lblLine1: TLabel
        Left = 24
        Width = 73
        Caption = 'Carbohydrate'
      end
      inherited edtNumber: TEnhancedEdit
        Left = 103
      end
    end
    inline edtLeaves: Tframe1Line
      Left = 200
      Top = 88
      Width = 153
      Height = 21
      TabOrder = 6
      inherited lblLine1: TLabel
        Left = 24
        Width = 73
        Caption = 'Foliage DW'
      end
      inherited edtNumber: TEnhancedEdit
        Left = 103
      end
    end
    inline edtFineRoot: Tframe1Line
      Left = 200
      Top = 56
      Width = 153
      Height = 21
      TabOrder = 7
      inherited lblLine1: TLabel
        Left = 24
        Width = 73
        Caption = 'Fineroot DW'
      end
      inherited edtNumber: TEnhancedEdit
        Left = 103
      end
    end
    inline edtPollen: Tframe1Line
      Left = 392
      Top = 88
      Width = 153
      Height = 21
      TabOrder = 8
      inherited lblLine1: TLabel
        Left = 40
        Width = 57
        Caption = 'Pollen DW'
      end
      inherited edtNumber: TEnhancedEdit
        Left = 103
      end
    end
    inline edtBranches: Tframe1Line
      Left = 392
      Top = 56
      Width = 153
      Height = 21
      TabOrder = 9
      inherited lblLine1: TLabel
        Left = 40
        Width = 57
        Caption = 'Branch DW'
      end
      inherited edtNumber: TEnhancedEdit
        Left = 103
      end
    end
  end
  object ChkResetNPools: TCheckBox
    Left = 88
    Top = 384
    Width = 97
    Height = 17
    Caption = 'Reset N pools'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
end
