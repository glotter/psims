object frmAllocationParameters: TfrmAllocationParameters
  Left = 289
  Top = 159
  BorderStyle = bsDialog
  Caption = 'Allocation Parameters'
  ClientHeight = 531
  ClientWidth = 506
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
  object Label18: TLabel
    Left = 24
    Top = 340
    Width = 153
    Height = 13
    AutoSize = False
    Caption = 'Minimum allocation to stemwood'
    WordWrap = True
  end
  object Label22: TLabel
    Left = 24
    Top = 365
    Width = 121
    Height = 27
    AutoSize = False
    Caption = 'Ratio of [N] in heartwood and sapwood'
    WordWrap = True
  end
  object btnOK: TBitBtn
    Left = 16
    Top = 488
    Width = 75
    Height = 25
    TabOrder = 7
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 136
    Top = 488
    Width = 75
    Height = 25
    TabOrder = 8
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 256
    Top = 488
    Width = 75
    Height = 25
    HelpContext = 3300
    TabOrder = 9
    Kind = bkHelp
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
    object lblLine1: TLabel
      Left = 8
      Top = 22
      Width = 41
      Height = 13
      AutoSize = False
      Caption = 'Foliage'
      WordWrap = True
    end
    object Label1: TLabel
      Left = 128
      Top = 22
      Width = 57
      Height = 19
      AutoSize = False
      Caption = 'Branches'
      WordWrap = True
    end
    object Label2: TLabel
      Left = 288
      Top = 22
      Width = 57
      Height = 19
      AutoSize = False
      Caption = 'Sapwood'
      WordWrap = True
    end
    object Label3: TLabel
      Left = 8
      Top = 50
      Width = 41
      Height = 13
      AutoSize = False
      Caption = 'Bark'
      WordWrap = True
    end
    object Label4: TLabel
      Left = 128
      Top = 50
      Width = 49
      Height = 13
      AutoSize = False
      Caption = 'Fine roots'
      WordWrap = True
    end
    object Label5: TLabel
      Left = 288
      Top = 50
      Width = 65
      Height = 13
      AutoSize = False
      Caption = 'Coarse roots'
      WordWrap = True
    end
    object edtC_FoliageAlloc: TEdit
      Left = 48
      Top = 20
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Foliage'
    end
    object edtC_BranchAlloc: TEdit
      Left = 192
      Top = 20
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'Branches'
    end
    object edtC_SapwoodAlloc: TEdit
      Left = 352
      Top = 20
      Width = 52
      Height = 21
      TabOrder = 4
      Text = 'Sapwood'
    end
    object edtC_BarkAlloc: TEdit
      Left = 48
      Top = 48
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'Bark'
    end
    object edtC_FineRootAlloc: TEdit
      Left = 192
      Top = 48
      Width = 52
      Height = 21
      TabOrder = 3
      Text = 'Bark'
    end
    object edtC_CoarseRootAlloc: TEdit
      Left = 352
      Top = 48
      Width = 52
      Height = 21
      TabOrder = 5
      Text = 'Coarse roots'
    end
  end
  object grpAllocation_Ratios: TGroupBox
    Left = 16
    Top = 96
    Width = 473
    Height = 177
    Caption = 'Carbon allocation ratios'
    TabOrder = 1
    object Label6: TLabel
      Left = 8
      Top = 20
      Width = 153
      Height = 13
      AutoSize = False
      Caption = 'Fine root:foliage (unstressed)'
      WordWrap = True
    end
    object Label7: TLabel
      Left = 248
      Top = 20
      Width = 153
      Height = 13
      AutoSize = False
      Caption = 'Fine root:foliage (stressed)'
      WordWrap = True
    end
    object Label8: TLabel
      Left = 8
      Top = 52
      Width = 153
      Height = 13
      AutoSize = False
      Caption = 'foliage:branch (@h=10m)'
      WordWrap = True
    end
    object Label9: TLabel
      Left = 248
      Top = 52
      Width = 153
      Height = 13
      AutoSize = False
      Caption = 'Stemwood : branch'
      WordWrap = True
    end
    object Label10: TLabel
      Left = 8
      Top = 84
      Width = 153
      Height = 13
      AutoSize = False
      Caption = 'Coarse roots : stem wood'
      WordWrap = True
    end
    object Label11: TLabel
      Left = 248
      Top = 84
      Width = 153
      Height = 13
      AutoSize = False
      Caption = 'Bark : stem wood'
      WordWrap = True
    end
    object Label12: TLabel
      Left = 8
      Top = 116
      Width = 153
      Height = 13
      AutoSize = False
      Caption = 'Allocation to fruits'
      WordWrap = True
    end
    object Label13: TLabel
      Left = 248
      Top = 116
      Width = 153
      Height = 13
      AutoSize = False
      Caption = 'Allocation to pollen'
      WordWrap = True
    end
    object Label14: TLabel
      Left = 8
      Top = 148
      Width = 153
      Height = 13
      AutoSize = False
      Caption = 'Minimum age for reproduction'
      WordWrap = True
    end
    object Label15: TLabel
      Left = 248
      Top = 148
      Width = 153
      Height = 13
      AutoSize = False
      Caption = 'Excess N uptake ratio'
      WordWrap = True
    end
    object edtRootLeafRatio1: TEdit
      Left = 168
      Top = 16
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Fine root:foliage (1)'
    end
    object edtRootLeafRatio2: TEdit
      Left = 400
      Top = 16
      Width = 52
      Height = 21
      TabOrder = 5
      Text = 'Fine root:foliage (2)'
    end
    object edtLeafBranchRatio: TEdit
      Left = 168
      Top = 48
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'Foliage:branch ratio'
    end
    object edtWoodBranchRatio: TEdit
      Left = 400
      Top = 48
      Width = 52
      Height = 21
      TabOrder = 6
      Text = 'Stem : branch'
    end
    object edtCoarseRootWoodRatio: TEdit
      Left = 168
      Top = 80
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'Coarse root:stemwood'
    end
    object edtBarkWoodRatio: TEdit
      Left = 400
      Top = 80
      Width = 52
      Height = 21
      TabOrder = 7
      Text = 'Bark:stemwood'
    end
    object edtC_FruitAlloc: TEdit
      Left = 168
      Top = 112
      Width = 52
      Height = 21
      TabOrder = 3
      Text = 'Fruit allocation'
    end
    object edt_CPollenAlloc: TEdit
      Left = 400
      Top = 112
      Width = 52
      Height = 21
      TabOrder = 8
      Text = 'Pollen allocation'
    end
    object edtSexAge: TEdit
      Left = 168
      Top = 144
      Width = 52
      Height = 21
      TabOrder = 4
      Text = 'Sex age'
    end
    object edtExcessNUptake: TEdit
      Left = 400
      Top = 144
      Width = 52
      Height = 21
      TabOrder = 9
      Text = 'Excess N uptake'
    end
  end
  object grpStem_Allom_Paras: TGroupBox
    Left = 16
    Top = 280
    Width = 233
    Height = 49
    Caption = 'Stem allometric relationships [Wt = f(D, H)]'
    TabOrder = 2
    object Label16: TLabel
      Left = 8
      Top = 21
      Width = 41
      Height = 13
      AutoSize = False
      Caption = 'DBH'
      WordWrap = True
    end
    object Label17: TLabel
      Left = 120
      Top = 21
      Width = 41
      Height = 13
      AutoSize = False
      Caption = 'Height'
      WordWrap = True
    end
    object edtWDSLope: TEdit
      Left = 48
      Top = 19
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'W-diam slope'
    end
    object edtWHSlope: TEdit
      Left = 168
      Top = 19
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'W-height slope'
    end
  end
  object grpAllometric_H_vs_dbh: TGroupBox
    Left = 256
    Top = 280
    Width = 233
    Height = 97
    Caption = 'Allometric relationship height vs DBH'
    TabOrder = 5
    object Label19: TLabel
      Left = 8
      Top = 21
      Width = 49
      Height = 13
      AutoSize = False
      Caption = 'Intercept'
      WordWrap = True
    end
    object Label20: TLabel
      Left = 128
      Top = 21
      Width = 33
      Height = 13
      AutoSize = False
      Caption = 'Slope'
      WordWrap = True
    end
    object Label21: TLabel
      Left = 8
      Top = 53
      Width = 57
      Height = 28
      AutoSize = False
      Caption = 'Min dbh for allom. eqn.'
      WordWrap = True
    end
    object edtHDInter: TEdit
      Left = 64
      Top = 19
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'H-diam intercept'
    end
    object edtHDSlope: TEdit
      Left = 160
      Top = 19
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'H-diam slope'
    end
    object edtmindbh: TEdit
      Left = 64
      Top = 56
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'Min diameter'
    end
  end
  object grpNRatios: TGroupBox
    Left = 16
    Top = 400
    Width = 473
    Height = 81
    Caption = 'Ratios of [N] in plant component to [N] in foliage'
    TabOrder = 6
    object Label23: TLabel
      Left = 8
      Top = 25
      Width = 57
      Height = 13
      AutoSize = False
      Caption = 'Stemwood'
      WordWrap = True
    end
    object Label24: TLabel
      Left = 8
      Top = 53
      Width = 57
      Height = 13
      AutoSize = False
      Caption = 'Bark'
      WordWrap = True
    end
    object Label25: TLabel
      Left = 136
      Top = 25
      Width = 57
      Height = 13
      AutoSize = False
      Caption = 'Fine roots'
      WordWrap = True
    end
    object Label26: TLabel
      Left = 136
      Top = 53
      Width = 57
      Height = 13
      AutoSize = False
      Caption = 'Branches'
      WordWrap = True
    end
    object Label27: TLabel
      Left = 272
      Top = 25
      Width = 57
      Height = 13
      AutoSize = False
      Caption = 'Fruits'
      WordWrap = True
    end
    object Label28: TLabel
      Left = 272
      Top = 53
      Width = 57
      Height = 13
      AutoSize = False
      Caption = 'Pollen'
      WordWrap = True
    end
    object edtbWood: TEdit
      Left = 64
      Top = 22
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'b-Wood'
    end
    object edtbBark: TEdit
      Left = 64
      Top = 51
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'b-Bark'
    end
    object edtbFineRoots: TEdit
      Left = 192
      Top = 22
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'b-Fine roots'
    end
    object edtbBranch: TEdit
      Left = 192
      Top = 51
      Width = 52
      Height = 21
      TabOrder = 3
      Text = 'b-Branches'
    end
    object edtbFruit: TEdit
      Left = 328
      Top = 22
      Width = 52
      Height = 21
      TabOrder = 4
      Text = 'b-Fruits'
    end
    object edtbPollen: TEdit
      Left = 328
      Top = 51
      Width = 52
      Height = 21
      TabOrder = 5
      Text = 'b-Pollen'
    end
  end
  object edtMinWoodAllocation: TEdit
    Left = 184
    Top = 336
    Width = 52
    Height = 21
    TabOrder = 3
    Text = 'Minimum wood allocation'
  end
  object edtWoodRetrans: TEdit
    Left = 184
    Top = 368
    Width = 52
    Height = 21
    TabOrder = 4
    Text = 'wood N retranslocation'
  end
end
