object frmInitialPools: TfrmInitialPools
  Left = 272
  Top = 63
  BorderStyle = bsDialog
  Caption = 'Pools'
  ClientHeight = 661
  ClientWidth = 712
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
  object lblDay: TLabel
    Left = 252
    Top = 572
    Width = 17
    Height = 13
    AutoSize = False
    Caption = 'DD'
    WordWrap = True
  end
  object lblMonth: TLabel
    Left = 280
    Top = 572
    Width = 17
    Height = 13
    AutoSize = False
    Caption = 'MM'
    WordWrap = True
  end
  object lblYear: TLabel
    Left = 312
    Top = 572
    Width = 33
    Height = 13
    AutoSize = False
    Caption = 'YYYY'
    WordWrap = True
  end
  object lblAge: TLabel
    Left = 72
    Top = 507
    Width = 65
    Height = 13
    AutoSize = False
    Caption = 'Plant age'
    WordWrap = True
  end
  object lblStocking: TLabel
    Left = 216
    Top = 508
    Width = 65
    Height = 13
    AutoSize = False
    Caption = 'Stocking'
    WordWrap = True
  end
  object lblStartingDate: TLabel
    Left = 176
    Top = 588
    Width = 65
    Height = 13
    AutoSize = False
    Caption = 'Starting date:'
    WordWrap = True
  end
  object lblDash1: TLabel
    Left = 272
    Top = 588
    Width = 9
    Height = 13
    AutoSize = False
    Caption = '/'
    WordWrap = True
  end
  object lblDash2: TLabel
    Left = 304
    Top = 588
    Width = 9
    Height = 13
    AutoSize = False
    Caption = '/'
    WordWrap = True
  end
  object lblHeight: TLabel
    Left = 64
    Top = 540
    Width = 65
    Height = 13
    AutoSize = False
    Caption = 'Plant height'
    WordWrap = True
  end
  object lblDBH: TLabel
    Left = 216
    Top = 540
    Width = 65
    Height = 13
    AutoSize = False
    Caption = 'DBH'
    WordWrap = True
  end
  object grpPools: TGroupBox
    Left = 40
    Top = 8
    Width = 457
    Height = 377
    Caption = 'Plant pools (kg ha-1)'
    TabOrder = 0
    object Label3: TLabel
      Left = 16
      Top = 27
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Sapwood DW'
      WordWrap = True
    end
    object Label4: TLabel
      Left = 168
      Top = 27
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Sapwood N'
      WordWrap = True
    end
    object Label5: TLabel
      Left = 16
      Top = 59
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Heartwood DW'
      WordWrap = True
    end
    object Label6: TLabel
      Left = 168
      Top = 59
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Heartwood N'
      WordWrap = True
    end
    object Label7: TLabel
      Left = 16
      Top = 91
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Foliage DW'
      WordWrap = True
    end
    object Label8: TLabel
      Left = 168
      Top = 91
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Foliage N'
      WordWrap = True
    end
    object Label9: TLabel
      Left = 16
      Top = 123
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Fineroot DW'
      WordWrap = True
    end
    object Label10: TLabel
      Left = 168
      Top = 122
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Fineroot N'
      WordWrap = True
    end
    object Label11: TLabel
      Left = 16
      Top = 155
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Coarseroot DW'
      WordWrap = True
    end
    object Label12: TLabel
      Left = 168
      Top = 155
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Coarseroot N'
      WordWrap = True
    end
    object Label13: TLabel
      Left = 16
      Top = 187
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Branch DW'
      WordWrap = True
    end
    object Label14: TLabel
      Left = 168
      Top = 187
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Branch N'
      WordWrap = True
    end
    object Label15: TLabel
      Left = 16
      Top = 219
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Bark DW'
      WordWrap = True
    end
    object Label16: TLabel
      Left = 168
      Top = 219
      Width = 65
      Height = 13
      AutoSize = False
      Caption = 'Bark N'
      WordWrap = True
    end
    object Label17: TLabel
      Left = 16
      Top = 251
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Fruit DW'
      WordWrap = True
    end
    object Label18: TLabel
      Left = 168
      Top = 251
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Fruit N'
      WordWrap = True
    end
    object Label19: TLabel
      Left = 16
      Top = 283
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Pollen DW'
      WordWrap = True
    end
    object Label20: TLabel
      Left = 168
      Top = 283
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Pollen N'
      WordWrap = True
    end
    object Label21: TLabel
      Left = 16
      Top = 315
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Carbohydrate'
      WordWrap = True
    end
    object Label22: TLabel
      Left = 168
      Top = 315
      Width = 81
      Height = 13
      AutoSize = False
      Caption = 'Soluble N'
      WordWrap = True
    end
    object Label23: TLabel
      Left = 16
      Top = 347
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Leaf primordia'
      WordWrap = True
    end
    object Label24: TLabel
      Left = 168
      Top = 347
      Width = 73
      Height = 14
      AutoSize = False
      Caption = 'Primordia N'
      WordWrap = True
    end
    object Label32: TLabel
      Left = 320
      Top = 26
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Sapwood P'
      WordWrap = True
    end
    object Label33: TLabel
      Left = 320
      Top = 58
      Width = 65
      Height = 13
      AutoSize = False
      Caption = 'Heartwood P'
      WordWrap = True
    end
    object Label34: TLabel
      Left = 320
      Top = 90
      Width = 65
      Height = 13
      AutoSize = False
      Caption = 'Foliage P'
      WordWrap = True
    end
    object Label35: TLabel
      Left = 320
      Top = 122
      Width = 65
      Height = 13
      AutoSize = False
      Caption = 'Fineroot P'
      WordWrap = True
    end
    object Label36: TLabel
      Left = 320
      Top = 154
      Width = 65
      Height = 13
      AutoSize = False
      Caption = 'Coarseroot P'
      WordWrap = True
    end
    object Label37: TLabel
      Left = 320
      Top = 186
      Width = 65
      Height = 13
      AutoSize = False
      Caption = 'Branch P'
      WordWrap = True
    end
    object Label38: TLabel
      Left = 320
      Top = 218
      Width = 49
      Height = 13
      AutoSize = False
      Caption = 'Bark P'
      WordWrap = True
    end
    object Label39: TLabel
      Left = 320
      Top = 250
      Width = 49
      Height = 13
      AutoSize = False
      Caption = 'Fruit P'
      WordWrap = True
    end
    object Label40: TLabel
      Left = 320
      Top = 282
      Width = 65
      Height = 13
      AutoSize = False
      Caption = 'Pollen P'
      WordWrap = True
    end
    object Label41: TLabel
      Left = 320
      Top = 314
      Width = 65
      Height = 13
      AutoSize = False
      Caption = 'Soluble P'
      WordWrap = True
    end
    object Label42: TLabel
      Left = 320
      Top = 346
      Width = 65
      Height = 14
      AutoSize = False
      Caption = 'Primordia P'
      WordWrap = True
    end
    object edtSapWoodC: TEdit
      Left = 95
      Top = 23
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Sapwood C'
    end
    object edtSapwoodN: TEdit
      Left = 239
      Top = 23
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'Sapwood N'
    end
    object edtHeartWoodC: TEdit
      Left = 95
      Top = 55
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'Heartwood C'
    end
    object edtHeartwoodN: TEdit
      Left = 239
      Top = 55
      Width = 52
      Height = 21
      TabOrder = 3
      Text = 'Heartwood N'
    end
    object edtLeavesC: TEdit
      Left = 95
      Top = 87
      Width = 52
      Height = 21
      TabOrder = 4
      Text = 'Foliage C'
    end
    object edtFoliageN: TEdit
      Left = 239
      Top = 87
      Width = 52
      Height = 21
      TabOrder = 5
      Text = 'Foliage N'
    end
    object edtFineRootC: TEdit
      Left = 95
      Top = 119
      Width = 52
      Height = 21
      TabOrder = 6
      Text = 'Fineroot C'
    end
    object edtFineRootN: TEdit
      Left = 239
      Top = 119
      Width = 52
      Height = 21
      TabOrder = 7
      Text = 'Fineroot N'
    end
    object edtCoarseRootC: TEdit
      Left = 95
      Top = 151
      Width = 52
      Height = 21
      TabOrder = 8
      Text = 'Coarseroot C'
    end
    object edtCoarseRootN: TEdit
      Left = 239
      Top = 151
      Width = 52
      Height = 21
      TabOrder = 9
      Text = 'Coarseroot N'
    end
    object edtBranchesC: TEdit
      Left = 95
      Top = 183
      Width = 52
      Height = 21
      TabOrder = 10
      Text = 'Branch C'
    end
    object edtBranchN: TEdit
      Left = 239
      Top = 183
      Width = 52
      Height = 21
      TabOrder = 11
      Text = 'Branch N'
    end
    object edtBarkC: TEdit
      Left = 95
      Top = 215
      Width = 52
      Height = 21
      TabOrder = 12
      Text = 'Bark C'
    end
    object edtBarkN: TEdit
      Left = 239
      Top = 215
      Width = 52
      Height = 21
      TabOrder = 13
      Text = 'Bark N'
    end
    object edtFruitC: TEdit
      Left = 95
      Top = 247
      Width = 52
      Height = 21
      TabOrder = 14
      Text = 'Fruit C'
    end
    object edtFruitN: TEdit
      Left = 239
      Top = 247
      Width = 52
      Height = 21
      TabOrder = 15
      Text = 'Fruit N'
    end
    object edtPollenC: TEdit
      Left = 95
      Top = 279
      Width = 52
      Height = 21
      TabOrder = 16
      Text = 'Pollen C'
    end
    object edtPollenN: TEdit
      Left = 239
      Top = 279
      Width = 52
      Height = 21
      TabOrder = 17
      Text = 'Pollen N'
    end
    object edtSolubleC: TEdit
      Left = 95
      Top = 311
      Width = 52
      Height = 21
      TabOrder = 18
      Text = 'CH2O'
    end
    object edtSolubleN: TEdit
      Left = 239
      Top = 311
      Width = 52
      Height = 21
      TabOrder = 19
      Text = 'Soluble N'
    end
    object edtReservesC: TEdit
      Left = 95
      Top = 343
      Width = 52
      Height = 21
      TabOrder = 20
      Text = 'Primordia'
    end
    object edtReservesN: TEdit
      Left = 239
      Top = 343
      Width = 52
      Height = 21
      TabOrder = 21
      Text = 'Primordia N'
    end
    object edtSapwoodP: TEdit
      Left = 391
      Top = 23
      Width = 52
      Height = 21
      TabOrder = 22
      Text = 'Sapwood P'
    end
    object edtHeartwoodP: TEdit
      Left = 391
      Top = 55
      Width = 52
      Height = 21
      TabOrder = 23
      Text = 'Heartwood N'
    end
    object edtFoliageP: TEdit
      Left = 391
      Top = 87
      Width = 52
      Height = 21
      TabOrder = 24
      Text = 'Foliage P'
    end
    object edtFinerootP: TEdit
      Left = 391
      Top = 119
      Width = 52
      Height = 21
      TabOrder = 25
      Text = 'Fineroot P'
    end
    object edtCoarserootP: TEdit
      Left = 391
      Top = 151
      Width = 52
      Height = 21
      TabOrder = 26
      Text = 'Coarseroot P'
    end
    object edtBranchP: TEdit
      Left = 391
      Top = 183
      Width = 52
      Height = 21
      TabOrder = 27
      Text = 'Branch P'
    end
    object edtBarkP: TEdit
      Left = 391
      Top = 215
      Width = 52
      Height = 21
      TabOrder = 28
      Text = 'Bark P'
    end
    object edtFruitP: TEdit
      Left = 391
      Top = 247
      Width = 52
      Height = 21
      TabOrder = 29
      Text = 'Fruit P'
    end
    object edtPollenP: TEdit
      Left = 391
      Top = 279
      Width = 52
      Height = 21
      TabOrder = 30
      Text = 'Pollen P'
    end
    object edtSolubleP: TEdit
      Left = 391
      Top = 311
      Width = 52
      Height = 21
      TabOrder = 31
      Text = 'Soluble P'
    end
    object edtReservesP: TEdit
      Left = 391
      Top = 343
      Width = 52
      Height = 21
      TabOrder = 32
      Text = 'Primordia P'
    end
  end
  object btnOK: TBitBtn
    Left = 40
    Top = 628
    Width = 75
    Height = 25
    TabOrder = 8
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 160
    Top = 628
    Width = 75
    Height = 25
    TabOrder = 9
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 280
    Top = 628
    Width = 75
    Height = 25
    TabOrder = 10
    OnClick = btnHelpClick
    Kind = bkHelp
  end
  object edtPlantAge: TEdit
    Left = 143
    Top = 504
    Width = 52
    Height = 21
    TabOrder = 1
    Text = 'Age'
  end
  object edtStocking: TEdit
    Left = 287
    Top = 504
    Width = 52
    Height = 21
    TabOrder = 2
    Text = 'Stocking'
  end
  object edtStartDay: TEdit
    Left = 247
    Top = 584
    Width = 23
    Height = 21
    TabOrder = 5
    Text = 'Day'
  end
  object edtStartMonth: TEdit
    Left = 279
    Top = 584
    Width = 23
    Height = 21
    TabOrder = 6
    Text = 'Month'
  end
  object edtStartYear: TEdit
    Left = 311
    Top = 584
    Width = 34
    Height = 21
    TabOrder = 7
    Text = 'Year'
  end
  object edtHeight: TEdit
    Left = 143
    Top = 536
    Width = 52
    Height = 21
    TabOrder = 3
    Text = 'Height'
  end
  object edtDBH: TEdit
    Left = 287
    Top = 536
    Width = 52
    Height = 21
    TabOrder = 4
    Text = 'DBH'
  end
  object grpWeeds: TGroupBox
    Left = 40
    Top = 400
    Width = 457
    Height = 89
    Caption = 'Weed pools (kg ha-1)'
    TabOrder = 11
    object lblWeedLeavesC: TLabel
      Left = 21
      Top = 25
      Width = 67
      Height = 13
      AutoSize = False
      Caption = 'Leaves'
      WordWrap = True
    end
    object lblWeedRootsC: TLabel
      Left = 22
      Top = 59
      Width = 67
      Height = 13
      AutoSize = False
      Caption = 'Roots'
      WordWrap = True
    end
    object lblWeedLeavesN: TLabel
      Left = 165
      Top = 33
      Width = 67
      Height = 13
      AutoSize = False
      Caption = 'Leaf N'
      WordWrap = True
    end
    object lblWeedRootsN: TLabel
      Left = 165
      Top = 59
      Width = 67
      Height = 13
      AutoSize = False
      Caption = 'Root N'
      WordWrap = True
    end
    object Label43: TLabel
      Left = 334
      Top = 28
      Width = 51
      Height = 13
      AutoSize = False
      Caption = 'Leaf P'
      WordWrap = True
    end
    object Label44: TLabel
      Left = 334
      Top = 59
      Width = 35
      Height = 13
      AutoSize = False
      Caption = 'Root P'
      WordWrap = True
    end
    object edtWeedLeavesC: TEdit
      Left = 94
      Top = 25
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'WeedCLeaves'
    end
    object edtWeedRootsC: TEdit
      Left = 95
      Top = 55
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'WeedCRoots'
    end
    object edtWeedLeavesN: TEdit
      Left = 238
      Top = 25
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'WeedNLeaves'
    end
    object edtWeedRootsN: TEdit
      Left = 238
      Top = 55
      Width = 52
      Height = 21
      TabOrder = 3
      Text = 'WeedNRoots'
    end
    object edtWeedLeavesP: TEdit
      Left = 391
      Top = 22
      Width = 52
      Height = 21
      TabOrder = 4
      Text = 'WeedPLeaves'
    end
    object edtWeedRootsP: TEdit
      Left = 391
      Top = 55
      Width = 52
      Height = 21
      TabOrder = 5
      Text = 'WeedPRoots'
    end
  end
end
