object frmDecompositionParameters: TfrmDecompositionParameters
  Left = 269
  Top = 169
  BorderStyle = bsDialog
  Caption = 'Decomposition Parameters'
  ClientHeight = 386
  ClientWidth = 970
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
  object Label0: TLabel
    Left = 16
    Top = 19
    Width = 89
    Height = 30
    AutoSize = False
    Caption = 'Critcal C:N ratio of the active pool'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 184
    Top = 19
    Width = 81
    Height = 30
    AutoSize = False
    Caption = 'Decomposition rate adjustment'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 344
    Top = 21
    Width = 97
    Height = 28
    AutoSize = False
    Caption = 'Fertility adjustment at start-up'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 536
    Top = 13
    Width = 113
    Height = 44
    AutoSize = False
    Caption = 'Decomposibility of inert organic matter relative to resistant OM'
    WordWrap = True
  end
  object Label4: TLabel
    Left = 16
    Top = 69
    Width = 89
    Height = 44
    AutoSize = False
    Caption = 'Ratio of C:N ratios in structural and metabolic litter'
    WordWrap = True
  end
  object Label5: TLabel
    Left = 184
    Top = 75
    Width = 65
    Height = 30
    AutoSize = False
    Caption = 'Percent silt and clay'
    WordWrap = True
  end
  object Label6: TLabel
    Left = 344
    Top = 73
    Width = 97
    Height = 32
    AutoSize = False
    Caption = 'Mineral N fraction immobilised (g kg-1)'
    WordWrap = True
  end
  object Label7: TLabel
    Left = 536
    Top = 69
    Width = 97
    Height = 41
    AutoSize = False
    Caption = 'Percentage of N immobilised in the slow pool'
    WordWrap = True
  end
  object Label11: TLabel
    Left = 536
    Top = 126
    Width = 113
    Height = 44
    AutoSize = False
    Caption = 
      'Term describing the extent of lignin inhibition of litter decomp' +
      'osition'
    WordWrap = True
  end
  object Label15: TLabel
    Left = 536
    Top = 182
    Width = 113
    Height = 44
    AutoSize = False
    Caption = 'Residual decomposition activity under extremely dry conditions'
    WordWrap = True
  end
  object Label14: TLabel
    Left = 335
    Top = 205
    Width = 113
    Height = 41
    AutoSize = False
    Caption = 'Decomp. sensitivity to water stress relative to plant processes'
    WordWrap = True
  end
  object Label18: TLabel
    Left = 736
    Top = 19
    Width = 89
    Height = 30
    AutoSize = False
    Caption = 'Critcal C:P ratios of the active pool'
    WordWrap = True
  end
  object Label19: TLabel
    Left = 736
    Top = 69
    Width = 89
    Height = 44
    AutoSize = False
    Caption = 'Ratio of C:P ratios in structural and metabolic litter'
    WordWrap = True
  end
  object Label17: TLabel
    Left = 330
    Top = 272
    Width = 105
    Height = 44
    AutoSize = False
    Caption = 'Annual % OM trans- ferred from surface to 1st soil layer'
    WordWrap = True
  end
  object lblWeathering: TLabel
    Left = 736
    Top = 130
    Width = 89
    Height = 26
    AutoSize = False
    Caption = 'Rock weathering (% per 100 years)'
    WordWrap = True
  end
  object Label16: TLabel
    Left = 736
    Top = 182
    Width = 89
    Height = 47
    AutoSize = False
    Caption = 'P occlusion (% of secondary P per 100 years)'
    WordWrap = True
  end
  object Label20: TLabel
    Left = 736
    Top = 235
    Width = 89
    Height = 31
    AutoSize = False
    Caption = 'Labile to 2ary inorganic P (%)'
    WordWrap = True
  end
  object Label21: TLabel
    Left = 736
    Top = 272
    Width = 89
    Height = 31
    AutoSize = False
    Caption = '2ary inorganic to labile P (%) '
    WordWrap = True
  end
  object lblPhosphatase: TLabel
    Left = 736
    Top = 309
    Width = 89
    Height = 44
    AutoSize = False
    Caption = 'Phosphatase act. (% organic P per year at max P lim)'
    WordWrap = True
  end
  object lblCPmax: TLabel
    Left = 896
    Top = 13
    Width = 41
    Height = 13
    AutoSize = False
    Caption = 'max'
    WordWrap = True
  end
  object lblCPmin: TLabel
    Left = 896
    Top = 40
    Width = 41
    Height = 13
    AutoSize = False
    Caption = 'min'
    WordWrap = True
  end
  object btnOK: TBitBtn
    Left = 256
    Top = 344
    Width = 75
    Height = 25
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 24
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 360
    Top = 344
    Width = 75
    Height = 25
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 25
  end
  object btnHelp: TBitBtn
    Left = 464
    Top = 344
    Width = 75
    Height = 25
    HelpContext = 3100
    DoubleBuffered = True
    Kind = bkHelp
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 26
  end
  object grpLignin: TGroupBox
    Left = 8
    Top = 120
    Width = 505
    Height = 57
    Caption = 'Lignin concentrations'
    TabOrder = 11
    object Label8: TLabel
      Left = 16
      Top = 25
      Width = 65
      Height = 14
      AutoSize = False
      Caption = 'Dead foliage'
      WordWrap = True
    end
    object Label9: TLabel
      Left = 176
      Top = 25
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Dead roots'
      WordWrap = True
    end
    object Label10: TLabel
      Left = 368
      Top = 25
      Width = 41
      Height = 13
      AutoSize = False
      Caption = 'Wood'
      WordWrap = True
    end
    object edtLeafLignin: TEdit
      Left = 103
      Top = 22
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Leaf lignin'
    end
    object edtRootLignin: TEdit
      Left = 255
      Top = 22
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'Leaf lignin'
    end
    object edtWoodLignin: TEdit
      Left = 439
      Top = 22
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'Wood lignin'
    end
  end
  object grpCoarseLitter: TGroupBox
    Left = 8
    Top = 192
    Width = 321
    Height = 57
    Caption = 'Decomposability relative to structural litter'
    TabOrder = 14
    object Label12: TLabel
      Left = 16
      Top = 27
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Branches'
      WordWrap = True
    end
    object Label13: TLabel
      Left = 184
      Top = 27
      Width = 65
      Height = 13
      AutoSize = False
      Caption = 'Dead stems'
      WordWrap = True
    end
    object edtBranchRatio: TEdit
      Left = 101
      Top = 23
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Branch decomposability'
    end
    object edtWoodRatio: TEdit
      Left = 253
      Top = 23
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'Wood decomposability'
    end
  end
  object btnLitterInput: TButton
    Left = 40
    Top = 344
    Width = 75
    Height = 25
    Caption = 'Litter Input'
    TabOrder = 23
    OnClick = btnLitterInputClick
  end
  object edtCriticalCN: TEdit
    Left = 111
    Top = 23
    Width = 52
    Height = 21
    TabOrder = 0
    Text = 'Critical C:N'
  end
  object edtRateAdjust: TEdit
    Left = 263
    Top = 23
    Width = 52
    Height = 21
    TabOrder = 2
    Text = 'Rate adjustment'
  end
  object edtFertilityAdjust: TEdit
    Left = 447
    Top = 23
    Width = 52
    Height = 21
    TabOrder = 4
    Text = 'Fertility adjustment'
  end
  object edtInertOM: TEdit
    Left = 655
    Top = 23
    Width = 52
    Height = 21
    TabOrder = 6
    Text = 'Inert organic matter'
  end
  object edtRelativeCN: TEdit
    Left = 111
    Top = 79
    Width = 52
    Height = 21
    TabOrder = 1
    Text = 'Relative C:N'
  end
  object edtFineSoil: TEdit
    Left = 263
    Top = 79
    Width = 52
    Height = 21
    TabOrder = 3
    Text = 'Fine soil'
  end
  object edtImmobilise: TEdit
    Left = 447
    Top = 79
    Width = 52
    Height = 21
    TabOrder = 5
    Text = 'Imoobilised'
  end
  object edtImmobiliseInSlow: TEdit
    Left = 655
    Top = 79
    Width = 52
    Height = 21
    TabOrder = 9
    Text = 'Immobilised in slow'
  end
  object edtLigninInhibition: TEdit
    Left = 655
    Top = 135
    Width = 52
    Height = 21
    TabOrder = 12
    Text = 'Lignin inhibition'
  end
  object edtMinDecomp: TEdit
    Left = 655
    Top = 192
    Width = 52
    Height = 21
    TabOrder = 16
    Text = 'Minimum decomposition'
  end
  object edtRelWaterSens: TEdit
    Left = 447
    Top = 215
    Width = 52
    Height = 21
    TabOrder = 15
    Text = 'Water sensitivity'
  end
  object edtCriticalCPmax: TEdit
    Left = 831
    Top = 10
    Width = 52
    Height = 21
    TabOrder = 7
    Text = 'Critical C:P(max)'
  end
  object edtRelativeCP: TEdit
    Left = 831
    Top = 79
    Width = 52
    Height = 21
    TabOrder = 10
    Text = 'Relative C:P'
  end
  object edtOMIncorporate: TEdit
    Left = 447
    Top = 280
    Width = 52
    Height = 21
    TabOrder = 19
    Text = 'OM transfer to the soil'
  end
  inline rgLayerOption: TRadioGroup
    Left = 8
    Top = 255
    Width = 241
    Height = 69
    Items.Strings = (
      'Soil simulated as a single bulked layer'
      'Dynamics of each layer modelled separately')
    TabOrder = 18
  end
  object edtWeathering: TEdit
    Left = 831
    Top = 135
    Width = 52
    Height = 21
    TabOrder = 13
    Text = 'Weathering'
  end
  object edtOccludedP_rate: TEdit
    Left = 831
    Top = 192
    Width = 52
    Height = 21
    TabOrder = 17
    Text = 'Occlusion'
  end
  object edtLabile_to_Secondary: TEdit
    Left = 831
    Top = 235
    Width = 52
    Height = 21
    TabOrder = 20
    Text = 'Labile to 2ndary'
  end
  object edtSecondary_to_Labile: TEdit
    Left = 831
    Top = 272
    Width = 52
    Height = 21
    TabOrder = 21
    Text = '2ndary to labile'
  end
  object edtPhosphatase: TEdit
    Left = 831
    Top = 317
    Width = 52
    Height = 21
    TabOrder = 22
    Text = 'Phosphatase'
  end
  object edtCriticalCPmin: TEdit
    Left = 831
    Top = 37
    Width = 52
    Height = 21
    TabOrder = 8
    Text = 'Critical C:P(min)'
  end
end
