object frmEquilParameters: TfrmEquilParameters
  Left = 274
  Top = 300
  ActiveControl = btnOK
  Caption = 'Equilibrium runs'
  ClientHeight = 392
  ClientWidth = 589
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblLine1: TLabel
    Left = 24
    Top = 181
    Width = 73
    Height = 13
    AutoSize = False
    Caption = 'Target value'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 256
    Top = 78
    Width = 130
    Height = 30
    AutoSize = False
    Caption = 'Value for convergence criterion 1 (Target) * 1000'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 256
    Top = 120
    Width = 113
    Height = 42
    AutoSize = False
    Caption = 'Value for convergence criterion 2 (Slow SON) * 1000'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 256
    Top = 168
    Width = 129
    Height = 45
    AutoSize = False
    Caption = 'Value for convergence criterion 3 (Resistant SON) * 1000'
    WordWrap = True
  end
  object Label4: TLabel
    Left = 256
    Top = 219
    Width = 129
    Height = 14
    AutoSize = False
    Caption = 'Minimum value for delta'
    WordWrap = True
  end
  object Label5: TLabel
    Left = 256
    Top = 251
    Width = 129
    Height = 14
    AutoSize = False
    Caption = 'Maximum value for delta'
    WordWrap = True
  end
  object Label6: TLabel
    Left = 256
    Top = 283
    Width = 137
    Height = 14
    AutoSize = False
    Caption = 'Percent adjustment in delta'
    WordWrap = True
  end
  object Label7: TLabel
    Left = 256
    Top = 315
    Width = 137
    Height = 14
    AutoSize = False
    Caption = 'Maximum change ratio'
    WordWrap = True
  end
  object Label8: TLabel
    Left = 256
    Top = 341
    Width = 129
    Height = 30
    AutoSize = False
    Caption = 'Boost param. for changes to resistant organic matter'
    WordWrap = True
  end
  object lblTargetPLimit: TLabel
    Left = 170
    Top = 148
    Width = 55
    Height = 29
    Alignment = taCenter
    AutoSize = False
    Caption = 'Target P limitation'
    WordWrap = True
  end
  object btnOK: TButton
    Left = 16
    Top = 359
    Width = 57
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 14
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 96
    Top = 359
    Width = 57
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 15
  end
  object edtMaxIterations: TGroupBox
    Left = 256
    Top = 16
    Width = 97
    Height = 49
    Caption = 'Maximum iterations'
    TabOrder = 4
    object edtEquilRuns: TEdit
      Left = 24
      Top = 18
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Max iterations'
    end
  end
  object rgEquilType: TRadioGroup
    Left = 16
    Top = 10
    Width = 217
    Height = 127
    Caption = 'Equilibrium target'
    Items.Strings = (
      'Soil organic matter (tC)'
      'Foliage N concentration (gN / kgDW)'
      'Foliage nitrogen amount (kgN)'
      'Foliage mass (tDW)'
      'Wood mass (tDW)')
    TabOrder = 0
  end
  object edtSteadyLimit: TGroupBox
    Left = 368
    Top = 16
    Width = 97
    Height = 49
    Caption = 'Steady count max'
    TabOrder = 5
    object edtGoodCount: TEdit
      Left = 24
      Top = 18
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Good count'
    end
  end
  object rgEquilParameter: TRadioGroup
    Left = 19
    Top = 219
    Width = 209
    Height = 63
    Caption = 'Parameter to change'
    Items.Strings = (
      'Biological N fixation'
      'N fractional loss')
    TabOrder = 2
  end
  object chkKeepPlantPools: TCheckBox
    Left = 24
    Top = 288
    Width = 201
    Height = 25
    Caption = 'Each run with same initial plant pools'
    TabOrder = 3
  end
  object btnHelp: TBitBtn
    Left = 170
    Top = 358
    Width = 67
    Height = 25
    HelpContext = 2800
    TabOrder = 16
    Kind = bkHelp
  end
  object edtTargetValue: TEdit
    Left = 96
    Top = 178
    Width = 52
    Height = 21
    TabOrder = 1
    Text = 'Target'
  end
  object edtCriterion1: TEdit
    Left = 392
    Top = 82
    Width = 52
    Height = 21
    TabOrder = 6
    Text = 'Criterion 1'
  end
  object edtCriterion2: TEdit
    Left = 392
    Top = 130
    Width = 52
    Height = 21
    TabOrder = 7
    Text = 'Criterion 2'
  end
  object edtCriterion3: TEdit
    Left = 392
    Top = 178
    Width = 52
    Height = 21
    TabOrder = 8
    Text = 'Criterion 3'
  end
  object edtDeltaMin: TEdit
    Left = 392
    Top = 215
    Width = 52
    Height = 21
    TabOrder = 9
    Text = 'Min delta'
  end
  object edtDeltaMax: TEdit
    Left = 392
    Top = 247
    Width = 52
    Height = 21
    TabOrder = 10
    Text = 'Max delta'
  end
  object edtDeltaAdjust: TEdit
    Left = 392
    Top = 279
    Width = 52
    Height = 21
    TabOrder = 11
    Text = '% delta'
  end
  object edtMaxChangeRatio: TEdit
    Left = 392
    Top = 311
    Width = 52
    Height = 21
    TabOrder = 12
    Text = 'Max change ratio'
  end
  object edtBoostResistant: TEdit
    Left = 392
    Top = 344
    Width = 52
    Height = 21
    TabOrder = 13
    Text = 'Boost resistant'
  end
  object edtTargetPlimit: TEdit
    Left = 170
    Top = 178
    Width = 52
    Height = 21
    TabOrder = 17
    Text = 'P Target'
  end
  object grpOscillation: TGroupBox
    Left = 480
    Top = 16
    Width = 97
    Height = 49
    Caption = 'Oscillation count'
    TabOrder = 18
    object edtOscillationCount: TEdit
      Left = 24
      Top = 18
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Good count'
    end
  end
  object chkAdjustPInput: TCheckBox
    Left = 24
    Top = 319
    Width = 201
    Height = 25
    Caption = 'Include adjustment to P input'
    TabOrder = 19
  end
end
