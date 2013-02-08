object frmEquilFinished: TfrmEquilFinished
  Left = 487
  Top = 241
  Width = 473
  Height = 421
  Caption = 'Results of equilibrium search'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblFound: TLabel
    Left = 192
    Top = 56
    Width = 124
    Height = 16
    Caption = 'found or not found'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblFinal: TLabel
    Left = 256
    Top = 112
    Width = 72
    Height = 16
    Caption = 'Final values'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblWhat_to_do: TLabel
    Left = 16
    Top = 312
    Width = 439
    Height = 16
    Caption = 
      'Press '#39'OK'#39' to store these results or '#39'Cancel'#39' to ignore these ru' +
      'ns'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblSearchType: TLabel
    Left = 16
    Top = 16
    Width = 96
    Height = 20
    Caption = 'SearchType'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblLine1: TLabel
    Left = 24
    Top = 59
    Width = 73
    Height = 13
    AutoSize = False
    Caption = 'Target value'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 384
    Top = 59
    Width = 57
    Height = 13
    AutoSize = False
    Caption = 'iterations'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 24
    Top = 91
    Width = 73
    Height = 13
    AutoSize = False
    Caption = 'Final value'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 16
    Top = 131
    Width = 113
    Height = 30
    AutoSize = False
    Caption = 'Value for convergence criterion 1 (Target)'
    WordWrap = True
  end
  object Label4: TLabel
    Left = 16
    Top = 179
    Width = 113
    Height = 30
    AutoSize = False
    Caption = 'Value for convergence criterion 2 (Slow SON)'
    WordWrap = True
  end
  object Label5: TLabel
    Left = 16
    Top = 227
    Width = 129
    Height = 30
    AutoSize = False
    Caption = 'Value for convergence criterion 3 (Resistant SON)'
    WordWrap = True
  end
  object lblResult: TLabel
    Left = 16
    Top = 283
    Width = 129
    Height = 14
    AutoSize = False
    Caption = 'Biological N fixation '
    WordWrap = True
  end
  object Label6: TLabel
    Left = 160
    Top = 112
    Width = 34
    Height = 16
    Caption = 'Limits'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object btnOK: TButton
    Left = 80
    Top = 352
    Width = 57
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnHelp: TButton
    Left = 392
    Top = 352
    Width = 57
    Height = 25
    Caption = 'Help'
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 168
    Top = 352
    Width = 57
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object edtTargetValue: TEdit
    Left = 95
    Top = 55
    Width = 52
    Height = 21
    TabOrder = 3
    Text = 'Search target'
  end
  object edtIterations: TEdit
    Left = 335
    Top = 55
    Width = 42
    Height = 21
    TabOrder = 4
    Text = 'Search target'
  end
  object edtFinalValue: TEdit
    Left = 95
    Top = 87
    Width = 52
    Height = 21
    TabOrder = 5
    Text = 'Final value'
  end
  object edtCriterion1: TEdit
    Left = 151
    Top = 135
    Width = 52
    Height = 21
    TabOrder = 6
    Text = 'Criterion 1'
  end
  object edtConverg1: TEdit
    Left = 263
    Top = 135
    Width = 52
    Height = 21
    TabOrder = 7
    Text = 'Convergence 1'
  end
  object edtCriterion2: TEdit
    Left = 151
    Top = 183
    Width = 52
    Height = 21
    TabOrder = 8
    Text = 'Criterion 2'
  end
  object edtConverg2: TEdit
    Left = 263
    Top = 183
    Width = 52
    Height = 21
    TabOrder = 9
    Text = 'Convergence 2'
  end
  object edtCriterion3: TEdit
    Left = 151
    Top = 231
    Width = 52
    Height = 21
    TabOrder = 10
    Text = 'Criterion 3'
  end
  object edtConverg3: TEdit
    Left = 263
    Top = 231
    Width = 52
    Height = 21
    TabOrder = 11
    Text = 'Convergence 3'
  end
  object edtResult: TEdit
    Left = 151
    Top = 279
    Width = 52
    Height = 21
    TabOrder = 12
    Text = 'Result'
  end
end
