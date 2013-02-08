object frmFittingParameters: TfrmFittingParameters
  Left = 274
  Top = 288
  ActiveControl = btnOK
  Caption = 'Parameter fitting controls'
  ClientHeight = 388
  ClientWidth = 613
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
  object Label17: TLabel
    Left = 25
    Top = 180
    Width = 176
    Height = 28
    AutoSize = False
    Caption = 
      'Exponent in random paramete search (0.5 .. 5). Larger is more co' +
      'nservative'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 25
    Top = 245
    Width = 152
    Height = 28
    AutoSize = False
    Caption = 'Minimum change for parameter to be declared '#39'changed'#39' (%)'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 25
    Top = 211
    Width = 144
    Height = 28
    AutoSize = False
    Caption = 'Maximum number of parameters to randomise'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 25
    Top = 279
    Width = 144
    Height = 28
    AutoSize = False
    Caption = 'Minimum value for multiplier in changing parameters (* 1e6)'
    WordWrap = True
  end
  object Label4: TLabel
    Left = 361
    Top = 180
    Width = 120
    Height = 28
    AutoSize = False
    Caption = 'Delta parameter change to establish slopes (%)'
    WordWrap = True
  end
  object lblCriterion3: TLabel
    Left = 415
    Top = 53
    Width = 120
    Height = 44
    AutoSize = False
    Caption = 'Ratio for changing parameters for individual best pameters'
    WordWrap = True
  end
  object Label5: TLabel
    Left = 361
    Top = 214
    Width = 120
    Height = 28
    AutoSize = False
    Caption = 'Maximum change in parameters per step'
    WordWrap = True
  end
  object btnOK: TButton
    Left = 40
    Top = 339
    Width = 57
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 136
    Top = 339
    Width = 57
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object edtMaxIterations: TGroupBox
    Left = 16
    Top = 8
    Width = 97
    Height = 49
    Caption = 'Maximum iterations'
    TabOrder = 0
    object edtEquilRuns: TEdit
      Left = 24
      Top = 18
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Max iterations'
    end
  end
  object btnHelp: TBitBtn
    Left = 230
    Top = 339
    Width = 67
    Height = 25
    HelpContext = 2800
    DoubleBuffered = True
    Kind = bkHelp
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 3
  end
  object grpClimate: TGroupBox
    Left = 16
    Top = 99
    Width = 337
    Height = 49
    Caption = 'Observational data file'
    TabOrder = 4
    object edtObsFile: TEdit
      Left = 9
      Top = 16
      Width = 257
      Height = 21
      TabOrder = 0
    end
    object btnObservation: TButton
      Left = 272
      Top = 16
      Width = 57
      Height = 21
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnObservationClick
    end
  end
  object chkIncludeWeights: TCheckBox
    Left = 144
    Top = 16
    Width = 209
    Height = 17
    Caption = 'Include weightings with observations'
    TabOrder = 5
  end
  object chkRunBatch: TCheckBox
    Left = 144
    Top = 39
    Width = 209
    Height = 42
    Caption = 
      'Run batch mode for parameter fitting. Ensure that batch file and' +
      ' paramters have been set up corrcetly.'
    TabOrder = 6
    WordWrap = True
    OnClick = chkRunBatchClick
  end
  object edtMonteCarloExponent: TEdit
    Left = 237
    Top = 184
    Width = 52
    Height = 21
    TabOrder = 7
    Text = 'MC exponent'
  end
  object edtCriterion1: TEdit
    Left = 237
    Top = 246
    Width = 52
    Height = 21
    TabOrder = 8
    Text = 'min change'
  end
  object edtMaxToRandomise: TEdit
    Left = 237
    Top = 213
    Width = 52
    Height = 21
    TabOrder = 9
    Text = 'Max to randomise'
  end
  object edtCriterion2: TEdit
    Left = 237
    Top = 280
    Width = 52
    Height = 21
    TabOrder = 10
    Text = 'Min muliplier'
  end
  object chkBestIndividual: TCheckBox
    Left = 396
    Top = 16
    Width = 209
    Height = 31
    Caption = 
      'Check whether individual parameters give better fit improvement ' +
      'than matrix'
    TabOrder = 11
    WordWrap = True
    OnClick = chkBestIndividualClick
  end
  object edtDelta: TEdit
    Left = 541
    Top = 184
    Width = 52
    Height = 21
    TabOrder = 12
    Text = 'min change'
  end
  object edtCriterion3: TEdit
    Left = 541
    Top = 60
    Width = 52
    Height = 21
    TabOrder = 13
    Text = 'min change'
  end
  object edtMaxChange: TEdit
    Left = 541
    Top = 218
    Width = 52
    Height = 21
    TabOrder = 14
    Text = 'max change'
  end
  object dlgOpenObservationFile: TOpenDialog
    HelpContext = 1210
    DefaultExt = 'ob!'
    Filter = 'Observation files (*.ob!)|*.ob!'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a climate file to use...'
    Left = 192
    Top = 115
  end
end
