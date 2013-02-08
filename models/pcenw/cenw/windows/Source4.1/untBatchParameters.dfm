object frmBatchParameters: TfrmBatchParameters
  Left = 593
  Top = 253
  Caption = 'Batch'
  ClientHeight = 234
  ClientWidth = 383
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
  object grpBatch: TGroupBox
    Left = 21
    Top = 19
    Width = 337
    Height = 49
    Caption = 'Batch file'
    TabOrder = 0
    object edtBatch: TEdit
      Left = 8
      Top = 16
      Width = 257
      Height = 21
      TabOrder = 0
    end
    object btnBatch: TButton
      Left = 272
      Top = 16
      Width = 57
      Height = 21
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnBatchClick
    end
  end
  object btnOK: TButton
    Left = 24
    Top = 192
    Width = 57
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 96
    Top = 192
    Width = 57
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object grpDisplayNum: TGroupBox
    Left = 24
    Top = 72
    Width = 209
    Height = 49
    Caption = 'Maximum number of iterations (-1 for all)'
    TabOrder = 1
    object edtBatchRuns: TEdit
      Left = 77
      Top = 17
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'n batches'
    end
  end
  object btnHelp: TBitBtn
    Left = 170
    Top = 191
    Width = 67
    Height = 25
    HelpContext = 2700
    TabOrder = 4
    Kind = bkHelp
  end
  object grpLength: TGroupBox
    Left = 24
    Top = 127
    Width = 337
    Height = 49
    Caption = 'Start saving data in output file'
    TabOrder = 5
    object lblYears: TLabel
      Left = 8
      Top = 21
      Width = 35
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Years'
    end
    object lblMonths: TLabel
      Left = 120
      Top = 21
      Width = 41
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Months'
    end
    object lblDays: TLabel
      Left = 224
      Top = 21
      Width = 40
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Days'
    end
    object edtYears: TEdit
      Left = 61
      Top = 18
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Years'
    end
    object edtMonths: TEdit
      Left = 173
      Top = 18
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'Months'
    end
    object edtDays: TEdit
      Left = 269
      Top = 18
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'Days'
    end
  end
  object btnGotoList: TButton
    Left = 248
    Top = 87
    Width = 113
    Height = 25
    Caption = 'Select batch variables'
    TabOrder = 6
    OnClick = btnGotoListClick
  end
  object dlgOpenBatch: TOpenDialog
    HelpContext = 1210
    DefaultExt = 'cl!'
    Filter = 'CenW batch files (*.bt!)|*.bt!'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a batch file to use...'
    Left = 184
    Top = 32
  end
end
