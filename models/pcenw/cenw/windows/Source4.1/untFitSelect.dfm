object frmFitSelect: TfrmFitSelect
  Left = 277
  Top = 194
  BorderStyle = bsDialog
  ClientHeight = 385
  ClientWidth = 1020
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
  object lblMin1: TLabel
    Left = 156
    Top = 3
    Width = 49
    Height = 11
    AutoSize = False
    Caption = 'Minimum'
    WordWrap = True
  end
  object lblMax1: TLabel
    Left = 266
    Top = 3
    Width = 49
    Height = 11
    AutoSize = False
    Caption = 'Maximum'
    WordWrap = True
  end
  object lblMin2: TLabel
    Left = 376
    Top = 3
    Width = 49
    Height = 11
    AutoSize = False
    Caption = 'Minimum'
    WordWrap = True
  end
  object lblCurrent2: TLabel
    Left = 431
    Top = 3
    Width = 49
    Height = 11
    AutoSize = False
    Caption = 'Current'
    WordWrap = True
  end
  object lblCurrent1: TLabel
    Left = 211
    Top = 3
    Width = 49
    Height = 11
    AutoSize = False
    Caption = 'Current'
    WordWrap = True
  end
  object lblMax2: TLabel
    Left = 486
    Top = 3
    Width = 49
    Height = 11
    AutoSize = False
    Caption = 'Maximum'
    WordWrap = True
  end
  object lblSumOfSquares: TLabel
    Left = 8
    Top = 203
    Width = 130
    Height = 14
    AutoSize = False
    Caption = 'Sum of Squares:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object btnOK: TBitBtn
    Left = 8
    Top = 352
    Width = 75
    Height = 25
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 97
    Top = 352
    Width = 75
    Height = 25
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object btnHelp: TBitBtn
    Left = 359
    Top = 352
    Width = 75
    Height = 25
    HelpContext = 6300
    DoubleBuffered = True
    Kind = bkHelp
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 2
  end
  object btnSelectAll: TBitBtn
    Left = 185
    Top = 352
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Select all'
    DoubleBuffered = True
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 3
    OnClick = btnSelectAllClick
  end
  object btnSelectNone: TBitBtn
    Left = 270
    Top = 352
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Select none'
    DoubleBuffered = True
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 4
    OnClick = btnSelectNoneClick
  end
  object edtSumOfSquaresInitial: TEdit
    Left = 144
    Top = 200
    Width = 72
    Height = 24
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    Text = 'SS initial'
  end
  object edtSumOfSquaresBest: TEdit
    Left = 243
    Top = 200
    Width = 72
    Height = 24
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    Text = 'SS best'
  end
end
