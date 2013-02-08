object frmEquilAdjust: TfrmEquilAdjust
  Left = 536
  Top = 281
  BorderStyle = bsDialog
  Caption = 'Adjust variables'
  ClientHeight = 183
  ClientWidth = 280
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblTop: TLabel
    Left = 48
    Top = 24
    Width = 176
    Height = 16
    Caption = 'Adjust these variables by'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblPerc1: TLabel
    Left = 240
    Top = 48
    Width = 12
    Height = 16
    Caption = '%'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblPerc3: TLabel
    Left = 240
    Top = 96
    Width = 12
    Height = 16
    Caption = '%'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblPerc2: TLabel
    Left = 240
    Top = 72
    Width = 12
    Height = 16
    Caption = '%'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblLine1: TLabel
    Left = 48
    Top = 51
    Width = 137
    Height = 13
    AutoSize = False
    Caption = 'Search variable'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 48
    Top = 75
    Width = 129
    Height = 13
    AutoSize = False
    Caption = 'Slow organic matter pools'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 48
    Top = 99
    Width = 129
    Height = 13
    AutoSize = False
    Caption = 'Resistant OM pools'
    WordWrap = True
  end
  object btnOK: TBitBtn
    Left = 104
    Top = 144
    Width = 81
    Height = 25
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 0
    OnClick = btnOKClick
  end
  object edtAdjustVariable: TEdit
    Left = 185
    Top = 47
    Width = 52
    Height = 21
    TabOrder = 1
    Text = 'Variable adjustment'
  end
  object edtAdjustSlow: TEdit
    Left = 185
    Top = 71
    Width = 52
    Height = 21
    TabOrder = 2
    Text = 'Slow pool adjustment'
  end
  object edtAdjustResistant: TEdit
    Left = 185
    Top = 95
    Width = 52
    Height = 21
    TabOrder = 3
    Text = 'Resistant pool adjustment'
  end
end
