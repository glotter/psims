object frmSensSelect: TfrmSensSelect
  Left = 277
  Top = 194
  BorderStyle = bsDialog
  ClientHeight = 385
  ClientWidth = 434
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
  object lblRange: TLabel
    Left = 19
    Top = 16
    Width = 153
    Height = 60
    Caption = 'Select percentage change for sensitivity test'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object lblPercent: TLabel
    Left = 266
    Top = 35
    Width = 16
    Height = 20
    Caption = '%'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
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
    TabOrder = 0
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 97
    Top = 352
    Width = 75
    Height = 25
    TabOrder = 1
    OnClick = btnCancelClick
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 359
    Top = 352
    Width = 75
    Height = 25
    HelpContext = 6300
    TabOrder = 2
    Kind = bkHelp
  end
  object edtRange: TEdit
    Left = 178
    Top = 32
    Width = 82
    Height = 28
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    Text = 'edtRange'
  end
  object btnSelectAll: TBitBtn
    Left = 185
    Top = 352
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Select all'
    TabOrder = 4
    OnClick = btnSelectAllClick
    NumGlyphs = 2
  end
  object btnSelectNone: TBitBtn
    Left = 270
    Top = 352
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Select none'
    TabOrder = 5
    OnClick = btnSelectNoneClick
    NumGlyphs = 2
  end
end
