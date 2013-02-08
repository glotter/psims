object frmSaveWeatherInfo: TfrmSaveWeatherInfo
  Left = 277
  Top = 194
  BorderStyle = bsDialog
  Caption = 'Save Weather Variables'
  ClientHeight = 385
  ClientWidth = 250
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
    Left = 88
    Top = 352
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 168
    Top = 352
    Width = 75
    Height = 25
    HelpContext = 6300
    TabOrder = 2
    Kind = bkHelp
  end
  object rgSaveGeneric: TRadioGroup
    Left = 97
    Top = 16
    Width = 153
    Height = 33
    Caption = 'Save'
    Columns = 3
    Items.Strings = (
      'All'
      'None'
      'As is')
    TabOrder = 3
    OnClick = rgSaveGenericClick
  end
end
