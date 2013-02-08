object frmSaveSoilInfo: TfrmSaveSoilInfo
  Left = 281
  Top = 34
  BorderStyle = bsDialog
  Caption = 'Save SOM Variables'
  ClientHeight = 649
  ClientWidth = 351
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
    Top = 616
    Width = 65
    Height = 25
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 104
    Top = 616
    Width = 65
    Height = 25
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 1
  end
  object btnHelp: TBitBtn
    Left = 200
    Top = 616
    Width = 65
    Height = 25
    HelpContext = 6200
    DoubleBuffered = True
    Kind = bkHelp
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 2
  end
  object rgOutputLayers: TRadioGroup
    Left = 201
    Top = 8
    Width = 112
    Height = 57
    Caption = 'Output per'
    Items.Strings = (
      'Bulked soil only'
      'Per layer')
    TabOrder = 3
  end
  object rgSaveGeneric: TRadioGroup
    Left = 8
    Top = 568
    Width = 257
    Height = 33
    Caption = 'Save'
    Columns = 3
    Items.Strings = (
      'All'
      'None'
      'As is')
    TabOrder = 4
    OnClick = rgSaveGenericClick
  end
end
