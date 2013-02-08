object frmSoilOrganicMatterDisplay: TfrmSoilOrganicMatterDisplay
  Left = 265
  Top = 100
  BorderStyle = bsDialog
  Caption = 'Display Options - Soil Organic Matter Pools'
  ClientHeight = 594
  ClientWidth = 379
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
  object lblMin: TLabel
    Left = 144
    Top = 16
    Width = 41
    Height = 13
    Caption = 'Minimum'
  end
  object lblMax: TLabel
    Left = 216
    Top = 16
    Width = 44
    Height = 13
    Caption = 'Maximum'
  end
  object btnOK: TBitBtn
    Left = 56
    Top = 560
    Width = 75
    Height = 25
    TabOrder = 0
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 136
    Top = 560
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 216
    Top = 560
    Width = 75
    Height = 25
    HelpContext = 7400
    TabOrder = 2
    Kind = bkHelp
  end
end
