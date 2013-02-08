object frmWeedParameters: TfrmWeedParameters
  Left = 380
  Top = 230
  BorderStyle = bsDialog
  Caption = 'Weather Parameters'
  ClientHeight = 202
  ClientWidth = 409
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
  object Label9: TLabel
    Left = 202
    Top = 19
    Width = 105
    Height = 18
    AutoSize = False
    Caption = 'Foliage turn-over (yr-1)'
    WordWrap = True
  end
  object Label10: TLabel
    Left = 24
    Top = 24
    Width = 89
    Height = 13
    AutoSize = False
    Caption = 'Maximal height'
    WordWrap = True
  end
  object Label11: TLabel
    Left = 202
    Top = 43
    Width = 87
    Height = 28
    AutoSize = False
    Caption = 'Allocation to new foliage growth'
    WordWrap = True
  end
  object Label12: TLabel
    Left = 24
    Top = 43
    Width = 83
    Height = 30
    AutoSize = False
    Caption = 'Half height at leaf biomass DW'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 202
    Top = 79
    Width = 83
    Height = 42
    AutoSize = False
    Caption = #39'KM'#39' for tree root weight for half maximal P uptake'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 20
    Top = 79
    Width = 87
    Height = 37
    AutoSize = False
    Caption = #39'KM'#39' for tree root weight for half maximal N uptake'
    WordWrap = True
  end
  object btnOK: TBitBtn
    Left = 56
    Top = 152
    Width = 75
    Height = 25
    TabOrder = 4
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 160
    Top = 152
    Width = 75
    Height = 25
    TabOrder = 5
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 264
    Top = 152
    Width = 75
    Height = 25
    HelpContext = 3600
    TabOrder = 6
    Kind = bkHelp
  end
  object edtSenescence: TEdit
    Left = 317
    Top = 17
    Width = 52
    Height = 21
    TabOrder = 1
    Text = 'Snescence'
  end
  object edtMaxheight: TEdit
    Left = 113
    Top = 20
    Width = 52
    Height = 21
    TabOrder = 0
    Text = 'Max height'
  end
  object edtLUE: TEdit
    Left = 317
    Top = 46
    Width = 52
    Height = 21
    TabOrder = 3
    Text = 'Alloc to leaves'
  end
  object edtKMheight: TEdit
    Left = 113
    Top = 47
    Width = 52
    Height = 21
    TabOrder = 2
    Text = 'KM height'
  end
  object edtKMRootPlantNUptake: TEdit
    Left = 113
    Top = 89
    Width = 52
    Height = 21
    TabOrder = 7
    Text = 'KM for N uptake'
  end
  object edtKMRootPlantPUptake: TEdit
    Left = 317
    Top = 89
    Width = 52
    Height = 21
    TabOrder = 8
    Text = 'KM for P uptake'
  end
end
