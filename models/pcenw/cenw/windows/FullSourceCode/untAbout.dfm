object frmAbout: TfrmAbout
  Left = 277
  Top = 194
  BorderStyle = bsDialog
  Caption = 'About CenW'
  ClientHeight = 391
  ClientWidth = 354
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
  object btnOK: TBitBtn
    Left = 96
    Top = 352
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object btnHelp: TBitBtn
    Left = 192
    Top = 352
    Width = 75
    Height = 25
    HelpContext = 4100
    Enabled = False
    TabOrder = 1
    Kind = bkHelp
  end
  object Panel1: TPanel
    Left = 72
    Top = 8
    Width = 209
    Height = 169
    BevelOuter = bvLowered
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 193
      Height = 17
      Alignment = taCenter
      AutoSize = False
      Caption = 'CenW    Version 3.1'
      WordWrap = True
    end
    object Label2: TLabel
      Left = 8
      Top = 40
      Width = 193
      Height = 17
      Alignment = taCenter
      AutoSize = False
      Caption = 'Copyright (C) 2006 by'
      WordWrap = True
    end
    object Label3: TLabel
      Left = 8
      Top = 64
      Width = 193
      Height = 17
      Alignment = taCenter
      AutoSize = False
      Caption = 'CRC for Greenhouse Accounting'
      WordWrap = True
    end
    object Label5: TLabel
      Left = 8
      Top = 112
      Width = 193
      Height = 17
      Alignment = taCenter
      AutoSize = False
      Caption = 'GPO Box 475'
      WordWrap = True
    end
    object Label6: TLabel
      Left = 8
      Top = 128
      Width = 193
      Height = 17
      Alignment = taCenter
      AutoSize = False
      Caption = 'Canberra   ACT 2601'
      WordWrap = True
    end
    object Label7: TLabel
      Left = 8
      Top = 144
      Width = 193
      Height = 17
      Alignment = taCenter
      AutoSize = False
      Caption = 'Australia'
      WordWrap = True
    end
    object Label4: TLabel
      Left = 8
      Top = 83
      Width = 193
      Height = 17
      Alignment = taCenter
      AutoSize = False
      Caption = 'Australian National University'
      WordWrap = True
    end
  end
  object Panel2: TPanel
    Left = 120
    Top = 184
    Width = 113
    Height = 81
    BevelOuter = bvLowered
    TabOrder = 3
    object Label8: TLabel
      Left = 40
      Top = 8
      Width = 65
      Height = 17
      AutoSize = False
      Caption = 'CARBON'
      WordWrap = True
    end
    object Label9: TLabel
      Left = 40
      Top = 24
      Width = 65
      Height = 17
      AutoSize = False
      Caption = 'ENERGY'
      WordWrap = True
    end
    object Label10: TLabel
      Left = 40
      Top = 40
      Width = 65
      Height = 17
      AutoSize = False
      Caption = 'NUTRIENTS'
      WordWrap = True
    end
    object Label11: TLabel
      Left = 40
      Top = 56
      Width = 65
      Height = 17
      AutoSize = False
      Caption = 'WATER'
      WordWrap = True
    end
    object Label12: TLabel
      Left = 16
      Top = 8
      Width = 17
      Height = 17
      AutoSize = False
      Caption = 'C'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object Label13: TLabel
      Left = 16
      Top = 24
      Width = 17
      Height = 17
      AutoSize = False
      Caption = 'E'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object Label14: TLabel
      Left = 16
      Top = 40
      Width = 17
      Height = 17
      AutoSize = False
      Caption = 'N'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object Label15: TLabel
      Left = 16
      Top = 56
      Width = 17
      Height = 17
      AutoSize = False
      Caption = 'W'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
  end
  object Panel3: TPanel
    Left = 32
    Top = 272
    Width = 289
    Height = 49
    BevelOuter = bvLowered
    TabOrder = 4
    object Label16: TLabel
      Left = 8
      Top = 8
      Width = 273
      Height = 17
      Alignment = taCenter
      AutoSize = False
      Caption = 'Author: Miko Kirschbaum, with input from'
      WordWrap = True
    end
    object Label17: TLabel
      Left = 8
      Top = 24
      Width = 273
      Height = 17
      Alignment = taCenter
      AutoSize = False
      Caption = 'Steffen Noe, Michael Reed and Guillaume Simioni.'
      WordWrap = True
    end
  end
  object Panel4: TPanel
    Left = 19
    Top = 22
    Width = 34
    Height = 34
    AutoSize = True
    BevelOuter = bvLowered
    TabOrder = 5
    object Image1: TImage
      Left = 1
      Top = 1
      Width = 32
      Height = 32
      AutoSize = True
      Picture.Data = {
        055449636F6E0000010001002020100000000000E80200001600000028000000
        2000000040000000010004000000000080020000000000000000000000000000
        0000000000000000000080000080000000808000800000008000800080800000
        80808000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
        FFFFFF0011991111719911111171199111179911119917117999111777111999
        1179997111991177199917771111199917799917119917719999777111117999
        7119991117997119999911771117799971799977179971999199777777779979
        9199799711991999119911177777991997991991119919911199111777119919
        91991991EE9999EEEE99EEE777EE99E99E99E99EEE9999EEEE99EEE777EE99EE
        999EE99EEE999EEEEE99EEE777E99EEE999EEE99EE99EEEEEE99EEE777E99EEE
        999EEE99EEEEEEEE2722EEE777EEEEEEEEEEEEEEEEEEE2EEE227277777EEEE2E
        EEEEEEEEEEEE22E2E2E2722777EEE2722EEEEEEEEEE2722EEEE22EE777EEE277
        22EE2EEEEEE22772E2EEEEE777EEEE227EE2E2EEEEE772277222EEE77722EE27
        7222272EEEEEE272277222E7772222EE722772EEEEEE9999E27222E277722229
        9999999EEE99999997272227777722E99999999EEE99EEE99927777772277279
        972222EEE99EEEEE9972272E2EE277299E2EEEEEE99EEEE277227272E2722779
        92EEEEEEE99EE227222277222E7E2279999992EEE99E2272E22772EE727E2EE9
        999997EEE99EE2EEEE2722E227E2E7299EEEE22EE99922EE99772722EE227229
        92EE2E22EE99EEE9997272E2227777299EEEE2EEEE9999999EE2EEE2E2222729
        9999999EEE279999EE2EEEEE22EEE7799999999EEEE2E7E2EEEEEEEEEEEEE22E
        E2EEEEEE00000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000}
    end
  end
end
