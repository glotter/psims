object frmFileIO: TfrmFileIO
  Left = 316
  Top = 329
  BorderStyle = bsDialog
  Caption = 'Problems with File Loading'
  ClientHeight = 264
  ClientWidth = 256
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
  object LblInfo: TLabel
    Left = 24
    Top = 16
    Width = 200
    Height = 20
    Caption = 'What do you want to do?'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object btnSearch: TButton
    Left = 40
    Top = 56
    Width = 169
    Height = 25
    Caption = 'Search for suitable file'
    TabOrder = 0
    OnClick = btnSearchClick
  end
  object btnDefaults: TButton
    Left = 40
    Top = 96
    Width = 169
    Height = 25
    Caption = 'Run with default parameters'
    TabOrder = 1
    OnClick = btnDefaultsClick
  end
  object btnAbort: TButton
    Left = 40
    Top = 136
    Width = 169
    Height = 25
    Caption = 'Abort program execution'
    TabOrder = 2
    OnClick = btnAbortClick
  end
  object btnExtract: TButton
    Left = 40
    Top = 176
    Width = 169
    Height = 25
    Caption = 'Try to extract information from file'
    TabOrder = 3
    OnClick = btnExtractClick
  end
  object btnHelp: TButton
    Left = 40
    Top = 216
    Width = 169
    Height = 25
    Caption = 'Help'
    TabOrder = 4
  end
  object dlgOpenFile: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select the file'
    Left = 8
    Top = 64
  end
end
