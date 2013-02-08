object frmGenericListDialogue: TfrmGenericListDialogue
  Left = 224
  Top = 212
  BorderStyle = bsDialog
  Caption = 'Generic list dialogue '
  ClientHeight = 433
  ClientWidth = 786
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
  object lblTextBox: TLabel
    Left = 608
    Top = 99
    Width = 169
    Height = 13
    AutoSize = False
    Caption = 'Threshold temperature for heat sum'
    WordWrap = True
  end
  object grpTable: TGroupBox
    Left = 8
    Top = 32
    Width = 553
    Height = 337
    Caption = 'Data entry'
    TabOrder = 0
    object Label1: TLabel
      Left = 24
      Top = 16
      Width = 41
      Height = 41
      AutoSize = False
      Caption = 'Label 1'
      Visible = False
      WordWrap = True
    end
    object Label2: TLabel
      Left = 80
      Top = 16
      Width = 41
      Height = 41
      AutoSize = False
      Caption = 'Label 2'
      Visible = False
      WordWrap = True
    end
    object Label3: TLabel
      Left = 136
      Top = 16
      Width = 41
      Height = 41
      AutoSize = False
      Caption = 'Label 3'
      Visible = False
      WordWrap = True
    end
    object Label4: TLabel
      Left = 184
      Top = 16
      Width = 41
      Height = 41
      AutoSize = False
      Caption = 'Label 4'
      Visible = False
      WordWrap = True
    end
    object Label5: TLabel
      Left = 232
      Top = 16
      Width = 41
      Height = 35
      AutoSize = False
      Caption = 'Label 5'
      Visible = False
      WordWrap = True
    end
    object Label6: TLabel
      Left = 272
      Top = 16
      Width = 41
      Height = 41
      AutoSize = False
      Caption = 'Label 6'
      Visible = False
      WordWrap = True
    end
    object Label7: TLabel
      Left = 312
      Top = 16
      Width = 41
      Height = 41
      AutoSize = False
      Caption = 'Label 7'
      Visible = False
      WordWrap = True
    end
    object Label8: TLabel
      Left = 352
      Top = 16
      Width = 41
      Height = 41
      AutoSize = False
      Caption = 'Label 8'
      Visible = False
      WordWrap = True
    end
    object Label9: TLabel
      Left = 392
      Top = 16
      Width = 41
      Height = 41
      AutoSize = False
      Caption = 'Label 9'
      Visible = False
      WordWrap = True
    end
    object Label10: TLabel
      Left = 432
      Top = 16
      Width = 41
      Height = 41
      AutoSize = False
      Caption = 'Label 10'
      Visible = False
      WordWrap = True
    end
  end
  object sgTable: TStringGrid
    Left = 17
    Top = 96
    Width = 536
    Height = 265
    BiDiMode = bdLeftToRight
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs]
    ParentBiDiMode = False
    TabOrder = 15
    OnClick = sgTableClick
    ColWidths = (
      64
      60
      38
      47
      64)
    RowHeights = (
      24
      22
      24
      24
      24)
  end
  object btnOK: TBitBtn
    Left = 8
    Top = 392
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 11
    OnClick = btnOKClick
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
  object btnCancel: TBitBtn
    Left = 112
    Top = 392
    Width = 75
    Height = 25
    TabOrder = 12
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 216
    Top = 392
    Width = 75
    Height = 25
    HelpContext = 8400
    TabOrder = 13
    Kind = bkHelp
  end
  object btnSave: TBitBtn
    Left = 608
    Top = 280
    Width = 163
    Height = 25
    Caption = 'Save Sequence'
    TabOrder = 8
    OnClick = btnSaveClick
  end
  object btnLoad: TBitBtn
    Left = 608
    Top = 312
    Width = 163
    Height = 25
    Caption = 'Load Sequence'
    TabOrder = 9
    OnClick = btnLoadClick
  end
  object btnDeleteAll: TBitBtn
    Left = 608
    Top = 248
    Width = 163
    Height = 25
    Caption = 'Delete All'
    TabOrder = 7
    OnClick = btnDeleteAllClick
  end
  object btnDelete: TBitBtn
    Left = 608
    Top = 216
    Width = 163
    Height = 25
    Caption = 'Delete'
    TabOrder = 6
    OnClick = btnDeleteClick
  end
  object btnDuplicate: TBitBtn
    Left = 608
    Top = 184
    Width = 163
    Height = 25
    Caption = 'Duplicate'
    TabOrder = 5
    OnClick = btnDuplicateClick
  end
  object btnInsert: TBitBtn
    Left = 608
    Top = 152
    Width = 163
    Height = 25
    Caption = 'Insert'
    TabOrder = 4
    OnClick = btnInsertClick
  end
  object rgOne: TRadioGroup
    Left = 608
    Top = 8
    Width = 161
    Height = 33
    Caption = 'Radio 1'
    Items.Strings = (
      'Line 1')
    TabOrder = 1
    OnClick = rgOneClick
  end
  object rgTwo: TRadioGroup
    Left = 608
    Top = 56
    Width = 161
    Height = 33
    Caption = 'Radio 2'
    Items.Strings = (
      'Line 1')
    TabOrder = 2
    OnClick = rgTwoClick
  end
  object edtTextBox: TEdit
    Left = 665
    Top = 119
    Width = 52
    Height = 21
    TabOrder = 3
    Text = 'EdtBox'
  end
  object btnRedraw: TBitBtn
    Left = 320
    Top = 392
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Redraw'
    ModalResult = 1
    TabOrder = 14
    OnClick = btnRedrawClick
    NumGlyphs = 2
  end
  object btnLoadExtra: TBitBtn
    Left = 608
    Top = 344
    Width = 163
    Height = 25
    Caption = 'Load extra sequence'
    TabOrder = 10
    OnClick = btnLoadExtraClick
  end
  object dlgLoadSequence: TOpenDialog
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 568
    Top = 392
  end
  object dlgSaveSequence: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofShowHelp, ofEnableSizing]
    Left = 600
    Top = 392
  end
end
