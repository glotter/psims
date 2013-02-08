object frmGenericListDialogue: TfrmGenericListDialogue
  Left = 224
  Top = 212
  BorderStyle = bsDialog
  Caption = 'Generic list dialogue '
  ClientHeight = 491
  ClientWidth = 786
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClick = ChkSetConstantClick
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblTextBox: TLabel
    Left = 615
    Top = 115
    Width = 169
    Height = 13
    AutoSize = False
    Caption = 'Text box label'
    WordWrap = True
  end
  object grpTable: TGroupBox
    Left = 8
    Top = 33
    Width = 553
    Height = 408
    Caption = 'Data entry'
    TabOrder = 14
    object Label1: TLabel
      Left = 24
      Top = 16
      Width = 41
      Height = 41
      Alignment = taCenter
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
      Alignment = taCenter
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
      Alignment = taCenter
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
      Alignment = taCenter
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
      Alignment = taCenter
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
      Alignment = taCenter
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
      Alignment = taCenter
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
      Alignment = taCenter
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
      Alignment = taCenter
      AutoSize = False
      Caption = 'Label 9'
      Visible = False
      WordWrap = True
    end
    object Label10: TLabel
      Left = 432
      Top = 17
      Width = 41
      Height = 41
      Alignment = taCenter
      AutoSize = False
      Caption = 'Label 10'
      Visible = False
      WordWrap = True
    end
    object Label11: TLabel
      Left = 479
      Top = 17
      Width = 41
      Height = 41
      Alignment = taCenter
      AutoSize = False
      Caption = 'Label 11'
      Visible = False
      WordWrap = True
    end
  end
  object sgTable: TStringGrid
    Left = 8
    Top = 96
    Width = 553
    Height = 345
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
    Left = 14
    Top = 458
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 10
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
    Left = 118
    Top = 458
    Width = 75
    Height = 25
    TabOrder = 11
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 222
    Top = 458
    Width = 75
    Height = 25
    HelpContext = 8400
    TabOrder = 12
    Kind = bkHelp
  end
  object btnSave: TBitBtn
    Left = 615
    Top = 297
    Width = 163
    Height = 25
    Caption = 'Save Sequence'
    TabOrder = 7
    OnClick = btnSaveClick
  end
  object btnLoad: TBitBtn
    Left = 615
    Top = 328
    Width = 163
    Height = 25
    Caption = 'Load Sequence'
    TabOrder = 8
    OnClick = btnLoadClick
  end
  object btnDeleteAll: TBitBtn
    Left = 615
    Top = 264
    Width = 163
    Height = 25
    Caption = 'Delete All'
    TabOrder = 6
    OnClick = btnDeleteAllClick
  end
  object btnDelete: TBitBtn
    Left = 615
    Top = 232
    Width = 163
    Height = 25
    Caption = 'Delete'
    TabOrder = 5
    OnClick = btnDeleteClick
  end
  object btnDuplicate: TBitBtn
    Left = 615
    Top = 200
    Width = 163
    Height = 25
    Caption = 'Duplicate'
    TabOrder = 4
    OnClick = btnDuplicateClick
  end
  object btnInsert: TBitBtn
    Left = 615
    Top = 168
    Width = 163
    Height = 25
    Caption = 'Insert'
    TabOrder = 3
    OnClick = btnInsertClick
  end
  object rgOne: TRadioGroup
    Left = 615
    Top = 33
    Width = 161
    Height = 33
    Caption = 'Radio 1'
    Items.Strings = (
      'Line 1')
    TabOrder = 0
    OnClick = rgOneClick
  end
  object rgTwo: TRadioGroup
    Left = 615
    Top = 72
    Width = 161
    Height = 33
    Caption = 'Radio 2'
    Items.Strings = (
      'Line 1')
    TabOrder = 1
    OnClick = rgTwoClick
  end
  object edtTextBox: TEdit
    Left = 672
    Top = 135
    Width = 52
    Height = 21
    TabOrder = 2
    Text = 'EdtBox'
  end
  object btnRedraw: TBitBtn
    Left = 326
    Top = 458
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Redraw'
    ModalResult = 1
    TabOrder = 13
    OnClick = btnRedrawClick
    NumGlyphs = 2
  end
  object btnLoadExtra: TBitBtn
    Left = 615
    Top = 360
    Width = 163
    Height = 25
    Caption = 'Load extra sequence'
    TabOrder = 9
    OnClick = btnLoadExtraClick
  end
  object ChkSetConstant: TCheckBox
    Left = 32
    Top = 8
    Width = 137
    Height = 25
    Caption = 'Set a constant column'
    TabOrder = 16
    OnClick = ChkSetConstantClick
  end
  object chkIncrement: TCheckBox
    Left = 192
    Top = 8
    Width = 177
    Height = 25
    Caption = 'Create an incrementing column'
    TabOrder = 17
    OnClick = ChkSetConstantClick
  end
  object dlgLoadSequence: TOpenDialog
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 510
    Top = 458
  end
  object dlgSaveSequence: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofShowHelp, ofEnableSizing]
    Left = 542
    Top = 458
  end
end
