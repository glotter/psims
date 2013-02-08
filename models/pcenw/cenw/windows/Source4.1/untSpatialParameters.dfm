object frmSpatialParameters: TfrmSpatialParameters
  Left = 260
  Top = 194
  BorderStyle = bsDialog
  Caption = 'Spatial Parameters'
  ClientHeight = 446
  ClientWidth = 528
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
  object Label7: TLabel
    Left = 328
    Top = 160
    Width = 73
    Height = 14
    AutoSize = False
    Caption = 'Temp max'
    WordWrap = True
  end
  object Label8: TLabel
    Left = 328
    Top = 136
    Width = 73
    Height = 13
    AutoSize = False
    Caption = 'Temp min'
    WordWrap = True
  end
  object Label9: TLabel
    Left = 328
    Top = 112
    Width = 81
    Height = 13
    AutoSize = False
    Caption = 'Rain max'
    WordWrap = True
  end
  object Label10: TLabel
    Left = 328
    Top = 89
    Width = 73
    Height = 13
    AutoSize = False
    Caption = 'Rain min'
    WordWrap = True
  end
  object Label11: TLabel
    Left = 328
    Top = 51
    Width = 81
    Height = 13
    AutoSize = False
    Caption = 'n calculations'
    WordWrap = True
  end
  object Label12: TLabel
    Left = 328
    Top = 27
    Width = 105
    Height = 13
    AutoSize = False
    Caption = 'Initial calculations'
    WordWrap = True
  end
  object grpCoordinates: TGroupBox
    Left = 32
    Top = 8
    Width = 273
    Height = 188
    Caption = 'Coordinates'
    TabOrder = 0
    object lblReleaseFertiliser: TLabel
      Left = 8
      Top = 129
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'Maximum latitude'
      WordWrap = True
    end
    object lblLeaching: TLabel
      Left = 8
      Top = 103
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'Minimum latitude'
      WordWrap = True
    end
    object Label15: TLabel
      Left = 8
      Top = 51
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'Maximum longitude'
      WordWrap = True
    end
    object Label16: TLabel
      Left = 8
      Top = 27
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'Minimum longitude'
      WordWrap = True
    end
    object Label4: TLabel
      Left = 8
      Top = 78
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'Longitude interval'
      WordWrap = True
    end
    object Label3: TLabel
      Left = 10
      Top = 153
      Width = 143
      Height = 13
      AutoSize = False
      Caption = 'Latitude interval'
      WordWrap = True
    end
    object edtLatMin: TEdit
      Left = 207
      Top = 126
      Width = 52
      Height = 21
      TabOrder = 3
      Text = 'Min lat'
    end
    object edtLatMax: TEdit
      Left = 207
      Top = 99
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'Max lat'
    end
    object edtLongMin: TEdit
      Left = 207
      Top = 48
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'Min long'
    end
    object edtLongMax: TEdit
      Left = 207
      Top = 21
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Max long'
    end
    object edtLongInterval: TEdit
      Left = 207
      Top = 72
      Width = 52
      Height = 21
      TabOrder = 4
      Text = 'Long interval'
    end
    object edtLatInterval: TEdit
      Left = 207
      Top = 153
      Width = 52
      Height = 21
      TabOrder = 5
      Text = 'Lat interval'
    end
  end
  object btnOK: TBitBtn
    Left = 119
    Top = 360
    Width = 75
    Height = 25
    TabOrder = 7
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 239
    Top = 360
    Width = 75
    Height = 25
    TabOrder = 8
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 359
    Top = 360
    Width = 75
    Height = 25
    HelpContext = 2600
    TabOrder = 9
    Kind = bkHelp
  end
  object edtTempMax: TEdit
    Left = 439
    Top = 157
    Width = 52
    Height = 21
    TabOrder = 6
    Text = 'Temp max'
  end
  object edtTempMin: TEdit
    Left = 439
    Top = 133
    Width = 52
    Height = 21
    TabOrder = 5
    Text = 'Temp min'
  end
  object edtRainMax: TEdit
    Left = 439
    Top = 109
    Width = 52
    Height = 21
    TabOrder = 4
    Text = 'Rain max'
  end
  object edtRainMin: TEdit
    Left = 439
    Top = 85
    Width = 52
    Height = 21
    TabOrder = 3
    Text = 'Rain min'
  end
  object edtCalcs: TEdit
    Left = 439
    Top = 48
    Width = 52
    Height = 21
    TabOrder = 2
    Text = 'Calcs'
  end
  object edtInitial: TEdit
    Left = 439
    Top = 24
    Width = 52
    Height = 21
    TabOrder = 1
    Text = 'Initial'
  end
  object rgPlantType: TRadioGroup
    Left = 32
    Top = 208
    Width = 113
    Height = 57
    ItemIndex = 0
    Items.Strings = (
      'Optimal plant'
      'Exotic plant')
    TabOrder = 10
  end
  object rgSoilType: TRadioGroup
    Left = 160
    Top = 208
    Width = 145
    Height = 57
    ItemIndex = 0
    Items.Strings = (
      'Find equilibrium soil'
      'Use given soil values')
    TabOrder = 11
  end
  object grpSpatialFile: TGroupBox
    Left = 32
    Top = 280
    Width = 337
    Height = 49
    Caption = 'Spatial data file'
    TabOrder = 12
    object edtSpatialFile: TEdit
      Left = 8
      Top = 16
      Width = 257
      Height = 21
      TabOrder = 0
    end
    object btnSpatialFile: TButton
      Left = 271
      Top = 16
      Width = 57
      Height = 21
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnSpatialFileClick
    end
  end
  object chkNumeric: TCheckBox
    Left = 359
    Top = 232
    Width = 132
    Height = 17
    Alignment = taLeftJustify
    BiDiMode = bdRightToLeft
    Caption = 'Include numeric output'
    ParentBiDiMode = False
    TabOrder = 13
  end
  object dlgOpenSpatialFile: TOpenDialog
    HelpContext = 1210
    DefaultExt = 'sp!'
    Filter = 'Spatial files (*.sp!)|*.sp!'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a spatial data file to use...'
    Left = 200
    Top = 291
  end
end
