object frmMultipleRunParameters: TfrmMultipleRunParameters
  Left = 272
  Top = 187
  BorderStyle = bsDialog
  Caption = 'MultipleRun Parameters'
  ClientHeight = 394
  ClientWidth = 744
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
  object btnRun: TBitBtn
    Left = 376
    Top = 347
    Width = 73
    Height = 25
    Caption = 'Run'
    Default = True
    ModalResult = 1
    TabOrder = 11
    OnClick = btnRunClick
    NumGlyphs = 2
  end
  object btnCancel: TBitBtn
    Left = 568
    Top = 347
    Width = 75
    Height = 25
    TabOrder = 13
    OnClick = btnCancelClick
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 656
    Top = 347
    Width = 67
    Height = 25
    HelpContext = 2300
    TabOrder = 14
    Kind = bkHelp
  end
  object grpPools: TGroupBox
    Left = 8
    Top = 328
    Width = 337
    Height = 49
    Caption = 'File for transferring pools between projects'
    TabOrder = 10
    object edtPools: TEdit
      Left = 8
      Top = 16
      Width = 257
      Height = 21
      TabOrder = 0
    end
    object btnInitial: TButton
      Left = 272
      Top = 16
      Width = 57
      Height = 21
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnPoolsClick
    end
  end
  object grpFile1: TGroupBox
    Left = 8
    Top = 8
    Width = 337
    Height = 49
    Caption = 'First project file'
    TabOrder = 0
    object edtProjectFile1: TEdit
      Left = 8
      Top = 16
      Width = 257
      Height = 21
      TabOrder = 0
    end
    object btnProjectFile1: TButton
      Left = 272
      Top = 16
      Width = 57
      Height = 21
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnProjectFile1Click
    end
  end
  object grpFile2: TGroupBox
    Left = 8
    Top = 64
    Width = 337
    Height = 49
    Caption = 'Second project file'
    TabOrder = 1
    object edtProjectFile2: TEdit
      Left = 8
      Top = 16
      Width = 257
      Height = 21
      TabOrder = 0
    end
    object btnProjectFile2: TButton
      Left = 272
      Top = 16
      Width = 57
      Height = 21
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnProjectFile2Click
    end
  end
  object grpFile3: TGroupBox
    Left = 8
    Top = 120
    Width = 337
    Height = 49
    Caption = 'Third project file'
    TabOrder = 2
    object edtProjectFile3: TEdit
      Left = 8
      Top = 16
      Width = 257
      Height = 21
      TabOrder = 0
    end
    object btnProjectFile3: TButton
      Left = 272
      Top = 16
      Width = 57
      Height = 21
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnProjectFile3Click
    end
  end
  object grpFile4: TGroupBox
    Left = 8
    Top = 176
    Width = 337
    Height = 49
    Caption = 'Fourth project file'
    TabOrder = 3
    object edtProjectFile4: TEdit
      Left = 8
      Top = 16
      Width = 257
      Height = 21
      TabOrder = 0
    end
    object btnProjectFile4: TButton
      Left = 272
      Top = 16
      Width = 57
      Height = 21
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnProjectFile4Click
    end
  end
  object grpFile5: TGroupBox
    Left = 8
    Top = 232
    Width = 337
    Height = 49
    Caption = 'Fifth project file'
    TabOrder = 4
    object edtProjectFile5: TEdit
      Left = 8
      Top = 16
      Width = 257
      Height = 21
      TabOrder = 0
    end
    object btnProjectFile5: TButton
      Left = 272
      Top = 16
      Width = 57
      Height = 21
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnProjectFile5Click
    end
  end
  object grpFile6: TGroupBox
    Left = 376
    Top = 8
    Width = 337
    Height = 49
    Caption = 'Sixth project file'
    TabOrder = 5
    object edtProjectFile6: TEdit
      Left = 8
      Top = 16
      Width = 257
      Height = 21
      TabOrder = 0
    end
    object btnProjectFile6: TButton
      Left = 272
      Top = 16
      Width = 57
      Height = 21
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnProjectFile6Click
    end
  end
  object grpFile7: TGroupBox
    Left = 376
    Top = 64
    Width = 337
    Height = 49
    Caption = 'Seventh project file'
    TabOrder = 6
    object edtProjectFile7: TEdit
      Left = 8
      Top = 16
      Width = 257
      Height = 21
      TabOrder = 0
    end
    object btnProjectFile7: TButton
      Left = 272
      Top = 16
      Width = 57
      Height = 21
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnProjectFile7Click
    end
  end
  object grpFile8: TGroupBox
    Left = 376
    Top = 120
    Width = 337
    Height = 49
    Caption = 'Eight project file'
    TabOrder = 7
    object edtProjectFile8: TEdit
      Left = 8
      Top = 16
      Width = 257
      Height = 21
      TabOrder = 0
    end
    object btnProjectFile8: TButton
      Left = 272
      Top = 16
      Width = 57
      Height = 21
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnProjectFile8Click
    end
  end
  object grpFile9: TGroupBox
    Left = 376
    Top = 176
    Width = 337
    Height = 49
    Caption = 'Ninth project file'
    TabOrder = 8
    object edtProjectFile9: TEdit
      Left = 8
      Top = 16
      Width = 257
      Height = 21
      TabOrder = 0
    end
    object btnProjectFile9: TButton
      Left = 272
      Top = 16
      Width = 57
      Height = 21
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnProjectFile9Click
    end
  end
  object grpFile10: TGroupBox
    Left = 376
    Top = 232
    Width = 337
    Height = 49
    Caption = 'Tenth project file'
    TabOrder = 9
    object edtProjectFile10: TEdit
      Left = 8
      Top = 16
      Width = 257
      Height = 21
      TabOrder = 0
    end
    object btnProjectFile10: TButton
      Left = 272
      Top = 16
      Width = 57
      Height = 21
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnProjectFile10Click
    end
  end
  object btnSave_Run: TBitBtn
    Left = 464
    Top = 347
    Width = 89
    Height = 25
    Caption = 'Save and run'
    ModalResult = 1
    TabOrder = 12
    OnClick = btnSaveRunClick
    NumGlyphs = 2
  end
  object dlgOpenPools: TOpenDialog
    HelpContext = 1400
    DefaultExt = 'il!'
    Filter = 'CenW initial values files (*.il!)|*.il!'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a pool file to transfer between projects...'
    Left = 168
    Top = 344
  end
  object dlgOpenProjectFile1: TOpenDialog
    HelpContext = 1210
    DefaultExt = 'pj!'
    Filter = 'CenW project files (*.pj!)|*.pj!'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a project file to use...'
    Left = 176
    Top = 16
  end
  object dlgOpenProjectFile2: TOpenDialog
    HelpContext = 1210
    DefaultExt = 'pj!'
    Filter = 'CenW project files (*.pj!)|*.pj!'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a project file to use...'
    Left = 176
    Top = 72
  end
  object dlgOpenProjectFile3: TOpenDialog
    HelpContext = 1210
    DefaultExt = 'pj!'
    Filter = 'CenW project files (*.pj!)|*.pj!'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a project file to use...'
    Left = 176
    Top = 128
  end
  object dlgOpenProjectFile4: TOpenDialog
    HelpContext = 1210
    DefaultExt = 'pj!'
    Filter = 'CenW project files (*.pj!)|*.pj!'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a project file to use...'
    Left = 176
    Top = 184
  end
  object dlgOpenProjectFile5: TOpenDialog
    HelpContext = 1210
    DefaultExt = 'pj!'
    Filter = 'CenW project files (*.pj!)|*.pj!'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a project file to use...'
    Left = 176
    Top = 240
  end
  object dlgOpenProjectFile6: TOpenDialog
    HelpContext = 1210
    DefaultExt = 'pj!'
    Filter = 'CenW project files (*.pj!)|*.pj!'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a project file to use...'
    Left = 536
    Top = 16
  end
  object dlgOpenProjectFile7: TOpenDialog
    HelpContext = 1210
    DefaultExt = 'pj!'
    Filter = 'CenW project files (*.pj!)|*.pj!'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a project file to use...'
    Left = 536
    Top = 72
  end
  object dlgOpenProjectFile8: TOpenDialog
    HelpContext = 1210
    DefaultExt = 'pj!'
    Filter = 'CenW project files (*.pj!)|*.pj!'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a project file to use...'
    Left = 536
    Top = 128
  end
  object dlgOpenProjectFile9: TOpenDialog
    HelpContext = 1210
    DefaultExt = 'pj!'
    Filter = 'CenW project files (*.pj!)|*.pj!'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a project file to use...'
    Left = 536
    Top = 184
  end
  object dlgOpenProjectFile10: TOpenDialog
    HelpContext = 1210
    DefaultExt = 'pj!'
    Filter = 'CenW project files (*.pj!)|*.pj!'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a project file to use...'
    Left = 536
    Top = 240
  end
end
