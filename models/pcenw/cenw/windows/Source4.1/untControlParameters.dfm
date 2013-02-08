object frmControlParameters: TfrmControlParameters
  Left = 249
  Top = 166
  BorderStyle = bsDialog
  Caption = 'Control Parameters'
  ClientHeight = 618
  ClientWidth = 774
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
  object Label3: TLabel
    Left = 154
    Top = 242
    Width = 55
    Height = 26
    AutoSize = False
    Caption = 'Date seperator'
    WordWrap = True
  end
  object lblStartingDay: TLabel
    Left = 558
    Top = 276
    Width = 71
    Height = 13
    AutoSize = False
    Caption = 'Starting day'
  end
  object lblStartingMonth: TLabel
    Left = 558
    Top = 300
    Width = 71
    Height = 13
    AutoSize = False
    Caption = 'Starting month'
  end
  object lblStartingYear: TLabel
    Left = 558
    Top = 326
    Width = 71
    Height = 13
    AutoSize = False
    Caption = 'Starting year'
  end
  object btnOK: TBitBtn
    Left = 424
    Top = 417
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    DoubleBuffered = True
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
    ParentDoubleBuffered = False
    TabOrder = 14
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 424
    Top = 473
    Width = 75
    Height = 25
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 15
  end
  object btnHelp: TBitBtn
    Left = 424
    Top = 529
    Width = 75
    Height = 25
    HelpContext = 3400
    DoubleBuffered = True
    Kind = bkHelp
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 16
  end
  object grpLength: TGroupBox
    Left = 16
    Top = 128
    Width = 337
    Height = 49
    Caption = 'Length of simulation'
    TabOrder = 1
    object lblYears: TLabel
      Left = 8
      Top = 21
      Width = 39
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Years'
    end
    object lblMonths: TLabel
      Left = 120
      Top = 21
      Width = 41
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Months'
    end
    object lblDays: TLabel
      Left = 224
      Top = 21
      Width = 40
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Days'
    end
    object edtYears: TEdit
      Left = 61
      Top = 18
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'Years'
    end
    object edtMonths: TEdit
      Left = 173
      Top = 18
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'Months'
    end
    object edtDays: TEdit
      Left = 269
      Top = 18
      Width = 52
      Height = 21
      TabOrder = 2
      Text = 'Days'
    end
  end
  object grpDisplayNum: TGroupBox
    Left = 16
    Top = 184
    Width = 225
    Height = 49
    Caption = 'Output options'
    TabOrder = 2
    object Label1: TLabel
      Left = 10
      Top = 15
      Width = 55
      Height = 32
      AutoSize = False
      Caption = 'n screen displays'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Label2: TLabel
      Left = 130
      Top = 15
      Width = 39
      Height = 32
      AutoSize = False
      Caption = 'n disk saves'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object edtDisplayNum: TEdit
      Left = 61
      Top = 18
      Width = 52
      Height = 21
      TabOrder = 0
      Text = 'n displays'
    end
    object edtDiskNum: TEdit
      Left = 170
      Top = 18
      Width = 52
      Height = 21
      TabOrder = 1
      Text = 'n disk saves'
    end
  end
  object grpClimate: TGroupBox
    Left = 16
    Top = 291
    Width = 337
    Height = 49
    Caption = 'Climate data file'
    TabOrder = 4
    object edtClimate: TEdit
      Left = 8
      Top = 16
      Width = 257
      Height = 21
      TabOrder = 0
    end
    object btnClimate: TButton
      Left = 272
      Top = 16
      Width = 57
      Height = 21
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnClimateClick
    end
  end
  object grpOutput: TGroupBox
    Left = 16
    Top = 347
    Width = 337
    Height = 49
    Caption = 'Data output file'
    TabOrder = 5
    object edtOutput: TEdit
      Left = 8
      Top = 16
      Width = 257
      Height = 21
      TabOrder = 0
    end
    object btnOutput: TButton
      Left = 272
      Top = 16
      Width = 57
      Height = 21
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnOutputClick
    end
  end
  object grpPlant: TGroupBox
    Left = 16
    Top = 515
    Width = 337
    Height = 49
    Caption = 'Plant file'
    TabOrder = 8
    object edtPlant: TEdit
      Left = 8
      Top = 16
      Width = 257
      Height = 21
      TabOrder = 0
    end
    object btnPlant: TButton
      Left = 272
      Top = 16
      Width = 57
      Height = 21
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnPlantClick
    end
  end
  object grpSite: TGroupBox
    Left = 16
    Top = 403
    Width = 337
    Height = 49
    Caption = 'Site file'
    TabOrder = 6
    object edtSite: TEdit
      Left = 8
      Top = 16
      Width = 257
      Height = 21
      TabOrder = 0
    end
    object btnSite: TButton
      Left = 272
      Top = 16
      Width = 57
      Height = 21
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnSiteClick
    end
  end
  object grpInitial: TGroupBox
    Left = 16
    Top = 459
    Width = 337
    Height = 49
    Caption = 'Initial pool sizes'
    TabOrder = 7
    object edtInitial: TEdit
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
      OnClick = btnInitialClick
    end
  end
  object grpTitle: TGroupBox
    Left = 16
    Top = 72
    Width = 337
    Height = 49
    Caption = 'Project title'
    TabOrder = 0
    object edtTitle: TEdit
      Left = 8
      Top = 16
      Width = 321
      Height = 21
      TabOrder = 0
    end
  end
  object rgCalculation: TRadioGroup
    Left = 384
    Top = 120
    Width = 153
    Height = 65
    Caption = 'Calculation scope'
    Items.Strings = (
      'Decomposition only'
      'Full growth simulation')
    TabOrder = 10
  end
  object rgClimate: TRadioGroup
    Left = 384
    Top = 16
    Width = 153
    Height = 89
    Caption = 'Climate data'
    Items.Strings = (
      'Observed'
      'Constant'
      'Simulated')
    TabOrder = 9
  end
  object grpProjectName: TGroupBox
    Left = 16
    Top = 17
    Width = 337
    Height = 49
    Caption = 'Name of project file (for information only)'
    TabOrder = 17
    object edtProjectFile: TEdit
      Left = 8
      Top = 16
      Width = 321
      Height = 21
      Enabled = False
      TabOrder = 0
    end
  end
  object edtResetPlantPools: TCheckBox
    Left = 392
    Top = 232
    Width = 145
    Height = 25
    Alignment = taLeftJustify
    BiDiMode = bdRightToLeft
    Caption = 'Reset plant pools at start'
    ParentBiDiMode = False
    TabOrder = 12
  end
  object chkIsotopes: TCheckBox
    Left = 392
    Top = 202
    Width = 145
    Height = 25
    Alignment = taLeftJustify
    BiDiMode = bdRightToLeft
    Caption = 'Include C isotopes'
    ParentBiDiMode = False
    TabOrder = 11
  end
  object chkIncludeP: TCheckBox
    Left = 392
    Top = 324
    Width = 145
    Height = 25
    Alignment = taLeftJustify
    BiDiMode = bdRightToLeft
    Caption = 'Include phosphorus cycle'
    ParentBiDiMode = False
    TabOrder = 13
    Visible = False
  end
  object chkFlags: TCheckBox
    Left = 16
    Top = 239
    Width = 113
    Height = 36
    Alignment = taLeftJustify
    BiDiMode = bdRightToLeft
    Caption = 'Flags for back ground events'
    ParentBiDiMode = False
    TabOrder = 3
    WordWrap = True
  end
  object chkIncludeWeeds: TCheckBox
    Left = 392
    Top = 262
    Width = 145
    Height = 25
    Alignment = taLeftJustify
    BiDiMode = bdRightToLeft
    Caption = 'Include weed competition'
    ParentBiDiMode = False
    TabOrder = 18
  end
  object rgDateSystem: TRadioGroup
    Left = 256
    Top = 183
    Width = 97
    Height = 102
    Caption = 'Date system'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -7
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Font.Quality = fqNonAntialiased
    Items.Strings = (
      'DDMMYYY'
      'YYYYMMDD'
      'MMDDYYYY'
      'YYYYDDMM')
    ParentFont = False
    TabOrder = 19
  end
  object edtDateSeperator: TEdit
    Left = 215
    Top = 246
    Width = 23
    Height = 21
    TabOrder = 20
    Text = '/'
  end
  object chkStartingDate: TCheckBox
    Left = 392
    Top = 293
    Width = 145
    Height = 25
    Alignment = taLeftJustify
    BiDiMode = bdRightToLeft
    Caption = 'Start run on set date'
    ParentBiDiMode = False
    TabOrder = 21
    OnClick = chkStartingDateClick
  end
  object edtStartingDay: TEdit
    Left = 635
    Top = 272
    Width = 52
    Height = 21
    TabOrder = 22
    Text = 'Day to start'
  end
  object edtStartingMonth: TEdit
    Left = 635
    Top = 297
    Width = 52
    Height = 21
    TabOrder = 23
    Text = 'Month to start'
  end
  object edtStartingYear: TEdit
    Left = 635
    Top = 322
    Width = 52
    Height = 21
    TabOrder = 24
    Text = 'Year to start'
  end
  object dlgOpenSite: TOpenDialog
    HelpContext = 1220
    DefaultExt = 'st!'
    Filter = 'CenW site files (*.st!)|*.st!'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a site file to use...'
    Left = 192
    Top = 419
  end
  object dlgOpenInitial: TOpenDialog
    HelpContext = 1400
    DefaultExt = 'il!'
    Filter = 'CenW initial values files (*.il!)|*.il!'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select an intial values file to use...'
    Left = 192
    Top = 475
  end
  object dlgOpenOutput: TOpenDialog
    HelpContext = 1210
    DefaultExt = 'dt!'
    Filter = 'Data output files (*.dt!)|*.dt!'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofEnableSizing]
    Title = 'Select a file to save the data output to...'
    Left = 192
    Top = 363
  end
  object dlgOpenClimate: TOpenDialog
    HelpContext = 1210
    DefaultExt = 'cl!'
    Filter = 'CenW climate files (*.cl!)|*.cl!'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a climate file to use...'
    Left = 192
    Top = 307
  end
  object dlgOpenPlant: TOpenDialog
    HelpContext = 1210
    DefaultExt = 'pl!'
    Filter = 'CenW plant files (*.pl!)|*.pl!'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a plant file to use...'
    Left = 192
    Top = 531
  end
end
