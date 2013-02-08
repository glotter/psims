object frmIrrigationManagement: TfrmIrrigationManagement
  Left = 319
  Top = 96
  BorderStyle = bsDialog
  Caption = 'Irrigation Management'
  ClientHeight = 273
  ClientWidth = 511
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
  object lblLine1: TLabel
    Left = 344
    Top = 139
    Width = 17
    Height = 13
    AutoSize = False
    Caption = 'DD'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 392
    Top = 139
    Width = 33
    Height = 13
    AutoSize = False
    Caption = 'YYYY'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 368
    Top = 139
    Width = 17
    Height = 13
    AutoSize = False
    Caption = 'MM'
    WordWrap = True
  end
  object lblIrrigationFraction: TLabel
    Left = 264
    Top = 73
    Width = 97
    Height = 13
    AutoSize = False
    Caption = '% of field capacity'
    WordWrap = True
  end
  object lblIrrigationAmount: TLabel
    Left = 264
    Top = 97
    Width = 57
    Height = 13
    AutoSize = False
    Caption = 'mm'
    WordWrap = True
  end
  object Label8: TLabel
    Left = 272
    Top = 155
    Width = 65
    Height = 13
    AutoSize = False
    Caption = 'Starting date:'
    WordWrap = True
  end
  object Label9: TLabel
    Left = 360
    Top = 155
    Width = 9
    Height = 13
    AutoSize = False
    Caption = '/'
    WordWrap = True
  end
  object Label10: TLabel
    Left = 388
    Top = 155
    Width = 9
    Height = 13
    AutoSize = False
    Caption = '/'
    WordWrap = True
  end
  object Label11: TLabel
    Left = 272
    Top = 187
    Width = 65
    Height = 13
    AutoSize = False
    Caption = 'End date:'
    WordWrap = True
  end
  object Label12: TLabel
    Left = 360
    Top = 187
    Width = 9
    Height = 13
    AutoSize = False
    Caption = '/'
    WordWrap = True
  end
  object Label13: TLabel
    Left = 388
    Top = 187
    Width = 9
    Height = 13
    AutoSize = False
    Caption = '/'
    WordWrap = True
  end
  object btnOK: TBitBtn
    Left = 264
    Top = 232
    Width = 75
    Height = 25
    TabOrder = 11
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 344
    Top = 232
    Width = 75
    Height = 25
    TabOrder = 12
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 424
    Top = 232
    Width = 75
    Height = 25
    HelpContext = 8200
    TabOrder = 13
    Kind = bkHelp
  end
  object chkIrrigation: TCheckBox
    Left = 168
    Top = 24
    Width = 97
    Height = 17
    Caption = 'Apply irrigation'
    TabOrder = 0
  end
  object rgIrrigationType: TRadioGroup
    Left = 32
    Top = 56
    Width = 169
    Height = 65
    Caption = 'Irrigation type'
    ItemIndex = 0
    Items.Strings = (
      'Irrigate to a set percentage'
      'Irrigate constant daily amount:')
    TabOrder = 1
    OnClick = rgIrrigationTypeClick
  end
  object grpIrrigationInterval: TGroupBox
    Left = 32
    Top = 136
    Width = 193
    Height = 121
    Caption = 'Interval between irrigation applications'
    TabOrder = 4
    object Label5: TLabel
      Left = 64
      Top = 27
      Width = 33
      Height = 13
      AutoSize = False
      Caption = 'Days'
      WordWrap = True
    end
    object Label6: TLabel
      Left = 64
      Top = 59
      Width = 41
      Height = 13
      AutoSize = False
      Caption = 'Months'
      WordWrap = True
    end
    object Label7: TLabel
      Left = 64
      Top = 91
      Width = 41
      Height = 13
      AutoSize = False
      Caption = 'Years'
      WordWrap = True
    end
    object edtDays: TEdit
      Left = 111
      Top = 23
      Width = 34
      Height = 21
      TabOrder = 0
      Text = 'Days'
    end
    object edtMonths: TEdit
      Left = 111
      Top = 55
      Width = 34
      Height = 21
      TabOrder = 1
      Text = 'Months'
    end
    object edtYears: TEdit
      Left = 111
      Top = 87
      Width = 34
      Height = 21
      TabOrder = 2
      Text = 'Years'
    end
  end
  object edtIrrigationFraction: TEdit
    Left = 207
    Top = 69
    Width = 52
    Height = 21
    TabOrder = 2
    Text = 'Irrigation fraction'
  end
  object edtIrrigationAmount: TEdit
    Left = 207
    Top = 95
    Width = 52
    Height = 21
    TabOrder = 3
    Text = 'Irrigation amount'
  end
  object edtStartDay: TEdit
    Left = 341
    Top = 151
    Width = 20
    Height = 21
    TabOrder = 5
    Text = 'Day'
  end
  object edtStartMonth: TEdit
    Left = 367
    Top = 151
    Width = 20
    Height = 21
    TabOrder = 6
    Text = 'Month'
  end
  object edtStartYear: TEdit
    Left = 394
    Top = 151
    Width = 34
    Height = 21
    TabOrder = 7
    Text = 'Year'
  end
  object edtEndDay: TEdit
    Left = 341
    Top = 183
    Width = 20
    Height = 21
    TabOrder = 8
    Text = 'Day'
  end
  object edtEndMonth: TEdit
    Left = 367
    Top = 183
    Width = 20
    Height = 21
    TabOrder = 9
    Text = 'Month'
  end
  object edtEndYear: TEdit
    Left = 394
    Top = 183
    Width = 34
    Height = 21
    TabOrder = 10
    Text = 'Year'
  end
end
