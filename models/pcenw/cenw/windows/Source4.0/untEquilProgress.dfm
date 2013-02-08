object frmEquilProgress: TfrmEquilProgress
  Left = 640
  Top = 300
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Progress...'
  ClientHeight = 374
  ClientWidth = 374
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDesigned
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object lblLine1: TLabel
    Left = 8
    Top = 11
    Width = 121
    Height = 13
    AutoSize = False
    Caption = 'Search target value'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 8
    Top = 35
    Width = 153
    Height = 13
    AutoSize = False
    Caption = 'Search variable'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 59
    Width = 153
    Height = 13
    AutoSize = False
    Caption = 'Convergence 1 - target value'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 8
    Top = 83
    Width = 153
    Height = 13
    AutoSize = False
    Caption = 'Convergence 2 - slow SOM'
    WordWrap = True
  end
  object Label4: TLabel
    Left = 8
    Top = 107
    Width = 153
    Height = 13
    AutoSize = False
    Caption = 'Convergence 3 - resistant SOM'
    WordWrap = True
  end
  object Label5: TLabel
    Left = 8
    Top = 131
    Width = 121
    Height = 13
    AutoSize = False
    Caption = 'Iterations'
    WordWrap = True
  end
  object Label6: TLabel
    Left = 8
    Top = 155
    Width = 121
    Height = 13
    AutoSize = False
    Caption = 'Near target count'
    WordWrap = True
  end
  object Label7: TLabel
    Left = 8
    Top = 179
    Width = 129
    Height = 30
    AutoSize = False
    Caption = 'Boost param. for changes to resistant organic matter'
    WordWrap = True
  end
  object lblPTargetInfo: TLabel
    Left = 8
    Top = 238
    Width = 121
    Height = 13
    AutoSize = False
    Caption = 'Search P target'
    WordWrap = True
  end
  object Label8: TLabel
    Left = 8
    Top = 215
    Width = 121
    Height = 13
    AutoSize = False
    Caption = 'Delta'
    WordWrap = True
  end
  object lblP: TLabel
    Left = 250
    Top = 184
    Width = 17
    Height = 16
    AutoSize = False
    Caption = 'P'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    ShowAccelChar = False
    WordWrap = True
  end
  object lblPRestrict: TLabel
    Left = 295
    Top = 179
    Width = 54
    Height = 33
    Alignment = taCenter
    AutoSize = False
    Caption = 'P change boost'
    WordWrap = True
  end
  object lblOscillations: TLabel
    Left = 87
    Top = 272
    Width = 180
    Height = 25
    AutoSize = False
    Caption = 'Oscillations!!'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -24
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    ShowAccelChar = False
    WordWrap = True
  end
  object btnCancel: TBitBtn
    Left = 15
    Top = 320
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
    OnClick = btnCancelClick
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      333333333333333333333333000033338833333333333333333F333333333333
      0000333911833333983333333388F333333F3333000033391118333911833333
      38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
      911118111118333338F3338F833338F3000033333911111111833333338F3338
      3333F8330000333333911111183333333338F333333F83330000333333311111
      8333333333338F3333383333000033333339111183333333333338F333833333
      00003333339111118333333333333833338F3333000033333911181118333333
      33338333338F333300003333911183911183333333383338F338F33300003333
      9118333911183333338F33838F338F33000033333913333391113333338FF833
      38F338F300003333333333333919333333388333338FFF830000333333333333
      3333333333333333333888330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object btnManualUpdate: TButton
    Left = 127
    Top = 320
    Width = 105
    Height = 25
    Caption = 'Manual update'
    TabOrder = 1
    OnClick = btnManualUpdateClick
  end
  object edtSearchValue: TEdit
    Left = 167
    Top = 7
    Width = 52
    Height = 21
    TabOrder = 2
    Text = 'Search value'
  end
  object edtTargetValue: TEdit
    Left = 231
    Top = 7
    Width = 52
    Height = 21
    TabOrder = 3
    Text = 'Search target'
  end
  object edtSearchVariable: TEdit
    Left = 167
    Top = 31
    Width = 52
    Height = 21
    TabOrder = 4
    Text = 'Search Variable'
  end
  object edtConverg1: TEdit
    Left = 167
    Top = 55
    Width = 52
    Height = 21
    TabOrder = 5
    Text = 'Convergence target'
  end
  object edtLimitConverg1: TEdit
    Left = 231
    Top = 55
    Width = 52
    Height = 21
    TabOrder = 6
    Text = 'Convergence limit 1'
  end
  object edtConverg2: TEdit
    Left = 167
    Top = 79
    Width = 52
    Height = 21
    TabOrder = 7
    Text = 'Convergence slow OM'
  end
  object edtLimitConverg2: TEdit
    Left = 231
    Top = 79
    Width = 52
    Height = 21
    TabOrder = 8
    Text = 'Convergence limit 2'
  end
  object edtConverg3: TEdit
    Left = 167
    Top = 103
    Width = 52
    Height = 21
    TabOrder = 9
    Text = 'Convergence resistant OM'
  end
  object edtLimitConverg3: TEdit
    Left = 231
    Top = 103
    Width = 52
    Height = 21
    TabOrder = 10
    Text = 'Convergence limit 3'
  end
  object edtIterations: TEdit
    Left = 167
    Top = 127
    Width = 52
    Height = 21
    TabOrder = 11
    Text = 'Iterations'
  end
  object edtMaxIterations: TEdit
    Left = 231
    Top = 127
    Width = 52
    Height = 21
    TabOrder = 12
    Text = 'Max iterations'
  end
  object edtGoodCount: TEdit
    Left = 167
    Top = 151
    Width = 52
    Height = 21
    TabOrder = 13
    Text = 'Good count'
  end
  object edtLimitGoodCount: TEdit
    Left = 231
    Top = 151
    Width = 52
    Height = 21
    TabOrder = 14
    Text = 'Good count limit'
  end
  object edtBoostResistant: TEdit
    Left = 167
    Top = 180
    Width = 52
    Height = 21
    TabOrder = 15
    Text = 'Boost resistant'
  end
  object edtPTargetValue: TEdit
    Left = 167
    Top = 234
    Width = 52
    Height = 21
    TabOrder = 16
    Text = 'Search P value'
  end
  object edtPlimitTarget: TEdit
    Left = 231
    Top = 234
    Width = 52
    Height = 21
    TabOrder = 17
    Text = 'Search target'
  end
  object edtDelta: TEdit
    Left = 167
    Top = 207
    Width = 52
    Height = 21
    TabOrder = 18
    Text = 'Delta'
  end
  object edtPDelta: TEdit
    Left = 231
    Top = 207
    Width = 52
    Height = 21
    TabOrder = 19
    Text = 'Delta'
  end
  object edtPRestrict: TEdit
    Left = 295
    Top = 207
    Width = 52
    Height = 21
    TabOrder = 20
    Text = 'Delta'
  end
  object chkHoldPInput: TCheckBox
    Left = 252
    Top = 320
    Width = 125
    Height = 25
    Caption = 'Keep current P input'
    TabOrder = 21
    OnClick = chkHoldPInputClick
  end
end
