object frmGraph: TfrmGraph
  Left = 159
  Top = 109
  Caption = 'Graph'
  ClientHeight = 613
  ClientWidth = 862
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object chtGraph: TChart
    Left = 0
    Top = 0
    Width = 862
    Height = 613
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Legend.ColorWidth = 50
    Legend.DividingLines.Width = 3
    Legend.Font.Height = -13
    Legend.Symbol.Width = 50
    MarginTop = 9
    Title.Text.Strings = (
      'CenW')
    Title.Visible = False
    BottomAxis.Automatic = False
    BottomAxis.AutomaticMaximum = False
    LeftAxis.Automatic = False
    LeftAxis.AutomaticMaximum = False
    LeftAxis.AutomaticMinimum = False
    LeftAxis.ExactDateTime = False
    LeftAxis.Increment = 0.100000000000000000
    LeftAxis.Maximum = 1.000000000000000000
    RightAxis.Automatic = False
    RightAxis.AutomaticMaximum = False
    RightAxis.AutomaticMinimum = False
    RightAxis.Maximum = 200.000000000000000000
    RightAxis.Minimum = 3.000000000000000000
    RightAxis.Title.Caption = 'hello there'
    View3D = False
    Align = alClient
    Color = clWhite
    TabOrder = 0
    object lblSeries: TLabel
      Left = 32
      Top = 8
      Width = 73
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Selected series'
    end
    object cmbSeries: TComboBox
      Left = 112
      Top = 5
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbSeriesChange
    end
    object btnPrint: TButton
      Left = 328
      Top = 8
      Width = 41
      Height = 17
      Caption = 'Print'
      TabOrder = 1
      OnClick = btnPrintClick
    end
    object btnCopy: TButton
      Left = 264
      Top = 8
      Width = 41
      Height = 17
      Caption = 'Copy'
      TabOrder = 2
      OnClick = btnCopyClick
    end
  end
end
