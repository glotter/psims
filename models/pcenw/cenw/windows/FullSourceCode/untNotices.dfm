object frmNotices: TfrmNotices
  Left = 64
  Top = 215
  Width = 759
  Height = 457
  Caption = 'Notices'
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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object memoNotices: TMemo
    Left = 0
    Top = 0
    Width = 751
    Height = 430
    Align = alClient
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
end
