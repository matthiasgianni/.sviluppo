object FormDebug: TFormDebug
  Left = 0
  Top = 0
  Caption = 'Debug'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object PanelStatus: TPanel
    Left = 0
    Top = 416
    Width = 624
    Height = 25
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    ExplicitTop = 415
    ExplicitWidth = 620
  end
end
