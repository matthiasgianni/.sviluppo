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
  object pnlStatus: TPanel
    Left = 0
    Top = 416
    Width = 624
    Height = 25
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
  end
  object TimerDebug: TTimer
    Interval = 500
    OnTimer = TimerDebugTimer
    Left = 568
    Top = 16
  end
end
