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
  object PageAutomationControls: TPageControl
    Left = 0
    Top = 0
    Width = 289
    Height = 416
    ActivePage = ts1
    Align = alLeft
    TabOrder = 1
    object ts1: TTabSheet
      Caption = 'ts1'
      object PanelAutomationControls: TPanel
        Left = 0
        Top = 0
        Width = 281
        Height = 386
        Align = alClient
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 0
        ExplicitLeft = -16
        ExplicitWidth = 297
      end
    end
  end
  object TimerDebug: TTimer
    Interval = 500
    OnTimer = TimerDebugTimer
    Left = 568
    Top = 16
  end
end
