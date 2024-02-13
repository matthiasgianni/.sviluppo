object FrameDebug: TFrameDebug
  Left = 0
  Top = 0
  Width = 410
  Height = 312
  Color = clBtnFace
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  object TimerUpdate: TTimer
    Enabled = False
    Interval = 500
    OnTimer = TimerUpdateTimer
    Left = 30
    Top = 16
  end
end
