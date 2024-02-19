object FrameDebug: TFrameDebug
  Left = 0
  Top = 0
  Width = 410
  Height = 312
  Color = clBtnFace
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 410
    Height = 312
    ActivePage = tsRX
    Align = alClient
    TabOrder = 0
    object tsRX: TTabSheet
      Caption = 'PLC'
    end
    object tsTX: TTabSheet
      Caption = 'Scrittura'
      ImageIndex = 1
    end
  end
  object TimerUpdate: TTimer
    Enabled = False
    Interval = 500
    OnTimer = TimerUpdateTimer
    Left = 38
    Top = 56
  end
end
