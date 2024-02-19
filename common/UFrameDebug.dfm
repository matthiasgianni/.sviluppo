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
    ParentCustomHint = False
    ActivePage = tsRX
    Align = alClient
    BiDiMode = bdLeftToRight
    DoubleBuffered = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    HotTrack = True
    ParentBiDiMode = False
    ParentDoubleBuffered = False
    ParentFont = False
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
    TabStop = False
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
