object DMPLC: TDMPLC
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 199
  Width = 227
  object Timer: TTimer
    Interval = 500
    OnTimer = TimerTimer
    Left = 24
    Top = 16
  end
end
