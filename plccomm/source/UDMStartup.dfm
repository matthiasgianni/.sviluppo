object DMStartup: TDMStartup
  OnCreate = DataModuleCreate
  Height = 315
  Width = 310
  object ComPortPolling: TTimer
    OnTimer = ComPortPollingTimer
    Left = 38
    Top = 16
  end
end
