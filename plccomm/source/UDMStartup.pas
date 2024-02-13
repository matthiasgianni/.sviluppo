unit UDMStartup;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Utils, UComPort, Vcl.ExtCtrls;

type
  TDMStartup = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
    Config: TConfigSettings;
    ComThread: TComThread;

    FComPortCom: string;
    FComPortTerminator: string;
    FEnableComPort: Boolean;
  public
    { Public declarations }
    ComPort: TComPort;
  end;

var
  DMStartup: TDMStartup;

implementation

{$R *.dfm}

procedure TDMStartup.DataModuleCreate(Sender: TObject);
begin
  // ComPort settings
  Config := GetConfiguration('ComPort');
  FEnableComPort := StrToBool(GetParameterValue(Config, 'Enable com port'));
  FComPortCom := GetParameterValue(Config, 'COM');
  FComPortTerminator := GetParameterValue(Config, 'Terminator');

  if FEnableComPort then
  begin
    ComPort := TComPort.Create(FComPortCom, FComPortTerminator);
    ComThread := TComThread.Create(True, ComPort, 1000);
    ComThread.Start;
  end;
end;

end.
