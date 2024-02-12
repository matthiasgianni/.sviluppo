unit UDMStartup;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Utils, UComPort, Vcl.ExtCtrls;

type
  TDMStartup = class(TDataModule)
    ComPortPolling: TTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure ComPortPollingTimer(Sender: TObject);
  private
    { Private declarations }
    Config: TConfigSettings;
    FComPortCom: string;
    FComPortTerminator: string;
  public
    { Public declarations }
    ComPort: TComPort;
    ComPortData: string;
    ComPortError: string;

    procedure StartComPortPolling;
    procedure StopComPortPolling;
  end;

var
  DMStartup: TDMStartup;

implementation

{$R *.dfm}

procedure TDMStartup.StartComPortPolling;
begin
  ComPortPolling.Enabled := True;
end;

procedure TDMStartup.StopComPortPolling;
begin
  ComPortPolling.Enabled := False;
end;

procedure TDMStartup.ComPortPollingTimer(Sender: TObject);
begin
  ComPortData := ComPort.ReadCom(ComPortError);
end;

procedure TDMStartup.DataModuleCreate(Sender: TObject);
begin
  // ComPort settings
  Config := GetConfiguration('ComPort');
  FComPortCom := GetParameterValue(Config, 'COM');
  FComPortTerminator := GetParameterValue(Config, 'Terminator');

  ComPort := TComPort.Create;
  ComPort.Port := FComPortCom;
  ComPort.Terminator := FComPortTerminator;
  ComPortPolling.Enabled := False;
end;

end.
