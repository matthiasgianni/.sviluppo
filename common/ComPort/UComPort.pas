unit UComPort;

interface

uses
  System.SysUtils, System.Classes, synaser, Utils;

type
  TComPort = class(TBlockSerial)
  private
    FPort: string;
    FTerminator: string;
    FComResult: string;
  public
    property ComResult: string read FComResult write FComResult;

    procedure ReadCom;

    constructor Create(APort: string; ATerminator: string = #13#10); reintroduce;
  end;

  TComThread = class(TThread)
  private
    FComPort: TComPort;
    FInterval: Integer;
    procedure DoPolling;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; AComPort: TComPort; AInterval: Integer);
    destructor Destroy; override;
  end;

implementation

constructor TComPort.Create(APort: string; ATerminator: string);
begin
  FPort := APort;
  FTerminator := ATerminator;
end;

procedure TComPort.ReadCom;
begin
  try
    if (Self <> nil) and (not InstanceActive) then
      Connect(FPort);

    if LastError <> 0 then
    begin
      raise ECustomException.Create(GetErrorDesc(LastError));
      CloseSocket;
    end else
      if WaitingDataEx > 0 then
        FComResult := RecvTerminated(1000, FTerminator);
  except
    on E: ECustomException do
      E.LogError;
  end;
end;

{ TComThread }

constructor TComThread.Create(CreateSuspended: Boolean; AComPort: TComPort; AInterval: Integer);
begin
  inherited Create(CreateSuspended);

  FreeOnTerminate := True;
  FComPort := AComPort;
  FInterval := AInterval;
end;

destructor TComThread.Destroy;
begin
  inherited;
end;

procedure TComThread.DoPolling;
begin
  while not Terminated do
  begin
    Synchronize(DoPolling);
    Sleep(FInterval);
  end;
end;

procedure TComThread.Execute;
begin
  inherited;
  FComPort.ReadCom;
end;

end.
