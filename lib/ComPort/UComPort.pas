unit UComPort;

interface

uses
  System.SysUtils, synaser;

type
  TComPort = class(TBlockSerial)
  private
    FPort: string;
    FTerminator: string;
  public
    property Port: string read FPort write FPort;
    property Terminator: string read FTerminator write FTerminator;

    function ReadCom(var AError: String): string;
    constructor Create;
  end;

implementation

constructor TComPort.Create;
begin
  inherited;
  FPort := '';
  FTerminator := #13#10; // terminatore default
end;

function TComPort.ReadCom(var AError: String): string;
var
  LData: string;
begin
  if FPort = '' then
  begin
    AError := 'Nessuna porta specificata';
    Exit;
  end;

  AError := '';
  try
    if (Self <> nil) and (not Self.InstanceActive) then
      Self.Connect(FPort);

    if Self.LastError <> 0 then
    begin
      AError := Self.GetErrorDesc(Self.LastError);
      Self.CloseSocket;
    end else
      if Self.WaitingDataEx > 0 then
        Result := Self.RecvTerminated(1000, FTerminator);
  except
    on E: Exception do
      AError := E.Message;
  end;
end;

end.
