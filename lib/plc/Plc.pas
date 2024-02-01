// FOR TESTING TIA SIMULATOR SHOULD HAVE matthias AS INSTANCE

unit Plc;

interface

uses
  nodave, Utils, System.Generics.Collections;

type
  TSignal = record
    SignalIndex: Integer;
    DataBlock: Integer;
    ByteIndex: Integer;
    BitIndex: Integer;
    Value: Boolean;
    Name: String;
  end;

  TSignalCollection = class(TList<TSignal>);

  TPLC = class(TObject)
  private
    FConnected: Boolean;
    FdS: _daveOSserialType;
    FdI: pdaveInterface;
    FdC: pdaveConnection;

    FPLCIp: String;
    FPLCRack: Integer;
    FPLCSlot: Integer;
  public
    property PLCIp: String read FPLCIp write FPLCIp;
    property Connected: Boolean read FConnected write FConnected;

    procedure Disconnect;

    function Connect: Boolean;
    procedure ReadSignalsFromPLC(Signals: TSignalCollection; var AError: String);

    constructor Create(AIp: String; ARack, ASlot: Integer); reintroduce;
  end;

implementation

{ TPLC }

constructor TPLC.Create(AIp: String; ARack, ASlot: Integer);
begin
  FPLCIp := AIp;
  FPLCRack := ARack;
  FPLCSlot := ASlot;
end;

function TPLC.Connect: Boolean;
var
  LIp: AnsiString;
  I: Integer;
begin
  FConnected := False;

  LIp := FPLCIp + #0;

  FdS.rfd := openSocket(102, @LIp[1]);
  FdS.wfd := FdS.rfd;

  if FdS.rfd > 0 then
  begin
    FdI := daveNewInterface(FdS, 'IF1', 0, daveProtoISOTCP, daveSpeed187k);
    FdI^.timeout := 500;

    if (daveInitAdapter(FdI) = 0) then
    begin
      FdC := daveNewConnection(FdI, 0, FPLCRack, FPLCSlot);

      if daveConnectPLC(FdC) = 0 then
      begin
        FConnected := True;
      end else
      begin
        FConnected := False;
        // Chiudi la connessione in caso di errore
        daveDisconnectAdapter(FdI);
      end;
    end else
    begin
      FConnected := False;
    end;
  end else
  begin
    FConnected := False;
  end;
  Result := FConnected;
end;

procedure TPLC.Disconnect;
begin
  if FConnected then
  begin
    daveDisconnectPLC(FdC);
    daveDisconnectAdapter(FdI);
    FConnected := False;
  end;
end;

procedure TPLC.ReadSignalsFromPLC(Signals: TSignalCollection; var AError: String);
var
  i: Integer;
  signal: TSignal;
  readResult: Integer;
  buffer: Byte; // Buffer separato per ogni segnale
begin
  AError := '';
  // Cicla attraverso ogni segnale
  for i := 0 to Signals.Count - 1 do
  begin
    signal := Signals[i];

    // Effettua la lettura dal PLC
    readResult := daveReadBytes(FdC, daveDB, signal.DataBlock, signal.ByteIndex, 1, @buffer);

    // Verifica il risultato della lettura
    if readResult = 0 then
    begin
      // Aggiorna il valore del segnale nella lista
      signal.Value := (buffer and (1 shl signal.BitIndex)) <> 0;
      Signals[i] := signal;
    end else
    begin
      AError := string(daveStrerror(readResult));
    end;
  end;
end;


end.
