// FOR TESTING TIA SIMULATOR SHOULD HAVE matthias AS INSTANCE

unit Plc;

interface

uses
  nodave, Utils, System.SysUtils, System.Generics.Collections, System.Classes;

type
  TSignal = record
    SignalIndex: Integer;
    DataBlock: Integer;
    ByteIndex: Integer;
    BitIndex: Integer;
    SignalLength: Integer;
    Value: Variant;
    Name: String;
    InError: Boolean;
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

    FSignalCollection: TSignalCollection;
    FReadingError: string;
  public
    property Connected: Boolean read FConnected write FConnected;
    property SignalCollection: TSignalCollection read FSignalCollection write FSignalCollection;
    property ReadingError: string read FReadingError write FReadingError;

    procedure ReadSignalsFromPLC;
    procedure Disconnect;

    function Connect(var AError: string): Boolean;

    constructor Create(AIp: String; ARack, ASlot: Integer); reintroduce;
  end;

  TPlcPollingThread = class(TThread)
  private
    FPLC: TPLC;
    FInterval: Integer;
    procedure DoPolling;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; APLC: TPLC; AInterval: Integer);
    destructor Destroy; override;
  end;

implementation

{ TPLC }

constructor TPLC.Create(AIp: String; ARack, ASlot: Integer);
begin
  FPLCIp := AIp;
  FPLCRack := ARack;
  FPLCSlot := ASlot;
  FConnected := False;
end;

function TPLC.Connect(var AError: string): Boolean;
var
  LIp: AnsiString;
  I: Integer;
begin
  FConnected := False;
  AError := '';

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
        FConnected := True
      else
        daveDisconnectAdapter(FdI);
    end;
  end;
  Result := FConnected;
  AError := Format('Errore in connessione al plc (%s)', [FPLCIp]);
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

procedure TPLC.ReadSignalsFromPLC;
var
  i: Integer;
  signal: TSignal;
  readResult: Integer;
  buffer: TArray<Byte>; // Buffer generico per contenere dati da PLC
begin
  FReadingError := '';

  // Cicla attraverso ogni segnale
  for i := 0 to FSignalCollection.Count - 1 do
  begin
    signal := FSignalCollection[i];

    // Effettua la lettura dal PLC
    if signal.SignalLength = 1 then
      SetLength(buffer, 1) // Se � un bit, setta la lunghezza del buffer a 1
    else
      SetLength(buffer, signal.SignalLength); // Altrimenti, setta la lunghezza del buffer in base alla lunghezza del segnale

    readResult := daveReadBytes(FdC, daveDB, signal.DataBlock, signal.ByteIndex, Length(buffer), @buffer[0]);

    // Verifica il risultato della lettura
    if readResult = 0 then
    begin
      if signal.SignalLength = 1 then
        // Aggiorna il valore del singolo bit nella lista
        signal.Value := Ord((buffer[0] and (1 shl signal.BitIndex)) <> 0)
      else
        // Aggiorna il valore del dato a pi� byte nella lista
        signal.Value := Swap(PWord(@buffer[0])^);

      signal.InError := False;
    end else
    begin
      FReadingError := string(daveStrerror(readResult));
      signal.InError := True;
    end;
    FSignalCollection[i] := signal;
  end;
end;

{ TPlcPollingThread }

constructor TPlcPollingThread.Create(CreateSuspended: Boolean; APLC: TPLC; AInterval: Integer);
begin
  inherited Create(CreateSuspended);

  FreeOnTerminate := True;
  FPLC := APLC;
  FInterval := AInterval;
end;

destructor TPlcPollingThread.Destroy;
begin

  inherited;
end;

procedure TPlcPollingThread.DoPolling;
begin
  while not Terminated do
  begin
    Synchronize(DoPolling);
    Sleep(FInterval);
  end;
end;

procedure TPlcPollingThread.Execute;
var
  LErrorMessage: string;
begin
  inherited;

  try
    if not FPLC.Connect(LErrorMessage) then
      raise ECustomException.Create(LErrorMessage)
    else
      FPLC.ReadSignalsFromPLC;
  except
    on E: ECustomException do
    begin
      E.LogError;
      FPLC.Disconnect;
    end;
  end;
end;

end.
