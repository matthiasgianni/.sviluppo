// FOR TESTING TIA SIMULATOR SHOULD HAVE matthias AS INSTANCE

unit Plc;

interface

uses
  nodave, Utils, System.SysUtils, System.Generics.Collections, System.Classes;

type
  TSignalType = (RX, TX);

  TSignal = record
    SignalIndex: Integer;
    SignalType: TSignalType;
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

    procedure RXPLC;
    procedure TXPLC;
    procedure Connect;
    procedure Disconnect;

    constructor Create(AIp: String; ARack, ASlot: Integer); reintroduce;
  end;

  TPlcPollingThread = class(TThread)
  private
    FPLC: TPLC;
    FInterval: Integer;
    FFirstIteration: Boolean;
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

procedure TPLC.Connect;
var
  LIp: AnsiString;
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
        FConnected := True
      else
        daveDisconnectAdapter(FdI);
    end;
  end;
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

procedure TPLC.RXPLC;
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

    // Skip segnale se non di lettura
    if not (signal.SignalType = TSignalType.RX) then
      Continue;

    // Effettua la lettura dal PLC
    if signal.SignalLength = 1 then
      SetLength(buffer, 1) // Se è un bit, setta la lunghezza del buffer a 1
    else
      SetLength(buffer, signal.SignalLength); // Altrimenti, setta la lunghezza del buffer in base alla lunghezza del segnale

    readResult := daveReadBytes(FdC, daveDB, signal.DataBlock, signal.ByteIndex, Length(buffer), @buffer[0]);

    // Verifica il risultato della lettura
    if readResult = 0 then
    begin
      if signal.SignalLength = 1 then
        // Aggiorna il valore del singolo bit nella lista
        signal.Value := Ord((buffer[0] and (1 shl signal.BitIndex)) <> 0)
      else if signal.SignalLength = 2 then
        // Aggiorna il valore del dato a più byte nella lista
        signal.Value := Swap(PWord(@buffer[0])^)
      else if signal.SignalLength = 4 then
        // Leggi i 4 byte dal buffer come un intero a 32 bit
        signal.Value := SwapLong(PInteger(@buffer[0])^);

      signal.InError := False;
    end else
    begin
      FReadingError := string(daveStrerror(readResult));
      signal.InError := True;
    end;
    FSignalCollection[i] := signal;
  end;
end;

procedure TPLC.TXPLC;
var
  i: Integer;
  signal: TSignal;
  writeResult: Integer;
  buffer: TArray<Byte>;
begin
  // Cicla attraverso ogni segnale
  for i := 0 to FSignalCollection.Count - 1 do
  begin
    signal := FSignalCollection[i];

    // Skip segnale se non di scrittura
    if not (signal.SignalType = TSignalType.TX) then
      Continue;

    // Imposta il buffer in base alla lunghezza del segnale
    SetLength(buffer, signal.SignalLength);

    // Inizializza il buffer a zero per evitare valori non desiderati
    FillChar(buffer[0], Length(buffer), 0);

    // Scrivi il valore del segnale nel buffer solo se la lunghezza del buffer è 1
    if signal.SignalLength = 1 then
      buffer[0] := signal.Value
    else
    begin
      // Qui dovresti implementare la logica per convertire il valore del segnale in un array di byte
      // e copiarlo nel buffer in base all'ordinamento dei byte richiesto dal PLC.
      // Ad esempio, se il segnale è un intero a 16 bit, potresti fare qualcosa del genere:
      // PWord(@buffer[0])^ := Swap(signal.Value);
    end;

    // Scrivi i dati nel PLC utilizzando un buffer separato per ciascun segnale
    writeResult := daveWriteBytes(FdC, daveDB, signal.DataBlock, signal.ByteIndex, Length(buffer), @buffer[0]);

    // Verifico il risultato della scrittura
    if writeResult = 0 then
      signal.InError := False
    else
      signal.InError := True;
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
  if not FPLC.Connected then
  begin
    try
      FPLC.Connect;
    except
      on E: ECustomException do
        E.LogError;
    end;
  end;

  if FPLC.Connected then
  begin
    if FFirstIteration then
    begin
      FPLC.TXPLC;
      FFirstIteration := False;
    end;

    FPLC.RXPLC;
  end;
end;

procedure TPlcPollingThread.Execute;
begin
  FFirstIteration := True;
  while not Terminated do
  begin
    // Esegui l'operazione di polling
    DoPolling;
    // Attendi per l'intervallo di tempo specificato prima di procedere con il prossimo ciclo di polling
    Sleep(FInterval);
  end;
end;

end.
