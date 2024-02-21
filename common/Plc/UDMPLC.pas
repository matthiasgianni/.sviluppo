unit UDmPlc;

interface

uses
  System.SysUtils, System.Variants, System.Rtti, System.JSON, System.IOUtils, System.Classes, Vcl.Dialogs, Vcl.ExtCtrls,
  Utils, Plc;

type
  PLCAttribute = class(TCustomAttribute)
  private
    FConstantName: string;
  public
    constructor Create(const ConstantName: string);
    property ConstantName: string read FConstantName;
  end;

  TPlcProcessorCustom = class
  public type
    TProcessorThread = class(TThread)
    private
      FInterval: Integer;
      FFirstIteration: Boolean;
      procedure DoPolling;
    protected
      procedure Execute; override;
    public
      constructor Create(CreateSuspended: Boolean; AInterval: Integer);
      destructor Destroy; override;
    end;
  private
    FPLC: TPLC;
    FProcessorThread: TPlcProcessorCustom.TProcessorThread;
    procedure MapPLCAttributes;
  public
    constructor Create(APLC: TPLC); virtual;
  end;

  TDMPLC = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    Config: TConfigSettings;
    FSignalCollection: TSignalCollection;

    procedure LoadPLCConfiguration;
    procedure LoadSignalsJSON(var Signals: TSignalCollection);
  public
    { Public declarations }
    PLC: TPLC;
    Processor: TPlcProcessorCustom;
    PLCConnected: Boolean;

    procedure WriteValue(ASignalIdx: Integer; AValue: Variant);
    function ReadValue(ASignalIdx: Integer): Variant;
    function GetSignal(ASignalIdx: Integer): TSignal;
  end;

var
  DMPLC: TDMPLC;

implementation

{$R *.dfm}

procedure TDMPLC.DataModuleCreate(Sender: TObject);
begin
  LoadPLCConfiguration;
end;

procedure TDMPLC.DataModuleDestroy(Sender: TObject);
begin
  if (Assigned(PLC)) and (Assigned(PLC.Thread)) then
  begin
    PLC.Free;
    PLC.Thread.Terminate;
  end;
end;

procedure TDMPLC.LoadPLCConfiguration;
var
  IP: String;
  Rack, Slot: Integer;
  PLCEnabled: Boolean;
begin
  // Leggo la configurazione del PLC
  Config := GetConfiguration('PLC');
  IP := GetParameterValue(Config, 'Ip');
  Rack := StrToInt(GetParameterValue(Config, 'Rack'));
  Slot := StrToInt(GetParameterValue(Config, 'Slot'));
  PLCEnabled := StrToBool(GetParameterValue(Config, 'PLC Enabled'));

  FSignalCollection := TSignalCollection.Create;
  LoadSignalsJSON(FSignalCollection);

  if PLCEnabled then
  begin
    // Creazione plc
    PLC := TPLC.Create(IP, Rack, Slot);
    // Assegnazione della collezione di segnali al plc
    PLC.SignalCollection := FSignalCollection;
    // Assegnazione del thread plc
    PLC.Thread := TPLC.TPlcPollingThread.Create(True, PLC, 500);
    PLC.Thread.Start;

    Processor := TPlcProcessorCustom.Create(PLC);
  end;
end;

procedure TDMPLC.LoadSignalsJSON(var Signals: TSignalCollection);
var
  JSONString: string;
  JSONObject: TJSONObject;
  SignalsArray: TJSONArray;
  SignalObject: TJSONObject;
  i: Integer;
  Signal: TSignal;
  LFileName: String;
  LSignalDim: TJSONValue;
begin
  LFileName := ExtractFilePath(ParamStr(0)) + 'SignalCollection.json';
  if not FileExists(LFileName) then
  begin
    //ShowMessage('Il file JSON non esiste: ' + FileName);
    Exit;
  end;

  JSONString := TFile.ReadAllText(LFileName);
  JSONObject := TJSONObject.ParseJSONValue(JSONString) as TJSONObject;

  try
    if Assigned(JSONObject) then
    begin
      SignalsArray := JSONObject.GetValue('signals') as TJSONArray;

      if Assigned(SignalsArray) then
      begin
        for i := 0 to SignalsArray.Count - 1 do
        begin
          SignalObject := SignalsArray.Items[i] as TJSONObject;

          if SignalObject.GetValue('Type').Value = 'RX' then
            Signal.SignalType := TSignalType.RX
          else
            Signal.SignalType := TSignalType.TX;

          Signal.DataBlock := SignalObject.GetValue('DB').Value.ToInteger;
          Signal.ByteIndex := SignalObject.GetValue('byte').Value.ToInteger;
          Signal.BitIndex := SignalObject.GetValue('bit').Value.ToInteger;

          if Signal.SignalLength = 1 then
            Signal.Value := False
          else
            Signal.Value := 0;

          if SignalObject.TryGetValue('dim', LSignalDim) then
            Signal.SignalLength := LSignalDim.Value.ToInteger
          else
            Signal.SignalLength := 1;

          Signal.Name := SignalObject.GetValue('Name').Value;
          Signal.SignalIndex := i;
          Signal.InError := False;
          Signal.ConstName := SignalObject.GetValue('ConstName').Value;

          Signals.Add(Signal);
        end;
      end;
    end;
  finally
    JSONObject.Free;
  end;
end;

function TDMPLC.ReadValue(ASignalIdx: Integer): Variant;
begin
  Result := GetSignal(ASignalIdx).Value;
end;

procedure TDMPLC.WriteValue(ASignalIdx: Integer; AValue: Variant);
var
  LSignal, LTmpSignal: TSignal;
begin
  LSignal := GetSignal(ASignalIdx);

  // CONTROLLO DOPPIO (ANCHE IN METODO PLC)
  if not (LSignal.SignalType = TSignalType.TX) then
    Exit;

  LTmpSignal := LSignal;

  // Modifica il valore del segnale
  LTmpSignal.Value := AValue;
  // Re-inserisci il segnale modificato nella lista nella stessa posizione
  PLC.SignalCollection[ASignalIdx] := LTmpSignal;

  PLC.TXPLC;
end;

function TDMPLC.GetSignal(ASignalIdx: Integer): TSignal;
begin
  Result := PLC.SignalCollection[ASignalIdx];
end;

{ TPlcProcessorCustom }

constructor TPlcProcessorCustom.Create(APLC: TPLC);
begin
  inherited Create;
  FPLC := APLC;

  FProcessorThread := TPlcProcessorCustom.TProcessorThread.Create(True, 500);
  FProcessorThread.Start;

  MapPLCAttributes;
end;

procedure TPlcProcessorCustom.MapPLCAttributes;
var
  Signal: TSignal;
  Context: TRttiContext;
  Prop: TRttiProperty;
  Attribute: TCustomAttribute;
begin
  Context := TRttiContext.Create;
  try
    for Signal in FPLC.SignalCollection do
    begin
      for Prop in Context.GetType(Self.ClassType).GetProperties do
      begin
        for Attribute in Prop.GetAttributes do
        begin
          if (Attribute is PLCAttribute) and (PLCAttribute(Attribute).ConstantName = Signal.ConstName) then
          begin
            if varType(Signal.Value) = varBoolean then
              Prop.SetValue(Self, Boolean(Signal.Value));
//              varInteger: Prop.SetValue(Self, Integer(Signal.Value));
//              varInt64: Prop.SetValue(Self, Int64(Signal.Value));
//              varString: Prop.SetValue(Self, string(Signal.Value));
          end;
        end;
      end;
    end;
  finally
    Context.Free;
  end;
end;

{ PLCAttribute }

constructor PLCAttribute.Create(const ConstantName: string);
begin
  inherited Create;
  FConstantName := ConstantName;
end;

{ TPlcProcessorCustom.TProcessorThread }

constructor TPlcProcessorCustom.TProcessorThread.Create(CreateSuspended: Boolean; AInterval: Integer);
begin
  inherited Create(CreateSuspended);

  FreeOnTerminate := True;
  FInterval := AInterval;
end;

destructor TPlcProcessorCustom.TProcessorThread.Destroy;
begin

  inherited;
end;

procedure TPlcProcessorCustom.TProcessorThread.DoPolling;
begin

end;

procedure TPlcProcessorCustom.TProcessorThread.Execute;
begin
  while not Terminated do
  begin
    // Esegui l'operazione di polling
    DoPolling;
    // Attendi per l'intervallo di tempo specificato prima di procedere con il prossimo ciclo di polling
    Sleep(FInterval);
  end;
end;

end.
