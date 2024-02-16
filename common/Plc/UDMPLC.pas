unit UDmPlc;

interface

uses
  System.SysUtils, System.JSON, System.IOUtils, System.Classes, Vcl.Dialogs, Plc, Utils,
  Vcl.ExtCtrls;

type
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
    PLCThread: TPlcPollingThread;
    PLCConnected: Boolean;

    procedure WriteValue(ASignalIdx: Integer; AValue: Variant);
    function ReadValue(ASignalIdx: Integer): Variant;
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
  PLC.Free;
  PLCThread.Terminate;
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
    PLC := TPLC.Create(IP, Rack, Slot);
    PLC.SignalCollection := FSignalCollection;
    PLCThread := TPlcPollingThread.Create(True, PLC, 500);
    PLCThread.Start;
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
          Signal.Value := 0;

          if SignalObject.TryGetValue('dim', LSignalDim) then
            Signal.SignalLength := LSignalDim.Value.ToInteger
          else
            Signal.SignalLength := 1;

          Signal.Name := SignalObject.GetValue('Name').Value;
          Signal.SignalIndex := i;
          Signal.InError := False;

          Signals.Add(Signal);
        end;
      end;
    end;
  finally
    JSONObject.Free;
  end;
end;

function TDMPLC.ReadValue(ASignalIdx: Integer): Variant;
var
  i: Integer;
  LModifiedSignal: TSignal;
  LResult: Variant;
begin
  for i := 0 to PLC.SignalCollection.Count - 1 do
  begin
    if PLC.SignalCollection[i].SignalIndex = ASignalIdx then
    begin
      LResult := PLC.SignalCollection[i].Value;
      if LResult = 0 then
        LResult := False;
      if LResult = 1 then
        LResult := True;
      Result := LResult;
      Break;
    end;
  end;
end;

procedure TDMPLC.WriteValue(ASignalIdx: Integer; AValue: Variant);
var
  i: Integer;
  LModifiedSignal: TSignal;
begin
  for i := 0 to PLC.SignalCollection.Count - 1 do
  begin
    if PLC.SignalCollection[i].SignalIndex = ASignalIdx then
    begin
      // CONTROLLO DOPPIO (ANCHE IN METODO PLC)
      if not (PLC.SignalCollection[i].SignalType = TSignalType.TX) then
        Exit;

      LModifiedSignal := PLC.SignalCollection[i];
      // Modifica il valore del segnale
      LModifiedSignal.Value := AValue;
      // Re-inserisci il segnale modificato nella lista nella stessa posizione
      PLC.SignalCollection[i] := LModifiedSignal;

      PLC.TXPLC;

      Exit;
    end;
  end;
end;

end.
