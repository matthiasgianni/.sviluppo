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
    PLCThread := TPlcPollingThread.Create(True, PLC, 1000);

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

          Signal.DataBlock := SignalObject.GetValue('DB').Value.ToInteger;
          Signal.ByteIndex := SignalObject.GetValue('byte').Value.ToInteger;
          Signal.BitIndex := SignalObject.GetValue('bit').Value.ToInteger;
          Signal.Value := 0;
          Signal.SignalLength := SignalObject.GetValue('dim').Value.ToInteger;
          Signal.Name := SignalObject.GetValue('Name').Value;
          Signal.SignalIndex := i;
          Signal.InError := True;

          Signals.Add(Signal);
        end;
      end;
    end;
  finally
    JSONObject.Free;
  end;
end;

end.
