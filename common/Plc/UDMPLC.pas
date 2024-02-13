unit UDmPlc;

interface

uses
  System.SysUtils, System.JSON, System.IOUtils, System.Classes, Vcl.Dialogs, Plc, Utils,
  Vcl.ExtCtrls;

type
  TDMPLC = class(TDataModule)
    Timer: TTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
    Config: TConfigSettings;
    FError: Boolean;
    FErrorMessage: string;

    procedure LoadPLCConfiguration;
    procedure InitializePLC;
    procedure LoadSignalsJSON(var Signals: TSignalCollection);
    procedure ReadBytes;
  public
    { Public declarations }
    PLC: TPLC;
    PLCConnected: Boolean;
    SignalCollection: TSignalCollection;

    property Error: Boolean read FError write FError;
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
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
  FError := False;
  FErrorMessage := '';
  PLC.Disconnect;
  Timer.Enabled := False;
end;

procedure TDMPLC.LoadPLCConfiguration;
var
  IP: String;
  Rack, Slot: Integer;
  PLCEnabled: Boolean;
begin
  FError := False;
  FErrorMessage := '';
  Timer.Enabled := False;
  PLCConnected := False;
  Timer.Interval := 500;

  // Leggo la configurazione del PLC
  Config := GetConfiguration('PLC');
  IP := GetParameterValue(Config, 'Ip');
  Rack := StrToInt(GetParameterValue(Config, 'Rack'));
  Slot := StrToInt(GetParameterValue(Config, 'Slot'));
  PLCEnabled := StrToBool(GetParameterValue(Config, 'PLC Enabled'));

  SignalCollection := TSignalCollection.Create;
  LoadSignalsJSON(SignalCollection);

  PLC := TPLC.Create(IP, Rack, Slot);
  if PLCEnabled then
    InitializePLC;
end;

procedure TDMPLC.InitializePLC;
begin
  try
    if PLC.Connect(FErrorMessage) then
    begin
      PLCConnected := True;
      if SignalCollection.Count > 0 then
        Timer.Enabled := True;
    end
    else
      raise ECustomException.Create(FErrorMessage);
  except
    on E: ECustomException do
    begin
      E.LogError;
      // Ensure cleanup in case of an exception
      PLC.Disconnect;
      Timer.Enabled := False;
    end;
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

          Signals.Add(Signal);
        end;
      end;
    end;
  finally
    JSONObject.Free;
  end;
end;

procedure TDMPLC.ReadBytes;
var
  LError: String;
begin
  PLC.ReadSignalsFromPLC(SignalCollection, LError);
  if LError <> '' then
  begin
    Timer.Enabled := False;
    FError := True;
    FErrorMessage := LError;
    PLC.Disconnect;
    //MessageDlg(LError, mtError, mbOKCancel, 0);
  end;
end;

procedure TDMPLC.TimerTimer(Sender: TObject);
begin
  if PLCConnected then
    ReadBytes;
end;

end.
