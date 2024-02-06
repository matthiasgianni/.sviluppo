unit Utils;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.WinXCtrls, System.JSON, System.IOUtils,
  System.Generics.Collections, UInfoFrame;

type
  TSignalType = (stBool, stInt);
  TLogType = (ltOk, ltWarning, ltError);

  TParameter = record
    Name: string;
    Value: string;
  end;

  TConfigSettings = record
    ConfigName: string;
    Parameters: TArray<TParameter>;
  end;

  procedure LogStatus(ASurface: TControl; AMsg: String; ALogType: TLogType);
  function IntToBool(const AValue: Integer): Boolean;

  // JSON METHODS
  procedure LoadConfigurations;
  function GetConfiguration(const ConfigName: string): TConfigSettings;
  function GetParameterValue(const Config: TConfigSettings; const ParamName: string;
    const DefaultValue: string = ''): string;

  procedure ShowCustomMessageForm(AParent: TWinControl; AType: TLogType; const AMessage: String);

var
  Configurations: array of TConfigSettings;

implementation

procedure LogStatus(ASurface: TControl; AMsg: String; ALogType: TLogType);
var
  LColor: TColor;
begin
  if ASurface = nil then
    Exit;

  if not (ASurface is TPanel) then
    Exit;

  if ALogType = ltOk then
    LColor := $00E06666
  else if ALogType = ltWarning then
    LColor := clWebOrange
  else if ALogType = ltError then
    LColor := clWebRed;

  TPanel(ASurface).ParentColor := False;

  TPanel(ASurface).Font.Color := clWhite;
  TPanel(ASurface).Font.Name := 'Open Sans';
  TPanel(ASurface).Font.Style := [fsBold];

  TPanel(ASurface).Color := LColor;
  TPanel(ASurface).Caption := AMsg;
end;

function IntToBool(const AValue: Integer): Boolean;
begin
  Result := False;
  if AValue = 1 then
    Result := True;
end;

procedure LoadConfigurations;
var
  JSONString: string;
  JSONValue: TJSONValue;
  ConfigArray, ParamArray: TJSONArray;
  ConfigItem, ParamItem: TJSONValue;
  ConfigSettings: TConfigSettings;
  Parameter: TParameter;
  I, J: Integer;
begin
  // Leggi il contenuto del file
  JSONString := TFile.ReadAllText(ExtractFilePath(ParamStr(0)) + 'config.json');

  // Parsa la stringa JSON
  JSONValue := TJSONObject.ParseJSONValue(JSONString);

  try
    if Assigned(JSONValue) and (JSONValue is TJSONObject) then
    begin
      // Accedi all'array delle configurazioni
      ConfigArray := TJSONObject(JSONValue).GetValue('Configuration') as TJSONArray;

      if Assigned(ConfigArray) then
      begin
        // Inizializza l'array di configurazioni
        SetLength(Configurations, ConfigArray.Count);

        // Itera sugli elementi dell'array delle configurazioni
        for I := 0 to ConfigArray.Count - 1 do
        begin
          ConfigItem := ConfigArray.Items[I];

          // Popola il record con il nome della configurazione
          ConfigSettings.ConfigName := TJSONObject(ConfigItem).GetValue('ConfigName').Value;

          // Accedi all'array di parametri
          ParamArray := TJSONObject(ConfigItem).GetValue('Parameters') as TJSONArray;

          // Inizializza l'array di parametri
          SetLength(ConfigSettings.Parameters, ParamArray.Count);

          // Itera sugli elementi dell'array dei parametri
          for J := 0 to ParamArray.Count - 1 do
          begin
            ParamItem := ParamArray.Items[J];

            // Popola il record con i valori del parametro
            Parameter.Name := TJSONObject(ParamItem).GetValue('Name').Value;
            Parameter.Value := TJSONObject(ParamItem).GetValue('Value').Value;

            // Salva il parametro nell'array di parametri
            ConfigSettings.Parameters[J] := Parameter;
          end;

          // Salva il record nell'array di configurazioni
          Configurations[I] := ConfigSettings;
        end;
      end;
    end;
  finally
    JSONValue.Free;
  end;
end;

function GetConfiguration(const ConfigName: string): TConfigSettings;
var
  I: Integer;
begin
  // Cerca la configurazione nel vettore
  for I := 0 to High(Configurations) do
  begin
    if SameText(Configurations[I].ConfigName, ConfigName) then
    begin
      Result := Configurations[I];
      Exit;
    end;
  end;

  // Se non viene trovata alcuna configurazione, restituisci un record vuoto
  FillChar(Result, SizeOf(Result), 0);
end;

function GetParameterValue(const Config: TConfigSettings; const ParamName: string; const DefaultValue: string = ''): string;
var
  Param: TParameter;
begin
  Result := DefaultValue; // Imposta il valore di default

  for Param in Config.Parameters do
  begin
    if SameText(Param.Name, ParamName) then
    begin
      Result := Param.Value;
      Exit;
    end;
  end;
end;

procedure ShowCustomMessageForm(AParent: TWinControl; AType: TLogType; const AMessage: String);
var
  I: Integer;
  LFrame: TInfoFrame;
  LIconID: Integer;
begin
  // Rimuovo eventuale frame creato in precedenza
  for I := Pred(AParent.ControlCount) downto 0 do
  begin
    if AParent.Controls[I] is TInfoFrame then
      TInfoFrame(AParent.Controls[I]).Free;
  end;

  LFrame := TInfoFrame.Create(AParent);
  LFrame.Parent := AParent;
  LFrame.Align := alBottom;
  LFrame.AlignWithMargins := True;
  LFrame.Margins.Left := 10;
  LFrame.Margins.Top := 10;

  case AType of
    ltOk: begin
      LIconID := 0;
      LFrame.Color := $00C1FFC1;
    end;
    ltWarning: begin
      LIconID := 1;
      LFrame.Color := $00A6C2FF;
    end;
    ltError: begin
      LIconID := 2;
      LFrame.Color := $00EEDDFF;
    end;
  end;

  LFrame.SetIcon(LIconID);
  LFrame.SetMessage(AMessage);
end;

end.
