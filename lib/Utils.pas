unit Utils;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.JSON, System.IOUtils,
  System.Generics.Collections, UInfoForm;

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

  TAutomationControl = class(TPanel)
  private
    FSignalType: TSignalType;
    FSignalID: Integer;
    FState: Boolean;
  protected
    procedure Paint; override;
  public
    procedure SetState(const Value: Boolean);
    constructor Create(AOwner: TComponent); override;
  published
    property SignalID: Integer read FSignalID write FSignalID;
    property State: Boolean read FState write SetState default False;
  end;

  procedure LogStatus(ASurface: TControl; AMsg: String; ALogType: TLogType);
  function IntToBool(const AValue: Integer): Boolean;

  // JSON METHODS
  procedure LoadConfigurations;
  function GetConfiguration(const ConfigName: string): TConfigSettings;
  function GetParameterValue(const Config: TConfigSettings; const ParamName: string): string;

  procedure ShowCustomMessageForm(AType: TLogType; const AMessage: String);

var
  Configurations: array of TConfigSettings;

implementation

constructor TAutomationControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Stato predefinito
  FState := False;

  // Dimensioni
  Self.Width := 100;
  Self.Height := 25;

  // Posizionamento
  AlignWithMargins := True;
  Margins.Left := 10;
  Margins.Top := 10;

  // Stili
  ParentBackground := False;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  BevelKind := bkNone;
  Font.Name := 'Open Sans';
  Font.Size := 10;
  Font.Style := [fsBold];
end;

procedure TAutomationControl.SetState(const Value: Boolean);
begin
  if FState <> Value then
  begin
    FState := Value;
    Repaint;
  end;
end;

procedure TAutomationControl.Paint;
begin
  inherited Paint;
  if FState then
    Color := $005CB85C
  else
  begin
    Font.Color := clWhite;
    Color := $003F3FE4;
  end;

  Caption := UpperCase(Caption);
end;

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
  //TPanel(ASurface).ParentBackgrou

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

function GetParameterValue(const Config: TConfigSettings; const ParamName: string): string;
var
  Param: TParameter;
begin
  Result := '';

  for Param in Config.Parameters do
  begin
    if SameText(Param.Name, ParamName) then
    begin
      Result := Param.Value;
      Exit;
    end;
  end;
end;

procedure ShowCustomMessageForm(AType: TLogType; const AMessage: String);
var
  LForm: TInfoForm;
  LIconID: Integer;
begin
  LForm := TInfoForm.Create(Application);
  try
    case AType of
      ltOk: LIconID := 0;
      ltWarning: LIconID := 1;
      ltError: LIconID := 2;
    end;

    LForm.SetIcon(LIconID);
    LForm.SetMessage(AMessage);
    LForm.ShowModal;
  finally
    LForm.Free;
  end;
end;

end.
