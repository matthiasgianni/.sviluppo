unit UDebug;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Plc, Utils, UDMPLC;

type
  TFormDebug = class(TForm)
    pnlStatus: TPanel;
    TimerDebug: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TimerDebugTimer(Sender: TObject);
  private
    { Private declarations }
    FDMPLC: TDMPLC;
    Config: TConfigSettings;

    procedure GenerateAutomationControls;
    procedure RefreshAutomationControls;
  public
    { Public declarations }
  end;

var
  FormDebug: TFormDebug;

implementation

{$R *.dfm}

procedure TFormDebug.FormCreate(Sender: TObject);
begin
  TimerDebug.Enabled := False;
  FDMPLC := TDMPLC.Create(nil);

  if not FDMPLC.PLC.Connected then
  begin
    ShowCustomMessageForm(ltError, 'Impossibile stabilire una connessione con il plc ' + FDMPLC.PLC.PLCIp);
    Application.Terminate;
  end;

  TimerDebug.Enabled := True;
  ShowCustomMessageForm(ltOk, 'Connessione al plc eseguita correttamente');

  GenerateAutomationControls;
  RefreshAutomationControls;
end;

procedure TFormDebug.GenerateAutomationControls;
var
  LSignal: TSignal;
begin
  if not FDMPLC.PLC.Connected then
    Exit;

  for LSignal in FDMPLC.SignalCollection do
  begin
    with TAutomationControl.Create(Self) do
    begin
      Parent := Self;
      Align := alTop;
      Name := 'AUTOMATIONCONTROL_' + IntToStr(LSignal.SignalIndex);
      Caption := LSignal.Name;
      SignalID := LSignal.SignalIndex;
    end;
  end;
end;

procedure TFormDebug.RefreshAutomationControls;
var
  I: Integer;
  LSignal: TSignal;
  LAutoCtrl: TAutomationControl;
begin
  if not FDMPLC.PLC.Connected then
    Exit;

  if FDMPLC.SignalCollection.Count = 0 then
    Exit;

  for I := 0 to Pred(Self.ControlCount) do
  begin
    if Self.Controls[I] is TAutomationControl then
    begin
      LAutoCtrl := TAutomationControl(Self.Controls[I]);

      // Ottieni l'indice del segnale associato al checkbox dal Tag
      if (LAutoCtrl.SignalID >= 0) and (LAutoCtrl.SignalId < FDMPLC.SignalCollection.Count) and
         (Pos('AUTOMATIONCONTROL_', LAutoCtrl.Name) > 0) then
      begin
        // Segnale dalla collezione
        LSignal := FDMPLC.SignalCollection[LAutoCtrl.SignalID];

        // Aggiorna il valore del checkbox in base al valore del segnale
        LAutoCtrl.SetState(LSignal.Value);
      end;
    end;
  end;
end;

procedure TFormDebug.TimerDebugTimer(Sender: TObject);
begin
  RefreshAutomationControls;
end;

end.
