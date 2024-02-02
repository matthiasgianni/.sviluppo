unit UDebug;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Math, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Plc, Utils, UDmPlc,
  Vcl.WinXCtrls;

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

    procedure GenerateAutomationControls(APlcConnected: Boolean);
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
  GenerateAutomationControls(FDMPLC.PLC.Connected);
end;

procedure TFormDebug.GenerateAutomationControls(APlcConnected: Boolean);
const
  NumColumns = 3; // Imposta il numero desiderato di colonne
  ControlWidth = 60; // Larghezza del pannello
  ControlHeight = 20; // Altezza del pannello
  Margin = 10; // Margine tra i controlli
var
  I, Row, Column: Integer;
  Panel: TAutomationControl;
  LabelDesc: TLabel;
  LSignal: TSignal;
begin
  I := 1;
  for LSignal in FDMPLC.SignalCollection do
  begin
    // Calcola la posizione della riga e della colonna
    Row := (I - 1) div NumColumns;
    Column := (I - 1) mod NumColumns;

    // Crea il pannello
    Panel := TAutomationControl.Create(Self);
    Panel.Parent := Self;
    Panel.Left := Column * (ControlWidth + Margin) + Margin;
    Panel.Top := Row * (ControlHeight + Margin) + Margin;
    Panel.Width := ControlWidth;
    Panel.Height := ControlHeight;
    
    Panel.SignalID := LSignal.SignalIndex;
    Panel.Name := 'AUTOMATIONCONTROL_' + IntToStr(LSignal.SignalIndex);

    Panel.Caption := '';
    if not APlcConnected then
    begin
      Panel.Color := clGray;
      Panel.Caption := 'xxx';
    end else
    begin
      // Crea la label
      LabelDesc := TLabel.Create(Self);
      LabelDesc.Parent := Panel;
      LabelDesc.Caption := LSignal.Name;
      LabelDesc.Font.Name := 'Open Sans';
      LabelDesc.Left := 5;
      LabelDesc.Top := 5;

      TimerDebug.Enabled := True;
    end;

    Inc(I);
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
