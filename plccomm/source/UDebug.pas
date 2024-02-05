unit UDebug;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Math, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Plc, Utils, UDmPlc,
  CustomControls;

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
  //TimerDebug.Enabled := True;
  FDMPLC := TDMPLC.Create(nil);
  GenerateAutomationControls;
end;

procedure TFormDebug.GenerateAutomationControls;
const
  ControlWidth = 40; // Larghezza del pannello
  ControlHeight = 15; // Altezza del pannello
  HorizontalMargin = 60; // Margine orizzontale tra i controlli
  VerticalMargin = 20; // Margine verticale tra i controlli
var
  I, Row, Column: Integer;
  Panel: TAutomationControl;
  LabelDesc: TLabel;
  LSignal: TSignal;
  NumColumns: Integer; // Numero di colonne desiderato
begin
  NumColumns := 3; // Imposta il numero desiderato di colonne

  I := 0;
  for LSignal in FDMPLC.SignalCollection do
  begin
    Row := I div NumColumns;
    Column := I mod NumColumns;

    // Crea il pannello
    Panel := TAutomationControl.Create(Self);
    Panel.Parent := Self;
    Panel.Left := HorizontalMargin + Column * (ControlWidth + HorizontalMargin);
    Panel.Top := VerticalMargin + Row * (ControlHeight + VerticalMargin);
    Panel.Width := ControlWidth;
    Panel.Height := ControlHeight;

    Panel.SignalID := LSignal.SignalIndex;
    Panel.Name := 'AUTOMATIONCONTROL_' + IntToStr(LSignal.SignalIndex);
    Panel.Caption := '';


    // Crea la label
    LabelDesc := TLabel.Create(Self);
    LabelDesc.Parent := Self;
    LabelDesc.Top := VerticalMargin + Row * (ControlHeight + VerticalMargin) - LabelDesc.Height;
    LabelDesc.Left := Panel.Left;
    LabelDesc.Caption := LSignal.Name;

    Inc(I);
  end;
end;


procedure TFormDebug.RefreshAutomationControls;
var
  I: Integer;
  LSignal: TSignal;
  LAutoCtrl: TAutomationControl;
begin
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
        //LAutoCtrl.IsPlcConnected := FDMPLC.PLCConnected;
        LAutoCtrl.SetValue(LSignal.Value);
      end;
    end;
  end;
end;

procedure TFormDebug.TimerDebugTimer(Sender: TObject);
begin
  RefreshAutomationControls;
end;

end.
