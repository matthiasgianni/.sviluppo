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

    procedure GenerateDebugControls;
    procedure RefreshDebugControls;
    procedure RefreshDebug;
  public
    { Public declarations }
  end;

var
  FormDebug: TFormDebug;

implementation

{$R *.dfm}

procedure TFormDebug.FormCreate(Sender: TObject);
begin
  FDMPLC := TDMPLC.Create(nil);

  if FDMPLC.PLC.Connected then
    LogStatus(pnlStatus, 'Connessione al plc avvenuta correttamente!', ltOk)
  else
  begin
    LogStatus(pnlStatus, 'Impossibile connettersi al plc', ltError);
    Exit;
  end;

  GenerateDebugControls;
  RefreshDebug;
end;

procedure TFormDebug.GenerateDebugControls;
var
  LSignal: TSignal;
begin
  for LSignal in FDMPLC.SignalCollection do
  begin
    with TCheckBox.Create(Self) do
    begin
      Parent := Self;
      Align := alTop;
      AlignWithMargins := True;
      Margins.Left := 10;
      Margins.Top := 10;
      Name := 'DEBUGCONTROL_' + IntToStr(LSignal.SignalIndex);
      Caption := LSignal.Name;
      Checked := False;
      Tag := LSignal.SignalIndex;
    end;
  end;
end;

procedure TFormDebug.RefreshDebug;
begin
  RefreshDebugControls;
end;

procedure TFormDebug.RefreshDebugControls;
var
  I: Integer;
  LSignal: TSignal;
  LCheckBox: TCheckBox;
begin
  if FDMPLC.SignalCollection.Count = 0 then
    Exit;

  for I := 0 to Pred(Self.ControlCount) do
  begin
    if Self.Controls[I] is TCheckBox then
    begin
      LCheckBox := TCheckBox(Self.Controls[I]);

      // Ottieni l'indice del segnale associato al checkbox dal Tag
      if (LCheckBox.Tag >= 0) and (LCheckBox.Tag < FDMPLC.SignalCollection.Count) and
         (Pos('DEBUGCONTROL_', LCheckBox.Name) > 0) then
      begin
        // Segnale dalla collezione
        LSignal := FDMPLC.SignalCollection[LCheckBox.Tag];

        // Aggiorna il valore del checkbox in base al valore del segnale
        LCheckBox.Checked := LSignal.Value;
      end;
    end;
  end;
end;

procedure TFormDebug.TimerDebugTimer(Sender: TObject);
begin
  RefreshDebug;
end;

end.
