unit UFrameDebug;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Utils, CustomControls, Plc;

type
  TFrameDebug = class(TFrame)
    TimerUpdate: TTimer;
    procedure TimerUpdateTimer(Sender: TObject);
  private
    { Private declarations }
    FConfig: TConfigSettings;
    FColNum: Integer;
    FHorizontalMargin: Integer;
    FVerticalMargin: Integer;
    FControlWidth: Integer;
    FControlHeight: Integer;

    procedure GenerateControls;
    procedure Update;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  UDmPlc;

{ TFrameDebug }

constructor TFrameDebug.Create(AOwner: TComponent);
begin
  inherited;

  // Configrazioni base per il frame
  FConfig := GetConfiguration('Debug');
  FColNum := StrToInt(GetParameterValue(FConfig, 'Columns', '3'));

  FHorizontalMargin := StrToInt(GetParameterValue(FConfig, 'HorizontalMargin', '40'));
  FVerticalMargin := StrToInt(GetParameterValue(FConfig, 'VerticalMargin', '20'));

  FControlWidth := StrToInt(GetParameterValue(FConfig, 'ControlWidth', '40'));
  FControlHeight := StrToInt(GetParameterValue(FConfig, 'ControlHeight', '20'));

  TimerUpdate.Enabled := False;
  if (DMPLC.PLCConnected) and (DMPLC.SignalCollection.Count > 0) then
  begin
    GenerateControls;
    TimerUpdate.Enabled := True;
  end;
end;

procedure TFrameDebug.GenerateControls;
var
  I, Row, Column: Integer;
  Panel: TAutomationControl;
  LabelDesc: TLabel;
  LSignal: TSignal;
begin
  I := 0;
  for LSignal in DMPLC.SignalCollection do
  begin
    Row := I div FColNum;
    Column := I mod FColNum;

    // Crea il pannello
    Panel := TAutomationControl.Create(Self);
    Panel.Parent := Self;
    Panel.Left := FHorizontalMargin + Column * (FControlWidth + FHorizontalMargin);
    Panel.Top := FVerticalMargin + Row * (FControlHeight + FVerticalMargin);
    Panel.Width := FControlWidth;
    Panel.Height := FControlHeight;

    Panel.SignalID := LSignal.SignalIndex;
    Panel.SignalLength := LSignal.SignalLength;
    Panel.Name := 'AUTOMATIONCONTROL_' + IntToStr(LSignal.SignalIndex);
    Panel.Caption := '';
    Panel.Cursor := crHandPoint;
    //Panel.OnClick := ControlClick;

    // Crea la label
    LabelDesc := TLabel.Create(Self);
    LabelDesc.Parent := Self;
    LabelDesc.Top := FVerticalMargin + Row * (FControlHeight + FVerticalMargin) - LabelDesc.Height;
    LabelDesc.Left := Panel.Left;
    LabelDesc.Caption := LSignal.Name;

    Inc(I);
  end;
end;

procedure TFrameDebug.Update;
var
  I: Integer;
  LSignal: TSignal;
  LAutoCtrl: TAutomationControl;
begin
  for I := 0 to Pred(Self.ControlCount) do
  begin
    if Self.Controls[I] is TAutomationControl then
    begin
      LAutoCtrl := TAutomationControl(Self.Controls[I]);

      // Ottieni l'indice del segnale associato al checkbox dal Tag
      if (LAutoCtrl.SignalID >= 0) and (LAutoCtrl.SignalId < DMPLC.SignalCollection.Count) and
         (Pos('AUTOMATIONCONTROL_', LAutoCtrl.Name) > 0) then
      begin
        // Segnale dalla collezione
        LSignal := DMPLC.SignalCollection[LAutoCtrl.SignalID];

        // Gestione errore
        LAutoCtrl.Error := DMPLC.Error;
        //if DMPLC.Error then
          //LogStatus(pnlStatus, DMPLC.ErrorMessage, ltError);

        // Set del valore
        LAutoCtrl.SetValue(LSignal.Value);
      end;
    end;
  end;

  TimerUpdate.Enabled := not DMPLC.Error;
end;

procedure TFrameDebug.TimerUpdateTimer(Sender: TObject);
begin
  Update;
end;

end.