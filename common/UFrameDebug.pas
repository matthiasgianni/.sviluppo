unit UFrameDebug;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Utils, CustomControls, Plc, Vcl.ComCtrls;

type
  TFrameDebug = class(TFrame)
    TimerUpdate: TTimer;
    PageControl: TPageControl;
    tsRX: TTabSheet;
    tsTX: TTabSheet;
    procedure TimerUpdateTimer(Sender: TObject);
  private
    { Private declarations }
    FConfig: TConfigSettings;
    FColNum: Integer;
    FHorizontalMargin: Integer;
    FVerticalMargin: Integer;
    FControlWidth: Integer;
    FControlHeight: Integer;

    procedure GenerateControls; overload;
    procedure GenerateControls(ASignalType: TSignalType); overload;
    procedure Refresh;
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
  if (DMPLC.PLC <> nil) then
  begin
    GenerateControls;
    TimerUpdate.Enabled := True;
  end;
end;

procedure TFrameDebug.GenerateControls;
begin
  // Generazione controlli LETTURA
  GenerateControls(TSignalType.RX);
  // Generazione controlli SCRITTURA
  GenerateControls(TSignalType.TX);
end;

procedure TFrameDebug.GenerateControls(ASignalType: TSignalType);
const
  K_START_POS_MARGIN = 20;
var
  I, Row, Column: Integer;
  Panel: TAutomationControl;
  LabelDesc: TLabel;
  LSignal: TSignal;
  LParent: TWinControl;
begin
  if ASignalType = TSignalType.RX then
    LParent := tsRX
  else
    LParent := tsTX;

  I := 0;
  for LSignal in DMPLC.PLC.SignalCollection do
  begin
    if LSignal.SignalType <> ASignalType then
      Continue;

    Row := I div FColNum;
    Column := I mod FColNum;

    // Crea il pannello
    Panel := TAutomationControl.Create(LParent);
    Panel.Parent := LParent;

    // Posizionamento dinamico
    if Column = 0 then
      Panel.Left := K_START_POS_MARGIN
    else 
      Panel.Left := K_START_POS_MARGIN + (FHorizontalMargin * Column) + (Column * FControlWidth);
      
    Panel.Top := FVerticalMargin + Row * (FControlHeight + FVerticalMargin);
    Panel.Width := FControlWidth;
    Panel.Height := FControlHeight;

    if LSignal.SignalType = TSignalType.TX then
      Panel.Cursor := crHandPoint;

    Panel.SignalID := LSignal.SignalIndex;
    Panel.SignalLength := LSignal.SignalLength;
    Panel.Name := 'AUTOMATIONCONTROL_' + IntToStr(LSignal.SignalIndex);
    Panel.Caption := '';

    // Crea la label
    LabelDesc := TLabel.Create(LParent);
    LabelDesc.Parent := LParent;
    LabelDesc.Top := FVerticalMargin + Row * (FControlHeight + FVerticalMargin) - LabelDesc.Height;
    LabelDesc.Left := Panel.Left;
    LabelDesc.Caption := LSignal.Name;

    Inc(I);
  end;
end;

procedure TFrameDebug.Refresh;
var
  I, J: Integer;
  LSignal: TSignal;
  LAutoCtrl: TAutomationControl;
begin
  for I := 0 to Pred(PageControl.PageCount) do
  begin
    for J := 0 to Pred(PageControl.Pages[I].ControlCount) do
    begin
      if PageControl.Pages[I].Controls[J] is TAutomationControl then
      begin
        LAutoCtrl := TAutomationControl(PageControl.Pages[I].Controls[J]);

        // Ottieni l'indice del segnale associato al checkbox dal Tag
        if (LAutoCtrl.SignalID >= 0) and (LAutoCtrl.SignalId < DMPLC.PLC.SignalCollection.Count) and
           (Pos('AUTOMATIONCONTROL_', LAutoCtrl.Name) > 0) then
        begin
          // Segnale dalla collezione
          LSignal := DMPLC.GetSignal(LAutoCtrl.SignalID);

          LAutoCtrl.Error := LSignal.InError;

          // Set del valore
          LAutoCtrl.SetValue(LSignal.Value);
        end;
      end;
    end;
  end;
end;

procedure TFrameDebug.TimerUpdateTimer(Sender: TObject);
begin
  Refresh;
end;

end.
