unit UDebug;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Math, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus, Vcl.Clipbrd,
  Plc, Utils, UDmPlc, CustomControls, Vcl.ComCtrls;

type
  TFormDebug = class(TForm)
    pnlStatus: TPanel;
    TimerDebug: TTimer;
    PageAutomationControls: TPageControl;
    ts1: TTabSheet;
    PanelAutomationControls: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure TimerDebugTimer(Sender: TObject);
  private
    { Private declarations }
    FDMPLC: TDMPLC;
    Config: TConfigSettings;
    FColNum: Integer;
    FHorizontalMargin: Integer;
    FVerticalMargin: Integer;
    FControlWidth: Integer;
    FControlHeight: Integer;

    procedure GenerateAutomationControls(AParent: TWinControl);
    procedure RefreshAutomationControls(AParent: TWinControl);
    procedure AutomationControlClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  FormDebug: TFormDebug;

implementation

{$R *.dfm}

procedure TFormDebug.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Config := GetConfiguration('Debug');
  FColNum := StrToInt(GetParameterValue(Config, 'Columns', '3'));

  FHorizontalMargin := StrToInt(GetParameterValue(Config, 'HorizontalMargin', '40'));
  FVerticalMargin := StrToInt(GetParameterValue(Config, 'VerticalMargin', '20'));

  FControlWidth := StrToInt(GetParameterValue(Config, 'ControlWidth', '40'));
  FControlHeight := StrToInt(GetParameterValue(Config, 'ControlHeight', '20'));

  FDMPLC := TDMPLC.Create(nil);
  GenerateAutomationControls(PanelAutomationControls);

  // Nascondo linguette tab
  for I := 0 to Pred(PageAutomationControls.PageCount) do
  begin
    PageAutomationControls.Pages[I].TabVisible := False;
    PageAutomationControls.Pages[I].Visible := True;
  end;
end;

procedure TFormDebug.GenerateAutomationControls(AParent: TWinControl);
var
  I, Row, Column: Integer;
  Panel: TAutomationControl;
  LabelDesc: TLabel;
  LSignal: TSignal;
begin
  I := 0;
  for LSignal in FDMPLC.SignalCollection do
  begin
    Row := I div FColNum;
    Column := I mod FColNum;

    // Crea il pannello
    Panel := TAutomationControl.Create(AParent);
    Panel.Parent := AParent;
    Panel.Left := FHorizontalMargin + Column * (FControlWidth + FHorizontalMargin);
    Panel.Top := FVerticalMargin + Row * (FControlHeight + FVerticalMargin);
    Panel.Width := FControlWidth;
    Panel.Height := FControlHeight;

    Panel.SignalID := LSignal.SignalIndex;
    Panel.Name := 'AUTOMATIONCONTROL_' + IntToStr(LSignal.SignalIndex);
    Panel.Caption := '';
    Panel.Cursor := crHandPoint;
    Panel.OnClick := AutomationControlClick;

    // Crea la label
    LabelDesc := TLabel.Create(AParent);
    LabelDesc.Parent := AParent;
    LabelDesc.Top := FVerticalMargin + Row * (FControlHeight + FVerticalMargin) - LabelDesc.Height;
    LabelDesc.Left := Panel.Left;
    LabelDesc.Caption := LSignal.Name;

    Inc(I);
  end;
end;

procedure TFormDebug.RefreshAutomationControls(AParent: TWinControl);
var
  I: Integer;
  LSignal: TSignal;
  LAutoCtrl: TAutomationControl;
begin
  if FDMPLC.SignalCollection.Count = 0 then
    Exit;

  for I := 0 to Pred(AParent.ControlCount) do
  begin
    if AParent.Controls[I] is TAutomationControl then
    begin
      LAutoCtrl := TAutomationControl(AParent.Controls[I]);

      // Ottieni l'indice del segnale associato al checkbox dal Tag
      if (LAutoCtrl.SignalID >= 0) and (LAutoCtrl.SignalId < FDMPLC.SignalCollection.Count) and
         (Pos('AUTOMATIONCONTROL_', LAutoCtrl.Name) > 0) then
      begin
        // Segnale dalla collezione
        LSignal := FDMPLC.SignalCollection[LAutoCtrl.SignalID];
        LAutoCtrl.SetValue(LSignal.Value);
        // Se è un bit allora evito di scriverci dentro
        if LSignal.SignalLength = 1 then
          LAutoCtrl.Caption := '';
      end;
    end;
  end;
end;

procedure TFormDebug.AutomationControlClick(Sender: TObject);
begin
  if Sender is TAutomationControl then
  begin
    try
      if TAutomationControl(Sender).Caption = '' then
        Exit;
      Clipboard.Clear;
      Clipboard.AsText := TAutomationControl(Sender).Caption;
      LogStatus(pnlStatus, Format('Testo copiato (%s)', [Clipboard.AsText]), ltOk);
    except
      on E: Exception do
        LogStatus(pnlStatus, Format('Errore durante la copia nella clipboard: %s', [E.Message]), ltError);
    end;
  end;
end;

procedure TFormDebug.TimerDebugTimer(Sender: TObject);
begin
  RefreshAutomationControls(PanelAutomationControls);
end;

end.
