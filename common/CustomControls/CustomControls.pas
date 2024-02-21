unit CustomControls;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Math, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.WinXCtrls, Vcl.Clipbrd, Plc, Utils;

type
  TAutomationControl = class(TPanel)
  private
    FValue: Variant;
    FSignalID: Integer;
    FSignalLength: Integer;
    FError: Boolean;

    procedure ControlClick(Sender: TObject);
    procedure ControlMouseEnter(Sender: TObject);
  published
    procedure SetValue(AValue: Variant);
  public
    property Value: Variant read FValue write SetValue;
    property SignalID: Integer read FSignalID write FSignalID;
    property SignalLength: Integer read FSignalLength write FSignalLength;
    property Error: Boolean read FError write FError;

    constructor Create(AOwner: TComponent); override;
  end;

const
  K_COLOR_TRUE = $007EC86F;
  K_COLOR_FALSE = $00507B2F;

implementation

uses
  UDMPLC;

{ TAutomationControl }

constructor TAutomationControl.Create(AOwner: TComponent);
var
  LBorder: TShape;
begin
  inherited Create(AOwner);

  LBorder := TShape.Create(Self);
  LBorder.Parent := Self;
  LBorder.Align := alClient;
  LBorder.Brush.Style := bsClear;
  LBorder.Enabled := False;

  Self.BevelInner := bvNone;
  Self.BevelKind := bkNone;
  Self.BevelOuter := bvNone;

  Self.AlignWithMargins := True;
  Self.Margins.Top := 10;
  Self.Margins.Left := 5;
  Self.Margins.Right := 5;

  Self.Font.Color := clWhite;

  Self.ParentBackground := False;
  Self.ShowHint := True;

  Self.OnClick := ControlClick;
  Self.OnMouseEnter := ControlMouseEnter;
end;

procedure TAutomationControl.SetValue(AValue: Variant);
begin
  Caption := AValue;

  if Error then
  begin
    Color := clGray;
    Caption := 'error';
    Font.Color := clWhite;
    Exit;
  end;

  // Bit
  if SignalLength = 1 then
  begin
    Caption := '';
    if AValue then
    begin
      Color := K_COLOR_TRUE;
      Font.Color := clBlack;
    end else
    begin
      Color := K_COLOR_FALSE;
      Font.Color := clWhite;
    end;
  end else
  begin
    Font.Color := clBlack;
    Color := cLWhite;
  end;
end;

procedure TAutomationControl.ControlClick(Sender: TObject);
var
  LValue: Variant;
  LSignal: TSignal;
begin
  if Sender is TAutomationControl then
  begin
    try
      LSignal := DMPLC.GetSignal(TAutomationControl(Sender).SignalID);
      if LSignal.SignalType = TSignalType.TX then
      begin
        LValue := DMPLC.ReadValue(LSignal.SignalIndex);
        DMPLC.WriteValue(LSignal.SignalIndex, not LValue);
      end;
    except
      on E: Exception do
        MessageDlg(E.Message, mtError, [mbOk], 0);
    end;
  end;
end;

procedure TAutomationControl.ControlMouseEnter(Sender: TObject);
var
  LHint: String;
  LSignal: TSignal;
begin
  if Sender is TAutomationControl then
  begin
    LSignal := DMPLC.GetSignal(TAutomationControl(Sender).SignalID);
    // sintassi indirizzo plc: DB10DBX20.0
    LHint := Format('DB%dDBX%d.%d', [LSignal.DataBlock, LSignal.ByteIndex, LSignal.BitIndex]);
    TAutomationControl(Sender).Hint := LHint;
  end;
end;

end.
