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
  K_COLOR_TRUE = $005BF886;
  K_COLOR_FALSE = $00057825;

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

  Self.OnClick := ControlClick;
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
      if TAutomationControl(Sender).Caption <> '' then
      begin
        Clipboard.Clear;
        Clipboard.AsText := TAutomationControl(Sender).Caption;
      end;

      // TEST TX
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

end.
