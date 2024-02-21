unit plc.processor.project;

interface

uses
  System.Classes, UDmPlc;

type
  TPlcProcessor = class(TPlcProcessorCustom)
  private
    FPresenza: Boolean;
    FAckPresenza: Boolean;
  published
    [PLC('RX_PRESENZA')]
    property Presenza: Boolean read FPresenza write FPresenza;
    [PLC('TX_ACK_PRESENZA')]
    property AckPresenza: Boolean read FAckPresenza write FAckPresenza;
  end;

implementation

end.
