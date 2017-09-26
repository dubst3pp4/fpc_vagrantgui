unit vagrantcli;
{ < Unit with classes to wrap the vagrant-cli tool in FreePascal }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process,
  Vagrantcommands;

type

  { record to wrap the output which is produced by vagrant when the
    --machine-readable option was set }
  TVagrantReadableOutput = record
    out_timestamp : integer;
    out_target    : string;
    out_type      : string;
    out_data      : array of string;
  end;

  { the @name is a class which represents a line / host produced by the
    command "vagrant global-status" }
  TVagrantStatus = class
  protected
    FId        : string;
    FName      : string;
    FProvider  : string;
    FState     : string;
    FDirectory : string;
    { get the value of a field, index is mapped to a field }
    function  GetField(Index : Integer) : string;
    { set the value of a field, index is mapped to a field }
    procedure SetField(Index : Integer; Value : string);
  public
    { id of the host }
    property Id        : string Index 1 read GetField write SetField;
    { name of the host }
    property Name      : string Index 2 read GetField write SetField;
    { provider (virtualbox, vmware) of the host }
    property Provider  : string Index 3 read GetField write SetField;
    { state of the host (running, poweroff) }
    property State     : string Index 4 read GetField write SetField;
    { directory of the host }
    property Directory : string Index 5 read GetField write SetField;
  end;

  TVagrantCLICallback = TCustomCommandCallback;

  { the @name represents the vagrant bin and its options,
    at the moment, only global-status is supported }
  TVagrantCLI = class
  protected
    FVagrantBin : ansistring;
    FOnUpCommand: TVagrantCLICallback;
    FOnHaltCommand: TVagrantCLICallback;
    procedure Init;
    { parses a line of the vagrant output to a TVagrantReadableOutput }
    function  ProcessOutputLine(Line : string) : TVagrantReadableOutput;
  public
    { creates a new instance }
    constructor Create;
    { tries to locate the vagrant bin }
    procedure FindVagrantBin;
    { wrapper to vagrant global-status }
    function GetGlobalStatus : TFPList;
    function UpCommand(id: string): boolean;
    function HaltCommand(id: string): boolean;

    { path to where vagrant is installed to }
    property VagrantBin : ansistring read FVagrantBin;
    property OnUpCommand: TVagrantCLICallback read FOnUpCommand write FOnUpCommand;
    property OnHaltCommand: TVagrantCLICallback read FOnHaltCommand write FOnHaltCommand;
  end;

implementation

uses
  DateUtils;

const
  FIND_CMD               = {$ifdef windows}'where.exe'{$endif}{$ifdef unix}'which'{$endif};

  VAGRANT_CMD            = {$ifdef windows}'vagrant.exe'{$endif}{$ifdef unix}'vagrant'{$endif};
  VAGRANT_LINE_DELIMITER = ',';
  VAGRANT_HEAD_DELIMITER = '----------------------------------------------------------------------------------------';

  ARG_MACHINE_READABLE   = '--machine-readable';
  ARG_GLOBAL_STATUS      = 'global-status';
  ARG_HALT               = 'halt';
  ARG_UP                 = 'up';

{ TVagrantStatus }

function TVagrantStatus.GetField(Index: Integer): string;
begin
  Case Index of
    1 : GetField := FId;
    2 : GetField := FName;
    3 : GetField := FProvider;
    4 : GetField := FState;
    5 : GetField := FDirectory;
    else
      raise Exception.Create('unknown field index ' + IntToStr(Index) );
  end;
end;

procedure TVagrantStatus.SetField(Index: Integer; Value: string);
begin
  Case Index of
    1 : FId := Value;
    2 : FName := Value;
    3 : FProvider := Value;
    4 : FState := Value;
    5 : FDirectory := Value;
    else
      raise Exception.Create('unknown field index ' + IntToStr(Index) );
   end;
end;

{ TVagrantCLI }

procedure TVagrantCLI.Init;
begin
  try
    FindVagrantBin;
  except
    raise;
  end;
end;

function TVagrantCLI.ProcessOutputLine(Line: string) : TVagrantReadableOutput;
var
  StrList : TStrings;
  Output  : TVagrantReadableOutput;
  i       : Integer;
begin
  { explode string at , }
  StrList               := TStringList.Create;
  StrList.Delimiter     := VAGRANT_LINE_DELIMITER;
  StrList.DelimitedText := Line;

  for i:= 0 to StrList.Count - 1 do
    begin
      case i of
        0: Output.out_timestamp := StrToInt(StrList[i]); { timestamp }
        1: Output.out_target := StrList[i];              { target }
        2: Output.out_type := StrList[i];                { type }
        else
          begin
            { grow array size by one }
            SetLength(Output.out_data, Length(Output.out_data)+1);
            Output.out_data[High(Output.out_data)] := StrList[i];
          end;
      end;
    end;
  Result := Output;
end;

constructor TVagrantCLI.Create;
begin
  try
    Init;
  except
    raise;
  end;
end;

procedure TVagrantCLI.FindVagrantBin;
var
  path : ansistring;
begin
  path := '';
  try
    if RunCommand(FIND_CMD, [VAGRANT_CMD], path) then
      FVagrantBin := Trim(path)
    else
      raise Exception.Create(VAGRANT_CMD + ' executable not found!' + sLineBreak
        + 'Please make sure that the Vagrant executable is in your path.');
  except
    raise;
  end;
end;

function TVagrantCLI.GetGlobalStatus: TFPList;
var
  Output        : ansistring;
  StrList       : TStrings;

  i, CountHeader, CountLine : integer;
  IsHeader      : boolean;

  VagrantLine   : TVagrantReadableOutput;
  VagrantStatus : TVagrantStatus;
  StatusList    : TFPList;

  Time1, Time2  : TDateTime;
  DiffSeconds   : Integer;
begin

  Time1 := Now;

  { get command output }
  Output := '';
  if RunCommand(VAGRANT_CMD, [ARG_MACHINE_READABLE, ARG_GLOBAL_STATUS], output) then
    Output := Trim(Output)
  else
    raise Exception.Create(VAGRANT_CMD + 'global-status failed.');

  { explode output line by line using a TStringList }
  StrList       := TStringList.Create;
  StrList.Text  := Output;

  CountHeader   := 0;
  CountLine     := 0;
  IsHeader      := true;
  VagrantStatus := nil;
  StatusList    := TFPList.Create();

  { foreach line, call ProcessOutputLine }
  for i:= 0 to StrList.Count - 1 do
    begin
      VagrantLine := ProcessOutputLine(StrList[i]);

      if (Length(VagrantLine.out_data) < 1) or (VagrantLine.out_data[0] <> 'info') then
        Continue;

      { count the headers to get the number of items }
      if IsHeader then
        begin
          if VagrantLine.out_data[1] = VAGRANT_HEAD_DELIMITER then
            begin
              IsHeader := false;
              Continue;
            end;
          CountHeader := CountHeader + 1;
          Continue;
        end;

      if CountLine = CountHeader then
        CountLine := 0;

      { skip remaining lines when they are less then CountHeader }
      if (CountLine = 0) and (i >= (StrList.Count - CountHeader)) then
        Break;

      if CountLine = 0 then
        begin
          { create new object }
          VagrantStatus    := TVagrantStatus.Create();
          VagrantStatus.Id := VagrantLine.out_data[1];
          StatusList.Add(VagrantStatus);
        end;

      if CountLine > 0 then
        begin
          case CountLine of
            1 : VagrantStatus.Name      := VagrantLine.out_data[1];
            2 : VagrantStatus.Provider  := VagrantLine.out_data[1];
            3 : VagrantStatus.State     := VagrantLine.out_data[1];
            4 : VagrantStatus.Directory := VagrantLine.out_data[1];
          end;
        end;

      CountLine := CountLine + 1;
    end; { /for}

  Time2 := Now;

  Result := StatusList;
  DiffSeconds := MilliSecondsBetween(Time2, Time1);
  WriteLn('GetGlobalStatus: ' + IntToStr(DiffSeconds) + 'ms');
end;

function TVagrantCLI.UpCommand(id: string): boolean;
var
  Output : TStringList;
  OutputStream: TStream;
  upCmd : TVagrantUpCommand;
  ExitStatus: integer;
begin

  upCmd        := TVagrantUpCommand.Create();
  if Assigned(FOnUpCommand) then
    upCmd.OnExecute := FOnUpCommand;

  OutputStream := TMemoryStream.Create;
  upCmd.execute([id], OutputStream, ExitStatus);

  Output := TStringList.Create;
  OutputStream.Position := 0;
  Output.LoadFromStream(OutputStream);

  OutputStream.Free;
  FreeAndNil(upCmd);

  if not (ExitStatus = 0) then
    Result := false
  else
    Result := true;
end;

function TVagrantCLI.HaltCommand(id: string): boolean;
var
  Output : TStringList;
  OutputStream: TStream;
  haltCmd : TVagrantHaltCommand;
  ExitStatus: integer;
begin

  haltCmd      := TVagrantHaltCommand.Create();
  if Assigned(FOnHaltCommand) then
    haltCmd.OnExecute := FOnHaltCommand;

  OutputStream := TMemoryStream.Create;
  haltCmd.execute([id], OutputStream, ExitStatus);

  Output := TStringList.Create;
  OutputStream.Position := 0;
  Output.LoadFromStream(OutputStream);

  OutputStream.Free;
  FreeAndNil(haltCmd);

  if not (ExitStatus = 0) then
    Result := false
  else
    Result := true;
end;


end.

