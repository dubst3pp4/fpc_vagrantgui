unit vagrantcommands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

type

  IExecutableCommand = interface
  {< Interface for all executable commands }
    ['{0CA93471-2F58-445F-8AA5-26F1604B8F45}']
    { execute the command }
    procedure execute(params: array of string);
  end;

  TCustomCommandCallback = procedure(var Output: string) of object;

  TCustomCommand = class(TInterfacedObject, IExecutableCommand)
  {< abstract command class which defines some helper functions }
  protected
    { the path to the binary }
    FBinary : ansistring;
    FProcess: TProcess;
    { method, which gets called during execution }
    FOnExecute: TCustomCommandCallback;
    { sets the binary path }
    procedure SetBinary(binary : ansistring);
  public
    { find the binary }
    function FindBinary(binary : ansistring) : ansistring;
    function IsRunning: boolean;
    { execute the command }
    procedure execute(params: array of string); virtual;
    property Binary : ansistring read FBinary;
    property OnExecute: TCustomCommandCallback read FOnExecute write FOnExecute;
  end;

  TVagrantCommand = class(TCustomCommand)
  {< base class for all vagrant commands
   searches for vagrant binary on initialization }
  public
    constructor Create;
  end;

  TVagrantUpCommand = class(TVagrantCommand)
  {< class to execute 'vagrant up' command }
  public
    procedure execute(params: array of string); override;
  end;

  TVagrantHaltCommand = class(TVagrantCommand)
  {< class to execute 'vagrant halt' command }
  public
    procedure execute(params: array of string); override;
  end;

  TVagrantGlobalStatusCommand = class(TVagrantCommand)
  {< class to execute 'vagrant global-status' command }
  public
      procedure execute(params: array of string); override;
  end;


implementation

const
  FIND_CMD         = {$ifdef unix}'which'{$endif}{$ifdef windows}'where.exe'{$endif};
  VAGRANT_CMD      = {$ifdef unix}'vagrant'{$endif}{$ifdef windows}'vagrant.exe'{$endif};
  VAGRANT_ARG_HALT = 'halt';
  VAGRANT_ARG_UP   = 'up';
  BUFFER_SIZE      = 2048;



{ TCustomCommand}

function TCustomCommand.FindBinary(binary : ansistring) : ansistring;
var
  path : ansistring;
begin
  path := '';
  try
    if RunCommand(FIND_CMD, [binary], path) then
      Result := Trim(path)
    else
      raise Exception.Create(binary + ' executable not found!' + sLineBreak
        + 'Please make sure that the Vagrant executable is in your path.');
  except
    raise;
  end;
end;

function TCustomCommand.IsRunning: boolean;
begin
  if not Assigned(FProcess) then
    Result := false
  else
    Result := FProcess.Running;
end;

procedure TCustomCommand.SetBinary(binary : ansistring);
begin
  FBinary := binary;
end;

procedure TCustomCommand.execute(params: array of string);
var
  i            : integer;
  BytesRead    : longint;
  Buffer       : array[1..BUFFER_SIZE] of byte;
  BufferTmp    : TBytes;
  str          : string;
begin
  FProcess := TProcess.Create(nil);
  FProcess.Options    := [poUsePipes];
  FProcess.Executable := FBinary;
  for i := 0 to Length(params)-1 do
  begin
    FProcess.Parameters.Add(params[i]);
  end;

  FProcess.Execute; // here we go!

  {
  repeat
    BytesRead := AProcess.Output.Read(Buffer, BUFFER_SIZE);
    SetLength(BufferTmp, BytesRead);
    if Assigned(FOnExecute) then
    begin
      Move(Buffer[0], BufferTmp[0], BytesRead);
      str := TEncoding.UTF8.GetString(BufferTmp);
      FOnExecute(str);
    end;
    OutputStream.Write(Buffer, BytesRead);
  until BytesRead = 0;
  ExitStatus := AProcess.ExitStatus;
  AProcess.Free;
  }
end;



{ TVagrantCommand }

constructor TVagrantCommand.Create;
begin
  inherited;
  SetBinary(FindBinary(VAGRANT_CMD));
end;



{ TVagrantUpCommand }

procedure TVagrantUpCommand.execute(params: array of string);
var
  id           : string;
begin
  if -1 = High(params) then
    raise Exception.Create('expected first param to be id of vagrant machine');
  id := params[0];
  inherited execute([VAGRANT_ARG_UP, id]);
end;



{ TVagrantHaltCommand }

procedure TVagrantHaltCommand.execute(params: array of string);
var
  id           : string;
begin
  if -1 = High(params) then
    raise Exception.Create('expected first param to be id of vagrant machine');
  id := params[0];
  inherited execute([VAGRANT_ARG_HALT, id]);
end;



{ TVagrantGlobalStatusCommand }

procedure TVagrantGlobalStatusCommand.execute(params: array of string);
begin

end;

end.

