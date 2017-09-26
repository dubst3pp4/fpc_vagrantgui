unit vagrantcommands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  IExecutableCommand = interface
  {< Interface for all executable commands }
    ['{0CA93471-2F58-445F-8AA5-26F1604B8F45}']
    { execute the command }
    procedure execute(params: array of string; var OutputStream: TStream; out ExitStatus: integer);
  end;

  TCustomCommandCallback = procedure(var Output: string) of object;

  TCustomCommand = class(TInterfacedObject, IExecutableCommand)
  {< abstract command class which defines some helper functions }
  protected
    { the path to the binary }
    FBinary : ansistring;
    { method, which gets called during execution }
    FOnExecute: TCustomCommandCallback;
    { sets the binary path }
    procedure SetBinary(binary : ansistring);
  public
    { find the binary }
    function FindBinary(binary : ansistring) : ansistring;
    { execute the command }
    procedure execute(params: array of string; var OutputStream: TStream; out ExitStatus: integer); virtual;
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
    procedure execute(params: array of string; var OutputStream: TStream; out ExitStatus: integer); override;
  end;

  TVagrantHaltCommand = class(TVagrantCommand)
  {< class to execute 'vagrant halt' command }
  public
    procedure execute(params: array of string; var OutputStream: TStream; out ExitStatus: integer); override;
  end;

  TVagrantGlobalStatusCommand = class(TVagrantCommand)
  {< class to execute 'vagrant global-status' command }
  public
      procedure execute(params: array of string; var OutputStream: TStream; out ExitStatus: integer); override;
  end;


implementation

uses
  Process;

const
  FIND_CMD         = {$ifdef unix}'which'{$endif}{$ifdef windows}'where.exe'{$endif};
  VAGRANT_CMD      = {$ifdef unix}'vagrant'{$endif}{$ifdef windows}'vagrant.exe'{$endif};
  VAGRANT_ARG_HALT = 'halt';
  VAGRANT_ARG_UP   = 'up';

  BUFFER_SIZE      = 2048;

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

procedure TCustomCommand.SetBinary(binary : ansistring);
begin
  FBinary := binary;
end;

procedure TCustomCommand.execute(params: array of string; var OutputStream: TStream; out ExitStatus: integer);
var
  i            : integer;
  AProcess     : TProcess;
  BytesRead    : longint;
  Buffer       : array[1..BUFFER_SIZE] of byte;
begin
  AProcess := TProcess.Create(nil);
  AProcess.Options    := [poUsePipes];
  AProcess.Executable := FBinary;
  for i := 0 to Length(params)-1 do
  begin
    AProcess.Parameters.Add(params[i]);
  end;

  AProcess.Execute; // here we go!

  { TODO: convert Buffer into UTF-8 string:
          str := TEncoding.UTF8.GetString(bytes);
          call callbackMethod and pass str! }
  repeat
    BytesRead := AProcess.Output.Read(Buffer, BUFFER_SIZE);
    OutputStream.Write(Buffer, BytesRead);
  until BytesRead = 0;
  ExitStatus := AProcess.ExitStatus;
  AProcess.Free;
end;

constructor TVagrantCommand.Create;
begin
  inherited;
  SetBinary(FindBinary(VAGRANT_CMD));
end;

procedure TVagrantUpCommand.execute(params: array of string; var OutputStream: TStream; out ExitStatus: integer);
var
  id           : string;
  AProcess     : TProcess;
  BytesRead    : longint;
  Buffer       : array[1..BUFFER_SIZE] of byte;
begin
  if -1 = High(params) then
    raise Exception.Create('expected first param to be id of vagrant machine');
  id := params[0];
  inherited execute([VAGRANT_ARG_UP, id], OutputStream, ExitStatus);
end;

procedure TVagrantHaltCommand.execute(params: array of string; var OutputStream: TStream; out ExitStatus: integer);
var
  id           : string;
  AProcess     : TProcess;
  BytesRead    : longint;
  Buffer       : array[1..BUFFER_SIZE] of byte;
begin
  if -1 = High(params) then
    raise Exception.Create('expected first param to be id of vagrant machine');
  id := params[0];
  inherited execute([VAGRANT_ARG_HALT, id], OutputStream, ExitStatus);
end;

procedure TVagrantGlobalStatusCommand.execute(params: array of string; var OutputStream: TStream; out ExitStatus: integer);
begin

end;

end.

