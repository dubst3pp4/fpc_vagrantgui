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

  TCustomCommand = class(TInterfacedObject, IExecutableCommand)
  {< abstract command class which defines some helper functions }
  protected
    { the path to the binary }
    FBinary : ansistring;
    { sets the binary path }
    procedure SetBinary(binary : ansistring);
  public
    { find the binary }
    function FindBinary(binary : ansistring) : ansistring;
    { execute the command }
    procedure execute(params: array of string); virtual; abstract;
    property Binary : ansistring read FBinary;
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

constructor TVagrantCommand.Create;
begin
  inherited;
  SetBinary(FindBinary(VAGRANT_CMD));
end;

procedure TVagrantUpCommand.execute(params: array of string);
var
  id     : string;
  output : string;
begin
  if -1 = High(params) then
    raise Exception.Create('expected first param to be id of vagrant machine');
  id := params[0];
  try
    if not RunCommand(FBinary, [VAGRANT_ARG_UP, id], output) then
      raise Exception.Create(FBinary + ' ' + VAGRANT_ARG_UP + ' ' + id + ' failed.');
   except
     raise;
   end;
end;

procedure TVagrantHaltCommand.execute(params: array of string);
var
  id     : string;
  output : string;
begin
  if -1 = High(params) then
    raise Exception.Create('expected first param to be id of vagrant machine');
  id := params[0];
  try
    if not RunCommand(FBinary, [VAGRANT_ARG_HALT, id], output) then
      raise Exception.Create(FBinary + ' ' + VAGRANT_ARG_HALT + ' ' + id + ' failed.');
   except
     raise;
   end;
end;

procedure TVagrantGlobalStatusCommand.execute(params: array of string);
begin

end;

end.

