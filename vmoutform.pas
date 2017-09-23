unit vmoutform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TVMOutWindow }

  TVMOutWindow = class(TForm)
    StatusMemo: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  VMOutWindow: TVMOutWindow;

implementation

{$R *.lfm}

end.

