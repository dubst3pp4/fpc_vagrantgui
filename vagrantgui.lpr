program vagrantgui;

{$mode objfpc}{$H+}

uses
      {$IFDEF UNIX}{$IFDEF UseCThreads}
      cthreads,
      {$ENDIF}{$ENDIF}
      Interfaces, // this includes the LCL widgetset
      Forms, globalstatusform, vagrantcli, vmoutform, vagrantcommands
      { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
	Application.CreateForm(TFormGlobalStatus, FormGlobalStatus);
  Application.CreateForm(TVMOutWindow, VMOutWindow);
  Application.Run;
end.

