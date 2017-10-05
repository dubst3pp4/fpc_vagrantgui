unit globalstatusform;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
			ComCtrls, Menus, ExtCtrls, vagrantcli, vagrantcommands, typinfo;

type

			{ TFormGlobalStatus }

      TFormGlobalStatus = class(TForm)
        ApplicationProperties1: TApplicationProperties;

        ImageListStatus     : TImageList;
				ListViewStatus      : TListView;
				MenuItemResume      : TMenuItem;
				MenuItemSuspend     : TMenuItem;
				MenuItemHalt        : TMenuItem;
				MenuItemUp          : TMenuItem;
				PopupMenuStatusItem : TPopupMenu;
				StatusBar1          : TStatusBar;
				TimerStatus         : TTimer;

        procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean
          );
        procedure FormActivate(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure ListViewStatusMouseDown(
          Sender : TObject;
          Button : TMouseButton;
					Shift  : TShiftState;
          X, Y   : Integer
        );
        procedure ListViewStatusSelectItem(Sender: TObject; Item: TListItem;
          Selected: Boolean);
        procedure MenuItemHaltClick(Sender: TObject);
        procedure MenuItemUpClick(Sender: TObject);
        procedure TimerStatusTimer(Sender: TObject);
        procedure UpdateListViewStatus;

      protected
        IsItemRightClick     : boolean;
        ClickedItem          : TListItem;

      private
        { private declarations }
        FVagrantCli   : TVagrantCLI;
      public
        { public declarations }
        procedure UpdateStatus(var output: string);
      end;

var
      FormGlobalStatus: TFormGlobalStatus;

implementation

uses
  vmoutform;

{$R *.lfm}

{ TFormGlobalStatus }

{ shows the right-click context menu }
procedure TFormGlobalStatus.ListViewStatusMouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  HitTests : THitTests;
  ListViewCursorPos : TPoint;
begin
  if Button = mbRight then
    IsItemRightClick := true
  else
    IsItemRightClick := false;

  ListViewCursorPos := ListViewStatus.ScreenToClient(Mouse.CursorPos);
  HitTests := ListViewStatus.GetHitTestInfoAt(ListViewCursorPos.X, ListViewCursorPos.Y);

  //locate the clicked item
  if IsItemRightClick and (HitTests <= [htOnIcon, htOnItem, htOnLabel, htOnStateIcon]) then
  begin
    ClickedItem := ListViewStatus.Selected;
    PopupMenuStatusItem.Popup();
  end;
end;

procedure TFormGlobalStatus.ListViewStatusSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  //if (not Selected) or (not IsItemRightClick) then exit;
  if not Assigned(ListViewStatus.Selected) then exit;
  if not Selected then exit;
end;

procedure TFormGlobalStatus.MenuItemHaltClick(Sender: TObject);
begin
  if not Assigned(ClickedItem) then exit;

  VMOutWindow.StatusMemo.Lines.Clear;
  FVagrantCli.HaltCommand(ClickedItem.Caption);

end;

procedure TFormGlobalStatus.MenuItemUpClick(Sender: TObject);
begin
  if not Assigned(ClickedItem) then exit;

  VMOutWindow.StatusMemo.Lines.Clear;
  FVagrantCli.UpCommand(ClickedItem.Caption);

end;

{ updates the vagrant status every x seconds }
procedure TFormGlobalStatus.TimerStatusTimer(Sender: TObject);
begin
  //TimerStatus.Enabled := false;
  UpdateListViewStatus;
  Application.ProcessMessages;
  //TimerStatus.Enabled := true;
end;

procedure TFormGlobalStatus.UpdateListViewStatus;
var
  StatusList   : TFPList;
  StatusItem   : TVagrantStatus;
  ListViewItem : TListItem;
  i, j         : integer;
  ItemFound    : boolean;
begin
  StatusList := FVagrantCli.GetGlobalStatus;

  // for StatusItemP in StatusList do
  for i := 0 to StatusList.Count-1 do
  begin
    ItemFound  := false;
    StatusItem := TVagrantStatus(StatusList[i]);

    // search the item
    for j := 0 to ListViewStatus.Items.Count-1 do
      begin
        if ListViewStatus.Items[j].Caption = StatusItem.Id then
          begin
            ItemFound := true;
            Break;
			    end;
		  end;

    if ItemFound then
      begin
        ListViewItem := ListViewStatus.Items[j];
        if ListViewItem.SubItems[2] <> StatusItem.State then
          ListViewItem.SubItems[2] := StatusItem.State;
		  end
    else
      begin
          ListViewItem         := ListViewStatus.Items.Add;
          ListViewItem.Caption := StatusItem.Id;
          ListViewItem.SubItems.Add(StatusItem.Name);
          ListViewItem.SubItems.Add(StatusItem.Provider);
          ListViewItem.SubItems.Add(StatusItem.State);
          ListViewItem.SubItems.Add(StatusItem.Directory);
  	  end;

    case StatusItem.State of
      'running'             : ListViewItem.ImageIndex := 1;
      'aborted', 'poweroff' : ListViewItem.ImageIndex := 0;
      'saved'               : ListViewItem.ImageIndex := 2;
		end;

    ListViewStatus.Repaint;
  end;
end;

procedure TFormGlobalStatus.FormCreate(Sender: TObject);
begin

  { initialize VagrantCLI }
  try
    FVagrantCli := TVagrantCLI.Create();
	except
    on e: Exception do
    begin
      FreeAndNil(FVagrantCli);
      ShowMessage(e.Message);
      Halt(-1);
		end;
	end;
  StatusBar1.SimpleText := 'Vagrant executable found: ' + FVagrantCli.VagrantBin;

  { events }
  FVagrantCli.OnUpCommand := @UpdateStatus;
  FVagrantCli.OnHaltCommand := @UpdateStatus;

  { vagrant global-status }
  UpdateListViewStatus;

end;

procedure TFormGlobalStatus.FormActivate(Sender: TObject);
begin
  if not VMOutWindow.Showing then VMOutWindow.ShowOnTop();
end;

procedure TFormGlobalStatus.ApplicationProperties1Idle(Sender: TObject;
  var Done: Boolean);
begin
  WriteLn('idle');
  FVagrantCli.CheckCommandQueue;
  Done := true;
end;

procedure TFormGlobalStatus.UpdateStatus(var output: string);
begin
  VMOutWindow.StatusMemo.Lines.Add(output);
  Application.ProcessMessages;
end;

end.

