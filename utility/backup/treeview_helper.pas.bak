
{**************************************************************************$
$        Unit name : treeview_helper.pas                                   $
$        Copyright : (C)cdbc.dk 2021                                       $
$        Programmer: Benny Christensen                                     $
$        Created   : 2021.02.23 /bc helper for dealing with ttreeview      $
$        Updated   : 2021.02.23 /bc Initial commit                         $
$                                                                          $
$                                                                          $
$                                                                          $
$                                                                          $
$                                                                          $
$                                                                          $
$                                                                          $
$                                                                          $
$                                                                          $
$**************************************************************************$
$        Purpose   :                                                       $
$        Help for wrangling ttreeview - ttreenode                          $
$                                                                          $
$                                                                          $
$                                                                          $
$                                                                          $
$                                                                          $
$                                                                          $
$**************************************************************************$
$        License   :                                                       $
$        "Beer License" - If you meet me one day, you'll buy me a beer :-) $
$        I'm NOT liable for anything! Use at your own risk!!!              $
$**************************************************************************}

unit treeview_helper;
{$mode objfpc}{$H+}
{-$define debug}
interface

uses
  Classes, SysUtils, ComCtrls;

{ function searches for 1.st occurence of the text provided }
{ dono what to do with level as of yet, just seems handy }
function SearchTreeViewLevelBool(const aTree: TTreeView;
                                 const aSearchString: string;
                                 const aLevel: integer = 1): boolean;

{ does the actual searching, stops at 1.st one found }
function GetNodeByText(const aTree: TTreeView;
                       const aValue: string;
                       aVisible: boolean;
                       const aLevel: integer = 1): TTreeNode;

implementation

{ is it there or not? level defaults to 1, 0 is root }
function SearchTreeViewLevelBool(const aTree: TTreeView;
                                 const aSearchString: string;
                                 const aLevel: integer = 1): boolean;
var
  Node: TTreeNode;
begin
  Result:= false;
  Node:= nil;
  Node:= GetNodeByText(aTree,aSearchString,false{,alevel});
  if Node <> nil then Result:= true;
end;

function GetNodeByText(const aTree: TTreeView;
                       const aValue: string;
                       aVisible: boolean;
                       const aLevel: integer = 1): TTreeNode;
var
  Node: TTreeNode;
begin
  Result:= nil;
  if aTree.Items.Count = 0 then exit;
  Node:= aTree.Items[0]; { ie.: root-node }
  while Node <> nil do begin
    if UpperCase(Node.Text) = UpperCase(aValue) then begin
      Result:= Node;
      if aVisible then Result.MakeVisible;
      break;
    end;
    Node:= Node.GetNext;
  end;
end;

(*
var 
  __Example: TObject;

function Example: TObject; { singleton }
begin
  if not assigned(__Example) then __Example:= TObject.Create;
  Result:= __Example;
end; { gets released on progam end }

function GetNodeByText(const aTree: TTreeView;
                       const aValue: string;
                       aVisible: boolean): TTreeNode;
var
  Node: TTreeNode;
begin
  Result:= nil;
  if aTree.Items.Count = 0 then exit;
  Node:= aTree.Items[0];
  while Node <> nil do begin
    if UpperCase(Node.Text) = UpperCase(aValue) then begin
      Result:= Node;
      if aVisible then Result.MakeVisible;
      break;
    end;
    Node:= Node.GetNext;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
 tn : TTreeNode;
begin
 tn:=GetNodeByText(TreeView1,Edit1.Text,CheckBox1.Checked);
 if tn = nil then
 ShowMessage('Not found!')
 elsebegin
 TreeView1.SetFocus;
 tn.Selected := True;
 end;
end;


*)

initialization
//  __Example:= nil;

finalization 
//  FreeAndNil(__Example);
  
end.

