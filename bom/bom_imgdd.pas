
{***************************************************************************
*        Unit name : bom_imgdd.pas                                         *
*        Copyright : (C)cdbc.dk 2022                                       *
*        Programmer: Benny Christensen                                     *
*        Created   : 10.12.2022 /bc bom-child of bom_dd, designed to cater *
*        Updated   : 10.12.2022 /bc for the image section in daily diary.  *
*                                   and deleting nodes in a treeview.      *
*                    10.12.2022 /bc updated cleartreeview to cater for all *
*                                   rootnodes...                           *
*                                                                          *
****************************************************************************
*        License:                                                          *
*        "Beer License" - If you meet me one day, you'll buy me a beer :-) *
*        I'm NOT liable for anything! Use at your own risk!!!              *
***************************************************************************}

unit bom_imgdd;
{$mode objfpc}{$H+}
{$define debug}
interface
uses
  Classes, SysUtils, bc_baselist;

  // 1 image item that knows how to load its own data and can hold a list of
  // images & thumbnails 200x150px

implementation

(*
var 
  __Example: TObject;

function Example: TObject; { singleton }
begin
  if not assigned(__Example) then __Example:= TObject.Create;
  Result:= __Example;
end; { gets released on progam end }
*)

initialization
//  __Example:= nil;

finalization 
//  FreeAndNil(__Example);
  
end.

