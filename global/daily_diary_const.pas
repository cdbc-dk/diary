unit daily_diary_const;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  MainTitle = 'Daily Diary';
  UnitVersion = '03.03.11.2022';
  cdsQuit = -999;
  { sql statements }
  { create daily diary table }
  CreateDb = 'CREATE TABLE daily_diary(id_dd integer primary key, date_dd integer, datestr_dd varchar(10), weeknumber_dd integer, text_dd blob, reserved_dd varchar(512));';
  { create autobackup table }
  CreateAB = 'CREATE TABLE autobackup(id_ab integer primary key, date_ab integer, frequency_ab integer, reserved_ab varchar(512));';
  { insert in daily diary by parameters }
  InsSql = 'INSERT INTO daily_diary(id_dd,date_dd,datestr_dd,weeknumber_dd,text_dd,reserved_dd) VALUES(null,:pdate,:pdatestr,:pweekno,:ptext,:pres);';
  { insert in autobackup by parameters }
  InsSqlAB = 'INSERT INTO autobackup(id_ab,date_ab,frequency_ab,reserved_ab) VALUES(null,:pdate,:pfrequency,:pres);';
  { update daily diary table by parameters }
  UpdSql = 'UPDATE daily_diary SET date_dd=:pdate, datestr_dd=:pdatestr, weeknumber_dd=:pweekno, text_dd=:ptext, reserved_dd=:pres WHERE id_dd=:pid;';
  { delete record from daily diary table by parameter }
  DelSql = 'DELETE FROM daily_diary WHERE id_dd=:pid;';
  { select all the records in daily diary table }
  SelSqlAsc = 'SELECT * FROM daily_diary ORDER BY date_dd ASC;';
  SelSqlAB = 'SELECT * FROM autobackup;';
  SelSqlDesc = 'SELECT * FROM daily_diary ORDER BY date_dd DESC;';
  { get a hold on the last inserted id }
  LastIdSql = 'SELECT LAST_INSERT_ROWID() AS id_Last;';
  LastId_ddSql = 'SELECT id_dd FROM daily_diary;';

  BooleanText: array[boolean] of string = ('No','Yes');

  { keyboard codes }
  cBckYellow = $00C0FFFF;
  cEditYellow = $00C0FFFF; // yellowish
  cEnter = $0D;
  cEsc = $1B;
  cIns = $2D;
  cF2 = $71;
  cEdit = cF2;
  cDel = $2E;
  cCr = #10;

  { dialog results }
  DlgYes = 6;
  DlgNo = 7;

  { function result codes }
  HR_OK    = 0;
  HR_ERROR = 1;

type
  { utility }
  PInteger = ^integer;

function DD_Databasename: string;
function DD_Inifilename: string;
function DD_AdjustTrailingSlash(const S: string): string;

implementation

function DD_Databasename: string;
begin
  Result:= ExtractFilePath(paramstr(0))+'db'+DirectorySeparator+'daily_diary.db3'; // *nix ~ /, binbows ~ \
end;

function DD_Inifilename: string;
begin
  Result:= ExtractFilePath(paramstr(0))+'daily_diary.ini'
end;

function DD_AdjustTrailingSlash(const S: string): string; { 29.07.2015 bc }
var Len: integer;
begin
  Result:= S;
  if S <> '' then begin
    Len:= length(S);
    while S[Len] = '/' do dec(Len);
    SetLength(Result,Len+1); { we want just the first slash }
  end;
end;

(*
  TLongBoolHelper = Type Helper for LongBool
*)
end.


