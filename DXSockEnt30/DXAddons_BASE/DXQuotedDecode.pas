unit DXQuotedDecode;

interface

///////////////////////////////////////////////////////////////////////////////
//         Unit: DXQuotedDecode
//       Author: G.E. Ozz Nixon Jr. (onixon@dxsock.com)
// ========================================================================
// Source Owner: DX, Inc. 1995-2002
//    Copyright: All code is the property of DX, Inc. Licensed for
//               resell by Brain Patchwork DX (tm) and part of the
//               DX (r) product lines, which are (c) 1999-2002
//               DX, Inc. Source may not be distributed without
//               written permission from both Brain Patchwork DX,
//               and DX, Inc.
//      License: (Reminder), None of this code can be added to other
//               developer products without permission. This includes
//               but not limited to DCU's, DCP's, DLL's, OCX's, or
//               any other form of merging our technologies. All of
//               your products released to a public consumer be it
//               shareware, freeware, commercial, etc. must contain a
//               license notification somewhere visible in the
//               application.
//               Example is Internet Explorer - Help->About screen
//               shows the licensed code contained in the application.
// Code Version: (3rd Generation Code)
// ========================================================================
//  Description:
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
   Classes;

{$I DXAddons.def}

function QuotedDecode(const source, destination:TStream):Boolean;

implementation

uses
   SysUtils,
   DXFileBuffer;

function QuotedDecode(const source, destination:TStream):Boolean;
var
   Loop:Integer;
   Ch:Char;
   Magic:Boolean;
   EOL:Boolean;
   Str:string;

begin
   Destination.Size:=0;
   Source.Seek(0, 0);
   Magic:=False;
   EOL:=False;
   Str:='';
   for Loop:=1 to Source.Size do begin
      Source.Read(Ch, 1);
      if Ch='=' then begin
         Magic:=True;
         Eol:=False;
      end;
      if not Magic then
         Destination.Write(Ch, 1)
      else begin
         if Ch<>'=' then begin
            if Ch<#32 then
               EOL:=True
            else begin
               if not EOL then begin
                  Str:=Str+Ch;
                  if Length(Str)=2 then begin
                     Str:='$'+Str;
                     Str:=Char(StrToInt(Str));
                     Destination.Write(Str[1], 1);
                     Magic:=False;
                     Str:='';
                  end;
               end
               else begin
                  if Ch>#31 then begin
                     Destination.Write(Ch, 1);
                     EOL:=False;
                     Magic:=False;
                  end;
               end;
            end;
         end;
      end;
   end;
   Destination.Seek(0, 0);
   Result:=True;
end;

end.

