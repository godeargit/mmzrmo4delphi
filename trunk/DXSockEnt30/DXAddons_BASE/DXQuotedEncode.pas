unit DXQuotedEncode;

interface

///////////////////////////////////////////////////////////////////////////////
//         Unit: DXQuotedEncoder
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

function QuotedEncode(const source, destination:TStream):Boolean;

implementation

uses
   SysUtils,
   DXFileBuffer;

function QuotedEncode(const source, destination:TStream):Boolean;
const
   EOL='='+#13#10;

var
   Loop:Integer;
   Ch:Char;
   LCtr:Integer;
   Str:string;

begin
   Destination.Size:=0;
   Source.Seek(0, 0);
   LCtr:=0;
   for Loop:=1 to Source.Size do begin
      Source.Read(Ch, 1);
      if (Ch<#32)or(ch='=')or
         ((Ch<#65)and(Ch>#61))or
         (Ch>#126) then begin
         if LCtr>65 then begin
            LCtr:=72-LCtr;
            Str:='';
            while LCtr>0 do begin
               Str:=Str+#32;
               Dec(LCtr);
            end;
            Str:=Str+EOL;
            Destination.Write(Str[1], Length(Str));
            LCtr:=0;
         end
         else
            Inc(LCtr, 3);
         Str:='='+IntToHex(Ord(Ch), 2);
         Destination.Write(Str[1], Length(Str));
         if LCtr>71 then begin
            Str:=EOL;
            Destination.Write(Str[1], Length(Str));
            LCtr:=0;
         end;
      end
      else begin
         Inc(LCtr);
         Destination.Write(Ch, 1);
         if LCtr>71 then begin
            Str:=EOL;
            Destination.Write(Str[1], Length(Str));
            LCtr:=0;
         end;
      end;
   end;
   Destination.Seek(0, 0);
   Result:=True;
end;

end.

