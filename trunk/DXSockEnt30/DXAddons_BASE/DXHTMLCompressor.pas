unit DXHTMLCompressor;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXHTMLCompressor
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
   DXString,
   Classes;

{$I DXAddons.def}

type
   // Our HTML compressor uses two different concepts to produce
   // clean/compact HTML output.
   //
   //
   //     *   <B>ctSpaceOnly</B> cleans up excess spacing in the
   //         \output html file. This includes tabs and whitespaces
   //         produced by many of the GUI html editors, and
   //         scripting engines.
   //     *   <B>ctTagOnly</B> cleans up many of the long name HTML
   //         tags with tag names that are shorter, and support the
   //         same attributes. So your output will look as close to
   //         the original when rendered by a web browser.
   //     *   <B>ctFull</B> enables both the stSpaceOnly and the
   //         ctTagOnly cleansing routines. <B>DEFAULT</B>
   //
   //
   // Summary
   // Define the cleansing method used by the parser for the HTML
   // Compressor component.
   TCompressType=(ctFull, ctSpaceOnly, ctTagsOnly);

   TDXHTMLCompressor=class(TDXComponent)
   private
      // Private declarations
      CmpType:TCompressType;
      fbInPre:Boolean;
      fAsText:Boolean;
   protected
      // Protected declarations
   public
      // Public declarations
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override;
         {$ENDIF}
      destructor Destroy; override;
      {$IFDEF VER100}
      function Compress(const HTMLSrc:string):string;
      procedure CompressStringList(var HTMLSrc:TStringList);
      procedure CompressStream(HTMLSrc:TStream);
      {$ELSE}
      function Compress(const HTMLSrc:string):string; overload;
      procedure Compress(var HTMLSrc:TStringList); overload;
      procedure Compress(HTMLSrc:TStream); overload;
      {$ENDIF}
   published
      // Published declarations
      property CompressionType:TCompressType read CmpType
         write CmpType;
      property RenderAsText:Boolean read fAsText
         write fAsText;
   end;

implementation

uses
   SysUtils;

const
   CommentStart='<!--';
   CommentEnd='-->';

   ///////////////////////////////////////////////////////////////////////////////
   //CREATE:
   //       Define the mode of optimization.
   ///////////////////////////////////////////////////////////////////////////////

constructor TDXHTMLCompressor.Create(AOwner:TComponent);
begin
   inherited create(AOwner);
   CmpType:=ctFull;
   fAsText:=False;
end;

///////////////////////////////////////////////////////////////////////////////
//DESTROY:
//        Destory this object.
///////////////////////////////////////////////////////////////////////////////

destructor TDXHTMLCompressor.Destroy;
begin
   inherited Destroy;
end;

///////////////////////////////////////////////////////////////////////////////
//COMPRESS:
//         This routine applies optimization logic against the supplied string.
///////////////////////////////////////////////////////////////////////////////

function TDXHTMLCompressor.Compress(const HTMLSrc:string):string;
var
   Buf:string;
   ShortCircuit:Boolean;

begin
   ShortCircuit:=False;
   Buf:=Uppercase(HTMLSrc);
   if QuickPos('<PRE>', Buf)>0 then fbInPre:=True;
   if QuickPos('</PRE>', Buf)>0 then begin
      fbInPre:=False;
      if QuickPos('<PRE>', Buf)>0 then begin
         ShortCircuit:=True;
      end;
   end;
   Buf:=HTMLSrc;
   if Buf<>'' then begin
      if (not fbInPre)and(not ShortCircuit) then begin
         if ((CmpType=ctFull)or(CmpType=ctSpaceOnly)) then begin
            Buf:=Trim(Buf);
            if CharPos(#0, Buf)+CharPos(#9, Buf)+CharPos(#13, Buf)+CharPos(#10,
               Buf)>0 then begin
               Buf:=StringReplace(Buf, #0, #32, [rfReplaceAll]);
               Buf:=StringReplace(Buf, #9, #32, [rfReplaceAll]);
               Buf:=StringReplace(Buf, #13, #32, [rfReplaceAll]);
               Buf:=StringReplace(Buf, #10, #32, [rfReplaceAll]);
               Buf:=Trim(Buf);
            end;
            while QuickPos(#32#32, Buf)>0 do
               Buf:=StringReplace(Buf, #32#32, #32, [rfReplaceAll]);
            Buf:=StringReplace(Buf, '> <', '><', [rfReplaceAll]);
         end;
      end;
      if (fAsText) then begin
         Buf:=StringReplace(Buf, '<', '&lt;', [rfReplaceAll]);
         Buf:=StringReplace(Buf, '>', '&gt;', [rfReplaceAll]);
         Buf:=StringReplace(Buf, #32#32, '&nbsp;&nbsp;', [rfReplaceAll]);
      end
      else begin
         if ((CmpType=ctFull)or(CmpType=ctTagsOnly))and
            (CharPos('<', Buf)>0) then begin
            Buf:=StringReplace(Buf, '<dfn', '<i', [rfReplaceAll, rfIgnoreCase]);
            Buf:=StringReplace(Buf, '</dfn', '</i', [rfReplaceAll,
               rfIgnoreCase]);
            Buf:=StringReplace(Buf, '<cite', '<i', [rfReplaceAll,
               rfIgnoreCase]);
            Buf:=StringReplace(Buf, '</cite', '</i', [rfReplaceAll,
               rfIgnoreCase]);
            Buf:=StringReplace(Buf, '<code', '<tt', [rfReplaceAll,
               rfIgnoreCase]);
            Buf:=StringReplace(Buf, '</code', '</tt', [rfReplaceAll,
               rfIgnoreCase]);
            Buf:=StringReplace(Buf, '<kbd', '<tt', [rfReplaceAll,
               rfIgnoreCase]);
            Buf:=StringReplace(Buf, '</kbd', '</tt', [rfReplaceAll,
               rfIgnoreCase]);
            Buf:=StringReplace(Buf, '<samp', '<tt', [rfReplaceAll,
               rfIgnoreCase]);
            Buf:=StringReplace(Buf, '</samp', '</tt', [rfReplaceAll,
               rfIgnoreCase]);
            Buf:=StringReplace(Buf, '<var', '<i', [rfReplaceAll, rfIgnoreCase]);
            Buf:=StringReplace(Buf, '</var', '</i', [rfReplaceAll,
               rfIgnoreCase]);
            Buf:=StringReplace(Buf, '<strong', '<b', [rfReplaceAll,
               rfIgnoreCase]);
            Buf:=StringReplace(Buf, '</strong', '</b', [rfReplaceAll,
               rfIgnoreCase]);
            Buf:=StringReplace(Buf, '<em', '<i', [rfReplaceAll, rfIgnoreCase]);
            Buf:=StringReplace(Buf, '</em', '</i', [rfReplaceAll,
               rfIgnoreCase]);
            Buf:=StringReplace(Buf, '<strike', '<s', [rfReplaceAll,
               rfIgnoreCase]);
            Buf:=StringReplace(Buf, '</strike', '</s', [rfReplaceAll,
               rfIgnoreCase]);
         end;
         Buf:=StringReplace(Buf, '&middot;', '-', [rfReplaceAll, rfIgnoreCase]);
      end;
   end;
   Result:=Buf;
end;

{$IFDEF VER100}

procedure TDXHTMLCompressor.CompressStringList(var HTMLSrc:TStringList);
{$ELSE}

procedure TDXHTMLCompressor.Compress(var HTMLSrc:TStringList);
{$ENDIF}
var
   MaxLoop, Loop:Integer;
   Ws:string;
//   output:String; // 4RC2
//   Ts:String; // 4RC2

begin
   Loop:=0;
   MaxLoop:=HTMLSrc.Count;
//   Output:='';
//   Ts:='';
   while Loop<MaxLoop do begin
      Ws:=HTMLSrc[Loop];
      try
         if Ws<>'' then Ws:=Compress(Ws);
      finally
      end;
      if Ws='' then begin
         HTMLSrc.Delete(Loop);
         Dec(MaxLoop);
      end
      else begin
         HTMLSrc[Loop]:=Ws;
         Inc(Loop);
{         If fbInPre then Begin
            Output:=Output+Ws+#13#10;
         End
         Else
         If Length(Ts)<200 then Ts:=Ts+Ws
         Else Begin
            Output:=Output+Ts+#13#10;
            Ts:=Ws;
         End;}
      end;
   end;
{   Output:=Output+Ts;
   HTMLSrc.Text:=Output;}
end;

{$IFDEF VER100}
procedure TDXHTMLCompressor.CompressStream(HTMLSrc:TStream);
{$ELSE}
procedure TDXHTMLCompressor.Compress(HTMLSrc:TStream);
{$ENDIF}
var
   StrList:TStringList;

begin
   StrList:=TStringList.Create;
   StrList.LoadFromStream(HTMLSrc);
   {$IFDEF VER100}
   CompressStringList(StrList);
   {$ELSE}
   Compress(StrList);
   {$ENDIF}
   {$IFDEF VER90}
   htmlsrc.Seek(0, 0);
   {$ELSE}
   htmlsrc.Size:=0;
   {$ENDIF}
   StrList.SaveToStream(HTMLSrc);
   HTMLSrc.Seek(0, 0);
   StrList.Free;
   StrList:=nil;
end;

end.

