unit DXURL;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXURL
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
// This component was developed to make URI parsing easier and generating them
// accurate. Since our initial release we have spent time developing components
// to make your development tasks easier. And this is a frequent question.
//
// To make a URI call CLEAR, then define the properties, then CompileURL. To
// parse a URI to its pieces call DecompileURL.
//
// When DECOMPILING:
// The ActualPath is the path guess.
// The ActualFile is the last part of the URI path (can be a directory)
// The URIPath is the RAW Path and Filename
// The QueryString is anything after the '?'
// You will still need to EscapeDecode the QueryString to parse it!
///////////////////////////////////////////////////////////////////////////////

uses
   DXString,
   Classes;

{$I DXAddons.def}

type
   TDXURL=class(TDXComponent)
   private
      FUser:string;
      FPassword:string;
      FHost:string;
      FPort:string;
      FPath:string;
      FActualPath:string;
      FActualFile:string;
      FQueryString:string;
   protected
      { Protected declarations }
   public
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override;
      {$ENDIF}
      destructor Destroy; override;
      function CompileURL:string;
      procedure DecompileURL(URL:string);
      procedure Clear;
   published
      property User:string read FUser
         write FUser;
      property Password:string read FPassword
         write FPassword;
      property Host:string read FHost
         write FHost;
      property Port:string read FPort
         write FPort;
      property URIPath:string read FPath
         write FPath;
      property ActualPath:string read FActualPath
         write FActualPath;
      property ActualFile:string read FActualFile
         write FActualPath;
      property QueryString:string read FQueryString
         write FQueryString;
   end;

implementation

uses
   SysUtils;

constructor TDXURL.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   Clear;
end;

destructor TDXURL.Destroy;
begin
   inherited Destroy;
end;

function TDXURL.CompileURL:string;
begin
   Result:='';
   if FUser<>'' then Result:=Result+FUser;
   if FPassword<>'' then Result:=Result+':'+FPassword;
   if Result<>'' then Result:=Result+'@';
   Result:=Result+FHost;
   if FPort<>'' then Result:=Result+':'+FPort;
   Result:=Result+'/';
   if FActualPath<>'' then Result:=Result+ToUnixSlashes(FActualPath)+'/';
   if FActualFile<>'' then Result:=Result+FActualFile;
   // A future release needs to base the '?' upon the protocol. '?' is HTTP,
   // ';' is FTP, #9 is Gopher.
   if FQueryString<>'' then Result:=Result+'?'+EscapeEncode(FQueryString);
   Result:=StringReplace(Result, '//', '/', [rfReplaceAll]);
end;

procedure TDXURL.DecompileURL(URL:string);
var
   LastPos:Integer;

begin
   Clear;
   if (QuickPos('//', URL)>0)and
      (QuickPos('//', URL)<10) then Delete(URL, 1, QuickPos('//', URL)+1);
   URL:=EscapeDecode(URL);
   LastPos:=CharPos('@', URL);
   if LastPos>0 then begin
      if LastPos<CharPos(':', URL) then begin
         FUser:=Copy(URL, 1, LastPos-1);
         Delete(URL, 1, LastPos);
         LastPos:=CharPos(':', URL);
         FPassword:=Copy(URL, 1, LastPos-1);
         Delete(URL, 1, LastPos);
      end;
   end;
   LastPos:=CharPos(':', URL);
   while IsNumeric(URL[LastPos+1]) do begin
      FPort:=FPort+URL[LastPos+1];
      Inc(LastPos);
   end;
   Delete(URL, CharPos(':', URL), Length(FPort)+1);
   LastPos:=CharPos('/', URL);
   if LastPos>0 then begin
      FHost:=Copy(URL, 1, LastPos-1);
      Delete(URL, 1, LastPos);
      FPath:=URL;
      LastPos:=CharPos('?', URL);
      if LastPos>0 then begin
         FActualPath:=Copy(URL, 1, LastPos-1);
         FQueryString:=Copy(URL, LastPos+1, Length(URL));
         Delete(URL, 1, LastPos);
      end
      else FActualPath:=FPath;
      if FActualPath<>'' then begin
         FActualFile:=FActualPath;
         while CharPos('/', FActualFile)>0 do
            Delete(FActualFile, 1, CharPos('/', FActualFile));
         Delete(FActualPath, (Length(FActualPath)-Length(FActualFile)),
            Length(FActualPath));
      end;
   end
   else begin
      FHost:=URL;
      FPath:='/';
      FActualPath:='/';
   end;
end;

procedure TDXURL.Clear;
begin
   FUser:='';
   FPassword:='';
   FHost:='';
   FPort:='';
   FPath:='';
   FActualPath:='';
   FActualFile:='';
   FQueryString:='';
end;

end.

