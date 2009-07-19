unit DXCGI;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXCGI
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
//  Description: Implementation of Common Gateway Interface Shells for HTTP
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
   DXHTTPHeaderTools,
   Classes;

{$I DXAddons.def}

{$WARNINGS OFF}
{$HINTS OFF}
type
   {$IFDEF OBJECTS_ONLY}
   // TDXCGI allows your servers to spawn a CGI (Common Gateway
   // Interface) application. Automatically passing standard input
   // and the required information via environmental variables to
   // the application. And automatically receiving the standard
   // \output and standard error.
   //
   //
   //
   // Summary
   // Support the execution of a Common Gatewaye Interface
   // application from yuor application.
   TDXCGI=class
      {$ELSE}
   TDXCGI=class(TComponent)
      {$ENDIF}
   private
      // Private declarations
   protected
      // Protected declarations
   public
      // Public declarations
      {$IFDEF OBJECTS_ONLY}
      constructor Create;
      {$ELSE}
      constructor Create(AOwner:TComponent); override;
      {$ENDIF}
      destructor Destroy; override;
      // <B>CGIProgram</B> should be set the full program name,
      // including extension. e.g. welcome.exe<B>
      //
      //
      //
      // PathToCGI</B> should be set to the full qualified path, where
      // the CGIProgram resides.<B>
      //
      //
      //
      // HeaderInfo</B> (optional) should be set to the PHeaderInfo
      // from the HTTP Server Core if this is a web based application.
      // Set to NIL otherwise.<B>
      //
      //
      //
      // \Output</B> will contain the standard output from the
      // application, even if it is a graphical file.<B>
      //
      //
      //
      // ErrorMsg</B> will contain the standard error output from the
      // application - if an error has occured.
      //
      //
      //
      // \Returns:
      //
      // True if the CGI application has been successfully called,
      // executed and returned with no operating system error. This
      // does not mean the CGI successfully ran as planned.
      //
      //
      //
      // False if the CGI application was not found, was a GUI
      // application, returned an operating system error. In this case
      // refer to the ErrorMsg variable.
      //
      //
      //
      // \See Also
      //
      // <LINK TDXCGI.ExecuteGUIScript, ExecuteGUIScript>
      //
      //
      //
      // Summary
      // Execute a standard CGI application.
      function ExecuteScript(const CGIProgram, PathToCGI:string;
         HeaderInfo:PHeaderInfo; var Output, ErrorMsg:string):Boolean;
      // <B>CGIProgram</B> should be set the full program name,
      // including extension. e.g. welcome.exe<B>
      //
      //
      //
      // PathToCGI</B> should be set to the full qualified path, where
      // the CGIProgram resides.<B>
      //
      //
      //
      // HeaderInfo</B> (optional) should be set to the PHeaderInfo
      // from the HTTP Server Core if this is a web based application.
      // Set to NIL otherwise.<B>
      //
      //
      //
      // \Output</B> will contain the standard output from the
      // application, even if it is a graphical file.<B>
      //
      //
      //
      // ErrorMsg</B> will contain the standard error output from the
      // application - if an error has occured.
      //
      //
      //
      // \Returns:
      //
      // True if the CGI application has been successfully called,
      // executed and returned with no operating system error. This
      // does not mean the CGI successfully ran as planned.
      //
      //
      //
      // False if the CGI application was not found, returned an
      // operating system error. In this case refer to the ErrorMsg
      // variable.<B>
      //
      //
      //
      // \Note</B> this call ignores any GUI related errors. As some
      // CGI engines supported by the Borland compilers are based upon
      // TForm, and thus report as a GUI application, even though they
      // are CGI applications.
      //
      //
      //
      // \See Also
      //
      // <LINK TDXCGI.ExecuteScript, ExecuteScript>
      //
      //
      //
      // Summary
      // Execute a GUI based CGI application.
      function ExecuteGUIScript(const CGIProgram, PathToCGI:string;
         HeaderInfo:PHeaderInfo; var Output, ErrorMsg:string):Boolean;
   published
   end;

implementation

uses
   Windows,
   DXString,
   SysUtils;

type
   TPipeReadStdThread=class(TThread)
      Error:Boolean;
      HPipe:DWORD;
      s:string;
      procedure Execute; override;
   end;

   TPipeWriteStdThread=class(TThread)
      HPipe:DWORD;
      s:string;
      procedure Execute; override;
   end;

   TPipeReadErrThread=class(TThread)
      HPipe:DWORD;
      s:string;
      procedure Execute; override;
   end;

procedure TPipeWriteStdThread.Execute;
var
   j:DWORD;
   slen:Integer;

begin
   slen:=Length(s);
   if slen>0 then
      WriteFile(HPipe, s[1], slen, j, nil);
end;

procedure TPipeReadErrThread.Execute;
var
   ss:ShortString;
   j:DWORD;

begin
   repeat
      if not ReadFile(HPipe, ss[1], 250, j, nil) then Break;
      ss[0]:=Char(j);
      s:=s+ss;
   until Terminated;
end;

procedure TPipeReadStdThread.Execute;
var
   j:DWORD;
   ss:ShortString;

begin
   repeat
      if not ReadFile(HPipe, ss[1], 250, j, nil) then Break;
      ss[0]:=Char(j);
      s:=s+ss;
   until Terminated;
   j:=GetLastError
end;

{$IFDEF OBJECTS_ONLY}

constructor TDXCGI.Create;
{$ELSE}

constructor TDXCGI.Create(AOwner:TComponent);
{$ENDIF}
begin
   {$IFDEF OBJECTS_ONLY}
   inherited Create;
   {$ELSE}
   inherited Create(AOwner);
   {$ENDIF}
end;

destructor TDXCGI.Destroy;
begin
   inherited Destroy;
end;

function TDXCGI.ExecuteScript(const CGIProgram, PathToCGI:string;
   HeaderInfo:PHeaderInfo; var Output, ErrorMsg:string):Boolean;
var
   Security:TSecurityAttributes;
   StdIn_Read, StdIn_Write:THandle;
   StdOut_Read, StdOut_Write:THandle;
   StdErr_Read, StdErr_Write:THandle;
   StartupInfo:TStartupInfo;
   Status:Boolean;
   ProcessInformation:TProcessInformation;
   PipeReadStdThread:TPipeReadStdThread;
   PipeWriteStdThread:TPipeWriteStdThread;
   PipeReadErrThread:TPipeReadErrThread;
   Actually:DWORD;
   ActualIn:ShortString;

   function GetEnvStr:string;
   var
      AuxS:string;
      p:PByteArray;
      j:Integer;

      procedure Add(const Name, Value:string);
      begin
         if Value<>'' then Result:=Result+Name+'='+Value+#0;
      end;

   begin
      p:=Pointer(GetEnvironmentStrings);
      j:=0;
      while (p^[j]<>0)or(p^[j+1]<>0) do
         Inc(j);
      Inc(j);
      SetLength(Result, j);
      Move(p^, Result[1], j);
      FreeEnvironmentStrings(Pointer(p));
      if Assigned(HeaderInfo) then begin
         AuxS:=ToUnixSlashes(AddbackSlash(PathToCGI));
         Add('PATH_INFO', AuxS);
         if AuxS<>'' then AuxS:=AddbackSlash(PathToCGI);
         Add('PATH_TRANSLATED', AuxS);
         Add('REMOTE_HOST', HeaderInfo^.ClientHost);
         Add('REMOTE_ADDR', HeaderInfo^.ClientAddr);
         Add('GATEWAY_INTERFACE', 'CGI/1.1');
         Add('SCRIPT_NAME', HeaderInfo^.URI);
         Add('REQUEST_METHOD', HeaderInfo^.Method);
         Add('HTTP_ACCEPT', HeaderInfo^.Accept);
         Add('HTTP_ACCEPT_CHARSET', HeaderInfo^.AcceptCharset);
         Add('HTTP_ACCEPT_ENCODING', HeaderInfo^.AcceptEncoding);
         Add('HTTP_ACCEPT_LANGUAGE', HeaderInfo^.AcceptLanguage);
         Add('HTTP_FROM', HeaderInfo^.From);
         Add('HTTP_HOST', HeaderInfo^.Host);
         Add('HTTP_REFERER', HeaderInfo^.Referer);
         Add('HTTP_USER_AGENT', HeaderInfo^.UserAgent);
         Add('HTTP_COOKIE', HeaderInfo^.Cookie);
         Add('QUERY_STRING', HeaderInfo^.QueryString);
         Add('SERVER_SOFTWARE', 'DXSock 3.0');
         Add('SERVER_NAME', 'Powered by DXSock');
         Add('SERVER_PROTOCOL', HeaderInfo^.Protocol);
         Add('CONTENT_TYPE', HeaderInfo^.ContentType);
         Add('CONTENT_LENGTH', IntToStr(HeaderInfo^.ContentLength));
         Add('USER_NAME', HeaderInfo^.AuthName);
         Add('USER_PASSWORD', HeaderInfo^.AuthPass);
         Add('AUTH_TYPE', HeaderInfo^.AuthType);
      end;
   end;

begin
   Result:=False;
   with Security do begin
      nLength:=SizeOf(TSecurityAttributes);
      lpSecurityDescriptor:=nil;
      bInheritHandle:=True;
   end;
   CreatePipe(StdIn_Read, StdIn_Write, @Security, 0);
   CreatePipe(StdOut_Read, StdOut_Write, @Security, 0);
   CreatePipe(StdErr_Read, StdErr_Write, @Security, 0);
   FillChar(StartupInfo, SizeOf(StartupInfo), 0);
   with StartupInfo do begin
      CB:=SizeOf(TStartupInfo);
      dwFlags:=STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      hStdInput:=StdIn_Read;
      hStdOutput:=StdOut_Write;
      hStdError:=StdErr_Write;
      wShowWindow:=SW_HIDE;
   end;
   Status:=CreateProcess(nil, PChar(CGIProgram), @Security, @Security, True,
      CREATE_SUSPENDED, PChar(GetEnvStr), PChar(PathToCGI), StartupInfo,
      ProcessInformation);
   if Status then begin
      if WaitForInputIdle(ProcessInformation.hProcess, 0)=WAIT_TIMEOUT then begin
         ErrorMsg:=CGIProgram+' is a GUI application!';
         TerminateProcess(ProcessInformation.hProcess, 0);
         CloseHandle(ProcessInformation.hThread);
         CloseHandle(ProcessInformation.hProcess);
      end;
   end
   else begin
      ErrorMsg:=SysErrorMessage(GetLastError);
      CloseHandle(StdIn_Read);
      CloseHandle(StdIn_Write);
      CloseHandle(StdOut_Read);
      CloseHandle(StdOut_Write);
      CloseHandle(StdErr_Read);
      CloseHandle(StdErr_Write);
      Exit;
   end;
   if Assigned(HeaderInfo) then begin
      if HeaderInfo.QueryString='' then begin
         PipeWriteStdThread:=nil;
      end
      else begin
         PipeWriteStdThread:=TPipeWriteStdThread.Create(True);
         PipeWriteStdThread.s:=HeaderInfo^.QueryString;
         PipeWriteStdThread.HPipe:=Stdin_Write;
         PipeWriteStdThread.Suspended:=False;
      end;
   end
   else begin
      PipeWriteStdThread:=nil;
   end;
   PipeReadErrThread:=TPipeReadErrThread.Create(True);
   PipeReadErrThread.HPipe:=StdErr_Read;
   PipeReadErrThread.Suspended:=False;
   PipeReadStdThread:=TPipeReadStdThread.Create(True);
   PipeReadStdThread.Priority:=tpLower;
   PipeReadStdThread.S:='';
   PipeReadStdThread.HPipe:=StdOut_Read;
   PipeReadStdThread.Suspended:=False;
   ResumeThread(ProcessInformation.hThread);
   WaitForSingleObject(ProcessInformation.hProcess, 50000);
   CloseHandle(ProcessInformation.hThread);
   CloseHandle(ProcessInformation.hProcess);
   CloseHandle(StdIn_Read);
   if PipeWriteStdThread<>nil then begin
      WaitForSingleObject(PipeWriteStdThread.Handle, INFINITE);
      PipeWriteStdThread.Terminate;
      PipeWriteStdThread.Free;
      PipeWriteStdThread:=nil;
   end;
   CloseHandle(StdIn_Write);
   CloseHandle(StdErr_Write);
   PipeReadErrThread.Terminate;
   WaitForSingleObject(PipeReadErrThread.Handle, INFINITE);
   ErrorMsg:=PipeReadErrThread.s;
   PipeReadErrThread.Free;
   PipeReadErrThread:=nil;
   CloseHandle(StdErr_Read);

   PipeReadStdThread.Terminate;
   CloseHandle(StdOut_Write);
   WaitForSingleObject(PipeReadStdThread.Handle, INFINITE);

   while not PipeReadStdThread.Error do begin
      if ReadFile(Stdout_READ, ActualIn[1], 250, Actually, nil) then begin
         ActualIn[0]:=Chr(Actually);
         PipeReadStdThread.s:=PipeReadStdThread.s+ActualIn;
      end
      else
         Break;
   end;
   CloseHandle(Stdout_Read);
   Output:=PipeReadStdThread.s;
   PipeReadStdThread.Free;
   PipeReadStdThread:=nil;
   Result:=True;
end;

function TDXCGI.ExecuteGUIScript(const CGIProgram, PathToCGI:string;
   HeaderInfo:PHeaderInfo; var Output, ErrorMsg:string):Boolean;
var
   Security:TSecurityAttributes;
   StdIn_Read, StdIn_Write:THandle;
   StdOut_Read, StdOut_Write:THandle;
   StdErr_Read, StdErr_Write:THandle;
   StartupInfo:TStartupInfo;
   Status:Boolean;
   ProcessInformation:TProcessInformation;
   PipeReadStdThread:TPipeReadStdThread;
   PipeWriteStdThread:TPipeWriteStdThread;
   PipeReadErrThread:TPipeReadErrThread;
   Actually:DWORD;
   ActualIn:ShortString;

   function GetEnvStr:string;
   var
      AuxS:string;
      p:PByteArray;
      j:Integer;

      procedure Add(const Name, Value:string);
      begin
         if Value<>'' then Result:=Result+Name+'='+Value+#0;
      end;

   begin
      p:=Pointer(GetEnvironmentStrings);
      j:=0;
      while (p^[j]<>0)or(p^[j+1]<>0) do
         Inc(j);
      Inc(j);
      SetLength(Result, j);
      Move(p^, Result[1], j);
      FreeEnvironmentStrings(Pointer(p));
      if Assigned(HeaderInfo) then begin
         AuxS:=ToUnixSlashes(AddbackSlash(PathToCGI));
         Add('PATH_INFO', AuxS);
         if AuxS<>'' then AuxS:=AddbackSlash(PathToCGI);
         Add('PATH_TRANSLATED', AuxS);
         Add('REMOTE_HOST', HeaderInfo^.ClientHost);
         Add('REMOTE_ADDR', HeaderInfo^.ClientAddr);
         Add('GATEWAY_INTERFACE', 'CGI/1.1');
         Add('SCRIPT_NAME', HeaderInfo^.URI);
         Add('REQUEST_METHOD', HeaderInfo^.Method);
         Add('HTTP_ACCEPT', HeaderInfo^.Accept);
         Add('HTTP_ACCEPT_CHARSET', HeaderInfo^.AcceptCharset);
         Add('HTTP_ACCEPT_ENCODING', HeaderInfo^.AcceptEncoding);
         Add('HTTP_ACCEPT_LANGUAGE', HeaderInfo^.AcceptLanguage);
         Add('HTTP_FROM', HeaderInfo^.From);
         Add('HTTP_HOST', HeaderInfo^.Host);
         Add('HTTP_REFERER', HeaderInfo^.Referer);
         Add('HTTP_USER_AGENT', HeaderInfo^.UserAgent);
         Add('HTTP_COOKIE', HeaderInfo^.Cookie);
         Add('QUERY_STRING', HeaderInfo^.QueryString);
         Add('SERVER_SOFTWARE', 'DXSock 3.0');
         Add('SERVER_NAME', 'Powered by DXSock');
         Add('SERVER_PROTOCOL', HeaderInfo^.Protocol);
         Add('CONTENT_TYPE', HeaderInfo^.ContentType);
         Add('CONTENT_LENGTH', IntToStr(HeaderInfo^.ContentLength));
         Add('USER_NAME', HeaderInfo^.AuthName);
         Add('USER_PASSWORD', HeaderInfo^.AuthPass);
         Add('AUTH_TYPE', HeaderInfo^.AuthType);
      end;
   end;

begin
   Result:=False;
   with Security do begin
      nLength:=SizeOf(TSecurityAttributes);
      lpSecurityDescriptor:=nil;
      bInheritHandle:=True;
   end;
   CreatePipe(StdIn_Read, StdIn_Write, @Security, 0);
   CreatePipe(StdOut_Read, StdOut_Write, @Security, 0);
   CreatePipe(StdErr_Read, StdErr_Write, @Security, 0);
   FillChar(StartupInfo, SizeOf(StartupInfo), 0);
   with StartupInfo do begin
      CB:=SizeOf(TStartupInfo);
      dwFlags:=STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      hStdInput:=StdIn_Read;
      hStdOutput:=StdOut_Write;
      hStdError:=StdErr_Write;
      wShowWindow:=SW_HIDE;
   end;
   Status:=CreateProcess(nil, PChar(CGIProgram), @Security, @Security, True,
      CREATE_SUSPENDED, PChar(GetEnvStr), PChar(PathToCGI), StartupInfo,
      ProcessInformation);
   if not Status then begin
      ErrorMsg:=SysErrorMessage(GetLastError);
      CloseHandle(StdIn_Read);
      CloseHandle(StdIn_Write);
      CloseHandle(StdOut_Read);
      CloseHandle(StdOut_Write);
      CloseHandle(StdErr_Read);
      CloseHandle(StdErr_Write);
      Exit;
   end;
   if Assigned(HeaderInfo) then begin
      if HeaderInfo.QueryString='' then begin
         PipeWriteStdThread:=nil;
      end
      else begin
         PipeWriteStdThread:=TPipeWriteStdThread.Create(True);
         PipeWriteStdThread.s:=HeaderInfo^.QueryString;
         PipeWriteStdThread.HPipe:=Stdin_Write;
         PipeWriteStdThread.Suspended:=False;
      end;
   end
   else begin
      PipeWriteStdThread:=nil;
   end;
   PipeReadErrThread:=TPipeReadErrThread.Create(True);
   PipeReadErrThread.HPipe:=StdErr_Read;
   PipeReadErrThread.Suspended:=False;
   PipeReadStdThread:=TPipeReadStdThread.Create(True);
   PipeReadStdThread.Priority:=tpLower;
   PipeReadStdThread.S:='';
   PipeReadStdThread.HPipe:=StdOut_Read;
   PipeReadStdThread.Suspended:=False;
   ResumeThread(ProcessInformation.hThread);
   WaitForSingleObject(ProcessInformation.hProcess, 50000);
   CloseHandle(ProcessInformation.hThread);
   CloseHandle(ProcessInformation.hProcess);
   CloseHandle(StdIn_Read);
   if PipeWriteStdThread<>nil then begin
      WaitForSingleObject(PipeWriteStdThread.Handle, INFINITE);
      PipeWriteStdThread.Terminate;
      PipeWriteStdThread.Free;
      PipeWriteStdThread:=nil;
   end;
   CloseHandle(StdIn_Write);
   CloseHandle(StdErr_Write);
   PipeReadErrThread.Terminate;
   WaitForSingleObject(PipeReadErrThread.Handle, INFINITE);
   ErrorMsg:=PipeReadErrThread.s;
   PipeReadErrThread.Free;
   PipeReadErrThread:=nil;
   CloseHandle(StdErr_Read);
   PipeReadStdThread.Terminate;
   CloseHandle(StdOut_Write);
   WaitForSingleObject(PipeReadStdThread.Handle, INFINITE);
   while not PipeReadStdThread.Error do begin
      if ReadFile(Stdout_READ, ActualIn[1], 250, Actually, nil) then begin
         ActualIn[0]:=Chr(Actually);
         PipeReadStdThread.s:=PipeReadStdThread.s+ActualIn;
      end
      else
         Break;
   end;
   CloseHandle(Stdout_Read);
   Output:=PipeReadStdThread.s;
   PipeReadStdThread.Free;
   PipeReadStdThread:=nil;
   Result:=True;
end;

end.

