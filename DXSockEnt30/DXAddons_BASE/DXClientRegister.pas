unit DXClientRegister;

interface

///////////////////////////////////////////////////////////////////////////////
//      Unit: DXRegister
//    Author: G.E. Ozz Nixon Jr. (administrator@delphix.com)
// Copyright: All code is the property of Brain Patchwork DX (tm) and part of
//            the DXSock (r) product, which are (c) 1999 Brain Patchwork DX.
//   Version: 2.0 (2nd Generation Code)
// ==========================================================================
///////////////////////////////////////////////////////////////////////////////

{$WARNINGS OFF}

uses
{Clients}
  DXSockClient,
  DXSockChargenClient,
  DXSockDayTimeClient,
  DXSockDiscardClient
  ;

{$I DXSOCK.DEF} {2.0}

{$IFNDEF OBJECTS_ONLY}
procedure Register;
{$ENDIF}

Implementation

{$IFDEF VER90}
   {$R DXSockD2.res}
{$ENDIF}

Uses
   Classes;

{$IFNDEF OBJECTS_ONLY}
procedure Register;
Begin
// NOT READY FOR PUBLIC RELEASE!!
{Clients}
  RegisterComponents('BPDX Clients', [
{$IFNDEF OBJECTS_ONLY}
        TDXSockClient,
{$ENDIF}
        TDXSockChargenClient,
        TDXSockDayTimeClient,
        TDXSockDiscardClient
        ]);

End;
{$ENDIF}


end.
