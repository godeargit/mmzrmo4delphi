unit DXNTPServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXNTPServerCore
//       Author: G.E. Ozz Nixon Jr. (staff@bpdx.com)
// ========================================================================
// Source Owner: DX, Inc. 1995-2003
//    Copyright: All code is the property of DX, Inc. Licensed for
//               resell by Brain Patchwork DX (tm) and part of the
//               DX (r) product lines, which are (c) 1999-2003
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
// Code Version: (4th Generation Code)
// ========================================================================
//  Description: *NOT FINISHED IN V3.0 YET*
// ========================================================================
// RFC958 - Network Time Protocol
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  TDXNTPServerCore = class(TDXServerCore)
  private
  protected
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread:TDXClientThread);
  published
  end;

implementation

Uses
   SysUtils,
   DXString;

constructor TDXNTPServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=123;
   ProtocolToBind:=wpUDPOnly;
end;

destructor TDXNTPServerCore.Destroy;
begin
  inherited Destroy;
end;

procedure TDXNTPServerCore.ProcessSession(ClientThread:TDXClientThread);
begin
   with ClientThread.Socket do
      If ValidSocket then
         Writeln('');
end;

(*
Appendix A.  UDP Header Format

   An NTP packet consists of the UDP header followed by the NTP data
   portion.  The format of the UDP header and the interpretation of its
   fields are described in [13] and are not part of the NTP
   specification.  They are shown below for completeness.

    0                   1                   2                   3
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |          Source Port          |       Destination Port        |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |            Length             |           Checksum            |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

   Source Port

      UDP source port number. In the case of unsymmetric mode and a
      client request this field is assigned by the client host, while
      for a server reply it is copied from the Destination Port field of
      the client request.  In the case of symmetric mode, both the
      Source Port and Destination Port fields are assigned the NTP
      service-port number 123.

   Destination Port

      UDP destination port number. In the case of unsymmetric mode and a
      client request this field is assigned the NTP service-port number
      123, while for a server reply it is copied form the Source Port
      field of the client request.  In the case of symmetric mode, both
      the Source Port and Destination Port fields are assigned the NTP
      service-port number 123.

   Length

      Length of the request or reply, including UDP header, in octets.

   Checksum

      Standard UDP checksum.

RFC 958                                                        September
Network Time Protocol

Appendix B.  NTP Data Format

   The format of the NTP data portion, which immediately follows the UDP
   header, is shown below along with a description of its fields.

    0                   1                   2                   3
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |LI |   Status  |      Type     |           Precision           |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                       Estimated Error                         |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                     Estimated Drift Rate                      |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                  Reference Clock Identifier                   |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               |
   |                 Reference Timestamp (64 bits)                 |
   |                                                               |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               |
   |                 Originate Timestamp (64 bits)                 |
   |                                                               |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               |
   |                  Receive Timestamp (64 bits)                  |
   |                                                               |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               |
   |                  Transmit Timestamp (64 bits)                 |
   |                                                               |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

   Leap Indicator (LI)

      Code warning of impending leap-second to be inserted at the end of
      the last day of the current month. Bits are coded as follows:

         00      no warning
         01      +1 second (following minute has 61 seconds)
         10      -1 second (following minute has 59 seconds)
         11      reserved for future use

   Status

      Code indicating status of local clock. Values are defined as
      follows:

RFC 958                                                        September
Network Time Protocol

         0       clock operating correctly
         1       carrier loss
         2       synch loss
         3       format error
         4       interface (Type 1) or link (Type 2) failure
         (additional codes reserved for future use)

   Reference Clock Type
   (Type)

      Code identifying the type of reference clock. Values are defined
      as follows:

         0       unspecified
         1       primary reference (e.g. radio clock)
         2       secondary reference using an Internet host via NTP
         3       secondary reference using some other host or protocol
         4       eyeball-and-wristwatch
         (additional codes reserved for future use)

   Precision

      Signed integer in the range +32 to -32 indicating the precision of
      the local clock, in seconds to the nearest power of two.

   Estimated Error

      Fixed-point number indicating the estimated error of the local
      clock at the time last set, in seconds with fraction point between
      bits 15 and 16.

   Estimated Drift Rate

      Signed fixed-point number indicating the estimated drift rate of
      the local clock, in dimensionless units with fraction point to the
      left of the high-order bit.

   Reference Clock
   Identifier

      Code identifying the particular reference clock. In the case of
      type 1 (primary reference), this is a left-justified, zero-filled
      ASCII string identifying the clock, for example:

         WWVB    WWVB radio clock (60 KHz)

RFC 958                                                        September
Network Time Protocol

         GOES    GOES satellite clock (468 HMz)
         WWV     WWV radio clock (2.5/5/10/15/20 MHz)
         (and others as necessary)

      In the case of type 2 (secondary reference) this is the 32-bit
      Internet address of the reference host. In other cases this field
      is reserved for future use and should be set to zero.

   Reference Timestamp

      Local time at which the local clock was last set or corrected.

   Originate Timestamp

      Local time at which the request departed the client host for the
      service host.

   Receive Timestamp

      Local time at which the request arrived at the service host.

   Transmit Timestamp

      Local time at which the reply departed the service host for the
      client host.

*)

end.

