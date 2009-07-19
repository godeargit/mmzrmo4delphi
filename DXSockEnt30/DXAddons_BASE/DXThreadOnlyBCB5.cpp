//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("DXThreadOnlyBCB5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("DXThreads4Components.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
