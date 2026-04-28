#ifdef _WIN32
#include <windows.h>

// This DLL exists solely to keep DLL_THREAD_ATTACH notifications active.
// By NOT calling DisableThreadLibraryCalls, any new thread created while the
// loader lock is held (e.g. during another DLL's static initialization) will
// need the loader lock to run DLL_THREAD_ATTACH for this DLL - reproducing the
// static-tracer deadlock seen in real applications.
BOOL WINAPI DllMain(HINSTANCE, DWORD fdwReason, LPVOID)
{
    switch (fdwReason)
    {
    case DLL_PROCESS_ATTACH:
        // Intentionally NOT calling DisableThreadLibraryCalls
        break;
    case DLL_THREAD_ATTACH:
        break;
    case DLL_THREAD_DETACH:
        break;
    case DLL_PROCESS_DETACH:
        break;
    }
    return TRUE;
}
#else
// Nothing needed on Linux - the loader lock issue is Windows-specific
void thread_attach_dll_dummy() {}
#endif
