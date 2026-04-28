#ifdef _WIN32
#  define TRACER_DLL_WRAPPER_EXPORT __declspec(dllexport)
#else
#  define TRACER_DLL_WRAPPER_EXPORT
#endif

// Thin wrapper DLL whose sole purpose is to make tracer_helper_dll.dll
// two levels deep in the dependency graph (exe -> wrapper -> tracer_helper_dll),
// which causes LdrpInitializeGraphRecurse to recurse and activates the
// loader initialization event that triggers the static-tracer deadlock.
TRACER_DLL_WRAPPER_EXPORT void tracer_dll_wrapper_dummy() {}
