#pragma once
#include <Safir/Utilities/Internal/VisibilityHelpers.h>

#ifdef tracer_helper_dll_EXPORTS
#  define TRACER_HELPER_DLL_API SAFIR_HELPER_DLL_EXPORT
#else
#  define TRACER_HELPER_DLL_API SAFIR_HELPER_DLL_IMPORT
#endif

TRACER_HELPER_DLL_API void tracer_helper_enable(bool enable);
TRACER_HELPER_DLL_API void tracer_helper_say_something();
