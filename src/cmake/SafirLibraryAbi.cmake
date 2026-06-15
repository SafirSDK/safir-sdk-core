# SafirLibraryAbi.cmake - explicit C++ ABI flavor classification
#
# On Windows the MSVC debug and release runtimes are not ABI compatible. A
# consumer building their app in Debug must link against Debug-flavor copies of
# any DLL whose public interface uses C++ standard library types (std::string,
# std::vector, exceptions, ...). DLLs with a pure C ABI - and all .exe targets -
# only need a single flavor.
#
# We classify every shared library explicitly:
#
#   safir_mark_dual_abi(<target>)
#       The library exposes a C++ ABI. On MSVC it keeps the global "d" debug
#       postfix so debug and release copies coexist (foo.dll, food.dll), and
#       the Windows release pipeline builds it in BOTH configurations.
#       The target is collected on the global aggregate `safir_dual_abi_libs`.
#
#   safir_mark_single_abi(<target>)
#       The library has a C ABI, or is a JNI shim, or is otherwise consumed
#       only at runtime. One flavor is enough; we clear the "d" postfix so
#       the filename is stable across configs.
#       The target is collected on the global aggregate `safir_single_abi_libs`.
#
# The two aggregates let the Windows release workflow build only the dual-ABI
# slice in Debug, then the full tree in RelWithDebInfo - instead of building
# everything twice. See CMakePresets.json.
#
# On non-MSVC platforms these macros are essentially no-ops (no debug postfix
# is set globally) but they still register the target on the aggregate, which
# is cheap and keeps the classification visible everywhere.
#
# Two end-of-configure validations enforce the classification:
#
#   1. Every SHARED library must be classified - catches "I added a new lib
#      and forgot to call a marker."
#   2. A single-ABI library cannot expose a dual-ABI library through PUBLIC
#      or INTERFACE link - catches the case where a single-flavor binary
#      transitively forces consumers to pick a flavor of a dual-flavor dep,
#      which can't be reconciled at runtime.

if (NOT TARGET safir_dual_abi_libs)
  add_custom_target(safir_dual_abi_libs)
  add_custom_target(safir_single_abi_libs)
endif()

function(safir_mark_dual_abi target)
  get_target_property(_type ${target} TYPE)
  if(NOT _type STREQUAL "SHARED_LIBRARY")
    message(FATAL_ERROR
      "safir_mark_dual_abi(${target}): target type is ${_type}, not SHARED_LIBRARY. "
      "ABI markers only apply to shared libraries.")
  endif()
  # The "d" postfix comes from the global CMAKE_DEBUG_POSTFIX default on
  # Windows; we don't need to set it again here. The point of this call is
  # the aggregate registration plus the explicit declaration of intent.
  add_dependencies(safir_dual_abi_libs ${target})
  set_property(GLOBAL APPEND PROPERTY SAFIR_DUAL_ABI_LIBS ${target})
endfunction()

function(safir_mark_single_abi target)
  get_target_property(_type ${target} TYPE)
  if(NOT _type STREQUAL "SHARED_LIBRARY")
    message(FATAL_ERROR
      "safir_mark_single_abi(${target}): target type is ${_type}, not SHARED_LIBRARY. "
      "ABI markers only apply to shared libraries.")
  endif()
  if (MSVC)
    set_target_properties(${target} PROPERTIES DEBUG_POSTFIX "")
  endif()
  add_dependencies(safir_single_abi_libs ${target})
  set_property(GLOBAL APPEND PROPERTY SAFIR_SINGLE_ABI_LIBS ${target})
endfunction()

# Record a PUBLIC/INTERFACE link dependency for the end-of-configure ABI
# propagation check. Called from the target_link_libraries() wrapper in
# src/CMakeLists.txt. We strip $<BUILD_INTERFACE:foo> wrappers because the
# inner target name is what carries the ABI classification.
function(_safir_record_propagating_link target dep)
  if (dep MATCHES "^\\$<BUILD_INTERFACE:(.*)>$")
    set(dep "${CMAKE_MATCH_1}")
  endif()
  set_property(GLOBAL APPEND PROPERTY SAFIR_LINK_TARGETS "${target}")
  set_property(GLOBAL APPEND PROPERTY SAFIR_LINK_DEPS "${dep}")
endfunction()

function(_safir_check_abi_classification)
  get_property(_dual GLOBAL PROPERTY SAFIR_DUAL_ABI_LIBS)
  get_property(_single GLOBAL PROPERTY SAFIR_SINGLE_ABI_LIBS)
  set(_classified ${_dual} ${_single})
  _safir_collect_targets("${CMAKE_SOURCE_DIR}" _all_targets)
  set(_unclassified)
  foreach(t IN LISTS _all_targets)
    get_target_property(_type ${t} TYPE)
    get_target_property(_imported ${t} IMPORTED)
    if (_type STREQUAL "SHARED_LIBRARY" AND NOT _imported)
      list(FIND _classified ${t} _idx)
      if (_idx EQUAL -1)
        list(APPEND _unclassified ${t})
      endif()
    endif()
  endforeach()
  if (_unclassified)
    message(FATAL_ERROR
      "The following SHARED libraries are not classified by ABI flavor:\n"
      "  ${_unclassified}\n"
      "Call safir_mark_dual_abi(<target>) for libraries with a C++ public ABI,\n"
      "or safir_mark_single_abi(<target>) for C-ABI / JNI / runtime-only libs.\n"
      "See src/cmake/SafirLibraryAbi.cmake for details.")
  endif()
endfunction()

# A single-ABI library (one binary on Windows) cannot expose a dual-ABI
# library (two binaries) through PUBLIC or INTERFACE link: its single binary
# has to commit to one flavor of the dependency at build time, which then
# conflicts with the other-flavor consumer that the propagation invites.
# PRIVATE deps don't propagate, so they're fine. dual->anything and
# single->single are also fine.
function(_safir_check_abi_propagation)
  get_property(_targets GLOBAL PROPERTY SAFIR_LINK_TARGETS)
  get_property(_deps GLOBAL PROPERTY SAFIR_LINK_DEPS)
  get_property(_dual GLOBAL PROPERTY SAFIR_DUAL_ABI_LIBS)
  get_property(_single GLOBAL PROPERTY SAFIR_SINGLE_ABI_LIBS)
  if(NOT _targets)
    return()
  endif()
  list(LENGTH _targets _n)
  math(EXPR _last "${_n} - 1")
  set(_violations)
  foreach(_i RANGE 0 ${_last})
    list(GET _targets ${_i} _t)
    list(GET _deps ${_i} _d)
    list(FIND _single "${_t}" _t_is_single)
    list(FIND _dual "${_d}" _d_is_dual)
    if(NOT _t_is_single EQUAL -1 AND NOT _d_is_dual EQUAL -1)
      list(APPEND _violations "  ${_t} -> ${_d}")
    endif()
  endforeach()
  if (_violations)
    list(REMOVE_DUPLICATES _violations)
    string(REPLACE ";" "\n" _vlist "${_violations}")
    message(FATAL_ERROR
      "ABI flavor leak: single-ABI libraries expose dual-ABI deps via PUBLIC/INTERFACE link:\n"
      "${_vlist}\n"
      "Either make the dependency PRIVATE, or reclassify the source library as dual-ABI.")
  endif()
endfunction()

function(_safir_collect_targets dir out_var)
  get_property(_subs DIRECTORY ${dir} PROPERTY SUBDIRECTORIES)
  get_property(_here DIRECTORY ${dir} PROPERTY BUILDSYSTEM_TARGETS)
  set(_acc ${_here})
  foreach(_sub IN LISTS _subs)
    _safir_collect_targets(${_sub} _child)
    list(APPEND _acc ${_child})
  endforeach()
  set(${out_var} ${_acc} PARENT_SCOPE)
endfunction()

cmake_language(DEFER DIRECTORY ${CMAKE_SOURCE_DIR}
  CALL _safir_check_abi_classification)
cmake_language(DEFER DIRECTORY ${CMAKE_SOURCE_DIR}
  CALL _safir_check_abi_propagation)
