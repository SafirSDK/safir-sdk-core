add_subdirectory(swre_dou.ss)
add_subdirectory(swre_library.ss/src)
add_subdirectory(swre_cpp.ss/src)
#add_subdirectory(swre_dotnet.ss/src               RelWithDebInfo
add_subdirectory(swre_java.ss/src)
#add_subdirectory(swre_ada.ss/                     RelWithDebInfo
add_subdirectory(swre_backdoor.ss/src)

#swre tests need dope, so we can't run it until now
#run_test_suite   src/swre/swre_test.ss                     swre_test
