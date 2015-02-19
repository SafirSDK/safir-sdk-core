option(JOOT "Safir dev mode" OFF)

if(NOT JOOT)
    ADD_EXECUTABLE(communication_test communication_test.cpp)

    TARGET_LINK_LIBRARIES(communication_test PRIVATE
      communication
      lluf_id
      ${Boost_PROGRAM_OPTIONS_LIBRARY}
      ${Boost_THREAD_LIBRARY}
      ${Boost_SYSTEM_LIBRARY})

    SAFIR_INSTALL(TARGETS communication_test TEST_SUITE)

else()    
    cmake_minimum_required(VERSION 2.8)
    message("Joel mode")
    project(communication_test CXX) #C is needed by FindProtobuf

    #------------------------------------------------
    # BOOST STUFF
    #------------------------------------------------
    #use dynamic linking with boost
    ADD_DEFINITIONS(-DBOOST_ALL_DYN_LINK)

    #disable deprecated functionality that we don't want
    ADD_DEFINITIONS(-DBOOST_FILESYSTEM_NO_DEPRECATED)
    ADD_DEFINITIONS(-DBOOST_SYSTEM_NO_DEPRECATED)

    #we want to use boost::chrono instead of std::chrono and date_time for threads and asio
    ADD_DEFINITIONS(-DBOOST_ASIO_DISABLE_STD_CHRONO)
    ADD_DEFINITIONS(-DBOOST_THREAD_DONT_USE_DATETIME)

    #Make Boost.Chrono header-only
    ADD_DEFINITIONS(-DBOOST_CHRONO_HEADER_ONLY)

    #Make sure we only use the header-only part of Boost.DateTime
    #on non microsoft compilers/platforms
    if(NOT MSVC)
      ADD_DEFINITIONS(-DBOOST_DATE_TIME_NO_LIB)
    endif()

    #Set up boost for any test code (i.e. CheckCXXSourceCompiles stuff)
    set(CMAKE_REQUIRED_INCLUDES ${Boost_INCLUDE_DIRS})
    set(CMAKE_REQUIRED_DEFINITIONS
      -DBOOST_ALL_DYN_LINK
      -DBOOST_FILESYSTEM_NO_DEPRECATED
      -DBOOST_SYSTEM_NO_DEPRECATED
      -DBOOST_ASIO_DISABLE_STD_CHRONO
      -DBOOST_THREAD_DONT_USE_DATETIME
      -DBOOST_CHRONO_HEADER_ONLY
      -DBOOST_DATE_TIME_NO_LIB)
    #--------- END BOOST ------------------------

    include_directories(../src/include)
    include_directories($ENV{SAFIR}/include)

    FILE(GLOB_RECURSE headers *.h)
    FILE(GLOB sources *.cpp)

    LINK_DIRECTORIES(../src-build)
    LINK_DIRECTORIES($ENV{SAFIR}/lib)

    FIND_PACKAGE(Boost COMPONENTS thread system chrono program_options)

    ADD_EXECUTABLE(communication_test ${sources} ${headers})

    if (NOT MSVC)
        #enable c++11
        SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")
    endif()

    TARGET_LINK_LIBRARIES(communication_test PRIVATE
      communication
      lluf_id
      ${Boost_LIBRARIES})

endif()

