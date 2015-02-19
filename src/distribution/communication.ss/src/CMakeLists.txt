option(JOOT "Safir dev mode" OFF)

if(NOT JOOT)
    FILE(GLOB_RECURSE headers *.h)
    FILE(GLOB sources *.cpp)

    PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS CommunicationMessage.proto)

    if (MSVC)
      SET_SOURCE_FILES_PROPERTIES(${PROTO_SRCS} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267 /wd4018")
    endif()

    ADD_LIBRARY(communication SHARED ${sources} ${headers} ${PROTO_SRCS} ${PROTO_HDRS} )

    #Make the generated protobuf headers available for us to include inside communication
    target_include_directories(communication PRIVATE ${CMAKE_CURRENT_BINARY_DIR})

    #Make our INTERNAL headers available to other parts of Core.
    target_include_directories(communication PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

    TARGET_LINK_LIBRARIES(communication PRIVATE
      lluf_id
      lluf_utils
      ${PROTOBUF_LITE_LIBRARIES})

    SAFIR_INSTALL(TARGETS communication)

else()
    cmake_minimum_required(VERSION 2.8)
    message("Joel mode")
    project(communication CXX C) #C is needed by FindProtobuf

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

    include_directories(.)
    include_directories(include)
    include_directories($ENV{SAFIR}/include)
    include_directories(../../../lluf/lluf_utils.ss/src/include)

    FILE(GLOB_RECURSE headers *.h)
    FILE(GLOB sources *.cpp)

    FIND_PACKAGE(Boost COMPONENTS thread system chrono)
    FIND_PACKAGE(Protobuf REQUIRED)

    INCLUDE_DIRECTORIES(${PROTOBUF_INCLUDE_DIRS} ${CMAKE_CURRENT_BINARY_DIR})

    PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS CommunicationMessage.proto)

    if (MSVC)
      SET_SOURCE_FILES_PROPERTIES(${PROTO_SRCS} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267 /wd4018")
    endif()

    ADD_LIBRARY(communication SHARED ${sources} ${headers} ${PROTO_SRCS} ${PROTO_HDRS} )

    TARGET_LINK_LIBRARIES(communication PRIVATE ${PROTOBUF_LITE_LIBRARIES})

    if (NOT MSVC)
        #enable c++11
        SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")
        TARGET_LINK_LIBRARIES(communication PRIVATE ${Boost_LIBRARIES})
    endif()

    #Make the generated protobuf headers available for us to include inside communication
    target_include_directories(communication PRIVATE ${CMAKE_CURRENT_BINARY_DIR})

endif()



