find_package(qt-advanced-docking-system CONFIG REQUIRED)

set(ui
    satemainwindow.ui
    typesystemwidget.ui
    dobobjecteditwidget.ui
    textvalueinput.ui
    comboboxvalueinput.ui
    operationswidget.ui
    registerhandlerdialog.ui
)

set(sources
    main.cpp
    satemainwindow.cpp
    typesystemwidget.cpp
    dobobjecteditwidget.cpp
    dobobjectmodel.cpp
    typesysteminheritancemodel.cpp
    typesystemnamespacemodel.cpp
    typesystemrepository.cpp
    typesystemcontextmenuhandler.cpp
    membertreeitem.cpp
    dobhandler.cpp
    iconfactory.cpp
    receivedmodel.cpp
    dobobjectdelegate.cpp
    valueinput.cpp
    operationswidget.cpp
    dobobjectbuilder.cpp
    registerhandlerdialog.cpp
    qdarkstyle/dark/darkstyle.qss
    qdarkstyle/dark/style.qss
    qdarkstyle/dark/ads.qss
)

set(widget_headers
    satemainwindow.h
    typesystemwidget.h
    dobobjecteditwidget.h
    dobobjectmodel.h
    typesysteminheritancemodel.h
    typesystemnamespacemodel.h
    typesystemcontextmenuhandler.h
    dobhandler.h
    dobinterface.h
    receivedmodel.h
    dobobjectdelegate.h
    valueinput.h
    operationswidget.h
    registerhandlerdialog.h
)

set(headers
    typesystemrepository.h
    membertreeitem.h
    iconfactory.h
    dobobjectbuilder.h
)

if (NOT TARGET Qt::Widgets)
  message(STATUS "Could not find Qt, will not build sate_qt")
  return()
endif()

QT_WRAP_UI(ui_headers ${ui})
QT_WRAP_CPP(moc_sources ${widget_headers} ${QT_WRAP_CPP_OPTIONS})
QT_ADD_RESOURCES(rcc_sources sate_resources.qrc qdarkstyle/dark/darkstyle.qrc)

ADD_EXECUTABLE(sate_qt WIN32 ${sources} ${ui_headers} ${moc_sources} ${headers} ${widget_headers} ${rcc_sources})

TARGET_LINK_LIBRARIES(sate_qt PRIVATE
  lluf_internal
  lluf_crash_reporter
  dots_internal
  dots_kernel
  dots_cpp
  dose_internal
  dose_cpp
  qt-advanced-docking-system::qt-advanced-docking-system
  Qt::Widgets)

SAFIR_INSTALL(TARGETS sate_qt COMPONENT Tools)
