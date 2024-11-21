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
    instanceswidget.cpp
    entityinstancesmodel.cpp
    messageinstancesmodel.cpp
    columninfo.cpp
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
    instanceswidget.h
    entityinstancesmodel.h
    messageinstancesmodel.h
    columninfo.h
  )

set(headers
    typesystemrepository.h
    membertreeitem.h
    iconfactory.h
    dobobjectbuilder.h
)

if (NOT TARGET Qt::Widgets)
  message(STATUS "Could not find Qt, will not build Qt Sate")
  return()
endif()

QT_WRAP_UI(ui_headers ${ui})
QT_WRAP_CPP(moc_sources ${widget_headers} ${QT_WRAP_CPP_OPTIONS})
QT_ADD_RESOURCES(rcc_sources
  sate_resources.qrc
  style/dark/darkstyle.qrc
  style/light/lightstyle.qrc
  style/customizations/customizations.qrc
)

ADD_EXECUTABLE(sate_next WIN32 ${sources} ${ui_headers} ${moc_sources} ${headers} ${widget_headers} ${rcc_sources})

TARGET_LINK_LIBRARIES(sate_next PRIVATE
  douf_time_cpp
  lluf_internal
  lluf_crash_reporter
  dots_internal
  dots_kernel
  dots_cpp
  dose_internal
  dose_cpp
  qt-advanced-docking-system::qt-advanced-docking-system
  Qt::Widgets)

SAFIR_INSTALL(TARGETS sate_next COMPONENT Tools)
