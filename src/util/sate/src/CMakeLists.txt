find_package(qt-advanced-docking-system CONFIG REQUIRED)

set(ui
    satemainwindow.ui
    typesystemwidget.ui
    dobobjecteditwidget.ui
    textvalueinput.ui
    comboboxvalueinput.ui
    operationswidget.ui
    registerhandlerdialog.ui
    connectdialog.ui
    outputwidget.ui
    parameterswidget.ui
    subscribedialog.ui
    enumwidget.ui
    scriptwidget.ui
    settingsdialog.ui
    fileinfowidget.ui
    textwidget.ui
)

set(sources
    main.cpp
    satemainwindow.cpp
    typesystemwidget.cpp
    dobobjecteditwidget.cpp
    dobobjectmodel.cpp
    dobobjecteditcontextmenuhandler.cpp
    typesysteminheritancemodel.cpp
    typesystemnamespacemodel.cpp
    typesystemrepository.cpp
    typesystemcontextmenuhandler.cpp
    membertreeitem.cpp
    dobhandler.cpp
    dobnative.cpp
    dobwebsocket.cpp
    iconfactory.cpp
    dobobjectdelegate.cpp
    valueinput.cpp
    operationswidget.cpp
    dobobjectbuilder.cpp
    registerhandlerdialog.cpp
    connectdialog.cpp
    instanceswidget.cpp
    instancesmodelutils.cpp
    entityinstancesmodel.cpp
    messageinstancesmodel.cpp
    columninfo.cpp
    outputwidget.cpp
    parametersmodel.cpp
    parameterswidget.cpp
    subscribedialog.cpp
    enumwidget.cpp
    enummodel.cpp
    scriptengine.cpp
    scriptwidget.cpp
    scriptmodel.cpp
    scriptcli.cpp
    typeaheadwidget.cpp
    utilities.cpp
    dobcalltojson.cpp
    settings_manager.cpp
    settingsdialog.cpp
    fileinfowidget.cpp
    textwidget.cpp
    sate.rc
  )

set(widget_headers
    satemainwindow.h
    typesystemwidget.h
    dobobjecteditwidget.h
    dobobjectmodel.h
    dobobjecteditcontextmenuhandler.h
    typesysteminheritancemodel.h
    typesystemnamespacemodel.h
    typesystemcontextmenuhandler.h
    dobhandler.h
    dobnative.h
    dobwebsocket.h
    dobinterface.h
    dobobjectdelegate.h
    valueinput.h
    operationswidget.h
    registerhandlerdialog.h
    connectdialog.h
    instanceswidget.h
    entityinstancesmodel.h
    messageinstancesmodel.h
    columninfo.h
    outputwidget.h
    parametersmodel.h
    parameterswidget.h
    subscribedialog.h
    enumwidget.h
    enummodel.h
    scriptengine.h
    scriptwidget.h
    scriptmodel.h
    scriptcli.h
    typeaheadwidget.h
    settings_manager.h
    settingsdialog.h
    fileinfowidget.h
    textwidget.h
  )

set(headers
    typesystemrepository.h
    membertreeitem.h
    iconfactory.h
    dobobjectbuilder.h
    instancesmodelutils.h
    utilities.h
    dobcalltojson.h
)

if (NOT TARGET Qt::Widgets)
  message(STATUS "Could not find Qt, will not build Qt Sate")
  return()
endif()

QT_WRAP_UI(ui_headers ${ui})
QT_WRAP_CPP(moc_sources ${widget_headers} ${QT_WRAP_CPP_OPTIONS})
QT_ADD_RESOURCES(rcc_sources
  sate_resources.qrc
  ../../style/dark/darkstyle.qrc
  ../../style/light/lightstyle.qrc
  ../../style/customizations/customizations.qrc
)

ADD_EXECUTABLE(sate WIN32 ${sources} ${ui_headers} ${moc_sources} ${headers} ${widget_headers} ${rcc_sources})

TARGET_LINK_LIBRARIES(sate PRIVATE
  douf_time_cpp
  lluf_internal
  lluf_crash_reporter
  dots_internal
  dots_kernel
  dots_cpp
  dose_internal
  dose_cpp
  qt-advanced-docking-system::qt-advanced-docking-system
  Qt::WebSockets
  Qt::Svg
  Qt::Widgets)

SAFIR_INSTALL(TARGETS sate COMPONENT Tools)
