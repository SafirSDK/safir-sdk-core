/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
*
* Created by: Lars Hagström
*
*******************************************************************************
*
* This file is part of Safir SDK Core.
*
* Safir SDK Core is free software: you can redistribute it and/or modify
* it under the terms of version 3 of the GNU General Public License as
* published by the Free Software Foundation.
*
* Safir SDK Core is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#pragma once

#include <QMainWindow>
#include <QTimer>
#include <functional>
#include <boost/asio/io_context.hpp>
#include <thread>
#include <memory>
#include <Safir/Dob/Internal/ControlInfo.h>

#include <Safir/Dob/Connection.h>
#include "highlight_rule.h"
#include "settings_manager.h"

#ifdef _MSC_VER
#  pragma warning(push)
#  pragma warning (disable: 4275)
#endif

#if (QT_VERSION >= QT_VERSION_CHECK(6, 0, 0))
#  include <qtadvanceddocking-qt6/DockManager.h>
#  include <qtadvanceddocking-qt6/DockAreaWidget.h>
#  include <qtadvanceddocking-qt6/DockWidgetTab.h>
#  include <qtadvanceddocking-qt6/DockSplitter.h>
#else
#  include <qtadvanceddocking-qt5/DockManager.h>
#  include <qtadvanceddocking-qt5/DockAreaWidget.h>
#  include <qtadvanceddocking-qt5/DockWidgetTab.h>
#  include <qtadvanceddocking-qt5/DockSplitter.h>
#endif

#ifdef _MSC_VER
#  pragma warning(pop)
#endif


class QLabel;
class QTableView;
class QTextBrowser;
class AppsWidget;
class LogWidget;
class TracerDataReceiver;
class HighlightWidget;

namespace Ui { class TracerViewerMainWindow; }

class TracerViewerMainWindow
    : public QMainWindow
    , public Safir::Dob::StopHandler
    , public Safir::Dob::Dispatcher
{
    Q_OBJECT

public:
    TracerViewerMainWindow(QWidget *parent = nullptr);
    ~TracerViewerMainWindow();

    void OnDoDispatch() override;
    void OnStopOrder() override;
private slots:
    void OnConnectedToDob();
    void OnConnectionClosed();
    void OnThemeChanged();
    void OnEditTracer(const std::int64_t instanceId);
    void UpdateStats();
    void OnCloseCurrentTab();
    void OnOpenFile();
    void OnGenerateTestData();
    void OnSaveFile();
    void ApplyHighlightRulesToAllLogs();
    void OnClearSettingsAndQuit();

private:
    void ConfigureDockManager();
    void BuildUi();
    void SetupToolBarActions();
    void SetupStatusBar();
    void WireSignals();
    void StartStatsTimer();
    void StartDobConnection();
    void SetupAsio();

    void AddTab(const QString& title,
                QWidget* widget);

    Ui::TracerViewerMainWindow *ui;
    QToolBar* const m_toolBar;
    QLabel* m_instanceLabel = nullptr;
    QLabel* const m_connectedLabel;
    QLabel* const m_receivedLabel;
    QLabel* const m_droppedLabel;
    QLabel* const m_bufferLabel;
    QLabel* const m_socketStatusLabel;
    QAction* m_followAction     = nullptr;   // toolbar toggle (Live-Log only)
    QAction* m_jumpToEndAction  = nullptr;   // toolbar push-button (non-live logs)
    QAction* m_clearAction      = nullptr;   // toolbar push-button
    QAction* m_highlightAction  = nullptr;   // toolbar push-button
    QTimer  m_statsTimer;

    ads::CDockManager* m_dockManager = nullptr;
    ads::CDockAreaWidget* m_centralDockArea = nullptr;
    ads::CDockAreaWidget* m_leftDockArea = nullptr;
    ads::CDockWidget* m_editDock = nullptr;

    Safir::Dob::Connection m_dobConnection;
    bool m_connected = false;
    std::atomic_flag m_isDispatchSignalled;

    std::shared_ptr<SettingsManager>  m_settingsManager;
    AppsWidget* m_apps;
    LogWidget* m_liveLog;
    HighlightWidget* m_highlight;

    std::shared_ptr<TracerDataReceiver> m_dataReceiver;

    // ---- Control-Info subscription needs an ioContext, which we run in a bg thread.
    boost::asio::io_context m_io;
    boost::asio::executor_work_guard<boost::asio::io_context::executor_type>  m_ioWork;
    std::thread m_ioThread;
    std::unique_ptr<Safir::Dob::Internal::Control::ControlInfoReceiver> m_controlInfoReceiver;


};
