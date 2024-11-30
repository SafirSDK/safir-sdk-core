/******************************************************************************
*
* Copyright Saab AB, 2024 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
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
#include <functional>

#if (QT_VERSION >= QT_VERSION_CHECK(6, 0, 0))
#  include <qt6advanceddocking/DockManager.h>
#  include <qt6advanceddocking/DockAreaWidget.h>
#else
#  include <qt5advanceddocking/DockManager.h>
#  include <qt5advanceddocking/DockAreaWidget.h>
#endif

#include "dobhandler.h"

class QLabel;
class QTableView;
class TypesystemWidget;
class QTextBrowser;
class InstancesWidget;
class ConnectDialog;
class OutputWidget;

namespace Ui { class SateMainWindow; }

class SateMainWindow : public QMainWindow
{
    Q_OBJECT

public:
    SateMainWindow(QWidget *parent = nullptr);
    ~SateMainWindow();

private slots:
    void OnOpenEntityInstanceViewer(const int64_t typeId, const bool includeSubclasses);
    void OnOpenMessageInstanceViewer(const int64_t typeId,
                                     const Safir::Dob::Typesystem::ChannelId& channel,
                                     const bool includeSubclasses);
    void OnConnectedToDob(const QString& connectionName);
    void OnConnectionClosed();
    void OnOpenObjectEdit(const int64_t typeId);
    void OnOpenObjectEditWithInstance(int64_t typeId,
                                      QString channelHandler,
                                      int64_t instance,
                                      const Safir::Dob::Typesystem::ObjectPtr& object);
    void OnOpenDouFile(const int64_t typeId);
    void OnDarkMode();
    void OnLightMode();
    void OnFocusedDockWidgetChanged(ads::CDockWidget* old, ads::CDockWidget* now);
    void OnStatusBarInfoChanged();
    void OnResetWindows();
private:
    void OpenInstanceViewer(const int64_t typeId,
                            const bool includeSubclasses,
                            const std::function<InstancesWidget* ()>& factory);

    void AddXmlPage(const QString& title, const QString& text);
    void AddJsonPage(const QString& title, const QString& text);

    void AddTab(const QString& title, QWidget* widget);

    Ui::SateMainWindow *ui;
    QToolBar* const m_toolBar;
    QLabel* m_instanceLabel = nullptr;
    QLabel* const m_connectedLabel;

    ads::CDockManager* m_dockManager;
    ads::CDockAreaWidget* m_centralDockArea;

    DobHandler m_dob;
    bool m_connected = false;
    ConnectDialog* m_connectDialog;

    TypesystemWidget* m_typesystem;
    OutputWidget* m_output;

    QList<QLabel*> m_statusBarLabels;
    QList<QWidget*> m_statusBarSeparators;
    QMetaObject::Connection m_currentStatusBarConnection;
};

