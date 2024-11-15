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
#include "satemainwindow.h"
#include "./ui_satemainwindow.h"
#include "typesystemrepository.h"
#include "dobobjecteditwidget.h"
#include "dobhandler.h"
#include "instanceswidget.h"
#include "receivedmodel.h"

#include <QtConcurrent/QtConcurrent>
#include <QLabel>
#include <QTabWidget>
#include <QTableView>
#include <QHeaderView>
#include <QCloseEvent>
#include <QMessageBox>

#include <Safir/Utilities/Internal/Expansion.h>

SateMainWindow::SateMainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::SateMainWindow)
{
    ui->setupUi(this);

    m_dob = std::make_unique<DobHandler>();

    ads::CDockManager::setConfigFlag(ads::CDockManager::DockAreaHasUndockButton, false);
    ads::CDockManager::setConfigFlag(ads::CDockManager::DockAreaHasCloseButton, false);
    ads::CDockManager::setConfigFlag(ads::CDockManager::MiddleMouseButtonClosesTab, true);
    ads::CDockManager::setConfigFlag(ads::CDockManager::TabCloseButtonIsToolButton, true);
    //ads::CDockManager::setAutoHideConfigFlags(ads::CDockManager::DefaultAutoHideConfig);

    m_dockManager = new ads::CDockManager(this);
    QFile f(":qdarkstyle/dark/ads.qss");
    if (f.exists())   {
        f.open(QFile::ReadOnly | QFile::Text);
        QTextStream ts(&f);
        m_dockManager->setStyleSheet(ts.readAll());
    }


    //Set up the central area as always present
    QLabel* label = new QLabel();
    label->setText("Welcome to the next generation Sate. SateNext!");
    label->setAlignment(Qt::AlignCenter);
    auto* centralDockWidget = new ads::CDockWidget("CentralWidget");
    centralDockWidget->setWidget(label);
    centralDockWidget->setFeature(ads::CDockWidget::NoTab, true);
    m_centralDockArea = m_dockManager->setCentralWidget(centralDockWidget);

    m_typesystem = new TypesystemWidget(this);
    m_typesystem->Initialize(m_dob.get());

    auto* typesystemDock = new ads::CDockWidget("Typesystem");
    typesystemDock->setWidget(m_typesystem);
    m_dockManager->addDockWidget(ads::LeftDockWidgetArea, typesystemDock);
    ui->menuView->addAction(typesystemDock->toggleViewAction());

    // TypesystemWidget signal handling
    connect(m_typesystem, &TypesystemWidget::OpenObjectEdit, this, &SateMainWindow::OnOpenObjectEdit);

    m_instanceLabel = new QLabel(tr("SAFIR_INSTANCE: %1").arg(Safir::Utilities::Internal::Expansion::GetSafirInstance()));
    ui->statusbar->addPermanentWidget(m_instanceLabel);
    ui->statusbar->showMessage(tr("Trying to connect to DOB..."), std::numeric_limits<int>::max());

    // DOB signal handling
    connect(m_dob.get(), &DobInterface::ConnectedToDob, this, &SateMainWindow::OnConnectedToDob);
    connect(m_typesystem, &TypesystemWidget::OpenInstanceViewer, this, &SateMainWindow::OnOpenInstanceViewer);
    connect(m_typesystem, &TypesystemWidget::OpenDouFile, this, &SateMainWindow::OnOpenDouFile);

    // Received table
    m_received = new QTableView();
    m_received->setModel(new ReceivedModel(m_dob.get(), m_received));
    m_received->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    m_received->setColumnWidth(1, 250);
    m_received->horizontalHeader()->setSectionResizeMode(3, QHeaderView::ResizeToContents);
    m_received->setColumnWidth(3, 250);
    m_received->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Stretch);
    m_received->horizontalHeader()->setDefaultAlignment(Qt::AlignLeft);

    connect(m_received, &QTableView::doubleClicked, this, &SateMainWindow::OnReceivedTableDoubleClicked);


    auto* receivedDock = new ads::CDockWidget("Received");
    receivedDock->setWidget(m_received);
    m_dockManager->addDockWidget(ads::BottomDockWidgetArea, receivedDock);
    ui->menuView->addAction(receivedDock->toggleViewAction());


    m_dob->Open("SATE", 0);
}

SateMainWindow::~SateMainWindow()
{
    delete ui;

    for (auto&& dock: m_dockManager->dockWidgetsMap())
    {
        m_dockManager->removeDockWidget(dock);
    }

    if (!m_connected)
    {
        //If the connect is still pending we need to kill the program very forcefully, since there is no way
        //to interrupt the Open call.
        std::quick_exit(0);
    }
}

void SateMainWindow::OnOpenInstanceViewer(const int64_t typeId, const bool includeSubclasses)
{
    const auto* const cls = TypesystemRepository::Instance().GetClass(typeId);

    auto* dock = m_dockManager->findDockWidget(cls->name);
    if (dock != nullptr)
    {
        if (dock->property("includeSubclasses").toBool() != includeSubclasses)
        {
            m_dockManager->removeDockWidget(dock);
            ui->statusbar->showMessage("Closed a previous viewer for the same type but with different recursiveness.", 30000);
        }
        else
        {
            dock->raise();
            return;
        }
    }

    // no previous dock found, create new
    auto ev = new InstancesWidget(m_dob.get(), typeId, includeSubclasses, this);
    dock = new ads::CDockWidget(cls->name);
    dock->setWidget(ev);
    dock->setFeature(ads::CDockWidget::DockWidgetDeleteOnClose, true);
    dock->setProperty("includeSubclasses",includeSubclasses);
    if (includeSubclasses)
    {
        dock->setIcon(QIcon(":/img/icons/instance_viewer_recursive.png"));
    }
    else
    {
        dock->setIcon(QIcon(":/img/icons/instance_viewer.png"));
    }

    if (cls->dobBaseClass == TypesystemRepository::Entity)
    {
        if (includeSubclasses)
        {
            dock->setTabToolTip(tr("All instances of the entity %1 and its subclasses").arg(cls->name));
        }
        else
        {
            dock->setTabToolTip(tr("All instances of the entity %1").arg(cls->name));
        }
    }
    else
    {
        throw std::logic_error("Need to add an approprate tooltip for this type");
    }

    m_dockManager->addDockWidget(ads::CenterDockWidgetArea, dock, m_centralDockArea);
}

void SateMainWindow::OnConnectedToDob(const QString& connectionName)
{
    QString msg = tr("Connected as %1").arg(connectionName);
    ui->statusbar->showMessage(msg, 30000);
    m_instanceLabel->setToolTip(msg);
    m_connected = true;
}

void SateMainWindow::OnReceivedTableDoubleClicked(const QModelIndex& ix)
{
    if (!ix.isValid())
    {
        return;
    }

    auto recvObjItem = static_cast<ReceivedModel*>(m_received->model())->ReadItem(ix.row());
    if (recvObjItem.object)
    {
        auto oe = new DobObjectEditWidget(m_dob.get(),
                                          recvObjItem.typeId,
                                          recvObjItem.channelHandler,
                                          recvObjItem.instance,
                                          recvObjItem.object,
                                          this);
        auto* dock = new ads::CDockWidget(TypesystemRepository::Instance().GetClass(recvObjItem.typeId)->name);
        dock->setWidget(oe);
        dock->setFeature(ads::CDockWidget::DockWidgetDeleteOnClose, true);
        m_dockManager->addDockWidget(ads::CenterDockWidgetArea, dock, m_centralDockArea);
    }
}

void SateMainWindow::OnOpenObjectEdit(const int64_t typeId)
{
    auto oe = new DobObjectEditWidget(m_dob.get(), typeId, this);
    auto* dock = new ads::CDockWidget(TypesystemRepository::Instance().GetClass(typeId)->name);
    dock->setWidget(oe);
    dock->setFeature(ads::CDockWidget::DockWidgetDeleteOnClose, true);
    m_dockManager->addDockWidget(ads::CenterDockWidgetArea, dock, m_centralDockArea);
}

void SateMainWindow::OnOpenDouFile(const int64_t typeId)
{
    QMessageBox mb;
    mb.setText("TODO: Open Dou file for " + QString::fromStdWString(sdt::Operations::GetName(typeId)));
    mb.exec();
}
