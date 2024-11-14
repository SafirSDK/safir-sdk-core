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
    label->setText("Welcome to SateNy!");
    label->setAlignment(Qt::AlignCenter);
    auto* centralDockWidget = new ads::CDockWidget("CentralWidget");
    centralDockWidget->setWidget(label);
    centralDockWidget->setFeature(ads::CDockWidget::NoTab, true);// set the flag before adding the widget to dock manager
    m_centralDockArea = m_dockManager->setCentralWidget(centralDockWidget);

    m_typesystem = new TypesystemWidget();
    m_typesystem->Initialize(m_dob.get());

    auto* typesystemDock = new ads::CDockWidget("Typesystem");
    typesystemDock->setWidget(m_typesystem);
    m_dockManager->addDockWidget(ads::LeftDockWidgetArea, typesystemDock);
    ui->menuView->addAction(typesystemDock->toggleViewAction());

    // TypesystemWidget signal handling
    connect(m_typesystem, &TypesystemWidget::OpenObjectEdit, this, [this](int64_t tid)
    {
        auto oe = new DobObjectEditWidget(m_dob.get(), tid, this);
        auto* dock = new ads::CDockWidget(TypesystemRepository::Instance().GetClass(tid)->name);
        dock->setWidget(oe);
        m_dockManager->addDockWidget(ads::CenterDockWidgetArea, dock, m_centralDockArea);
    });

    ui->statusbar->addPermanentWidget(new QLabel(tr("SAFIR_INSTANCE: %1").arg(Safir::Utilities::Internal::Expansion::GetSafirInstance())));
    ui->statusbar->showMessage("Trying to connect to DOB...", std::numeric_limits<int>::max());

    // DOB signal handling
    connect(m_dob.get(), &DobInterface::ConnectedToDob, this, [this](const QString& n)
    {
        ui->statusbar->showMessage(QString("Connected as ") + n, std::numeric_limits<int>::max());
        m_connected = true;
    });

#if 0
    // TabWidget signal handling
    ui->tabWidget->tabBar()->setContextMenuPolicy(Qt::CustomContextMenu);

    connect(ui->tabWidget->tabBar(), &QTabBar::customContextMenuRequested, [this](const QPoint& p){
        auto index = ui->tabWidget->tabBar()->tabAt(p);
        if (index > -1)
        {
            QMenu menu;
            connect(menu.addAction("Close other tabs"), &QAction::triggered, this, [this, index]{CloseOtherTabs(index);});
            connect(menu.addAction("Close all tabs"), &QAction::triggered, this, [this]{CloseOtherTabs(-1);});
            menu.addSeparator();
            menu.addAction("Locate in type tree");
            menu.exec(ui->tabWidget->tabBar()->mapToGlobal(p));
        }
    });

#endif


    connect(m_typesystem, &TypesystemWidget::OpenEntityViewer, this, [](int64_t tid)
    {
        QMessageBox mb;
        mb.setText("TODO: Open EntityViewer for " + QString::fromStdWString(sdt::Operations::GetName(tid)));
        mb.exec();
    });

    connect(m_typesystem, &TypesystemWidget::OpenDouFile, this, [](int64_t tid)
    {
        QMessageBox mb;
        mb.setText("TODO: Open Dou file for " + QString::fromStdWString(sdt::Operations::GetName(tid)));
        mb.exec();
    });

    // Received table
    m_received = new QTableView();
    m_received->setModel(new ReceivedModel(m_dob.get(), m_received));
    m_received->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    m_received->setColumnWidth(1, 250);
    m_received->horizontalHeader()->setSectionResizeMode(3, QHeaderView::ResizeToContents);
    m_received->setColumnWidth(3, 250);
    m_received->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Stretch);
    m_received->horizontalHeader()->setDefaultAlignment(Qt::AlignLeft);

    connect(m_received, &QTableView::doubleClicked, this, [this](const QModelIndex& ix)
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
            m_dockManager->addDockWidget(ads::CenterDockWidgetArea, dock, m_centralDockArea);
        }
    });


    auto* receivedDock = new ads::CDockWidget("Received");
    receivedDock->setWidget(m_received);
    m_dockManager->addDockWidget(ads::BottomDockWidgetArea, receivedDock);
    ui->menuView->addAction(receivedDock->toggleViewAction());


    m_dob->Open("SATE", 0);
}

SateMainWindow::~SateMainWindow()
{
    delete ui;
    if (!m_connected)
    {
        //If the connect is still pending we need to kill the program very forcefully, since there is no way
        //to interrupt the Open call.
        std::quick_exit(0);
    }
}
