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
#include <QCloseEvent>
#include <QMessageBox>

#include <Safir/Utilities/Internal/Expansion.h>

SateMainWindow::SateMainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::SateMainWindow)
{
    ui->setupUi(this);

    // Make the left dock area stronger than top/bottom areas
    setCorner(Qt::TopLeftCorner, Qt::LeftDockWidgetArea);
    setCorner(Qt::BottomLeftCorner, Qt::LeftDockWidgetArea);

    // Create menu
    auto m = createPopupMenu();
    m->setTitle("Windows");
    ui->menubar->addMenu(m);

    //Hide entity view at start
    ui->entityViewDockWidget->hide();

    m_dob = std::make_unique<DobHandler>();

    // set default sizes of tree/tabView
    ui->typeTree->Initialize(m_dob.get());

    ui->statusbar->addPermanentWidget(new QLabel(tr("SAFIR_INSTANCE: %1").arg(Safir::Utilities::Internal::Expansion::GetSafirInstance())));
    ui->statusbar->showMessage("Trying to connect to DOB...", std::numeric_limits<int>::max());

    // DOB signal handling
    connect(m_dob.get(), &DobInterface::ConnectedToDob, this, [this](const QString& n)
    {
        ui->statusbar->showMessage(QString("Connected as ") + n, std::numeric_limits<int>::max());
    });

    // TabWidget signal handling
    ui->tabWidget->tabBar()->setContextMenuPolicy(Qt::CustomContextMenu);

    connect(ui->tabWidget, &QTabWidget::tabCloseRequested, this, [this](int i){
        QWidget * widget = this->ui->tabWidget->widget(i);
        this->ui->tabWidget->removeTab(i);
        delete widget;
    });

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

    // TypesystemWidget signal handling
    connect(ui->typeTree, &TypesystemWidget::OpenObjectEdit, this, [this](int64_t tid)
    {
        auto oe = new DobObjectEditWidget(m_dob.get(), tid, this);
        auto tabIndex = ui->tabWidget->addTab(oe, TypesystemRepository::Instance().GetClass(tid)->name);
        ui->tabWidget->setCurrentIndex(tabIndex);
    });

    connect(ui->typeTree, &TypesystemWidget::OpenEntityViewer, this, [](int64_t tid)
    {
        QMessageBox mb;
        mb.setText("TODO: Open EntityViewer for " + QString::fromStdWString(sdt::Operations::GetName(tid)));
        mb.exec();
    });

    connect(ui->typeTree, &TypesystemWidget::OpenDouFile, this, [](int64_t tid)
    {
        QMessageBox mb;
        mb.setText("TODO: Open Dou file for " + QString::fromStdWString(sdt::Operations::GetName(tid)));
        mb.exec();
    });

    // Received table
    ui->receivedTableView->setModel(new ReceivedModel(m_dob.get(), ui->receivedTableView));
    ui->receivedTableView->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    ui->receivedTableView->setColumnWidth(1, 250);
    ui->receivedTableView->horizontalHeader()->setSectionResizeMode(3, QHeaderView::ResizeToContents);
    ui->receivedTableView->setColumnWidth(3, 250);
    ui->receivedTableView->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Stretch);
    ui->receivedTableView->horizontalHeader()->setDefaultAlignment(Qt::AlignLeft);
    connect(ui->receivedTableView, &QTableView::doubleClicked, this, [this](const QModelIndex& ix)
    {
        if (!ix.isValid())
        {
            return;
        }

        auto recvObjItem = static_cast<ReceivedModel*>(ui->receivedTableView->model())->ReadItem(ix.row());
        if (recvObjItem.object)
        {
            auto oe = new DobObjectEditWidget(m_dob.get(), recvObjItem.typeId, recvObjItem.channelHandler, recvObjItem.instance,
                                              recvObjItem.object, this);
            auto tabIndex = ui->tabWidget->addTab(oe, TypesystemRepository::Instance().GetClass(recvObjItem.typeId)->name);
            ui->tabWidget->setCurrentIndex(tabIndex);
        }
    });

    m_dob->Open("SATE", 0);
}

SateMainWindow::~SateMainWindow()
{
    delete ui;
}

void SateMainWindow::CloseTab(int index)
{
    QWidget * widget = this->ui->tabWidget->widget(index);
    this->ui->tabWidget->removeTab(index);
    delete widget;
}

void SateMainWindow::CloseOtherTabs(int indexToKeep)
{
    QWidget* keepWidget = indexToKeep > -1 ? ui->tabWidget->widget(indexToKeep) : nullptr;
    for (int i = ui->tabWidget->count() - 1; i >= 0; --i)
    {
        auto w = ui->tabWidget->widget(i);
        if (w != keepWidget)
        {
            this->ui->tabWidget->removeTab(i);
            delete w;
        }
    }
}
