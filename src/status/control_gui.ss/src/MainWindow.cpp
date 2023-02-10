/******************************************************************************
*
* Copyright Saab AB, 2015, 2023 (http://safirsdkcore.com)
*
* Created by: Samuel Waxin / samuel.waxin@consoden.se
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
#include <string>
#include <boost/filesystem.hpp>

#include "MainWindow.h"

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4244)
#pragma warning (disable: 4251)
#pragma warning (disable: 4800)
#endif

#include "ui_MainWindow.h"

#ifdef _MSC_VER
#pragma warning (pop)
#endif


#include <Safir/Utilities/Internal/Expansion.h>
#include "Safir/Control/Operation.h"
#include "Safir/Control/Parameters.h"
#include "Safir/Dob/OverflowException.h"

#ifdef _MSC_VER
#pragma warning (push)
//fix for vs2010 which complains about the usage of this in the member initializer list
#pragma warning (disable: 4355) 
#endif
MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
    , m_dispatchEvent(static_cast<QEvent::Type>(QEvent::User+666))
    , m_conThread(&m_dobConnection, this, this, 0)
    , m_rebootConfigured(!Safir::Control::Parameters::RebootCommand().empty())
    , m_shutdownConfigured(!Safir::Control::Parameters::ShutdownCommand().empty())
{
    installEventFilter(this);
    ui->setupUi(this);

    m_dobConnectionLabel = new QLabel(this);
    statusBar()->setStyleSheet("QStatusBar::item { border: 0px solid black }; ");
    statusBar()->addPermanentWidget(m_dobConnectionLabel);

    UpdateStatus();

    QObject::connect(&m_conThread, SIGNAL(ConnectedToDob()), this, SLOT(OnConnected()));
    m_conThread.start();
}
#ifdef _MSC_VER
#pragma warning (pop)
#endif

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::OnConnected()
{
    m_dobConnection.Open(L"safir_control_gui", QTime::currentTime().toString("hh:mm:ss.zzz").toStdWString(), 0, this, this);

    QSortFilterProxyModel *proxyModel = new QSortFilterProxyModel(this);
    m_nodeTableModel = new NodeTableModel(this);

    proxyModel->setSourceModel(m_nodeTableModel);
    ui->nodeTableView->setModel(proxyModel);
    ui->nodeTableView->horizontalHeader()->setSectionResizeMode(QHeaderView::ResizeToContents);
    ui->nodeTableView->verticalHeader()->setSectionResizeMode(QHeaderView::ResizeToContents);

    ui->nodeTableView->setColumnHidden(NODE_ID_COLUMN, false);

    connect(ui->nodeTableView->selectionModel(),
            &QItemSelectionModel::selectionChanged,
            this,
            &MainWindow::UpdateStatus);

    SetupContextMenu();

    UpdateStatus();

    m_dobConnection.SubscribeEntity(Safir::Dob::Typesystem::EntityId(Safir::Control::Status::ClassTypeId,
                                                                     Safir::Dob::Typesystem::InstanceId(0)),
                                    true, true, this );
}

void MainWindow::SetupContextMenu()
{
    ui->nodeTableView->setContextMenuPolicy(Qt::CustomContextMenu);

    connect(ui->nodeTableView, SIGNAL(customContextMenuRequested(QPoint)),
            this,
            SLOT(customMenuRequested(QPoint)));

    m_NodeContextmenu = new QMenu(this);

    QAction* reboot = new QAction("Reboot", this);
    connect(reboot, SIGNAL(triggered()),
            this,
            SLOT(on_pushButton_RebootNode_clicked()));

    QAction* shutdown = new QAction("Shutdown", this);
    connect(shutdown, SIGNAL(triggered()),
            this,
            SLOT(on_pushButton_ShutdownNode_clicked()));

    QAction* stop = new QAction("Stop", this);
    connect(stop, SIGNAL(triggered()),
            this,
            SLOT(on_pushButton_StopNode_clicked()));

    m_NodeContextmenu->addAction(stop);
    m_NodeContextmenu->addAction(reboot);
    m_NodeContextmenu->addAction(shutdown);

    reboot->setEnabled(m_rebootConfigured);
    shutdown->setEnabled(m_shutdownConfigured);
}



//------------------------------------------------------------
// DOB stuff
//------------------------------------------------------------
void MainWindow::OnDoDispatch()
{
    QApplication::postEvent(this, new QEvent(m_dispatchEvent));
}

void MainWindow::OnStopOrder()
{
    m_dobConnection.Close();
    this->close();

}

void MainWindow::OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
{
    switch (entityProxy.GetTypeId()) {
    case Safir::Control::Status::ClassTypeId:
    {
        Safir::Control::StatusPtr status
                = std::dynamic_pointer_cast<Safir::Control::Status>(entityProxy.GetEntity());

        if (status)
        {
            HandleStatusEntity(status);
        }
    }
        break;
    default:
        break;
    }
}

void MainWindow::OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
{
    switch (entityProxy.GetTypeId()) {
    case Safir::Control::Status::ClassTypeId:
    {
        Safir::Control::StatusPtr status
                = std::dynamic_pointer_cast<Safir::Control::Status>(entityProxy.GetEntity());

        if (status)
        {
            HandleStatusEntity(status);
        }
    }
        break;
    default:
        break;
    }
}

void MainWindow::OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool /*del*/)
{
    switch (entityProxy.GetTypeId()) {
    case Safir::Control::Status::ClassTypeId:
        {
            HandleStatusEntity(nullptr);
        }
        break;
    default:
        break;
    }
}

void MainWindow::OnResponse(const Safir::Dob::ResponseProxy responseProxy)
{
    if (responseProxy.IsSuccess())
    {
        statusBar()->showMessage("Command returned success", 5000);
    }
    else
    {
        statusBar()->showMessage("Command returned error", 5000);
    }
}

void MainWindow::OnNotRequestOverflow()
{
}


//------------------------------------------------------------
// Own DOB stuff
//------------------------------------------------------------
void MainWindow::HandleStatusEntity(const Safir::Control::StatusPtr status)
{

    m_statusRunning = status != nullptr;;
    UpdateStatus();

    if (status != nullptr && !status->NodeId().IsNull())
    {
        m_nodeTableModel->setOwnNodeId(status->NodeId());
    }
    
    if (status != nullptr && !status->SystemIncarnation().IsNull())
    {
        m_incarnationId = QString::number(status->SystemIncarnation().GetVal());
    }
    else
    {
        m_incarnationId = tr("UNKNOWN");
    }
}

void MainWindow::SendRequestOnAllNodes(Safir::Control::Operation::Enumeration operation)
{
    Safir::Control::CommandPtr command = Safir::Control::Command::Create();
    command->Operation().SetVal(operation);

    SendRequest(command);
}

void MainWindow::SendRequestOnSpecificNode(Safir::Control::Operation::Enumeration operation, int64_t nodeId)
{
    Safir::Control::CommandPtr command = Safir::Control::Command::Create();
    command->Operation().SetVal(operation);
    command->NodeId().SetVal(nodeId);

    SendRequest(command);
}

void MainWindow::SendRequest(Safir::Control::CommandPtr command)
{
    try
    {
        m_dobConnection.ServiceRequest(command, Safir::Dob::Typesystem::HandlerId(), this);
    }
    catch (...)
    {
        //No overflow handling, user will have to press again, show error msg for 5 seconds
        statusBar()->showMessage("Failed to send command", 5000);
    }
}

//------------------------------------------------------------
// GUI stuff
//------------------------------------------------------------

bool MainWindow::DisplayConfirmationDialog(QString name, Safir::Control::Operation::Enumeration operation)
{
    QString op;

    switch (operation) {
    case Safir::Control::Operation::Reboot:
        op = "rebooted";
        break;
    case Safir::Control::Operation::Shutdown:
        op = "shutdown";
        break;
    case Safir::Control::Operation::Stop:
        op = "stopped";
        break;

    default:
        break;
    }

    QString message = QString("The node <b>%1</b> will be %2.\nAre you sure?").arg( name ).arg( op );
    return QMessageBox::warning(this, "Node control", message, QMessageBox::No | QMessageBox::Yes, QMessageBox::No) == QMessageBox::Yes;
}

bool MainWindow::DisplayConfirmationDialog(Safir::Control::Operation::Enumeration operation)
{
    QString op;

    switch (operation)
    {
    case Safir::Control::Operation::Reboot:
        op = "rebooted";
        break;
    case Safir::Control::Operation::Shutdown:
        op = "shutdown";
        break;
    case Safir::Control::Operation::Stop:
        op = "stopped";
        break;

    default:
        break;
    }

    QString message = QString("All nodes will be %2.\nAre you sure?").arg( op );
    return QMessageBox::warning(this, "Node control", message, QMessageBox::No | QMessageBox::Yes, QMessageBox::No) == QMessageBox::Yes;
}

void MainWindow::customMenuRequested(QPoint pos)
{
    QModelIndex index= ui->nodeTableView->indexAt(pos);

    if (index != QModelIndex())
        m_NodeContextmenu->popup(ui->nodeTableView->viewport()->mapToGlobal(pos));
}

void MainWindow::on_pushButton_StopNode_clicked()
{
    QModelIndexList selected = ui->nodeTableView->selectionModel()->selectedIndexes();

    if (DisplayConfirmationDialog(m_nodeTableModel->data(m_nodeTableModel->index(selected[0].row(), NAME_COLUMN)).toString(), Safir::Control::Operation::Stop))
    {
        SendRequestOnSpecificNode(Safir::Control::Operation::Stop, m_nodeTableModel->data(m_nodeTableModel->index(selected[0].row(), NODE_ID_COLUMN)).toLongLong());
    }
}

void MainWindow::on_pushButton_RebootNode_clicked()
{
    QModelIndexList selected = ui->nodeTableView->selectionModel()->selectedIndexes();

    if (DisplayConfirmationDialog(m_nodeTableModel->data(m_nodeTableModel->index(selected[0].row(), NAME_COLUMN)).toString(), Safir::Control::Operation::Reboot))
    {
        SendRequestOnSpecificNode(Safir::Control::Operation::Reboot, m_nodeTableModel->data(m_nodeTableModel->index(selected[0].row(), NODE_ID_COLUMN)).toLongLong());
    }
}

void MainWindow::on_pushButton_ShutdownNode_clicked()
{
    QModelIndexList selected = ui->nodeTableView->selectionModel()->selectedIndexes();

    if (DisplayConfirmationDialog(m_nodeTableModel->data(m_nodeTableModel->index(selected[0].row(), NAME_COLUMN)).toString(), Safir::Control::Operation::Shutdown))
    {
        SendRequestOnSpecificNode(Safir::Control::Operation::Shutdown, m_nodeTableModel->data(m_nodeTableModel->index(selected[0].row(), NODE_ID_COLUMN)).toLongLong());
    }
}

void MainWindow::on_pushButton_StopAll_clicked()
{
    if (DisplayConfirmationDialog(Safir::Control::Operation::Stop))
    {
        SendRequestOnAllNodes(Safir::Control::Operation::Stop);
    }
}

void MainWindow::on_pushButton_RebootAll_clicked()
{
    if (DisplayConfirmationDialog(Safir::Control::Operation::Reboot))
    {
        SendRequestOnAllNodes(Safir::Control::Operation::Reboot);
    }
}

void MainWindow::on_pushButton_ShutdownAll_clicked()
{
    if (DisplayConfirmationDialog(Safir::Control::Operation::Shutdown))
    {
        SendRequestOnAllNodes(Safir::Control::Operation::Shutdown);
    }
}

void MainWindow::UpdateStatus()
{
    QString status;

    if (m_dobConnection.IsOpen() && m_statusRunning)
    {
        status += tr("Connected");
    }
    else if (m_dobConnection.IsOpen())
    {
        status += tr("safir_status not running");
    }
    else
    {
        status += tr("Not connected");
    }

    status += tr(" | System Incarnation Id: %1").arg(m_incarnationId);
    status += tr(" | SAFIR_INSTANCE: %1").arg(Safir::Utilities::Internal::Expansion::GetSafirInstance());

    m_dobConnectionLabel->setText(status);

    bool somethingSelected = m_dobConnection.IsOpen() && ui->nodeTableView->selectionModel()->hasSelection();

    ui->pushButton_RebootNode->setEnabled(somethingSelected && m_statusRunning && m_rebootConfigured);
    ui->pushButton_ShutdownNode->setEnabled(somethingSelected && m_statusRunning && m_shutdownConfigured);
    ui->pushButton_StopNode->setEnabled(somethingSelected && m_statusRunning);

    ui->pushButton_RebootAll->setEnabled(m_statusRunning && m_rebootConfigured);
    ui->pushButton_ShutdownAll->setEnabled(m_statusRunning && m_shutdownConfigured);
    ui->pushButton_StopAll->setEnabled(m_statusRunning);

    if (!m_rebootConfigured)
    {
        ui->pushButton_RebootAll->setToolTip(tr("Safir.Control.Parameters.RebootCommand is not set"));
        ui->pushButton_RebootNode->setToolTip(tr("Safir.Control.Parameters.RebootCommand is not set"));
    }

    if (!m_shutdownConfigured)
    {
        ui->pushButton_ShutdownAll->setToolTip(tr("Safir.Control.Parameters.ShutdownCommand is not set"));
        ui->pushButton_ShutdownNode->setToolTip(tr("Safir.Control.Parameters.ShutdownCommand is not set"));
    }

    if (!m_statusRunning)
    {
        ui->pushButton_StopNode->setToolTip(tr("safir_status is not running"));
        ui->pushButton_RebootNode->setToolTip(tr("safir_status is not running"));
        ui->pushButton_ShutdownNode->setToolTip(tr("safir_status is not running"));
        ui->pushButton_StopAll->setToolTip(tr("safir_status is not running"));
        ui->pushButton_RebootAll->setToolTip(tr("safir_status is not running"));
        ui->pushButton_ShutdownAll->setToolTip(tr("safir_status is not running"));
    }

    if (m_NodeContextmenu != nullptr)
    {
        m_NodeContextmenu->setEnabled(m_statusRunning);
    }
}

void MainWindow::on_actionExit_triggered()
{
      OnStopOrder();
}

bool MainWindow::eventFilter(QObject*, QEvent* e)
{
    if (e->type()==m_dispatchEvent)
    {
        m_dobConnection.Dispatch();
        return true;
    }
    return false;
}
