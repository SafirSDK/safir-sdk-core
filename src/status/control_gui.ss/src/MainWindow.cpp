/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safir.sourceforge.net)
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
#include <QMessageBox>
#include "MainWindow.h"
#include "NodeTableModel.h"
#include "ui_MainWindow.h"

#include "Safir/Control/Operation.h"
#include "Safir/Dob/OverflowException.h"

MainWindow::MainWindow(QWidget *parent)
    :QMainWindow(parent)
    ,ui(new Ui::MainWindow)
    ,m_dispatchEvent(static_cast<QEvent::Type>(QEvent::User+666))
    ,m_conThread(&m_dobConnection, this, this, 0)
{


    installEventFilter(this);
    ui->setupUi(this);

    m_dobConnectionLabel = new QLabel("Not connected");
    statusBar()->addWidget(m_dobConnectionLabel);

    QObject::connect(&m_conThread, SIGNAL(ConnectedToDob()), this, SLOT(OnConnected()));
    m_conThread.start();
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::OnConnected()
{
    m_dobConnection.Open(L"safir_control_gui", QTime::currentTime().toString("hh:mm:ss.zzz").toStdWString(), 0, this, this);
    statusBar()->showMessage("Connected - System Incarnation Id: UNKNOWN");


    m_dobConnection.SubscribeEntity(
                Safir::Dob::Typesystem::EntityId(Safir::Control::Status::ClassTypeId, Safir::Dob::Typesystem::InstanceId(0)), true, true, this );


    ui->nodeTableView->setModel(new NodeTableModel(this));
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

//------------------------------------------------------------
// DOB stuff
//------------------------------------------------------------
void MainWindow::OnDoDispatch()
{
    QApplication::postEvent(this, new QEvent(m_dispatchEvent));
}

void MainWindow::OnStopOrder()
{

}

void MainWindow::OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
{
    switch (entityProxy.GetTypeId()) {
    case Safir::Control::Status::ClassTypeId:
    {
        Safir::Control::StatusPtr status
                = boost::dynamic_pointer_cast<Safir::Control::Status>(entityProxy.GetEntity());

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
                = boost::dynamic_pointer_cast<Safir::Control::Status>(entityProxy.GetEntity());

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
        HandleStatusEntity(Safir::Control::Status::Create());
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
    if (status->SystemIncarnation().IsNull())
    {
        m_dobConnectionLabel->setText("Connected - System Incarnation Id: UNKNOWN");
    }
    else
    {
        QString message = QString("Connected - System Incarnation Id: %1").arg(status->SystemIncarnation().GetVal());
        m_dobConnectionLabel->setText(message);
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
void MainWindow::on_actionExit_triggered()
{
      this->close();
}


void MainWindow::on_pushButton_StopNode_clicked()
{
    SendRequestOnSpecificNode(Safir::Control::Operation::Stop,0);
}

void MainWindow::on_pushButton_RebootNode_clicked()
{
    SendRequestOnSpecificNode(Safir::Control::Operation::Restart,0);
}

void MainWindow::on_pushButton_ShutdownNode_clicked()
{
    SendRequestOnSpecificNode(Safir::Control::Operation::Shutdown,0);
}

void MainWindow::on_pushButton_StopAll_clicked()
{
    SendRequestOnAllNodes(Safir::Control::Operation::Stop);
}

void MainWindow::on_pushButton_RebootAll_clicked()
{
    SendRequestOnAllNodes(Safir::Control::Operation::Restart);
}

void MainWindow::on_pushButton_ShutdownAll_clicked()
{
    SendRequestOnAllNodes(Safir::Control::Operation::Shutdown);
}
