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
#include "mainwindow.h"
#include "ui_mainwindow.h"


MainWindow::MainWindow(QWidget *parent)
    :QMainWindow(parent)
    ,ui(new Ui::MainWindow)
    ,m_dispatchEvent(static_cast<QEvent::Type>(QEvent::User+666))
    ,m_conThread(&m_dobConnection, this, this, 0)
{

    installEventFilter(this);
    ui->setupUi(this);

    statusBar()->showMessage("Not connected");
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
    statusBar()->showMessage("Connected");
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

void MainWindow::OnNewEntity(const Safir::Dob::EntityProxy /*entityProxy*/)
{
}

void MainWindow::OnUpdatedEntity(const Safir::Dob::EntityProxy /*entityProxy*/)
{
}

void MainWindow::OnDeletedEntity(const Safir::Dob::EntityProxy /*entityProxy*/, const bool /*del*/)
{
}

void MainWindow::OnResponse(const Safir::Dob::ResponseProxy responseProxy)
{
}

void MainWindow::OnNotRequestOverflow()
{
}
