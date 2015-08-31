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
#ifndef MAINWINDOW_H
#define MAINWINDOW_H


#include <Safir/Dob/Connection.h>
#include "Safir/Control/Status.h"
#include "Safir/Control/Command.h"

#include "NodeTableModel.h"

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4244)
#pragma warning (disable: 4251)
#endif

#include <QMessageBox>
#include <QMainWindow>
#include <QEvent>
#include <QThread>
#include <QTime>
#include <QLabel>
#include <QSortFilterProxyModel>
#include <QItemSelectionModel>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

//----------------------------------------
class DobConnector : public QThread
{
    Q_OBJECT
public:
    DobConnector(Safir::Dob::Connection* con, Safir::Dob::StopHandler* s, Safir::Dob::Dispatcher* d, QObject* /*parent*/)
        :m_con(con)
        ,m_stop(s)
        ,m_disp(d)
    {
    }

signals:
    void ConnectedToDob();

protected:
    virtual void run()
    {
        m_con->Open(L"safir_control_gui", QTime::currentTime().toString("hh:mm:ss.zzz").toStdWString(), 0, m_stop, m_disp);
        m_con->Close();
        emit ConnectedToDob();
    }

private:
    Safir::Dob::Connection* m_con;
    Safir::Dob::StopHandler* m_stop;
    Safir::Dob::Dispatcher* m_disp;
};
//----------------------------------------

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow,
        public Safir::Dob::StopHandler,
        public Safir::Dob::Dispatcher,
        public Safir::Dob::EntitySubscriber,
        public Safir::Dob::Requestor
{
    Q_OBJECT
    
public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

    virtual bool eventFilter(QObject* o, QEvent* e);


    //Dob stuff
    virtual void OnDoDispatch();
    virtual void OnStopOrder();
    virtual void OnNewEntity(const Safir::Dob::EntityProxy entityProxy);
    virtual void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy);
    virtual void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool /*del*/);
    virtual void OnResponse(const Safir::Dob::ResponseProxy responseProxy);
    virtual void OnNotRequestOverflow();

private:
    Ui::MainWindow *ui;
    Safir::Dob::Connection m_dobConnection;
    QEvent::Type m_dispatchEvent;
    DobConnector m_conThread;
    NodeTableModel* m_nodeTableModel;
    QLabel *m_dobConnectionLabel;


    void HandleStatusEntity(const Safir::Control::StatusPtr status);
    void SendRequestOnAllNodes(Safir::Control::Operation::Enumeration operation);
    void SendRequestOnSpecificNode(Safir::Control::Operation::Enumeration operation, int64_t nodeId);
    void SendRequest(Safir::Control::CommandPtr command);
    bool DisplayConfirmationDialog(QString index, Safir::Control::Operation::Enumeration operation);
    bool DisplayConfirmationDialog(Safir::Control::Operation::Enumeration operation);


private slots:
    void OnConnected();
    void on_actionExit_triggered();
    void on_pushButton_StopNode_clicked();
    void on_pushButton_RebootNode_clicked();
    void on_pushButton_ShutdownNode_clicked();
    void on_pushButton_StopAll_clicked();
    void on_pushButton_RebootAll_clicked();
    void on_pushButton_ShutdownAll_clicked();
    void nodeListSelectionChanged(const QItemSelection & selected, const QItemSelection & deselected);
};



#endif // MAINWINDOW_H
