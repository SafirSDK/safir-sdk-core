/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / stlrha
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
#ifndef DOSEMON_H
#define DOSEMON_H
#include "common_header.h"
#include "ui_dosemon.h"
#include <set>
#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Internal/StateDeleter.h>
#include <boost/atomic.hpp>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#pragma warning (disable: 4100)
#endif

#include <boost/asio.hpp>
#include <boost/thread.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

class DoseMon : public QWidget, private Ui::DoseMonDlg
{
    Q_OBJECT

public:
    DoseMon(QWidget *parent = 0);

public slots:
    void TreeItemActivated ( QTreeWidgetItem * item, int column );

    void CloseTab(int index);
    void UpdateTreeWidget();

private:
    void closeEvent(QCloseEvent* event) override;
    void AddConnection(const Safir::Dob::Internal::Connection & connection,
                       std::set<QString>& localConnectionNames,
                       std::set<QString>& remoteConnectionNames);
    bool ActivateTab(const QString& name);
    void AddEntitesToTreeWidget();

    QTimer m_updateTimer;
    boost::thread m_doseInternalInitializer;
    boost::atomic<bool> m_doseInternalInitialized;
    bool m_entitiesAdded;
};


#endif
