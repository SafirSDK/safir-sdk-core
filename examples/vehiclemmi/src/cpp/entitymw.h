/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
*
* Created by: Petter Lönnstedt / stpeln
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

#ifndef __ENTITYMW_H
#define __ENTITYMW_H

#include "entitytablehandler.h"
#include "entitydialog.h"
#include "servicedialog.h"
#include "messagedialog.h"
#include "categoryinfodialog.h"

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4251)
#pragma warning (disable: 4244)
#pragma warning (disable: 4800)
#endif

#include <QtGui>
#include "ui_entitymw.h"

#ifdef _MSC_VER
#pragma warning(pop)
#endif

namespace VehicleMmiCppQt
{
    /** 
    * Implements main window that contains a vehicle table and buttons to create, 
    * update and delete vehicles. It also contains a button to calculate speed
    * by using a service and a button to retrieve category information from a database. 
    * The vehicle table changes are handled by class EntityTableHandler that receives 
    * a pointer to the table in the constructor.
   */
        
    class EntityMw : 
        public QMainWindow, 
        // Allows this class to receive response on sent requests
        public Safir::Dob::Requestor
    {
        Q_OBJECT

    public:
        /** 
        * Create table and dialogs. Attach to Dob.
        */
        EntityMw(QWidget *parent = 0, Qt::WindowFlags flags = 0);
        
        /** 
        * Detach from Dob.
        */
        ~EntityMw();

        /** 
        * Overrides Safir.Dob.Requestor. Called by the Dob 
        * when a response is received on a sent request
        */
        virtual void OnResponse(const Safir::Dob::ResponseProxy responseProxy);
        
        /** 
        * Overrides Safir.Dob.Requestor. Called by the Dob 
        * to indicate that it is meningful to make a retry 
        * after an overflow situation
        */
        virtual void OnNotRequestOverflow();

    private:
        Ui::EntityMwClass ui;

        Safir::Dob::SecondaryConnection m_secDobConnection;

        EntityTableHandler* m_pEntityTableHandler;

        EntityDialog* m_pEntityDlg;
        ServiceDialog* m_pServiceDlg;
        MessageDialog* m_pMessageDlg;
        CategoryInfoDialog* m_pCategoryInfoDlg;

    private slots:
        void on_actionCreate_triggered();
        void on_actionUpdate_triggered();
        void on_actionDelete_triggered();
        void on_actionSpeed_triggered();
        void on_actionCategory_triggered();
        void on_actionDeleteCategoryInfo_triggered();
    };

} // namespace VehicleMmiCppQt

#endif // __ENTITYMW_H
