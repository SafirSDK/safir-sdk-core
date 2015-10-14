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

#ifndef __MESSAGEDIALOG_H
#define __MESSAGEDIALOG_H

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4251)
#endif

#include <QDialog>
#include "ui_messagedialog.h"

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#include <Safir/Dob/Connection.h>

namespace VehicleMmiCppQt
{
    /** 
    * Implements class that subscribes for VehicleMsg and shows a dialog 
    * with the text received the message.
    */
    class MessageDialog : 
        public QDialog, 
        // Allows this class to subscribe for messages
        public Safir::Dob::MessageSubscriber
    {
        Q_OBJECT

    public:
        /** 
        * Attach to Dob and subscribe for VehicleMsg.
        */
        MessageDialog(QWidget *parent = 0);

        /** 
        * Detach from Dob.
        */
        ~MessageDialog();
        
        /** 
         * Overrides Safir::Dob::MessageSubscriber. Called by the Dob 
         * when a message is sent that is subscribed for.
         */
        void OnMessage(const Safir::Dob::MessageProxy messageProxy);
 
    private:
        void Show();    // Show dialog

        Safir::Dob::SecondaryConnection m_secDobConnection;
        Ui::MessageDialogClass          ui;

    };
}
#endif // __SERVICEDIALOG_H
