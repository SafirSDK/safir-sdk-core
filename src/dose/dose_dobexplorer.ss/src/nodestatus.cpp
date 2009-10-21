/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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

#include "common_header.h"
#include "nodestatus.h"
#include <iostream>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/DistributionChannelParameters.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <boost/lexical_cast.hpp>
#include "dosecom_stuff.h"




NodeStatus::NodeStatus(QWidget * /*parent*/):
    m_timer(this),
    m_yellowBrush(Qt::yellow),
    m_redBrush(Qt::red),
    m_greenBrush(Qt::green),
    m_blueBrush(Qt::blue),
    m_grayBrush(Qt::lightGray)
{
    setupUi(this); // this sets up GUI

    if (Safir::Dob::DistributionChannelParameters::DistributionChannels(0)->MulticastAddress() == L"127.0.0.1")
    {
        hboxLayout->addWidget(new QLabel("System is in StandAlone mode. \nNo NodeStatus is available."));
        nodeTable->setVisible(false);
        return;
    }

    connect(&m_timer, SIGNAL(timeout()), this, SLOT(UpdateTable()));
    m_timer.start(1000);

    nodeTable->verticalHeader()->hide();

    nodeTable->setRowCount(Safir::Dob::NodeParameters::NumberOfNodes());
    for(int i = 0; i < Safir::Dob::NodeParameters::NumberOfNodes(); ++i)
    {
        nodeTable->setItem(i,0,new QTableWidgetItem(boost::lexical_cast<std::string>(i).c_str()));
        nodeTable->setItem(i,1,new QTableWidgetItem(""));
        nodeTable->setItem(i,2,new QTableWidgetItem(Safir::Dob::Typesystem::Utilities::ToUtf8(Safir::Dob::NodeParameters::Nodes(i)->NodeName().GetVal()).c_str()));
        nodeTable->setItem(i,3,new QTableWidgetItem("Unknown"));
        nodeTable->setItem(i,4,new QTableWidgetItem("Not Started"));
        nodeTable->setItem(i,5,new QTableWidgetItem("0"));

        if (i == Safir::Dob::ThisNodeParameters::NodeNumber())
        {
            nodeTable->item(i,4)->setText("Me");
            nodeTable->item(i,0)->setBackground(m_yellowBrush);
            nodeTable->item(i,1)->setBackground(m_yellowBrush);
            nodeTable->item(i,5)->setToolTip("The number of packets sent by this node. \n\nGray color means that the node has not sent anything in the last 1 second");
        }
        else
        {
            nodeTable->item(i,5)->setToolTip("The number of packets received from this node. \n\nGray color means that nothing has been received in last 1 second");
        }
    }

    m_defaultBrush = nodeTable->item(0,2)->background();
    UpdateTable();

}



void NodeStatus::UpdateTable()
{
    DOSE_SHARED_DATA_S * pShm = (DOSE_SHARED_DATA_S *) Get_NodeSharedData_Pointer();

    for (int jx= 0 ; jx< Safir::Dob::NodeParameters::NumberOfNodes() ; jx++)
    {
        const bool isMe = jx == Safir::Dob::ThisNodeParameters::NodeNumber();

        if (!isMe)
        {
            switch (pShm->NodeStatusTable[jx].Status)
            {
            case NODESTATUS_FREE:
                {
                    continue;
                }

            case NODESTATUS_UP:
                {
                    nodeTable->item(jx,4)->setText("Up");
                    nodeTable->item(jx,0)->setBackground(m_greenBrush);
                }
                break;

            case NODESTATUS_DOWN:
                {
                    nodeTable->item(jx,4)->setText("Down");
                    nodeTable->item(jx,0)->setBackground(m_redBrush);
                    nodeTable->item(jx,1)->setBackground(m_redBrush);
                }
                break;

            case NODESTATUS_NEW:
                {
                    nodeTable->item(jx,4)->setText("New");
                    nodeTable->item(jx,0)->setBackground(m_blueBrush);
                }
                break;
            }
        }
        char ipaddr[100];
        IpAddr_ToString(pShm->NodeStatusTable[jx].IpAddr_nw,ipaddr);
        nodeTable->item(jx,3)->setText(ipaddr);

        if (isMe)
        {
            if(pShm->BitMapToBePoolDistributed64 | pShm->BitMapBeingPoolDistributed64)
            {
                nodeTable->item(jx,1)->setBackground(m_blueBrush);
            }
            else
            {
                nodeTable->item(jx,1)->setBackground(m_yellowBrush);
            }
        }
        else
        {
            boost::uint64_t BitMap64 = (dcom_ulong64) 1 << jx;

            if(pShm->NodeStatusTable[jx].Status == 'U')
            {
                if((pShm->BitMapToBePoolDistributed64
                    | pShm->BitMapBeingPoolDistributed64) & BitMap64)
                {
                    nodeTable->item(jx,1)->setBackground(m_blueBrush);
                }
                else
                {
                    nodeTable->item(jx,1)->setBackground(m_greenBrush);
                }
            }
            else if(pShm->NodeStatusTable[jx].Status == 'N')
            {
                if(((pShm->BitMapToBePoolDistributed64
                     | pShm->BitMapBeingPoolDistributed64) & BitMap64) == 0)
                {
                    nodeTable->item(jx,1)->setBackground(m_greenBrush);
                }
                else
                {
                    nodeTable->item(jx,1)->setBackground(m_blueBrush);
                }
            }
        }

        std::string newValue;
        if(isMe)
        {
            newValue = boost::lexical_cast<std::string>(pShm->Statistics.TotTxCount).c_str();
        }
        else
        {
            newValue = boost::lexical_cast<std::string>(pShm->NodeStatusTable[jx].RxCount).c_str();
        }

        if (nodeTable->item(jx,5)->text() != newValue.c_str())
        {
            nodeTable->item(jx,5)->setBackground(m_defaultBrush);
            nodeTable->item(jx,5)->setText(newValue.c_str());
        }
        else
        {
            nodeTable->item(jx,5)->setBackground(m_grayBrush);
        }

    }
    nodeTable->resizeColumnsToContents();
}
