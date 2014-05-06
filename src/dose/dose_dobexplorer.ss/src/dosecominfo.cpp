/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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
#include "dosecominfo.h"
#include "dosecom_stuff.h"
#include <sstream>
#include <math.h>
#include <Safir/Dob/DistributionChannelParameters.h>

DoseComInfo::DoseComInfo(QWidget * /*parent*/):
    m_timer(this)
{
    setupUi(this); // this sets up GUI

    if (Safir::Dob::DistributionChannelParameters::DistributionChannels(0)->MulticastAddress() == L"127.0.0.1")
    {
        hboxLayout->addWidget(new QLabel("System is in StandAlone mode. \nNo NodeStatus is available."));
        doseInfo->setVisible(false);
        debugLevel->setVisible(false);
        debugLevelLabel->setVisible(false);
        return;
    }

    connect(&m_timer, SIGNAL(timeout()), this, SLOT(Update()));
    m_timer.start(3000);

//QFont font("");
//font.setStyleHint(QFont::TypeWriter);
//doseInfo->setFont(font);

    Update();

    connect(debugLevel, SIGNAL(valueChanged(double)), this, SLOT(DebugLevelChanged(double)));
}



void DoseComInfo::Update()
{
    DOSE_SHARED_DATA_S * pShm = (DOSE_SHARED_DATA_S *) Get_NodeSharedData_Pointer();
    char buf[1024];
    Get_Status_Info(pShm,buf);
    doseInfo->setPlainText(buf);
}


void DoseComInfo::DebugLevelChanged(double newValue)
{
    DOSE_SHARED_DATA_S * pShm = (DOSE_SHARED_DATA_S *) Get_NodeSharedData_Pointer();
    pShm->Debug = (int)newValue;
}
