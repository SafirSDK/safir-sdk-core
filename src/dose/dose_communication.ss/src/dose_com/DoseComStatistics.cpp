/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
*
* Created by: Lars Engdahl / stlsen
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

#include "DosePlatform.h"
#include "DoseOsInterface.h"
#include "DoseComConfig.h"
#include "DoseComStatistics.h"

// All statistic counters are kept here

static
Statistics::RX_STATISTICS_S Rx_Statistics[MAX_NUM_PRIO_CHANNELS] = {{0}};

static
Statistics::TX_STATISTICS_S Tx_Statistics[MAX_NUM_PRIO_CHANNELS] = {{0}};

//*************************************************************
// Writing to Statistic counters must be fast
// It is done using the pointers retreived by these functions
//**************************************************************

Statistics::RX_STATISTICS_S *Statistics::GetPtrToRxStatistics(int Ix)
{
    return &Rx_Statistics[Ix];
}

Statistics::TX_STATISTICS_S *Statistics::GetPtrToTxStatistics(int Ix)
{
    return &Tx_Statistics[Ix];
}

/*--------------------- end DoseComStatistics.cpp ---------------------*/
