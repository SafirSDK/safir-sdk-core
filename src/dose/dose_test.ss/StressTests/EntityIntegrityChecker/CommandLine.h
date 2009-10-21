/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
*
* Created by: Hannah Myerscough / sthamy
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

#ifndef __COMM_OPT_H__
#define __COMM_OPT_H__
#include <iostream>
#include <boost/noncopyable.hpp>

#include "../common/CommonIncludes.h"

#include <Safir/Dob/Typesystem/Defs.h>

class CommandLine:
    private boost::noncopyable
{
public:
    static CommandLine & Instance();

    bool Parse(int argc, char* argv[]);

    const boost::program_options::variables_map & Options() const {return m_variablesMap;}

    //only one of these can be true
    bool Owner() const {return m_owner;}
    bool Subscriber() const {return m_subscriber;}

    bool Ack() const {return 0 == m_variablesMap.count("no-ack");}
    int SleepTime() const {return m_sleepTime;}

    int NumInstances() const {return m_numInstances;}
private:
    CommandLine() {};
    ~CommandLine() {};

    boost::program_options::variables_map m_variablesMap;

    bool m_owner;
    bool m_subscriber;

    int m_sleepTime;

    int m_numInstances;
};
#endif
