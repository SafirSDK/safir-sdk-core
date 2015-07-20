/******************************************************************************
*
* Copyright Saab AB, 2006-2015 (http://safir.sourceforge.net)
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

#ifndef __COMM_OPT_H__
#define __COMM_OPT_H__
#include <iostream>
#include <boost/noncopyable.hpp>

#include "../common/CommonIncludes.h"


class CommandLine:
    private boost::noncopyable
{
public:
    static CommandLine & Instance();

    bool Parse(int argc, char* argv[]);

    const boost::program_options::variables_map & Options() const {return m_variablesMap;}


    int Timeout() const {return m_timeOut;}
    int Count() const {return m_count;}


private:
    CommandLine() {};
    ~CommandLine() {};

    boost::program_options::variables_map m_variablesMap;

    int m_timeOut;
    int m_count;

};
#endif
