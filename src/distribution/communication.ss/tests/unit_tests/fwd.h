/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#ifndef __SAFIR_DOB_COMMUNICATION_FWD_H__
#define __SAFIR_DOB_COMMUNICATION_FWD_H__

#include <iostream>
#include <string>
#include <map>
#include <boost/make_shared.hpp>
#include <boost/optional.hpp>
#include <boost/thread.hpp>
#include <boost/lexical_cast.hpp>

#include "../../src/MessageQueue.h"
#include "../../src/Node.h"
#include "../../src/DeliveryHandler.h"
#include "../../src/HeartbeatSender.h"
#include "../../src/Reader.h"
#include "../../src/Writer.h"
#include "../../src/Discoverer.h"
#include "../../src/AckedDataSender.h"

#ifdef _MSC_VER
#pragma warning(disable:4127) //Get rid of warning that this if-expression is constant (comparing two constants)
#endif

#define CHECK(expr) {if (!(expr)) { std::cout<<"Test failed! Line: "<<__LINE__<<", expr: "<< #expr <<std::endl; exit(1);}}
#define CHECKMSG(expr, msg) {if (!(expr)) { std::cout<<"Test failed! Line: "<<__LINE__<<", expr: "<< #expr <<", msg: "<<msg<<std::endl; exit(1);}}
#define CHECKINF(expr, msg) {if (!(expr)) { std::cout<<"Test failed! Line: "<<__LINE__<<", expr: "<< #expr <<", msg: " ## msg << std::endl; exit(1);}}

namespace Com = Safir::Dob::Internal::Com;

void Wait(int millis) {boost::this_thread::sleep_for(boost::chrono::milliseconds(millis));}

boost::shared_ptr<char[]> MakeShared(const std::string& str)
{
    boost::shared_ptr<char[]> ptr(new char[str.length()]);
    memcpy(ptr.get(), str.c_str(), str.length());
    return ptr;
}

#endif
