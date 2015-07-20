/******************************************************************************
*
* Copyright Saab AB, 2013-2015 (http://safir.sourceforge.net)
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
#pragma once

#include <iostream>
#include <string>
#include <boost/atomic.hpp>
#include <map>
#include <queue>
#include <boost/make_shared.hpp>
#include <boost/optional.hpp>
#include <boost/lexical_cast.hpp>

#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100)
#endif

#include <boost/thread.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

#include "../../src/MessageQueue.h"
#include "../../src/Node.h"
#include "../../src/DeliveryHandler.h"
#include "../../src/HeartbeatSender.h"
#include "../../src/DataReceiver.h"
#include "../../src/Writer.h"
#include "../../src/Discoverer.h"
#include "../../src/DataSender.h"
#include "../../src/Resolver.h"


#ifdef _MSC_VER
#pragma warning(disable:4127) //Get rid of warning that this if-expression is constant (comparing two constants)
#endif

#define CHECK(expr) {if (!(expr)) { std::cout<<"Test failed! Line: "<<__LINE__<<", expr: "<< #expr <<std::endl; exit(1);}}
#define CHECKMSG(expr, msg) {if (!(expr)) { std::cout<<"Test failed! Line: "<<__LINE__<<", expr: "<< #expr <<", msg: "<<msg<<std::endl; exit(1);}}
#define CHECKINF(expr, msg) {if (!(expr)) { std::cout<<"Test failed! Line: "<<__LINE__<<", expr: "<< #expr <<", msg: " ## msg << std::endl; exit(1);}}

#define TRACELINE {std::cout<<"line "<<__LINE__<<std::endl;}

namespace Com = Safir::Dob::Internal::Com;

void Wait(int millis) {boost::this_thread::sleep_for(boost::chrono::milliseconds(millis));}

boost::shared_ptr<char[]> MakeShared(const std::string& str)
{
    boost::shared_ptr<char[]> ptr(new char[str.length()]);
    memcpy(ptr.get(), str.c_str(), str.length());
    return ptr;
}
