/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

//
// Test program for Connection class.
//

#include <DoseTest/GlobalMessage.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/ConnectionMiscAspect.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Typesystem/Serialization.h>

#include <iostream>
#include <sstream>

#include <ace/OS_NS_unistd.h>

#include <boost/lexical_cast.hpp>

class TestSendMsg :
   public Safir::Dob::StopHandler,
   public Safir::Dob::Dispatcher,
   public Safir::Dob::MessageSender
{
public:
   TestSendMsg() {};
   ~TestSendMsg();


   // Overrides Safir::Dob::StopHandler
   void  OnStopOrder() {};

   // Overrides Safir::Dob::Dispatcher
   void OnDoDispatch() {};

   // Overrides Safir::Dob::MessageSender
   void OnNotMessageOverflow() {};


   // Tests
   void Connect();
   void Status();
   void Send(const Safir::Dob::MessagePtr & message);


private:
   Safir::Dob::Connection m_connection;

};

// ----------------------------------------------------------------------

TestSendMsg::~TestSendMsg()
{
    if (m_connection.IsOpen())
    {
        m_connection.Close();
    }
}



void
TestSendMsg::Connect()
{
   m_connection.Open(L"TestSendMsg",
                     L"0",
                     0,
                     this,
                     this);
}

void
TestSendMsg::Status()
{
   std::wcout << "TestSendMsg::Status() - Open: " << m_connection.IsOpen() <<  std::endl;

   if (m_connection.IsOpen())
       std::wcout << "TestSendMsg::Status() - Name: " << Safir::Dob::ConnectionMiscAspect(m_connection).GetConnectionName() << std::endl;

}

void
TestSendMsg::Send(const Safir::Dob::MessagePtr & message)
{
   if (m_connection.IsOpen())
       m_connection.Send(message, Safir::Dob::Typesystem::ChannelId(), this);
}



// ----------------------------------------------------------------------

// Main entry
int main(int argc, char** argv)
{
   std::wcout << "TestSendMsg starting..." << std::endl;

   std::wstring infoMsg = L"hello world";


   if (argc > 1)
   {
      infoMsg = Safir::Dob::Typesystem::Utilities::ToWstring(argv[1]);
   }



   TestSendMsg c;
   c.Connect();
   c.Status();

   DoseTest::GlobalMessagePtr msg = DoseTest::GlobalMessage::Create();
   msg->Info().SetVal(infoMsg);

   for(int i = 0;; ++i)
   {

      std::wostringstream os;

      os << infoMsg << ". id: " << i;

      DoseTest::GlobalMessagePtr msg = DoseTest::GlobalMessage::Create();
      msg->Info().SetVal(os.str());

      c.Send(msg);

      std::wcout << "Sent msg: " << Safir::Dob::Typesystem::Serialization::ToXml(msg) << std::endl;
      ACE_OS::sleep(5);
   }


//   std::wcout << "TestSendMsg stopping..." << std::endl;

//   return 0;
}
