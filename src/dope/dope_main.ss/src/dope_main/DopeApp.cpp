/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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
#include "DopeApp.h"

#include <Safir/Dob/PersistenceParameters.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include "FilePersistor.h"
#include <Safir/SwReports/SwReport.h>
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4702)
#endif

#include <boost/lexical_cast.hpp>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#ifndef NO_DATABASE_SUPPORT
#include "OdbcPersistor.h"
#endif


//The context used by dope to connect to the dob. It is a special context that
//is only meant for dope.
const Safir::Dob::Typesystem::Int32 PERSISTANCE_CONTEXT = -1;

//-------------------------------------------------------
DopeApp::DopeApp():
    m_dispatcher(m_dobConnection),
    m_debug(L"DopeApp")
{
    //perform sanity check!
    if (Safir::Dob::NodeParameters::PersistentDbNode() == -1)
    {
        //std::wcout << "DOPE was started even though Safir.Dob.NodeParameters.PersistentDbNode is set to -1. Please check your configuration"<<std::endl;
        Safir::SwReports::SendFatalErrorReport
            (L"Bad Configuration",L"DopeApp::DopeApp",
             L"DOPE was started even though Safir.Dob.NodeParameters.PersistentDbNode is set to -1. Please check your configuration");
        exit(-1);
    }

    if (Safir::Dob::NodeParameters::PersistentDbNode() != Safir::Dob::ThisNodeParameters::NodeNumber())
    {
        /*std::wcout << std::wstring(L"DOPE was started on a node that is not the node that DOSE expects to be the PersistentDbNode ")
            + L"Safir::Dob::NodeParameters::PersistentDbNode = "
            + boost::lexical_cast<std::wstring>(Safir::Dob::NodeParameters::PersistentDbNode())
            + L" and Safir::Dob::ThisNodeParameters::NodeNumber = "
            + boost::lexical_cast<std::wstring>(Safir::Dob::ThisNodeParameters::NodeNumber()) << std::endl;*/
        Safir::SwReports::SendFatalErrorReport
            (L"Bad Configuration",L"DopeApp::DopeApp",
            std::wstring(L"DOPE was started on a node that is not the node that DOSE expects to be the PersistentDbNode ")
            + L"Safir::Dob::NodeParameters::PersistentDbNode = "
            + boost::lexical_cast<std::wstring>(Safir::Dob::NodeParameters::PersistentDbNode())
            + L" and Safir::Dob::ThisNodeParameters::NodeNumber = "
            + boost::lexical_cast<std::wstring>(Safir::Dob::ThisNodeParameters::NodeNumber()));
        exit(-1);

    }


    m_dobConnection.Open(L"DOPE", L"0", PERSISTANCE_CONTEXT, this, &m_dispatcher);
    m_debug << "Opened DOB connection"<<std::endl;

    m_keeper.Start(*this);
    m_debug << "Started keeper"<<std::endl;


    switch (Safir::Dob::PersistenceParameters::Backend())
    {
    case Safir::Dob::PersistenceBackend::File:
        {
            m_debug << "Using file persistence" << std::endl;
            m_persistenceHandler.reset(new FilePersistor());
        }
        break;

    case Safir::Dob::PersistenceBackend::Odbc:
        {
            m_debug << "Using database persistence" << std::endl;
#ifndef NO_DATABASE_SUPPORT
            m_persistenceHandler.reset(new OdbcPersistor());
#endif
        }
        break;

    default:
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Unknown backend!",__WFILE__,__LINE__);
    }

    m_persistenceHandler->Start();
}

//-------------------------------------------------------
DopeApp::~DopeApp()
{

}


//-------------------------------------------------------
void DopeApp::OnStopOrder()
{
    m_debug << "Got Stop order, will terminate"<< std::endl;
    ACE_Reactor::instance()->end_reactor_event_loop();
}



//-------------------------------------------------------
void
DopeApp::HandleCommand(const std::vector<std::wstring>& cmdTokens)
{
    if (cmdTokens[0] == L"ShowBackend")
    {
        m_debug << "Using backend: " << Safir::Dob::PersistenceBackend::ToString(Safir::Dob::PersistenceParameters::Backend()) <<std::endl;
    }
}

//-------------------------------------------------------
std::wstring
DopeApp::GetHelpText()
{
    return L"ShowBackend - Show which backend is used";
}


void
DopeApp::Run()
{
    ACE_Reactor::instance()->run_reactor_event_loop();
}
