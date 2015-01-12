/******************************************************************************
*
* Copyright Saab AB, 2011-2013 (http://safir.sourceforge.net)
*
* Created by: Anders Widén / stawi
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
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Utilities/Internal/Atomic.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/InstanceIdPolicy.h>
#include <boost/noncopyable.hpp>
#include <boost/function.hpp>
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int.hpp>
#include <boost/random/variate_generator.hpp>


#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4267)
#endif

#include <ace/Reactor.h>

#ifdef _MSC_VER
  #pragma warning(pop)
#endif

class Dispatcher:
    public ACE_Event_Handler,
    public Safir::Dob::Dispatcher,
    private boost::noncopyable
{
public:
    Dispatcher(const boost::function<void(void)> & dispatchCallback):
        m_dispatchCallback(dispatchCallback),
        m_isNotified(0){}

private:
    virtual void OnDoDispatch()
    {
        if (m_isNotified == 0)
        {
            m_isNotified = 1;
            ACE_Reactor::instance()->notify(this);
        }
    }
    virtual int handle_exception(ACE_HANDLE)
    {
        m_isNotified = 0;
        m_dispatchCallback();
        return 0;
    }

    const boost::function<void(void)> m_dispatchCallback;
    Safir::Utilities::Internal::AtomicUint32 m_isNotified;
};

class App:
    public Safir::Dob::StopHandler,
    public Safir::Dob::EntityHandlerPending,
    public Safir::Dob::EntitySubscriber,
    public ACE_Event_Handler,
    private boost::noncopyable
{
public:
    explicit App(const bool isFallbackHandler);
    void Run();

private:

    virtual void OnStopOrder();

    virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                       const Safir::Dob::Typesystem::HandlerId& handlerId);
    
    virtual void OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                         const Safir::Dob::Typesystem::HandlerId& handlerId);

    virtual void OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId       typeId,
                                         const Safir::Dob::Typesystem::HandlerId&   handlerId);

    virtual void OnInjectedNewEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy);

    virtual void OnNewEntity(const Safir::Dob::EntityProxy entityProxy);

    virtual void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy);

    virtual void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy,
                                     const bool                    deletedByOwner);
    
    virtual void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                 Safir::Dob::ResponseSenderPtr        responseSender);
    
    virtual void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                 Safir::Dob::ResponseSenderPtr        responseSender);
    
    virtual void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                 Safir::Dob::ResponseSenderPtr        responseSender);

    virtual int handle_timeout(const ACE_Time_Value&, const void*);

    void DispatchConnection() {m_connection.Dispatch();};

    bool                        m_isFallbackHandler;

    Safir::Dob::Connection      m_connection;
    Dispatcher                  m_dispatcher;

    int                                         m_connectionInst;
    Safir::Dob::Typesystem::TypeId              m_typeId;
    Safir::Dob::InstanceIdPolicy::Enumeration   m_instanceIdPolicy;

    long m_activeDurationTimerId;
    long m_setDeleteTimerId;
    long m_regUnregTimerId;

    long m_counter;

    int m_nbrOfInstances;

    boost::mt19937 m_rng;                 // produces randomness out of thin air

    boost::uniform_int<> m_handlerInterval; 
    boost::variate_generator<boost::mt19937&, boost::uniform_int<> > m_randomHandler;

    boost::uniform_int<> m_instanceInterval; 
    boost::variate_generator<boost::mt19937&, boost::uniform_int<> > m_randomInstance;

    boost::uniform_int<> m_actionInterval;
    boost::variate_generator<boost::mt19937&, boost::uniform_int<> > m_randomAction;

    std::vector<bool> m_setAllowed;
};
