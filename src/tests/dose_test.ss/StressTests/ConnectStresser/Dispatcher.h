#ifndef DISPATCHER_H
#define DISPATCHER_H

#include <Safir/Utilities/Internal/Atomic.h>
#include <Safir/Dob/Connection.h>


class Dispatcher:
    public Safir::Dob::Dispatcher,
    private boost::noncopyable
{
public:
    Dispatcher(const boost::function<void(void)> & dispatchCallback,
               boost::asio::io_service & ioService)
        : m_dispatchCallback(dispatchCallback)
        , m_isNotified(0)
        , m_ioService(ioService)
    {}

private:
    virtual void OnDoDispatch()
    {
        if (m_isNotified == 0)
        {
            m_isNotified = 1;
            m_ioService.post(boost::bind(&Dispatcher::Dispatch,this));
        }
    }
    virtual void Dispatch()
    {
        m_isNotified = 0;
        m_dispatchCallback();
    }

    const boost::function<void(void)> m_dispatchCallback;
    Safir::Utilities::Internal::AtomicUint32 m_isNotified;
    boost::asio::io_service & m_ioService;
};

#endif // DISPATCHER_H

