#include <cstdlib>
#include <boost/noncopyable.hpp>

const unsigned long NUM_LOGS=100000;

#if defined _WIN32

#include <windows.h>

LARGE_INTEGER frequency()
{
    LARGE_INTEGER freq;
    QueryPerformanceFrequency(&freq);
    return freq;
}


inline double now()
{
    static LARGE_INTEGER freq = frequency();
    LARGE_INTEGER t;
    QueryPerformanceCounter(&t);
    return t.QuadPart/(double)freq.QuadPart;
}

#elif defined(linux) || defined(__linux) || defined(__linux__)
inline double now()
{
    timespec t;
    clock_gettime(CLOCK_REALTIME,&t);
    return t.tv_sec + t.tv_nsec / 1.0e9;
}
#else
#error blahonga
#endif


class Timer
{
public:
    Timer():
        m_accumulated(0.0)
    {}

    inline void start()
    {
        m_start = now();
    }

    inline void stop()
    {
        const double tstop = now();
        m_accumulated += tstop - m_start;
    }

    double elapsed() const {return m_accumulated;}
private:
    double m_accumulated;
    double m_start;
};

class ScopeTimer
    : public boost::noncopyable
{
public:
    inline explicit ScopeTimer(Timer& timer)
        :m_timer(timer)
    {
        timer.start();
    }
    
    inline ~ScopeTimer()
    {
        m_timer.stop();
    }
private:
    Timer& m_timer;
};



void work()
{
    static long dummy = 0;
     
    for (int k = 0; k < 500; ++k)
    {
        for (unsigned long j = 0; j < 20; ++j)
        {
            dummy += j%2;
            if (j%3 == 2)
            {
                --dummy;
            }
        }
        dummy += rand();
    }
    
}
