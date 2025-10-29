/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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
#ifndef __SWRE_TRACER_H__
#define __SWRE_TRACER_H__

#include <Safir/Application/Internal/SwReportExportDefs.h>
#include <Safir/Application/Internal/TraceStreamBuffer.h>
#include <Safir/Dob/ConnectionBase.h>
#include <iostream>

namespace Safir
{
namespace Application
{
    /**
     * This class just contains two static methods, for starting and stopping the tracers backdoor.
     */
    class SWRE_INTERFACE_CPP_API TracerBackdoor
    {
    public:
        /**
         * Start reception of trace on/off commands
         *
         * The given connection must be opened before this method is called.
         * The connection name is just used for listening to the appropriate
         * backdoor commands. The tracer will open its own connection in a
         * background thread.
         *
         * @param connection [in] Connection whose name determines which backdoor commands to observe.
         *                        The connection must already be open when this function is called.
         */
        static void Start(const Safir::Dob::ConnectionBase& connection);

        /**
         * Stop reception of trace on/off commands
         */
        static void Stop();
    };


    /**
     * A class for trace logging.
     */
    class SWRE_INTERFACE_CPP_API Tracer
    {
    private:
        typedef std::basic_ios<wchar_t, std::char_traits<wchar_t> > ios_type;
    public:
        typedef std::basic_ostream<wchar_t, std::char_traits<wchar_t> > stream_type;

        /**
         * Constructor.
         * Create a logger with a certain prefix.
         * @param prefix [in] The prefix for this logger.
         */
        explicit Tracer(const std::wstring & prefix);

        /**
         * Destructor
         */
        ~Tracer();

        /**
         * Turn logging of this prefix on or off.
         *
         * @param enabled [in] The state to set logging to.
         */
        void Enable(const bool enabled) {if (m_isEnabled == NULL) {InitializeEnabledHandling();} *m_isEnabled = enabled;}

        /**
         * Check whether this prefix is enabled or not.
         *
         * @return True if logging of this prefix is enabled.
         */
        inline bool IsEnabled() const {if (m_isEnabled == NULL) {InitializeEnabledHandling();} return *m_isEnabled;}

        /**
         * Output operator for io manipulators.
         * Checks whether the prefix is enabled before executing the function.
         */
        inline const Tracer & operator << (stream_type & (* _Pfn)(stream_type&)) const
        {
            if (IsEnabled())
            {
                (*_Pfn)(m_ostream);
            }
            return *this;
        }

        /**
         * Output operator for io manipulators.
         * Checks whether the prefix is enabled before executing the function.
         */
        inline const Tracer & operator << (std::ios_base & (* _Pfn)(std::ios_base &)) const
        {
            if (IsEnabled())
            {
                (*_Pfn)(m_ostream);
            }
            return *this;
        }

        /**
         * Output operator for io manipulators.
         * Checks whether the prefix is enabled before executing the function.
         */
        inline const Tracer & operator<<(ios_type & (*_Pfn)(ios_type&)) const
        {
            if (IsEnabled())
            {
                (*_Pfn)(m_ostream);
            }
            return *this;
        }

        /**
         * Output operator for data.
         * Checks whether the prefix is enabled before performing the output.
         */
        template <class T>
        inline const Tracer & operator << (const T & data) const
        {
            if (IsEnabled())
            {
                m_ostream << data;
            }
            return *this;
        }

        /**
         * Get the underlying ostream of the logger.
         *
         * Since the tracer is not an ostream itself it is sometimes
         * desirable to get the underlying ostream to be able to pass to existing
         * functions that take ostreams as argument. Note however
         * that the checks for whether the prefix is enabled are not performed
         * within this stream! So to use this reliably you need to surround the use
         * of the stream with checks to see if the prefix is enabled or not.
         *
         * @return The underlying ostream of the tracer.
         */
        stream_type & stream() const {return m_ostream;}
    private:
        void InitializeEnabledHandling() const;

        mutable stream_type m_ostream;
        mutable Internal::TraceStreamBuffer m_buf;
        mutable volatile bool * m_isEnabled;
    };

}
}
#endif

