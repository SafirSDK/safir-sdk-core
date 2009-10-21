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
#ifndef __SWRE_TRACER_H__
#define __SWRE_TRACER_H__

#include <Safir/Application/Internal/SwReportExportDefs.h>
#include <Safir/Application/Internal/TraceStreamBuffer.h>
#include <iostream>

namespace Safir
{
namespace Application
{
    /**
     * A class for trace logging.
     */
    class SWRE_API Tracer
    {
    private:
        typedef std::basic_ios<wchar_t, std::char_traits<wchar_t> > ios_type;
    public:
        typedef std::basic_ostream<wchar_t, std::char_traits<wchar_t> > stream_type;

        /**
         * Set the program name of the current executable.
         * This should be set to argv[0] in the main program.
         *
         * @param programName [in] The name of the executable
         */
        static void SetProgramName(const std::wstring & programName);

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
         * Force a flush of the internal buffer.
         */
        void flush() const;

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

