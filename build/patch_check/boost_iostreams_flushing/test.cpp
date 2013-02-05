/******************************************************************************
*
* Copyright Saab Systems AB, 2012 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4512)
#pragma warning (disable: 4996)
#endif

#include <boost/iostreams/stream.hpp>
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/stream_buffer.hpp>
#include <boost/iostreams/concepts.hpp>
#include <boost/iostreams/categories.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/tee.hpp>
#include <boost/thread/mutex.hpp>
#include <iostream>

#ifdef _MSC_VER
#pragma warning (pop)
#endif


class DummyFilter
{
public:
    typedef wchar_t char_type;
    struct category :
        boost::iostreams::output_filter_tag,
        boost::iostreams::flushable_tag {};

    template <typename Sink>
    bool put( Sink& dest, const wchar_t ch )
    {
        return boost::iostreams::put( dest, ch );
    }

    template <typename Sink>
    bool flush(Sink& s) const {return boost::iostreams::flush(s);}
};

bool check(const std::string& filename)
{
    std::wifstream file(filename.c_str());
    std::vector<wchar_t> charbuf(200,0);
    file.getline(&charbuf[0],charbuf.size());
    return std::wstring(&charbuf[0]).find(L"blahonga") != std::wstring::npos;
}



int main()
{
    std::wofstream outputstream("output_fstream.txt");
    
    typedef boost::iostreams::tee_device<boost::iostreams::wfile_sink, std::wostream> my_tee;
    my_tee tee(boost::iostreams::wfile_sink("output_file_sink.txt"),outputstream);
    boost::iostreams::filtering_wostreambuf buf;
    std::wostream out(&buf);
    DummyFilter filter;
    buf.push(filter);
    buf.push(tee);

    out << "blahonga blahonga blahonga!!!" << std::endl;

    int retval = 0;
    
    if (!check("output_file_sink.txt"))
    {
        std::wcout << "boost::iostreams::wfile_sink does not appear to get flushed" << std::endl;
        retval = 1;
    }

    if (!check("output_fstream.txt"))
    {
        std::wcout << "std::wofstream does not appear to get flushed" << std::endl;
        retval = 1;
    }

    if (retval == 0)
    {
        std::wcout << "success" << std::endl;
    }

    return retval;
}

