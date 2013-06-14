/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://www.safirsdk.com)
*
* Created by: Joel Ottosson / joot
*
*******************************************************************************/
#ifndef __CLASSIC_STRING_CAST_H__
#define __CLASSIC_STRING_CAST_H__

#include <iomanip>
#include <sstream>
#include <limits>
#include <boost/lexical_cast.hpp>
#include <boost/mpl/contains.hpp>
#include <boost/mpl/vector.hpp>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
/**
  * The function 'classic_string_cast' is to be used the same way as boost::lexical_cast. The
  * difference is that classic_string_cast always uses locale classic even when the global locale is
  * something else. Practically that means that classic_string_cast will always use dot '.' as decimal
  * separator, never coma ','.
  * The function will throw the exception boost::bad_lexical_cast if a conversion fails.
  *
  * Function:
  *          Target classic_string_cast<Target, Source>(const Source& src)
  *
  * Usage example:
  *             std::wstring numStr=classic_string_cast<std::wstring>(123);
  *             double dblVal=classic_string_cast<double>("123.456");
  */


struct TargetIsString{};
struct SourceIsString{};

template <bool Target, bool Source>
struct IsStringType
{
    typedef TargetIsString type;
};

template <>
struct IsStringType<false, true>
{
    typedef SourceIsString type;
};

//Declaration without implementation. We only allow conversions to and/or from string or wstring
template<typename Target, typename Source, typename IsString>
struct classic_string_cast_impl;

template<typename Target, typename Source>
struct classic_string_cast_impl<Target, Source, TargetIsString>
{
    inline Target operator()(const Source& src) const
    {
        typedef typename Target::traits_type::char_type CharType;
        typedef std::numeric_limits< Source > Limit;
        std::basic_stringstream<CharType> os;
        os.imbue(std::locale::classic());
        os<<std::setprecision(Limit::digits10)<<src;
        return os.str();
    }
};

template<typename Target, typename Source>
struct classic_string_cast_impl<Target, Source, SourceIsString>
{
    inline Target operator()(const Source& src) const
    {
        typedef typename Source::traits_type::char_type CharType;
        typedef std::numeric_limits< Source > Limit;
        std::basic_stringstream<CharType> is(src);
        is.imbue(std::locale::classic());
        Target result;
        is>>std::setprecision(Limit::digits10)>>result;
        if (!is.eof()) //not all chars were read, means that some chars are not allowed
        {
            throw boost::bad_lexical_cast();
        }
        return result;
    }
};

//-----------------------------------
// The function classic_string_cast
//-----------------------------------
template<typename Target, typename Source>
inline Target classic_string_cast(const Source& src)
{
    typedef typename boost::mpl::contains<  boost::mpl::vector< std::string, std::wstring >,
                                            Target >::type TargetIsString;
    typedef typename boost::mpl::contains<  boost::mpl::vector< std::string, std::wstring >,
                                            Source >::type SourceIsString;
    typedef typename IsStringType< TargetIsString::value, SourceIsString::value >::type SelectedVersion;
    return classic_string_cast_impl< Target, Source, SelectedVersion > ()(src);
}

//specialization for raw pointer char*
template<typename Target> inline Target classic_string_cast(const char* src)
{
    return classic_string_cast< Target, std::string>(src);
}

//specialization for raw pointer wchar_t*
template<typename Target> inline Target classic_string_cast(const wchar_t* src)
{
    return classic_string_cast< Target, std::wstring>(src);
}

}}}} //Safir::Dob::Typesystem::Internal

#endif // CLASSIC_STRING_CAST_H
